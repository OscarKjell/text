library(tibble)
library(dplyr)
library(text)
library(testthat)

context("textWordPrediction")

# Build a small, self-contained fixture: a vocabulary of words, text responses
# assembled from that vocabulary, a numeric outcome, and one decontextualised
# embedding vector per word. This gives full vocabulary coverage so the ridge
# models can be trained deterministically without a real embedding space.
make_word_prediction_fixture <- function(seed = 42) {
  set.seed(seed)

  vocab <- c("happy", "calm", "joy", "love", "peace", "harmony", "sad",
             "angry", "mad", "stress", "worry", "fear", "balance",
             "content", "hope")

  # 30 participants, each with 2-3 words drawn from the vocabulary
  responses <- replicate(30, paste(sample(vocab, sample(2:3, 1)), collapse = " "))

  positive <- c("happy", "calm", "joy", "love", "peace", "harmony",
                "balance", "content", "hope")

  # Outcomes loosely tied to how many positive words a response contains
  x <- sapply(responses, function(r) sum(strsplit(r, " ")[[1]] %in% positive)) +
    rnorm(30, 5, 1)
  y <- sapply(responses, function(r) sum(strsplit(r, " ")[[1]] %in% positive[1:4])) +
    rnorm(30, 5, 1)

  # One 10-dimensional decontextualised embedding per vocabulary word
  emb <- tibble::as_tibble(
    matrix(rnorm(length(vocab) * 10),
           nrow = length(vocab),
           dimnames = list(NULL, paste0("Dim", 1:10)))
  )
  word_types <- dplyr::bind_cols(tibble::tibble(words = vocab, n = 1L), emb)

  list(
    words      = unname(responses),
    x          = unname(x),
    y          = unname(y),
    word_types = word_types,
    n_vocab    = length(vocab)
  )
}

test_that("textWordPrediction 1-DIMENSION returns word-level prediction data", {
  skip_on_cran()

  fx <- make_word_prediction_fixture()

  res <- text::textWordPrediction(
    words                 = fx$words,
    word_types_embeddings = fx$word_types,
    x                     = fx$x,
    n_models              = 2,
    n_permutations        = 100,
    seed                  = 1003
  )

  # Output shape expected by textProjectionPlot()
  expect_type(res, "list")
  expect_true("model_x" %in% names(res))
  expect_true("word_data" %in% names(res))
  expect_false("model_y" %in% names(res))

  wd <- res$word_data
  expect_true(tibble::is_tibble(wd))
  expect_true(all(c("words", "n", "word_mean_value_x", "x_plotted", "p_values_x") %in%
                    names(wd)))
  expect_false(any(c("y_plotted", "p_values_y") %in% names(wd)))

  # One row per unique vocabulary word, all with valid coordinates
  expect_equal(nrow(wd), fx$n_vocab)
  expect_is(wd$words[1], "character")
  expect_is(wd$x_plotted[1], "numeric")
  expect_true(all(is.finite(wd$x_plotted)))

  # Permutation p-values are proper probabilities
  expect_true(all(wd$p_values_x >= 0 & wd$p_values_x <= 1, na.rm = TRUE))
})

test_that("textWordPrediction 2-DIMENSIONS adds a y-axis model and coordinates", {
  skip_on_cran()

  fx <- make_word_prediction_fixture()

  res <- text::textWordPrediction(
    words                 = fx$words,
    word_types_embeddings = fx$word_types,
    x                     = fx$x,
    y                     = fx$y,
    n_models              = 2,
    n_permutations        = 50,
    seed                  = 1003
  )

  expect_true(all(c("model_x", "model_y", "word_data") %in% names(res)))

  wd <- res$word_data
  expect_true(tibble::is_tibble(wd))
  expect_true(all(c("words", "n", "word_mean_value_x", "word_mean_value_y",
                    "x_plotted", "p_values_x", "y_plotted", "p_values_y") %in%
                    names(wd)))
  expect_equal(nrow(wd), fx$n_vocab)
  expect_true(all(is.finite(wd$x_plotted)))
  expect_true(all(is.finite(wd$y_plotted)))
  expect_true(all(wd$p_values_y >= 0 & wd$p_values_y <= 1, na.rm = TRUE))
})

test_that("textWordPrediction drops words with no embedding and messages the user", {
  skip_on_cran()

  fx <- make_word_prediction_fixture()

  # Remove three words from the embedding space so they have no representation.
  dropped <- c("fear", "worry", "stress")
  partial_word_types <- fx$word_types[!fx$word_types$words %in% dropped, ]

  expect_message(
    res <- text::textWordPrediction(
      words                 = fx$words,
      word_types_embeddings = partial_word_types,
      x                     = fx$x,
      n_models              = 2,
      n_permutations        = 0,
      seed                  = 1003
    ),
    "Skipping 3 of"
  )

  # The words without embeddings must not appear in the output, and every
  # remaining word must have a finite coordinate (no NA embedding rows survived).
  expect_false(any(dropped %in% res$word_data$words))
  expect_equal(nrow(res$word_data), fx$n_vocab - length(dropped))
  expect_true(all(is.finite(res$word_data$x_plotted)))
})

test_that("textWordPrediction errors clearly when too few words have embeddings", {
  skip_on_cran()

  fx <- make_word_prediction_fixture()

  # Keep only two words in the embedding space -> below the 3-word minimum.
  tiny_word_types <- fx$word_types[fx$word_types$words %in% c("happy", "calm"), ]

  expect_error(
    text::textWordPrediction(
      words                 = fx$words,
      word_types_embeddings = tiny_word_types,
      x                     = fx$x,
      n_models              = 2,
      n_permutations        = 0,
      seed                  = 1003
    ),
    "Fewer than 3 words have a valid embedding"
  )
})

test_that("textWordPrediction with n_permutations = 0 skips p-values", {
  skip_on_cran()

  fx <- make_word_prediction_fixture()

  res <- text::textWordPrediction(
    words                 = fx$words,
    word_types_embeddings = fx$word_types,
    x                     = fx$x,
    n_models              = 2,
    n_permutations        = 0,
    seed                  = 1003
  )

  expect_true(all(is.na(res$word_data$p_values_x)))
  expect_true(all(is.finite(res$word_data$x_plotted)))
})
