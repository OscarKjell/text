library(tibble)
library(dplyr)
library(text)
library(testthat)

context("textWordPrediction")

# Build a small, self-contained fixture: a vocabulary of words, text responses
# assembled from that vocabulary, a numeric outcome, and one decontextualised
# embedding vector per word. This gives full vocabulary coverage so the ridge
# predictions can be computed deterministically without a real embedding space.
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

  res <- suppressMessages(text::textWordPrediction(
    words                 = fx$words,
    word_types_embeddings = fx$word_types,
    x                     = fx$x,
    min_freq_words_test   = 1,
    n_permutations        = 199,
    seed                  = 1003
  ))

  expect_type(res, "list")
  expect_true(all(c("word_data", "lambda_x", "loo_r") %in% names(res)))
  expect_false("model_x" %in% names(res))

  wd <- res$word_data
  expect_true(tibble::is_tibble(wd))
  expect_true(all(c("words", "n", "word_mean_value_x", "x_plotted",
                    "p_values_x", "p_z_x") %in% names(wd)))
  expect_false(any(c("y_plotted", "p_values_y") %in% names(wd)))

  expect_equal(nrow(wd), fx$n_vocab)
  expect_is(wd$words[1], "character")
  expect_is(wd$x_plotted[1], "numeric")
  expect_true(all(is.finite(wd$x_plotted)))
  expect_true(all(wd$p_values_x > 0 & wd$p_values_x <= 1))
  expect_true(all(wd$p_z_x >= 0 & wd$p_z_x <= 1))
})

test_that("textWordPrediction 2-DIMENSIONS adds y-axis coordinates", {
  skip_on_cran()

  fx <- make_word_prediction_fixture()

  res <- suppressMessages(text::textWordPrediction(
    words                 = fx$words,
    word_types_embeddings = fx$word_types,
    x                     = fx$x,
    y                     = fx$y,
    min_freq_words_test   = 1,
    n_permutations        = 199,
    seed                  = 1003
  ))

  expect_true(all(c("word_data", "lambda_x", "lambda_y") %in% names(res)))

  wd <- res$word_data
  expect_true(tibble::is_tibble(wd))
  expect_true(all(c("words", "n", "word_mean_value_x", "word_mean_value_y",
                    "x_plotted", "y_plotted", "p_values_x", "p_values_y") %in%
                    names(wd)))
  expect_equal(nrow(wd), fx$n_vocab)
  expect_true(all(is.finite(wd$x_plotted)))
  expect_true(all(is.finite(wd$y_plotted)))
  expect_true(all(wd$p_values_y > 0 & wd$p_values_y <= 1))
})

test_that("textWordPrediction drops words with no embedding", {
  skip_on_cran()

  fx <- make_word_prediction_fixture()

  # Remove two words from the embeddings: they should not be tested
  dropped <- c("hope", "balance")
  word_types_reduced <- fx$word_types[!fx$word_types$words %in% dropped, ]

  res <- suppressMessages(text::textWordPrediction(
    words                 = fx$words,
    word_types_embeddings = word_types_reduced,
    x                     = fx$x,
    min_freq_words_test   = 1,
    n_permutations        = 99,
    seed                  = 1003
  ))

  expect_false(any(dropped %in% res$word_data$words))
  expect_equal(nrow(res$word_data), fx$n_vocab - length(dropped))
  expect_true(all(is.finite(res$word_data$x_plotted)))
})

test_that("textWordPrediction errors clearly when too few words have embeddings", {
  skip_on_cran()

  fx <- make_word_prediction_fixture()

  word_types_tiny <- fx$word_types[1:2, ]

  expect_error(
    suppressMessages(text::textWordPrediction(
      words                 = fx$words,
      word_types_embeddings = word_types_tiny,
      x                     = fx$x,
      n_permutations        = 99
    )),
    "Fewer than 3 words have a valid embedding"
  )
})

test_that("textWordPrediction with n_permutations = 0 skips p-values", {
  skip_on_cran()

  fx <- make_word_prediction_fixture()

  res <- suppressMessages(text::textWordPrediction(
    words                 = fx$words,
    word_types_embeddings = fx$word_types,
    x                     = fx$x,
    min_freq_words_test   = 1,
    n_permutations        = 0,
    seed                  = 1003
  ))

  expect_true(all(is.na(res$word_data$p_values_x)))
  expect_true(all(is.finite(res$word_data$x_plotted)))
})

test_that("textWordPrediction drops participants with missing x and warns on n_models", {
  skip_on_cran()

  fx <- make_word_prediction_fixture()
  x_na <- fx$x
  x_na[1:3] <- NA

  expect_warning(
    res <- suppressMessages(text::textWordPrediction(
      words                 = fx$words,
      word_types_embeddings = fx$word_types,
      x                     = x_na,
      min_freq_words_test   = 1,
      n_models              = 25,
      n_permutations        = 99,
      seed                  = 1003
    )),
    "n_models is deprecated"
  )

  expect_false(anyNA(res$word_data$x_plotted))
})
