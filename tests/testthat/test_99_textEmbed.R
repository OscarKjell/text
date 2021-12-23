library(text)
library(tibble)
library(testthat)

context("Embedding of text and retrieval of word embeddings")

# helper function to skip tests if we don't have the 'foo' module
skip_if_no_transformers <- function() {
  have_transformers <- reticulate::py_module_available("transformers")
  if (!have_transformers) {
    skip("transformers not available for testing")
  }
}
skip_if_no_torch <- function() {
  have_torch <- reticulate::py_module_available("torch")
  if (!have_torch) {
    skip("torch not available for testing")
  }
}

test_that("textEmbedLayerAggregation 'all': layer =  aggregate_tokens = 'mean' produces aggregated word embeddings", {
  skip_on_cran()

  # skip_on_cran()
  aggregated_embeddings <- textEmbedLayerAggregation(embeddings_from_huggingface2$context,
    layers = 0:1,
    aggregate_layers = "mean",
    aggregate_tokens = "normalize"
  )

  expect_is(aggregated_embeddings$harmonywords[[1]][1], "numeric")
  expect_true(tibble::is_tibble(aggregated_embeddings$harmonywords))
  length_dims_mean <- length(aggregated_embeddings[[1]])

  aggregated_embeddings_con <- textEmbedLayerAggregation(embeddings_from_huggingface2$context,
    layers = 0:1,
    aggregate_layers = "concatenate",
    aggregate_tokens = "mean"
  )
  length_dims_con <- length(aggregated_embeddings_con[[1]])
  expect_true(2 * length_dims_mean == length_dims_con)

  # Expect error
  expect_error(aggregated_embeddings <- textEmbedLayerAggregation(embeddings_from_huggingface2$context,
                                                    layers = 0:3,
                                                    aggregate_layers = "mean",
                                                    aggregate_tokens = "mean"
  )
  )


})

test_that("textEmbedLayerAggregation 1:2 'min' tokens_select = '[CLS]' produces aggregated word embeddings", {
  skip_on_cran()

  # skip_on_cran()
  aggregated_embeddings <- textEmbedLayerAggregation(embeddings_from_huggingface2$context,
    layers = 1:2,
    aggregate_layers = "concatenate",
    aggregate_tokens = "min",
    tokens_select = "[CLS]"
  )

  expect_is(aggregated_embeddings$harmonywords[[1]][1], "numeric")
  expect_true(tibble::is_tibble(aggregated_embeddings$harmonywords))
})

test_that("textEmbedLayerAggregation 1:2 'max' tokens_deselect = '[CLS]' produces aggregated word embeddings", {
  skip_on_cran()

  # skip_on_cran() library(purrr)
  aggregated_embeddings <- textEmbedLayerAggregation(embeddings_from_huggingface2$context,
    layers = "all",
    aggregate_tokens = "max",
    tokens_deselect = c("[CLS]")
  )

  expect_is(aggregated_embeddings$harmonywords[[1]][1], "numeric")
  expect_true(tibble::is_tibble(aggregated_embeddings$harmonywords))
})

test_that("textEmbedStatic with example space", {
  skip_on_cran()

  # Create example space
  words <- c("happy", "joy", "smile")
  Dim1 <- c(0.1, 4, 7)
  Dim2 <- c(2, 5, 8)
  Dim3 <- c(3, 6, 9)
  test_space <- tibble(words, Dim1, Dim2, Dim3)
  test_space
  # Create example data
  word_response <- c("happy", "joy smile", "adfae", NA, "")
  rating_response <- c(25, 30, 30, 2, 10)
  tibble_response <- tibble(word_response, rating_response)
  tibble_response

  # Test function
  test_result <- textEmbedStatic(df = tibble_response, space = test_space, tk_df = "null", aggregate = "mean")
  test_result
  # rlang::last_error()
  expect_is(test_result$word_response[[1]][[1]], "numeric")
  expect_is(test_result, "list")
})

# Potentially below works on GitHUB but not on Mac?

test_that("installing text", {

  #help(textrpp_install)
  textrpp_install(prompt = FALSE)

  #help(textrpp_initialize)
  textrpp_initialize()
})

test_that("textEmbedLayersOutput contexts=TRUE, decontexts = FALSE returns a list", {
  skip_on_cran()

  # skip_on_cran()
  # skip_if_no_transformers()
  # skip_if_no_torch

  # x <- Language_based_assessment_data_8[1:2, 1:2]
  text_to_test_import1 <- c("test this", "hope it works")
  text_to_test_import2 <- c("I am happy", "Let us go")
  x <- tibble::tibble(text_to_test_import1, text_to_test_import2)

  embeddings <- textEmbedLayersOutput(x,
    model = "bert-base-uncased",
    contexts = TRUE,
    decontexts = FALSE,
    return_tokens = TRUE,
    layers = "all"
  )

  # Is the first value there and numeric
  expect_that(embeddings[[1]][[1]][[1]][[1]][[1]], is.character)
  # If below line fail it might be because the output in huggingface has changed,
  # so that 770 needs to be something else
  expect_that(ncol(embeddings[[1]][[1]][[1]]), equals(771))
})

test_that("textEmbedLayersOutput bert-base-uncased contexts=FALSE, decontexts = TRUE returns a list", {
  skip_on_cran()
  # skip_if_no_transformers()
  # skip_if_no_torch

  # x <- Language_based_assessment_data_8[1:2, 1:2]
  text_to_test_import1 <- c("jag mår bra", "vad händer")
  text_to_test_import2 <- c("ön är vacker", "molnen svävar")
  x <- tibble::tibble(text_to_test_import1, text_to_test_import2)

  embeddings <- textEmbedLayersOutput(x,
    model = "bert-base-uncased",
    contexts = FALSE,
    decontexts = TRUE,
    layers = "all"
  )
  expect_that(embeddings, is_a("list"))

  # Is the first value there and numeric
  expect_that(embeddings[[1]][[1]][[1]][[1]][[1]], is.character)
  # If below line fail it might be because the output in huggingface has changed,
  # so that 770 needs to be something else
  expect_that(ncol(embeddings[[1]][[1]][[1]][[1]]), equals(771))
})

test_that("textEmbed", {
  skip_on_cran()
  # skip_if_no_transformers()
  # skip_if_no_torch

  # x <- Language_based_assessment_data_8[1:2, 1:2]
  text_to_test_import1 <- c("test this", "hope it works")
  text_to_test_import2 <- c("I am happy", "Let us go")
  x <- tibble::tibble(text_to_test_import1, text_to_test_import2)

  embeddings_decontextsT <- textEmbed(x,
    model = "bert-base-uncased",
    decontexts = TRUE
  )

  embeddings_decontextsF <- textEmbed(x,
    model = "bert-base-uncased",
    decontexts = FALSE
  )

  expect_that(embeddings_decontextsT, is_a("list"))
  expect_that(embeddings_decontextsF, is_a("list"))



  long_text_test <- c("Humour (British English) or humor (American English; see spelling differences) is the tendency to experiences to provoke laughter and provide amusement. The term derives from the humoral medicine of the ancient Greeks, which taught that the balance of fluids in the human body, known as humours (Latin: humor, body fluid), controlled human health and emotion.
 People of all ages and cultures respond to humour. Most people are able to experience humour—be amused, smile or laugh at something funny (such as a pun or joke)—and thus are considered to have a sense of humour. The hypothetical person lacking a sense of humour would likely find the behaviour inducing it to be inexplicable, strange, or even irrational. Though ultimately decided by personal taste, the extent to which a person finds something humorous depends on a host of variables, including geographical location, culture, maturity, level of education, intelligence and context. For example, young children may favour slapstick such as Punch and Judy puppet shows or the Tom and Jerry cartoons, whose physical nature makes it accessible to them. By contrast, more sophisticated forms of humour such as satire require an understanding of its social meaning and context, and thus tend to appeal to a more mature audience.
 Humour (British English) or humor (American English; see spelling differences) is the tendency of experiences to provoke laughter and provide amusement. The term derives from the humoral medicine of the ancient Greeks, which taught that the balance of fluids in the human body, known as humours (Latin: humor, body fluid), controlled human health and emotion.
 People of all ages and cultures respond to humour. Most people are able to experience humour—be amused, smile or laugh at something funny (such as a pun or joke)—and thus are considered to have a sense of humour. The hypothetical person lacking a sense of humour would likely find the behaviour inducing it to be inexplicable, strange, or even irrational. Though ultimately decided by personal taste, the extent to which a person finds something humorous depends on a host of variables, including geographical location, culture, maturity, level of education, intelligence and context. For example, young children may favour slapstick such as Punch and Judy puppet shows or the Tom and Jerry cartoons, whose physical nature makes it accessible to them. By contrast, more sophisticated forms of humour such as satire require an understanding of its social meaning and context, and thus tend to appeal to a more mature audience.
 Humour (British English) or humor (American English; see spelling differences) is the tendency of experiences to provoke laughter and provide amusement. The term derives from the humoral medicine of the ancient Greeks, which taught that the balance of fluids in the human body, known as humours (Latin: humor, body fluid), controlled human health and emotion.
 People of all ages and cultures respond to humour. Most people are able to experience humour—be amused, smile or laugh at something funny (such as a pun or joke)—and thus are considered to have a sense of humour. The hypothetical person lacking a sense of humour would likely find the behaviour inducing it to be inexplicable, strange, or even irrational. Though ultimately decided by personal taste, the extent to which a person finds something humorous depends on a host of variables, including geographical location, culture, maturity, level of education, intelligence and context. For example, young children may favour slapstick such as Punch and Judy puppet shows or the Tom and Jerry cartoons, whose physical nature makes it accessible to them. By contrast, more sophisticated forms of humour such as satire require an understanding of its social meaning and context, and thus tend to appeal to a more mature audience.
 ")
  long_text_embedding <- textEmbed(long_text_test,
    model = "bert-base-uncased"
  )

  expect_that(long_text_embedding, is_a("list"))
})
