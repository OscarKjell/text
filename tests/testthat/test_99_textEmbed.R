# Test your package with Ctrl/Cmd + Shift + T or devtools::test()

library(text)
library(tibble)
# library(testthat)
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

test_that("textLayerAggregation 'all': layer =   aggregate_tokens = 'mean' produces aggregated word embeddings", {

  # skip_on_cran()
  aggregated_embeddings <- textLayerAggregation(embeddings_from_huggingface2$context,
    layers = 0:1,
    aggregate_layers = "mean",
    aggregate_tokens = "mean"
  )

  expect_is(aggregated_embeddings$harmonywords[[1]][1], "numeric")
  expect_true(tibble::is_tibble(aggregated_embeddings$harmonywords))
  length_dims_mean <- length(aggregated_embeddings[[1]])

  aggregated_embeddings_con <- textLayerAggregation(embeddings_from_huggingface2$context,
                                                layers = 0:1,
                                                aggregate_layers = "concatenate",
                                                aggregate_tokens = "mean"
  )
  length_dims_con <- length(aggregated_embeddings_con[[1]])

  expect_true(2*length_dims_mean == length_dims_con)

})

#test_that("textLayerAggregation concatenate produces long/aggregated word embeddings", {

  # skip_on_cran()
#  aggregated_embeddings <- textLayerAggregation(embeddings_from_huggingface2$context,
#                                                layers = c(1:2),
#                                                aggregate_tokens = "concatenate"
#  )
#  aggregated_embeddings
#  expect_is(aggregated_embeddings$harmonywords[[1]][1], "numeric")
#  expect_true(tibble::is_tibble(aggregated_embeddings$harmonywords))
#})




test_that("textLayerAggregation 1:2 'min' tokens_select = '[CLS]' produces aggregated word embeddings", {

  # skip_on_cran()
  aggregated_embeddings <- textLayerAggregation(embeddings_from_huggingface2$context,
    layers = 1:2,
    aggregate_layers = "concatenate",
    aggregate_tokens = "min",
    tokens_select = "[CLS]"
  )

  expect_is(aggregated_embeddings$harmonywords[[1]][1], "numeric")
  expect_true(tibble::is_tibble(aggregated_embeddings$harmonywords))
})

test_that("textLayerAggregation 1:2 'max' tokens_deselect = '[CLS]' produces aggregated word embeddings", {

  # skip_on_cran()
  aggregated_embeddings <- textLayerAggregation(embeddings_from_huggingface2$context,
    layers = 1:2,
    aggregate_tokens = "max",
    tokens_deselect = "[CLS]"
  )

  expect_is(aggregated_embeddings$harmonywords[[1]][1], "numeric")
  expect_true(tibble::is_tibble(aggregated_embeddings$harmonywords))
})





test_that("textStaticEmbed with example space", {

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
  #Test function
  test_result <- textStaticEmbed(df=tibble_response, space=test_space, tk_df = "null", aggregate = "mean")
  test_result
  # rlang::last_error()
  expect_is(test_result$word_response[[1]][[1]], "numeric")
  expect_is(test_result, "list")
})


# Potentially below works on GitHUB but not on Mac?

test_that("textHuggingFace contexts=TRUE, decontexts = FALSE returns a list", {
  # skip_on_cran()
  # skip_if_no_transformers()
  # skip_if_no_torch

  # x <- Language_based_assessment_data_8[1:2, 1:2]
  text_to_test_import1 <- c("test this", "hope it works")
  text_to_test_import2 <- c("I am happy", "Let us go")
  x <- tibble::tibble(text_to_test_import1, text_to_test_import2)

  embeddings <- textHuggingFace(x, contexts = TRUE, decontexts = FALSE, layers = "all")
  expect_that(embeddings, is_a("list"))

  # Is the first value there and numeric
  expect_that(embeddings[[1]][[1]][[1]][[1]], is.character)
  # If below line fail it might be because the output in huggingface has changed,
  # so that 770 needs to be something else
  expect_that(ncol(embeddings[[1]][[1]][[1]]), equals(770))
})

test_that("textHuggingFace bert-base-multilingual-cased contexts=FALSE, decontexts = TRUE returns a list", {
  # skip_on_cran()
  # skip_if_no_transformers()
  # skip_if_no_torch

  # x <- Language_based_assessment_data_8[1:2, 1:2]
  text_to_test_import1 <- c("jag mår bra", "vad händer")
  text_to_test_import2 <- c("ön är vacker", "molnen svävar")
  x <- tibble::tibble(text_to_test_import1, text_to_test_import2)

  embeddings <- textHuggingFace(x, model = "bert-base-multilingual-uncased", contexts = FALSE, decontexts = TRUE, layers = "all")
  expect_that(embeddings, is_a("list"))

  # Is the first value there and numeric
  expect_that(embeddings[[1]][[1]][[1]][[1]][[1]], is.character)
  # If below line fail it might be because the output in huggingface has changed,
  # so that 770 needs to be something else
  expect_that(ncol(embeddings[[1]][[1]][[1]][[1]]), equals(770))
})

test_that("textEmbed", {
  # skip_on_cran()
  # skip_if_no_transformers()
  # skip_if_no_torch

  # x <- Language_based_assessment_data_8[1:2, 1:2]
  text_to_test_import1 <- c("test this", "hope it works")
  text_to_test_import2 <- c("I am happy", "Let us go")
  x <- tibble::tibble(text_to_test_import1, text_to_test_import2)

  embeddings <- textEmbed(x)
  expect_that(embeddings, is_a("list"))
})
