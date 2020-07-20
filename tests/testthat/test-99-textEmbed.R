# Test your package with Ctrl/Cmd + Shift + T or devtools::test()

library(text)
library(tibble)

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

test_that("textLayerAggregation 'all' 'mean' produces aggregated word embeddings", {

  # skip_on_cran()
  aggregated_embeddings <- textLayerAggregation(embeddings_from_huggingface2$context,
    layers = "all",
    aggregation = "mean"
  )

  expect_is(aggregated_embeddings$harmonywords[[1]][1], "numeric")
  expect_true(tibble::is_tibble(aggregated_embeddings$harmonywords))
})


test_that("textLayerAggregation 1:2 'min' tokens_select = '[CLS]' produces aggregated word embeddings", {

  # skip_on_cran()
  aggregated_embeddings <- textLayerAggregation(embeddings_from_huggingface2$context,
    layers = 1:2,
    aggregation = "min",
    tokens_select = "[CLS]"
  )

  expect_is(aggregated_embeddings$harmonywords[[1]][1], "numeric")
  expect_true(tibble::is_tibble(aggregated_embeddings$harmonywords))
})

test_that("textLayerAggregation 1:2 'max' tokens_deselect = '[CLS]' produces aggregated word embeddings", {

  # skip_on_cran()
  aggregated_embeddings <- textLayerAggregation(embeddings_from_huggingface2$context,
    layers = 1:2,
    aggregation = "max",
    tokens_deselect = "[CLS]"
  )

  expect_is(aggregated_embeddings$harmonywords[[1]][1], "numeric")
  expect_true(tibble::is_tibble(aggregated_embeddings$harmonywords))
})

test_that("textHuggingFace contexts=TRUE, decontexts = FALSE returns a list", {
  # skip_on_cran()
  # skip_if_no_transformers()
  # skip_if_no_torch

  # x <- Language_based_assessment_data_8_10[1:2, 1:2]
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

  # x <- Language_based_assessment_data_8_10[1:2, 1:2]
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

  # x <- Language_based_assessment_data_8_10[1:2, 1:2]
  text_to_test_import1 <- c("test this", "hope it works")
  text_to_test_import2 <- c("I am happy", "Let us go")
  x <- tibble::tibble(text_to_test_import1, text_to_test_import2)

  embeddings <- textEmbed(x)
  expect_that(embeddings, is_a("list"))
})
