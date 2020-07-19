#Test your package with Ctrl/Cmd + Shift + T or devtools::test()
#usethis::use_testthat()


library(text)
library(tibble)


context("Embedding of text and retrieval of word embeddings")

# helper function to skip tests if we don't have the 'foo' module
skip_if_no_transformers <- function() {
  have_transformers <- reticulate::py_module_available("transformers")
  if (!have_transformers)
    skip("transformers not available for testing")
}
skip_if_no_torch <- function() {
  have_torch <- reticulate::py_module_available("torch")
  if (!have_torch)
    skip("torch not available for testing")
}

test_that("textLayerAggregation 1:2 produces aggregated word embeddings", {

  #skip_on_cran()
  aggregated_embeddings <-  textLayerAggregation(embeddings_from_huggingface2$context, layers = 1:2)

  expect_is(aggregated_embeddings$harmonywords[[1]][1], 'numeric')
  expect_true(tibble::is_tibble(aggregated_embeddings$harmonywords))

})

#install.packages("testthat")

test_that("Embedding produces similarity scores", {
  #skip_on_cran()
  skip_if_no_transformers()
  skip_if_no_torch

  #x <- Language_based_assessment_data_8_10[1:2, 1:2]
  text_to_test_import1 <- c("test this", "hope it works")
  text_to_test_import2 <- c("I am happy", "Let us go")
  x <- tibble::tibble(text_to_test_import1, text_to_test_import2)

  embeddings <- textHuggingFace(x, contexts=TRUE, decontexts = FALSE)
  expect_that(embeddings, is_a("list"))

})


test_that("Embedding produces similarity scores", {
  #skip_on_cran()
  skip_if_no_transformers()
  skip_if_no_torch

  #x <- Language_based_assessment_data_8_10[1:2, 1:2]
  text_to_test_import1 <- c("test this", "hope it works")
  text_to_test_import2 <- c("I am happy", "Let us go")
  x <- tibble::tibble(text_to_test_import1, text_to_test_import2)

  embeddings <- textEmbed(x)
  expect_that(embeddings, is_a("list"))

})




#
#test_that("Testing sourcing python file in testthat", {
#
#  output <- f1_sourced_from_python()
#  expect_that(output, is_a("character"))
#
#})


#test_that("textLayerAggregation 'all' produces aggregated word embeddings", {
#
#  #skip_on_cran()
#  aggregated_embeddings <-  textLayerAggregation(embeddings_from_huggingface2$context, layers = 'all')
#
#  expect_is(aggregated_embeddings$harmonywords[[1]][1], 'numeric')
#  expect_true(tibble::is_tibble(aggregated_embeddings$harmonywords))
#
#})


#test_that("textEmbed produces a list of word embeddings", {

#  #skip_on_cran()

#  text_to_test_import1 <- c("Let's test this", "hope it works")
#  text_to_test_import2 <- c("I'm happy", "Let's go")
#  x <- tibble::tibble(text_to_test_import1, text_to_test_import2)
#  #data_tibble_to_test
#  library(text)
#  x <- Language_based_assessment_data_8_10[1, 1]
#  wordembeddings <- text::textHuggingFace(x, layers = 11:12)
#  testthat::expect_that(wordembeddings, testthat::is_a("list"))

#wordembeddings_aggr_layers <- textLayerAggregation(wordembeddings,  layers = 12)
#testthat::expect_that(wordembeddings_aggr_layers, testthat::is_a("list"))
#help(textLayerAggregation)

# Is the first value there and numeric
#expect_that(wordembeddings_text[[1]][[1]][[1]], is.numeric)
#If below line fail it might be because the output in the extract_features in RBERT have changed,
#so that 773 needs to be something else
#expect_that(ncol(wordembeddings_text[[1]]), equals(768) )
#})



#test_that("textEmbed produces a list of word embeddings, that are 768 dimensions", {
#  #library(testthat)
#  #library(text)
#  #library(tibble)
#  text_to_test_import1 <- c("Let's test this", "hope it works")
#  text_to_test_import2 <- c("I'm happy", "Let's go")
#  data_tibble_to_test <- tibble::tibble(text_to_test_import1, text_to_test_import2)
#  data_tibble_to_test
#  wordembeddings_text <- textEmbed(data_tibble_to_test)
#
#  expect_that(wordembeddings_text, is_a("list"))
#  # Is the first value there and numeric
#  expect_that(wordembeddings_text[[1]][[1]][[1]], is.numeric)
#  #If below line fail it might be because the output in the extract_features in RBERT have changed,
#  #so that 773 needs to be something else
#  expect_that(ncol(wordembeddings_text[[1]]), equals(768) )
#})
#

#context("textHuggingFace of text and retrieval of word embeddings")
#
#test_that("textHuggingFace produces a list of word embeddings, that are 768 dimensions", {
#
#  text_to_test_import1 <- c("Let's test this", "hope it works")
#  text_to_test_import2 <- c("I'm happy", "Let's go")
#  data_tibble_to_test <- tibble::tibble(text_to_test_import1, text_to_test_import2)
#  data_tibble_to_test
#  wordembeddings_text <- textHuggingFace(data_tibble_to_test)
#
#  expect_that(wordembeddings_text, is_a("list"))
#  # Is the first value there and numeric
#  expect_that(wordembeddings_text[[1]][[1]][[1]], is.character)
#  expect_that(wordembeddings_text[[1]][[1]][[1]][[3]][[1]], is.numeric)
#  #If below line fail it might be because the output in the extract_features in RBERT have changed,
#  #so that 773 needs to be something else
#  expect_that(ncol(wordembeddings_text[[1]][[1]][[1]]), equals(770) )
#})
#
