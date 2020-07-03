#Test your package with Ctrl/Cmd + Shift + T or devtools::test().
#usethis::use_testthat()

library(reticulate)
library(testthat)
#test_check("text")
help("skip_on_cran")

context("Embedding of text and retrieval of word embeddings")

test_that("textEmbed produces a list of word embeddings, that are 768 dimensions", {
  #skip_if_no_python()
  skip_on_cran()

  #library(testthat)
  #library(text)
  #library(tibble)
  #text_to_test_import1 <- c("Let's test this", "hope it works")
  #text_to_test_import2 <- c("I'm happy", "Let's go")
  #data_tibble_to_test <- tibble::tibble(text_to_test_import1, text_to_test_import2)
  #data_tibble_to_test
  x <- sq_data_tutorial8_10[1:2, 1:2]
  #wordembeddings <- textHuggingFace(x)
  #wordembeddings_text <- text::textEmbed(data_tibble_to_test)

  expect_that(wordembeddings_text, is_a("list"))
  # Is the first value there and numeric
  #expect_that(wordembeddings_text[[1]][[1]][[1]], is.numeric)
  #If below line fail it might be because the output in the extract_features in RBERT have changed,
  #so that 773 needs to be something else
  #expect_that(ncol(wordembeddings_text[[1]]), equals(768) )
})



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










