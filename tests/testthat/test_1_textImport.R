#Test your package with Ctrl/Cmd + Shift + T or devtools::test().
#usethis::use_testthat()

library(testthat)
library(text)
library(tibble)

#test_check("text")

context("Import of text and retrieval of word embeddings")

test_that("textImport gives list of word embeddings", {

  text_to_test_import1 <- c("hello I want to test this function", "let us hope it will work")
  text_to_test_import2 <- c("Now I want to have fun", "The game is what I do like")
  data_tibble_to_test <- tibble(text_to_test_import1, text_to_test_import2)
  data_tibble_to_test
  wordembeddings_text <- textImport(data_tibble_to_test)

  expect_that(wordembeddings_text, is_a("list"))
  #If below line fail it might be because the output in the extract_features in RBERT have changed,
  #so that 773 needs to be something else
  expect_that(ncol(wordembeddings_text[[1]]), equals(773) )
})

