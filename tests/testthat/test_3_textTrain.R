#Test your package with Ctrl/Cmd + Shift + T or devtools::test().
#usethis::use_testthat()

library(testthat)
library(text)
library(tibble)
library(psych)
library(dplyr)
#test_check("text")

context("Training Functions")

wordembeddings_text <- readRDS("/Users/oscar/Desktop/0 Studies/5 R statistical semantics package/text/tests/testthat/wordembeddings_for_testthat.RData")
numeric_variables <- readRDS("/Users/oscar/Desktop/0 Studies/5 R statistical semantics package/text/tests/testthat/numeric_for_testthat.RData")


test_that("textTrain produces trainged results", {

  trained <- textTrain(wordembeddings_text$harmonytexts,
                       numeric_variables$hilstotal)
  expect_that(trained, is_a("list"))

})







