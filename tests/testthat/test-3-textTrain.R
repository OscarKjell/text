#Test your package with Ctrl/Cmd + Shift + T or devtools::test().
#usethis::use_testthat()

library(testthat)
library(text)
library(tibble)
#library(psych)
library(dplyr)
#test_check("text")

context("Training Functions")

test_that("textTrain produces list of results with prediction being numeric", {


  trained <- textTrain(wordembeddings4_10$harmonytext, Language_based_assessment_data_8_10$hilstotal,
   nrFolds_k = 2, strata_y = NULL)

  expect_that(trained, is_a("list"))

  expect_is(trained$prediction$.pred[1], 'numeric')


})







