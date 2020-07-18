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



test_that("textTrainLists produces a list of results with prediction being numeric", {
  wordembeddings <- wordembeddings4_10[1:2]
  ratings_data <- Language_based_assessment_data_8_10[5:6]
  results <- textTrainLists(wordembeddings, ratings_data, nrFolds_k = 2)

  expect_that(results, is_a("list"))
  expect_is(results$predscores$harmonywords_hilstotal_pred[1], 'character')
})


test_that("textTrainMultiTexts produces list of results with prediction being numeric", {
  wordembeddings <- wordembeddings4_10[1:2]
  ratings_data <- Language_based_assessment_data_8_10$hilstotal
  results <- textTrainMultiTexts(wordembeddings, ratings_data, nrFolds_k = 2)

  expect_that(results, is_a("list"))
  expect_is(results$prediction$.pred[1], 'numeric')
})


test_that("textTrainMultiTexts produces list of results with prediction being numeric", {
  wordembeddings <- wordembeddings4_10
  ratings_data <- Language_based_assessment_data_8_10
  results <- textTrainRandomForest(wordembeddings$harmonytext, ratings_data$gender, nrFolds_k = 5, trees = 5)
  expect_that(results, is_a("list"))
  expect_is(results$prediction$.pred_1[1], 'numeric')
})



