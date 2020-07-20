
library(testthat)
library(text)
library(tibble)
library(dplyr)


context("Training Functions")

test_that("textTrain produces list of results with prediction being numeric", {
  trained <- textTrain(wordembeddings4_10$harmonytext, Language_based_assessment_data_8_10$hilstotal,
  nrFolds_k = 2, strata_y = NULL)

  expect_that(trained, is_a("list"))
  expect_is(trained$prediction$.pred[1], 'numeric')
})

test_that("textTrainLists regression produces a list of results with prediction being numeric", {
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

test_that("textTrainLists randomForest produces list of results with prediction being numeric", {
  wordembeddings <- wordembeddings4_10[1:2]
  ratings_data1 <- factor(c("young",  "old"))
  ratings_data2 <- factor(c("young",  "old"))
  ratings_data <- tibble(ratings_data1, ratings_data2)
  results <- textTrainLists(wordembeddings, ratings_data, nrFolds_k = 2, trainMethod = "randomForest")

  expect_that(results, is_a("list"))
  expect_is(results$predscores$harmonywords_ratings_data1..pred_old[1][1], 'numeric')
})
