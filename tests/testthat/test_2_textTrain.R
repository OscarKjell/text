#.rs.restartR()
library(testthat)
library(text)
library(tibble)
library(dplyr)


context("Training Functions")

test_that("textTrain produces list of results with prediction being numeric", {
  trained <- textTrain(wordembeddings4_10$harmonytext,
                       Language_based_assessment_data_8_10$hilstotal,
                       outside_strata_y = NULL,
                       inside_strata_y = NULL,
                       penalty = c(1),
                       mixture = c(0),
                       multi_cores = TRUE
  )

  expect_that(trained, is_a("list"))
  expect_is(trained$prediction$predictions[1], "numeric")
})


test_that("textTrain produces list of results with prediction being categorical", {

  example_categories <- as.factor(c(1, 2, 1, 2, 1, 2, 1, 2, 1, 2))
  trained <- textTrain(wordembeddings4_10$harmonytext,
                       example_categories,
                       outside_strata_y = NULL,
                       inside_strata_y = NULL,
                       mtry = c(1),
                       min_n = c(1),
                       trees = 1000,
                       multi_cores = FALSE)

  expect_that(trained, is_a("list"))
  expect_is(trained$truth_predictions$truth[1], "factor")
})


test_that("textTrainLists regression produces a list of results with prediction being numeric", {
  wordembeddings <- wordembeddings4_10[1:2]
  ratings_data <- Language_based_assessment_data_8_10[5:6]
  results <- textTrainLists(wordembeddings,
                            ratings_data,
                            outside_strata_y = NULL,
                            inside_strata_y = NULL,
                            penalty = c(1),
                            mixture = c(0))

  expect_that(results, is_a("list"))
  expect_is(results$predictions$harmonywords_hilstotal_pred[1], "numeric")
})

test_that("textTrainLists randomForest produces list of results with prediction being numeric", {
  wordembeddings <- wordembeddings4_10[1:2]
  ratings_data1 <- factor(c("young", "old", "young", "old", "young", "old", "young", "old", "young", "old"))
  ratings_data2 <- factor(c("young", "old", "young", "old", "young", "old", "young", "old", "young", "old"))
  ratings_data <- tibble(ratings_data1, ratings_data2)
  results <- textTrainLists(wordembeddings,
                            ratings_data,
                            force_train_method = "random_forest",
                            outside_strata_y = NULL,
                            inside_strata_y = NULL,
                            mtry = c(1),
                            min_n = c(1),
                            trees = 1000)

  expect_that(results, is_a("list"))
  expect_is(results$results$p_value[1], "character")
})



#test_that("textTrainMultiTexts produces list of results with prediction being numeric", {
#  wordembeddings <- wordembeddings4_10[1:2]
#  ratings_data <- Language_based_assessment_data_8_10$hilstotal
#  results <- textTrainMultiTexts(wordembeddings,
#                                 ratings_data)

#  expect_that(results, is_a("list"))
#  expect_is(results$prediction$.pred[1], "numeric")
#})

#test_that("textTrainMultiTexts produces list of results with prediction being numeric", {
#  wordembeddings <- wordembeddings4_10
#  ratings_data <- Language_based_assessment_data_8_10
#  results <- textTrainRandomForest(wordembeddings$harmonytext, ratings_data$gender, nrFolds_k = 5, trees = 5)
#  expect_that(results, is_a("list"))
#  expect_is(results$prediction$.pred_1[1], "numeric")
#})

