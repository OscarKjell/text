#.rs.restartR()
library(testthat)
library(text)
library(tibble)
library(dplyr)


context("Training Functions")

test_that("textTrain produces list of results with prediction being numeric", {

  trained <- textTrain(wordembeddings4$harmonytext,
                       Language_based_assessment_data_8$hilstotal,
                       #outside_strata_y = NULL,
                       #inside_strata_y = NULL,
                       penalty = c(1),
                       mixture = c(0),
                       trees = c(1000),
                       preprocess_PCA_thresh = c(0.95),
                       multi_cores = FALSE
  )

  #warnings()
  testthat::expect_that(trained, is_a("list"))
  testthat::expect_is(trained$prediction$predictions[1], "numeric")
})


test_that("textTrain produces list of results with prediction being categorical", {

  example_categories <- as.factor(c(1, 2, 1, 2, 1, 2, 1, 2, 1, 2,
                                    1, 2, 1, 2, 1, 2, 1, 2, 1, 2,
                                    1, 2, 1, 2, 1, 2, 1, 2, 1, 2,
                                    1, 2, 1, 2, 1, 2, 1, 2, 1, 2))
  trained <- textTrain(wordembeddings4$harmonytext,
                       example_categories,
                       #outside_strata_y = NULL,
                       #inside_strata_y = NULL,
                       mtry = c(1),
                       min_n = c(1),
                       trees = c(1000),
                       preprocess_PCA_thresh = c(0.95),
                       multi_cores = FALSE,
                       eval_measure = "roc_auc",
                       force_train_method = "random_forest") #sens bal_accuracy f_measure

  testthat::expect_that(trained, testthat::is_a("list"))
  testthat::expect_is(trained$truth_predictions$truth[1], "factor")
})



test_that("textTrainRandomForest with Extremely Randomized Trees produces list of results with prediction being categorical", {

  example_categories <- as.factor(c(1, 2, 1, 2, 1, 2, 1, 2, 1, 2,
                                    1, 2, 1, 2, 1, 2, 1, 2, 1, 2,
                                    1, 2, 1, 2, 1, 2, 1, 2, 1, 2,
                                    1, 2, 1, 2, 1, 2, 1, 2, 1, 2))
  trained <- textTrainRandomForest(wordembeddings4$harmonytext,
                       example_categories,
                       #outside_strata_y = NULL,
                       #inside_strata_y = NULL,
                       mtry = c(1),
                       min_n = c(1),
                       trees = c(1000),
                       preprocess_PCA_thresh = c(0.95),
                       extremely_randomised_splitrule = "gini",
                       multi_cores = TRUE,
                       eval_measure = "roc_auc") #sens bal_accuracy f_measure

  testthat::expect_that(trained, testthat::is_a("list"))
  testthat::expect_is(trained$truth_predictions$truth[1], "factor")
})




test_that("textTrainLists regression produces a list of results with prediction being numeric", {
  wordembeddings <- wordembeddings4[1:2]
  ratings_data <- Language_based_assessment_data_8[5:6]
  results <- textTrainLists(wordembeddings,
                            ratings_data,
                            preprocess_PCA_thresh = c(0.95),
                            #outside_strata_y = NULL,
                            #inside_strata_y = NULL,
                            penalty = c(1),
                            mixture = c(0))

  testthat::expect_that(results, testthat::is_a("list"))
  testthat::expect_is(results$predictions$harmonywords_hilstotal_pred[1], "numeric")
})

test_that("textTrainLists randomForest produces list of results with prediction being numeric", {
  wordembeddings <- wordembeddings4[1:2]
  ratings_data1 <- factor(rep(c("young", "old", "young", "old", "young", "old", "young", "old", "young", "old"), 4))
  ratings_data2 <- factor(rep(c("young", "old", "young", "old", "young", "old", "young", "old", "young", "old"), 4))
  ratings_data <- tibble(ratings_data1, ratings_data2)
  results_rf <- textTrainLists(wordembeddings,
                            ratings_data,
                            force_train_method = "random_forest",
                            #outside_strata_y = NULL,
                            #inside_strata_y = NULL,
                            mtry = c(2),
                            min_n = c(1),
                            preprocess_PCA_thresh = c(0.95),
                            trees = c(1000),
                            eval_measure = "accuracy")

  testthat::expect_that(results_rf, testthat::is_a("list"))
  testthat::expect_is(results_rf$results$p_value[1], "character")
})



#test_that("textTrainMultiTexts produces list of results with prediction being numeric", {
#  wordembeddings <- wordembeddings4[1:2]
#  ratings_data <- Language_based_assessment_data_8$hilstotal
#  results <- textTrainMultiTexts(wordembeddings,
#                                 ratings_data)

#  expect_that(results, is_a("list"))
#  expect_is(results$prediction$.pred[1], "numeric")
#})

#test_that("textTrainMultiTexts produces list of results with prediction being numeric", {
#  wordembeddings <- wordembeddings4
#  ratings_data <- Language_based_assessment_data_8
#  results <- textTrainRandomForest(wordembeddings$harmonytext, ratings_data$gender, nrFolds_k = 5, trees = 5)
#  expect_that(results, is_a("list"))
#  expect_is(results$prediction$.pred_1[1], "numeric")
#})






