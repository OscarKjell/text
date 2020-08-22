#.rs.restartR()
library(testthat)
library(text)
library(tibble)
library(dplyr)


context("Training Functions")

test_that("textTrain Regression produces list of results with prediction being numeric", {

  trained_min_halving <- textTrain(wordembeddings4$harmonytext,
                       Language_based_assessment_data_8$hilstotal,
                       #outside_strata_y = NULL,
                       #inside_strata_y = NULL,
                       penalty = c(1),
                       mixture = c(0),
                       preprocess_PCA = "min_halving",
                       multi_cores = TRUE
  )

  #warnings()
  testthat::expect_that(trained_min_halving, is_a("list"))
  testthat::expect_is(trained_min_halving$prediction$predictions[1], "numeric")
  rlang::last_trace()

  trained_1 <- textTrain(wordembeddings4$harmonytext,
                       Language_based_assessment_data_8$hilstotal,
                       #outside_strata_y = NULL,
                       #inside_strata_y = NULL,
                       penalty = c(1),
                       mixture = c(0),
                       preprocess_PCA = c(1), #, 3
                       multi_cores = FALSE
  )

  #warnings()
  testthat::expect_that(trained_1, is_a("list"))
  testthat::expect_is(trained_1$prediction$predictions[1], "numeric")

  trained_NA <- textTrain(wordembeddings4$harmonytext,
                       Language_based_assessment_data_8$hilstotal,
                       #outside_strata_y = NULL,
                       #inside_strata_y = NULL,
                       penalty = c(1),
                       mixture = c(0),
                       preprocess_PCA = NA,
                       multi_cores = TRUE
  )

  #warnings()
  testthat::expect_that(trained_NA, is_a("list"))
  testthat::expect_is(trained_NA$prediction$predictions[1], "numeric")


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
                                   mode_rf = "classification",
                                   mtry = c(1),
                                   min_n = c(1),
                                   trees = c(1000),
                                   preprocess_PCA = c(0.95),
                                   extremely_randomised_splitrule = NULL,
                                   multi_cores = TRUE,
                                   eval_measure = "roc_auc") #sens bal_accuracy f_measure

  testthat::expect_that(trained, testthat::is_a("list"))
  testthat::expect_is(trained$truth_predictions$truth[1], "factor")

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
                                   trees = c(1000, 1001, 999),
                                   preprocess_PCA = c(3, 4),
                                   extremely_randomised_splitrule = "gini",
                                   multi_cores = TRUE,
                                   eval_measure = "bal_accuracy") #sens bal_accuracy f_measure

  testthat::expect_that(trained, testthat::is_a("list"))
  testthat::expect_is(trained$truth_predictions$truth[1], "factor")
})

test_that("textTrain Random Forest produces list of results with prediction being categorical", {

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
                       preprocess_PCA = "min_halving",
                       multi_cores = FALSE,
                       eval_measure = "roc_auc",
                       force_train_method = "random_forest") #sens bal_accuracy f_measure

  testthat::expect_that(trained, testthat::is_a("list"))
  testthat::expect_is(trained$truth_predictions$truth[1], "factor")

  trained2 <- textTrain(wordembeddings4$harmonytext,
                       example_categories,
                       #outside_strata_y = NULL,
                       #inside_strata_y = NULL,
                       mtry = c(1),
                       min_n = c(1),
                       trees = c(1000),
                       preprocess_PCA = 2,
                       multi_cores = FALSE,
                       eval_measure = "sens",
                       force_train_method = "random_forest") #sens bal_accuracy f_measure

  testthat::expect_that(trained2, testthat::is_a("list"))
  testthat::expect_is(trained2$truth_predictions$truth[1], "factor")

  trained_NA <- textTrain(wordembeddings4$harmonytext,
                        example_categories,
                        #outside_strata_y = NULL,
                        #inside_strata_y = NULL,
                        mtry = c(1),
                        min_n = c(1),
                        trees = c(1000),
                        preprocess_PCA = NA,
                        multi_cores = FALSE,
                        eval_measure = "spec",
                        force_train_method = "random_forest") #sens bal_accuracy f_measure

  testthat::expect_that(trained_NA, testthat::is_a("list"))
  testthat::expect_is(trained_NA$truth_predictions$truth[1], "factor")
})


#sessionInfo()
test_that("textTrainLists Regression produces a list of results with prediction being numeric", {
  wordembeddings <- wordembeddings4[1]
  ratings_data <- Language_based_assessment_data_8[5:6]
  results <- textTrainLists(wordembeddings,
                            ratings_data,
                            preprocess_PCA = c(0.90),
                            #outside_strata_y = NULL,
                            #inside_strata_y = NULL,
                            penalty = c(2),
                            mixture = c(0),
                            force_train_method = "regression")

  testthat::expect_that(results, testthat::is_a("list"))
  testthat::expect_is(results$results$correlation[1], "character")
})

test_that("textTrainLists randomForest produces list of results with prediction being numeric", {
  x <- wordembeddings4[1]
  #x <- wordembeddings4[1:2]

  y1 <- factor(rep(c("young", "old", "young", "old", "young", "old", "young", "old", "young", "old"), 4))
  y2 <- factor(rep(c("young", "old", "young", "old", "young", "old", "young", "old", "young", "old"), 4))
  y <- tibble::tibble(y1, y2)
  results_rf <- textTrainLists(x,
                               y,
                               force_train_method = "random_forest",
                               mtry = c(1),
                               min_n = c(1),
                               preprocess_PCA = c(0.95),
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






