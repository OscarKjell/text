#.rs.restartR()
library(testthat)
library(text)
library(tibble)
library(dplyr)


context("Training Functions")

test_that("textTrain Regression produces list of results with prediction being numeric", {


  trained_min_halving <- textTrainRegression(wordembeddings4[1],
                                   Language_based_assessment_data_8[6],
                                   #outside_strata_y = NULL,
                                   #inside_strata_y = NULL,
                                   penalty = c(1),
                                   mixture = c(0),
                                   preprocess_PCA = "min_halving",
                                   multi_cores = TRUE,
                                   #force_train_method = "automatic",
                                   save_output = "only_results"
  )

#  trained_min_halving <- textTrain(wordembeddings4$harmonytext,
#                       Language_based_assessment_data_8$hilstotal,
#                       #outside_strata_y = NULL,
#                       #inside_strata_y = NULL,
#                       penalty = c(1),
#                       mixture = c(0),
#                       preprocess_PCA = "min_halving",
#                       multi_cores = TRUE,
#                       force_train_method = "automatic",
#                       save_output = "only_results"
#  )
  # print(object.size(trained_min_halving), units = "b")              # 177 136 bytes; 152 168 bytes; 173 752 bytes
  # print(object.size(trained_min_halving$final_recipe), units = "b") # 45 384  bytes;  20 432 bytes;  42 016  bytes

  cat(text:::colourise("text", fg = "blue", bg = "light gray"))

  #warnings()
  testthat::expect_that(trained_min_halving, is_a("list"))
  testthat::expect_is(trained_min_halving$correlation$statistic[[1]], "numeric")

  trained_1 <- textTrain(wordembeddings4$harmonytext,
                       Language_based_assessment_data_8$hilstotal,
                       #outside_strata_y = NULL,
                       #inside_strata_y = NULL,
                       penalty = c(1),
                       mixture = c(0),
                       preprocess_PCA = c(1), #, 3
                       multi_cores = FALSE,
                       force_train_method = "regression",
                       save_output = "only_results_predictions"
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

  example_categories <- as.factor(c(1, NA, 1, 2, 1, 2, 1, 2, 1, 2,
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
                                   eval_measure = "roc_auc", #sens bal_accuracy f_measure
                                   save_output = "only_results"
                                   )

  testthat::expect_that(trained, testthat::is_a("list"))
  testthat::expect_is(trained$results$.estimate[1], "numeric")

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
                                   preprocess_PCA = c(3),
                                   extremely_randomised_splitrule = "gini",
                                   multi_cores = TRUE,
                                   eval_measure = "bal_accuracy",
                                   save_output = "only_results_predictions") #sens bal_accuracy f_measure

  testthat::expect_that(trained, testthat::is_a("list"))
  testthat::expect_is(trained$truth_predictions$truth[1], "factor")
  example_categories_tibble <- tibble::as_tibble_col(example_categories)
  trained <- textTrainRandomForest(wordembeddings4[1],
                                   example_categories_tibble,
                                   #outside_strata_y = NULL,
                                   #inside_strata_y = NULL,
                                   mtry = c(1),
                                   min_n = c(1),
                                   trees = c(1000),
                                   preprocess_PCA = NA,
                                   extremely_randomised_splitrule = "gini",
                                   multi_cores = TRUE) #sens bal_accuracy f_measure

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
                       eval_measure = "f_measure",
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
                        force_train_method = "random_forest",
                        mtry = c(1),
                        min_n = c(1),
                        trees = c(1000),
                        preprocess_PCA = NA,
                        multi_cores = FALSE,
                        eval_measure = "spec") #sens bal_accuracy f_measure

  testthat::expect_that(trained_NA, testthat::is_a("list"))
  testthat::expect_is(trained_NA$truth_predictions$truth[1], "factor")
})


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
                            force_train_method = "regression",
                            method_cor = "spearman")

  testthat::expect_that(results, testthat::is_a("list"))
  testthat::expect_is(results$results$rho_correlation[1], "character")


  results_or <- textTrainLists(wordembeddings,
                            ratings_data,
                            preprocess_PCA = c(0.90),
                            #outside_strata_y = NULL,
                            #inside_strata_y = NULL,
                            penalty = c(2),
                            mixture = c(0),
                            force_train_method = "regression",
                            save_output = "only_results",
                            method_cor = "kendall")

  testthat::expect_that(results_or, testthat::is_a("list"))
  testthat::expect_is(results_or$results$tau_correlation[1], "character")

  results_or_p <- textTrainLists(wordembeddings,
                               ratings_data,
                               preprocess_PCA = c(0.90),
                               #outside_strata_y = NULL,
                               #inside_strata_y = NULL,
                               penalty = c(2),
                               mixture = c(0),
                               force_train_method = "regression",
                               save_output = "only_results_predictions")

  testthat::expect_that(results_or_p, testthat::is_a("list"))
  testthat::expect_is(results_or_p$results$correlation[1], "character")

})



test_that("textTrainLists randomForest produces list of results with prediction being numeric", {
  x <- wordembeddings4[1]
  #x <- wordembeddings4$harmonywords
  #x <- wordembeddings4[1:2]

  y1 <- factor(rep(c("young", "old", "young", "old", "young", "old", "young", "old", "young", "old"), 4))
  y2 <- factor(rep(c("young", "old", "young", "old", "young", "old", "young", "old", "young", "old"), 4))
  y <- tibble::tibble(y1, y2)
  #y <- tibble::as_tibble_col(y1)


  results_rf_et <- textTrain(x,
                          y,
                          force_train_method = "automatic",
                          mtry = c(1),
                          min_n = c(1),
                          preprocess_PCA = c(0.95),
                          trees = c(1000),
                          eval_measure = "accuracy",
                          extremely_randomised_splitrule = "extratrees",
                          save_output = "all"
  )

  testthat::expect_that(results_rf_et, testthat::is_a("list"))
  testthat::expect_is(results_rf_et$results$p_value[1], "character")


  results_rf <- textTrain(x,
                          y,
                          force_train_method = "automatic",
                          mtry = c(1),
                          min_n = c(1),
                          preprocess_PCA = NA,
                          trees = c(1000),
                          eval_measure = "kappa",
                          save_output = "all"
                          )

  testthat::expect_that(results_rf, testthat::is_a("list"))
  testthat::expect_is(results_rf$results$p_value[1], "character")

  results_rf_or_p <- textTrain(x,
                               y,
                               #force_train_method = "random_forest",
                               mtry = c(1),
                               min_n = c(1),
                               preprocess_PCA = c(0.95),
                               trees = c(1000),
                               eval_measure = "precision",
                               save_output = "only_results_predictions")

  testthat::expect_that(results_rf_or_p, testthat::is_a("list"))
  testthat::expect_is(results_rf_or_p$results$p_value[1], "character")


  results_rf_or <- textTrain(x,
                          y,
                          #force_train_method = "random_forest",
                          mtry = c(1),
                          min_n = c(1),
                          preprocess_PCA = c(0.95),
                          trees = c(1000),
                          eval_measure = "precision",
                          save_output = "only_results")

  testthat::expect_that(results_rf_or, testthat::is_a("list"))
  testthat::expect_is(results_rf_or$results$p_value[1], "character")


})







