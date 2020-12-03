library(testthat)
library(text)
library(tibble)
library(dplyr)

context("Training Functions")

test_that("textTrain Regression produces list of results with prediction being numeric", {
  skip_on_cran()

  trained_min_halving <- textTrainRegression(wordembeddings4[1],
    Language_based_assessment_data_8[6],
    cv_method = "cv_folds",
    outside_folds = 2,
    inside_folds = 2,
    outside_strata_y = NULL,
    inside_strata_y = NULL,
    model = "regression",
    eval_measure = "rmse",
    penalty = c(1),
    mixture = c(0),
    preprocess_PCA = 1,
    multi_cores = FALSE,
    # force_train_method = "automatic",
    save_output = "only_results"
  )

  testthat::expect_that(trained_min_halving, is_a("list"))
  testthat::expect_is(trained_min_halving$results$statistic[[1]], "numeric")


  trained_logistic <- textTrainRegression(
    x = wordembeddings4[1],
    y = as.factor(Language_based_assessment_data_8$gender),
    cv_method = "validation_split",
    outside_folds = 2,
    inside_folds = 3/4,
    outside_strata_y = NULL,
    inside_strata_y = NULL,
    model = "logistic",
    eval_measure = "bal_accuracy",
    penalty = c(1),
    mixture = c(0),
    preprocess_PCA = "min_halving",
    multi_cores = "multi_cores_sys_default",
    save_output = "only_results"
  )
  testthat::expect_that(trained_logistic, is_a("list"))
  testthat::expect_is(trained_logistic$results_metrics$.estimate[[1]], "numeric")


  trained_logistic2 <- textTrainRegression(
    x = wordembeddings4[1],
    y = as.factor(Language_based_assessment_data_8$gender),
    cv_method = "cv_folds",
    outside_folds = 2,
    inside_folds = 2,
    outside_strata_y = NULL,
    inside_strata_y = NULL,
    model = "logistic",
    eval_measure = "accuracy",
    penalty = c(1),
    mixture = c(0),
    preprocess_PCA = 1,
    multi_cores = "multi_cores_sys_default",
    save_output = "only_results_predictions"
  )
  testthat::expect_that(trained_logistic2, is_a("list"))
  testthat::expect_is(trained_logistic2$results_metrics$.estimate[[1]], "numeric")



  # testing with one component; and thus a standard logistic.
  trained_logistic_PCA1 <- textTrainRegression(wordembeddings4[1],
    as.factor(Language_based_assessment_data_8$gender),
    outside_folds = 2,
    inside_folds = 2,
    outside_strata_y = NULL,
    inside_strata_y = NULL,
    model = "logistic",
    eval_measure = "precision",
    penalty = c(1),
    mixture = c(0),
    preprocess_PCA = 1,
    multi_cores = "multi_cores_sys_default",
    # force_train_method = "automatic",
    save_output = "all"
  )
  testthat::expect_that(trained_logistic_PCA1, is_a("list"))
  testthat::expect_is(trained_logistic_PCA1$results_metrics$.estimate[[1]], "numeric")

  predict_list_form <- text::textPredict(trained_logistic_PCA1, wordembeddings4[1])
  testthat::expect_is(predict_list_form$.pred_class[[1]], "factor")

  trained_1 <- textTrain(wordembeddings4$harmonytext,
    Language_based_assessment_data_8$hilstotal,
    outside_folds = 2,
    inside_folds = 2,
    outside_strata_y = NULL,
    inside_strata_y = NULL,
    penalty = c(1),
    mixture = c(0),
    preprocess_PCA = NA,
    multi_cores = "multi_cores_sys_default",
    force_train_method = "regression",
    save_output = "only_results_predictions"
  )

  testthat::expect_that(trained_1, is_a("list"))
  testthat::expect_is(trained_1$prediction$predictions[1], "numeric")

  trained_NA <- textTrain(wordembeddings4$harmonytext,
    Language_based_assessment_data_8$hilstotal,
    outside_folds = 2,
    inside_folds = 2,
    outside_strata_y = NULL,
    inside_strata_y = NULL,
    penalty = c(1),
    mixture = c(0),
    preprocess_PCA = NA,
    multi_cores = "multi_cores_sys_default"
  )

  testthat::expect_that(trained_NA, is_a("list"))
  testthat::expect_is(trained_NA$prediction$predictions[1], "numeric")
})



test_that("textTrain Random Forest produces list of results with prediction being categorical", {
  skip_on_cran()


  example_categories <- as.factor(c(
    1, 2, 1, 2, 1, 2, 1, 2, 1, 2,
    1, 2, 1, 2, 1, 2, 1, 2, 1, 2,
    1, 2, 1, 2, 1, 2, 1, 2, 1, 2,
    1, 2, 1, 2, 1, 2, 1, 2, 1, 2
  ))
  trained1 <- textTrain(wordembeddings4$harmonytext,
    example_categories,
    cv_method = "validation_split",
    outside_folds = 2,
    inside_folds = 3/4,
    outside_strata_y = NULL,
    inside_strata_y = NULL,
    mtry = c(1),
    min_n = c(1),
    trees = c(1000),
    preprocess_PCA = "min_halving",
    multi_cores = "multi_cores_sys_default",
    eval_measure = "f_measure",
    force_train_method = "random_forest"
  )

  testthat::expect_that(trained1, testthat::is_a("list"))
  testthat::expect_is(trained1$truth_predictions$truth[1], "factor")

  trained2 <- textTrain(wordembeddings4$harmonytext,
    example_categories,
    outside_folds = 2,
    inside_folds = 2,
    outside_strata_y = NULL,
    inside_strata_y = NULL,
    mtry = c(1),
    min_n = c(1),
    trees = c(1000),
    preprocess_PCA = 2,
    multi_cores = "multi_cores_sys_default",
    eval_measure = "sens",
    force_train_method = "random_forest"
  )

  testthat::expect_that(trained2, testthat::is_a("list"))
  testthat::expect_is(trained2$truth_predictions$truth[1], "factor")

  trained_NA <- textTrain(wordembeddings4$harmonytext,
    example_categories,
    outside_folds = 2,
    inside_folds = 2,
    outside_strata_y = NULL,
    inside_strata_y = NULL,
    force_train_method = "random_forest",
    mtry = c(1),
    min_n = c(1),
    trees = c(1000),
    preprocess_PCA = NA,
    multi_cores = "multi_cores_sys_default",
    eval_measure = "spec"
  )

  testthat::expect_that(trained_NA, testthat::is_a("list"))
  testthat::expect_is(trained_NA$truth_predictions$truth[1], "factor")
})



test_that("textTrainRandomForest with Extremely Randomized Trees produces list of results with prediction being categorical", {
  skip_on_cran()

  example_categories <- as.factor(c(
    1, NA, 1, 2, 1, 2, 1, 2, 1, 2,
    1, 2, 1, 2, 1, 2, 1, 2, 1, 2,
    1, 2, 1, 2, 1, 2, 1, 2, 1, 2,
    1, 2, 1, 2, 1, 2, 1, 2, 1, 2
  ))
  trained_rf_95 <- textTrainRandomForest(
    x = wordembeddings4$harmonytext,
    y = example_categories,
    outside_folds = 2,
    inside_folds = 2,
    outside_strata_y = NULL,
    inside_strata_y = NULL,
    mode_rf = "classification",
    mtry = c(1),
    min_n = c(1),
    trees = c(1000),
    preprocess_PCA = c(0.95),
    extremely_randomised_splitrule = NULL,
    multi_cores = "multi_cores_sys_default",
    eval_measure = "roc_auc",
    save_output = "only_results",
    event_level = "second"
  )

  testthat::expect_that(trained_rf_95, testthat::is_a("list"))
  testthat::expect_is(trained_rf_95$results$.estimate[1], "numeric")

  example_categories <- as.factor(c(
    1, 2, 1, 2, 1, 2, 1, 2, 1, 2,
    1, 2, 1, 2, 1, 2, 1, 2, 1, 2,
    1, 2, 1, 2, 1, 2, 1, 2, 1, 2,
    1, 2, 1, 2, 1, 2, 1, 2, 1, 2
  ))
  trained_rf_3 <- textTrainRandomForest(wordembeddings4$harmonytext,
    example_categories,
    outside_folds = 2,
    inside_folds = 2,
    outside_strata_y = NULL,
    inside_strata_y = NULL,
    mtry = c(1),
    min_n = c(1),
    trees = c(1000),
    preprocess_PCA = c(3),
    extremely_randomised_splitrule = "gini",
    multi_cores = FALSE,
    eval_measure = "kappa",
    save_output = "only_results_predictions"
  )

  testthat::expect_that(trained_rf_3, testthat::is_a("list"))
  testthat::expect_is(trained_rf_3$truth_predictions$truth[1], "factor")

  example_categories_tibble <- tibble::as_tibble_col(example_categories)
  trained_rf_NA <- textTrainRandomForest(wordembeddings4[1],
    example_categories_tibble,
    outside_folds = 2,
    inside_folds = 2,
    outside_strata_y = NULL,
    inside_strata_y = NULL,
    mtry = c(1),
    min_n = c(1),
    trees = c(1000),
    preprocess_PCA = NA,
    extremely_randomised_splitrule = "gini",
    multi_cores = "multi_cores_sys_default"
  )

  testthat::expect_that(trained_rf_NA, testthat::is_a("list"))
  testthat::expect_is(trained_rf_NA$truth_predictions$truth[1], "factor")
})



test_that("textTrainLists Regression produces a list of results with prediction being numeric", {
  skip_on_cran()

  # Two word embeddings and one vector
  results <- textTrain(wordembeddings4[1:2],
    Language_based_assessment_data_8$hilstotal,
    outside_folds = 2,
    inside_folds = 2,
    outside_strata_y = NULL,
    inside_strata_y = NULL,
    preprocess_PCA = c(0.90),
    model = "regression",
    penalty = c(2),
    mixture = c(0),
    force_train_method = "regression",
    method_cor = "spearman",
    multi_cores = "multi_cores_sys_default"
  )

  testthat::expect_that(results, testthat::is_a("list"))
  testthat::expect_is(results$results$rho_correlation[1], "character")

  # One word embedding and two rating scales
  results_or <- textTrainLists(wordembeddings4$harmonywords,
    Language_based_assessment_data_8[5:6],
    outside_folds = 2,
    inside_folds = 2,
    outside_strata_y = NULL,
    inside_strata_y = NULL,
    preprocess_PCA = c(0.90),
    # outside_strata_y = NULL,
    # inside_strata_y = NULL,
    penalty = c(2),
    mixture = c(0),
    force_train_method = "regression",
    save_output = "only_results",
    method_cor = "kendall",
    multi_cores = "multi_cores_sys_default"
  )

  testthat::expect_that(results_or, testthat::is_a("list"))
  testthat::expect_is(results_or$results$tau_correlation[1], "character")

  wordembeddings <- wordembeddings4[1]
  ratings_data1 <- Language_based_assessment_data_8[5]
  ratings_data2 <- Language_based_assessment_data_8[6]
  factors1 <- tibble::as_tibble_col(as.factor(Language_based_assessment_data_8$gender))
  ratings_data <- cbind(ratings_data1, ratings_data2, factors1)


  results_or_p <- textTrainLists(wordembeddings,
    ratings_data,
    preprocess_PCA = c(0.90),
    outside_folds = 2,
    inside_folds = 2,
    outside_strata_y = NULL,
    inside_strata_y = NULL,
    penalty = c(2),
    mixture = c(0),
    force_train_method = "automatic",
    save_output = "only_results_predictions",
    multi_cores = "multi_cores_sys_default"
  )

  testthat::expect_that(results_or_p, testthat::is_a("list"))
  testthat::expect_is(results_or_p$results$correlation[1], "character")

  # FORCE RANDOM FORREST EVen though categorical variables are not most present
  results_or_p <- textTrain(x = wordembeddings,
                            y = ratings_data,
                            preprocess_PCA = c(0.90),
                            outside_folds = 2,
                            inside_folds = 2,
                            outside_strata_y = NULL,
                            inside_strata_y = NULL,
                            penalty = c(2),
                            mixture = c(0),
                            force_train_method = "random_forest",
                            save_output = "only_results_predictions",
                            multi_cores = "multi_cores_sys_default"
  )

  testthat::expect_that(results_or_p, testthat::is_a("list"))
  testthat::expect_is(results_or_p$results$.estimate[1], "numeric")


  factors1 <- as.factor(Language_based_assessment_data_8$gender)
  factors2 <- as.factor(Language_based_assessment_data_8$gender)
  rating1 <- Language_based_assessment_data_8$hilstotal

  ratings_data_factors <- tibble::tibble(factors1, factors2, rating1)

  # Logistic
  results_list_logistic <- textTrainLists(wordembeddings,
    ratings_data_factors,
    preprocess_PCA = c(0.90),
    outside_folds = 2,
    inside_folds = 2,
    outside_strata_y = NULL,
    inside_strata_y = NULL,
    penalty = c(2),
    mixture = c(0),
    force_train_method = "automatic",
    model = "logistic",
    eval_measure = "default",
    save_output = "only_results_predictions",
    multi_cores = "multi_cores_sys_default"
  )

  testthat::expect_that(results_or_p, testthat::is_a("list"))
  testthat::expect_is(results_or_p$results$.estimate[1], "numeric")


  results_list_logistic <- textTrain(wordembeddings,
                                          ratings_data_factors,
                                          preprocess_PCA = c(0.90),
                                          outside_folds = 2,
                                          inside_folds = 2,
                                          outside_strata_y = NULL,
                                          inside_strata_y = NULL,
                                          penalty = c(2),
                                          mixture = c(0),
                                          force_train_method = "regression",
                                          save_output = "only_results_predictions",
                                          multi_cores = "multi_cores_sys_default"
  )

  testthat::expect_that(results_or_p, testthat::is_a("list"))
  testthat::expect_is(results_or_p$results$.estimate[1], "numeric")



})


test_that("textTrainLists randomForest produces list of results with prediction being numeric", {
  skip_on_cran()

  x <- wordembeddings4[1]

  y1 <- factor(rep(c("young", "old", "young", "old", "young", "old", "young", "old", "young", "old"), 4))
  y2 <- factor(rep(c("young", "old", "young", "old", "young", "old", "young", "old", "young", "old"), 4))
  y <- tibble::tibble(y1, y2)

  results_rf_et <- textTrain(x,
    y,
    force_train_method = "automatic",
    outside_folds = 2,
    inside_folds = 2,
    outside_strata_y = NULL,
    inside_strata_y = NULL,
    mtry = c(1),
    min_n = c(1),
    preprocess_PCA = c(0.95),
    trees = c(1000),
    eval_measure = "accuracy",
    extremely_randomised_splitrule = "extratrees",
    save_output = "all",
    multi_cores = "multi_cores_sys_default"
  )

  testthat::expect_that(results_rf_et, testthat::is_a("list"))
  testthat::expect_is(results_rf_et$results$p_value[1], "character")

  results_rf <- textTrain(x,
    y,
    force_train_method = "automatic",
    outside_folds = 2,
    inside_folds = 2,
    outside_strata_y = NULL,
    inside_strata_y = NULL,
    mtry = c(1),
    min_n = c(1),
    preprocess_PCA = NA,
    trees = c(1000),
    eval_measure = "kappa",
    save_output = "all",
    multi_cores = "multi_cores_sys_default"
  )

  testthat::expect_that(results_rf, testthat::is_a("list"))
  testthat::expect_is(results_rf$results$p_value[1], "character")

  results_rf_or_p <- textTrain(x,
    y,
    # force_train_method = "random_forest",
    outside_folds = 2,
    inside_folds = 2,
    outside_strata_y = NULL,
    inside_strata_y = NULL,
    mtry = c(1),
    min_n = c(1),
    preprocess_PCA = c(0.95),
    trees = c(1000),
    eval_measure = "precision",
    save_output = "only_results_predictions",
    multi_cores = "multi_cores_sys_default"
  )

  testthat::expect_that(results_rf_or_p, testthat::is_a("list"))
  testthat::expect_is(results_rf_or_p$results$p_value[1], "character")


  results_rf_or <- textTrain(x,
    y,
    # force_train_method = "random_forest",
    outside_folds = 2,
    inside_folds = 2,
    outside_strata_y = NULL,
    inside_strata_y = NULL,
    mtry = c(1),
    min_n = c(1),
    preprocess_PCA = c(0.95),
    trees = c(1000),
    eval_measure = "precision",
    save_output = "only_results",
    multi_cores = "multi_cores_sys_default"
  )

  testthat::expect_that(results_rf_or, testthat::is_a("list"))
  testthat::expect_is(results_rf_or$results$p_value[1], "character")
})




test_that("textTrainRegression adding wordembeddings together", {
  skip_on_cran()

  multi_we_PCA_09 <- textTrainRegression(wordembeddings4[1:2],
    Language_based_assessment_data_8$hilstotal,
    outside_folds = 2,
    inside_folds = 2,
    outside_strata_y = NULL,
    inside_strata_y = NULL,
    preprocess_PCA = c(0.9),
    penalty = 1,
    multi_cores = "multi_cores_sys_default"
  )

  testthat::expect_that(multi_we_PCA_09, testthat::is_a("list"))
  testthat::expect_is(multi_we_PCA_09$results[[1]][[1]], "numeric")


  # Prediction based on multiple we
  predictions_multi <- text::textPredict(multi_we_PCA_09, wordembeddings4[1:2])
  testthat::expect_is(predictions_multi$.pred[[1]], "numeric")


  multi_we_PCA_3 <- textTrainRegression(wordembeddings4[1:2],
    Language_based_assessment_data_8$hilstotal,
    outside_folds = 2,
    inside_folds = 2,
    outside_strata_y = NULL,
    inside_strata_y = NULL,
    preprocess_PCA = 3,
    penalty = 1,
    multi_cores = "multi_cores_sys_default"
  )

  testthat::expect_that(multi_we_PCA_3, testthat::is_a("list"))
  testthat::expect_is(multi_we_PCA_3$results[[1]][[1]], "numeric")


  multi_we_PCA_NA <- textTrainRegression(wordembeddings4[1:2],
    Language_based_assessment_data_8$hilstotal,
    outside_folds = 2,
    inside_folds = 2,
    outside_strata_y = NULL,
    inside_strata_y = NULL,
    preprocess_PCA = NA,
    penalty = 1,
    multi_cores = "multi_cores_sys_default"
  )

  testthat::expect_that(multi_we_PCA_NA, testthat::is_a("list"))
  testthat::expect_is(multi_we_PCA_NA$results[[1]][[1]], "numeric")
})


test_that("textTrainRandomForest adding wordembeddings together", {
  y <- as.factor(c(1, 2, 1, 2, 1, 2, 1, 2, 1, 2))

  multi_we_RF_PCA_09 <- textTrainRandomForest(wordembeddings4[1:2],
    y,
    outside_folds = 2,
    inside_folds = 2,
    outside_strata_y = NULL,
    inside_strata_y = NULL,
    preprocess_PCA = 0.9,
    mtry = c(1),
    min_n = c(1),
    multi_cores = "multi_cores_sys_default"
  )

  testthat::expect_that(multi_we_RF_PCA_09, testthat::is_a("list"))
  testthat::expect_is(multi_we_RF_PCA_09$results$.estimate[[1]], "numeric")


  multi_we_RF_PCA_3 <- textTrainRandomForest(wordembeddings4[1:2],
    y,
    outside_folds = 2,
    inside_folds = 2,
    outside_strata_y = NULL,
    inside_strata_y = NULL,
    preprocess_PCA = 3,
    mtry = c(1),
    min_n = c(1),
    multi_cores = "multi_cores_sys_default"
  )

  testthat::expect_that(multi_we_RF_PCA_3, testthat::is_a("list"))
  testthat::expect_is(multi_we_RF_PCA_3$results$.estimate[[1]], "numeric")


  multi_we_RF_PCA_NA <- textTrainRandomForest(wordembeddings4[1:2],
    y,
    outside_folds = 2,
    inside_folds = 2,
    outside_strata_y = NULL,
    inside_strata_y = NULL,
    preprocess_PCA = NA,
    mtry = c(1),
    min_n = c(1),
    multi_cores = "multi_cores_sys_default"
  )


  testthat::expect_that(multi_we_RF_PCA_NA, testthat::is_a("list"))
  testthat::expect_is(multi_we_RF_PCA_NA$results$.estimate[[1]], "numeric")
})
