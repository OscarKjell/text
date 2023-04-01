library(testthat)
library(text)
library(tibble)
library(dplyr)

context("Training Functions")

test_that("textTrain Regression produces list of results with prediction being numeric", {
  skip_on_cran()

  trained_min_halving <- text::textTrainRegression(
    x = word_embeddings_4$texts["harmonywords"],
    y = Language_based_assessment_data_8[6],
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
  testthat::expect_equal(trained_min_halving$results$statistic[[1]], 0.2979104, tolerance = 0.00001)

  trained_logistic <- text::textTrainRegression(
    x = word_embeddings_4$texts["harmonywords"],
    y = as.factor(Language_based_assessment_data_8$gender),
    cv_method = "validation_split",
    outside_folds = 2,
    inside_folds = 3 / 4,
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
  testthat::expect_equal(trained_logistic$results_metrics$.estimate[[1]], 0.475)

  trained_logistic2 <- text::textTrainRegression(
    x = word_embeddings_4$texts[1],
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
  testthat::expect_equal(trained_logistic2$results_metrics$.estimate[[1]], 0.525)


  # testing with one component; and thus a standard logistic.
  trained_logistic_PCA1 <- text::textTrainRegression(
    x = word_embeddings_4$texts[1],
    y = as.factor(Language_based_assessment_data_8$gender),
    outside_folds = 2,
    # inside_folds = 2,
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
  testthat::expect_equal(trained_logistic_PCA1$results_metrics$.estimate[[1]], 0.525)

  predict_list_form <- text::textPredict(trained_logistic_PCA1,
    word_embeddings_4$texts[1],
    dim_names = TRUE
  )
  testthat::expect_is(predict_list_form[[1]][1], "factor")

  trained_1 <- text::textTrain(
    x = word_embeddings_4$texts$harmonytext,
    y = Language_based_assessment_data_8$hilstotal,
    outside_folds = 2,
    inside_folds = 2 / 3,
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
  testthat::expect_equal(trained_1$prediction$predictions[1], 28.5811, tolerance = 0.001)


  trained_NA <- text::textTrain(
    x = word_embeddings_4$texts$harmonytext,
    y = Language_based_assessment_data_8$hilstotal,
    outside_folds = 2,
    inside_folds = 2 / 3,
    outside_strata_y = NULL,
    inside_strata_y = NULL,
    penalty = c(1),
    mixture = c(0),
    preprocess_PCA = NA,
    multi_cores = "multi_cores_sys_default"
  )

  testthat::expect_that(trained_NA, is_a("list"))
  testthat::expect_is(trained_NA$predictions$predictions[1], "numeric")
  testthat::expect_equal(trained_NA$predictions$predictions[1], 28.5811, tolerance = 0.001)
})



test_that("textTrain Random Forest produces list of results with prediction being categorical", {
  skip_on_cran()


  example_categories <- as.factor(c(
    1, 2, 1, 2, 1, 2, 1, 2, 1, 2,
    1, 2, 1, 2, 1, 2, 1, 2, 1, 2,
    1, 2, 1, 2, 1, 2, 1, 2, 1, 2,
    1, 2, 1, 2, 1, 2, 1, 2, 1, 2
  ))
  trained1 <- text::textTrain(
    x = word_embeddings_4$texts$harmonytext,
    y = example_categories,
    cv_method = "validation_split",
    outside_folds = 2,
    inside_folds = 3 / 4,
    outside_strata_y = NULL,
    inside_strata_y = NULL,
    mtry = c(1),
    min_n = c(1),
    trees = c(1000),
    preprocess_PCA = "min_halving",
    multi_cores = FALSE,
    eval_measure = "f_measure",
    force_train_method = "random_forest"
  )

  testthat::expect_that(trained1, testthat::is_a("list"))
  testthat::expect_is(trained1$truth_predictions$truth[1], "factor")
  testthat::expect_equal(trained1$truth_predictions$.pred_1[1], 0.297)

  trained2 <- text::textTrain(
    x = word_embeddings_4$texts$harmonytext,
    y = example_categories,
    outside_folds = 2,
    inside_folds = 3 / 4,
    outside_strata_y = NULL,
    inside_strata_y = NULL,
    mtry = c(1),
    min_n = c(1),
    trees = c(1000),
    preprocess_PCA = 2,
    multi_cores = FALSE,
    eval_measure = "sens",
    force_train_method = "random_forest"
  )

  testthat::expect_that(trained2, testthat::is_a("list"))
  testthat::expect_is(trained2$truth_predictions$truth[1], "factor")
  testthat::expect_equal(trained2$truth_predictions$.pred_1[1], 0.318)

  trained_NA <- text::textTrain(
    x = word_embeddings_4$texts$harmonytext,
    y = example_categories,
    outside_folds = 2,
    inside_folds = 2 / 3,
    outside_strata_y = NULL,
    inside_strata_y = NULL,
    force_train_method = "random_forest",
    mtry = c(1),
    min_n = c(1),
    trees = c(1000),
    preprocess_PCA = NA,
    multi_cores = FALSE,
    eval_measure = "spec"
  )

  testthat::expect_that(trained_NA, testthat::is_a("list"))
  testthat::expect_is(trained_NA$truth_predictions$truth[1], "factor")
  testthat::expect_equal(trained_NA$truth_predictions$.pred_1[1], 0.352)
})



test_that("textTrainRandomForest with Extremely
          Randomized Trees produces list of results
          with prediction being categorical", {
  skip_on_cran()

  example_categories <- as.factor(c(
    1, NA, 1, 2, 1, 2, 1, 2, 1, 2,
    1, 2, 1, 2, 1, 2, 1, 2, 1, 2,
    1, 2, 1, 2, 1, 2, 1, 2, 1, 2,
    1, 2, 1, 2, 1, 2, 1, 2, 1, 2
  ))
  trained_rf_95 <- text::textTrainRandomForest(
    x = word_embeddings_4$texts$harmonytext,
    y = example_categories,
    outside_folds = 2,
    inside_folds = 2 / 3,
    outside_strata_y = NULL,
    inside_strata_y = NULL,
    mode_rf = "classification",
    mtry = c(1),
    min_n = c(1),
    trees = c(1000),
    preprocess_PCA = c(0.95),
    extremely_randomised_splitrule = NULL,
    multi_cores = FALSE,
    eval_measure = "roc_auc",
    save_output = "only_results",
    event_level = "second"
  )

  testthat::expect_that(trained_rf_95, testthat::is_a("list"))
  testthat::expect_is(trained_rf_95$results$.estimate[1], "numeric")
  testthat::expect_equal(trained_rf_95$results$.estimate[1], 0.4102564, tolerance = 0.001)

  example_categories <- as.factor(c(
    1, 2, 1, 2, 1, 2, 1, 2, 1, 2,
    1, 2, 1, 2, 1, 2, 1, 2, 1, 2,
    1, 2, 1, 2, 1, 2, 1, 2, 1, 2,
    1, 2, 1, 2, 1, 2, 1, 2, 1, 2
  ))
  trained_rf_3 <- text::textTrainRandomForest(
    x = word_embeddings_4$texts$harmonytext,
    y = example_categories,
    outside_folds = 2,
    inside_folds = 2 / 3,
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
  testthat::expect_equal(trained_rf_3$truth_predictions$.pred_1[1], 0.107)

  example_categories_tibble <- tibble::as_tibble_col(example_categories)
  trained_rf_NA <- text::textTrainRandomForest(
    x = word_embeddings_4$texts[1],
    y = example_categories_tibble,
    outside_folds = 2,
    inside_folds = 2 / 3,
    outside_strata_y = NULL,
    inside_strata_y = NULL,
    mtry = c(1),
    min_n = c(1),
    trees = c(1000),
    preprocess_PCA = NA,
    extremely_randomised_splitrule = "gini",
    multi_cores = FALSE
  )

  testthat::expect_that(trained_rf_NA, testthat::is_a("list"))
  testthat::expect_is(trained_rf_NA$truth_predictions$truth[1], "factor")
  testthat::expect_equal(trained_rf_NA$truth_predictions$.pred_1[1], 0.606)
})



test_that("textTrainLists Regression produces a list of results with prediction being numeric", {
  skip_on_cran()

  # One word embedding and two rating scales help(textTrainRegression)
  results_or <- text::textTrainLists(
    x = word_embeddings_4$texts$harmonywords,
    y = Language_based_assessment_data_8[5:6],
    outside_folds = 2,
    inside_folds = 2 / 3,
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
    multi_cores = FALSE
  )

  testthat::expect_that(results_or, testthat::is_a("list"))
  testthat::expect_is(results_or$results$tau_correlation[1], "character")
  testthat::expect_equal(results_or$results$tau_correlation[1], "0.21297093352316")

  word_embedding <- word_embeddings_4$texts[1]
  ratings_data1 <- Language_based_assessment_data_8[5]
  ratings_data2 <- Language_based_assessment_data_8[6]
  factors1 <- tibble::as_tibble_col(as.factor(Language_based_assessment_data_8$gender))
  ratings_data <- cbind(ratings_data1, ratings_data2, factors1)


  results_or_p1 <- text::textTrainLists(
    x = word_embedding,
    y = ratings_data,
    preprocess_PCA = c(0.90),
    outside_folds = 2,
    inside_folds = 2 / 3,
    outside_strata_y = NULL,
    inside_strata_y = NULL,
    penalty = c(2),
    mixture = c(0),
    force_train_method = "automatic",
    save_output = "only_results_predictions",
    multi_cores = FALSE
  )

  testthat::expect_that(results_or_p1, testthat::is_a("list"))
  testthat::expect_is(results_or_p1$results$correlation[1], "character")
  testthat::expect_equal(results_or_p1$results$correlation[1], "0.374437383412247") # "0.374437383412246" "0.374436371225743"

  # FORCE RANDOM FORREST Even though categorical variables are not most present
  results_or_p2 <- text::textTrain(
    x = word_embedding,
    y = ratings_data,
    preprocess_PCA = c(0.90),
    outside_folds = 2,
    inside_folds = 2 / 3,
    outside_strata_y = NULL,
    inside_strata_y = NULL,
    penalty = c(2),
    mixture = c(0),
    force_train_method = "random_forest",
    save_output = "only_results_predictions",
    multi_cores = FALSE,
    seed = 22
  )
  # multi_cores_sys_default will result it slightly different results
  testthat::expect_that(results_or_p2, testthat::is_a("list"))
  testthat::expect_is(results_or_p2$results$.estimate[1], "numeric")
  testthat::expect_equal(results_or_p2$results$.estimate[1], 0.525, tolerance = 0.001)
  #testthat::expect_equal(results_or_p2D$results$.estimate[1], 0.500, tolerance = 0.001)
  #testthat::expect_equal(results_or_p2F$results$.estimate[1], 0.500, tolerance = 0.001)
  #testthat::expect_equal(results_or_p2T$results$.estimate[1], 0.500, tolerance = 0.001)
  #testthat::expect_equal(results_or_p2$results$.estimate[1], 0.425, tolerance = 0.001)
  #testthat::expect_equal(results_or_p2$results$.estimate[1], 0.475, tolerance = 0.001)


  factors1 <- as.factor(Language_based_assessment_data_8$gender)
  factors2 <- as.factor(Language_based_assessment_data_8$gender)
  rating1 <- Language_based_assessment_data_8$hilstotal

  ratings_data_factors <- tibble::tibble(factors1, factors2, rating1)

  # Logistic
  results_list_logistic1 <- text::textTrainLists(
    x = word_embeddings_4$texts[1],
    y = ratings_data_factors,
    preprocess_PCA = c(0.90),
    outside_folds = 2,
    inside_folds = 2 / 3,
    outside_strata_y = NULL,
    inside_strata_y = NULL,
    penalty = c(2),
    mixture = c(0),
    force_train_method = "automatic",
    # model = "logistic",
    eval_measure = "default",
    save_output = "only_results_predictions",
    multi_cores = FALSE
  )

  testthat::expect_that(results_list_logistic1, testthat::is_a("list"))
  testthat::expect_equal(results_list_logistic1$results[[2]][1], "0.538720538720539")


  results_list_logistic <- text::textTrain(
    x = word_embedding,
    y = ratings_data_factors,
    preprocess_PCA = c(0.90),
    outside_folds = 2,
    inside_folds = 2 / 3,
    outside_strata_y = NULL,
    inside_strata_y = NULL,
    penalty = c(2),
    mixture = c(0),
    force_train_method = "regression",
    save_output = "only_results_predictions",
    multi_cores = FALSE
  )

  testthat::expect_that(results_list_logistic, testthat::is_a("list"))
  testthat::expect_is(results_list_logistic$results[[2]][[1]], "integer")
  testthat::expect_equal(results_list_logistic$results[[3]][[1]], 0.008647702, tolerance = 0.0001)
})


test_that("textTrainLists randomForest produces list of results with prediction being numeric", {
  skip_on_cran()

  x <- word_embeddings_4$texts[1]

  y1 <- factor(rep(c("young", "old", "young", "old", "young", "old", "young", "old", "young", "old"), 4))
  y2 <- factor(rep(c("young", "old", "young", "old", "young", "old", "young", "old", "young", "old"), 4))
  y <- tibble::tibble(y1, y2)

  results_rf_et <- text::textTrain(
    x = x,
    y = y,
    force_train_method = "random_forest",
    outside_folds = 2,
    inside_folds = 2 / 3,
    outside_strata_y = NULL,
    inside_strata_y = NULL,
    mtry = c(1),
    min_n = c(1),
    preprocess_PCA = c(0.95),
    trees = c(1000),
    eval_measure = "accuracy",
    extremely_randomised_splitrule = "extratrees",
    save_output = "all",
    multi_cores = FALSE
  )

  testthat::expect_that(results_rf_et, testthat::is_a("list"))
  testthat::expect_is(results_rf_et$results$p_value[1], "character")
  testthat::expect_equal(results_rf_et$results$precision[1], 0.4705882, tolerance = 0.0001)

  results_rf <- text::textTrain(
    x = x,
    y = y,
    force_train_method = "random_forest",
    outside_folds = 2,
    inside_folds = 2 / 3,
    outside_strata_y = NULL,
    inside_strata_y = NULL,
    mtry = c(1),
    min_n = c(1),
    preprocess_PCA = NA,
    trees = c(1000),
    eval_measure = "kappa",
    save_output = "all",
    multi_cores = FALSE
  )

  testthat::expect_that(results_rf, testthat::is_a("list"))
  testthat::expect_is(results_rf$results$p_value[1], "character")
  testthat::expect_equal(results_rf$results$p_value[1], "0.522372193561587")

  results_rf_or_p <- text::textTrain(
    x = x,
    y = y,
    force_train_method = "random_forest",
    outside_folds = 2,
    inside_folds = 2 / 3,
    outside_strata_y = NULL,
    inside_strata_y = NULL,
    mtry = c(1),
    min_n = c(1),
    preprocess_PCA = c(0.95),
    trees = c(1000),
    eval_measure = "precision",
    save_output = "only_results_predictions",
    multi_cores =FALSE
  )

  testthat::expect_that(results_rf_or_p, testthat::is_a("list"))
  testthat::expect_is(results_rf_or_p$results$p_value[1], "character")
  testthat::expect_equal(results_rf_or_p$results$precision[1], 0.4705882, tolerance = 0.0001)


  results_rf_or <- text::textTrain(
    x = x,
    y = y,
    force_train_method = "random_forest",
    outside_folds = 2,
    inside_folds = 2 / 3,
    outside_strata_y = NULL,
    inside_strata_y = NULL,
    mtry = c(1),
    min_n = c(1),
    preprocess_PCA = c(0.95),
    trees = c(1000),
    eval_measure = "precision",
    save_output = "only_results",
    multi_cores = FALSE
  )

  testthat::expect_that(results_rf_or, testthat::is_a("list"))
  testthat::expect_is(results_rf_or$results$p_value[1], "character")
  testthat::expect_equal(results_rf_or$results$roc_auc[1], 0.38625)
})




test_that("textTrainRegression adding word_embedding together", {
  skip_on_cran()

  multi_we_PCA_09 <- text::textTrainRegression(
    x = word_embeddings_4$texts[1:2],
    y = Language_based_assessment_data_8$hilstotal,
    outside_folds = 2,
    inside_folds = 2 / 3,
    outside_strata_y = NULL,
    inside_strata_y = NULL,
    preprocess_PCA = c(0.9),
    penalty = 1,
    multi_cores = FALSE
  )

  testthat::expect_that(multi_we_PCA_09, testthat::is_a("list"))
  testthat::expect_is(multi_we_PCA_09$results[[1]][[1]], "numeric")
  testthat::expect_equal(multi_we_PCA_09$results[[1]][[1]], 1.159983, tolerance = 0.0001)


  # Prediction based on multiple we
  predictions_multi <- text::textPredict(multi_we_PCA_09, word_embeddings_4$texts[1:2], dim_names = TRUE)
  testthat::expect_is(predictions_multi[[1]][[1]], "numeric")
  testthat::expect_equal(predictions_multi[[1]][[1]], 19.70077, tolerance = 0.0001)


  multi_we_PCA_3 <- text::textTrainRegression(
    x = word_embeddings_4$texts[1:2],
    y = Language_based_assessment_data_8$hilstotal,
    outside_folds = 2,
    inside_folds = 2 / 3,
    outside_strata_y = NULL,
    inside_strata_y = NULL,
    preprocess_PCA = 3,
    penalty = 1,
    multi_cores = FALSE
  )

  testthat::expect_that(multi_we_PCA_3, testthat::is_a("list"))
  testthat::expect_is(multi_we_PCA_3$results[[1]][[1]], "numeric")
  testthat::expect_equal(multi_we_PCA_3$results[[1]][[1]], 1.456567, tolerance = 0.0001)


  multi_we_PCA_NA <- text::textTrainRegression(
    x = word_embeddings_4$texts[1:2],
    y = Language_based_assessment_data_8$hilstotal,
    outside_folds = 2,
    inside_folds = 2 / 3,
    outside_strata_y = NULL,
    inside_strata_y = NULL,
    preprocess_PCA = NA,
    penalty = 1,
    multi_cores = FALSE
  )

  testthat::expect_that(multi_we_PCA_NA, testthat::is_a("list"))
  testthat::expect_is(multi_we_PCA_NA$results[[1]][[1]], "numeric")
  testthat::expect_equal(multi_we_PCA_NA$results[[1]][[1]], 1.58414, tolerance = 0.001)
})


test_that("textTrainRandomForest adding word_embedding together", {
  skip_on_cran()
  y <- as.factor(rep(c(1, 2, 1, 2, 1, 2, 1, 2, 1, 2), 4))

  multi_we_RF_PCA_09 <- text::textTrainRandomForest(
    x = word_embeddings_4$texts[1:2],
    y = y,
    outside_folds = 2,
    inside_folds = 2 / 3,
    outside_strata_y = NULL,
    inside_strata_y = NULL,
    preprocess_PCA = 0.9,
    mtry = c(1),
    min_n = c(1),
    multi_cores = FALSE
  )

  testthat::expect_that(multi_we_RF_PCA_09, testthat::is_a("list"))
  testthat::expect_is(multi_we_RF_PCA_09$results$.estimate[[1]], "numeric")
  testthat::expect_equal(multi_we_RF_PCA_09$results$.estimate[[1]], 0.4)


  multi_we_RF_PCA_3 <- text::textTrainRandomForest(
    x = word_embeddings_4$texts[1:2],
    y = y,
    outside_folds = 2,
    inside_folds = 2 / 3,
    outside_strata_y = NULL,
    inside_strata_y = NULL,
    preprocess_PCA = 3,
    mtry = c(1),
    min_n = c(1),
    multi_cores = FALSE
  )

  testthat::expect_that(multi_we_RF_PCA_3, testthat::is_a("list"))
  testthat::expect_is(multi_we_RF_PCA_3$results$.estimate[[1]], "numeric")
  testthat::expect_equal(multi_we_RF_PCA_3$results$.estimate[[1]], 0.375)


  multi_we_RF_PCA_NA <- text::textTrainRandomForest(
    x = word_embeddings_4$texts[1:2],
    y = y,
    outside_folds = 2,
    inside_folds = 2 / 3,
    outside_strata_y = NULL,
    inside_strata_y = NULL,
    preprocess_PCA = NA,
    mtry = c(1),
    min_n = c(1),
    multi_cores = FALSE
  )


  testthat::expect_that(multi_we_RF_PCA_NA, testthat::is_a("list"))
  testthat::expect_is(multi_we_RF_PCA_NA$results$.estimate[[1]], "numeric")
  testthat::expect_equal(multi_we_RF_PCA_NA$results$.estimate[[1]], 0.325)
})



test_that("textPredictTest t-test and bootstrapped test", {
  skip_on_cran()
  set.seed(1)
  # Test data
  y1 <- runif(10)
  yhat1 <- runif(10)
  y2 <- runif(10)
  yhat2 <- runif(10)

  boot_test <- text::textPredictTest(y1, yhat1, y2, yhat2, bootstraps_times = 10)

  testthat::expect_that(boot_test, testthat::is_a("list"))

  boot_test2 <- text::textPredictTest(y1 = y1, yhat1, y2 = NULL, yhat2)
  testthat::expect_that(boot_test2, testthat::is_a("list"))
  testthat::expect_equal(boot_test2$Test$statistic[[1]], 0.233267, tolerance = 0.0001)
  testthat::expect_equal(boot_test2$Effect_size, 0.06198192, tolerance = 0.0001)
})


test_that("training with only x_append (without word embeddings)", {
  skip_on_cran()
  #help("textTrainRegression")
  test_firstTRUE <- text::textTrainRegression(
    x = word_embeddings_4$texts$harmonywords,
    x_append = Language_based_assessment_data_8[6:7],
    y = Language_based_assessment_data_8[5],
    outside_folds = 2,
    append_first = TRUE,
    multi_cores = FALSE
  )
  testthat::expect_that(test_firstTRUE, testthat::is_a("list"))

  test_firstTRUE <- text::textTrainRandomForest(
    x = word_embeddings_4$texts$harmonywords,
    x_append = Language_based_assessment_data_8[6:7],
    y = Language_based_assessment_data_8["gender"],
    outside_folds = 2,
    append_first = TRUE
  )
  testthat::expect_that(test_firstTRUE, testthat::is_a("list"))


  test1 <- text::textTrainRegression(
    x = NULL,
    x_append = Language_based_assessment_data_8[6:7],
    y = Language_based_assessment_data_8[5],
    outside_folds = 2
  )

  testthat::expect_that(test1, testthat::is_a("list"))

  test2 <- text::textTrainRandomForest(
    x = NULL,
    x_append = Language_based_assessment_data_8[6:7],
    y = Language_based_assessment_data_8[8],
    outside_folds = 2
  )

  testthat::expect_that(test2, testthat::is_a("list"))
})
