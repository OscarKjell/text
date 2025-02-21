library(testthat)
library(text)
library(tibble)
library(dplyr)

context("Random Forest")


test_that("textTrain Random Forest produces list of results with prediction being categorical", {
  skip_on_cran()

 set.seed(1)
 example_categories1 <- sample(c(1, 2), 40, replace = T)
 example_categories1 <- as.factor(example_categories1)

 model_rf_1 <- text::textTrain(
    x = word_embeddings_4$texts$harmonytext,
    y = example_categories1,
    cv_method = "validation_split",
    outside_folds = 2,
    inside_folds = 3 / 4,
    outside_strata = FALSE,
    inside_strata = FALSE,
    mtry = c(1),
    min_n = c(1),
    trees = c(1000),
    preprocess_PCA = "min_halving",
    multi_cores = FALSE,
    eval_measure = "f_measure",
    force_train_method = "random_forest"
  )

  testthat::expect_that(model_rf_1, testthat::is_a("list"))
  testthat::expect_is(model_rf_1$truth_predictions$truth[1], "factor")

  if (Sys.info()["sysname"] == "Darwin" | Sys.info()["sysname"] == "Windows") {
    testthat::expect_equal(model_rf_1$truth_predictions$.pred_1[1], 0.404)
  }
  if (Sys.info()["sysname"] == "Linux") {
    testthat::expect_equal(model_rf_1$truth_predictions$.pred_1[1], 0.413, tolerance = 0.01)
  }

  model_rf_1_pred <- textPredict(
    model_info = model_rf_1,
    word_embeddings = word_embeddings_4$texts["harmonytexts"]
  )
  model_rf_1_pred$`harmonytexts__cv_method="validation_split"pred`
  testthat::expect_equal(as.character(model_rf_1_pred[[1]][1]), "1")
  testthat::expect_equal(as.character(model_rf_1_pred[[1]][2]), "2")
  testthat::expect_equal(as.character(model_rf_1_pred[[1]][3]), "1")
  testthat::expect_equal(as.character(model_rf_1_pred[[1]][4]), "1")
  testthat::expect_equal(as.character(model_rf_1_pred[[1]][5]), "2")

  example_categories <- as.factor(c(
    1, 2, 1, 2, 1, 2, 1, 2, 1, 2,
    1, 2, 1, 2, 1, 2, 1, 2, 1, 2,
    1, 2, 1, 2, 1, 2, 1, 2, 1, 2,
    1, 2, 1, 2, 1, 2, 1, 2, 1, 2
  ))

  trained2 <- text::textTrain(
    x = word_embeddings_4$texts$harmonytext,
    y = example_categories,
    outside_folds = 2,
    inside_folds = 3 / 4,
    outside_strata = FALSE,
    inside_strata = FALSE,
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
  testthat::expect_equal(trained2$truth_predictions$.pred_1[1], 0.306)

  trained_NA <- text::textTrain(
    x = word_embeddings_4$texts$harmonytext,
    y = example_categories,
    outside_folds = 2,
    inside_folds = 2 / 3,
    outside_strata = FALSE,
    inside_strata = FALSE,
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
  testthat::expect_equal(trained_NA$truth_predictions$.pred_1[1], 0.361)
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
    outside_strata = FALSE,
    inside_strata = FALSE,
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
  #  testthat::expect_equal(trained_rf_95$results$.estimate[1], 0.4102564, tolerance = 0.001) R4.2

  if (Sys.info()["sysname"] == "Darwin" | Sys.info()["sysname"] == "Windows") {
    testthat::expect_equal(trained_rf_95$results$.estimate[1], 0.4615385, tolerance = 0.001)
  }
  if (Sys.info()["sysname"] == "Linux") {
    testthat::expect_equal(trained_rf_95$results$.estimate[1], 0.4102564, tolerance = 0.001)
  }


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
    outside_strata = FALSE,
    inside_strata = FALSE,
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
  #  testthat::expect_equal(trained_rf_3$truth_predictions$.pred_1[1], 0.107) R4.2


  if (Sys.info()["sysname"] == "Darwin" | Sys.info()["sysname"] == "Windows") {
    testthat::expect_equal(trained_rf_3$truth_predictions$.pred_1[1], 0.134)
  }
  if (Sys.info()["sysname"] == "Linux") {
    testthat::expect_equal(trained_rf_3$truth_predictions$.pred_1[1], 0.107)
  }


  example_categories_tibble <- tibble::as_tibble_col(example_categories)
  trained_rf_NA <- text::textTrainRandomForest(
    x = word_embeddings_4$texts[1],
    y = example_categories_tibble,
    outside_folds = 2,
    inside_folds = 2 / 3,
    outside_strata = FALSE,
    inside_strata = FALSE,
    mtry = c(1),
    min_n = c(1),
    trees = c(1000),
    preprocess_PCA = NA,
    extremely_randomised_splitrule = "gini",
    multi_cores = FALSE
  )

  testthat::expect_that(trained_rf_NA, testthat::is_a("list"))
  testthat::expect_is(trained_rf_NA$truth_predictions$truth[1], "factor")
  testthat::expect_equal(trained_rf_NA$truth_predictions$.pred_1[1], 0.587)
})



test_that("textTrainLists Regression produces a list of results with prediction being numeric", {
  skip_on_cran()
  word_embedding <- word_embeddings_4$texts[1]
  ratings_data1 <- Language_based_assessment_data_8[5]
  ratings_data2 <- Language_based_assessment_data_8[6]
  factors1 <- tibble::as_tibble_col(as.factor(Language_based_assessment_data_8$gender))
  ratings_data <- cbind(ratings_data1, ratings_data2, factors1)

  # FORCE RANDOM FORREST Even though categorical variables are not most present
  results_or_p2 <- text::textTrain(
    x = word_embedding,
    y = ratings_data,
    preprocess_PCA = c(0.90),
    outside_folds = 2,
    inside_folds = 2 / 3,
    outside_strata = FALSE,
    inside_strata = FALSE,
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
    outside_strata = FALSE,
    inside_strata = FALSE,
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
    outside_strata = FALSE,
    inside_strata = FALSE,
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
  #  testthat::expect_equal(results_rf_et$results$precision[1], 0.4705882, tolerance = 0.0001) R 4.2


  if (Sys.info()["sysname"] == "Darwin" | Sys.info()["sysname"] == "Windows") {
    testthat::expect_equal(results_rf_et$results$precision[1], 0.4444444, tolerance = 0.0001)
  }
  if (Sys.info()["sysname"] == "Linux") {
    testthat::expect_equal(results_rf_et$results$precision[1], 0.4705882, tolerance = 0.0001)
  }


  results_rf <- text::textTrain(
    x = x,
    y = y,
    force_train_method = "random_forest",
    outside_folds = 2,
    inside_folds = 2 / 3,
    outside_strata = FALSE,
    inside_strata = FALSE,
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
  testthat::expect_equal(results_rf$results$p_value[1], "0.191418425237607")

  results_rf_or_p <- text::textTrain(
    x = x,
    y = y,
    force_train_method = "random_forest",
    outside_folds = 2,
    inside_folds = 2 / 3,
    outside_strata = FALSE,
    inside_strata = FALSE,
    mtry = c(1),
    min_n = c(1),
    preprocess_PCA = c(0.95),
    trees = c(1000),
    eval_measure = "precision",
    save_output = "only_results_predictions",
    multi_cores = FALSE
  )

  testthat::expect_that(results_rf_or_p, testthat::is_a("list"))
  testthat::expect_is(results_rf_or_p$results$p_value[1], "character")
  #  testthat::expect_equal(results_rf_or_p$results$precision[1], 0.4705882, tolerance = 0.0001) # R 4.2


  if (Sys.info()["sysname"] == "Darwin" | Sys.info()["sysname"] == "Windows") {
    testthat::expect_equal(results_rf_or_p$results$precision[1], 0.4444444, tolerance = 0.0001) # R 4.3
  }
  if (Sys.info()["sysname"] == "Linux") {
    testthat::expect_equal(results_rf_or_p$results$precision[1], 0.4705882, tolerance = 0.0001) # R 4.3
  }

  results_rf_or <- text::textTrain(
    x = x,
    y = y,
    force_train_method = "random_forest",
    outside_folds = 2,
    inside_folds = 2 / 3,
    outside_strata = FALSE,
    inside_strata = FALSE,
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
  #  testthat::expect_equal(results_rf_or$results$roc_auc[1], 0.38625) # R 4.2



  if (Sys.info()["sysname"] == "Darwin" | Sys.info()["sysname"] == "Windows") {
    testthat::expect_equal(results_rf_or$results$roc_auc[1], 0.41)
  }
  if (Sys.info()["sysname"] == "Linux") {
    testthat::expect_equal(results_rf_or$results$roc_auc[1], 0.38625)
  }

})



test_that("textTrainRandomForest adding word_embedding together", {
  skip_on_cran()
  y <- as.factor(rep(c(1, 2, 1, 2, 1, 2, 1, 2, 1, 2), 4))

  multi_we_RF_PCA_09 <- text::textTrainRandomForest(
    x = word_embeddings_4$texts[1:2],
    y = y,
    outside_folds = 2,
    inside_folds = 2 / 3,
    outside_strata = FALSE,
    inside_strata = FALSE,
    preprocess_PCA = 0.9,
    mtry = c(1),
    min_n = c(1),
    multi_cores = FALSE
  )

  testthat::expect_that(multi_we_RF_PCA_09, testthat::is_a("list"))
  testthat::expect_is(multi_we_RF_PCA_09$results$.estimate[[1]], "numeric")
  testthat::expect_equal(multi_we_RF_PCA_09$results$.estimate[[1]], 0.425)


  multi_we_RF_PCA_3 <- text::textTrainRandomForest(
    x = word_embeddings_4$texts[1:2],
    y = y,
    outside_folds = 2,
    inside_folds = 2 / 3,
    outside_strata = FALSE,
    inside_strata = FALSE,
    preprocess_PCA = 3,
    mtry = c(1),
    min_n = c(1),
    multi_cores = FALSE
  )

  testthat::expect_that(multi_we_RF_PCA_3, testthat::is_a("list"))
  testthat::expect_is(multi_we_RF_PCA_3$results$.estimate[[1]], "numeric")
  #  testthat::expect_equal(multi_we_RF_PCA_3$results$.estimate[[1]], 0.375) #R4.2

  if (Sys.info()["sysname"] == "Darwin" | Sys.info()["sysname"] == "Windows") {
    testthat::expect_equal(multi_we_RF_PCA_3$results$.estimate[[1]], 0.35)    # R4.3
  }
  if (Sys.info()["sysname"] == "Linux") {
    testthat::expect_equal(multi_we_RF_PCA_3$results$.estimate[[1]], 0.375) #R4.2
  }


  multi_we_RF_PCA_NA <- text::textTrainRandomForest(
    x = word_embeddings_4$texts[1:2],
    y = y,
    outside_folds = 2,
    inside_folds = 2 / 3,
    outside_strata = FALSE,
    inside_strata = FALSE,
    preprocess_PCA = NA,
    mtry = c(1),
    min_n = c(1),
    multi_cores = FALSE
  )


  testthat::expect_that(multi_we_RF_PCA_NA, testthat::is_a("list"))
  testthat::expect_is(multi_we_RF_PCA_NA$results$.estimate[[1]], "numeric")
  testthat::expect_equal(multi_we_RF_PCA_NA$results$.estimate[[1]], 0.4)
})


test_that("textTrainRandomForest running multiple categories in X", {
  skip_on_cran()

  set.seed(1)
  tibble_copy <- Language_based_assessment_data_8 %>%
    #mutate(kind_2=sample(letters[1:2],n(),T) %>% as.factor) %>%
    mutate(kind_3 = sample(letters[1:3], n(), T) %>% as.factor)

  ## testing with three categories
  trained_rf_3 <- text::textTrainRandomForest(
    x = word_embeddings_4$texts$harmonywords,
    y = tibble_copy$kind_3,
    simulate.p.value = T,
    multi_cores = FALSE # This makes it reproducable, and so it runs the same on Windows
  )


  testthat::expect_that(trained_rf_3, testthat::is_a("list"))
  testthat::expect_is(trained_rf_3$results$.estimate[[1]], "numeric")

  testthat::expect_equal(trained_rf_3$results$.estimator[[1]], "multiclass")

  testthat::expect_equal(length(trained_rf_3$roc_curve_plot$layers), 2)
  testthat::expect_true(ggplot2::is.ggplot(trained_rf_3$roc_curve_plot))


  if (Sys.info()["sysname"] == "Darwin") {
    testthat::expect_equal(trained_rf_3$results$.estimate[[1]], .325, tolerance = 0.001)
    testthat::expect_equal(trained_rf_3$results$.estimate[[6]], -0.08, tolerance = 0.0001)
  }
  if (Sys.info()["sysname"] == "Linux" | Sys.info()["sysname"] == "Windows") {
    testthat::expect_equal(trained_rf_3$results$.estimate[[1]], .375, tolerance = 0.001)
    testthat::expect_equal(trained_rf_3$results$.estimate[[6]], 0.00498, tolerance = 0.001)
  }

})



test_that("training with only x_append (without word embeddings)", {
  skip_on_cran()

  test_firstTRUE <- text::textTrainRandomForest(
    x = word_embeddings_4$texts$harmonywords,
    x_append = Language_based_assessment_data_8[6:7],
    y = Language_based_assessment_data_8["gender"],
    outside_folds = 2,
    append_first = TRUE
  )
  testthat::expect_that(test_firstTRUE, testthat::is_a("list"))

  test2 <- text::textTrainRandomForest(
    x = NULL,
    x_append = Language_based_assessment_data_8[6:7],
    y = Language_based_assessment_data_8[8],
    outside_folds = 2
  )

  testthat::expect_that(test2, testthat::is_a("list"))
})
