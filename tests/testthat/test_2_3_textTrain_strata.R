library(testthat)
library(text)
library(tibble)
library(dplyr)

context("Training Functions")

test_that("textTrain with strata settings", {
  skip_on_cran()

  df1 <- Language_based_assessment_data_8[c(1, 5:8)]
  colnames(df1) <- c("text", "hilstotal", "swlstotal", "age", "gender")
  df2 <- Language_based_assessment_data_8[c(2, 5:8)]
  colnames(df2) <- c("text", "hilstotal", "swlstotal", "age", "gender")
  df3 <- Language_based_assessment_data_8[c(3, 5:8)]
  colnames(df3) <- c("text", "hilstotal", "swlstotal", "age", "gender")
  df4 <- Language_based_assessment_data_8[c(4, 5:8)]
  colnames(df4) <- c("text", "hilstotal", "swlstotal", "age", "gender")

  df1_4 <- dplyr::bind_rows(df1, df2, df3, df4)

  df1_4_emb <- textEmbed(df1_4,
    keep_token_embeddings = FALSE
  )



  strata_y <- text::textTrainRegression(
    x = df1_4_emb$texts[c("text")],
    y = df1_4[2],
    cv_method = "cv_folds", # validation_split cv_folds
    outside_folds = 2,
    inside_folds = 2,
    strata = "y", # Language_based_assessment_data_8[8], #"y"
    outside_strata = TRUE,
    strata_breaks = 3,
    inside_strata = TRUE,
    model = "regression",
    eval_measure = "rmse",
    penalty = c(1),
    mixture = c(0),
    preprocess_PCA = NA, # 1, #NA, #1,
    multi_cores = FALSE,
    # force_train_method = "automatic",
    save_output = "only_results"
  )
  strata_y$results[[4]]
  testthat::expect_equal(strata_y$results[[4]][[1]], 0.7024823, tolerance = 0.0001)

  strata_ydf <- text::textTrainRegression(
    x = df1_4_emb$texts[c("text")],
    y = df1_4[2],
    cv_method = "cv_folds", # validation_split cv_folds
    outside_folds = 2,
    inside_folds = 2,
    strata = df1_4[2],
    outside_strata = TRUE,
    strata_breaks = 3,
    inside_strata = TRUE,
    model = "regression",
    eval_measure = "rmse",
    penalty = c(1),
    mixture = c(0),
    preprocess_PCA = NA, # 1, #NA, #1,
    multi_cores = FALSE,
    # force_train_method = "automatic",
    save_output = "only_results"
  )
  strata_ydf$results[[4]]
  testthat::expect_equal(strata_ydf$results[[4]][[1]], .7024823, tolerance = 0.0001)

  strata_ydf_inner <- text::textTrainRegression(
    x = df1_4_emb$texts[c("text")],
    y = df1_4[2],
    cv_method = "cv_folds", # validation_split cv_folds
    outside_folds = 2,
    inside_folds = 2,
    strata = df1_4[2],
    outside_strata = FALSE,
    strata_breaks = 3,
    inside_strata = TRUE,
    model = "regression",
    eval_measure = "rmse",
    penalty = c(1),
    mixture = c(0),
    preprocess_PCA = NA, # 1, #NA, #1,
    multi_cores = FALSE,
    # force_train_method = "automatic",
    save_output = "only_results"
  )
  strata_ydf_inner$results[[4]]
  testthat::expect_equal(strata_ydf_inner$results[[4]][[1]], .7024823, tolerance = 0.0001)


  strata_ydf_outer <- text::textTrainRegression(
    x = df1_4_emb$texts[c("text")],
    y = df1_4[2],
    cv_method = "cv_folds", # validation_split cv_folds
    outside_folds = 2,
    inside_folds = 2,
    strata = df1_4[2],
    outside_strata = TRUE,
    strata_breaks = 3,
    inside_strata = FALSE,
    model = "regression",
    eval_measure = "rmse",
    penalty = c(1),
    mixture = c(0),
    preprocess_PCA = NA, # 1, #NA, #1,
    multi_cores = FALSE,
    # force_train_method = "automatic",
    save_output = "only_results"
  )
  strata_ydf_outer$results[[4]]
  testthat::expect_equal(strata_ydf_outer$results[[4]][[1]], .6623173, tolerance = 0.0001)

  strata_NO <- text::textTrainRegression(
    x = df1_4_emb$texts[c("text")],
    y = df1_4[2],
    cv_method = "cv_folds", # validation_split cv_folds
    outside_folds = 2,
    inside_folds = 2,
    strata = NULL,
    outside_strata = FALSE,
    strata_breaks = 3,
    inside_strata = FALSE,
    model = "regression",
    eval_measure = "rmse",
    penalty = c(1),
    mixture = c(0),
    preprocess_PCA = NA, # 1, #NA, #1,
    multi_cores = FALSE,
    # force_train_method = "automatic",
    save_output = "only_results"
  )
  strata_NO$results[[4]]
  testthat::expect_equal(strata_NO$results[[4]][[1]], .6623173, tolerance = 0.0001)

  strata_gender_df <- text::textTrainRegression(
    x = df1_4_emb$texts[c("text")],
    y = df1_4[2],
    cv_method = "cv_folds", # validation_split cv_folds
    outside_folds = 2,
    inside_folds = 2,
    strata = df1_4[5],
    outside_strata = TRUE,
    strata_breaks = 3,
    inside_strata = TRUE,
    model = "regression",
    eval_measure = "rmse",
    penalty = c(1),
    mixture = c(0),
    preprocess_PCA = NA, # 1, #NA, #1,
    multi_cores = FALSE,
    # force_train_method = "automatic",
    save_output = "only_results"
  )
  strata_gender_df$results[[4]]
  testthat::expect_equal(strata_gender_df$results[[4]][[1]], .6808301, tolerance = 0.0001)


  strata_y$results[[4]]
  strata_ydf$results[[4]]
  strata_ydf_inner$results[[4]]
  strata_NO$results[[4]]
  strata_ydf_outer$results[[4]]
  strata_gender_df$results[[4]]

  #

  two_emb <- text::textTrainRegression(
    x = word_embeddings_4$texts[c("harmonytexts", "satisfactiontexts")],
    y = Language_based_assessment_data_8[5],
    cv_method = "cv_folds", # validation_split cv_folds
    outside_folds = 2,
    inside_folds = 2,
    strata = Language_based_assessment_data_8[6],
    outside_strata = TRUE,
    strata_breaks = 3,
    inside_strata = TRUE,
    model = "regression",
    eval_measure = "rmse",
    penalty = c(1),
    mixture = c(0),
    preprocess_PCA = NA, # 1, #NA, #1,
    multi_cores = FALSE,
    # force_train_method = "automatic",
    save_output = "only_results"
  )

  testthat::expect_equal(two_emb$results[[4]][[1]], 0.3412349, tolerance = 0.0001)





  example_categories <- as.factor(c(
    1, 2, 1, 2, 1, 2, 1, 2, 1, 2,
    1, 2, 1, 2, 1, 2, 1, 2, 1, 2,
    1, 2, 1, 2, 1, 2, 1, 2, 1, 2,
    1, 2, 1, 2, 1, 2, 1, 2, 1, 2
  ))

  trained_rf_gender <- text::textTrainRandomForest(
    x = word_embeddings_4$texts$harmonytext,
    y = example_categories,
    outside_folds = 2,
    inside_folds = 2 / 3,
    outside_strata = TRUE,
    strata = Language_based_assessment_data_8[8],
    inside_strata = TRUE,
    mtry = c(1),
    min_n = c(1),
    trees = c(1000),
    preprocess_PCA = c(3),
    extremely_randomised_splitrule = "gini",
    multi_cores = FALSE,
    eval_measure = "kappa",
    save_output = "only_results_predictions"
  )

  trained_rf_gender

  if (Sys.info()["sysname"] == "Darwin" | Sys.info()["sysname"] == "Windows") {
    testthat::expect_equal(trained_rf_gender$results[[3]][[1]], .375, tolerance = 0.0001)
  }
  if (Sys.info()["sysname"] == "Linux" ) {
    testthat::expect_equal(trained_rf_gender$results[[3]][[1]], .35, tolerance = 0.0001)
  }



})
