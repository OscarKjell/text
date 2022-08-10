

library(testthat)
library(tibble)
library(text)

context("Big analyses flow")


test_that("Testing textEmbed as well as train", {
  skip_on_cran()

  descr1 <- textDescriptives(Language_based_assessment_data_8[1])
  expect_that(descr1[[1]], is_a("character"))
  descr2 <- textDescriptives(Language_based_assessment_data_8[1:2])
  expect_that(descr2[[2]][[1]], is_a("integer"))

  harmony_word_embeddings <- textEmbed(Language_based_assessment_data_8[1:20, 1:2],
    model = "bert-base-uncased",
    dim_name = TRUE,
    layers = c(11:12),
    context_layers = c(11:12),
    decontext_layers = c(11:12)
  )

  expect_equal(harmony_word_embeddings$satisfactiontexts[[1]][1], 0.3403273, tolerance = 0.0001)
  expect_equal(harmony_word_embeddings$satisfactiontexts[[1]][2], 0.1531016, tolerance = 0.00001)

  # Below should not work since it does not have the same number og
  text_train_results1 <- textTrainRegression(
    x = harmony_word_embeddings$satisfactiontexts,
    y = Language_based_assessment_data_8$hilstotal[1:20],
    cv_method = "cv_folds",
    outside_folds = 2,
    inside_folds = 2,
    outside_strata_y = NULL,
    inside_strata_y = NULL,
    # preprocess_PCA = c(0.20),
    preprocess_PCA = NA,
    penalty = 1e-16,
    multi_cores = "multi_cores_sys_default"
  )

  text_train_results1 <- textTrainRegression(
    x = harmony_word_embeddings["satisfactiontexts"],
    y = Language_based_assessment_data_8["hilstotal"][1:20, ],
    cv_method = "cv_folds",
    outside_folds = 2,
    inside_folds = 2,
    outside_strata_y = NULL,
    inside_strata_y = NULL,
    # preprocess_PCA = c(0.20),
    preprocess_PCA = NA,
    penalty = 1e-16,
    multi_cores = "multi_cores_sys_default"
  )

  expect_that(text_train_results1$results$estimate[1], is_a("numeric"))
  expect_gt(text_train_results1$results$estimate[1], .3)
  expect_equal(text_train_results1$results$estimate[[1]], 0.3273128, tolerance = 0.00001)

  # Train with x_variable
  text_train_results2 <- text::textTrainRegression(
    x = harmony_word_embeddings[1:2],
    x_append = Language_based_assessment_data_8[1:20, 6:7],
    y = Language_based_assessment_data_8[1:20, 5],
    cv_method = "cv_folds",
    outside_folds = 2,
    inside_folds = 2,
    outside_strata_y = NULL,
    inside_strata_y = NULL,
    # preprocess_PCA = c(0.20),
    preprocess_PCA = NA,
    penalty = 1e-16,
    multi_cores = "multi_cores_sys_default"
  )

  # Predict
  hils_predicted_scores1 <- textPredict(
    model_info = text_train_results1,
    word_embeddings = harmony_word_embeddings$satisfactiontexts,
    dim_names = FALSE
  )

  expect_that(hils_predicted_scores1[[1]], is_a("numeric"))
  expect_equal(hils_predicted_scores1[[1]][1], 11.89, tolerance = 0.1)

  # Same as above with different input
  hils_predicted_scores1b <- textPredict(
    model_info = text_train_results1,
    word_embeddings = harmony_word_embeddings,
    dim_names = TRUE
  )
  expect_that(hils_predicted_scores1b[[1]], is_a("numeric"))
  expect_equal(hils_predicted_scores1b[[1]][1], 11.89, tolerance = 0.1)

  # Predict
  hils_predicted_scores1 <- textPredict(
    model_info = text_train_results2,
    word_embeddings = harmony_word_embeddings,
    x_append = Language_based_assessment_data_8[1:20, ],
    dim_names = TRUE
  )
  expect_equal(hils_predicted_scores1[[1]][1], 10.404, tolerance = 0.01)


  # Predict ALL
  models_1_2 <- list(text_train_results1, text_train_results2)
  all_predictions <- textPredictAll(
    models = models_1_2,
    word_embeddings = harmony_word_embeddings,
    x_append = Language_based_assessment_data_8[1:20, 5:8]
  )

  expect_equal(all_predictions[[1]][1], 11.89, tolerance = 0.1)
  expect_equal(all_predictions[[2]][1], 10.404, tolerance = 0.01)


  proj <- textProjection(
    words = Language_based_assessment_data_8[1],
    word_embeddings = harmony_word_embeddings$satisfactiontexts,
    single_word_embeddings = harmony_word_embeddings$singlewords_we,
    x = Language_based_assessment_data_8$hilstotal,
    y = Language_based_assessment_data_8$swlstotal,
    pca = NULL,
    aggregation = "mean",
    split = "quartile",
    word_weight_power = 1,
    min_freq_words_test = 0,
    Npermutations = 10,
    n_per_split = 5,
    seed = 1003
  )

  expect_that(proj[[1]][[1]][[1]][[1]], is_a("numeric"))
  expect_equal(proj[[1]][[1]][[1]][[1]], -0.402433, tolerance = 0.0000001)

  # help(textProjectionPlot)
  plot_proj <- textProjectionPlot(proj,
    explore_words = c("happy"),
    y_axes = TRUE
  )
  plot_proj$processed_word_data$n[1]
  expect_that(plot_proj$processed_word_data$n[1], is_a("numeric"))
  # expect_equal(plot_proj$processed_word_data$n[1], 2)
  # expect_equal(plot_proj$processed_word_data$n[1], 1)

  one_character <- plot_proj$processed_word_data %>%
    dplyr::filter(words == "-")
  expect_equal(one_character$n, 1)
  # help(textWordPrediction)
  #  pred_word <- textWordPrediction(words = Language_based_assessment_data_8[1],
  #                                  single_word_embeddings = harmony_word_embeddings$singlewords_we,
  #                                  x = Language_based_assessment_data_8$hilstotal)
  #
  #  plot_pred <- textPlot(pred_word,
  #                        explore_words = c("happy"),
  #                        y_axes = FALSE)
  #


  text_train_results <- textTrain(
    x = harmony_word_embeddings$satisfactiontexts,
    y = Language_based_assessment_data_8$hilstotal[1:20],
    cv_method = "cv_folds",
    outside_folds = 2,
    inside_folds = 2,
    outside_strata_y = NULL,
    inside_strata_y = NULL,
    # preprocess_PCA = c(0.20),
    preprocess_PCA = NA,
    penalty = 1e-16,
    multi_cores = "multi_cores_sys_default"
  )

  expect_that(text_train_results$results$estimate[1], is_a("numeric"))
  expect_gt(text_train_results$results$estimate[1], 0.3)
  expect_equal(text_train_results$results$estimate[[1]], 0.3273128, tolerance = 0.0001)


  # Predict
  hils_predicted_scores1 <- textPredict(
    model_info = text_train_results,
    word_embeddings = harmony_word_embeddings
  )

  expect_that(hils_predicted_scores1[[1]][1], is_a("numeric"))
  expect_equal(hils_predicted_scores1[[1]][1], 11.89219, tolerance = 0.000001)
})
