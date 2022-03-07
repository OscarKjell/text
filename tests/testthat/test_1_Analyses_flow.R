

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

  harmony_word_embeddings <- textEmbed(Language_based_assessment_data_8[1],
    model = "bert-base-uncased",
    layers = c(11),
    context_layers = c(11),
    decontext_layers = c(11),
  )

  # help(textProjection)
  proj <- textProjection(
    words = Language_based_assessment_data_8[1],
    word_embeddings = harmony_word_embeddings$satisfactiontexts,
    single_word_embeddings = harmony_word_embeddings$singlewords_we,
    x = Language_based_assessment_data_8$hilstotal,
    y = NULL,
    pca = NULL,
    aggregation = "mean",
    split = "quartile",
    word_weight_power = 1,
    min_freq_words_test = 0,
    Npermutations = 10,
    n_per_split = 5,
    seed = 1003)
  expect_that(proj[[1]][[1]][[1]][[1]], is_a("numeric"))

  # help(text)
  plot_proj <- textProjectionPlot(proj,
                        explore_words = c("happy joy", "happy joy"))

  expect_that(plot_proj$processed_word_data$n[1], is_a("numeric"))

  text_train_results <- textTrain(
    x = harmony_word_embeddings$satisfactiontexts,
    y = Language_based_assessment_data_8$hilstotal,
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


  # Predict
  hils_predicted_scores1 <- textPredict(
    model_info = text_train_results,
    new_data = harmony_word_embeddings$satisfactiontexts
  )

  expect_that(hils_predicted_scores1$.pred[1], is_a("numeric"))
})
