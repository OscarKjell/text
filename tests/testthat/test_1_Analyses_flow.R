

library(testthat)
library(tibble)
library(text)

context("Big analyses flow")


#testing_size_model_NEW2_New <- textTrainRegression(wordembeddings4$harmonywords, Language_based_assessment_data_8$hilstotal)

#

test_that("Testing textEmbed as well as train", {
  skip_on_cran()

  library(text)

  harmony_word_embeddings <- textEmbed(Language_based_assessment_data_8[1],
    model = "bert-base-uncased",
    layers = c(0, 12),
    context_layers = 12,
    decontext_layers = 0
  )

  text_train_results <- textTrain(
    x = harmony_word_embeddings$harmonywords,
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
    new_data = harmony_word_embeddings$harmonywords
  )

  expect_that(hils_predicted_scores1$.pred[1], is_a("numeric"))
})
