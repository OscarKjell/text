


library(testthat)
library(text)
library(tibble)

context("Big analyses flow")

test_that("Testing textEmbed as well as train", {

  harmony_word_embeddings <- textEmbed(Language_based_assessment_data_8[1],
                                       model = "bert-base-uncased",
                                       layers=11)

  text_train_results <- textTrain(x = harmony_word_embeddings$harmonywords,
                                  y = Language_based_assessment_data_8$hilstotal,
                                  preprocess_PCA = c(0.20),
                                  penalty = 1e-16)

  expect_that(text_train_results$results$estimate[1], is_a("numeric"))
  expect_gt(text_train_results$results$estimate[1], 0.3)


  # Predict
  hils_predicted_scores1 <- textPredict(model_info = text_train_results,
                                       new_data = harmony_word_embeddings$harmonywords)
  #hils_predicted_scores1

  expect_that(hils_predicted_scores1$.pred[1], is_a("numeric"))
})

