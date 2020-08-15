


library(testthat)
library(text)
library(tibble)

context("Big analyses flow")

test_that("Testing textEmbed as well as train", {

  harmony_word_embeddings <- textEmbed(Language_based_assessment_data_8$harmonywords,
                                       model = "bert-base-uncased",
                                       layers=11)

  text_train_results <- textTrain(harmony_word_embeddings$harmonywords,
                                  Language_based_assessment_data_8$hilstotal,
                                  preprocess_PCA_thresh = c(0.20),
                                  penalty = 1e-16)

  text_train_results$correlation$estimate[1]

  expect_that(text_train_results$correlation$estimate[1], is_a("numeric"))
  expect_gt(text_train_results$correlation$estimate[1], 0.5)
})

