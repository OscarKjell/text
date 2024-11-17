library(text)
library(tibble)
library(testthat)

context("Testing Assess tutorial")

test_that("Assess tutorial", {
  skip_on_cran()

  # Install the text package (only needed the first time)
# install.packages("text")
# library(text)
# textrpp_install()
# textrpp_initialize()

  #help(textAssess)
  # Example text to access
  text_to_assess = c(
    "I feel down and blue all the time.",
    "I feel great and have no worries that bothers me.")

  # Predict depression severity scores by downloading the pre-trained model, creating word embeddings, and applying the model to the embeddings
  depression_scores <- text::textPredict(
    model_info = "depressiontext_robertaL23_phq9_Gu2024",
    texts = text_to_assess,
    dim_name = FALSE)

  testthat::expect_that(depression_scores, testthat::is_a("tbl"))
  testthat::expect_equal(depression_scores[[1]][[1]], 17.16903, tolerance = 0.0001)

  # Assess the harmony in life on the same text (using the previously made embeddings)
  harmony_in_life_scores <- textAssess(
    model_info = "harmony_text_roberta-large_23_HILS_Kjell2022",
    texts = text_to_assess,
    dim_name = FALSE)

  testthat::expect_that(harmony_in_life_scores, testthat::is_a("tbl"))
  testthat::expect_equal(harmony_in_life_scores[[1]][[1]], 12.35453, tolerance = 0.0001)

  # Assigning implicit motives labels using fine-tuned models
  implicit_motive <- text::textClassify(
    model_info = "implicit_power_fine_tuned_roberta",
    texts = text_to_assess)

  testthat::expect_that(implicit_motive, testthat::is_a("tbl_df"))
  testthat::expect_equal(implicit_motive[[1]][[1]][[1]], 0, tolerance = 0.0001)
  testthat::expect_equal(implicit_motive$.pred_0[[1]], 0.9676571, tolerance = 0.0001)

})

