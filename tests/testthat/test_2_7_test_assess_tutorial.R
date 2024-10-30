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
  depression_scores <- textPredict(
    model_info =
      "https://github.com/theharmonylab/open_models/raw/main/response_format_2024/depressiontext_robertaL23_phq9_Gu2024.rds",
    texts = text_to_assess,
    dim_name = FALSE)

  testthat::expect_that(depression_scores, testthat::is_a("tbl"))
  testthat::expect_equal(depression_scores[[1]][[1]], 17.16903, tolerance = 0.0001)

  # Assess the harmony in life on the same text (using the previously made embeddings)
  harmony_in_life_scores <- textAssess(
    model_info = "https://github.com/theharmonylab/open_models/raw/main/hilsswls2022/harmony_text_roberta-large_23_HILS_Kjell2022.rds",
    texts = text_to_assess,
    dim_name = FALSE)

  testthat::expect_that(harmony_in_life_scores, testthat::is_a("tbl"))
  testthat::expect_equal(harmony_in_life_scores[[1]][[1]], 12.35453, tolerance = 0.0001)

  # Assigness implicit motives labels using fine-tuned models
  implicit_motive <- textClassify(
    model_info = "theharmonylab/implicit-motives-power-roberta-large",
    model_type = "finetuned",
    texts = text_to_assess)

  testthat::expect_that(implicit_motive, testthat::is_a("tbl_df"))
  testthat::expect_equal(implicit_motive[[1]][[1]][[1]], 0, tolerance = 0.0001)
  testthat::expect_equal(implicit_motive$.pred_0[[1]], 0.9676571, tolerance = 0.0001)

})

