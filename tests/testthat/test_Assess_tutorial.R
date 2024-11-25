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
help(str_starts)
  lbam <- text::textLBAM()

  lbam %>%
    filter(str_starts(Construct_Concept_Behaviours, "Dep")) %>%
    dplyr::select(Construct_Concept_Behaviours, Name)

  #help(textAssess)
  # Example text to access
  text_to_assess = c(
    "I feel down and blue all the time.",
    "I feel great and have no worries that bothers me.")

  # Predict depression severity scores by downloading the pre-trained model, creating word embeddings, and applying the model to the embeddings
  depression_scores <- text::textPredict(
    model_info = "depression_text_phq9_roberta23_gu2024",
    texts = text_to_assess,
    dim_name = FALSE)

  model_Gu2024 <- readRDS("depressiontext_robertaL23_phq9_Gu2024.rds")
  model_Gu2024

  testthat::expect_that(depression_scores, testthat::is_a("tbl"))
  testthat::expect_equal(depression_scores[[1]][[1]], 17.16903, tolerance = 0.0001)

  # Assess the harmony in life on the same text (using the previously made embeddings)
  harmony_in_life_scores <- textAssess(
    model_info = "harmony_text_roberta23_kjell2022",
    texts = text_to_assess,
    dim_name = FALSE)

  testthat::expect_that(harmony_in_life_scores, testthat::is_a("tbl"))
  testthat::expect_equal(harmony_in_life_scores[[1]][[1]], 12.35453, tolerance = 0.0001)

  implicit_motive <- text::textClassify(
    model_info = "implicit_power_roberta_ft_nilsson2024",
    texts = text_to_assess,
    logging_level = "debug")

  testthat::expect_that(implicit_motive, testthat::is_a("tbl_df"))
  testthat::expect_equal(implicit_motive[[1]][[1]][[1]], 0, tolerance = 0.0001)
  testthat::expect_equal(implicit_motive$.pred_0[[1]], 0.9676571, tolerance = 0.0001)

})

