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

  # Assess the harmony in life on the same text (using the previously made embeddings)
  harmony_in_life_scores <- textAssess(
    model_info = "https://github.com/theharmonylab/open_models/raw/main/hilsswls2022/harmony_text_roberta-large_23_HILS_Kjell2022.rds",
    texts = text_to_assess,
    dim_name = FALSE)

  # Assigness implicit motives labels using fine-tuned models
  implicit_motive <- textClassify(
    model_info = "theharmonylab/implicit-motives-power-roberta-large",
    model_type = "finetuned",
    texts = text_to_assess)



})

