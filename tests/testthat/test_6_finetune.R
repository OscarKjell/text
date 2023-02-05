
library(testthat)
library(tibble)
library(text)

context("Testing tasks")


test_that("Task Fine-tuning tests", {
  skip_on_cran()
textModels()
  help("textFineTuneTask")
  unlink("./run_reg", recursive = TRUE)
  text::textFineTuneTask(Language_based_assessment_data_8[,c("satisfactiontexts", "hilstotal")],
                         model_name_or_path = "distilroberta-base",
                         is_regression = TRUE,
                         output_dir = "./run_reg")

  # remove the folder
  unlink("./run_reg", recursive = TRUE)

  unlink("./run_clf", recursive = TRUE)
  text::textFineTuneTask(Language_based_assessment_data_8[,c("satisfactiontexts", "gender")],
                         model_name_or_path = "distilroberta-base",
                         is_regression = FALSE,
                         output_dir = "./run_clf",
                         label_names = c("male", "female"))

  # remove the folder
  unlink("./run_clf", recursive = TRUE)

})

