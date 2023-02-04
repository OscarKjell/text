
library(testthat)
library(tibble)
library(text)

context("Testing tasks")


test_that("Task Fine-tuning tests", {
  skip_on_cran()

  text::textFineTuneTask(Language_based_assessment_data_8[,c("satisfactiontexts", "hilstotal")],
                   is_regression = TRUE,
                   output_dir = "./run_reg")

  text::textFineTuneTask(Language_based_assessment_data_8[,c("satisfactiontexts", "gender")],
                   is_regression = FALSE,
                   output_dir = "./run_clf",
                   label_names = c("male", "female"))

})

