
library(testthat)
library(tibble)
library(text)

context("Testing tasks")


test_that("Task Fine-tuning tests", {
  skip_on_cran()

  #help("textFineTuneTask")
  unlink("./run_reg", recursive = TRUE)
  test <- text::textFineTuneTask(Language_based_assessment_data_8[,c("satisfactiontexts",
                                                             "hilstotal")],
                         model_name_or_path = "distilbert-base-uncased",
                         is_regression = TRUE,
                         output_dir = "./run_reg")

  # This is because some systems show this as: "\033[0;32mCompleted\033[0m"
  test1 <- stringr::str_extract(test, "Completed")
  testthat::expect_equal(test1, "Completed")

  # Remove the folder
  unlink("./run_reg", recursive = TRUE)
#  textModels()
#  unlink("./run_clf", recursive = TRUE)
#  text::textFineTuneTask(Language_based_assessment_data_8[,c("satisfactiontexts",
#                                                             "gender")],
#                         model_name_or_path = "distilbert-base-uncased",
#                         is_regression = FALSE,
#                         output_dir = "./run_clf",
#                         label_names = c("male", "female"))
#
#  # remove the folder
#  unlink("./run_clf", recursive = TRUE)

})

