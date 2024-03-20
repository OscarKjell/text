
library(testthat)
library(tibble)
library(text)
library(stringi)

context("Testing tasks")


test_that("Task Fine-tuning tests", {
  skip_on_cran()

  text::textrpp_initialize()
# It is removed for now since it does not complete in Windows without error
#
#  #help("textFineTuneTask")
#  unlink("./run_reg", recursive = TRUE)
  if (Sys.info()["sysname"] != "Darwin") {

  task_reg_test <- text::textFineTuneTask(
    Language_based_assessment_data_8[1:20 ,c("satisfactiontexts", "hilstotal")],
    model_name_or_path = "distilbert-base-uncased",
    is_regression = TRUE,
    num_train_epochs = 1,
    output_dir = "./run_reg",
    pytorch_mps_high_watermark_ratio = TRUE,
    tokenizer_parallelism = TRUE)

  #
  testthat::expect_equal(task_reg_test,
                         "Completed - see results in the created output folder (output_dir)")

  # Remove the folder
  unlink("./run_reg", recursive = TRUE)
  unlink("./tests/testthat/logs", recursive = TRUE)
  }

#  textModels()
#  unlink("./run_clf", recursive = TRUE)
#  text::textFineTuneTask(Language_based_assessment_data_8[,c("satisfactiontexts",
#                                                             "gender")],
#                         model_name_or_path = "distilbert-base-uncased",
#                         is_regression = FALSE,
#                         output_dir = "./run_clf",
#                         label_names = c("male", "female")
#                         )
#
#  # remove the folder
#  unlink("./run_clf", recursive = TRUE)

  a <- Language_based_assessment_data_8[1][1:10,]
  s <- Language_based_assessment_data_8[1][1:10,]
  d <- Language_based_assessment_data_8[1][1:10,]
  f <- Language_based_assessment_data_8[1][1:10,]

  test_data <- dplyr::bind_rows(a, s, d, f, a, a, a, a, a)
  # help(textFineTuneDomain)
  domain_test <- textFineTuneDomain(
    text_data = test_data, # Language_based_assessment_data_8[1][1:10,],
    output_dir = "./runs_domain",
    model_name_or_path = "bert-base-uncased",
    num_train_epochs = 3
    )

  testthat::expect_equal(domain_test,
                         "Completed - see results in the created output folder (output_dir)")

  unlink("./runs_domain", recursive = TRUE)

  })


