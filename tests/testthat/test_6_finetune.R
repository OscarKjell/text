
library(testthat)
library(tibble)
library(text)
library(stringi)

context("Testing tasks")


test_that("Task Fine-tuning tests", {
  skip_on_cran()

# It is removed for now since it does not complete in Windows without error
#
#  #help("textFineTuneTask")
#  unlink("./run_reg", recursive = TRUE)
  task_reg_test <- text::textFineTuneTask(
    Language_based_assessment_data_8[1:20 ,c("satisfactiontexts", "hilstotal")],
    model_name_or_path = "distilbert-base-uncased",
    is_regression = TRUE,
    num_train_epochs = 1,
    output_dir = "./run_reg",
    tokenizer_parallelism = TRUE)

  #
  testthat::expect_equal(task_reg_test,
                         "Completed - see results in the created output folder (output_dir)")

  # Remove the folder
  unlink("./run_reg", recursive = TRUE)


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


#  # help(textFineTuneDomain)
#  domain_test <- textFineTuneDomain(
#    text_data = Language_based_assessment_data_8[1][1:10,],
#    output_dir = "./runs_domain",
#    model_name_or_path = "distilbert-base-uncased",
#    num_train_epochs = 1
#    )
#
#  testthat::expect_equal(domain_test,
#                         "Completed - see results in the created output folder (output_dir)")
#
#  unlink("./runs_domain", recursive = TRUE)

  })


