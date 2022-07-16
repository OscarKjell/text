

library(testthat)
library(tibble)
library(text)

context("Testing tasks")


test_that("textClassify", {
  skip_on_cran()

  # help(textClassify)
#  sen1 <- textClassify("I like you. I love you",
#                       model = "cardiffnlp/twitter-roberta-base-sentiment",
#                       return_incorrect_results = TRUE,
#                       return_all_scores = TRUE,
#                       function_to_apply = "none")

  sen2 <- textClassify("I like you. I love you",
                        model = "distilbert-base-uncased-finetuned-sst-2-english",
                        return_incorrect_results = TRUE,
                        return_all_scores = FALSE,
                        function_to_apply = "softmax")

  #expect_equal(sen1$score_x, 0.9998739, tolerance = 0.0001)
  expect_equal(sen2$score_x, 4.67502, tolerance = 0.001)

})

test_that("textClassify", {
  skip_on_cran()

  generated_text <- textGeneration("The meaning of life is")
  generated_text
  expect_that(generated_text$generatedtext, is_a("character"))

})
