
library(text)
library(tibble)
library(testthat)

context("Testing Domain Compare")

test_that("Testing Domain Compare", {
  skip_on_cran()

  training_language <- textTokenizeAndCount(Language_based_assessment_data_8["harmonytexts"])
  testthat::expect_that(training_language, testthat::is_a("tbl_df"))
  testthat::expect_equal(training_language$word[[1]], "i")
  testthat::expect_equal(training_language$n[[1]], 196, tolerance = 0.0001)

  assess_language <- textTokenizeAndCount(Language_based_assessment_data_8["satisfactiontexts"])


  comparison <- textDomainCompare(training_language, assess_language)

  testthat::expect_that(comparison, testthat::is_a("list"))
  testthat::expect_equal(comparison$overlapp_percentage[[1]], 0.4917127, tolerance = 0.0001)
  testthat::expect_equal(comparison$test_recall_percentage[[1]], 0.6267606, tolerance = 0.0001)
  testthat::expect_equal(comparison$cosine_similarity[[1]], 0.9523402, tolerance = 0.0001)
  testthat::expect_equal(comparison$cosine_similarity_standardised[[1]], 0.9423569, tolerance = 0.0001)

})


