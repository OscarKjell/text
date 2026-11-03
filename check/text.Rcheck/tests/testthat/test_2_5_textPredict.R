library(testthat)
library(text)
library(tibble)
library(dplyr)

context("Prediction")

test_that("textPredictTest t-test and bootstrapped test", {
  skip_on_cran()
  set.seed(1)

  # Test data
  y1 <- runif(10)
  yhat1 <- runif(10)
  y2 <- runif(10)
  yhat2 <- runif(10)

  boot_test <- text::textPredictTest(
    y1,
    yhat1,
    y2,
    yhat2,
    method = "bootstrap_overlap",
    bootstraps_times = 10,
    seed = 6134

  )

  testthat::expect_that(boot_test, testthat::is_a("list"))
  testthat::expect_equal(boot_test$overlapp_p_value, 0.7398745, tolerance = 0.0001)

  boot_test2 <- text::textPredictTest(
    y1 = y1,
    yhat1,
    y2 = NULL,
    yhat2,
    method = "t-test",
    seed = 6134
  )

  testthat::expect_that(boot_test2, testthat::is_a("list"))
  testthat::expect_equal(boot_test2$Test$statistic[[1]], 0.233267, tolerance = 0.0001)
  testthat::expect_equal(boot_test2$Effect_size, 0.06198192, tolerance = 0.0001)


  # Test data
  set.seed(1)
  y1 <- sample(c(1, 2), 20, replace = T)
  yhat1 <- runif(20)

  y2 <- sample(c(1, 2), 20, replace = T)
  yhat2 <- runif(20)

  # Difference
  boot_test_auc1 <- text::textPredictTest(
    y1 = as.factor(y1),
    y2 = as.factor(y2),
    yhat1,
    yhat2,
    method = "bootstrap_difference",
    statistic = "auc",
    bootstraps_times = 100,
    seed = 6134
  )

  testthat::expect_equal(boot_test_auc1$AUC1_Mean, .4979517, tolerance = 0.0001)
  testthat::expect_equal(boot_test_auc1$AUC1_SD, .1443483, tolerance = 0.0001)
  testthat::expect_equal(boot_test_auc1$AUC1_CI_Lower[[1]], .2603434, tolerance = 0.0001)
  testthat::expect_equal(boot_test_auc1$AUC1_CI_Upper[[1]], .8199375, tolerance = 0.0001)
  testthat::expect_equal(boot_test_auc1$AUC2_Mean, .6816483, tolerance = 0.0001)
  testthat::expect_equal(boot_test_auc1$Diff_Mean, -0.1836966, tolerance = 0.0001)
  if (Sys.info()["sysname"] == "Darwin") {
    testthat::expect_equal(boot_test_auc1$P_Value_One_Tailed_Greater, .84, tolerance = 0.0001)
    testthat::expect_equal(boot_test_auc1$P_Value_One_Tailed_Less, 0.16, tolerance = 0.0001)
    testthat::expect_equal(boot_test_auc1$P_Value_Two_Tailed,  0.32, tolerance = 0.0001)

  }

  boot_test_auc2 <- text::textPredictTest(
    y1 = as.factor(y1),
    y2 = as.factor(y1),
    yhat1,
    yhat2,
    method = "bootstrap_difference",
    statistic = "auc",
    bootstraps_times = 100,
    seed = 6134
  )

  if (Sys.info()["sysname"] == "Darwin") {
  testthat::expect_equal(boot_test_auc2$P_Value_Two_Tailed, 0.78, tolerance = 0.0001)
  }

  # OVERLAP
  boot_test_auc1 <- text::textPredictTest(
    y1 = y1,
    y2 = y2,
    yhat1,
    yhat2,
    method = "bootstrap_overlap",
    statistic = "auc",
    bootstraps_times = 10,
    seed = 6134
  )

  testthat::expect_equal(boot_test_auc1$overlapp_p_value, .6622498, tolerance = 0.0001)


  boot_test_auc2 <- text::textPredictTest(
    y1 = y1,
    yhat1,
    y2 = y1,
    yhat2,
    method = "bootstrap_overlap",
    statistic = "auc",
    bootstraps_times = 10,
    seed = 6134
  )

  testthat::expect_equal(boot_test_auc2$overlapp_p_value, 0.5782996, tolerance = 0.0001)
})

