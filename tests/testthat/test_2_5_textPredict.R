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

  boot_test <- text::textPredictTest(y1,
                                     yhat1,
                                     y2,
                                     yhat2,
                                     method = "bootstrap",
                                     bootstraps_times = 10
  )

  testthat::expect_that(boot_test, testthat::is_a("list"))
  testthat::expect_equal(boot_test$overlapp_p_value, 0.7398745, tolerance = 0.0001)

  boot_test2 <- text::textPredictTest(
    y1 = y1,
    yhat1,
    y2 = NULL,
    yhat2,
    method = "t-test"
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


  boot_test_auc1 <- text::textPredictTest(
    y1 = y1,
    yhat1,
    y2 = y2,
    yhat2,
    method = "bootstrap",
    statistic = "auc",
    times = 10
  )

  testthat::expect_equal(boot_test_auc1$overlapp_p_value, 0.4530578, tolerance = 0.0001)


  boot_test_auc2 <- text::textPredictTest(
    y1 = y1,
    yhat1,
    y2 = y1,
    yhat2,
    method = "bootstrap",
    statistic = "auc",
    bootstraps_times = 10
  )

  testthat::expect_equal(boot_test_auc2$overlapp_p_value, 0.5782996, tolerance = 0.0001)
})

test_that("1. textPredict generates embeddings from text and 2. automatically codes implicit motives", {
  skip_on_cran()

  # Test data
  implicit_motive_data <- dplyr::mutate(.data = text::Language_based_assessment_data_8, participant_id = dplyr::row_number(), story_id = rep(1:5, each=8))

  #help(textPredict)
  predictions <- text::textPredict(
    texts = implicit_motive_data$satisfactiontexts,
    model_info = "implicit_power_roberta_large_L23_v1",
    participant_id = implicit_motive_data$participant_id,
    story_id = implicit_motive_data$story_id,
    dataset_to_merge_predictions = implicit_motive_data,
    previous_sentence = TRUE
    )

  testthat::expect_is(predictions$sentence_predictions$texts[1], "character")
  testthat::expect_equal(predictions$person_predictions$person_prob[40], 0.176565, tolerance = 0.0001)

  # Observe; when converting to numeric, zeros are replaced by ones, and ones are replaced by twos.

  # sentence predictions
  testthat::expect_equal(as.numeric(predictions$sentence_predictions$power_class[24]), 1, tolerance = 0.0001)
  testthat::expect_equal(sum(as.numeric(predictions$sentence_predictions$power_class)), 190, tolerance = 0.0001)

  # person-level predictions
  testthat::expect_equal(sum(as.numeric(predictions$person_predictions$participant_id[10])), 10, tolerance = 0.0001)
  testthat::expect_equal(sum(as.numeric(predictions$person_predictions$person_prob[10])), -1.144509, tolerance = 0.0001)

  # story-level predictions
  testthat::expect_equal(sum(as.numeric(predictions$story_predictions$story_id[5])), 5, tolerance = 0.0001)
  testthat::expect_equal(sum(as.numeric(predictions$story_predictions$story_prob[5])), 1.51721, tolerance = 0.0001)

  # dataset merging
  testthat::expect_equal(sum(as.numeric(predictions$dataset$person_prob[40])), 0.1765719, tolerance = 0.0001)
  testthat::expect_equal(sum(as.numeric(predictions$dataset$person_class[10])), 0, tolerance = 0.0001)
  testthat::expect_equal(sum(as.numeric(predictions$dataset$person_prob_no_wc_correction[5])), .4265997, tolerance = 0.0001)
  testthat::expect_equal(sum(as.numeric(predictions$dataset$person_class_no_wc_correction[5])), 0, tolerance = 0.0001)

  # Tests for the old way of merging dataset.
#  testthat::expect_equal(sum(as.numeric(predictions$dataset$.pred_0_1[40])), 0.9891365, tolerance = 0.0001)
#  testthat::expect_equal(sum(as.numeric(predictions$dataset$person_prob_2[10])), -1.144509, tolerance = 0.0001)
#  testthat::expect_equal(sum(as.numeric(predictions$dataset$story_prob_3[5])), 1.51721, tolerance = 0.0001)
})
