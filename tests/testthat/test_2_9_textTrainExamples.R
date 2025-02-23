# install.packages("devtools")

library(testthat)
library(tibble)
library(text)
library(topics)
library(vdiffr)
context("Testing tasks")


test_that("textTrainExamples tests", {
  testthat::skip_on_os(c("linux", "windows"))  # Skip on Ubuntu (Linux) and Windows
  skip_on_cran()

  # Train word embeddings to assess depression
  # Examine the relationship between the embeddings and the depression scores
  hil_model <- text::textTrain(
    x = word_embeddings_4$texts["harmonytexts"],      # text embeddings as predictor
    y = Language_based_assessment_data_8["hilstotal"])  # depression scores as target



  # One-dimensional plot
  examples_1d <- text::textTrainExamples(
    text = Language_based_assessment_data_8["harmonytexts"],
    x_variable = hil_model$predictions$predictions,
    y_variable = NULL,
    n_examples = 5,
    jitter = 0.005,
    filter_words = NULL,
  distribution_color = c("darkred", "gray", "darkgreen")
  )
  examples_1d
  testthat::expect_equal(examples_1d$examples$x_variable[1:5],
                         c(13.55430, 13.56396, 16.08792, 18.11642, 18.12162), tolerance = 0.0001)
  testthat::expect_equal(examples_1d$examples$x_variable_grouped[1:10],
                         c(1, 1, 1, 1, 1, 3, 3, 3, 3, 3), tolerance = 1)
  testthat::expect_equal(examples_1d$examples$harmonytexts[1],
                         "I am in harmony, my house is quiet and peaceful except for the occasional fights with my fiance, but we are happy, and so is our cat")

  # Two-dimensional plot
  examples_2d <- text::textTrainExamples(
    text = Language_based_assessment_data_8["harmonytexts"],
    x_variable = hil_model$predictions$predictions,
    y_variable = hil_model$predictions$y,
    #  type = "prediction_errors",
    n_examples = 2,
    jitter = 0.05,
    filter_words = NULL,
    target_color = "darkgreen",
    predictions_color = "darkblue",
    error_color = "darkred"
  )

  examples_2d$scatter_plot
  examples_2d$examples
  testthat::expect_equal(examples_2d$examples$x_variable[1:5],
                         c(20.67785, 13.55430, 25.12102, 24.63476, 35.97318), tolerance = 0.0001)
  testthat::expect_equal(examples_2d$examples$x_variable_grouped[1:10],
                         c(1, 1, 1, 1, 1, 1, 2, 3, 2, 2), tolerance = 1)
  testthat::expect_equal(examples_2d$examples$harmonytexts[13],
                         "I wish I was more at peace but the imbalance in my life is hurting those chances. I don't know what to do with the relationships I currently have. I feel incompatible but somehow unable to get rid of those relationships. Its almost like I enjoy it for some odd reason. I may just like arguing and therefore seem to stay in this depressing cycle.")


  # Error plot
  examples_error <- text::textTrainExamples(
    text = Language_based_assessment_data_8$harmonytexts, #Language_based_assessment_data_8["harmonytexts"],
    x_variable = hil_model$predictions["predictions"],
    y_variable = hil_model$predictions["y"],
    type = "prediction_errors",
    n_examples = 2,
    jitter = 0.05,
    filter_words = NULL,
    target_color = "darkgreen",
    predictions_color = "darkblue",
    error_color = "darkred",
    grid_legend_x_axes_label = NULL,
    grid_legend_y_axes_label = NULL
  )
  examples_error$error_plot
  examples_error$histogram_plot
  examples_error$scatter_plot
  examples_error$examples

  testthat::expect_equal(examples_error$examples$x_variable[1:5],
                         c(20.67785, 13.55430, 25.12102, 24.63476, 35.97318), tolerance = 0.0001)
  testthat::expect_equal(examples_error$examples$x_variable_grouped[1:10],
                         c(1, 1, 1, 1, 1, 1, 2, 3, 2, 2), tolerance = 1)
  testthat::expect_equal(examples_error$examples$text[6],
                         "I am contented with the way things are going in my life. I am fulfilled as a wife, mother, and teacher. I am satisfied with my appearance and how people perceive me.")


  examples_error_plot <- function() {
    examples_error$error_plot
  }
  expect_doppelganger("examples_error_plot", examples_error_plot())

  examples_histogram_plot <- function() {
    examples_error$histogram_plot
  }
  expect_doppelganger("examples_histogram_plot", examples_histogram_plot())

  examples_scatter_plot <- function() {
    examples_error$scatter_plot
  }
  expect_doppelganger("examples_scatter_plot", examples_scatter_plot())


})



