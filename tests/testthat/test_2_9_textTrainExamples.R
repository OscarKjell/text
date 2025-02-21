# install.packages("devtools")

library(testthat)
library(tibble)
library(text)
library(topics)
library(vdiffr)
context("Testing tasks")


test_that("textClassify tests", {
  skip_on_cran()

  # Train word embeddings to assess depression
  # Examine the relationship between the embeddings and the depression scores
  hil_model <- textTrain(
    x = word_embeddings_4$texts["harmonytexts"],      # text embeddings as predictor
    y = Language_based_assessment_data_8["hilstotal"])  # depression scores as target

  # One-dimensional plot
  examples_1d <- textTrainExamples(
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
  examples_2d <- textTrainExamples(
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
                         c(13.56396, 20.61906, 16.08792, 21.32384, 20.67785), tolerance = 0.0001)
  testthat::expect_equal(examples_2d$examples$x_variable_grouped[1:10],
                         c(1, 1, 1, 1, 1, 1, 2, 3, 2, 2), tolerance = 1)
  testthat::expect_equal(examples_2d$examples$harmonytexts[13],
                         "My life is now in peaceful harmony.I do not let what others think or say or do affect me anymore.I have blocked out all negativity from my life.")


  # Error plot
  examples_error <- textTrainExamples(
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
    grid_legend_y_axes_label = NULL,
    #  x_axis_range = c(-1, 50),
    #  y_axis_range = c(-1, 50),
  )
  examples_error$error_plot
  examples_error$histogram_plot
  examples_error$scatter_plot
  examples_error$examples

  testthat::expect_equal(examples_error$examples$x_variable[1:5],
                         c(13.56396, 20.61906, 16.08792, 21.32384, 20.67785), tolerance = 0.0001)
  testthat::expect_equal(examples_error$examples$x_variable_grouped[1:10],
                         c(1, 1, 1, 1, 1, 1, 2, 3, 2, 2), tolerance = 1)
  testthat::expect_equal(examples_error$examples$text[6],
                         "I am in harmony, my house is quiet and peaceful except for the occasional fights with my fiance, but we are happy, and so is our cat")


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



