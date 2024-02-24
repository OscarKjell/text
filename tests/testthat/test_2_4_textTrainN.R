
library(testthat)
library(text)
library(tibble)
library(dplyr)

context("textTrainN")





test_that("training with only x_append (without word embeddings)", {
  skip_on_cran()


  # n_cross_val = 1 help(textTrainN)
  tibble_to_plot_1 <- textTrainN(
    x = word_embeddings_4$texts[c("satisfactiontexts", "harmonytexts")],
    y = Language_based_assessment_data_8$hilstotal,
    sample_percents = c(25, 50),
    n_cross_val = 1,
    handle_word_embeddings = "concatenate"
  )

  testthat::expect_that(tibble_to_plot_1, testthat::is_a("tbl"))


  testthat::expect_equal(tibble_to_plot_1$Test1[[1]], -0.2393972, tolerance = 0.0001)
  testthat::expect_equal(tibble_to_plot_1$Test1[[2]], 0.1855261, tolerance = 0.0001)
  testthat::expect_equal(tibble_to_plot_1$mean[[1]], -0.2393972, tolerance = 0.0001)


  plot_object_1 <- textTrainNPlot(
    train_data = list(tibble_to_plot_1),
    n_cross_val = 1,
    x_unit = "percent"
  )

  testthat::expect_equal(length(plot_object_1$layers), 2)
  testthat::expect_true(ggplot2::is.ggplot(plot_object_1))


  # n_cross_val = 2
  tibble_to_plot <- textTrainN(
    x = word_embeddings_4$texts$harmonytext,
    y = Language_based_assessment_data_8$hilstotal,
    sample_percents = c(25, 50),
    n_cross_val = 2
  )

  testthat::expect_that(tibble_to_plot, testthat::is_a("tbl"))


  testthat::expect_equal(tibble_to_plot$Test1[[1]], -0.5720588, tolerance = 0.0001)
  testthat::expect_equal(tibble_to_plot$Test1[[2]], 0.2862313, tolerance = 0.0001)
  testthat::expect_equal(tibble_to_plot$Test2[[1]], -0.3158723, tolerance = 0.0001)
  testthat::expect_equal(tibble_to_plot$mean[[1]], -0.4439655, tolerance = 0.0001)
  testthat::expect_equal(tibble_to_plot$std[[1]], 0.1811512, tolerance = 0.0001)


  plot_object <- textTrainNPlot(
    train_data = list(tibble_to_plot),
    n_cross_val = 3,
    x_unit = "quantity"
  )

  testthat::expect_equal(length(plot_object$layers), 3)
  testthat::expect_true(ggplot2::is.ggplot(plot_object))

})
