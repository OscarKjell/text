library(testthat)
library(text)
library(tibble)
library(dplyr)

context("textTrainN")


test_that("training with only x_append (without word embeddings)", {
  skip_on_cran()

  # n_cross_val = 2
  tibble_to_plot_a <- textTrainN(
    x = word_embeddings_4$texts$harmonytext,
    y = Language_based_assessment_data_8$hilstotal,
    sample_percents = c(25, 50),
    n_cross_val = 2,
    sampling_strategy = "random",
    use_same_penalty_mixture = FALSE
  )

  testthat::expect_that(tibble_to_plot_a$results, testthat::is_a("tbl"))


  testthat::expect_equal(tibble_to_plot_a$results$Test1[[1]], -0.5720588, tolerance = 0.001)
  testthat::expect_equal(tibble_to_plot_a$results$Test1[[2]], 0.2862313, tolerance = 0.001)
  testthat::expect_equal(tibble_to_plot_a$results$Test2[[1]], 0.06883358, tolerance = 0.001)
  testthat::expect_equal(tibble_to_plot_a$results$mean[[1]], -0.2516126, tolerance = 0.001)
  testthat::expect_equal(tibble_to_plot_a$results$std[[1]], 0.4531793, tolerance = 0.001)
  testthat::expect_equal(tibble_to_plot_a$results$std_err[[1]], 0.1433079, tolerance = 0.001)


  plot_object <- textTrainNPlot(
    results_data = list(tibble_to_plot_a$results),
    x_unit = "quantity",
    error_bar = "std_err"
  )

  testthat::expect_equal(length(plot_object$layers), 2)
  testthat::expect_true(ggplot2::is.ggplot(plot_object))

  tibble_to_plot_b <- textTrainN(
    x = word_embeddings_4$texts$harmonytext,
    y = Language_based_assessment_data_8$hilstotal,
    breaks = c(25, 50),
    n_cross_val = 2,
    sampling_strategy = "subsets",
    use_same_penalty_mixture = TRUE
  )

  testthat::expect_equal(tibble_to_plot_b$results$Test1[[1]], -0.5398959, tolerance = 0.001)
  testthat::expect_equal(tibble_to_plot_b$results$Test1[[2]], 0.1459097, tolerance = 0.001)
  testthat::expect_equal(tibble_to_plot_b$results$Test2[[1]], -0.1807492, tolerance = 0.001)
#  testthat::expect_equal(tibble_to_plot_b$results$mean[[1]], -0.4071123, tolerance = 0.001)
#  testthat::expect_equal(tibble_to_plot_b$results$std[[1]], 0.2076554, tolerance = 0.001)
#  testthat::expect_equal(tibble_to_plot_b$results$std_err[[1]], 0.0656664, tolerance = 0.001)

  plot_object_b <- textTrainNPlot(
    results_data = list(tibble_to_plot_a$results,
                        tibble_to_plot_b$results),
    x_unit = "quantity",
    error_bar = "std_err"
  )

  testthat::expect_equal(length(plot_object_b$layers), 2)
  testthat::expect_true(ggplot2::is.ggplot(plot_object_b))


  # n_cross_val = 1 help(textTrainN)
  tibble_to_plot_1 <- textTrainN(
    x = word_embeddings_4$texts[c("satisfactiontexts", "harmonytexts")],
    y = Language_based_assessment_data_8$hilstotal,
    sample_percents = c(25, 50),
    n_cross_val = 1,
    handle_word_embeddings = "concatenate",
    sampling_strategy = "random",
    use_same_penalty_mixture = FALSE
  )

  testthat::expect_that(tibble_to_plot_1$results, testthat::is_a("tbl"))


  testthat::expect_equal(tibble_to_plot_1$results$Test1[[1]], -0.2393972, tolerance = 0.0001)
  testthat::expect_equal(tibble_to_plot_1$results$Test1[[2]], 0.1855261, tolerance = 0.0001)
  testthat::expect_equal(tibble_to_plot_1$results$mean[[1]], -0.2393972, tolerance = 0.0001)


  plot_object_1 <- textTrainNPlot(
    results_data = list(tibble_to_plot_1$results),
  #  n_cross_val = 1,
    x_unit = "percent"
  )

  testthat::expect_equal(length(plot_object_1$layers), 2)
  testthat::expect_true(ggplot2::is.ggplot(plot_object_1))


  tibble_to_plot_2 <- textTrainN(
    x = word_embeddings_4$texts[c("satisfactiontexts", "harmonytexts")],
    y = Language_based_assessment_data_8$hilstotal,
    sample_percents = c(25, 50),
    n_cross_val = 1,
    handle_word_embeddings = "concatenate",
    sampling_strategy = "subsets",
    use_same_penalty_mixture = FALSE
  )

  testthat::expect_that(tibble_to_plot_2$results, testthat::is_a("tbl"))


  testthat::expect_equal(tibble_to_plot_2$results$Test1[[1]], -0.7918303, tolerance = 0.0001)
  testthat::expect_equal(tibble_to_plot_2$results$Test1[[2]], -0.2423784, tolerance = 0.0001)
  testthat::expect_equal(tibble_to_plot_2$results$mean[[1]], -0.7918303, tolerance = 0.0001)


  plot_object_1 <- textTrainNPlot(
    results_data = list(tibble_to_plot_2$results),
    #n_cross_val = 1,
    x_unit = "percent"
  )

  testthat::expect_equal(length(plot_object_1$layers), 2)
  testthat::expect_true(ggplot2::is.ggplot(plot_object_1))

})
