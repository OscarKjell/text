library(testthat)
library(text)
library(tibble)
library(dplyr)

context("textTrainN")


test_that("training with only x_append (without word embeddings)", {
  skip_on_cran()

  if (Sys.getenv("GITHUB_ACTIONS") == "true") {
    multi_cores = FALSE
  } else {
    multi_cores = "multi_cores_sys_default"
  }


  print("training with only x_append (without word embeddings)")
  # n_cross_val = 2
  tibble_to_plot_a <- text::textTrainN(
    x = word_embeddings_4$texts$harmonytext,
    y = Language_based_assessment_data_8$hilstotal,
    sample_percents = c(40, 60),
    n_cross_val = 2,
    sampling_strategy = "random",
    use_same_penalty_mixture = FALSE,
    multi_cores = multi_cores
  )

  testthat::expect_that(tibble_to_plot_a$results, testthat::is_a("tbl"))

# Old (validtion_split)
#  testthat::expect_equal(tibble_to_plot_a$results$Test1[[1]], 0.476083, tolerance = 0.001)
#  testthat::expect_equal(tibble_to_plot_a$results$Test1[[2]], 0.3156565, tolerance = 0.001)
#  testthat::expect_equal(tibble_to_plot_a$results$Test2[[1]], 0.2439266, tolerance = 0.001)
#  testthat::expect_equal(tibble_to_plot_a$results$mean[[1]], 0.3600048, tolerance = 0.001)
#  testthat::expect_equal(tibble_to_plot_a$results$std[[1]], 0.1641594, tolerance = 0.001)
#  testthat::expect_equal(tibble_to_plot_a$results$std_err[[1]], 0.04103986, tolerance = 0.001)


  testthat::expect_equal(tibble_to_plot_a$results$Test1[[1]], 0.1665256, tolerance = 0.001)
  testthat::expect_equal(tibble_to_plot_a$results$Test1[[2]], 0.2451203, tolerance = 0.001)
  testthat::expect_equal(tibble_to_plot_a$results$Test2[[1]], 0.2439266, tolerance = 0.001)
  testthat::expect_equal(tibble_to_plot_a$results$mean[[1]], 0.2052261, tolerance = 0.001)
  testthat::expect_equal(tibble_to_plot_a$results$std[[1]], 0.05473071, tolerance = 0.001)
  testthat::expect_equal(tibble_to_plot_a$results$std_err[[1]], 0.01368268, tolerance = 0.001)



  plot_object <- textTrainNPlot(
    results_data = list(tibble_to_plot_a$results),
    x_unit = "quantity",
    error_bar = "std_err"
  )

  testthat::expect_equal(length(plot_object$layers), 3)
  testthat::expect_true(ggplot2::is.ggplot(plot_object))

  tibble_to_plot_b <- textTrainN(
    x = word_embeddings_4$texts$harmonytext,
    y = Language_based_assessment_data_8$hilstotal,
    # breaks = c(25, 50),
    sample_percents = c(40, 60),
    n_cross_val = 2,
    sampling_strategy = "subsets",
    use_same_penalty_mixture = TRUE
  )

  # Old
#  testthat::expect_equal(tibble_to_plot_b$results$Test1[[1]], -0.05730675, tolerance = 0.001)
#  testthat::expect_equal(tibble_to_plot_b$results$Test1[[2]], 0.3331562, tolerance = 0.001)
#  testthat::expect_equal(tibble_to_plot_b$results$Test2[[1]], -0.8474074, tolerance = 0.001)
  testthat::expect_equal(tibble_to_plot_b$results$Test1[[1]], -0.2614036, tolerance = 0.001)
  testthat::expect_equal(tibble_to_plot_b$results$Test1[[2]], 0.3869961, tolerance = 0.001)
  testthat::expect_equal(tibble_to_plot_b$results$Test2[[1]], -0.8474074, tolerance = 0.001)


  plot_object_b <- textTrainNPlot(
    results_data = list(tibble_to_plot_a$results,
                        tibble_to_plot_b$results),
    x_unit = "quantity",
    error_bar = "std_err"
  )

  testthat::expect_equal(length(plot_object_b$layers), 6)
  testthat::expect_true(ggplot2::is.ggplot(plot_object_b))


  # n_cross_val = 1 help(textTrainN)
  tibble_to_plot_1 <- textTrainN(
    x = word_embeddings_4$texts[c("satisfactiontexts", "harmonytexts")],
    y = Language_based_assessment_data_8$hilstotal,
    sample_percents = c(50, 60),
    n_cross_val = 1,
    handle_word_embeddings = "concatenate",
    sampling_strategy = "random",
    use_same_penalty_mixture = FALSE
  )

  testthat::expect_that(tibble_to_plot_1$results, testthat::is_a("tbl"))

# OLD
#  testthat::expect_equal(tibble_to_plot_1$results$Test1[[1]], -0.2346003, tolerance = 0.0001)
#  testthat::expect_equal(tibble_to_plot_1$results$Test1[[2]], -0.1531245, tolerance = 0.0001)
#  testthat::expect_equal(tibble_to_plot_1$results$mean[[1]],  -0.2346003, tolerance = 0.0001)

  testthat::expect_equal(tibble_to_plot_1$results$Test1[[1]], -0.2693465, tolerance = 0.0001)
  testthat::expect_equal(tibble_to_plot_1$results$Test1[[2]], -0.05520974, tolerance = 0.0001)
  testthat::expect_equal(tibble_to_plot_1$results$mean[[1]],  -0.2693465, tolerance = 0.0001)


  plot_object_1 <- textTrainNPlot(
    results_data = list(tibble_to_plot_1$results),
  #  n_cross_val = 1,
    x_unit = "percent"
  )

  testthat::expect_equal(length(plot_object_1$layers), 3)
  testthat::expect_true(ggplot2::is.ggplot(plot_object_1))


  tibble_to_plot_2 <- textTrainN(
    x = word_embeddings_4$texts[c("satisfactiontexts", "harmonytexts")],
    y = Language_based_assessment_data_8$hilstotal,
    sample_percents = c(50, 60),
    n_cross_val = 1,
    handle_word_embeddings = "concatenate",
    sampling_strategy = "subsets",
    use_same_penalty_mixture = FALSE
  )

  testthat::expect_that(tibble_to_plot_2$results, testthat::is_a("tbl"))

# OLD
#  testthat::expect_equal(tibble_to_plot_2$results$Test1[[1]], 0.08913115, tolerance = 0.0001)
#  testthat::expect_equal(tibble_to_plot_2$results$Test1[[2]], 0.4013327, tolerance = 0.0001)
#  testthat::expect_equal(tibble_to_plot_2$results$mean[[1]], 0.08913115, tolerance = 0.0001)

  testthat::expect_equal(tibble_to_plot_2$results$Test1[[1]], 0.05921353, tolerance = 0.0001)
  testthat::expect_equal(tibble_to_plot_2$results$Test1[[2]], 0.4940671, tolerance = 0.0001)
  testthat::expect_equal(tibble_to_plot_2$results$mean[[1]], 0.05921353, tolerance = 0.0001)


  plot_object_1 <- textTrainNPlot(
    results_data = list(tibble_to_plot_2$results),
    #n_cross_val = 1,
    x_unit = "percent"
  )

  testthat::expect_equal(length(plot_object_1$layers), 3)
  testthat::expect_true(ggplot2::is.ggplot(plot_object_1))

})
