library(tibble)
library(dplyr)
library(text)
library(testthat)

context("textPlot Functions")

test_that("textProjection MEAN and PCA produces a tibble with character variable and numeric variable.", {
  skip_on_cran()

  # Pre-processing data for plotting
  df_for_plotting1 <- text::textProjection(
    words = Language_based_assessment_data_8$harmonywords[1:10],
    word_embeddings = word_embeddings_4$texts$harmonywords[1:10, ],
    word_embeddings_4$word_types,
    Language_based_assessment_data_8$hilstotal[1:10],
    split = "mean",
    Npermutations = 2,
    n_per_split = 1,
    pca = 2
  )

  testthat::expect_true(tibble::is_tibble(df_for_plotting1[[2]]))
  testthat::expect_is(df_for_plotting1[[2]]$words[1], "character")
  testthat::expect_is(df_for_plotting1[[2]]$n[1], "numeric")
  testthat::expect_equal(df_for_plotting1[[2]]$dot.x[1], -3.847934, tolerance = 0.001)
})

test_that("textProjection with QUARTILE, 0.9 PCA and
          2-dimensions produces a tibble with character
          variable and numeric variable.", {
  skip_on_cran()

  # Pre-processing data for plotting
  df_for_plotting2 <- text::textProjection(
    words = Language_based_assessment_data_8$harmonywords[1:12],
    word_embeddings = word_embeddings_4$texts$harmonywords[1:12, ],
    word_types_embeddings = word_embeddings_4$word_types,
    Language_based_assessment_data_8$hilstotal[1:12],
    Language_based_assessment_data_8$swlstotal[1:12],
    split = "quartile",
    Npermutations = 2,
    n_per_split = 3,
    pca = 0.9
  )

  expect_true(tibble::is_tibble(df_for_plotting2[[2]]))
  expect_is(df_for_plotting2[[2]]$words[1], "character")
  expect_is(df_for_plotting2[[2]]$n[1], "numeric")
  expect_equal(df_for_plotting2[[2]]$dot.x[3], 4.948596, tolerance = 0.001)
})

test_that("textProjectionPlot 1-DIMENSIONS produces a plot", {
  skip_on_cran()

  # remotes::install_github("tidyverse/ggplot2", ref = remotes::github_pull("5592"))
  # Dot Product Projection Plot help(textProjectionPlot)
  p1 <- text::textProjectionPlot(
    word_data = DP_projections_HILS_SWLS_100,
    k_n_words_to_test = TRUE,
    min_freq_words_test = 1,
    plot_n_words_square = 3,
    plot_n_words_p = 3,
    plot_n_word_extreme = 1,
    plot_n_word_frequency = 1,
    plot_n_words_middle = 1,
    # x_axes = TRUE,
    y_axes = FALSE,
    p_alpha = 0.05,
    title_top = " Dot Product Projection (DPP)",
    x_axes_label = "Low vs. High HILS score",
    y_axes_label = "Low vs. High SWLS score",
    p_adjust_method = "bonferroni",
    scale_y_axes_lim = NULL,
    group_embeddings1 = F,
    group_embeddings2 = F,
    projection_embedding = F
  )

  expect_true(ggplot2::is.ggplot(p1$final_plot))
  expect_equal(p1$processed_word_data$dot.y[1], 2.988819, tolerance = 0.00001)
})


test_that("textProjectionPlot 1-DIMENSIONS produces a plot", {
  skip_on_cran()

  # Dot Product Projection Plot
  p2 <- text::textProjectionPlot(
    word_data = DP_projections_HILS_SWLS_100,
    k_n_words_to_test = TRUE,
    min_freq_words_test = 1,
    plot_n_words_square = 3,
    plot_n_words_p = 3,
    plot_n_word_extreme = 1,
    plot_n_word_frequency = 1,
    plot_n_words_middle = 1,
    # x_axes = FALSE,
    y_axes = TRUE,
    p_alpha = 0.05,
    title_top = " Dot Product Projection (DPP)",
    x_axes_label = "Low vs. High HILS score",
    y_axes_label = "Low vs. High SWLS score",
    p_adjust_method = "bonferroni",
    scale_y_axes_lim = NULL,
    group_embeddings1 = T,
    group_embeddings2 = T,
    projection_embedding = T,
  )

  expect_true(ggplot2::is.ggplot(p2$final_plot))
  expect_equal(p2$processed_word_data$x_plotted[1], 1.415753, tolerance = 0.0001)
})


test_that("textProjectionPlot 2-DIMENSIONS produces a plot", {
  skip_on_cran()

  # Dot Product Projection Plot
  p3 <- text::textProjectionPlot(
    word_data = DP_projections_HILS_SWLS_100,
    k_n_words_to_test = FALSE,
    min_freq_words_test = 1,
    plot_n_words_square = 3,
    plot_n_words_p = 3,
    plot_n_word_extreme = 1,
    plot_n_word_frequency = 1,
    plot_n_words_middle = 1,
    # x_axes = TRUE,
    y_axes = TRUE,
    p_alpha = 0.05,
    title_top = " Dot Product Projection (DPP)",
    x_axes_label = "Low vs. High HILS score",
    y_axes_label = "Low vs. High SWLS score",
    p_adjust_method = "fdr",
    scale_y_axes_lim = NULL
  )

  expect_true(ggplot2::is.ggplot(p3$final_plot))
  expect_equal(p3$processed_word_data$x_plotted[2], 0.7323493, tolerance = 0.0001)
})


test_that("textCentrality produces a tibble with character variable and numeric variable.", {
  skip_on_cran()

  df_for_plotting <- text::textCentrality(
    Language_based_assessment_data_8$harmonywords[1:2],
    word_embeddings_4$texts$harmonywords[1:2, ],
    word_embeddings_4$word_types,
    method = "euclidean"
  )

  expect_is(df_for_plotting$words[1], "character")
  expect_is(df_for_plotting$n[1], "integer")
  expect_true(tibble::is_tibble(df_for_plotting))
  expect_equal(df_for_plotting$n[1], 1)

  plot_c <- text::textCentralityPlot(df_for_plotting,
    x_axes = "central_semantic_similarity"
  )

  expect_true(ggplot2::is.ggplot(plot_c$final_plot))
})

test_that("textCentralityPlot produces a plot.", {
  skip_on_cran()

  # Plot help(textCentralityPlot)
  centrality_plot <- text::textCentralityPlot(
    word_data = centrality_data_harmony,
    min_freq_words_test = 10,
    plot_n_word_extreme = 10,
    plot_n_word_frequency = 10,
    plot_n_words_middle = 10,
    titles_color = "#61605e",
    # x_axes = "central_cosine",

    title_top = "Semantic Centrality Plot",
    x_axes_label = "Semantic Centrality",
    word_font = NULL,
    centrality_color_codes = c("#EAEAEA", "#85DB8E", "#398CF9", "#9e9d9d"),
    word_size_range = c(3, 8),
    point_size = 0.5,
    arrow_transparency = 0.1,
    points_without_words_size = 0.5,
    points_without_words_alpha = 0.5
  )

  expect_true(ggplot2::is.ggplot(centrality_plot$final_plot))
  expect_equal(centrality_plot$processed_word_data$n[2], 21)
  expect_equal(centrality_plot$processed_word_data$central_semantic_similarity[2], 0.5079464, tolerance = 0.00001)
})



test_that("textPCA produces a tibble with character variable and numeric variable.", {
  skip_on_cran()


  df_for_plotting2d <- textPCA(
    words = Language_based_assessment_data_8$harmonywords,
    word_types_embeddings = word_embeddings_4$word_types
  )


  expect_is(df_for_plotting2d$words[1], "character")
  expect_is(df_for_plotting2d$n[1], "integer")
  expect_true(tibble::is_tibble(df_for_plotting2d))
  expect_equal(df_for_plotting2d$n[1], 2)
})

test_that("textPCAPlot produces a plot.", {
  skip_on_cran()

  # Plot
  principle_component_plot_projection <- textPCAPlot(PC_projections_satisfactionwords_40)
  # principle_component_plot_projection

  expect_true(ggplot2::is.ggplot(principle_component_plot_projection$final_plot))
  expect_equal(principle_component_plot_projection$processed_word_data$n[2], 2)
})
