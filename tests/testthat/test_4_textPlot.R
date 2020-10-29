
library(tibble)
library(dplyr)
library(testthat)

context("textPlot Functions")

test_that("textProjection MEAN and PCA produces a tibble with character variable and numeric variable.", {
  skip_on_cran()

  # Data
  wordembeddings <- wordembeddings4
  data <- Language_based_assessment_data_8
  # Pre-processing data for plotting
  df_for_plotting <- textProjection(data$harmonywords[1:5],
    wordembeddings$harmonywords[1:5, ],
    wordembeddings$singlewords_we,
    data$hilstotal[1:5],
    split = "mean",
    Npermutations = 2,
    n_per_split = 1,
    pca = 2
  )

  expect_true(tibble::is_tibble(df_for_plotting))
  expect_is(df_for_plotting$words[1], "character")
  expect_is(df_for_plotting$n[1], "integer")
})

test_that("textProjection with QUARTILE, 0.9 PCA and 2-dimensions produces a tibble with character variable and numeric variable.", {
  skip_on_cran()

  # Data
  wordembeddings <- wordembeddings4
  data <- Language_based_assessment_data_8
  harmonywords <- data$harmonywords[1:10]
  harmonywordembeddings <- wordembeddings$harmonywords[1:10, ]
  wordembeddingssinglewords_we <- wordembeddings$singlewords_we
  hilstotal <- data$hilstotal[1:10]
  swlstotal <- data$swlstotal[1:10]
  # Pre-processing data for plotting
  df_for_plotting <- textProjection(harmonywords,
    harmonywordembeddings,
    wordembeddingssinglewords_we,
    hilstotal,
    swlstotal,
    split = "quartile",
    Npermutations = 2,
    n_per_split = 3,
    pca = 0.9
  )

  expect_true(tibble::is_tibble(df_for_plotting))
  expect_is(df_for_plotting$words[1], "character")
  expect_is(df_for_plotting$n[1], "integer")
})

test_that("textProjectionPlot 1-DIMENSIONS produces a plot", {
  skip_on_cran()

  # Dot Product Projection Plot
  p <- textProjectionPlot(
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
    scale_y_axes_lim = NULL
  )

  expect_true(ggplot2::is.ggplot(p$final_plot))
})


test_that("textProjectionPlot 1-DIMENSIONS produces a plot", {
  skip_on_cran()

  # Dot Product Projection Plot
  p <- textProjectionPlot(
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
    scale_y_axes_lim = NULL
  )

  expect_true(ggplot2::is.ggplot(p$final_plot))
})


test_that("textProjectionPlot 2-DIMENSIONS produces a plot", {
  skip_on_cran()

  # Dot Product Projection Plot
  p <- textProjectionPlot(
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

  expect_true(ggplot2::is.ggplot(p$final_plot))
})


test_that("textCentrality produces a tibble with character variable and numeric variable.", {
  skip_on_cran()

  wordembeddings <- wordembeddings4
  data <- Language_based_assessment_data_8
  df_for_plotting <- textCentrality(
    data$harmonywords[1:2],
    wordembeddings$harmonywords[1:2, ],
    wordembeddings$singlewords_we
  )


  expect_is(df_for_plotting$words[1], "character")
  expect_is(df_for_plotting$n[1], "integer")
  expect_true(tibble::is_tibble(df_for_plotting))
})

test_that("textCentralityPlot produces a plot.", {
  skip_on_cran()

  # Plot
  centrality_plot <- textCentralityPlot(
    word_data = centrality_data_harmony,
    min_freq_words_test = 10,
    plot_n_word_extreme = 10,
    plot_n_word_frequency = 10,
    plot_n_words_middle = 10,
    titles_color = "#61605e",
    x_axes = "central_cosine",

    title_top = "Semantic Centrality Plot",
    x_axes_label = "Semantic Centrality",

    word_font = NULL,
    centrality_color_codes = c("#EAEAEA", "#85DB8E", "#398CF9", "#000000"),
    word_size_range = c(3, 8),
    point_size = 0.5,
    arrow_transparency = 0.1,
    points_without_words_size = 0.5,
    points_without_words_alpha = 0.5
  )

  expect_true(ggplot2::is.ggplot(centrality_plot$final_plot))
})



test_that("textCentrality produces a tibble with character variable and numeric variable.", {
  skip_on_cran()


  df_for_plotting2d <- textPCA(
    words = Language_based_assessment_data_8$harmonywords,
    single_wordembeddings = wordembeddings4$singlewords_we
  )
  df_for_plotting2d


  expect_is(df_for_plotting2d$words[1], "character")
  expect_is(df_for_plotting2d$n[1], "integer")
  expect_true(tibble::is_tibble(df_for_plotting2d))
})

test_that("textCentralityPlot produces a plot.", {
  skip_on_cran()

  # Plot
  principle_component_plot_projection <- textPCAPlot(PC_projections_satisfactionwords_40)
  # principle_component_plot_projection

  expect_true(ggplot2::is.ggplot(principle_component_plot_projection$final_plot))
})
