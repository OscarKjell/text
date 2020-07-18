#Test your package with Ctrl/Cmd + Shift + T or devtools::test().

library(tibble)
library(dplyr)
#test_check("text")

context("textPlot Functions")

test_that("textProjectionData produces a tibble with character variable and numeric variable.", {
  #Data
  wordembeddings <- wordembeddings4_10
  data <- Language_based_assessment_data_8_10
  # Pre-processing data for plotting
  df_for_plotting <- textProjectionData(data$harmonywords[1:2],
                                        wordembeddings$harmonywords[1:2,],
                                        wordembeddings$singlewords_we,
                                        data$hilstotal[1:2],
                                        split = "median",
                                        Npermutations = 2,
                                        n_per_split = 1
  )
  df_for_plotting

  expect_true(tibble::is_tibble(df_for_plotting))

  expect_is(df_for_plotting$words[1], 'character')
  expect_is(df_for_plotting$n[1], 'integer')
})



test_that("textProjectionPlot produces a plot", {
  # Dot Product Projection Plot
  p <- textProjectionPlot(
    word_data = DP_projections_HILS_SWLS_100,
    k_n_words_to_test = FALSE,
    min_freq_words = 1,
    plot_n_words_square = 3,
    plot_n_words_p = 3,
    plot_n_word_extreme = 1,
    plot_n_word_frequency = 1,
    plot_n_words_middle = 1,
    x_axes = TRUE,
    y_axes = FALSE,
    p_alpha = 0.05,
    title_top = " Dot Product Projection (DPP)",
    x_axes_label = "Low vs. High HILS score",
    y_axes_label = "Low vs. High SWLS score",
    p_adjust_method = "bonferroni",
    scale_y_axes_lim = NULL)

  expect_true(ggplot2::is.ggplot(p))

 })




test_that("textCentralityData produces a tibble with character variable and numeric variable.", {
  wordembeddings <- wordembeddings4_10
  data <- Language_based_assessment_data_8_10
  df_for_plotting <- textCentralityData(data$harmonywords[1:2],
                                       wordembeddings$harmonywords[1:2,],
                                       wordembeddings$singlewords_we
  )

  expect_is(df_for_plotting$words[1], 'character')
  expect_is(df_for_plotting$n[1], 'integer')
  expect_true(tibble::is_tibble(df_for_plotting))

})


test_that("textCentralityPlot produces a plot.", {

   # Plot
   centrality_plot <- textCentralityPlot(
    word_data = centrality_data_harmony,
    min_freq_words = 10,
    plot_n_word_extreme = 10,
    plot_n_word_frequency = 10,
    plot_n_words_middle = 10,
    titles_color = "#61605e",
    x_axes = "central_cosine",

    title_top = "Semantic Centrality Plot",
    x_axes_label = "Semantic Centrality",

    word_font = NULL,
    centrality_color_codes = c("#EAEAEA","#85DB8E", "#398CF9", "#000000"),
    word_size_range = c(3, 8),
    point_size = 0.5,
    arrow_transparency = 0.1,
    points_without_words_size = 0.5,
    points_without_words_alpha = 0.5,
  )

   expect_true(ggplot2::is.ggplot(centrality_plot))
})



