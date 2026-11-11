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
    word_types_embeddings = word_embeddings_4$word_types,
    x = Language_based_assessment_data_8$hilstotal[1:10],
    split = "mean",
    Npermutations = 2,
    n_per_split = 1,
    pca = 2
  )

  testthat::expect_true(tibble::is_tibble(df_for_plotting1[[2]]))
  testthat::expect_is(df_for_plotting1[[2]]$words[1], "character")
  testthat::expect_is(df_for_plotting1[[2]]$n[1], "numeric")
#  testthat::expect_equal(df_for_plotting1[[2]]$dot.x[1], -3.847934, tolerance = 0.001) #Not anchoring the G1 and G2 ambeddings
  testthat::expect_equal(df_for_plotting1[[2]]$dot.x[1], -4.215651, tolerance = 0.001)


  # Pre-processing data for plotting
  df_for_plotting1_no_split <- text::textProjection(
    words = Language_based_assessment_data_8$harmonywords[1:10],
    word_embeddings = word_embeddings_4$texts$harmonywords[1:10, ],
    word_embeddings_4$word_types,
    Language_based_assessment_data_8$hilstotal[1:10],
    split = "no",
    Npermutations = 2,
    n_per_split = 1,
    pca = 2
  )

  testthat::expect_true(tibble::is_tibble(df_for_plotting1_no_split[[2]]))
  testthat::expect_is(df_for_plotting1_no_split[[2]]$words[1], "character")
  testthat::expect_is(df_for_plotting1_no_split[[2]]$n[1], "numeric")
 # testthat::expect_equal(df_for_plotting1_no_split[[2]]$dot.x[1], -1.537714, tolerance = 0.001) #Not anchoring the G1 and G2 ambeddings
  testthat::expect_equal(df_for_plotting1_no_split[[2]]$dot.x[1],  -1.039681, tolerance = 0.001)

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

  # Dot Product Projection Plot
  p3 <- text::textPlot(
    word_data = DP_projections_HILS_SWLS_100,
    k_n_words_to_test = FALSE,
    min_freq_words_test = 1,
    plot_n_words_square = 0,
    plot_n_words_p = 0,
    plot_n_word_extreme = 0,
    plot_n_word_extreme_xy = 5,
    plot_n_word_frequency = 0,
    plot_n_words_middle = 0,
    plot_n_word_random = 0,
    # x_axes = TRUE,
    y_axes = TRUE,
    p_alpha = 0.05,
    title_top = " Dot Product Projection (DPP)",
    x_axes_label = "Low vs. High HILS score",
    y_axes_label = "Low vs. High SWLS score",
    p_adjust_method = "fdr",
    scale_y_axes_lim = NULL
  )
  p3
  expect_true(ggplot2::is.ggplot(p3$final_plot))
  expect_equal(p3$processed_word_data$x_plotted[2], 0.7323493, tolerance = 0.0001)


  # Cohens_d pipeline
  # Pre-processing data for plotting
  df_cohensD <- text::textProjection(
    words = Language_based_assessment_data_8$harmonywords,
    word_embeddings = word_embeddings_4$texts$harmonywords,
    word_types_embeddings = word_embeddings_4$word_types,
    Language_based_assessment_data_8$hilstotal,
    Language_based_assessment_data_8$swlstotal,
    split = "quartile",
    Npermutations = 1000,
    n_per_split = 3
  )

  p_cohensD <- text::textPlot(
    word_data = df_cohensD,
    min_freq_words_test = 1,
    plot_n_words_square = 3,
    plot_n_words_p = 3,
    plot_n_word_extreme = 1,
    plot_n_word_frequency = 1,
    plot_n_words_middle = 1,
    projection_metric = "cohens_d",
    y_axes = F,
    p_alpha = 0.0005,
    title_top = "Cohen's D Dot Product Projection (DPP)",
    x_axes_label = "Low vs. High HILS score",
    y_axes_label = "Low vs. High SWLS score",
   # p_adjust_method = "fdr",
    scale_y_axes_lim = NULL
  )
  p_cohensD
  expect_true(ggplot2::is.ggplot(p_cohensD$final_plot))

})


test_that("textCentrality produces a tibble with character variable and numeric variable.", {
  skip_on_cran()

  df_for_plotting <- text::textCentrality(
    Language_based_assessment_data_8$harmonywords[1:20],
    word_embeddings_4$texts$harmonywords[1:20, ],
    word_embeddings_4$word_types,
    method = "euclidean"
  )

  expect_is(df_for_plotting$words[1], "character")
  expect_is(df_for_plotting$n[1], "integer")
  expect_true(tibble::is_tibble(df_for_plotting))
  expect_equal(df_for_plotting$n[1], 2)

  plot_c <- text::textCentralityPlot(
    word_data = df_for_plotting,
    x_axes = "central_semantic_similarity"
  )
  plot_c
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
  expect_equal(centrality_plot$processed_word_data$central_semantic_similarity[2],
               0.5079464, tolerance = 0.00001)
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

test_that("textProjection project_texts = TRUE returns text_data with finite projections", {
  skip_on_cran()

  proj_texts <- text::textProjection(
    words = Language_based_assessment_data_8$harmonywords,
    word_embeddings = word_embeddings_4$texts$harmonywords,
    word_types_embeddings = word_embeddings_4$word_types,
    x = Language_based_assessment_data_8$hilstotal,
    Npermutations = 20,
    n_per_split = 20,
    project_texts = TRUE,
    project_texts_unit = "texts"
  )

  testthat::expect_named(
    proj_texts,
    c("background", "word_data", "text_data", "text_null_distribution")
  )
  td <- proj_texts$text_data
  testthat::expect_true(tibble::is_tibble(td))
  testthat::expect_equal(nrow(td), length(Language_based_assessment_data_8$harmonywords))
  testthat::expect_true(all(c(
    "participant_id", "texts", "value.x", "dot.x", "p_values_dot.x", "p_z_dot.x",
    "null_mean.x", "null_sd.x", "cohens_d.x"
  ) %in% names(td)))
  # cohens_d is the dot standardised by the unit's own null distribution
  testthat::expect_equal(
    td$cohens_d.x,
    (td$dot.x - td$null_mean.x) / td$null_sd.x,
    tolerance = 1e-10
  )
  # pooled null sample for the distribution plot
  nd <- proj_texts$text_null_distribution
  testthat::expect_true(tibble::is_tibble(nd))
  testthat::expect_true(nrow(nd) > 0)
  testthat::expect_true(all(is.finite(nd$value)))
  testthat::expect_true(all(nd$dimension == "x"))
  testthat::expect_equal(td$participant_id, seq_along(Language_based_assessment_data_8$harmonywords))
  testthat::expect_true(all(is.finite(td$dot.x)))
  testthat::expect_true(all(td$p_values_dot.x > 0 & td$p_values_dot.x <= 1))
  testthat::expect_equal(td$value.x, Language_based_assessment_data_8$hilstotal)

  # The observed text dot products equal a direct projection onto the
  # returned axis (same anchoring and direction as the words).
  g1 <- as.numeric(proj_texts$background[[1]]$Aggregated_word_embedding_group1.x)
  g2 <- as.numeric(proj_texts$background[[1]]$Aggregated_word_embedding_group2.x)
  Tm <- as.matrix(dplyr::select(
    word_embeddings_4$texts$harmonywords,
    dplyr::starts_with("Dim")
  ))
  direct <- as.vector(Tm %*% (g2 - g1)) - sum((g1 + g2) / 2 * (g2 - g1))
  testthat::expect_equal(td$dot.x, direct, tolerance = 1e-8)

  # project_texts requires the default settings (same as the participant null)
  testthat::expect_error(
    text::textProjection(
      words = Language_based_assessment_data_8$harmonywords,
      word_embeddings = word_embeddings_4$texts$harmonywords,
      word_types_embeddings = word_embeddings_4$word_types,
      x = Language_based_assessment_data_8$hilstotal,
      pca = 2,
      project_texts = TRUE
    ),
    "project_texts"
  )
})

test_that("textProjection project_texts default splits responses into sentences", {
  skip_on_cran()

  texts <- Language_based_assessment_data_8$harmonywords
  proj_sent <- text::textProjection(
    words = texts,
    word_embeddings = word_embeddings_4$texts$harmonywords,
    word_types_embeddings = word_embeddings_4$word_types,
    x = Language_based_assessment_data_8$hilstotal,
    Npermutations = 20,
    n_per_split = 20,
    project_texts = TRUE
  )

  td <- proj_sent$text_data
  testthat::expect_true(all(c(
    "participant_id", "texts", "n_tokens", "value.x",
    "dot.x", "p_values_dot.x", "p_z_dot.x", "null_mean.x", "null_sd.x", "cohens_d.x"
  ) %in% names(td)))
  testthat::expect_true(all(td$participant_id %in% seq_along(texts)))
  testthat::expect_true(all(td$n_tokens >= 1))
  testthat::expect_true(all(is.finite(td$dot.x)))
  testthat::expect_true(all(td$p_values_dot.x > 0 & td$p_values_dot.x <= 1))
  # Each sentence carries its participant's x value
  testthat::expect_equal(
    td$value.x,
    Language_based_assessment_data_8$hilstotal[td$participant_id]
  )

  # The observed sentence dot products equal a direct projection of the
  # reconstructed sentence embeddings (mean of word-type embeddings) onto
  # the returned axis.
  wt <- text:::normalise_word_types(word_embeddings_4$word_types)
  S <- text:::perm_setup(text:::perm_count_matrix(texts), wt)
  sdf <- text:::perm_split_sentences(texts)
  se <- text:::perm_sentence_embeddings(sdf$sentences, S)
  keep <- se$n_tokens > 0
  testthat::expect_equal(nrow(td), sum(keep))
  testthat::expect_equal(td$texts, sdf$sentences[keep])
  g1 <- as.numeric(proj_sent$background[[1]]$Aggregated_word_embedding_group1.x)
  g2 <- as.numeric(proj_sent$background[[1]]$Aggregated_word_embedding_group2.x)
  direct <- as.vector(se$Tm[keep, , drop = FALSE] %*% (g2 - g1)) -
    sum((g1 + g2) / 2 * (g2 - g1))
  testthat::expect_equal(td$dot.x, direct, tolerance = 1e-8)
})

test_that("textPlot show_texts overlays text points and warns without text_data", {
  skip_on_cran()

  proj_texts <- text::textProjection(
    words = Language_based_assessment_data_8$harmonywords,
    word_embeddings = word_embeddings_4$texts$harmonywords,
    word_types_embeddings = word_embeddings_4$word_types,
    x = Language_based_assessment_data_8$hilstotal,
    Npermutations = 20,
    n_per_split = 20,
    project_texts = TRUE
  )

  p <- suppressWarnings(text::textPlot(
    proj_texts,
    show_texts = TRUE,
    plot_n_word_extreme = 3,
    plot_n_word_frequency = 3,
    plot_n_words_middle = 3
  ))
  testthat::expect_true(ggplot2::is_ggplot(p$final_plot))
  # The sentences/texts also get their own distribution plot (observed
  # projections vs. the pooled participant-permutation null).
  testthat::expect_true(ggplot2::is_ggplot(p$texts_distribution_plot))

  proj_no_texts <- proj_texts
  proj_no_texts$text_data <- NULL
  testthat::expect_warning(
    text::textPlot(
      proj_no_texts,
      show_texts = TRUE,
      plot_n_word_extreme = 3,
      plot_n_word_frequency = 3,
      plot_n_words_middle = 3
    ),
    "project_texts"
  )
})
