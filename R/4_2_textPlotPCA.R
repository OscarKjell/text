
####  textPCA and textPCAPlot #####

#' Compute 2 PCA dimensions of the word embeddings for individual words.
#' @param words Word or text variable to be plotted.
#' @param word_types_embeddings Word embeddings from textEmbed for individual words
#' (i.e., decontextualized embeddings).
#' @param seed Set different seed.
#' @return A dataframe with words, their frquency and two PCA dimensions from the word_embeddings
#' for the individual words that is used for the plotting in the textPCAPlot function.
#' @examples
#' \dontrun{
#' # Data
#' df_for_plotting2d <- textPCA(
#'   words = Language_based_assessment_data_8$harmonywords,
#'   word_types_embeddings = word_embeddings_4$singlewords_we
#' )
#' df_for_plotting2d
#' }
#' @seealso see \code{\link{textPCAPlot}}
#' @importFrom tibble as_tibble
#' @importFrom recipes recipe step_center step_scale step_naomit all_numeric prep bake
#' @export
textPCA <- function(words,
                    word_types_embeddings = word_types_embeddings_df,
                    seed = 1010) {
  textPCA_comment <- paste(
    "words =", substitute(words),
    "word_types_embeddings =", comment(word_types_embeddings)
  )


  set.seed(seed)
  # PCA on word_types_embeddings
  # Select word embeddings to be included in plot
  uniques_words_all <- unique_freq_words(words)
  uniques_words_all_wordembedding <- sapply(uniques_words_all$words, applysemrep, word_types_embeddings)
  uniques_words_all_wordembedding <- tibble::as_tibble(t(uniques_words_all_wordembedding))

  rec_pca <- recipes::recipe(~., data = uniques_words_all_wordembedding)
  pca_trans <- rec_pca %>%
    recipes::step_center(recipes::all_numeric()) %>%
    recipes::step_scale(recipes::all_numeric()) %>%
    recipes::step_naomit(Dim1, skip = TRUE)

  pca_trans <- recipes::step_pca(pca_trans, recipes::all_numeric(), num_comp = 2)

  pca_estimates <- recipes::prep(pca_trans, training = uniques_words_all_wordembedding)
  pca_data <- recipes::bake(pca_estimates, uniques_words_all_wordembedding)
  pca_data <- pca_data %>% stats::setNames(paste0("Dim_", names(.)))

  outputdata <- dplyr::bind_cols(uniques_words_all, pca_data)
  comment(outputdata) <- textPCA_comment
  outputdata
}

#' Plot words according to 2-D plot from 2 PCA components.
#' @param word_data Dataframe from textPCA
#' @param min_freq_words_test Select words to significance test that have occurred at least min_freq_words_test
#' (default = 1).
#' @param plot_n_word_extreme Number of words that are extreme on Supervised Dimension Projection per dimension.
#' (i.e., even if not significant; per dimensions, where duplicates are removed).
#' @param plot_n_word_frequency Number of words based on being most frequent.
#' (i.e., even if not significant).
#' @param plot_n_words_middle Number of words plotted that are in the middle in Supervised Dimension Projection score
#' (i.e., even if not significant;  per dimensions, where duplicates are removed).
#' @param title_top Title (default "  ")
#' @param titles_color Color for all the titles (default: "#61605e")
#' @param x_axes_label Label on the x-axes.
#' @param y_axes_label Label on the y-axes.
#' @param scale_x_axes_lim Manually set the length of the x-axes (default = NULL, which uses
#' ggplot2::scale_x_continuous(limits = scale_x_axes_lim); change e.g., by trying c(-5, 5)).
#' @param scale_y_axes_lim Manually set the length of the y-axes (default = NULL; which uses
#' ggplot2::scale_y_continuous(limits = scale_y_axes_lim); change e.g., by trying c(-5, 5)).
#' @param word_font Font type (default: NULL).
#' @param bivariate_color_codes The different colors of the words
#' (default: c("#398CF9", "#60A1F7", "#5dc688",
#' "#e07f6a", "#EAEAEA", "#40DD52",
#' "#FF0000", "#EA7467", "#85DB8E")).
#' @param word_size_range Vector with minimum and maximum font size (default: c(3, 8)).
#' @param position_jitter_hight Jitter height (default: .0).
#' @param position_jitter_width Jitter width (default: .03).
#' @param point_size Size of the points indicating the words' position (default: 0.5).
#' @param arrow_transparency Transparency of the lines between each word and point (default: 0.1).
#' @param points_without_words_size Size of the points not linked with a words
#' (default is to not show it, i.e., 0).
#' @param points_without_words_alpha Transparency of the points not linked with a words
#' (default is to not show it, i.e., 0).
#' @param legend_title Title on the color legend (default: "(PCA)".
#' @param legend_x_axes_label Label on the color legend (default: "(x)".
#' @param legend_y_axes_label Label on the color legend (default: "(y)".
#' @param legend_x_position Position on the x coordinates of the color legend (default: 0.02).
#' @param legend_y_position Position on the y coordinates of the color legend (default: 0.05).
#' @param legend_h_size Height of the color legend (default 0.15).
#' @param legend_w_size Width of the color legend (default 0.15).
#' @param legend_title_size Font size (default: 7).
#' @param legend_number_size Font size of the values in the legend (default: 2).
#' @param seed Set different seed.
#' @return A 1- or 2-dimensional word plot, as well as tibble with processed data used to plot..
#' @examples
#' # The test-data included in the package is called: DP_projections_HILS_SWLS_100
#'
#' # Supervised Dimension Projection Plot
#' principle_component_plot_projection <- textPCAPlot(PC_projections_satisfactionwords_40)
#' principle_component_plot_projection
#'
#' names(DP_projections_HILS_SWLS_100)
#' @seealso see \code{\link{textPCA}}
#' @importFrom tibble as_tibble tibble
#' @importFrom dplyr row_number slice mutate mutate_if bind_rows group_by summarize left_join %>%
#' @importFrom tidyr gather separate
#' @importFrom ggplot2 position_jitter element_text element_blank coord_fixed theme
#' theme_void theme_minimal aes labs scale_color_identity
#' @importFrom rlang sym
#' @importFrom cowplot ggdraw draw_plot
#' @importFrom purrr as_vector
#' @export
textPCAPlot <- function(word_data,
                        min_freq_words_test = 1,
                        plot_n_word_extreme = 5,
                        plot_n_word_frequency = 5,
                        plot_n_words_middle = 5,
                        titles_color = "#61605e",
                        title_top = "Principal Component (PC) Plot",
                        x_axes_label = "PC1",
                        y_axes_label = "PC2",
                        scale_x_axes_lim = NULL,
                        scale_y_axes_lim = NULL,
                        word_font = NULL,
                        bivariate_color_codes = c(
                          "#398CF9", "#60A1F7", "#5dc688",
                          "#e07f6a", "#EAEAEA", "#40DD52",
                          "#FF0000", "#EA7467", "#85DB8E"
                        ),
                        word_size_range = c(3, 8),
                        position_jitter_hight = .0,
                        position_jitter_width = .03,
                        point_size = 0.5,
                        arrow_transparency = 0.1,
                        points_without_words_size = 0.2,
                        points_without_words_alpha = 0.2,
                        legend_title = "PC",
                        legend_x_axes_label = "PC1",
                        legend_y_axes_label = "PC2",
                        legend_x_position = 0.02,
                        legend_y_position = 0.02,
                        legend_h_size = 0.2,
                        legend_w_size = 0.2,
                        legend_title_size = 7,
                        legend_number_size = 2,
                        seed = 1002) {
  textPCAPlot_comment <- paste(
    "INFORMATION ABOUT THE PROJECTION",
    comment(word_data),
    "INFORMATION ABOUT THE PLOT",
    "word_data =", substitute(word_data),
    "min_freq_words_test =", min_freq_words_test,
    "plot_n_word_extreme =", plot_n_word_extreme,
    "plot_n_word_frequency =", plot_n_word_frequency,
    "plot_n_words_middle =", plot_n_words_middle,
    "bivariate_color_codes =", paste(bivariate_color_codes, collapse = " "),
    "word_size_range =", paste(word_size_range, sep = "-", collapse = " - "),
    "position_jitter_hight =", position_jitter_hight,
    "position_jitter_width =", position_jitter_width,
    "point_size =", point_size,
    "arrow_transparency =", point_size,
    "points_without_words_size =", points_without_words_size,
    "points_without_words_alpha =", points_without_words_alpha,
    "legend_x_position =", legend_x_position,
    "legend_y_position =", legend_y_position,
    "legend_h_size =", legend_h_size,
    "legend_w_size =", legend_w_size,
    "legend_title_size =", legend_title_size,
    "legend_number_size =", legend_number_size
  )

  set.seed(seed)

  # Sorting out axes
  x_axes_1 <- "Dim_PC1"
  y_axes_1 <- "Dim_PC2"

  ### Selecting words to plot
  # Select min_freq_words_test
  word_data <- word_data[word_data$n >= min_freq_words_test, ]

  # Select plot_n_word_extreme and Select plot_n_word_frequency; plot_n_word_extreme=5
  word_data_extrem_max_PC1 <- word_data %>%
    dplyr::arrange(-Dim_PC1) %>%
    dplyr::slice(0:plot_n_word_extreme)

  word_data_extrem_max_PC2 <- word_data %>%
    dplyr::arrange(-Dim_PC2) %>%
    dplyr::slice(0:plot_n_word_extreme)

  # Select min
  word_data_extrem_min_PC1 <- word_data %>%
    dplyr::arrange(Dim_PC1) %>%
    dplyr::slice(0:plot_n_word_extreme)

  word_data_extrem_min_PC2 <- word_data %>%
    dplyr::arrange(Dim_PC2) %>%
    dplyr::slice(0:plot_n_word_extreme)

  # Select word frequency; plot_n_word_frequency = 5
  word_data_frequency <- word_data %>%
    dplyr::arrange(-n) %>%
    dplyr::slice(0:plot_n_word_frequency)

  # Select the middle range, order according to frequency and then select the plot_n_words_middle = 5
  # PC1
  mean_m_sd_PC1 <- mean(word_data$Dim_PC1, na.rm = TRUE) - (sd(word_data$Dim_PC1, na.rm = TRUE) / 2)
  mean_p_sd_PC1 <- mean(word_data$Dim_PC1, na.rm = TRUE) + (sd(word_data$Dim_PC1, na.rm = TRUE) / 2)

  word_data_middle_PC1 <- word_data %>%
    dplyr::filter(dplyr::between(word_data$Dim_PC1, mean_m_sd_PC1, mean_p_sd_PC1)) %>%
    dplyr::arrange(-n) %>%
    dplyr::slice(0:plot_n_words_middle)

  # PC2
  mean_m_sd_PC2 <- mean(word_data$Dim_PC2, na.rm = TRUE) - (sd(word_data$Dim_PC2, na.rm = TRUE) / 2)
  mean_p_sd_PC2 <- mean(word_data$Dim_PC2, na.rm = TRUE) + (sd(word_data$Dim_PC2, na.rm = TRUE) / 2)

  word_data_middle_PC2 <- word_data %>%
    dplyr::filter(dplyr::between(word_data$Dim_PC2, mean_m_sd_PC2, mean_p_sd_PC2)) %>%
    dplyr::arrange(-n) %>%
    dplyr::slice(0:plot_n_words_middle)

  # Combine selected words
  word_data_all <- word_data %>%
    dplyr::left_join(word_data_extrem_max_PC1 %>%
      dplyr::transmute(words, check_extreme_max_PC1 = 1), by = "words") %>%
    dplyr::left_join(word_data_extrem_max_PC2 %>%
      dplyr::transmute(words, check_extreme_max_PC2 = 1), by = "words") %>%
    dplyr::left_join(word_data_extrem_min_PC1 %>%
      dplyr::transmute(words, check_extreme_min_PC1 = 1), by = "words") %>%
    dplyr::left_join(word_data_extrem_min_PC2 %>%
      dplyr::transmute(words, check_extreme_min_PC2 = 1), by = "words") %>%
    dplyr::left_join(word_data_frequency %>%
      dplyr::transmute(words, check_extreme_frequency = 1), by = "words") %>%
    dplyr::left_join(word_data_middle_PC1 %>%
      dplyr::transmute(words, check_middle_PC1 = 1), by = "words") %>%
    dplyr::left_join(word_data_middle_PC2 %>%
      dplyr::transmute(words, check_middle_PC2 = 1), by = "words") %>%
    dplyr::mutate(extremes_all = rowSums(cbind(
      check_extreme_max_PC1, check_extreme_max_PC2,
      check_extreme_min_PC1, check_extreme_min_PC2,
      check_extreme_frequency,
      check_middle_PC1, check_middle_PC2
    ), na.rm = T))

  # Changing NAs to 0
  word_data_all$check_extreme_max_PC1[is.na(word_data_all$check_extreme_max_PC1)] <- 0
  word_data_all$check_extreme_max_PC2[is.na(word_data_all$check_extreme_max_PC2)] <- 0

  word_data_all$check_extreme_min_PC1[is.na(word_data_all$check_extreme_min_PC1)] <- 0
  word_data_all$check_extreme_min_PC2[is.na(word_data_all$check_extreme_min_PC2)] <- 0

  word_data_all$check_middle_PC1[is.na(word_data_all$check_middle_PC1)] <- 0
  word_data_all$check_middle_PC2[is.na(word_data_all$check_middle_PC2)] <- 0
  word_data_all$check_extreme_frequency[is.na(word_data_all$check_extreme_frequency)] <- 0

  # Categorize words to apply specific color
  word_data_all <- word_data_all %>%
    dplyr::mutate(colour_categories = dplyr::case_when(
      check_extreme_max_PC1 == 0 & check_extreme_min_PC1 == 1 & check_extreme_max_PC2 == 1 & check_extreme_min_PC2 == 0 ~ bivariate_color_codes[1],
      check_extreme_max_PC1 == 0 & check_extreme_min_PC1 == 0 & check_extreme_max_PC2 == 1 & check_extreme_min_PC2 == 0 ~ bivariate_color_codes[2],
      check_extreme_max_PC1 == 1 & check_extreme_min_PC1 == 0 & check_extreme_max_PC2 == 1 & check_extreme_min_PC2 == 0 ~ bivariate_color_codes[3],
      check_extreme_max_PC1 == 0 & check_extreme_min_PC1 == 1 & check_extreme_max_PC2 == 0 & check_extreme_min_PC2 == 0 ~ bivariate_color_codes[4],
      check_extreme_max_PC1 == 0 & check_extreme_min_PC1 == 0 & check_extreme_max_PC2 == 0 & check_extreme_min_PC2 == 0 ~ bivariate_color_codes[5],
      check_extreme_max_PC1 == 1 & check_extreme_min_PC1 == 0 & check_extreme_max_PC2 == 0 & check_extreme_min_PC2 == 0 ~ bivariate_color_codes[6],
      check_extreme_max_PC1 == 0 & check_extreme_min_PC1 == 1 & check_extreme_max_PC2 == 0 & check_extreme_min_PC2 == 1 ~ bivariate_color_codes[7],
      check_extreme_max_PC1 == 0 & check_extreme_min_PC1 == 0 & check_extreme_max_PC2 == 0 & check_extreme_min_PC2 == 1 ~ bivariate_color_codes[8],
      check_extreme_max_PC1 == 1 & check_extreme_min_PC1 == 0 & check_extreme_max_PC2 == 0 & check_extreme_min_PC2 == 1 ~ bivariate_color_codes[9]
    ))

  # Add or Remove values on y-axes
  y_axes_values <- ggplot2::element_blank()

  # Word data adjusted for if y_axes exists
  word_data_all_yadjusted <- word_data_all[word_data_all$extremes_all != 0 | word_data_all$extremes_all != 0, ]

  # Plot
  plot <-
    # construct ggplot; the !!sym( ) is to  turn the strings into symbols.
    ggplot2::ggplot(data = word_data_all, ggplot2::aes(!!rlang::sym(x_axes_1), !!rlang::sym(y_axes_1), label = words)) +
    ggplot2::geom_point(
      data = word_data_all,
      size = points_without_words_size,
      alpha = points_without_words_alpha,
      ggplot2::aes(color = colour_categories)
    ) +

    # ggrepel geom, make arrows transparent, color by rank, size by n word_data_all_yadjusted$colour_categories
    ggrepel::geom_text_repel(
      data = word_data_all_yadjusted,
      segment.alpha  = arrow_transparency,
      position = ggplot2::position_jitter(h = position_jitter_hight, w = position_jitter_width),
      ggplot2::aes(color = colour_categories, size = n, family = word_font),
    ) +
    ggplot2::scale_color_identity() +

    # Decide size and color of the points
    ggplot2::geom_point(
      data = word_data_all_yadjusted,
      size = point_size,
      ggplot2::aes(color = colour_categories)
    ) +

    # set word size range and the guide
    ggplot2::scale_size_continuous(
      range = word_size_range,
      guide = ggplot2::guide_legend(
        title = "Frequency",
        title.position = "top",
        direction = "horizontal",
        label.position = "bottom",
        ggplot2::element_text(color = titles_color)
      )
    ) +

    # Title
    ggplot2::ggtitle(paste0(title_top)) +
    ggplot2::labs(y = y_axes_label, x = x_axes_label) +

    # Help create possibility to remove y-axes numbers
    ggplot2::scale_x_continuous(limits = scale_x_axes_lim) +
    ggplot2::scale_y_continuous(limits = scale_y_axes_lim) +

    # Minimal theme, and turning off legends
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = c("bottom"),
      plot.title = ggplot2::element_text(hjust = 0.5),
      legend.justification = c("right", "top"),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      axis.text.y = y_axes_values,
      title = ggplot2::element_text(color = titles_color),
      axis.title.x = ggplot2::element_text(color = titles_color),
      axis.title.y = ggplot2::element_text(color = titles_color)
    )
  plot

  # Creating legend
  bivariate_color_data <- tibble::tibble(
    "1 - 3" = "#XXXXXX", "2 - 3" = "#XXXXXX", "3 - 3" = "#XXXXXX",
    "1 - 2" = "#XXXXXX", "2 - 2" = "#XXXXXX", "3 - 2" = "#XXXXXX",
    "1 - 1" = "#XXXXXX", "2 - 1" = "#XXXXXX", "3 - 1" = "#XXXXXX"
  )
  bivariate_color_data <- rbind(bivariate_color_data, bivariate_color_codes)
  bivariate_color_data <- bivariate_color_data[-1, ]

  legend <- bivariate_color_data %>%
    tidyr::gather("group", "fill") %>%
    tidyr::separate(group, into = c("x", "y"), sep = " - ") %>%
    dplyr::mutate(
      x = as.integer(x),
      y = as.integer(y)
    ) %>%
    ggplot2::ggplot(ggplot2::aes(x, y)) +
    ggplot2::geom_tile(ggplot2::aes(fill = fill)) +
    ggplot2::ggtitle(paste0(legend_title)) +
    ggplot2::scale_fill_identity() +
    ggplot2::labs(
      x = legend_x_axes_label,
      y = legend_y_axes_label
    ) +
    ggplot2::theme_void() +
    ggplot2::annotate(
      geom = "text", x = 1, y = 3, label = sum(word_data_all$colour_categories == bivariate_color_codes[1], na.rm = T),
      color = titles_color, size = legend_number_size
    ) +
    ggplot2::annotate(
      geom = "text", x = 2, y = 3, label = sum(word_data_all$colour_categories == bivariate_color_codes[2], na.rm = T),
      color = titles_color, size = legend_number_size
    ) +
    ggplot2::annotate(
      geom = "text", x = 3, y = 3, label = sum(word_data_all$colour_categories == bivariate_color_codes[3], na.rm = T),
      color = titles_color, size = legend_number_size
    ) +
    ggplot2::annotate(
      geom = "text", x = 1, y = 2, label = sum(word_data_all$colour_categories == bivariate_color_codes[4], na.rm = T),
      color = titles_color, size = legend_number_size
    ) +
    ggplot2::annotate(
      geom = "text", x = 2, y = 2, label = sum(word_data_all$colour_categories == bivariate_color_codes[5], na.rm = T),
      color = titles_color, size = legend_number_size
    ) +
    ggplot2::annotate(
      geom = "text", x = 3, y = 2, label = sum(word_data_all$colour_categories == bivariate_color_codes[6], na.rm = T),
      color = titles_color, size = legend_number_size
    ) +
    ggplot2::annotate(
      geom = "text", x = 1, y = 1, label = sum(word_data_all$colour_categories == bivariate_color_codes[7], na.rm = T),
      color = titles_color, size = legend_number_size
    ) +
    ggplot2::annotate(
      geom = "text", x = 2, y = 1, label = sum(word_data_all$colour_categories == bivariate_color_codes[8], na.rm = T),
      color = titles_color, size = legend_number_size
    ) +
    ggplot2::annotate(
      geom = "text", x = 3, y = 1, label = sum(word_data_all$colour_categories == bivariate_color_codes[9], na.rm = T),
      color = titles_color, size = legend_number_size
    ) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, size = legend_title_size + 1),
      title = ggplot2::element_text(color = titles_color),
      axis.title.x = ggplot2::element_text(color = titles_color),
      axis.title = ggplot2::element_text(size = legend_title_size),
      axis.title.y = ggplot2::element_text(angle = 90, color = titles_color)
    ) +
    ggplot2::coord_fixed()
  # legend

  # Plot both figure and legend together
  final_plot <- suppressWarnings(cowplot::ggdraw() +
    cowplot::draw_plot(plot, 0, 0, 1, 1) +
    cowplot::draw_plot(legend, legend_x_position, legend_y_position, legend_h_size, legend_w_size))

  output_plot_data <- list(final_plot, textPCAPlot_comment, word_data_all)
  names(output_plot_data) <- c("final_plot", "description", "processed_word_data")
  output_plot_data
}
