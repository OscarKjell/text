
#### Semantic Centrality Plot SC ####

#' Compute semantic similarity score between single words' word embeddings
#' and the aggregated word embedding of all words.
#' @param words Word or text variable to be plotted.
#' @param word_embeddings Word embeddings from textEmbed for the words to be plotted
#' (i.e., the aggregated word embeddings for the "words" variable).
#' @param single_word_embeddings Word embeddings from textEmbed for individual words
#' (i.e., the decontextualized word embeddings).
#' @param similarity_method Character string describing type of measure to be computed; default is "cosine" (see also
#' measures from textDistance (here computed as 1 - textDistance()) including "euclidean", "maximum", "manhattan", "canberra", "binary" and "minkowski").
#' @param aggregation Method to aggregate the word embeddings
#' (default = "mean"; see also "min", "max" or "[CLS]").
#' @param min_freq_words_test Option to  select words that have at least occurred a specified
#' number of times (default = 0); when creating the semantic similarity
#' scores.
#' @return A dataframe with variables (e.g., including semantic similarity, frequencies)
#' for the individual words that are used for the plotting in the textCentralityPlot function.
#' @examples
#' \dontrun{
#' df_for_plotting <- textCentrality(
#'   words = Language_based_assessment_data_8$harmonywords,
#'   word_embeddings = word_embeddings_4$harmonywords,
#'   single_word_embeddings = word_embeddings_4$singlewords_we
#' )
#' df_for_plotting
#' }
#' @seealso see \code{\link{textCentralityPlot}}  \code{\link{textProjection}}
#' @importFrom dplyr bind_rows
#' @importFrom tibble tibble
#' @export
textCentrality <- function(words,
                           word_embeddings,
                           single_word_embeddings = single_word_embeddings_df,
                           similarity_method = "cosine",
                           aggregation = "mean",
                           min_freq_words_test = 0) {

  textCentrality_description <- paste("words =", substitute(words),
    "word_embeddings =", comment(word_embeddings),
    "single_word_embeddings =", comment(single_word_embeddings),
    "similarity_method =", similarity_method,
    "aggregation =", aggregation,
    "min_freq_words_test =", min_freq_words_test,
    collapse = " "
  )

  # Create Central Point by aggregating all word embeddings
  Central_Point <- textEmbeddingAggregation(word_embeddings, aggregation = "mean")

  # Select embeddings for unique words
  # Group 1: getting unique words and their frequency min_freq_words_test=3
  all_unique_freq_words <- unique_freq_words(words)
  all_unique_freq_words_min_freq <- all_unique_freq_words[all_unique_freq_words$n >= min_freq_words_test, ]

  # Get word embeddings for each word (applysemrep function is created in 1_1_textEmbedd).
  all_single_wordembedding_a <- lapply(all_unique_freq_words_min_freq$words, applysemrep, single_word_embeddings)
  all_single_wordembedding_a1 <- dplyr::bind_rows(all_single_wordembedding_a)

  # Compute similarity to Central Point
  Central_Point_df <- tibble::as_tibble(t(replicate(nrow(all_single_wordembedding_a1), Central_Point)))
  central_semantic_similarity <- textSimilarity(all_single_wordembedding_a1, Central_Point_df, method = similarity_method)
  cenrtal_sss_df <- tibble::tibble(all_unique_freq_words_min_freq[, 1:2], central_semantic_similarity)
  cenrtal_sss_df$n_percent <- cenrtal_sss_df$n / sum(cenrtal_sss_df$n)
  comment(cenrtal_sss_df) <- textCentrality_description
  return(cenrtal_sss_df)
}
# End Semantic Centrality Plot data


#' Plot words according to semantic similarity to the aggregated word embedding.
#' @param word_data Tibble from textPlotData.
#' @param min_freq_words_test Select words to significance test that have occurred
#' at least min_freq_words_test (default = 1).
#' @param plot_n_word_extreme Number of words per dimension to plot with extreme
#' Supervised Dimension Projection value.
#' (i.e., even if not significant;  duplicates are removed).
#' @param plot_n_word_frequency Number of words to plot according to their frequency.
#' (i.e., even if not significant).
#' @param plot_n_words_middle Number of words to plot that are in the middle in Supervised Dimension Projection score
#' (i.e., even if not significant; duplicates are removed).
#' @param title_top Title (default "  ").
#' @param titles_color Color for all the titles (default: "#61605e").
#' @param x_axes Variable to be plotted on the x-axes (default is "central_semantic_similarity").
#' @param x_axes_label Label on the x-axes.
#' @param scale_x_axes_lim Length of the x-axes (default: NULL, which uses
#' c(min(word_data$central_semantic_similarity)-0.05, max(word_data$central_semantic_similarity)+0.05);
#' change this by e.g., try c(-5, 5)).
#' @param scale_y_axes_lim Length of the y-axes (default: NULL, which uses c(-1, 1);
#' change e.g., by trying c(-5, 5)).
#' @param word_font Type of font (default: NULL).
#' @param centrality_color_codes Colors of the words selected as plot_n_word_extreme
#' (minimum values), plot_n_words_middle, plot_n_word_extreme (maximum values) and
#' plot_n_word_frequency; the default is c("#EAEAEA","#85DB8E", "#398CF9", "#000000"), respectively.
#' @param word_size_range Vector with minimum and maximum font size (default: c(3, 8)).
#' @param position_jitter_hight Jitter height (default: .0).
#' @param position_jitter_width Jitter width (default: .03).
#' @param point_size Size of the points indicating the words' position (default: 0.5).
#' @param arrow_transparency Transparency of the lines between each word and point (default: 0.1).
#' @param points_without_words_size Size of the points not linked to a word
#' (default is to not show the point; , i.e., 0).
#' @param points_without_words_alpha Transparency of the points that are not linked to a word
#' (default is to not show it; i.e., 0).
#' @param legend_title Title of the color legend (default: "(SCP)").
#' @param legend_x_axes_label Label on the color legend (default: "(x)".
#' @param legend_x_position Position on the x coordinates of the color legend (default: 0.02).
#' @param legend_y_position Position on the y coordinates of the color legend (default: 0.05).
#' @param legend_h_size Height of the color legend (default 0.15).
#' @param legend_w_size Width of the color legend (default 0.15).
#' @param legend_title_size Font size of the title (default = 7).
#' @param legend_number_size Font size of the values in the legend (default = 2).
#' @param seed Set different seed.
#' @return A 1-dimensional word plot based on similarity to the aggregated word embedding,
#' as well as tibble with processed data used to plot.
#' @seealso see \code{\link{textCentrality}} and \code{\link{textProjection}}
#' @examples
#' # The test-data included in the package is called: centrality_data_harmony
#' names(centrality_data_harmony)
#' # Plot
#' # centrality_plot <- textCentralityPlot(
#' #  word_data = centrality_data_harmony,
#' #  min_freq_words_test = 10,
#' #  plot_n_word_extreme = 10,
#' #  plot_n_word_frequency = 10,
#' #  plot_n_words_middle = 10,
#' #  titles_color = "#61605e",
#' #  x_axes = "central_semantic_similarity",
#' #
#' #  title_top = "Semantic Centrality Plot",
#' #  x_axes_label = "Semantic Centrality",
#' #
#' #  word_font = NULL,
#' #  centrality_color_codes = c("#EAEAEA","#85DB8E", "#398CF9", "#000000"),
#' #  word_size_range = c(3, 8),
#' #  point_size = 0.5,
#' #  arrow_transparency = 0.1,
#' #  points_without_words_size = 0.5,
#' #  points_without_words_alpha = 0.5,
#' # )
#' # centrality_plot
#' @importFrom dplyr arrange slice filter between left_join transmute mutate case_when
#' @importFrom ggplot2 position_jitter element_text element_blank coord_fixed theme
#' theme_void theme_minimal aes labs scale_color_identity
#' @importFrom rlang sym
#' @export
textCentralityPlot <- function(word_data,
                               min_freq_words_test = 1,
                               plot_n_word_extreme = 10,
                               plot_n_word_frequency = 10,
                               plot_n_words_middle = 10,
                               titles_color = "#61605e",
                               x_axes = "central_semantic_similarity",
                               title_top = "Semantic Centrality Plot",
                               x_axes_label = "Semantic Centrality",
                               scale_x_axes_lim = NULL,
                               scale_y_axes_lim = NULL,
                               word_font = NULL,
                               centrality_color_codes = c("#EAEAEA", "#85DB8E", "#398CF9", "#9e9d9d"),
                               word_size_range = c(3, 8),
                               position_jitter_hight = .0,
                               position_jitter_width = .03,
                               point_size = 0.5,
                               arrow_transparency = 0.1,
                               points_without_words_size = 0.5,
                               points_without_words_alpha = 0.5,
                               legend_title = "SC",
                               legend_x_axes_label = "x",
                               legend_x_position = 0.02,
                               legend_y_position = 0.02,
                               legend_h_size = 0.2,
                               legend_w_size = 0.2,
                               legend_title_size = 7,
                               legend_number_size = 2,
                               seed = 1007) {
  textCentralityPlot_comment <- paste(
    "INFORMATION ABOUT THE PROJECTION",
    comment(word_data),
    "INFORMATION ABOUT THE PLOT",
    "word_data =", substitute(word_data),
    "min_freq_words_test =", min_freq_words_test,
    "plot_n_word_extreme =", plot_n_word_extreme,
    "plot_n_word_frequency =", plot_n_word_frequency,
    "plot_n_words_middle =", plot_n_words_middle,
    "centrality_color_codes =", paste(centrality_color_codes, collapse = " "),
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
  y_axes_label <- NULL
  y_axes_values <- element_blank()

  # Selected min_freq_words_test
  word_data1 <- word_data[word_data$n >= min_freq_words_test, ]

  # Select plot_n_word_extreme and Select plot_n_word_frequency
  word_data1_extrem_max_x <- word_data1 %>%
    dplyr::arrange(-central_semantic_similarity) %>%
    dplyr::slice(0:plot_n_word_extreme)

  word_data1_extrem_min_x <- word_data1 %>%
    dplyr::arrange(central_semantic_similarity) %>%
    dplyr::slice(0:plot_n_word_extreme)

  word_data1_frequency_x <- word_data1 %>%
    dplyr::arrange(-n) %>%
    dplyr::slice(0:plot_n_word_frequency)

  # Select the middle range, order according to frequency and then select the plot_n_words_middle = 5
  mean_m_sd_x <- mean(word_data1$central_semantic_similarity, na.rm = TRUE) - (sd(word_data1$central_semantic_similarity, na.rm = TRUE) / 10)
  mean_p_sd_x <- mean(word_data1$central_semantic_similarity, na.rm = TRUE) + (sd(word_data1$central_semantic_similarity, na.rm = TRUE) / 10)

  word_data1_middle_x <- word_data1 %>%
    dplyr::filter(dplyr::between(word_data1$central_semantic_similarity, mean_m_sd_x, mean_p_sd_x)) %>%
    dplyr::arrange(-n) %>%
    dplyr::slice(0:plot_n_words_middle)

  word_data1_all <- word_data1 %>%
    dplyr::left_join(word_data1_extrem_max_x %>%
      dplyr::transmute(words, check_extreme_max_x = 1), by = "words") %>%
    dplyr::left_join(word_data1_extrem_min_x %>%
      dplyr::transmute(words, check_extreme_min_x = 1), by = "words") %>%
    dplyr::left_join(word_data1_frequency_x %>%
      dplyr::transmute(words, check_extreme_frequency_x = 1), by = "words") %>%
    dplyr::left_join(word_data1_middle_x %>%
      dplyr::transmute(words, check_middle_x = 1), by = "words") %>%
    dplyr::mutate(extremes_all_x = rowSums(cbind(
      check_extreme_max_x, check_extreme_min_x,
      check_extreme_frequency_x, check_middle_x
    ), na.rm = T))

  # Categorise words to apply specific color
  word_data1_all <- word_data1_all %>%
    dplyr::mutate(colour_categories = dplyr::case_when(
      check_extreme_min_x == 1 ~ centrality_color_codes[1],
      check_middle_x == 1 ~ centrality_color_codes[2],
      check_extreme_max_x == 1 ~ centrality_color_codes[3],
      check_extreme_frequency_x == 1 ~ centrality_color_codes[4]
    ))

  if (is.null(scale_x_axes_lim)) {
    scale_x_axes_lim <- c(min(word_data1$central_semantic_similarity) - 0.05, max(word_data1$central_semantic_similarity) + 0.05)
  }
  if (is.null(scale_y_axes_lim)) {
    scale_y_axes_lim <- c(-1, 1)
  }

  # This solution is because it is not possible to send "0" as a parameter
  only_x_dimension <- 0
  y_axes <- "only_x_dimension"

  # Plot
  plot <-
    # construct ggplot; the !!sym( ) is to  turn the strings into symbols.
    ggplot2::ggplot(data = word_data1_all, ggplot2::aes(!!rlang::sym(x_axes), !!rlang::sym(y_axes), label = words)) +
    ggplot2::geom_point(
      data = word_data1_all,
      size = points_without_words_size,
      alpha = points_without_words_alpha,
      ggplot2::aes(color = "#EAEAEA")
    ) +

    # ggrepel geom, make arrows transparent, color by rank, size by n
    ggrepel::geom_text_repel(
      data = word_data1_all[word_data1_all$extremes_all_x == 1, ],
      segment.alpha  = arrow_transparency,
      position = ggplot2::position_jitter(h = position_jitter_hight, w = position_jitter_width),
      ggplot2::aes(color = colour_categories, size = n, family = word_font),
    ) +
    ggplot2::scale_color_identity() +

    # Decide size and color of the points
    ggplot2::geom_point(
      data = word_data1_all[word_data1_all$extremes_all_x == 1, ],
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
      plot.title = element_text(hjust = 0.5),
      legend.justification = c("right", "top"),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      axis.text.y = y_axes_values,
      title = ggplot2::element_text(color = titles_color),
      axis.title.x = ggplot2::element_text(color = titles_color),
      axis.title.y = ggplot2::element_text(color = titles_color)
    )
  final_plot <- plot

  output_plot_data <- list(final_plot, textCentralityPlot_comment, word_data1_all)
  names(output_plot_data) <- c("final_plot", "description", "processed_word_data")
  output_plot_data
}
###### End textCentralityPlot
