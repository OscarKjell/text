# # TODO Possibility to set this one? It may be that no words comes within this
# # TODO Possibility to set this one? It may be that no words comes within this textCentralitylot
# Just want to learn why this: # Position the embedding; i.e., taking the word embedding subtracted with aggregated word embedding

words <- c("words", "text", "hello", "-", "-")
#help(as_tibble)

#' Takes all words as input and arrange them in column with an accompanying column with frequency.
#' @param words Words
#' @return Column with all words and an accompanying column with their frequency.
#' @importFrom tibble as_tibble
#' @noRd
unique_freq_words <- function(words) {
  words_group1 <- data.frame(unlist(strsplit(tolower(words), " ")))
  # Remove empty cells (otherwise all words are put within " ", which create problems in getUniqueWordsAndFreq or textCentralityData)
  words_group <- words_group1[words_group1 != ""]
  words_group <- as.character(words_group)
  words_groupb <- tibble::as_tibble(words_group)
  sort(words_groupb$value)
  words_groupb <- table(words_groupb)
  words_groupb_freq <- tibble::as_tibble(words_groupb, .name_repair = make.names)
  colnames(words_groupb_freq) <- c("words", "n")
  words_groupb_freq
}




####################################
####################################
##################
##################   Dot Product Projection
##################
####################################
####################################


#' Compute Dot Product Projection and related variables for plotting words.
#' @param words Word or text variable to be plotted.
#' @param wordembeddings Word embeddings from textEmbed for the words to be plotted
#' (i.e., the aggregated word embeddings for the "words" parameter).
#' @param single_wordembeddings Word embeddings from textEmbed for individual words
#' (i.e., decontextualized embeddings).
#' @param x Numeric variable that the words should be plotted according to on the x-axes.
#' @param y Numeric variable that the words should be plotted according to on the y-axes (y=NULL).
#' @param pca Number of PCA dimensions applied to the word embeddings in the beginning of the function.
#' A number below 1 takes out \% of variance; An integer specify number of components to extract.
#' (default is NULL as this setting has not yet been evaluated).
#' @param aggregation Method to aggregate the word embeddings
#' (default = "mean"; see also "min", "max", and "[CLS]").
#' @param split Method to split the axes
#' (default = "quartile" involving selecting lower and upper quartile; see also "mean"). However, if the variable is
#' only containing two different values (i.e., being dichotomous) mean split is used.
#' @param word_weight_power Compute the power of the frequency of the words and multiply
#' the word embeddings with this in the computation of aggregated word embeddings for
#' group low (1) and group high (2). This increases the weight of more frequent words.
#' @param min_freq_words Option to select words that have occurred a specified number of
#' times (default = 0); when creating the dot product projection line
#' (i.e., single words receive dot product projection and p-value).
#' @param Npermutations Number of permutations in the creation of the null distribution.
#' @param n_per_split A setting to split Npermutations to avoid reaching computer memory limits;
#' the higher the faster, but too high may lead to abortion.
#' @return A dataframe with variables (e.g., including dot product projection, frequencies, p-values)
#' for the individual words that is used for the plotting in the textProjectionPlot function.
#' @examples
#' # Data
#' wordembeddings <- wordembeddings4
#' data <- Language_based_assessment_data_8
#' # Pre-processing data for plotting
#' df_for_plotting <- textProjectionData(
#'   words = data$harmonywords,
#'   wordembeddings = wordembeddings$harmonywords,
#'   single_wordembeddings = wordembeddings$singlewords_we,
#'   x = data$hilstotal,
#'   split = "median",
#'   Npermutations = 10,
#'   n_per_split = 1
#' )
#' df_for_plotting
#' @seealso see \code{\link{textProjectionPlot}}
#' @importFrom tibble as_tibble
#' @importFrom recipes recipe step_center step_scale step_naomit all_numeric prep bake
#' @importFrom tidyr uncount
#' @importFrom dplyr full_join rename starts_with
#' @importFrom stats median sd setNames complete.cases
#' @importFrom purrr as_vector
#' @export
textProjectionData <- function(words,
                               wordembeddings, # better to have these in and aggregate according to them as it becomes context (BERT) aggregated.
                               single_wordembeddings = single_wordembeddings_df,
                               x,
                               y = NULL,
                               pca = NULL,
                               aggregation = "mean",
                               split = "quartile",
                               word_weight_power = 1,
                               min_freq_words = 0,
                               Npermutations = 10000,
                               n_per_split = 50000) {
  set.seed(2020)
  # PCA on single_wordembeddings
  if (is.numeric(pca)) {
    # Select word embeddings to be included in plot
    uniques_words_all <- unique_freq_words(words)
    uniques_words_all_wordembedding <- sapply(uniques_words_all$words, applysemrep, single_wordembeddings)
    uniques_words_all_wordembedding <- tibble::as_tibble(t(uniques_words_all_wordembedding))

    rec_pca <- recipes::recipe(~., data = uniques_words_all_wordembedding)
    pca_trans <- rec_pca %>%
      recipes::step_center(recipes::all_numeric()) %>%
      recipes::step_scale(recipes::all_numeric()) %>%
      recipes::step_naomit(Dim1, skip = TRUE)

    if (pca < 1) { #pca=0.9
      pca_trans <- recipes::step_pca(pca_trans, recipes::all_numeric(), threshold = pca)
    } else if (pca >= 1) {
      pca_trans <- recipes::step_pca(pca_trans, recipes::all_numeric(), num_comp = pca)
    }

    pca_estimates <- recipes::prep(pca_trans, training = uniques_words_all_wordembedding)
    pca_data <- recipes::bake(pca_estimates, uniques_words_all_wordembedding)
    pca_data <- pca_data %>% stats::setNames(paste0("Dim_", names(.)))
    single_wordembeddings <- dplyr::bind_cols(uniques_words_all, pca_data)
    single_wordembeddings
  }

  # Make dataframe (and combine x and y)
  if (is.null(y)) {
    x <- tibble::as_tibble_col(x)
  } else {
    # Combine the dimensions for for-loop
    x <- tibble::tibble(x, y)
  }

  # Creating a list for the x and y dimensions
  word_data_list <- list()

  # For-loop for x and y input/dimensions; i.e., y if the plot has two dimensions (i_dim=1 i_dim=2) remove(i_dim)
  for (i_dim in seq_len(ncol(x))) {

    # Get the word embeddings and scale/category for the plot dimension (i.e., x or y from above)
    x0 <- x[i_dim]
    x1 <- cbind(words, x0)
    colnames(x1) <- c("words", "value")
    x2 <- tibble::as_tibble(cbind(x1, wordembeddings))

    # Splitting datasets up to low versus high according to median split
    #group1 <- x2[x2[2] < stats::median(purrr::as_vector(x2$value), na.rm = TRUE), ]
    #group2 <- x2[x2[2] > stats::median(purrr::as_vector(x2$value), na.rm = TRUE), ]
    group1 <- x2 %>%
      dplyr::filter(value < mean(purrr::as_vector(value), na.rm = TRUE))

    group2 <- x2 %>%
      dplyr::filter(value > mean(purrr::as_vector(value), na.rm = TRUE))

    # Use function addEqualNrNArows from 3_1_testSimilarity
    # Function adds rows of NA until group2 and group1 have the same amount of rows.
    if (nrow(group1) < nrow(group2)) {
      group1 <- addEqualNrNArows(group1, group2)
    } else if (nrow(group1) > nrow(group2)) {
      group2 <- addEqualNrNArows(group2, group1)
    } else {
      group1 <- group1
      group2 <- group2
    }

    ##########
    ####        Get word embeddings
    ##########
    # Group 1: getting unique words and their frequency
    words_group1b_freq <- unique_freq_words(group1$words)
    words_group1b_freq <- words_group1b_freq[words_group1b_freq$n >= min_freq_words, ]
    words_group1b_freq$n_g1_g2 <- words_group1b_freq$n * -1
    # Get word embeddings for each word (applysemrep function is created in 1_1_textEmbedd).
    words_group1_single_wordembedding <- lapply(words_group1b_freq$words, applysemrep, single_wordembeddings)
    words_group1_single_wordembedding_b <- dplyr::bind_rows(words_group1_single_wordembedding)

    # Group 2
    words_group2b_freq <- unique_freq_words(group2$words)
    words_group2b_freq <- words_group2b_freq[words_group2b_freq$n >= min_freq_words, ]
    words_group2b_freq$n_g1_g2 <- words_group2b_freq$n * 1
    words_group2_single_wordembedding <- lapply(words_group2b_freq$words, applysemrep, single_wordembeddings)
    words_group2_single_wordembedding_b <- dplyr::bind_rows(words_group2_single_wordembedding)

    # All: Group 1 & 2
    #words_group1_2_freq <- unique_freq_words(x2$words)
    #words_group1_2_freq_b <- words_group1_2_freq[words_group1_2_freq$n >= min_freq_words, ]
    #words_group1_2_freq_b <- dplyr::rename(words_group1_2_freq_b, n_all = n)
    #words_group1_2_single_wordembedding <- lapply(words_group1_2_freq_b$words, applysemrep, single_wordembeddings)

    ############
    ######         1 Create COMPARISON/Projection embedding: all Group 1 & Group 2 word embeddings.
    ############
    ### Sum all word embeddings in one column

    # split="median" split = "quartile"
    if (split == "mean") {
      words_group1_agg_single_wordembedding_c <- cbind(words_group1b_freq, words_group1_single_wordembedding_b)
      words_group2_agg_single_wordembedding_c <- cbind(words_group2b_freq, words_group2_single_wordembedding_b)
    } else if (split == "quartile") {
      # Select according to lower and upper quartile
      # However, if it is a dichotomous variable use mean
      if(length(unique(x1$value)) == 2){
        q1 <- summary(x1$value)[4][[1]]
        q3 <- summary(x1$value)[4][[1]]
      }else if(length(unique(x1$value)) > 2){
      q1 <- summary(x1$value)[2][[1]]
      q3 <- summary(x1$value)[5][[1]]
      }

      group1_agg <- x2 %>%
        dplyr::filter(x2$value <= q1, )

      group2_agg <- x2 %>%
        dplyr::filter(x2$value >= q3, )

      words_group1_agg_freq <- unique_freq_words(group1_agg$words)
      words_group1_agg_freq1 <- words_group1_agg_freq[words_group1_agg_freq$n >= min_freq_words, ]
      words_group1_agg_single_wordembedding <- lapply(words_group1_agg_freq1$words, applysemrep, single_wordembeddings)
      words_group1_agg_single_wordembedding_b <- dplyr::bind_rows(words_group1_agg_single_wordembedding)
      words_group1_agg_single_wordembedding_c <- cbind(words_group1_agg_freq1, words_group1_agg_single_wordembedding_b)

      words_group2_agg_freq <- unique_freq_words(group2_agg$words)
      words_group2_agg_freq1 <- words_group2_agg_freq[words_group2_agg_freq$n >= min_freq_words, ]
      words_group2_agg_single_wordembedding <- lapply(words_group2_agg_freq1$words, applysemrep, single_wordembeddings)
      words_group2_agg_single_wordembedding_b <- dplyr::bind_rows(words_group2_agg_single_wordembedding)
      words_group2_agg_single_wordembedding_c <- cbind(words_group2_agg_freq1, words_group2_agg_single_wordembedding_b)
    }

    words_group1_agg_single_wordembedding_c <- tibble::as_tibble(words_group1_agg_single_wordembedding_c)
    words_group2_agg_single_wordembedding_c <- tibble::as_tibble(words_group2_agg_single_wordembedding_c)

    # Weight words for aggregated word embedding: Repeat rows according to n word_weight_power
    words_group1_agg_single_wordembedding_d <- words_group1_agg_single_wordembedding_c %>%
      dplyr::mutate(., n1 = n^word_weight_power) %>%
      tidyr::uncount(n1)

    words_group2_agg_single_wordembedding_d <- words_group2_agg_single_wordembedding_c %>%
      dplyr::mutate(., n1 = n^word_weight_power) %>%
      tidyr::uncount(n1)

    Aggregated_word_embedding_group1 <- textEmbeddingAggregation(dplyr::select(words_group1_agg_single_wordembedding_d, dplyr::starts_with("Dim")), aggregation = aggregation)
    Aggregated_word_embedding_group2 <- textEmbeddingAggregation(dplyr::select(words_group2_agg_single_wordembedding_d, dplyr::starts_with("Dim")), aggregation = aggregation)

    ############
    ######         Project embedding
    #############
    projected_embedding <- Aggregated_word_embedding_group2 - Aggregated_word_embedding_group1

    # Position words in relation to Group 2 (High)
    all_unique_words_freq <- unique_freq_words(x2$words)
    # Get word embeddings for each word (applysemrep function is created in 1_1_textEmbedd).
    all_unique_words_we <- lapply(all_unique_words_freq$words, applysemrep, single_wordembeddings)
    all_unique_words_we_b <- dplyr::bind_rows(all_unique_words_we)

    # Position the embedding; i.e., taking the word embedding substracted with aggregated word embedding
    # version 1: word_new = word_old - ((group(high harmony) + group(low harmony)) / 2)
    words_positioned_embeddings <- all_unique_words_we_b - ((t(replicate(nrow(all_unique_words_we_b), Aggregated_word_embedding_group2)) +
      t(replicate(nrow(all_unique_words_we_b), Aggregated_word_embedding_group1))) / 2)

    # Project the embeddings using dot product.
    dot_products_observed <- rowSums(words_positioned_embeddings * t(replicate(nrow(all_unique_words_we_b), projected_embedding)))
    all_unique_words_freq$dot <- dot_products_observed

    ############
    ######         Comparison distributions for Project embedding
    #############
    # Get dataframe with ALL embedding to randomly draw from (without log transformed, and quartiles)
    words_group1_agg_single_wordembedding_e <- cbind(words_group1b_freq, words_group1_single_wordembedding_b)
    words_group1_agg_single_wordembedding_f <- words_group1_agg_single_wordembedding_e %>%
      dplyr::mutate(., n1_e = n) %>%
      tidyr::uncount(n1_e)

    words_group2_agg_single_wordembedding_e <- cbind(words_group2b_freq, words_group2_single_wordembedding_b)
    words_group2_agg_single_wordembedding_f <- words_group2_agg_single_wordembedding_e %>%
      dplyr::mutate(., n1_e = n) %>%
      tidyr::uncount(n1_e)

    words_group1_2_agg_single_wordembedding_e <- rbind(words_group1_agg_single_wordembedding_f, words_group2_agg_single_wordembedding_f)
    words_group1_2_agg_single_wordembedding_e1 <- dplyr::select(words_group1_2_agg_single_wordembedding_e, dplyr::starts_with("Dim"))


    # Splitting up the permutations in different loops to avoid memory issues
    forloops <- ceiling(Npermutations / n_per_split)
    dot_null_distribution <- list()

    # i=1
    for (i in 1:forloops) {
      ### Create new Projected embedding
      # Randomly split word embeddings into two groups: words_group1_2_agg_single_wordembedding_e1
      ind <- sample(c(TRUE, FALSE), nrow(words_group1_2_agg_single_wordembedding_e1), replace = TRUE)
      Aggregated_word_embedding_group1_random <- words_group1_2_agg_single_wordembedding_e1[ind, ]
      Aggregated_word_embedding_group1_random <- textEmbeddingAggregation(Aggregated_word_embedding_group1_random, aggregation = "mean")
      Aggregated_word_embedding_group2_random <- words_group1_2_agg_single_wordembedding_e1[!ind, ]
      Aggregated_word_embedding_group2_random <- textEmbeddingAggregation(Aggregated_word_embedding_group2_random, aggregation = "mean")
      projected_embedding_random <- Aggregated_word_embedding_group2_random - Aggregated_word_embedding_group1_random

      # Select random word embeddings according to setting
      indice <- sample(nrow(words_group1_2_agg_single_wordembedding_e1), n_per_split, replace = TRUE)
      random_group2_embedding <- words_group1_2_agg_single_wordembedding_e1[indice, ]

      # Position the embedding; i.e., taking the word embedding subtracted with aggregated word embedding
      # version 1: word_new = word_old - ((group(high harmony) + group(low harmony)) / 2)
      words_positioned_embeddings_random <- random_group2_embedding - ((t(replicate(nrow(random_group2_embedding), Aggregated_word_embedding_group2)) +
        t(replicate(nrow(random_group2_embedding), Aggregated_word_embedding_group1))) / 2)

      # project the embeddings using dot products
      dot_products_null <- as_tibble(rowSums(words_positioned_embeddings_random * t(replicate(nrow(words_positioned_embeddings_random), projected_embedding_random))))

      dot_null_distribution[i] <- dot_products_null
      dot_null_distribution
    }
    dot_null_distribution <- tibble::as_tibble(unlist(dot_null_distribution))

    ### Compare observed dot-product with null
    dot_null_distribution <- dot_null_distribution[stats::complete.cases(dot_null_distribution), ]
    p_values_dot_prod <- purrr::map(as.list(purrr::as_vector(dot_products_observed)), p_value_comparing_with_Null,
      dot_null_distribution,
      Npermutations = Npermutations, alternative = "two_sided"
    )
    p_values_dot_prod <- unlist(p_values_dot_prod)
    # Sort out dataframe
    dot_result <- cbind(all_unique_words_freq, dot_products_observed, tibble::as_tibble(unlist(p_values_dot_prod)))
    dot_result <- tibble::as_tibble(dot_result)
    colnames(dot_result) <- c("words", "n", "dot", "dot2", "p_values_dot")
    dot_result <- dplyr::select(dot_result, -c(dot2))
    words_group2b_freq <- dplyr::select(words_group2b_freq, -c(n))
    words_group1b_freq <- dplyr::select(words_group1b_freq, -c(n))

    dot_result1 <- dplyr::full_join(dot_result, words_group1b_freq, by = "words")
    dot_result2 <- dplyr::full_join(dot_result1, words_group2b_freq, by = "words")
    dot_result <- tibble::as_tibble(dot_result2)
    colnames(dot_result) <- c("words", "n", "dot", "p_values_dot", "n_g1", "n_g2")

    word_data_list[i_dim] <- list(dot_result)
  }
  # N_participants
  N_participant_responses <- length(words)

  # Arranging it to one tibble; accounting for x versus x and y input
  if (is.null(y) == TRUE) {
    word_data_tibble <- word_data_list[[1]]
    colnames(word_data_tibble) <-
      c("words", "n", "dot.x", "p_values_dot.x", "n_g1.x", "n_g2.x")
    word_data_tibble$n.percent <- word_data_tibble$n / sum(word_data_tibble$n)
    word_data_tibble$N_participant_responses <- c(rep(N_participant_responses, nrow(word_data_tibble)))
  } else {
    word_data_tibble <- dplyr::full_join(word_data_list[[1]], word_data_list[[2]], by = "words")
    word_data_tibble$n <- word_data_tibble$n.x
    word_data_tibble <- dplyr::select(word_data_tibble, -c(n.x, n.y))
    word_data_tibble$n.percent <- word_data_tibble$n / sum(word_data_tibble$n)
    word_data_tibble$N_participant_responses <- c(rep(N_participant_responses, nrow(word_data_tibble)))
  }
  return(word_data_tibble)
}
#### End textProjectionData
#############


#' Plot words according to Dot Product Projections.
#' @param word_data Dataframe from textProjectionData
#' @param k_n_words_to_test Select the k most frequent words to significance
#' test (k = sqrt(100*N); N = number of participant responses). Default = TRUE.
#' @param min_freq_words Select words to significance test that have occurred at least min_freq_words
#' (default = 1).
#' @param plot_n_words_square Select number of significant words in each square of the figure to plot.
#' @param plot_n_words_p Number of significant words to plot (n per x-axes and n per y-axes,
#' where duplicates are removed); selects fist according to lowest p-value and then to frequency.
#' @param plot_n_word_extreme Number of words that are extreme on dot product projection per dimension.
#' (i.e., even if not significant; per dimensions, where duplicates are removed).
#' @param plot_n_word_frequency Number of words based on being most frequent.
#' (i.e., even if not significant).
#' @param plot_n_words_middle Number of words plotted that are in the middle in dot product projection score
#' (i.e., even if not significant;  per dimensions, where duplicates are removed).
#' @param title_top Title (default "  ")
#' @param titles_color Color for all the titles (default: "#61605e")
# @param x_axes If TRUE, plotting on the x_axes.
#' @param y_axes If TRUE, also plotting on the y-axes (default is FALSE). Also plotting on
#' y-axes produces a two dimension 2-dimensional plot, but the textProjectionData function has to
#' have had a variable on the y-axes.
#' @param p_alpha Alpha (default = .05).
#' @param p_adjust_method Method to adjust/correct p-values for multiple comparisons
#' (default = "holm"; see also "none", "hochberg", "hommel", "bonferroni", "BH", "BY",  "fdr").
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
#' @param legend_title Title on the color legend (default: "(DPP)".
#' @param legend_x_axes_label Label on the color legend (default: "(x)".
#' @param legend_y_axes_label Label on the color legend (default: "(y)".
#' @param legend_x_position Position on the x coordinates of the color legend (default: 0.02).
#' @param legend_y_position Position on the y coordinates of the color legend (default: 0.05).
#' @param legend_h_size Height of the color legend (default 0.15).
#' @param legend_w_size Width of the color legend (default 0.15).
#' @param legend_title_size Font size (default: 7).
#' @param legend_number_size Font size of the values in the legend (default: 2).
#' @return A 1- or 2-dimensional word plot.
#' @examples
#' # The test-data included in the package is called: DP_projections_HILS_SWLS_100
#'
#' # Dot Product Projection Plot
#' plot_projection <- textProjectionPlot(
#'   word_data = DP_projections_HILS_SWLS_100,
#'   k_n_words_to_test = FALSE,
#'   min_freq_words = 1,
#'   plot_n_words_square = 3,
#'   plot_n_words_p = 3,
#'   plot_n_word_extreme = 1,
#'   plot_n_word_frequency = 1,
#'   plot_n_words_middle = 1,
#'   y_axes = FALSE,
#'   p_alpha = 0.05,
#'   title_top = " Dot Product Projection (DPP)",
#'   x_axes_label = "Low vs. High HILS score",
#'   y_axes_label = "Low vs. High SWLS score",
#'   p_adjust_method = "bonferroni",
#'   scale_y_axes_lim = NULL
#' )
#' plot_projection
#'
#' names(DP_projections_HILS_SWLS_100)
#' @seealso see \code{\link{textProjectionData}}
#' @importFrom tibble as_tibble tibble
#' @importFrom dplyr row_number slice mutate mutate_if bind_rows group_by summarize left_join %>%
#' @importFrom tidyr gather separate
#' @importFrom ggplot2 position_jitter element_text element_blank coord_fixed theme
#' theme_void theme_minimal aes labs scale_color_identity
#' @importFrom rlang sym
#' @importFrom cowplot ggdraw draw_plot
#' @importFrom purrr as_vector
#' @export
textProjectionPlot <- function(word_data,
                               k_n_words_to_test = FALSE,
                               min_freq_words = 1,
                               plot_n_words_square = 3,
                               plot_n_words_p = 5,
                               plot_n_word_extreme = 5,
                               plot_n_word_frequency = 5,
                               plot_n_words_middle = 5,
                               titles_color = "#61605e",
                               #x_axes = TRUE,
                               y_axes = FALSE,
                               p_alpha = 0.05,
                               p_adjust_method = "none",
                               title_top = "Dot Product Projection",
                               x_axes_label = "Dot product projection (DPP)",
                               y_axes_label = "Dot product projection (DPP)",
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
                               legend_title = "DPP",
                               legend_x_axes_label = "x",
                               legend_y_axes_label = "y",
                               legend_x_position = 0.02,
                               legend_y_position = 0.02,
                               legend_h_size = 0.2,
                               legend_w_size = 0.2,
                               legend_title_size = 7,
                               legend_number_size = 2) {
  set.seed(2020)

  # Sorting out axes
  #if (x_axes == TRUE) {
    x_axes_1 <- "dot.x"
    p_values_x <- "p_values_dot.x"
  #} else {
  #  x_axes_1 <- NULL
  #  p_values_x <- NULL
  #}

  if (y_axes == TRUE) {
    y_axes_1 <- "dot.y"
    p_values_y <- "p_values_dot.y"
    y_axes_values_hide <- FALSE
  } else if (y_axes == FALSE) {
    y_axes_1 <- NULL
    p_values_y <- NULL
    y_axes_values_hide <- TRUE
  }

  ### Selecting words to plot
  # Computing adjusted p-values with those words selected by min_freq_words
  word_data_padjusted <- word_data[word_data$n >= min_freq_words, ]

  # Computing adjusted p-values with those words selected by: k = sqrt(100*N)
  if (k_n_words_to_test == TRUE) {
    words_k <- sqrt(100 * word_data$N_participant_responses[1])
    word_data_padjusted <- word_data_padjusted %>%
      dplyr::arrange(-n) %>%
      dplyr::slice(0:words_k)
  }

  word_data_padjusted$adjusted_p_values.x <- stats::p.adjust(purrr::as_vector(word_data_padjusted[, p_values_x]), method = p_adjust_method)
  word_data <- dplyr::left_join(word_data, word_data_padjusted[, c("words", "adjusted_p_values.x")], by = "words")

  if (is.null(y_axes_1) == FALSE) {
    # Computing adjusted p-values
    word_data_padjusted_y <- word_data[word_data$n >= min_freq_words, ]
    word_data_padjusted_y$adjusted_p_values.y <- stats::p.adjust(purrr::as_vector(word_data_padjusted_y[, p_values_y]), method = p_adjust_method)
    word_data <- left_join(word_data, word_data_padjusted_y[, c("words", "adjusted_p_values.y")], by = "words")
  }

  # Select only words based on square-position; and then top frequency in each "square" (see legend) plot_n_words_square
  if (is.null(y_axes_1) == TRUE) {
    word_data <- word_data %>%
      dplyr::mutate(square_categories = dplyr::case_when(
        dot.x < 0 & adjusted_p_values.x < p_alpha ~ 1,
        dot.x < 0 & adjusted_p_values.x > p_alpha ~ 2,
        dot.x > 0 & adjusted_p_values.x < p_alpha ~ 3
      ))

    data_p_sq1 <- word_data[word_data$square_categories == 1, ] %>%
      dplyr::arrange(-n) %>%
      dplyr::slice(0:plot_n_words_square)
    #    data_p_sq2 <- word_data[word_data$square_categories==2, ] %>%
    #      dplyr::arrange(-n) %>%
    #      dplyr::slice(0:plot_n_words_square)
    data_p_sq3 <- word_data[word_data$square_categories == 3, ] %>%
      dplyr::arrange(-n) %>%
      dplyr::slice(0:plot_n_words_square)

    data_p_sq_all <- rbind(data_p_sq1, data_p_sq3) # data_p_sq2,
  }

  if (is.null(y_axes_1) == FALSE) {
    # Categorize words to apply specific color plot_n_words_square=1
    word_data <- word_data %>%
      dplyr::mutate(square_categories = dplyr::case_when(
        dot.x < 0 & adjusted_p_values.x < p_alpha & dot.y > 0 & adjusted_p_values.y < p_alpha ~ 1,
        adjusted_p_values.x > p_alpha & dot.y > 0 & adjusted_p_values.y < p_alpha ~ 2,
        dot.x > 0 & adjusted_p_values.x < p_alpha & dot.y > 0 & adjusted_p_values.y < p_alpha ~ 3,
        dot.x < 0 & adjusted_p_values.x < p_alpha & adjusted_p_values.y > p_alpha ~ 4,
        adjusted_p_values.x > p_alpha & adjusted_p_values.y > p_alpha ~ 5,
        dot.x > 0 & adjusted_p_values.x < p_alpha & adjusted_p_values.y > p_alpha ~ 6,
        dot.x < 0 & adjusted_p_values.x < p_alpha & dot.y < 0 & adjusted_p_values.y < p_alpha ~ 7,
        adjusted_p_values.x > p_alpha & dot.y < 0 & adjusted_p_values.y < p_alpha ~ 8,
        dot.x > 0 & adjusted_p_values.x < p_alpha & dot.y < 0 & adjusted_p_values.y < p_alpha ~ 9
      ))

    data_p_sq1 <- word_data[word_data$square_categories == 1, ] %>%
      dplyr::arrange(-n) %>%
      dplyr::slice(0:plot_n_words_square)
    data_p_sq2 <- word_data[word_data$square_categories == 2, ] %>%
      dplyr::arrange(-n) %>%
      dplyr::slice(0:plot_n_words_square)
    data_p_sq3 <- word_data[word_data$square_categories == 3, ] %>%
      dplyr::arrange(-n) %>%
      dplyr::slice(0:plot_n_words_square)
    data_p_sq4 <- word_data[word_data$square_categories == 4, ] %>%
      dplyr::arrange(-n) %>%
      dplyr::slice(0:plot_n_words_square)
    #  data_p_sq5 <- word_data[word_data$square_categories==5, ] %>%
    #    dplyr::arrange(-n) %>%
    #    dplyr::slice(0:plot_n_words_square)
    data_p_sq6 <- word_data[word_data$square_categories == 6, ] %>%
      dplyr::arrange(-n) %>%
      dplyr::slice(0:plot_n_words_square)
    data_p_sq7 <- word_data[word_data$square_categories == 7, ] %>%
      dplyr::arrange(-n) %>%
      dplyr::slice(0:plot_n_words_square)
    data_p_sq8 <- word_data[word_data$square_categories == 8, ] %>%
      dplyr::arrange(-n) %>%
      dplyr::slice(0:plot_n_words_square)
    data_p_sq9 <- word_data[word_data$square_categories == 9, ] %>%
      dplyr::arrange(-n) %>%
      dplyr::slice(0:plot_n_words_square)

    data_p_sq_all <- rbind(
      data_p_sq1, data_p_sq2, data_p_sq3,
      data_p_sq4, data_p_sq6, # data_p_sq5,
      data_p_sq7, data_p_sq8, data_p_sq9
    )
  }


  # Select only words below alpha; and then top dot.x
  data_p_x <- word_data %>%
    dplyr::filter(adjusted_p_values.x < p_alpha) %>%
    dplyr::arrange(-dot.x) %>%
    dplyr::slice(0:plot_n_words_p)

  # Select plot_n_word_extreme and Select plot_n_word_frequency
  word_data_extrem_max_x <- word_data %>%
    dplyr::arrange(-dot.x) %>%
    dplyr::slice(0:plot_n_word_extreme)

  word_data_extrem_min_x <- word_data %>%
    dplyr::arrange(dot.x) %>%
    dplyr::slice(0:plot_n_word_extreme)

  word_data_frequency_x <- word_data %>%
    dplyr::arrange(-n) %>%
    dplyr::slice(0:plot_n_word_frequency)

  # Select the middle range, order according to frequency and then select the plot_n_words_middle = 5
  mean_m_sd_x <- mean(word_data$dot.x, na.rm = TRUE) - (sd(word_data$dot.x, na.rm = TRUE) / 10) # TODO Possibility to set this one? It may be that no words comes within this
  mean_p_sd_x <- mean(word_data$dot.x, na.rm = TRUE) + (sd(word_data$dot.x, na.rm = TRUE) / 10)
  word_data_middle_x <- word_data %>%
    dplyr::filter(dplyr::between(word_data$dot.x, mean_m_sd_x, mean_p_sd_x)) %>%
    dplyr::arrange(-n) %>%
    dplyr::slice(0:plot_n_words_middle) # TODO selecting on frequency again. Perhaps point to have exact middle?

  word_data_x <- word_data %>%
    dplyr::left_join(data_p_sq_all %>%
      dplyr::transmute(words, check_p_square = 1), by = "words") %>%
    dplyr::left_join(data_p_x %>%
      dplyr::transmute(words, check_p_x = 1), by = "words") %>%
    dplyr::left_join(word_data_extrem_max_x %>%
      dplyr::transmute(words, check_extreme_max_x = 1), by = "words") %>%
    dplyr::left_join(word_data_extrem_min_x %>%
      dplyr::transmute(words, check_extreme_min_x = 1), by = "words") %>%
    dplyr::left_join(word_data_frequency_x %>%
      dplyr::transmute(words, check_extreme_frequency_x = 1), by = "words") %>%
    dplyr::left_join(word_data_middle_x %>%
      dplyr::transmute(words, check_middle_x = 1), by = "words") %>%
    dplyr::mutate(extremes_all_x = rowSums(cbind(
      check_p_square, check_p_x, check_extreme_max_x, check_extreme_min_x,
      check_extreme_frequency_x, check_middle_x
    ), na.rm = T))

  if (is.null(y_axes_1) == FALSE) {
    # Computing adjusted p-values
    # Select only words below alpha; and then top dot.x
    data_p_y <- word_data %>%
      dplyr::filter(adjusted_p_values.y < p_alpha) %>%
      dplyr::arrange(-dot.y) %>%
      dplyr::slice(0:plot_n_words_p)

    # Select plot_n_word_extreme and Select plot_n_word_frequency
    word_data_extrem_max_y <- word_data %>%
      dplyr::arrange(-dot.y) %>%
      dplyr::slice(0:plot_n_word_extreme)

    word_data_extrem_min_y <- word_data %>%
      dplyr::arrange(dot.y) %>%
      dplyr::slice(0:plot_n_word_extreme)

    word_data_frequency_y <- word_data %>%
      dplyr::arrange(-n) %>%
      dplyr::slice(0:plot_n_word_frequency)

    # Select the middle range, order according to frequency and then select the plot_n_words_middle =5
    mean_m_sd_y <- mean(word_data$dot.y, na.rm = TRUE) - (sd(word_data$dot.y, na.rm = TRUE) / 10) # TODO Possibility to set this one? It may be that no words comes within this range
    mean_p_sd_y <- mean(word_data$dot.y, na.rm = TRUE) + (sd(word_data$dot.y, na.rm = TRUE) / 10)
    word_data_middle_y <- word_data %>%
      dplyr::filter(dplyr::between(word_data$dot.y, mean_m_sd_y, mean_p_sd_y)) %>%
      dplyr::arrange(-n) %>%
      dplyr::slice(0:plot_n_words_middle) # TODO selecting on frequency again. perhaps point to have exact middle?

    word_data_all <- word_data_x %>%
      dplyr::left_join(data_p_y %>%
        dplyr::transmute(words, check_p_y = 1), by = "words") %>%
      dplyr::left_join(word_data_extrem_max_y %>%
        dplyr::transmute(words, check_extreme_max_y = 1), by = "words") %>%
      dplyr::left_join(word_data_extrem_min_y %>%
        dplyr::transmute(words, check_extreme_min_y = 1), by = "words") %>%
      dplyr::left_join(word_data_frequency_y %>%
        dplyr::transmute(words, check_extreme_frequency_y = 1), by = "words") %>%
      dplyr::left_join(word_data_middle_y %>%
        dplyr::transmute(words, check_middle_y = 1), by = "words") %>%
      dplyr::mutate(extremes_all_y = rowSums(cbind(
        check_p_y, check_extreme_max_y, check_extreme_min_y,
        check_extreme_frequency_y, check_middle_y
      ), na.rm = T)) %>%
      dplyr::mutate(extremes_all = rowSums(cbind(extremes_all_x, extremes_all_y), na.rm = T))


    # Categorise words to apply specific color
    word_data_all <- word_data_all %>%
      dplyr::mutate(colour_categories = dplyr::case_when(
        dot.x < 0 & adjusted_p_values.x < p_alpha & dot.y > 0 & adjusted_p_values.y < p_alpha ~ bivariate_color_codes[1],
        adjusted_p_values.x > p_alpha & dot.y > 0 & adjusted_p_values.y < p_alpha ~ bivariate_color_codes[2],
        dot.x > 0 & adjusted_p_values.x < p_alpha & dot.y > 0 & adjusted_p_values.y < p_alpha ~ bivariate_color_codes[3],
        dot.x < 0 & adjusted_p_values.x < p_alpha & adjusted_p_values.y > p_alpha ~ bivariate_color_codes[4],
        adjusted_p_values.x > p_alpha & adjusted_p_values.y > p_alpha ~ bivariate_color_codes[5],
        dot.x > 0 & adjusted_p_values.x < p_alpha & adjusted_p_values.y > p_alpha ~ bivariate_color_codes[6],
        dot.x < 0 & adjusted_p_values.x < p_alpha & dot.y < 0 & adjusted_p_values.y < p_alpha ~ bivariate_color_codes[7],
        adjusted_p_values.x > p_alpha & dot.y < 0 & adjusted_p_values.y < p_alpha ~ bivariate_color_codes[8],
        dot.x > 0 & adjusted_p_values.x < p_alpha & dot.y < 0 & adjusted_p_values.y < p_alpha ~ bivariate_color_codes[9]
      ))
  }

  if (is.null(y_axes_1) == TRUE) {
    word_data_all <- word_data_x %>%
      dplyr::mutate(colour_categories = dplyr::case_when(
        dot.x < 0 & adjusted_p_values.x < p_alpha ~ bivariate_color_codes[4],
        dot.x < 0 & adjusted_p_values.x > p_alpha ~ bivariate_color_codes[5],
        dot.x > 0 & adjusted_p_values.x < p_alpha ~ bivariate_color_codes[6]
      ))
  }

  # This solution is because it is not possible to send "0" as a parameter
  if (is.null(y_axes_1) == TRUE) {
    only_x_dimension <- 0
    y_axes_1 <- "only_x_dimension"
  }

  # Add or Remove values on y-axes
  if (y_axes_values_hide) {
    y_axes_values <- ggplot2::element_blank()
  } else {
    y_axes_values <- ggplot2::element_text()
  }

  # Word data adjusted for if y_axes exists
  if (y_axes == TRUE) {
    word_data_all_yadjusted <- word_data_all[word_data_all$extremes_all_x == 1 | word_data_all$extremes_all_y == 1, ]
  } else if (y_axes == FALSE) {
    word_data_all_yadjusted <- word_data_all[word_data_all$extremes_all_x == 1, ]
  }

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

    # ggrepel geom, make arrows transparent, color by rank, size by n
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
    "1 - 3" = "#0078FF", "2 - 3" = "blue", "3 - 3" = "#49FF00",
    "1 - 2" = "#8700FF", "2 - 2" = "#B8B8B8", "3 - 2" = "#34AC04",
    "1 - 1" = "#FF1300", "2 - 1" = "#FF8300", "3 - 1" = "#04AC93"
  )
  bivariate_color_data <- rbind(bivariate_color_data, bivariate_color_codes)
  bivariate_color_data <- bivariate_color_data[-1, ]

  if (y_axes_1 == "only_x_dimension") {
    # Only select 3 colors
    bivariate_color_data <- bivariate_color_data[, c(4, 5, 6)]
    colnames(bivariate_color_data) <- c("1 - 2", "2 - 2", "3 - 2")
    bivariate_color_data
    # Remove the y axes title on the legend
    legend_y_axes_label <- " "
  }
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
    #    ggplot2::annotate(geom="text", x=2, y=2, label="ns",
    #               color = titles_color, size=legend_number_size)+
    {if (y_axes_1 != "only_x_dimension" )
      ggplot2::annotate(
      geom = "text", x = 1, y = 3, label = sum(word_data_all$colour_categories == bivariate_color_codes[1], na.rm = T),
      color = titles_color, size = legend_number_size
    ) } +
    {if (y_axes_1 != "only_x_dimension" )
      ggplot2::annotate(
      geom = "text", x = 2, y = 3, label = sum(word_data_all$colour_categories == bivariate_color_codes[2], na.rm = T),
      color = titles_color, size = legend_number_size
    ) }+
    {if (y_axes_1 != "only_x_dimension" )
      ggplot2::annotate(
      geom = "text", x = 3, y = 3, label = sum(word_data_all$colour_categories == bivariate_color_codes[3], na.rm = T),
      color = titles_color, size = legend_number_size
    ) }+
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
    {if (y_axes_1 != "only_x_dimension" )
      ggplot2::annotate(
      geom = "text", x = 1, y = 1, label = sum(word_data_all$colour_categories == bivariate_color_codes[7], na.rm = T),
      color = titles_color, size = legend_number_size
    ) }+
    {if (y_axes_1 != "only_x_dimension" )
      ggplot2::annotate(
      geom = "text", x = 2, y = 1, label = sum(word_data_all$colour_categories == bivariate_color_codes[8], na.rm = T),
      color = titles_color, size = legend_number_size
    ) } +
    {if (y_axes_1 != "only_x_dimension" )
      ggplot2::annotate(
      geom = "text", x = 3, y = 1, label = sum(word_data_all$colour_categories == bivariate_color_codes[9], na.rm = T),
      color = titles_color, size = legend_number_size
    )} +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, size = legend_title_size + 1),
      title = ggplot2::element_text(color = titles_color),
      axis.title.x = ggplot2::element_text(color = titles_color),
      axis.title = ggplot2::element_text(size = legend_title_size),
      axis.title.y = ggplot2::element_text(angle = 90, color = titles_color)
    ) +
    ggplot2::coord_fixed()
  legend

  # Plot both figure and legend
  suppressWarnings(cowplot::ggdraw() +
    cowplot::draw_plot(plot, 0, 0, 1, 1) +
    cowplot::draw_plot(legend, legend_x_position, legend_y_position, legend_h_size, legend_w_size))
}
###### End textProjectionPlot




####################################
####################################
##################
##################   Semantic Centrality Plot SC
##################
####################################
####################################



#' Compute cosine semantic similarity score between single words' word embeddings
#' and the aggregated word embedding of all words.
#' @param words Word or text variable to be plotted.
#' @param wordembeddings Word embeddings from textEmbed for the words to be plotted
#' (i.e., the aggregated word embeddings for the "words" variable).
#' @param single_wordembeddings Word embeddings from textEmbed for individual words
#' (i.e., the decontextualized word embeddings).
#' @param aggregation Method to aggregate the word embeddings
#' (default = "mean"; see also "min", "max" or "[CLS]").
#' @param min_freq_words Option to  select words that have at least occurred a specified
#' number of times (default = 0); when creating the semantic similarity
#' scores within cosine similarity.
#' @return A dataframe with variables (e.g., including semantic similarity, frequencies)
#' for the individual words that are used for the plotting in the textCentralityPlot function.
#' @examples
#' wordembeddings <- wordembeddings4
#' data <- Language_based_assessment_data_8
#' df_for_plotting <- textCentralityData(
#'   data$harmonywords,
#'   wordembeddings$harmonywords,
#'   wordembeddings$singlewords_we
#' )
#' df_for_plotting
#' @seealso see \code{\link{textCentralityPlot}}  \code{\link{textProjectionData}}
#' @importFrom dplyr bind_rows
#' @importFrom tibble tibble
#' @export
textCentralityData <- function(words,
                               wordembeddings, # better to have these in and aggregate according to them as it becomes context (BERT) aggregated.
                               single_wordembeddings = single_wordembeddings_df,
                               aggregation = "mean",
                               # word_weight_power = 1,
                               min_freq_words = 0) {

  # Create Central Point by aggregating all word embeddings
  Central_Point <- textEmbeddingAggregation(wordembeddings, aggregation = "mean")

  # Select embeddings for unique words
  # Group 1: getting unique words and their frequency min_freq_words=3
  all_unique_freq_words <- unique_freq_words(words)
  all_unique_freq_words_min_freq <- all_unique_freq_words[all_unique_freq_words$n >= min_freq_words, ]

  # Get word embeddings for each word (applysemrep function is created in 1_1_textEmbedd).
  all_single_wordembedding_a <- lapply(all_unique_freq_words_min_freq$words, applysemrep, single_wordembeddings)
  all_single_wordembedding_a1 <- dplyr::bind_rows(all_single_wordembedding_a)

  # Compute Cosine
  central_cosine <- cosines(all_single_wordembedding_a1, t(replicate(nrow(all_single_wordembedding_a1), Central_Point)))
  cenrtal_cosine_df <- tibble::tibble(all_unique_freq_words_min_freq[, 1:2], central_cosine)
  cenrtal_cosine_df$n_percent <- cenrtal_cosine_df$n / sum(cenrtal_cosine_df$n)
  return(cenrtal_cosine_df)
}
# End Semantic Centrality Plot data

#' Plot words according to cosine semantic similarity to the aggregated word embedding.
#' @param word_data Tibble from textPlotData.
#' @param min_freq_words Select words to significance test that have occurred
#' at least min_freq_words (default = 1).
#' @param plot_n_word_extreme Number of words per dimension to plot with extreme
#' dot product projection value.
#' (i.e., even if not significant;  duplicates are removed).
#' @param plot_n_word_frequency Number of words to plot according to their frequency.
#' (i.e., even if not significant).
#' @param plot_n_words_middle Number of words to plot that are in the middle in dot product projection score
#' (i.e., even if not significant; duplicates are removed).
#' @param title_top Title (default "  ").
#' @param titles_color Color for all the titles (default: "#61605e").
#' @param x_axes Variable to be plotted on the x-axes (default is "central_cosine").
#' @param x_axes_label Label on the x-axes.
#' @param scale_x_axes_lim Length of the x-axes (default: NULL, which uses
#' c(min(word_data$central_cosine)-0.05, max(word_data$central_cosine)+0.05);
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
#' @param legend_title Title of the color legend (default: "(DPP)").
#' @param legend_x_axes_label Label on the color legend (default: "(x)".
#' @param legend_x_position Position on the x coordinates of the color legend (default: 0.02).
#' @param legend_y_position Position on the y coordinates of the color legend (default: 0.05).
#' @param legend_h_size Height of the color legend (default 0.15).
#' @param legend_w_size Width of the color legend (default 0.15).
#' @param legend_title_size Font size of the title (default = 7).
#' @param legend_number_size Font size of the values in the legend (default = 2).
#' @return A 1-dimensional word plot based on cosine similarity to the aggregated word embedding.
#' @seealso see \code{\link{textCentralityData}} and \code{\link{textProjectionData}}
#' @examples
#' # The test-data included in the package is called: centrality_data_harmony
#' names(centrality_data_harmony)
#' # Plot
#' # centrality_plot <- textCentralityPlot(
#' #  word_data = centrality_data_harmony,
#' #  min_freq_words = 10,
#' #  plot_n_word_extreme = 10,
#' #  plot_n_word_frequency = 10,
#' #  plot_n_words_middle = 10,
#' #  titles_color = "#61605e",
#' #  x_axes = "central_cosine",
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
#' @importFrom ggplot2 position_jitter element_text element_blank coord_fixed theme theme_void theme_minimal aes labs scale_color_identity
#' @importFrom rlang sym
#' @export
textCentralityPlot <- function(word_data,
                               min_freq_words = 1,
                               plot_n_word_extreme = 10,
                               plot_n_word_frequency = 10,
                               plot_n_words_middle = 10,
                               titles_color = "#61605e",
                               x_axes = "central_cosine",
                               title_top = "Semantic Centrality Plot",
                               x_axes_label = "Semantic Centrality",
                               scale_x_axes_lim = NULL,
                               scale_y_axes_lim = NULL,
                               word_font = NULL,
                               centrality_color_codes = c("#EAEAEA", "#85DB8E", "#398CF9", "#000000"),
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
                               legend_number_size = 2) {
  set.seed(2020)
  y_axes_label <- NULL
  y_axes_values <- element_blank()

  # Selected min_freq_words
  word_data <- word_data[word_data$n >= min_freq_words, ]

  # Select plot_n_word_extreme and Select plot_n_word_frequency
  word_data_extrem_max_x <- word_data %>%
    dplyr::arrange(-central_cosine) %>%
    dplyr::slice(0:plot_n_word_extreme)

  word_data_extrem_min_x <- word_data %>%
    dplyr::arrange(central_cosine) %>%
    dplyr::slice(0:plot_n_word_extreme)

  word_data_frequency_x <- word_data %>%
    dplyr::arrange(-n) %>%
    dplyr::slice(0:plot_n_word_frequency)


  # Select the middle range, order according to frequency and then select the plot_n_words_middle = 5
  mean_m_sd_x <- mean(word_data$central_cosine, na.rm = TRUE) - (sd(word_data$central_cosine, na.rm = TRUE) / 10) # TODO Possibility to set this one? It may be that no words comes within this
  mean_p_sd_x <- mean(word_data$central_cosine, na.rm = TRUE) + (sd(word_data$central_cosine, na.rm = TRUE) / 10)
  word_data_middle_x <- word_data %>%
    dplyr::filter(dplyr::between(word_data$central_cosine, mean_m_sd_x, mean_p_sd_x)) %>%
    dplyr::arrange(-n) %>%
    dplyr::slice(0:plot_n_words_middle) # TODO selecting on frequency again. perhaps point to have exact middle?

  word_data_all <- word_data %>%
    dplyr::left_join(word_data_extrem_max_x %>%
                       dplyr::transmute(words, check_extreme_max_x = 1), by = "words") %>%
    dplyr::left_join(word_data_extrem_min_x %>%
                       dplyr::transmute(words, check_extreme_min_x = 1), by = "words") %>%
    dplyr::left_join(word_data_frequency_x %>%
                       dplyr::transmute(words, check_extreme_frequency_x = 1), by = "words") %>%
    dplyr::left_join(word_data_middle_x %>%
                       dplyr::transmute(words, check_middle_x = 1), by = "words") %>%
    dplyr::mutate(extremes_all_x = rowSums(cbind(
      check_extreme_max_x, check_extreme_min_x,
      check_extreme_frequency_x, check_middle_x
    ), na.rm = T))

  # Categorise words to apply specific color
  word_data_all <- word_data_all %>%
    dplyr::mutate(colour_categories = dplyr::case_when(
      check_extreme_min_x == 1 ~ centrality_color_codes[1],
      check_middle_x == 1 ~ centrality_color_codes[2],
      check_extreme_max_x == 1 ~ centrality_color_codes[3],
      check_extreme_frequency_x == 1 ~ centrality_color_codes[4]
    ))

  if (is.null(scale_x_axes_lim)) {
    scale_x_axes_lim <- c(min(word_data$central_cosine) - 0.05, max(word_data$central_cosine) + 0.05)
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
    ggplot2::ggplot(data = word_data_all, ggplot2::aes(!!rlang::sym(x_axes), !!rlang::sym(y_axes), label = words)) +
    ggplot2::geom_point(
      data = word_data_all, # [word_data_all$extremes_all_x==0, ]
      size = points_without_words_size,
      alpha = points_without_words_alpha,
      ggplot2::aes(color = "#EAEAEA")
    ) +

    # ggrepel geom, make arrows transparent, color by rank, size by n
    ggrepel::geom_text_repel(
      data = word_data_all[word_data_all$extremes_all_x == 1, ],
      segment.alpha  = arrow_transparency,
      position = ggplot2::position_jitter(h = position_jitter_hight, w = position_jitter_width),
      ggplot2::aes(color = colour_categories, size = n, family = word_font),
    ) +
    ggplot2::scale_color_identity() +

    # Decide size and color of the points
    ggplot2::geom_point(
      data = word_data_all[word_data_all$extremes_all_x == 1, ],
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
  plot
}
###### End textCentralityPlot




####################################
####################################
##################
##################   text2DData and text2DPlot
##################
####################################
####################################


## # Data
#wordembeddings <- wordembeddings4$harmonywords
#single_wordembeddings <- wordembeddings4$singlewords_we
#words <- Language_based_assessment_data_8$harmonywords
#
#
#df_for_plotting2d <- text2dData(words = Language_based_assessment_data_8$harmonywords,
#                              single_wordembeddings =  wordembeddings4$singlewords_we)


#' Compute 2 PCA dimensions of the word embeddings for individual words.
#' @param words Word or text variable to be plotted.
#' @param single_wordembeddings Word embeddings from textEmbed for individual words
#' (i.e., decontextualized embeddings).
#' @return A dataframe with words, their frquency and two PCA dimensions from the wordembeddings
#' for the individual words that is used for the plotting in the text2dPlot function.
#' @examples
#' # Data
#' df_for_plotting2d <- text2DData(words = Language_based_assessment_data_8$harmonywords,
#'                                 single_wordembeddings =  wordembeddings4$singlewords_we)
#' df_for_plotting2d
#' @seealso see \code{\link{textProjectionPlot}}
#' @importFrom tibble as_tibble
#' @importFrom recipes recipe step_center step_scale step_naomit all_numeric prep bake
#' @export
text2DData <- function(words,
                       single_wordembeddings = single_wordembeddings_df) {
  set.seed(2020)
  # PCA on single_wordembeddings
  # Select word embeddings to be included in plot
  uniques_words_all <- unique_freq_words(words)
  uniques_words_all_wordembedding <- sapply(uniques_words_all$words, applysemrep, single_wordembeddings)
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

  outpudata <- dplyr::bind_cols(uniques_words_all, pca_data)
  outpudata
}

#df_for_plotting2d <- text2DData(words = Language_based_assessment_data_8$harmonywords,
#                                single_wordembeddings =  wordembeddings4$singlewords_we)


#library(tidyverse)
#satisfactionwords <- tibble::as_tibble_col(Language_based_assessment_data_8$satisfactionwords)
#wordembeddings_satisfactionwords <- textEmbed(satisfactionwords)
##
#satisfactionwords <- text2DData(words = Language_based_assessment_data_8$satisfactionwords,
#                               single_wordembeddings = wordembeddings_satisfactionwords$singlewords_we)
#satisfactionwords

# Example data
#word_data = df_for_plotting2d[complete.cases(df_for_plotting2d),]
#word_data = harmonywordsdata #df_for_plotting2d
#min_freq_words = 2
##plot_n_words_square = 1
##plot_n_words_p = 1
#plot_n_word_extreme = 10
#plot_n_word_frequency = 5
#plot_n_words_middle = 3
#titles_color = "#61605e"
#title_top = "PC"
#x_axes_label = "PC1"
#y_axes_label = "PC2"
#scale_x_axes_lim = NULL # c(-6, 6)
#scale_y_axes_lim = NULL # c(-6, 6)
#word_font = NULL
#bivariate_color_codes = c(
#  "#398CF9", "#60A1F7", "#5dc688",
#  "#e07f6a", "#808080", "#40DD52",
#  "#FF0000", "#EA7467", "#85DB8E", "#398CF9")
#word_size_range = c(3, 8)
#position_jitter_hight = .0
#position_jitter_width = .03
#point_size = 0.7
#arrow_transparency = 0.6
#points_without_words_size = 0.7
#points_without_words_alpha = 0.5
#legend_title = "PC"
#legend_x_axes_label = "PC1"
#legend_y_axes_label = "PC2"
#legend_x_position = 0.02
#legend_y_position = 0.02
#legend_h_size = 0.2
#legend_w_size = 0.2
#legend_title_size = 7
#legend_number_size = 2



#' Plot words according to Dot Product Projections.
#' @param word_data Dataframe from textProjectionData
#' @param min_freq_words Select words to significance test that have occurred at least min_freq_words
#' (default = 1).
#' @param plot_n_word_extreme Number of words that are extreme on dot product projection per dimension.
#' (i.e., even if not significant; per dimensions, where duplicates are removed).
#' @param plot_n_word_frequency Number of words based on being most frequent.
#' (i.e., even if not significant).
#' @param plot_n_words_middle Number of words plotted that are in the middle in dot product projection score
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
#' @param legend_title Title on the color legend (default: "(DPP)".
#' @param legend_x_axes_label Label on the color legend (default: "(x)".
#' @param legend_y_axes_label Label on the color legend (default: "(y)".
#' @param legend_x_position Position on the x coordinates of the color legend (default: 0.02).
#' @param legend_y_position Position on the y coordinates of the color legend (default: 0.05).
#' @param legend_h_size Height of the color legend (default 0.15).
#' @param legend_w_size Width of the color legend (default 0.15).
#' @param legend_title_size Font size (default: 7).
#' @param legend_number_size Font size of the values in the legend (default: 2).
#' @return A 1- or 2-dimensional word plot.
#' @examples
#' # The test-data included in the package is called: DP_projections_HILS_SWLS_100
#'
#' # Dot Product Projection Plot
#' principle_component_plot_projection <- text2DPlot(PC_projections_satisfactionwords_40)
#' principle_component_plot_projection
#'
#' names(DP_projections_HILS_SWLS_100)
#' @seealso see \code{\link{textProjectionData}}
#' @importFrom tibble as_tibble tibble
#' @importFrom dplyr row_number slice mutate mutate_if bind_rows group_by summarize left_join %>%
#' @importFrom tidyr gather separate
#' @importFrom ggplot2 position_jitter element_text element_blank coord_fixed theme
#' theme_void theme_minimal aes labs scale_color_identity
#' @importFrom rlang sym
#' @importFrom cowplot ggdraw draw_plot
#' @importFrom purrr as_vector
#' @export
text2DPlot <- function(word_data,
                       min_freq_words = 1,
                       plot_n_word_extreme = 5,
                       plot_n_word_frequency = 5,
                       plot_n_words_middle = 5,
                       titles_color = "#61605e",
                       title_top = "Principal Component Projection",
                       x_axes_label = "Principal Component 1 (PC1)",
                       y_axes_label = "Principal Component 2 (PC2)",
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
                       legend_number_size = 2) {
  set.seed(2020)

  # Sorting out axes
  x_axes_1 <- "Dim_PC1"
  y_axes_1 <- "Dim_PC2"
  y_axes_values_hide <- FALSE

  ### Selecting words to plot
  # Select min_freq_words
  word_data_original <- word_data
  word_data <- word_data[word_data$n >= min_freq_words, ]

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

  #Select word frequency; plot_n_word_frequency = 5
  word_data_frequency <- word_data %>%
    dplyr::arrange(-n) %>%
    dplyr::slice(0:plot_n_word_frequency)

  # Select the middle range, order according to frequency and then select the plot_n_words_middle = 5
  #PC1
  mean_m_sd_PC1 <- mean(word_data$Dim_PC1, na.rm = TRUE) - (sd(word_data$Dim_PC1, na.rm = TRUE) / 2) # TODO Possibility to set this one? It may be that no words comes within this
  mean_p_sd_PC1 <- mean(word_data$Dim_PC1, na.rm = TRUE) + (sd(word_data$Dim_PC1, na.rm = TRUE) / 2)

  word_data_middle_PC1 <- word_data %>%
    dplyr::filter(dplyr::between(word_data$Dim_PC1, mean_m_sd_PC1, mean_p_sd_PC1)) %>%
    dplyr::arrange(-n) %>%
    dplyr::slice(0:plot_n_words_middle) # TODO selecting on frequency again. perhaps point to have exact middle?

  # PC2
  mean_m_sd_PC2 <- mean(word_data$Dim_PC2, na.rm = TRUE) - (sd(word_data$Dim_PC2, na.rm = TRUE) / 2) # TODO Possibility to set this one? It may be that no words comes within this
  mean_p_sd_PC2 <- mean(word_data$Dim_PC2, na.rm = TRUE) + (sd(word_data$Dim_PC2, na.rm = TRUE) / 2)

  word_data_middle_PC2 <- word_data %>%
    dplyr::filter(dplyr::between(word_data$Dim_PC2, mean_m_sd_PC2, mean_p_sd_PC2)) %>%
    dplyr::arrange(-n) %>%
    dplyr::slice(0:plot_n_words_middle) # TODO selecting on frequency again. perhaps point to have exact middle?

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
    "1 - 3" = "#0078FF", "2 - 3" = "blue",    "3 - 3" = "#49FF00",
    "1 - 2" = "#8700FF", "2 - 2" = "#B8B8B8", "3 - 2" = "#34AC04",
    "1 - 1" = "#FF1300", "2 - 1" = "#FF8300", "3 - 1" = "#04AC93"
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
  suppressWarnings(cowplot::ggdraw() +
                     cowplot::draw_plot(plot, 0, 0, 1, 1) +
                     cowplot::draw_plot(legend, legend_x_position, legend_y_position, legend_h_size, legend_w_size))
}

###### End textProjectionPlot




#text2DPlot(word_data = harmonywordsdata, #df_for_plotting2d, #harmonywordsdata, #df_for_plotting2d,
#           min_freq_words = 2,
#           plot_n_word_extreme = 5,
#           plot_n_word_frequency = 5,
#           plot_n_words_middle = 5,
#           titles_color = "#61605e",
#           title_top = "Principal Component Projection",
#           x_axes_label = "Principal Component 1 (PC1)",
#           y_axes_label = "Principal Component 2 (PC2)",
#           scale_x_axes_lim = NULL,
#           scale_y_axes_lim = NULL,
#           word_font = NULL,
#           bivariate_color_codes = c(
#             "#398CF9", "#60A1F7", "#5dc688",
#             "#e07f6a", "#EAEAEA", "#40DD52",
#             "#FF0000", "#EA7467", "#85DB8E",
#             "#398CF9"
#           ),
#           word_size_range = c(3, 8),
#           position_jitter_hight = .0,
#           position_jitter_width = .03,
#           point_size = 0.5,
#           arrow_transparency = 0.1,
#           points_without_words_size = 0.2,
#           points_without_words_alpha = 0.2,
#           legend_title = "PC",
#           legend_x_axes_label = "PC1",
#           legend_y_axes_label = "PC2",
#           legend_x_position = 0.02,
#           legend_y_position = 0.02,
#           legend_h_size = 0.2,
#           legend_w_size = 0.2,
#           legend_title_size = 7,
#           legend_number_size = 2)
#












