
#### Supervised Dimension Projection ######

#word_embeddings <- word_embeddings_4
#data <- Language_based_assessment_data_8
#single_word_embeddings = word_embeddings$singlewords_we
#words = data$harmonywords[1:5]
#word_embeddings = word_embeddings$harmonywords[1:5, ]
#
#
#x=data$hilstotal[1:5]
#y = NULL
#split = "mean"
#Npermutations = 2
#n_per_split = 1
#pca = 2
#
#aggregation = "mean"
#min_freq_words_test = 0
#word_weight_power = 1


#' Compute Supervised Dimension Projection and related variables for plotting words.
#' @param words Word or text variable to be plotted.
#' @param word_embeddings Word embeddings from textEmbed for the words to be plotted
#' (i.e., the aggregated word embeddings for the "words" parameter).
#' @param single_word_embeddings Word embeddings from textEmbed for individual words
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
#' @param min_freq_words_test Option to select words that have occurred a specified number of
#' times (default = 0); when creating the Supervised Dimension Projection line
#' (i.e., single words receive Supervised Dimension Projection and p-value).
#' @param Npermutations Number of permutations in the creation of the null distribution.
#' @param n_per_split A setting to split Npermutations to avoid reaching computer memory limits;
#' the higher the faster, but too high may lead to abortion.
#' @param seed Set different seed.
#' @return A dataframe with variables (e.g., including Supervised Dimension Projection, frequencies, p-values)
#' for the individual words that is used for the plotting in the textProjectionPlot function.
#' @examples
#' # Data
#' word_embeddings <- word_embeddings_4
#' raw_data <- Language_based_assessment_data_8
#' # Pre-processing data for plotting
#' \dontrun{
#' df_for_plotting <- textProjection(
#'   words = raw_data$harmonywords,
#'   word_embeddings = word_embeddings$harmonywords,
#'   single_word_embeddings = word_embeddings$singlewords_we,
#'   x = raw_data$hilstotal,
#'   split = "mean",
#'   Npermutations = 10,
#'   n_per_split = 1
#' )
#' df_for_plotting
#' }
#' #' @seealso see \code{\link{textProjectionPlot}}
#' @importFrom tibble as_tibble
#' @importFrom recipes recipe step_center step_scale step_naomit all_numeric prep bake
#' @importFrom tidyr uncount
#' @importFrom dplyr full_join rename starts_with n
#' @importFrom stats median sd setNames complete.cases
#' @importFrom purrr as_vector
#' @export
textProjection <- function(words,
                           word_embeddings,
                           single_word_embeddings = single_word_embeddings_df,
                           x,
                           y = NULL,
                           pca = NULL,
                           aggregation = "mean",
                           split = "quartile",
                           word_weight_power = 1,
                           min_freq_words_test = 0,
                           Npermutations = 10000,
                           n_per_split = 50000,
                           seed = 1003) {

  # Description to include as a comment in the end of function
  textProjection_descriptions <- paste(
    "type = textProjection",
    "words =", substitute(words),
    "word_embeddings =", comment(word_embeddings),
    "single_word_embeddings =", comment(single_word_embeddings),
    "x =", substitute(x),
    "y =", substitute(y),
    "pca =", as.character(pca),
    "aggregation = ", aggregation,
    "split = ", split,
    "word_weight_power =", word_weight_power,
    "min_freq_words_test =", min_freq_words_test,
    "Npermutations =", Npermutations,
    "n_per_split =", n_per_split,
    sep = " ", collapse = " "
  )

  set.seed(seed)
  # PCA on single_word_embeddings
  if (is.numeric(pca)) {
    # Select word embeddings to be included in plot
    uniques_words_all <- unique_freq_words(words)
    uniques_words_all_wordembedding <- sapply(uniques_words_all$words, applysemrep, single_word_embeddings)
    uniques_words_all_wordembedding <- tibble::as_tibble(t(uniques_words_all_wordembedding))

    rec_pca <- recipes::recipe(~., data = uniques_words_all_wordembedding)
    pca_trans <- rec_pca %>%
      recipes::step_center(recipes::all_numeric()) %>%
      recipes::step_scale(recipes::all_numeric()) %>%
      recipes::step_naomit(Dim1, skip = TRUE)

    if (pca < 1) {
      pca_trans <- recipes::step_pca(pca_trans, recipes::all_numeric(), threshold = pca)
    } else if (pca >= 1) {
      pca_trans <- recipes::step_pca(pca_trans, recipes::all_numeric(), num_comp = pca)
    }

    pca_estimates <- recipes::prep(pca_trans, training = uniques_words_all_wordembedding)
    pca_data <- recipes::bake(pca_estimates, uniques_words_all_wordembedding)

    #pca_data <- pca_data %>% stats::setNames(paste0("Dim_", names(.)))
    pca_data <- pca_data %>% stats::setNames(paste0("Dim", 1:length(pca_data)))
    single_word_embeddings <- dplyr::bind_cols(uniques_words_all, pca_data)
    single_word_embeddings
  }

  # Make dataframe (and combine x and y)
  if (is.null(y)) {
    x <- tibble::as_tibble_col(x)
  } else {
    # Combine the dimensions for for-loop
    x <- tibble::tibble(x, y)
  }

  # Creating a list for the x and y dimensions; and one to save aggregated word embeddings and
  # dot product null distributions for both x and y
  # so that these can be saved and used for when manually adding words to the plot in the next step.
  word_data_list <- list()
  aggregated_embeddings_dot_null_distribution <- list()

  # For-loop for x and y input/dimensions; i.e., y if the plot has two dimensions (i_dim=1 i_dim=2) remove(i_dim)
  for (i_dim in seq_len(ncol(x))) {

    # Get the word embeddings and scale/category for the plot dimension (i.e., x or y from above)
    x0 <- x[i_dim]
    x1 <- cbind(words, x0)
    colnames(x1) <- c("words", "value")
    x2 <- tibble::as_tibble(cbind(x1, word_embeddings))

    ############
    ######         1 Create COMPARISON/Projection embedding: all Group 1 & Group 2 word embeddings.
    ############

    ### Sum all word embeddings in one column
    if (split == "mean" | split == "quartile") {

      # split="median" split = "quartile" or create interval sensitive
      if (split == "mean") {

        # Splitting datasets up to low versus high according to median split
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
      } else if (split == "quartile") {
        # Select according to lower and upper quartile
        # However, if it is a dichotomous variable use mean
        if (length(unique(x1$value)) == 2) {
          q1 <- summary(x1$value)[4][[1]]
          q3 <- summary(x1$value)[4][[1]]
        } else if (length(unique(x1$value)) > 2) {
          q1 <- summary(x1$value)[2][[1]]
          q3 <- summary(x1$value)[5][[1]]
        }

        group1 <- x2 %>%
          dplyr::filter(x2$value <= q1, )

        group2 <- x2 %>%
          dplyr::filter(x2$value >= q3, )
      }

      ## ##       Get word embeddings ####
      # Group 1: getting unique words and their frequency
      words_group1b_freq <- unique_freq_words(group1$words)
      words_group1b_freq <- words_group1b_freq[words_group1b_freq$n >= min_freq_words_test, ]
      words_group1b_freq$n_g1_g2 <- words_group1b_freq$n * -1

      # Get word embeddings for each word (applysemrep function is created in 1_1_textEmbedStatic).
      words_group1_single_wordembedding <- lapply(words_group1b_freq$words, applysemrep, single_word_embeddings)
      words_group1_single_wordembedding_b <- dplyr::bind_rows(words_group1_single_wordembedding)

      # Group 2
      words_group2b_freq <- unique_freq_words(group2$words)
      words_group2b_freq <- words_group2b_freq[words_group2b_freq$n >= min_freq_words_test, ]
      words_group2b_freq$n_g1_g2 <- words_group2b_freq$n * 1

      words_group2_single_wordembedding <- lapply(words_group2b_freq$words, applysemrep, single_word_embeddings)
      words_group2_single_wordembedding_b <- dplyr::bind_rows(words_group2_single_wordembedding)


      words_group1_agg_single_wordembedding_c <- cbind(words_group1b_freq, words_group1_single_wordembedding_b)
      words_group2_agg_single_wordembedding_c <- cbind(words_group2b_freq, words_group2_single_wordembedding_b)

      words_group1_agg_single_wordembedding_c <- tibble::as_tibble(words_group1_agg_single_wordembedding_c)
      words_group2_agg_single_wordembedding_c <- tibble::as_tibble(words_group2_agg_single_wordembedding_c)

      # Weight words for aggregated word embedding: Repeat rows according to n word_weight_power
      words_group1_agg_single_wordembedding_d <- words_group1_agg_single_wordembedding_c %>%
        dplyr::mutate(., n1 = n^word_weight_power) %>%
        tidyr::uncount(n1)

      words_group2_agg_single_wordembedding_d <- words_group2_agg_single_wordembedding_c %>%
        dplyr::mutate(., n1 = n^word_weight_power) %>%
        tidyr::uncount(n1)

      ## Get dataframe with ALL embeddings to randomly draw from (without log transformed, and quartiles)
      # for Comparison distribution
      words_group1_agg_single_wordembedding_e <- cbind(words_group1b_freq, words_group1_single_wordembedding_b)
      words_group1_agg_single_wordembedding_f <- words_group1_agg_single_wordembedding_e %>%
        dplyr::mutate(., n1_e = n) %>%
        tidyr::uncount(n1_e)

      words_group2_agg_single_wordembedding_e <- cbind(words_group2b_freq, words_group2_single_wordembedding_b)
      words_group2_agg_single_wordembedding_f <- words_group2_agg_single_wordembedding_e %>%
        dplyr::mutate(., n1_e = n) %>%
        tidyr::uncount(n1_e)

      words_group1_2_agg_single_wordembedding_e <- rbind(words_group1_agg_single_wordembedding_f,
                                                         words_group2_agg_single_wordembedding_f)
      words_group1_2_agg_single_wordembedding_e1 <- dplyr::select(words_group1_2_agg_single_wordembedding_e,
                                                                  dplyr::starts_with("Dim"))

      # Interval: No split. Weighting embeddings according to interval scale.
    } else if (split == "no") {

      # Getting unique words and their frequency
      words_group1b_freq <- unique_freq_words(x2$words)
      words_group1b_freq <- words_group1b_freq[words_group1b_freq$n >= min_freq_words_test, ]
      words_group1b_freq$n_g1_g2 <- words_group1b_freq$n * -1
      # Group 2
      words_group2b_freq <- unique_freq_words(x2$words)
      words_group2b_freq <- words_group2b_freq[words_group2b_freq$n >= min_freq_words_test, ]
      words_group2b_freq$n_g1_g2 <- words_group2b_freq$n * 1

      # Get each words on its own row and keep value so that it can be the weight for the word embedding
      words_values_sep <- x2 %>%
        select(words, value) %>%
        tidyr::separate_rows(words, sep = " ")

      # Get word embeddings for each word (applysemrep function is created in 1_1_textEmbedd).
      words_single_wordembedding <- lapply(words_values_sep$words, applysemrep, single_word_embeddings)
      words_single_wordembedding_b <- dplyr::bind_rows(words_single_wordembedding)
      words_single_wordembedding_c <- dplyr::bind_cols(words_values_sep, words_single_wordembedding_b)

      # weight the word embeddings with the value weight
      weights <- words_single_wordembedding_c$value^word_weight_power
      words_single_wordembedding_d <- words_single_wordembedding_c %>%
        select(-c(words, value))

      words_single_wordembedding_d_scaled <- scale(words_single_wordembedding_d)

      words_group2_agg_single_wordembedding_d <- tibble::as_tibble((words_single_wordembedding_d_scaled * weights) / mean(weights))

      # reversed weights
      weight_rev <- (max(words_single_wordembedding_c$value) + 1 - words_single_wordembedding_c$value)^word_weight_power
      words_group1_agg_single_wordembedding_d <- tibble::as_tibble((words_single_wordembedding_d_scaled * weight_rev) / mean(weight_rev))

      ## Get dataframe with ALL embeddings to randomly draw from (without log transformed,
      # and quartiles) for Comparison distribution
      # Shuffle weights/values
      weights_shuffled <- sample(words_single_wordembedding_c$value, replace = FALSE)
      words_single_wordembedding_d_weights_shuffled <- tibble::as_tibble((words_single_wordembedding_d_scaled * weights_shuffled) /
                                                                           mean(weights_shuffled))

      words_group1_2_agg_single_wordembedding_e1 <- words_single_wordembedding_d_weights_shuffled
    }

    Aggregated_word_embedding_group1 <- textEmbeddingAggregation(dplyr::select(words_group1_agg_single_wordembedding_d,
                                                                               dplyr::starts_with("Dim")), aggregation = aggregation)
    Aggregated_word_embedding_group2 <- textEmbeddingAggregation(dplyr::select(words_group2_agg_single_wordembedding_d,
                                                                               dplyr::starts_with("Dim")), aggregation = aggregation)

    ######  Project embedding ####
    projected_embedding <- Aggregated_word_embedding_group2 - Aggregated_word_embedding_group1

    # Position words in relation to Group 2 (High)
    all_unique_words_freq <- unique_freq_words(x2$words)
    # Get word embeddings for each word (applysemrep function is created in 1_1_textEmbedd).
    all_unique_words_we <- lapply(all_unique_words_freq$words, applysemrep, single_word_embeddings)
    all_unique_words_we_b <- dplyr::bind_rows(all_unique_words_we)

    if (split == "no") {
      # Applying the z-score parameters to all the unique word's embeddings
      scale_center_weights <- tibble::as_tibble_row(attr(words_single_wordembedding_d_scaled, "scaled:center")) %>%
        slice(rep(1:dplyr::n(), each = nrow(all_unique_words_we_b)))

      scale_scale_weights <- tibble::as_tibble_row(attr(words_single_wordembedding_d_scaled, "scaled:scale")) %>%
        slice(rep(1:dplyr::n(), each = nrow(all_unique_words_we_b)))

      all_unique_words_we_b <- tibble::as_tibble((all_unique_words_we_b - scale_center_weights) / scale_scale_weights)
    }

    # Position the embedding; i.e., taking the word embedding subtracted with aggregated word embedding
    embedding_to_anchour_with <- tibble::as_tibble_row((Aggregated_word_embedding_group2 +
                                                          Aggregated_word_embedding_group1) / 2)

    embedding_to_anchour_with <- embedding_to_anchour_with %>%
      dplyr::slice(rep(1:dplyr::n(), each = nrow(all_unique_words_we_b)))

    words_positioned_embeddings <- all_unique_words_we_b - embedding_to_anchour_with

    # Project the embeddings using dot product.
    projected_embedding_nrow <- tibble::as_tibble_row(projected_embedding) %>%
      dplyr::slice(rep(1:dplyr::n(), each = nrow(all_unique_words_we_b)))

    dot_products_observed <- rowSums(words_positioned_embeddings * projected_embedding_nrow)

    all_unique_words_freq$dot <- dot_products_observed


    # Computing the dot product projection for the aggregated and projected embeddings
    all_aggregated <- dplyr::bind_rows(Aggregated_word_embedding_group1,
                                       Aggregated_word_embedding_group2, projected_embedding)

    projected_embedding_a <- tibble::as_tibble_row(projected_embedding) %>%
      dplyr::slice(rep(1:dplyr::n(), each = nrow(all_aggregated)))

    dot_products_all_aggregated <- rowSums(all_aggregated * projected_embedding_a)

    DP_aggregate <- tibble::as_tibble_col(c("Group1*", "Group2*", "projected_embedding"), column_name = "words")
    DP_aggregate$n <- c(0, 0, 0)
    DP_aggregate$dot <- dot_products_all_aggregated
    dot_products_observed <- c(as.vector(dot_products_all_aggregated), dot_products_observed)
    # Add DP_aggregate to the words data
    all_unique_words_freq <- rbind(DP_aggregate, all_unique_words_freq)


    ###### Comparison distributions for Project embedding ####

    # Splitting up the permutations in different loops to avoid memory issues
    forloops <- ceiling(Npermutations / n_per_split)
    dot_null_distribution <- list()

    #  i = 1
    for (i in 1:forloops) {
      ### Create new Projected embedding
      # Randomly split word embeddings into two groups: words_group1_2_agg_single_wordembedding_e1
      ind <- sample(c(TRUE, FALSE), nrow(words_group1_2_agg_single_wordembedding_e1), replace = TRUE)
      Aggregated_word_embedding_group1_random <- words_group1_2_agg_single_wordembedding_e1[ind, ]
      Aggregated_word_embedding_group1_random <- textEmbeddingAggregation(Aggregated_word_embedding_group1_random,
                                                                          aggregation = "mean")
      Aggregated_word_embedding_group2_random <- words_group1_2_agg_single_wordembedding_e1[!ind, ]
      Aggregated_word_embedding_group2_random <- textEmbeddingAggregation(Aggregated_word_embedding_group2_random,
                                                                          aggregation = "mean")
      projected_embedding_random <- Aggregated_word_embedding_group2_random - Aggregated_word_embedding_group1_random

      # Select random word embeddings according to setting
      indice <- sample(nrow(words_group1_2_agg_single_wordembedding_e1), n_per_split, replace = TRUE)
      random_group2_embedding <- words_group1_2_agg_single_wordembedding_e1[indice, ]

      # Position the embedding; i.e., taking the word embedding subtracted with aggregated word embedding
      # version 1: word_new = word_old - ((group(high harmony) + group(low harmony)) / 2)

      Aggregated_word_embedding_group1_long <- tibble::as_tibble_row(Aggregated_word_embedding_group1) %>%
        dplyr::slice(rep(1:dplyr::n(), each = nrow(random_group2_embedding)))
      Aggregated_word_embedding_group2_long <- tibble::as_tibble_row(Aggregated_word_embedding_group2) %>%
        dplyr::slice(rep(1:dplyr::n(), each = nrow(random_group2_embedding)))

      words_positioned_embeddings_random <- random_group2_embedding - (Aggregated_word_embedding_group2_long +
                                                                         Aggregated_word_embedding_group1) / 2

      # project the embeddings using dot products

      projected_embedding_random_long <- tibble::as_tibble_row(projected_embedding_random) %>%
        dplyr::slice(rep(1:dplyr::n(), each = nrow(words_positioned_embeddings_random)))

      dot_products_null <- tibble::as_tibble(rowSums(words_positioned_embeddings_random * projected_embedding_random_long))

      dot_null_distribution[i] <- dot_products_null
      dot_null_distribution
    }
    dot_null_distribution <- tibble::as_tibble(unlist(dot_null_distribution))

    ### Compare observed dot-product with null
    dot_null_distribution <- dot_null_distribution[stats::complete.cases(dot_null_distribution), ]
    p_values_dot_prod <- purrr::map(as.list(purrr::as_vector(dot_products_observed)), p_value_comparing_with_Null,
      dot_null_distribution$value,
      alternative = "two_sided"
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
    dot_result$n_g1_g2.y[is.na(dot_result$n_g1_g2.y)] <- 0
    dot_result$n_g1_g2.x[is.na(dot_result$n_g1_g2.x)] <- 0
    colnames(dot_result) <- c("words", "n", "dot", "p_values_dot", "n_g1", "n_g2")


    to_be_saved_below <- list(
      tibble::as_tibble_row(Aggregated_word_embedding_group1),
      tibble::as_tibble_row(Aggregated_word_embedding_group2),
      dot_null_distribution
    )

    names(to_be_saved_below) <- c(
      "Aggregated_word_embedding_group1", "Aggregated_word_embedding_group2",
      "dot_null_distribution"
    )

    # Adding the scale parameters for the word embeddings so that words can be manually added in the textProjectionPlot.
    if (split == "no") {
      to_be_saved_below$scale_centre <- tibble::as_tibble_row(attr(words_single_wordembedding_d_scaled, "scaled:center"))
      to_be_saved_below$scale_scale <- tibble::as_tibble_row(attr(words_single_wordembedding_d_scaled, "scaled:scale"))
    }

    aggregated_embeddings_dot_null_distribution[i_dim] <- list(to_be_saved_below)
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

    # Naming
    names(aggregated_embeddings_dot_null_distribution[[1]]) <- c(
      "Aggregated_word_embedding_group1.x",
      "Aggregated_word_embedding_group2.x",
      "dot_null_distribution.x"
    )

    if (split == "no") {
      names(aggregated_embeddings_dot_null_distribution[[1]]) <- c(
        "Aggregated_word_embedding_group1.x",
        "Aggregated_word_embedding_group2.x",
        "dot_null_distribution.x",
        "scale_centre.x",
        "scale_scale.x"
      )
    }
  } else {
    word_data_tibble <- dplyr::full_join(word_data_list[[1]], word_data_list[[2]], by = "words")
    word_data_tibble$n <- word_data_tibble$n.x
    word_data_tibble <- dplyr::select(word_data_tibble, -c(n.x, n.y))
    word_data_tibble$n.percent <- word_data_tibble$n / sum(word_data_tibble$n)
    word_data_tibble$N_participant_responses <- c(rep(N_participant_responses, nrow(word_data_tibble)))

    # Naming
    names(aggregated_embeddings_dot_null_distribution[[1]]) <- c(
      "Aggregated_word_embedding_group1.x",
      "Aggregated_word_embedding_group2.x",
      "dot_null_distribution.x"
    )
    # Naming
    names(aggregated_embeddings_dot_null_distribution[[2]]) <- c(
      "Aggregated_word_embedding_group1.y",
      "Aggregated_word_embedding_group2.y",
      "dot_null_distribution.y"
    )

    if (split == "no") {
      names(aggregated_embeddings_dot_null_distribution[[1]]) <- c(
        "Aggregated_word_embedding_group1.x",
        "Aggregated_word_embedding_group2.x",
        "dot_null_distribution.x",
        "scale_centre.x",
        "scale_scale.x"
      )

      names(aggregated_embeddings_dot_null_distribution[[2]]) <- c(
        "Aggregated_word_embedding_group1.y",
        "Aggregated_word_embedding_group2.y",
        "dot_null_distribution.y",
        "scale_centre.y",
        "scale_scale.y"
      )
    }
  }


  # }

  word_data_tibble1 <- list(aggregated_embeddings_dot_null_distribution, word_data_tibble)
  names(word_data_tibble1) <- c("background", "word_data")

  comment(word_data_tibble1) <- textProjection_descriptions

  return(word_data_tibble1)
}
#### End textProjection
#############

#' Plot words according to Supervised Dimension Projection.
#' @param word_data Dataframe from textProjection
#' @param k_n_words_to_test Select the k most frequent words to significance
#' test (k = sqrt(100*N); N = number of participant responses). Default = TRUE.
#' @param min_freq_words_test Select words to significance test that have occurred at least min_freq_words_test
#' (default = 1).
#' @param min_freq_words_plot Select words to plot that has occurred at least min_freq_words_plot times.
#' @param plot_n_words_square Select number of significant words in each square of the figure to plot. The significant
#' words, in each square is selected according to most frequent words.
#' @param plot_n_words_p Number of significant words to plot on each(positive and negative) side of the x-axes and y-axes,
#' (where duplicates are removed); selects first according to lowest p-value and then according to frequency. Hence, on a two
#' dimensional plot it is possible that plot_n_words_p = 1 yield 4 words.
#' @param plot_n_word_extreme Number of words that are extreme on Supervised Dimension Projection per dimension.
#' (i.e., even if not significant; per dimensions, where duplicates are removed).
#' @param plot_n_word_frequency Number of words based on being most frequent.
#' (i.e., even if not significant).
#' @param plot_n_words_middle Number of words plotted that are in the middle in Supervised
#' Dimension Projection score (i.e., even if not significant;  per dimensions, where duplicates are removed).
#' @param title_top Title (default "  ")
#' @param titles_color Color for all the titles (default: "#61605e")
# @param x_axes If TRUE, plotting on the x_axes.
#' @param y_axes If TRUE, also plotting on the y-axes (default is FALSE). Also plotting on
#' y-axes produces a two dimension 2-dimensional plot, but the textProjection function has to
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
#' @param bivariate_color_codes The different colors of the words. Note that, at the moment,
#' two squares should not have the exact same colour-code because the numbers within the
#' squares of the legend will then be aggregated (and show the same, incorrect  value).
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
#' @param legend_title Title on the color legend (default: "(SDP)".
#' @param legend_x_axes_label Label on the color legend (default: "(x)".
#' @param legend_y_axes_label Label on the color legend (default: "(y)".
#' @param legend_x_position Position on the x coordinates of the color legend (default: 0.02).
#' @param legend_y_position Position on the y coordinates of the color legend (default: 0.05).
#' @param legend_h_size Height of the color legend (default 0.15).
#' @param legend_w_size Width of the color legend (default 0.15).
#' @param legend_title_size Font size (default: 7).
#' @param legend_number_size Font size of the values in the legend (default: 2).
#' @param group_embeddings1 Shows a point representing the aggregated word embedding for group 1 (default = FALSE).
#' @param group_embeddings2 Shows a point representing the aggregated word embedding for group 2 (default = FALSE).
#' @param projection_embedding Shows a point representing the aggregated direction embedding (default = FALSE).
#' @param aggregated_point_size Size of the points representing the group_embeddings1,
#' group_embeddings2 and projection_embedding
#' @param aggregated_shape Shape type of the points representing the group_embeddings1,
#' group_embeddings2 and projection_embeddingd
#' @param aggregated_color_G1 Color
#' @param aggregated_color_G2 Color
#' @param projection_color Color
#' @param seed Set different seed.
#' @param explore_words Explore where specific words are positioned in the embedding space.
#' For example, c("happy content", "sad down").
#' @param explore_words_color Specify the color(s) of the words being explored.
#' For example c("#ad42f5", "green")
#' @param explore_words_point Specify the names of the point for the aggregated word embeddings
#' of all the explored words.
#' @param explore_words_aggregation Specify how to aggregate the word embeddings of the explored words.
#' @param remove_words manually remove words from the plot (which is done just before the words are
#' plotted so that the remove_words are part of previous counts/analyses).
#' @param space Provide a semantic space if using static embeddings and wanting to explore words.
#' @param n_contrast_group_color Set color to words that have higher frequency (N) on the other
#' opposite side of its dot product projection (default = NULL).
#' @param n_contrast_group_remove Remove words that have higher frequency (N) on the other
#' opposite side of its dot product projection (default = FALSE).
#' @param scaling Scaling word embeddings before aggregation.
#' @return A 1- or 2-dimensional word plot, as well as tibble with processed data used to plot.
#' @examples
#' # The test-data included in the package is called: DP_projections_HILS_SWLS_100
#'
#' # Supervised Dimension Projection Plot
#' plot_projection <- textProjectionPlot(
#'   word_data = DP_projections_HILS_SWLS_100,
#'   k_n_words_to_test = FALSE,
#'   min_freq_words_test = 1,
#'   plot_n_words_square = 3,
#'   plot_n_words_p = 3,
#'   plot_n_word_extreme = 1,
#'   plot_n_word_frequency = 1,
#'   plot_n_words_middle = 1,
#'   y_axes = FALSE,
#'   p_alpha = 0.05,
#'   title_top = "Supervised Dimension Projection (SDP)",
#'   x_axes_label = "Low vs. High HILS score",
#'   y_axes_label = "Low vs. High SWLS score",
#'   p_adjust_method = "bonferroni",
#'   scale_y_axes_lim = NULL
#' )
#' plot_projection
#'
#' names(DP_projections_HILS_SWLS_100)
#' @seealso see \code{\link{textProjection}}
#' @importFrom tibble as_tibble tibble
#' @importFrom dplyr row_number slice mutate mutate_if bind_rows group_by summarize left_join %>% n
#' @importFrom tidyr gather separate
#' @importFrom ggplot2 position_jitter element_text element_blank coord_fixed theme
#' theme_void theme_minimal aes labs scale_color_identity
#' @importFrom rlang sym
#' @importFrom cowplot ggdraw draw_plot
#' @importFrom purrr as_vector
#' @importFrom stringi stri_split_boundaries
#' @export
textProjectionPlot <- function(word_data,
                               k_n_words_to_test = FALSE,
                               min_freq_words_test = 1,
                               min_freq_words_plot = 1,
                               plot_n_words_square = 3,
                               plot_n_words_p = 5,
                               plot_n_word_extreme = 5,
                               plot_n_word_frequency = 5,
                               plot_n_words_middle = 5,
                               titles_color = "#61605e",
                               # x_axes = TRUE,
                               y_axes = FALSE,
                               p_alpha = 0.05,
                               p_adjust_method = "none",
                               title_top = "Supervised Dimension Projection",
                               x_axes_label = "Supervised Dimension Projection (SDP)",
                               y_axes_label = "Supervised Dimension Projection (SDP)",
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
                               legend_title = "SDP",
                               legend_x_axes_label = "x",
                               legend_y_axes_label = "y",
                               legend_x_position = 0.02,
                               legend_y_position = 0.02,
                               legend_h_size = 0.2,
                               legend_w_size = 0.2,
                               legend_title_size = 7,
                               legend_number_size = 2,
                               group_embeddings1 = FALSE,
                               group_embeddings2 = FALSE,
                               projection_embedding = FALSE,
                               aggregated_point_size = 0.8,
                               aggregated_shape = 8,
                               aggregated_color_G1 = "black",
                               aggregated_color_G2 = "black",
                               projection_color = "blue",
                               seed = 1005,
                               explore_words = NULL,
                               explore_words_color = "#ad42f5",
                               explore_words_point = "ALL_1",
                               explore_words_aggregation = "mean",
                               remove_words = NULL,
                               n_contrast_group_color = NULL,
                               n_contrast_group_remove = FALSE,
                               space = NULL,
                               scaling = FALSE) {
  plot <- textPlot(
    word_data = word_data,
    k_n_words_to_test = k_n_words_to_test,
    min_freq_words_test = min_freq_words_test,
    min_freq_words_plot = min_freq_words_plot,
    plot_n_words_square = plot_n_words_square,
    plot_n_words_p = plot_n_words_p,
    plot_n_word_extreme = plot_n_word_extreme,
    plot_n_word_frequency = plot_n_word_frequency,
    plot_n_words_middle = plot_n_words_middle,
    titles_color = titles_color,
    # x_axes = TRUE,
    y_axes = y_axes,
    p_alpha = p_alpha,
    p_adjust_method = p_adjust_method,
    title_top = title_top,
    x_axes_label = x_axes_label,
    y_axes_label = y_axes_label,
    scale_x_axes_lim = scale_x_axes_lim,
    scale_y_axes_lim = scale_y_axes_lim,
    word_font = word_font,
    bivariate_color_codes = bivariate_color_codes,
    word_size_range = word_size_range,
    position_jitter_hight = position_jitter_hight,
    position_jitter_width = position_jitter_width,
    point_size = point_size,
    arrow_transparency = arrow_transparency,
    points_without_words_size = points_without_words_size,
    points_without_words_alpha = points_without_words_alpha,
    legend_title = legend_title,
    legend_x_axes_label = legend_x_axes_label,
    legend_y_axes_label = legend_y_axes_label,
    legend_x_position = legend_x_position,
    legend_y_position = legend_y_position,
    legend_h_size = legend_h_size,
    legend_w_size = legend_w_size,
    legend_title_size = legend_title_size,
    legend_number_size = legend_number_size,
    group_embeddings1 = group_embeddings1,
    group_embeddings2 = group_embeddings2,
    projection_embedding = projection_embedding,
    aggregated_point_size = aggregated_point_size,
    aggregated_shape = aggregated_shape,
    aggregated_color_G1 = aggregated_color_G1,
    aggregated_color_G2 = aggregated_color_G2,
    projection_color = projection_color,
    seed = seed,
    explore_words = explore_words,
    explore_words_color = explore_words_color,
    explore_words_point = explore_words_point,
    explore_words_aggregation = explore_words_aggregation,
    remove_words = remove_words,
    n_contrast_group_color = n_contrast_group_color,
    n_contrast_group_remove = n_contrast_group_remove,
    space = space,
    scaling = scaling
  )

#  plot$warning <- "This function will be depracted: instead use textPlot()"
  return(plot)
}
