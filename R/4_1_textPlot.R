

#' Takes all words as input and arrange them in column with an accompanying column with frequency.
#' @param words Words
#' @return Column with all words and an accompanying column with their frequency.
#' @importFrom tibble as_tibble
#' @noRd
unique_freq_words <- function(words) {
  words_group1 <- data.frame(unlist(strsplit(tolower(words), " ")))
  # Remove empty cells (otherwise all words are put within " ", which create problems in getUniqueWordsAndFreq or textCentrality)
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
##################   Supervised Dimension Projection
##################
####################################
####################################


#library(tidyverse)
#library(text)
#
#
#embeddings_all <- textEmbed(Language_based_assessment_data_8[1:2])
#
#
#proj_plot_test_100 <- textProjection(words = Language_based_assessment_data_8$harmonywords,
#                           wordembeddings = embeddings_all$harmonywords,
#                           single_wordembeddings = embeddings_all$singlewords_we,
#                           x = Language_based_assessment_data_8$hilstotal/1000,
#                           y = NULL,
#                           pca = NULL,
#                           aggregation = "mean",
#                           split = "no",
#                           word_weight_power = 1,
#                           min_freq_words_test = 0,
#                           Npermutations = 1000,
#                           n_per_split = 5000,
#                           seed = 1003)
#help(textProjectionPlot)
#textProjectionPlot(proj_plot_test_100,
#                   group_embeddings1 = T,
#                   group_embeddings2 = T,
#                   projection_embedding = T,)
#
#words = Language_based_assessment_data_8$harmonywords
#wordembeddings = embeddings_all$harmonywords      # better to have these in and aggregate according to them as it becomes context (BERT) aggregated.
#single_wordembeddings = embeddings_all$singlewords_we
#



# Pre-processing data for plotting

#ds <- readRDS("/Users/oscarkjell/Desktop/1 Projects/4 Lab/BSc students/Jenny2/ds.rds")
#text_cols_embeddings <- readRDS("/Users/oscarkjell/Desktop/1 Projects/4 Lab/BSc students/Jenny2/text_cols_embeddings.rds")
#
#HFDHinder_all_inre_motivation_fig_5 <- text::textProjection(
#  words = ds$HFDHinder_all,
#  wordembeddings = text_cols_embeddings$HFDHinder_all,
#  single_wordembeddings = text_cols_embeddings$singlewords_we,
#  x = ds$inre_motivation,
#  y = NULL,
#  pca = NULL,
#  aggregation = "mean",
#  split = "quartile",
#  word_weight_power = 1,
#  min_freq_words_test = 0,
#  Npermutations = 10000,
#  n_per_split = 50000,
#  seed = 1003)


#wordembeddings_hil <- read_rds("/Users/oscarkjell/Desktop/1 Projects/0 Research/0 text r-package/wordembeddings_hil.rds")
#words = Language_based_assessment_data_8$harmonywords
#wordembeddings = wordembeddings_hil$harmonywords
#single_wordembeddings = wordembeddings_hil$singlewords_we
#
#x = Language_based_assessment_data_8$hilstotal
#y = Language_based_assessment_data_8$swlstotal
#split = "mean"
#split = "quartile"
#Npermutations = 10
#n_per_split = 1
#
#pca = NULL
#aggregation = "mean"
#split = "quartile"
#split = "no"
#word_weight_power = 1
#min_freq_words_test = 0
#Npermutations = 1000
#n_per_split = 500
#seed = 1003
#
#i_dim=1
#

#' Compute Supervised Dimension Projection and related variables for plotting words.
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
#' wordembeddings <- wordembeddings4
#' raw_data <- Language_based_assessment_data_8
#' # Pre-processing data for plotting
#' df_for_plotting <- textProjection(
#'   words = raw_data$harmonywords,
#'   wordembeddings = wordembeddings$harmonywords,
#'   single_wordembeddings = wordembeddings$singlewords_we,
#'   x = raw_data$hilstotal,
#'   split = "mean",
#'   Npermutations = 10,
#'   n_per_split = 1
#' )
#' df_for_plotting
#' #' @seealso see \code{\link{textProjectionPlot}}
#' @importFrom tibble as_tibble
#' @importFrom recipes recipe step_center step_scale step_naomit all_numeric prep bake
#' @importFrom tidyr uncount
#' @importFrom dplyr full_join rename starts_with n
#' @importFrom stats median sd setNames complete.cases
#' @importFrom purrr as_vector
#' @export
textProjection <- function(words,
                           wordembeddings, # better to have these in and aggregate according to them as it becomes context (BERT) aggregated.
                           single_wordembeddings = single_wordembeddings_df,
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
  textProjection_descriptions <- paste("words =", substitute(words),
    "wordembeddings =", comment(wordembeddings),
    "single_wordembeddings =", comment(single_wordembeddings),
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

    if (pca < 1) {
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

  # Creating a list for the x and y dimensions; and one to save aggregated word embeddings and dot product null distributions for both x and y
  # so that these can be saved and used for when manually adding words to the plot in the next step.
  word_data_list <- list()
  aggregated_embeddings_dot_null_distribution<- list()

  # For-loop for x and y input/dimensions; i.e., y if the plot has two dimensions (i_dim=1 i_dim=2) remove(i_dim)
  for (i_dim in seq_len(ncol(x))) {

    # Get the word embeddings and scale/category for the plot dimension (i.e., x or y from above)
    x0 <- x[i_dim]
    x1 <- cbind(words, x0)
    colnames(x1) <- c("words", "value")
    x2 <- tibble::as_tibble(cbind(x1, wordembeddings))



    ############
    ######         1 Create COMPARISON/Projection embedding: all Group 1 & Group 2 word embeddings.
    ############

    ### Sum all word embeddings in one column
    if(split == "mean" | split == "quartile"){


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
      ###
      ##        Get word embeddings
      ###
      # Group 1: getting unique words and their frequency
      words_group1b_freq <- unique_freq_words(group1$words)
      words_group1b_freq <- words_group1b_freq[words_group1b_freq$n >= min_freq_words_test, ]
      words_group1b_freq$n_g1_g2 <- words_group1b_freq$n * -1

      # Get word embeddings for each word (applysemrep function is created in 1_1_textEmbedStatic).
      words_group1_single_wordembedding <- lapply(words_group1b_freq$words, applysemrep, single_wordembeddings)
      words_group1_single_wordembedding_b <- dplyr::bind_rows(words_group1_single_wordembedding)

      # Group 2
      words_group2b_freq <- unique_freq_words(group2$words)
      words_group2b_freq <- words_group2b_freq[words_group2b_freq$n >= min_freq_words_test, ]
      words_group2b_freq$n_g1_g2 <- words_group2b_freq$n * 1
      words_group2_single_wordembedding <- lapply(words_group2b_freq$words, applysemrep, single_wordembeddings)
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




      ## Get dataframe with ALL embeddings to randomly draw from (without log transformed, and quartiles) for Comparison distribution
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



      # Interval: No split. Weighting embeddings according to interval scale.
      } else if (split == "no"){

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
          tidyr::separate_rows(words, sep = ' ')

        # Get word embeddings for each word (applysemrep function is created in 1_1_textEmbedd).
        words_single_wordembedding   <- lapply(words_values_sep$words, applysemrep, single_wordembeddings)
        words_single_wordembedding_b <- dplyr::bind_rows(words_single_wordembedding)
        words_single_wordembedding_c <- dplyr::bind_cols(words_values_sep, words_single_wordembedding_b)
        #words_single_wordembedding_c <- tibble::as_tibble(words_single_wordembedding_c)

        # weight the word embeddings with the value weight
        weights <- words_single_wordembedding_c$value^word_weight_power
        words_single_wordembedding_d <- words_single_wordembedding_c %>%
          select(-c(words, value))

        words_single_wordembedding_d_scaled <- scale(words_single_wordembedding_d)

        words_group2_agg_single_wordembedding_d <- tibble::as_tibble((words_single_wordembedding_d_scaled * weights)/mean(weights))

        # reversed weights
        weight_rev <- (max(words_single_wordembedding_c$value)+1 - words_single_wordembedding_c$value)^word_weight_power
        words_group1_agg_single_wordembedding_d <- tibble::as_tibble((words_single_wordembedding_d_scaled * weight_rev)/mean(weight_rev))

        ## Get dataframe with ALL embeddings to randomly draw from (without log transformed, and quartiles) for Comparison distribution
        # Shuffle weights/values
        weights_shuffled  <- sample(words_single_wordembedding_c$value, replace=FALSE)
        words_single_wordembedding_d_weights_shuffled <- tibble::as_tibble((words_single_wordembedding_d_scaled * weights_shuffled)/mean(weights_shuffled))

        words_group1_2_agg_single_wordembedding_e1 <- words_single_wordembedding_d_weights_shuffled

    }

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

    if (split == "no"){
      # Applying the z-score parameters to all the unique word's embeddings
      scale_center_weights <- tibble::as_tibble_row(attr(words_single_wordembedding_d_scaled,"scaled:center")) %>%
        slice(rep(1:dplyr::n(), each=nrow(all_unique_words_we_b)))

      scale_scale_weights <- tibble::as_tibble_row(attr(words_single_wordembedding_d_scaled,"scaled:scale"))%>%
        slice(rep(1:dplyr::n(), each=nrow(all_unique_words_we_b)))

      all_unique_words_we_b <- tibble::as_tibble((all_unique_words_we_b - scale_center_weights)/scale_scale_weights)
    }

    # Position the embedding; i.e., taking the word embedding subtracted with aggregated word embedding
    embedding_to_anchour_with <- tibble::as_tibble_row((Aggregated_word_embedding_group2 + Aggregated_word_embedding_group1)/2)

    embedding_to_anchour_with <- embedding_to_anchour_with %>%
      dplyr::slice(rep(1:dplyr::n(), each = nrow(all_unique_words_we_b)))

    words_positioned_embeddings <- all_unique_words_we_b - embedding_to_anchour_with


    # Project the embeddings using dot product.
    projected_embedding_nrow <- tibble::as_tibble_row(projected_embedding) %>%
      dplyr::slice(rep(1:dplyr::n(), each = nrow(all_unique_words_we_b)))

    dot_products_observed <- rowSums(words_positioned_embeddings * projected_embedding_nrow)

    all_unique_words_freq$dot <- dot_products_observed


    # Computing the dot product projection for the aggregated and projected embeddings
    all_aggregated <- dplyr::bind_rows(Aggregated_word_embedding_group1, Aggregated_word_embedding_group2, projected_embedding)

    projected_embedding_a <- tibble::as_tibble_row(projected_embedding) %>%
      dplyr::slice(rep(1:dplyr::n(), each = nrow(all_aggregated)))

    dot_products_all_aggregated <- rowSums(all_aggregated * projected_embedding_a)

    # dot_products_all_aggregated <- rowSums(all_aggregated * t(replicate(nrow(all_aggregated), projected_embedding)))

    DP_aggregate <- tibble::as_tibble_col(c("Group1*", "Group2*", "projected_embedding"), column_name = "words")
    DP_aggregate$n <- c(0, 0, 0)
    DP_aggregate$dot <- dot_products_all_aggregated
    dot_products_observed <- c(as.vector(dot_products_all_aggregated), dot_products_observed)
    # Add DP_aggregate to the words data
    all_unique_words_freq <- rbind(DP_aggregate, all_unique_words_freq)


    ############
    ######         Comparison distributions for Project embedding
    #############


    # Splitting up the permutations in different loops to avoid memory issues
    forloops <- ceiling(Npermutations / n_per_split)
    dot_null_distribution <- list()

    #  i = 1
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

      Aggregated_word_embedding_group1_long <- tibble::as_tibble_row(Aggregated_word_embedding_group1) %>%
        dplyr::slice(rep(1:dplyr::n(), each = nrow(random_group2_embedding)))
      Aggregated_word_embedding_group2_long <- tibble::as_tibble_row(Aggregated_word_embedding_group2) %>%
        dplyr::slice(rep(1:dplyr::n(), each = nrow(random_group2_embedding)))

      words_positioned_embeddings_random <- random_group2_embedding - (Aggregated_word_embedding_group2_long + Aggregated_word_embedding_group1) / 2

      #words_positioned_embeddings_random_new[[1]]
      #words_positioned_embeddings_random_old[[1]]
      #words_positioned_embeddings_random_old <- random_group2_embedding - ((t(replicate(nrow(random_group2_embedding), Aggregated_word_embedding_group2)) +
      #  t(replicate(nrow(random_group2_embedding), Aggregated_word_embedding_group1))) / 2)



  #    embedding_to_anchour_with <- (Aggregated_word_embedding_group2 + Aggregated_word_embedding_group1)/2
  #    words_positioned_embeddings <- all_unique_words_we_b - t(replicate(nrow(all_unique_words_we_b), as.vector(embedding_to_anchour_with)))

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
                                    dot_null_distribution$value, alternative = "two_sided"
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


    to_be_saved_below <- list(tibble::as_tibble_row(Aggregated_word_embedding_group1),
                              tibble::as_tibble_row(Aggregated_word_embedding_group2),
            dot_null_distribution)

    names(to_be_saved_below) <- c("Aggregated_word_embedding_group1", "Aggregated_word_embedding_group2",
                                  "dot_null_distribution")

    # Adding the scale parameters for the word embeddings so that words can be manually added in the textProjectionPlot.
    if (split == "no") {
      to_be_saved_below$scale_centre <-  tibble::as_tibble_row(attr(words_single_wordembedding_d_scaled,"scaled:center"))
      to_be_saved_below$scale_scale <-  tibble::as_tibble_row(attr(words_single_wordembedding_d_scaled,"scaled:scale"))
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
    names(aggregated_embeddings_dot_null_distribution[[1]]) <- c("Aggregated_word_embedding_group1.x",
                                                                 "Aggregated_word_embedding_group2.x",
                                                                 "dot_null_distribution.x")

    if (split == "no" ){
      names(aggregated_embeddings_dot_null_distribution[[1]]) <- c("Aggregated_word_embedding_group1.x",
                                                                   "Aggregated_word_embedding_group2.x",
                                                                   "dot_null_distribution.x",
                                                                   "scale_centre.x",
                                                                   "scale_scale.x")

    }

  } else {
    word_data_tibble <- dplyr::full_join(word_data_list[[1]], word_data_list[[2]], by = "words")
    word_data_tibble$n <- word_data_tibble$n.x
    word_data_tibble <- dplyr::select(word_data_tibble, -c(n.x, n.y))
    word_data_tibble$n.percent <- word_data_tibble$n / sum(word_data_tibble$n)
    word_data_tibble$N_participant_responses <- c(rep(N_participant_responses, nrow(word_data_tibble)))

    # Naming
    names(aggregated_embeddings_dot_null_distribution[[1]]) <- c("Aggregated_word_embedding_group1.x",
                                                                 "Aggregated_word_embedding_group2.x",
                                                                 "dot_null_distribution.x")
    # Naming
    names(aggregated_embeddings_dot_null_distribution[[2]]) <- c("Aggregated_word_embedding_group1.y",
                                                                 "Aggregated_word_embedding_group2.y",
                                                                 "dot_null_distribution.y")

    if (split == "no") {
      names(aggregated_embeddings_dot_null_distribution[[1]]) <- c("Aggregated_word_embedding_group1.x",
                                                                   "Aggregated_word_embedding_group2.x",
                                                                   "dot_null_distribution.x",
                                                                   "scale_centre.x",
                                                                   "scale_scale.x")

      names(aggregated_embeddings_dot_null_distribution[[2]]) <- c("Aggregated_word_embedding_group1.y",
                                                                 "Aggregated_word_embedding_group2.y",
                                                                 "dot_null_distribution.y",
                                                                 "scale_centre.y",
                                                                 "scale_scale.y")
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


#####saveRDS(wordembeddings_hil, "/Users/oscarkjell/Desktop/1 Projects/0 Research/0 text r-package/wordembeddings_hil.rds")
#wordembeddings_hil <- read_rds("/Users/oscarkjell/Desktop/1 Projects/0 Research/0 text r-package/wordembeddings_hil.rds")
#wordembeddings <- wordembeddings4
#raw_data <- Language_based_assessment_data_8
###### Pre-processing data for plotting
#df_for_plotting <- text::textProjection(
#  words = Language_based_assessment_data_8$harmonywords,
#  wordembeddings = wordembeddings_hil$harmonywords,
#  single_wordembeddings = wordembeddings_hil$singlewords_we,
#  x = raw_data$hilstotal,
#  #y = raw_data$swlstotal,
#  split = "quartile",
#  word_weight_power = 1,
#  min_freq_words_test = 0,
#  Npermutations = 100000,
#  n_per_split = 50000,
#  seed = 1003
#)

#' Creates the plot object (except for the legend).
#' @return A plot object.
#' @noRd
textPlotting <- function(word_data_all = word_data_all,
                         word_data_all_yadjusted = word_data_all_yadjusted,
                         only_x_dimension = only_x_dimension,
                         x_axes_1 = x_axes_1,
                         y_axes_1 = y_axes_1,
                         group_embeddings1 = group_embeddings1,
                         group_embeddings2 = group_embeddings2,
                         projection_embedding = projection_embedding,
                         label = words,
                         points_without_words_size = points_without_words_size,
                         points_without_words_alpha = points_without_words_alpha,
                         colour_categories = colour_categories,
                         arrow_transparency = arrow_transparency,
                         scale_x_axes_lim = scale_x_axes_lim,
                         scale_y_axes_lim = scale_y_axes_lim,
                         position_jitter_hight = position_jitter_hight,
                         position_jitter_width = position_jitter_width,
                         word_font = word_font,
                         point_size = point_size,
                         aggregated_embeddings_data = aggregated_embeddings_data,
                         aggregated_point_size = aggregated_point_size,
                         aggregated_shape = aggregated_shape,
                         aggregated_color_G1 = aggregated_color_G1,
                         aggregated_color_G2 = aggregated_color_G2,
                         projection_color = projection_color,
                         word_size_range = word_size_range,
                         # titles
                         title_top = title_top,
                         titles_color = titles_color,
                         x_axes_label = x_axes_label,
                         y_axes_label = y_axes_label,
                         y_axes_values = y_axes_values){

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

    {if(group_embeddings1 == TRUE)
      # Aggregated point help(geom_point)
      ggplot2::geom_point(
        data = aggregated_embeddings_data[1,],
        size = aggregated_point_size,
        shape = aggregated_shape,
        ggplot2::aes(color = aggregated_color_G1)
      ) } +

    # Aggregated point 2
    {if(group_embeddings2 == TRUE)
      ggplot2::geom_point(
        data = aggregated_embeddings_data[2,],
        size = aggregated_point_size,
        shape = aggregated_shape,
        ggplot2::aes(color = aggregated_color_G2)
      ) } +

    # Projection embedding
    {if(projection_embedding == TRUE)
      ggplot2::geom_point(
        data = aggregated_embeddings_data[3,],
        size = aggregated_point_size,
        shape = aggregated_shape,
        ggplot2::aes(color = projection_color)
      ) } +

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

  return(plot)

}



#' Creates the legend for the plot.
#' @return A legend object that can be combined with the plot object.
#' @noRd
textLegend <- function(bivariate_color_codes = bivariate_color_codes,
                       y_axes_1 = y_axes_1,
                       fill = fill,
                       legend_title = legend_title,
                       legend_title_size = legend_title_size,
                       legend_x_axes_label = legend_x_axes_label,
                       legend_y_axes_label = legend_y_axes_label,
                       word_data_all = word_data_all,
                       legend_number_size = legend_number_size,
                       # only_x_dimension = only_x_dimension,
                       titles_color = titles_color){

  bivariate_color_data <- tibble::tibble(
    "1 - 3" = "#XXXXXX", "2 - 3" = "#XXXXXX", "3 - 3" = "#XXXXXX",
    "1 - 2" = "#XXXXXX", "2 - 2" = "#XXXXXX", "3 - 2" = "#XXXXXX",
    "1 - 1" = "#XXXXXX", "2 - 1" = "#XXXXXX", "3 - 1" = "#XXXXXX"
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
    {
      if (y_axes_1 != "only_x_dimension") {
        ggplot2::annotate(
          geom = "text", x = 1, y = 3, label = sum(word_data_all$colour_categories == bivariate_color_codes[1], na.rm = T),
          color = titles_color, size = legend_number_size
        )
      }
    } +
    {
      if (y_axes_1 != "only_x_dimension") {
        ggplot2::annotate(
          geom = "text", x = 2, y = 3, label = sum(word_data_all$colour_categories == bivariate_color_codes[2], na.rm = T),
          color = titles_color, size = legend_number_size
        )
      }
    } +
    {
      if (y_axes_1 != "only_x_dimension") {
        ggplot2::annotate(
          geom = "text", x = 3, y = 3, label = sum(word_data_all$colour_categories == bivariate_color_codes[3], na.rm = T),
          color = titles_color, size = legend_number_size
        )
      }
    } +
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
    {
      if (y_axes_1 != "only_x_dimension") {
        ggplot2::annotate(
          geom = "text", x = 1, y = 1, label = sum(word_data_all$colour_categories == bivariate_color_codes[7], na.rm = T),
          color = titles_color, size = legend_number_size
        )
      }
    } +
    {
      if (y_axes_1 != "only_x_dimension") {
        ggplot2::annotate(
          geom = "text", x = 2, y = 1, label = sum(word_data_all$colour_categories == bivariate_color_codes[8], na.rm = T),
          color = titles_color, size = legend_number_size
        )
      }
    } +
    {
      if (y_axes_1 != "only_x_dimension") {
        ggplot2::annotate(
          geom = "text", x = 3, y = 1, label = sum(word_data_all$colour_categories == bivariate_color_codes[9], na.rm = T),
          color = titles_color, size = legend_number_size
        )
      }
    } +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, size = legend_title_size + 1),
      title = ggplot2::element_text(color = titles_color),
      axis.title.x = ggplot2::element_text(color = titles_color),
      axis.title = ggplot2::element_text(size = legend_title_size),
      axis.title.y = ggplot2::element_text(angle = 90, color = titles_color)
    ) +
    ggplot2::coord_fixed()
  legend

}

#' Computes the dot product projection for added data.
#' @return Word_data_all_yadjusted with added infomration for the added words.
#' @noRd
textProjectingOwnWords <- function(word_data = word_data,
                                   word_data_all = word_data_all,
                                   word_data_all_yadjusted = word_data_all_yadjusted,
                                   y_axes = y_axes,
                                   explore_words = explore_words,
                                   explore_words_color = explore_words_color,
                                   explore_words_point = explore_words_point,
                                   explore_words_aggregation = explore_words_aggregation,
                                   space = space,
                                   textProjectionPlot_comment = textProjectionPlot_comment,
                                   scaling = scaling){

  # For loop for different batches of added words; i_add_w=1 explore_words = "happy harmony love"
  forloops_add_w <- length(explore_words)
  added_words_information <- list()

  for (i_add_w in 1:forloops_add_w) {

    # If using a contextualized language model
    if(is.null(space) == TRUE){


      # Creating word embeddings for the words.
      model_text <- sub(".*model: ", '', textProjectionPlot_comment)
      model_name <- sub(" layer.*", '', model_text)
      layers_text <- sub(".*layers: ", '', textProjectionPlot_comment)
      layers_number <- sub(" . textEmbedLayerAggregation.*", '', layers_text)
      layers_number_split <- stringi::stri_split_boundaries(layers_number,  type = "word",
                                                            skip_word_none = TRUE,
                                                            skip_word_number = FALSE)

      explore_words_embeddings <- textEmbed(explore_words[i_add_w],
                                            model = model_name,
                                            layers = dput(as.numeric(layers_number_split[[1]])))
    }
    # If using a static/decontextualized language model
    if(!is.null(space)==TRUE){
      explore_words_embeddings <- textEmbedStatic(data.frame(explore_words[i_add_w]),
                                                  space = space,
                                                  aggregate = explore_words_aggregation)

    }

    words <- tibble::as_tibble_col(explore_words_point[i_add_w])
    colnames(words) <- "words"
    n_words <- tibble::as_tibble_col(1)
    colnames(n_words) <- "n"

    # Scaling embeddings before aggregation
    if (scaling == TRUE){

      singlewords_we_x <- dplyr::select(explore_words_embeddings$singlewords_we, dplyr::starts_with("Dim"))

      # Applying scaling parameters to all the unique word's embeddings
      scale_center_weights <- word_data$background[[1]]$scale_centre.x %>%
        dplyr::slice(rep(1:dplyr::n(), each=nrow(singlewords_we_x)))

      scale_scale_weights <- word_data$background[[1]]$scale_scale.x %>%
        dplyr::slice(rep(1:dplyr::n(), each=nrow(singlewords_we_x)))

      singlewords_we_x_scaled <- tibble::as_tibble((singlewords_we_x - scale_center_weights)/scale_scale_weights)

      singlewords_we_x_scaled_w_n <- bind_cols(explore_words_embeddings$singlewords_we[1:2], singlewords_we_x_scaled)

      # Aggregate the words
      Aggregated_embedding_added_words <-  tibble::as_tibble_row(textEmbeddingAggregation(singlewords_we_x_scaled, aggregation = explore_words_aggregation))

      #Aggregated_embedding_added_words <- as_tibble(t(Aggregated_embedding_added_words))
      Mean1 <- dplyr::bind_cols(words, n_words, Aggregated_embedding_added_words)
      manual_words_mean1 <- bind_rows(singlewords_we_x_scaled_w_n, Mean1)

    } else {
      # Aggregate the words
      Aggregated_embedding_added_words <-  tibble::as_tibble_row(textEmbeddingAggregation(dplyr::select(explore_words_embeddings$singlewords_we, dplyr::starts_with("Dim")), aggregation = explore_words_aggregation))
      #Aggregated_embedding_added_words <- as_tibble(t(Aggregated_embedding_added_words))
      Mean1 <- dplyr::bind_cols(words, n_words, Aggregated_embedding_added_words)
      manual_words_mean1 <- bind_rows(explore_words_embeddings$singlewords_we, Mean1)
    }

    #### Project embedding on the x axes ######
    projected_embedding.x <- as.vector(word_data$background[[1]]$Aggregated_word_embedding_group2.x - word_data$background[[1]]$Aggregated_word_embedding_group1.x)

    # Position words in relation to Aggregated word embedding
    # Position the embedding; i.e., taking the word embedding subtracted with aggregated word embedding
    embedding_to_anchour_with.x <- tibble::as_tibble((word_data$background[[1]]$Aggregated_word_embedding_group2.x + word_data$background[[1]]$Aggregated_word_embedding_group1.x)/2)
    manual_words_mean1_1.x <- dplyr::select(manual_words_mean1, dplyr::starts_with("Dim"))

    embedding_to_anchour_with.x_df <- embedding_to_anchour_with.x %>%
      dplyr::slice(rep(1:dplyr::n(), each=nrow(manual_words_mean1_1.x)))

    words_positioned_embeddings <- tibble::as_tibble(manual_words_mean1_1.x - embedding_to_anchour_with.x_df)

    projected_embedding.x_df <- tibble::as_tibble(projected_embedding.x) %>%
      slice(rep(1:dplyr::n(), each = nrow(manual_words_mean1)))

    # Project the embeddings using dot product.
    #word_data$word_data[word_data$word_data$words == "love",]
    dot_products_observed.x <- rowSums(words_positioned_embeddings * projected_embedding.x_df)

    ### Compare observed dot-product with null
    p_values_dot_prod.x <- purrr::map(as.list(purrr::as_vector(dot_products_observed.x)), p_value_comparing_with_Null,
                                      word_data$background[[1]]$dot_null_distribution[[1]], alternative = "two_sided")

    p_values_dot_prod.x <- unlist(p_values_dot_prod.x)

    #### Project embedding on the Y axes ####

    if(y_axes == TRUE){
      projected_embedding.y <- as.vector(word_data$background[[2]]$Aggregated_word_embedding_group2.y - word_data$background[[2]]$Aggregated_word_embedding_group1.y)
      # Position words in relation to Aggregated word embedding
      # Position the embedding; i.e., taking the word embedding subtracted with aggregated word embedding
      embedding_to_anchour_with.y <- tibble::as_tibble((word_data$background[[2]]$Aggregated_word_embedding_group2.y + word_data$background[[2]]$Aggregated_word_embedding_group1.y)/2)
      manual_words_mean1_1.y <- dplyr::select(manual_words_mean1, dplyr::starts_with("Dim"))

      embedding_to_anchour_with.y_df <- embedding_to_anchour_with.y %>%
        dplyr::slice(rep(1:dplyr::n(), each=nrow(manual_words_mean1_1.y)))

      words_positioned_embeddings <- tibble::as_tibble(manual_words_mean1_1.y - embedding_to_anchour_with.y_df)

      projected_embedding.y_df <- tibble::as_tibble(projected_embedding.y) %>%
        slice(rep(1:dplyr::n(), each = nrow(manual_words_mean1)))

      # Project the embeddings using dot product.
      #word_data$word_data[word_data$word_data$words == "love",]
      dot_products_observed.y <- rowSums(words_positioned_embeddings * projected_embedding.y_df)

      ### Compare observed dot-product with null
      p_values_dot_prod.y <- purrr::map(as.list(purrr::as_vector(dot_products_observed.y)), p_value_comparing_with_Null,
                                        word_data$background[[2]]$dot_null_distribution[[1]], alternative = "two_sided")

      p_values_dot_prod.y <- unlist(p_values_dot_prod.y)

    }

    # Sort out dataframe
    explore_words_results <- manual_words_mean1[1:2]
    explore_words_results$dot.x <- dot_products_observed.x
    explore_words_results$p_values_dot.x <- p_values_dot_prod.x
    explore_words_results$adjusted_p_values.x <- p_values_dot_prod.x

    if(y_axes == TRUE){
      explore_words_results$dot.y <- dot_products_observed.y
      explore_words_results$p_values_dot.y <- p_values_dot_prod.y
      explore_words_results$adjusted_p_values.y <- p_values_dot_prod.y
    }

    explore_words_results$colour_categories <- explore_words_color[i_add_w] #"#e07f6a"   # "#e07f6a", "#EAEAEA", "#40DD52
    # TODO; should not have to print extreme?
    explore_words_results$extremes_all_x <- rep(NA, nrow(explore_words_results))#c(1, 1, 1)
    explore_words_results$n <- rep(mean(word_data_all$n), nrow(explore_words_results)) #c(300, 300, 300)
    explore_words_results$n.percent <- rep(0.5, nrow(explore_words_results)) # c(0.5, 0.5, 0.5)
    #explore_words_results$n_g1.x <- c(-1, -1, -1)
    explore_words_results$n_g2.x <- rep(5, nrow(explore_words_results)) #c(5, 5, 5)
    explore_words_results$N_participant_responses <- rep(max(word_data_all$N_participant_responses), nrow(explore_words_results)) #c(40, 40, 40)

    added_words_information[[i_add_w]] <- explore_words_results
  }
  added_words_information_unlist <- dplyr::bind_rows(added_words_information)
  word_data_all_yadjusted <- dplyr::bind_rows(word_data_all_yadjusted, added_words_information_unlist)

  return(word_data_all_yadjusted)
}




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
#' @param plot_n_words_middle Number of words plotted that are in the middle in Supervised Dimension Projection score
#' (i.e., even if not significant;  per dimensions, where duplicates are removed).
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
#' @param bivariate_color_codes The different colors of the words. Note that, at the moment, two squares should not have the
#' exact same colour-code because the numbers within the squares of the legend will then be aggregated (and show the same, incorrect  value).
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
#' @param aggregated_point_size Size of the points representing the group_embeddings1, group_embeddings2 and projection_embedding
#' @param aggregated_shape Shape type of the points representing the group_embeddings1, group_embeddings2 and projection_embeddingd
#' @param aggregated_color_G1 Color
#' @param aggregated_color_G2 Color
#' @param projection_color Color
#' @param seed Set different seed.
#' @param explore_words Explore where specific words are positioned in the embedding space. For example, c("happy content", "sad down").
#' @param explore_words_color Specify the color(s) of the words being explored. For example c("#ad42f5", "green")
#' @param explore_words_point Specify the names of the point for the aggregated word embeddings of all the explored words.
#' @param explore_words_aggregation Specify how to aggregate the word embeddings of the explored words.
#' @param remove_words manually remove words from the plot (which is done just before the words are plotted so that the remove_words are part of previous counts/analyses).
#' @param space Provide a semantic space if using static embeddings and wanting to explore words.
#' @param n_contrast_group_color Set color to words that have higher frequency (N) on the other opposite side of its dot product projection (default = NULL).
#' @param n_contrast_group_remove Remove words that have higher frequency (N) on the other opposite side of its dot product projection (default = FALSE).
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
textPlot <- function(word_data,
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


 #1 word_data_word_predictions = should be the same
 #2 make comment that say which type of data comming in.
 # here look in the comment(word_data)
  plot_type <- comment(word_data)
  # Making column names generic across different input
  if(!is.null(word_data$word_data_word_predictions)) {

    if(projection == TRUE){
      rename("dot.x" == "x_value")
    }
    if(centrality == TRUE){
      rename("consine.x" == "x_value")
    }
    if(superviced == TRUE){
      rename(superviced == "x_value")
    }

    #HERE FOLLOWS THE PLOTTING CODE


  }


  ##### Comment to be saved ####
  textProjectionPlot_comment <- paste(
    "INFORMATION ABOUT THE PROJECTION",
    comment(word_data),
    "INFORMATION ABOUT THE PLOT",
    "word_data =", substitute(word_data),
    "k_n_words_to_test =", k_n_words_to_test,
    "min_freq_words_test =", min_freq_words_test,
    "min_freq_words_plot =", min_freq_words_plot,
    "plot_n_words_square =", plot_n_words_square,
    "plot_n_words_p =", plot_n_words_p,
    "plot_n_word_extreme =", plot_n_word_extreme,
    "plot_n_word_frequency =", plot_n_word_frequency,
    "plot_n_words_middle =", plot_n_words_middle,
    "y_axes =", y_axes,
    "p_alpha =", p_alpha,
    "p_adjust_method =", p_adjust_method,
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

  #### Sorting out axes ####
  x_axes_1 <- "dot.x"
  p_values_x <- "p_values_dot.x"

  if (y_axes == TRUE) {
    y_axes_1 <- "dot.y"
    p_values_y <- "p_values_dot.y"
    y_axes_values_hide <- FALSE
  } else if (y_axes == FALSE) {
    y_axes_1 <- NULL
    p_values_y <- NULL
    y_axes_values_hide <- TRUE
  }

  #### Removing words MANUALY #######

  if (!is.null(remove_words)){
    word_data$word_data <- word_data$word_data %>% dplyr::filter(!words %in% remove_words)
  }

  #### Selecting words to plot ####
  # Computing adjusted p-values with those words selected by min_freq_words_test
  word_data_padjusted <- word_data$word_data[word_data$word_data$n >= min_freq_words_test, ]

  # Selected Aggregated points
  aggregated_embeddings_data <- word_data$word_data[word_data$word_data$n == 0,]

  # View(word_data_padjusted) Computing adjusted p-values with those words selected by: k = sqrt(100*N)
  if (k_n_words_to_test == TRUE) {
    words_k <- sqrt(100 * word_data$word_data$N_participant_responses[1])
    word_data_padjusted <- word_data_padjusted %>%
      dplyr::arrange(-n) %>%
      dplyr::slice(0:words_k)
  }
  # word_data_padjusted$p_values_dot.x
  word_data_padjusted$adjusted_p_values.x <- stats::p.adjust(purrr::as_vector(word_data_padjusted[, "p_values_dot.x"]), method = p_adjust_method)
  word_data1 <- dplyr::left_join(word_data$word_data, word_data_padjusted[, c("words", "adjusted_p_values.x")], by = "words")
  # word_data$adjusted_p_values.x

  if (is.null(y_axes_1) == FALSE) {
    # Computing adjusted p-values
    word_data1_padjusted_y <- word_data1[word_data1$n >= min_freq_words_test, ]
    word_data1_padjusted_y$adjusted_p_values.y <- stats::p.adjust(purrr::as_vector(word_data1_padjusted_y[, "p_values_dot.y"]), method = p_adjust_method)
    word_data1 <- left_join(word_data1, word_data1_padjusted_y[, c("words", "adjusted_p_values.y")], by = "words")
  }

  # Select only min_freq_words_plot to plot (i.e., after correction of multiple comparison for sig. test)
  word_data1 <- word_data1[word_data1$n >= min_freq_words_plot, ]

  #  Select only words based on square-position; and then top frequency in each "square" (see legend) plot_n_words_square
  if (is.null(y_axes_1) == TRUE) {
    word_data1 <- word_data1 %>%
      dplyr::mutate(square_categories = dplyr::case_when(
        dot.x < 0 & adjusted_p_values.x < p_alpha ~ 1,
        dot.x < 0 & adjusted_p_values.x > p_alpha ~ 2,
        dot.x > 0 & adjusted_p_values.x < p_alpha ~ 3
      ))

    data_p_sq1 <- word_data1[word_data1$square_categories == 1, ] %>%
      dplyr::arrange(-n) %>%
      dplyr::slice(0:plot_n_words_square)

    data_p_sq3 <- word_data1[word_data1$square_categories == 3, ] %>%
      dplyr::arrange(-n) %>%
      dplyr::slice(0:plot_n_words_square)

    data_p_sq_all <- rbind(data_p_sq1, data_p_sq3) # data_p_sq2,
  }

  if (is.null(y_axes_1) == FALSE) {
    # Categorize words to apply specific color plot_n_words_square=1
    word_data1 <- word_data1 %>%
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

    data_p_sq1 <- word_data1[word_data1$square_categories == 1, ] %>%
      dplyr::arrange(-n) %>%
      dplyr::slice(0:plot_n_words_square)
    data_p_sq2 <- word_data1[word_data1$square_categories == 2, ] %>%
      dplyr::arrange(-n) %>%
      dplyr::slice(0:plot_n_words_square)
    data_p_sq3 <- word_data1[word_data1$square_categories == 3, ] %>%
      dplyr::arrange(-n) %>%
      dplyr::slice(0:plot_n_words_square)
    data_p_sq4 <- word_data1[word_data1$square_categories == 4, ] %>%
      dplyr::arrange(-n) %>%
      dplyr::slice(0:plot_n_words_square)
    #  data_p_sq5 <- word_data1[word_data1$square_categories==5, ] %>%
    #    dplyr::arrange(-n) %>%
    #    dplyr::slice(0:plot_n_words_square)
    data_p_sq6 <- word_data1[word_data1$square_categories == 6, ] %>%
      dplyr::arrange(-n) %>%
      dplyr::slice(0:plot_n_words_square)
    data_p_sq7 <- word_data1[word_data1$square_categories == 7, ] %>%
      dplyr::arrange(-n) %>%
      dplyr::slice(0:plot_n_words_square)
    data_p_sq8 <- word_data1[word_data1$square_categories == 8, ] %>%
      dplyr::arrange(-n) %>%
      dplyr::slice(0:plot_n_words_square)
    data_p_sq9 <- word_data1[word_data1$square_categories == 9, ] %>%
      dplyr::arrange(-n) %>%
      dplyr::slice(0:plot_n_words_square)

    data_p_sq_all <- rbind(
      data_p_sq1, data_p_sq2, data_p_sq3,
      data_p_sq4, data_p_sq6, # data_p_sq5,
      data_p_sq7, data_p_sq8, data_p_sq9
    )
  }


  # Select only words below alpha; and then top dot.x
  data_p_x_neg <- word_data1 %>%
    dplyr::filter(adjusted_p_values.x < p_alpha) %>%
    dplyr::arrange(dot.x) %>%
    dplyr::slice(0:plot_n_words_p)

  data_p_x_pos <- word_data1 %>%
    dplyr::filter(adjusted_p_values.x < p_alpha) %>%
    dplyr::arrange(-dot.x) %>%
    dplyr::slice(0:plot_n_words_p)

  # Select plot_n_word_extreme and Select plot_n_word_frequency
  word_data1_extrem_max_x <- word_data1 %>%
    dplyr::arrange(-dot.x) %>%
    dplyr::slice(0:plot_n_word_extreme)

  word_data1_extrem_min_x <- word_data1 %>%
    dplyr::arrange(dot.x) %>%
    dplyr::slice(0:plot_n_word_extreme)

  word_data1_frequency_x <- word_data1 %>%
    dplyr::arrange(-n) %>%
    dplyr::slice(0:plot_n_word_frequency)

  # Select the middle range, order according to frequency and then select the plot_n_words_middle = 5
  mean_m_sd_x <- mean(word_data1$dot.x, na.rm = TRUE) - (sd(word_data1$dot.x, na.rm = TRUE) / 10)
  mean_p_sd_x <- mean(word_data1$dot.x, na.rm = TRUE) + (sd(word_data1$dot.x, na.rm = TRUE) / 10)
  word_data1_middle_x <- word_data1 %>%
    dplyr::filter(dplyr::between(word_data1$dot.x, mean_m_sd_x, mean_p_sd_x)) %>%
    dplyr::arrange(-n) %>%
    dplyr::slice(0:plot_n_words_middle)

  word_data1_x <- word_data1 %>%
    dplyr::left_join(data_p_sq_all %>%
                       dplyr::transmute(words, check_p_square = 1), by = "words") %>%
    dplyr::left_join(data_p_x_neg %>%
                       dplyr::transmute(words, check_p_x_neg = 1), by = "words") %>%
    dplyr::left_join(data_p_x_pos %>%
                       dplyr::transmute(words, check_p_x_pos = 1), by = "words") %>%
    dplyr::left_join(word_data1_extrem_max_x %>%
                       dplyr::transmute(words, check_extreme_max_x = 1), by = "words") %>%
    dplyr::left_join(word_data1_extrem_min_x %>%
                       dplyr::transmute(words, check_extreme_min_x = 1), by = "words") %>%
    dplyr::left_join(word_data1_frequency_x %>%
                       dplyr::transmute(words, check_extreme_frequency_x = 1), by = "words") %>%
    dplyr::left_join(word_data1_middle_x %>%
                       dplyr::transmute(words, check_middle_x = 1), by = "words") %>%
    dplyr::mutate(extremes_all_x = rowSums(cbind(
      check_p_square, check_p_x_neg, check_p_x_pos, check_extreme_max_x, check_extreme_min_x,
      check_extreme_frequency_x, check_middle_x
    ), na.rm = T))


  ###### Sort words for y-axes.
  if (is.null(y_axes_1) == FALSE) {
    # Computing adjusted p-values
    # Select only words below alpha; and then top dot.x
    data_p_y_neg <- word_data1 %>%
      dplyr::filter(adjusted_p_values.y < p_alpha) %>%
      dplyr::arrange(dot.y) %>%
      dplyr::slice(0:plot_n_words_p)

    data_p_y_pos <- word_data1 %>%
      dplyr::filter(adjusted_p_values.y < p_alpha) %>%
      dplyr::arrange(-dot.y) %>%
      dplyr::slice(0:plot_n_words_p)

    # Select plot_n_word_extreme and Select plot_n_word_frequency
    word_data1_extrem_max_y <- word_data1 %>%
      dplyr::arrange(-dot.y) %>%
      dplyr::slice(0:plot_n_word_extreme)

    word_data1_extrem_min_y <- word_data1 %>%
      dplyr::arrange(dot.y) %>%
      dplyr::slice(0:plot_n_word_extreme)

    word_data1_frequency_y <- word_data1 %>%
      dplyr::arrange(-n) %>%
      dplyr::slice(0:plot_n_word_frequency)

    # Select the middle range, order according to frequency and then select the plot_n_words_middle =5
    mean_m_sd_y <- mean(word_data1$dot.y, na.rm = TRUE) - (sd(word_data1$dot.y, na.rm = TRUE) / 10)
    mean_p_sd_y <- mean(word_data1$dot.y, na.rm = TRUE) + (sd(word_data1$dot.y, na.rm = TRUE) / 10)
    word_data1_middle_y <- word_data1 %>%
      dplyr::filter(dplyr::between(word_data1$dot.y, mean_m_sd_y, mean_p_sd_y)) %>%
      dplyr::arrange(-n) %>%
      dplyr::slice(0:plot_n_words_middle) # TODO selecting on frequency again. perhaps point to have exact middle?

    word_data_all <- word_data1_x %>%
      dplyr::left_join(data_p_y_pos %>%
                         dplyr::transmute(words, check_p_y_pos = 1), by = "words") %>%
      dplyr::left_join(data_p_y_neg %>%
                         dplyr::transmute(words, check_p_y_neg = 1), by = "words") %>%
      dplyr::left_join(word_data1_extrem_max_y %>%
                         dplyr::transmute(words, check_extreme_max_y = 1), by = "words") %>%
      dplyr::left_join(word_data1_extrem_min_y %>%
                         dplyr::transmute(words, check_extreme_min_y = 1), by = "words") %>%
      dplyr::left_join(word_data1_frequency_y %>%
                         dplyr::transmute(words, check_extreme_frequency_y = 1), by = "words") %>%
      dplyr::left_join(word_data1_middle_y %>%
                         dplyr::transmute(words, check_middle_y = 1), by = "words") %>%
      dplyr::mutate(extremes_all_y = rowSums(cbind(
        check_p_y_neg, check_p_y_pos, check_extreme_max_y, check_extreme_min_y,
        check_extreme_frequency_y, check_middle_y
      ), na.rm = T)) %>%
      dplyr::mutate(extremes_all = rowSums(cbind(extremes_all_x, extremes_all_y), na.rm = T))


    # Categorize words to apply specific color
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
    word_data_all <- word_data1_x %>%
      dplyr::mutate(colour_categories = dplyr::case_when(
        dot.x < 0 & adjusted_p_values.x < p_alpha ~ bivariate_color_codes[4],
        # dot.x < 0 & adjusted_p_values.x > p_alpha ~ bivariate_color_codes[5],
        # Some adjusted_p_values.x has NA becasue they where not tested as multiple input (this is because min_frequency selects out before)
        adjusted_p_values.x > p_alpha | is.na(adjusted_p_values.x) ~ bivariate_color_codes[5],
        dot.x > 0 & adjusted_p_values.x < p_alpha ~ bivariate_color_codes[6]
      ))
  }
  #View(word_data_all)

  #### Colorize words that are more frequent on the opposite side of the dot product projection ####
  if(is.character(n_contrast_group_color) == TRUE) {

    # Select words with MORE words in G1 and POSITIVE dot product (i.e., remove words that are more represented in the opposite group of its dot product projection)
    word_data_all$colour_categories[(abs(word_data_all$n_g1.x) > abs(word_data_all$n_g2.x) & word_data_all$dot.x>0)] <- n_contrast_group_color

    # Select words with MORE words in G2 and POSITIVE dot product (i.e., remove words that are more represented in the opposite group of its dot product projection)
    word_data_all$colour_categories[(abs(word_data_all$n_g1.x) < abs(word_data_all$n_g2.x) & word_data_all$dot.x<0)] <- n_contrast_group_color

  }

  #### Remove words that are more frequent on the opposite side of the dot product projection ####
  if(n_contrast_group_remove == TRUE) {

    word_data_all1  <- word_data_all %>%
      # Select words with MORE words in G1 and NEGATIVE dot product (i.e., do not select words that are more represented in the opposite group of its dot product projection)
      filter((abs(n_g1.x) > abs(n_g2.x) &
                dot.x < 0))

    word_data_all2  <- word_data_all %>%
      # Select words with MORE words in G2 and POSITIVE dot product (i.e., do not select words that are more represented in the opposite group of its dot product projection)
      filter((abs(n_g1.x) < abs(n_g2.x) &
                dot.x > 0))

    word_data_all <- bind_rows(word_data_all1, word_data_all2)

  }

  #### Preparing for the plot function ####

  # This solution is because it is not possible to send "0" as a parameter
  only_x_dimension = NULL
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
    word_data_all_yadjusted <- word_data_all[word_data_all$extremes_all_x >= 1 | word_data_all$extremes_all_y >= 1, ]
  } else if (y_axes == FALSE) {
    word_data_all_yadjusted <- word_data_all[word_data_all$extremes_all_x >= 1, ]
  }


  ##### Adding/exploring words MANUALY ######

  if (!is.null(explore_words) == TRUE) {

    word_data_all_yadjusted1 <- textProjectingOwnWords(word_data = word_data,
                                                       word_data_all = word_data_all,
                                                       word_data_all_yadjusted = word_data_all_yadjusted,
                                                       y_axes = y_axes,
                                                       explore_words = explore_words,
                                                       explore_words_color = explore_words_color,
                                                       explore_words_point = explore_words_point,
                                                       explore_words_aggregation = explore_words_aggregation,
                                                       space = space,
                                                       textProjectionPlot_comment = textProjectionPlot_comment,
                                                       scaling = scaling)
    word_data_all_yadjusted <- word_data_all_yadjusted1
  }

  #### Plotting  ####
  plot <- textPlotting(word_data_all = word_data_all,
                       word_data_all_yadjusted = word_data_all_yadjusted,
                       only_x_dimension = only_x_dimension,
                       x_axes_1 = x_axes_1,
                       y_axes_1 = y_axes_1,
                       group_embeddings1 = group_embeddings1,
                       group_embeddings2 = group_embeddings2,
                       projection_embedding = projection_embedding,
                       label = words,
                       points_without_words_size = points_without_words_size,
                       points_without_words_alpha = points_without_words_alpha,
                       colour_categories = colour_categories,
                       arrow_transparency = arrow_transparency,
                       scale_x_axes_lim = scale_x_axes_lim,
                       scale_y_axes_lim = scale_y_axes_lim,
                       position_jitter_hight = position_jitter_hight,
                       position_jitter_width = position_jitter_width,
                       word_font = word_font,
                       point_size = point_size,
                       aggregated_embeddings_data = aggregated_embeddings_data,
                       aggregated_point_size = aggregated_point_size,
                       aggregated_shape = aggregated_shape,
                       aggregated_color_G1 = aggregated_color_G1,
                       aggregated_color_G2 = aggregated_color_G2,
                       projection_color = projection_color,
                       word_size_range = word_size_range,
                       # titles
                       title_top = title_top,
                       titles_color = titles_color,
                       x_axes_label = x_axes_label,
                       y_axes_label = y_axes_label,
                       y_axes_values = y_axes_values
  )
  #plot

  #### Creating the legend ####

  legend <- textLegend(bivariate_color_codes = bivariate_color_codes,
                       y_axes_1 = y_axes_1,
                       fill = fill,
                       legend_title = legend_title,
                       legend_title_size = legend_title_size,
                       legend_x_axes_label = legend_x_axes_label,
                       legend_y_axes_label = legend_y_axes_label,
                       word_data_all = word_data_all,
                       legend_number_size = legend_number_size,
                       #only_x_dimension = only_x_dimension,
                       titles_color = titles_color)
  #legend

  #### Plot both figure and legend help(null_dev_env) ####
  final_plot <- suppressWarnings(cowplot::ggdraw() +
                                   cowplot::draw_plot(plot, 0, 0, 1, 1) +
                                   cowplot::draw_plot(legend, legend_x_position, legend_y_position, legend_h_size, legend_w_size))

  output_plot_data <- list(final_plot, textProjectionPlot_comment, word_data_all)
  names(output_plot_data) <- c("final_plot", "description", "processed_word_data")
  output_plot_data
}



