
library(text)
library(tidyverse)
# .libPaths()
# regarding warnings about reached elapsed time limit.
# gc()
# https://community.rstudio.com/t/reached-elapsed-time-limit-warnings-rstudio/36647/4


#DELETE THE FOLLOING FUNCTIONS AS THEY COME FROM OTHER FILES; EXISTS IN textSimilairty... help(map)
addEqualNrNArows <- function(x, y) {
  success <- FALSE
  while (!success) {
    # Add row with NA
    x <- rbind(x, rep(NA, length(x)))
    # check for success
    success <- nrow(x) == nrow(y)
  }
  return(x)
}
# REMOVE when done
cosines <- function(x, y) {
  rowSums(x * y, na.rm = TRUE) / (sqrt(rowSums(x * x, na.rm = TRUE)) * sqrt(rowSums(y * y, na.rm = TRUE)))
}
# REMOVE when done
applysemrep <- function(x, single_wordembeddings1) {
  # If semrep is found get it; if not return NA vector of dimensions
  if (sum(single_wordembeddings1$words == x[TRUE]) %in% 1) {
    x <- tolower(x)
    # Get the semantic representation for a word=x
    word1rep <- single_wordembeddings1[single_wordembeddings1$words == x, ]
    # Only get the semantic represenation as a vector without the actual word in the first column
    wordrep <- purrr::as_vector(word1rep %>% dplyr::select(dplyr::starts_with("V")))
    # If the word does not have a semrep return vector with NA the same number of dimensions as columns with V
  } else if (x %in% NA) {
    # The length() refers to how many column starts with V (i.e., how many dimensions)
    wordrep <- data.frame(matrix(ncol = length(single_wordembeddings1 %>% dplyr::select(dplyr::starts_with("V"))), nrow = 1))
    class(wordrep)
    wordrep <- as.numeric(wordrep)
  } else {
    wordrep <- data.frame(matrix(ncol = length(single_wordembeddings1 %>% dplyr::select(dplyr::starts_with("V"))), nrow = 1))
    wordrep <- as.numeric(wordrep)
  }
}
# REMOVE when done
textEmbeddingAggregation <- function(x, aggregation = "min"){
  if(aggregation == "min"){
    min_vector <- unlist(map(x, min, na.rm = TRUE))
  } else if (aggregation == "max") {
    max_vector <- unlist(map(x, max, na.rm = TRUE))
  } else if (aggregation == "mean") {
    mean_vector <- colMeans(x, na.rm = TRUE)
  } else if (aggregation == "CLS"){
    CLS <- x %>%
      dplyr::filter(token_index == 1, layer_index == 1)
  } else if (aggregation == "normalize1") {
    #    norma_vector <- unlist(map(x, norma))
    x2 <- x[complete.cases(x), ]
    x3 <- colSums(x2) # BELOW NEED FIXING; REMOVED FROM CRAN
    # x4 <- ppls::normalize.vector(x3)
  }
}

# REMOVE WHEN DONE: Examine how the ordered data's mean of the cosine compare with the random data's, null comparison distribution help(switch)
p_value_comparing_with_Null <- function(Observedresults, NULLresults, Npermutations, alternative){
  switch(alternative,
         "two_sided" = {
           p_value <- 2 * (min(sum(NULLresults < Observedresults), sum(NULLresults > Observedresults)) / sum(!is.na(NULLresults)))
         },
         # SHOULD NOT THESE: < BE <= CONSIDERING THAT THE DEF. FOR P IS "the probability of a result at least as extreme as the one observed"
         # https://stats.stackexchange.com/questions/120099/big-difference-in-p-value-from-permutation-test
         "less" = {
           p_value <- sum(NULLresults < Observedresults) / sum(!is.na(NULLresults))
         },
         "greater" = {
           p_value <- sum(NULLresults > Observedresults) / sum(!is.na(NULLresults))
         }
  )
  #  if (!is.na(p_value)) {   # This is removed so that p_values can be 0; otherwise need to do many bonferroni
  #    if (p_value == 0) { p_value <- 1 / (Npermutations + 1) }
  #  }
  return(p_value)
}




# KEEP HERE: Creat distribution: Helper Function
GroupX_cummalative_distribution_cosine <- function(words_groupX_single_wordembedding_b, Aggregated_word_embedding_groupX, Npermutations, n_per_split) {
  forloops <- ceiling(Npermutations/n_per_split)
  cosine_null_distribution <- list()
  for(i in 1:forloops){
    # Select random word embeddings accoridng to setting n_per_split = 1000
    indice <- sample(nrow(words_groupX_single_wordembedding_b), n_per_split, replace=TRUE)
    random_group2_embedding <- words_groupX_single_wordembedding_b[indice, ]

    # Compute the cosine between randomly drawn word embeddings and compute the mean
    group2_rcosines <- cosines(random_group2_embedding, t(replicate(nrow(random_group2_embedding), Aggregated_word_embedding_groupX)))
    cosine_null_distribution[i] <- as_tibble(group2_rcosines)
    cosine_null_distribution
  }
  cosine_null_distribution <- as_tibble(unlist(cosine_null_distribution))
  cosine_null_distribution <- cosine_null_distribution[complete.cases(cosine_null_distribution),]
}


# KEEP HERE: Helper Function, take all words as input and arrange them in column with an accomponing column with frequency
unique_freq_words <- function(words){
  words_group <- data.frame(unlist(strsplit(tolower(words), " ")))
  words_groupb <- tibble::as_tibble(as.character(words_group[,1]))
  words_groupb_freq <- as_tibble(table(words_groupb))
  colnames(words_groupb_freq) <- c("words", "n")
  words_groupb_freq
}

# This is just to get variables when testing the function
load("/Users/oscarkjell/Desktop/Projects/0 Research/0 text r-package/text_data_examples/sq_data_tutorial4_100.rda")
sq_data_tutorial4_100
load("/Users/oscarkjell/Desktop/Projects/0 Research/0 text r-package/text_data_examples/wordembeddings4_100.rda")
sq_data_tutorial8_100 <- read_rds("/Users/oscarkjell/Desktop/Projects/0 Research/0 text r-package/text_plot_test/sq_data_tutorial8_100.rda")



# Large
# words <- solmini_no_na_raw$harmonywords
# wordembeddings <- solmini_no_na_wordembeddings4_600$harmonywords
# single_wordembeddings <- solmini_no_na_wordembeddings4_600$singlewords_we
# x <- solmini_no_na_raw$hilstotal

# small
words = sq_data_tutorial8_100$harmonywords
wordembeddings = wordembeddings4_100$harmonywords
single_wordembeddings = wordembeddings4_100$singlewords_we
x = sq_data_tutorial8_100$hilstotal

#x <- rnorm(100, mean=50, sd=10)
y = NULL #sq_data_tutorial8_100$swlstotal
Npermutations = 1000
n_per_split = 100
aggregation = "mean"
i_dim = 1
word_weight_power = 1
split = "median"
# word_weight_power = 1 #; take the power of X of the frequency in the computation of aggreated word embeddings for group 1 and 2.
min_freq_words = 0
pca = NULL   # ; pca = 10 pca = 0.9 pca = 1 # Number of pca dimensions on the word embeddings; if under 1 it takes out % of variance; if equal or above 1 it takes out dimensions

# Function that creates semnatic t-test scores for single words for plotting purposes
textPlotDataTextDiff <- function(words,
                                 wordembeddings,
                                 single_wordembeddings = single_wordembeddings_df,
                                 pca = NULL,
                                 x,
                                 y = NULL,
                                 aggregation = "mean",
                                 split = "median",
                                 word_weight_power = 1,
                                 min_freq_words=0,
                                 n_per_split = 50000, # this is to avoid reaching memory limits, the higher the faster; but different systems can compute different
                                 Npermutations = 10000) {



  # PCA on single_wordembeddings
  if (is.numeric(pca)) {
    # Select word embeddings to be included in plot
    uniques_words_all <- unique_freq_words(words)
    uniques_words_all_wordembedding <- sapply(uniques_words_all$words, applysemrep, single_wordembeddings)
    uniques_words_all_wordembedding <- tibble::as_tibble(t(uniques_words_all_wordembedding))

    rec_pca <- recipes::recipe( ~ ., data = uniques_words_all_wordembedding)
    pca_trans <- rec_pca %>%
      recipes::step_center(recipes::all_numeric()) %>%
      recipes::step_scale(recipes::all_numeric()) %>%
      recipes::step_naomit(V1 , skip = TRUE)

    if(pca < 1) {
      pca_trans <- recipes::step_pca(pca_trans, recipes::all_numeric(), threshold = pca)
    } else if(pca >= 1) {
      pca_trans <- recipes::step_pca(pca_trans, recipes::all_numeric(), num_comp = pca)
    }

    pca_estimates <- recipes::prep(pca_trans, training = uniques_words_all_wordembedding)
    pca_data <- recipes::bake(pca_estimates, uniques_words_all_wordembedding)
    pca_data <- pca_data %>% setNames(paste0('V_', names(.)))
    single_wordembeddings <- dplyr::bind_cols(uniques_words_all, pca_data)
    single_wordembeddings
  }

  # Make datafram (and combine x and y)
  if (is.null(y) == TRUE) {
    x <- data.frame(x)
  } else {
    # Combine the dimensions for for-loop
    x <- data.frame(x, y)
  }

  # Creating a list for the x and y dimensions
  word_data_list <- list()

  # For-loop for x and y input/dimensions; i.e., y if the plot has two dimensions (i_dim=1 i_dim=2)
  for (i_dim in 1:ncol(x)) {

    # Get the word embeddings and scale/category for the plot dimension (i.e., x or y from above)
    x1 <- data.frame(words, x[i_dim])
    x2 <- tibble::as_tibble(cbind(x1, wordembeddings)) # Do I really need these aggregated embeddings

    # Splitting datasets up to low versus high according to median split
    group1 <- x2[ x2[2] < stats::median(x2[[2]]), ]
    group2 <- x2[ x2[2] > stats::median(x2[[2]]), ]

    # Use function addEqualNrNArows from textTtestscores
    # Function adds rows of NA until group2 and group1 have the same amount of rows.
    if (nrow(group1) < nrow(group2)) {
      group1 <- addEqualNrNArows(group1, group2)
    } else if (nrow(group1) > nrow(group2)) {
      group2 <- addEqualNrNArows(group2, group1)
    } else {
      group1 <- group1
      group2 <- group2
    }

    set.seed(2020)
    ##########
    ####        Get word embeddings
    ##########
    # Group 1: getting unique words and their frequency
    words_group1b_freq <- unique_freq_words(group1$words)
    words_group1b_freq <- words_group1b_freq[words_group1b_freq$n >= min_freq_words, ]
    words_group1b_freq$n_g1_g2 <- words_group1b_freq$n * -1
    # Get word embeddings for each word (applysemrep function is created in 1_1_textEmbedd).
    words_group1_single_wordembedding <- sapply(words_group1b_freq$words, applysemrep, single_wordembeddings)
    words_group1_single_wordembedding_b <- tibble::as_tibble(t(words_group1_single_wordembedding))

    # Group 2
    words_group2b_freq <- unique_freq_words(group2$words)
    words_group2b_freq <- words_group2b_freq[words_group2b_freq$n >= min_freq_words, ]
    words_group2b_freq$n_g1_g2 <- words_group2b_freq$n * 1
    words_group2_single_wordembedding <- sapply(words_group2b_freq$words, applysemrep, single_wordembeddings)
    words_group2_single_wordembedding_b <- tibble::as_tibble(t(words_group2_single_wordembedding))


    ############
    ######         1 Create COMPARISON/Projection embedding: all Group 1 & Group 2 word embeddings.
    ############
    ### Sum all word embeddings in one column

    # split="median" split = "quartile"
    if(split=="median"){

      words_group1_agg_single_wordembedding_c <- cbind(words_group1b_freq, words_group1_single_wordembedding_b)
      words_group2_agg_single_wordembedding_c <- cbind(words_group2b_freq, words_group2_single_wordembedding_b)

    } else if(split =="quartile"){
      # Get lower and upper quatiles
      z <- x1[,2]
      qs <- broom::tidy(summary(z))
      group1_agg <- x2[ x2[2] < qs$q1, ]
      group2_agg <- x2[ x2[2] > qs$q3, ]

      words_group1_agg_freq <- unique_freq_words(group1_agg$words)
      words_group1_agg_single_wordembedding <- sapply(words_group1_agg_freq$words, applysemrep, single_wordembeddings)
      words_group1_agg_single_wordembedding_b <- tibble::as_tibble(t(words_group1_agg_single_wordembedding))
      words_group1_agg_single_wordembedding_c <- cbind(words_group1_agg_freq, words_group1_agg_single_wordembedding_b)

      words_group2_agg_freq <- unique_freq_words(group2_agg$words)
      words_group2_agg_single_wordembedding <- sapply(words_group2_agg_freq$words, applysemrep, single_wordembeddings)
      words_group2_agg_single_wordembedding_b <- tibble::as_tibble(t(words_group2_agg_single_wordembedding))
      words_group2_agg_single_wordembedding_c <- cbind(words_group2_agg_freq, words_group2_agg_single_wordembedding_b)
    }

    words_group1_agg_single_wordembedding_c <- as_tibble(words_group1_agg_single_wordembedding_c)
    words_group2_agg_single_wordembedding_c <- as_tibble(words_group2_agg_single_wordembedding_c)

    # Weight words for aggregated word embedding: Repeat rows according to n word_weight_power = 2 word_weight_power = 5
    words_group1_agg_single_wordembedding_d <-  words_group1_agg_single_wordembedding_c %>%
      mutate(., n1 = n^word_weight_power)  %>%
      tidyr::uncount(n1)

    words_group2_agg_single_wordembedding_d <-  words_group2_agg_single_wordembedding_c %>%
      mutate(., n1 = n^word_weight_power)  %>%
      tidyr::uncount(n1)

    Aggregated_word_embedding_group1 <- textEmbeddingAggregation(dplyr::select(words_group1_agg_single_wordembedding_d, dplyr::starts_with("V")), aggregation = aggregation)
    Aggregated_word_embedding_group2 <- textEmbeddingAggregation(dplyr::select(words_group2_agg_single_wordembedding_d, dplyr::starts_with("V")), aggregation = aggregation)

    ############
    ######         Project embedding
    #############
    projected_embedding <- Aggregated_word_embedding_group2 - Aggregated_word_embedding_group1

    # Position words in relation to Group 2 (High)
    all_unique_words_freq <- unique_freq_words(x2$words)
    # Get word embeddings for each word (applysemrep function is created in 1_1_textEmbedd).
    all_unique_words_we <- sapply(all_unique_words_freq$words, applysemrep, single_wordembeddings)
    all_unique_words_we_b <- tibble::as_tibble(t(all_unique_words_we))

    # Position the embedding; i.e., taking the word embedding substrated with aggregated word embedding
    #version 1: word_new = word_old - ((group(high harmony) + group(low harmony)) / 2)
    words_positioned_embeddings <- all_unique_words_we_b - ((t(replicate(nrow(all_unique_words_we_b), Aggregated_word_embedding_group2)) +
                                                               t(replicate(nrow(all_unique_words_we_b), Aggregated_word_embedding_group1)))/2)

    #     # version 2: word_new = word_old - mean(all_words)  [1x768] x |words|
    #     Aggregated_word_embedding_group1_2 <- rbind(Aggregated_word_embedding_group1, Aggregated_word_embedding_group2)
    #     Aggregated_word_embedding_group1_2a <- textEmbeddingAggregation(Aggregated_word_embedding_group1_2, aggregation = "mean")
    #     words_positioned_embeddings <- all_unique_words_we_b - t(replicate(nrow(all_unique_words_we_b), Aggregated_word_embedding_group1_2a))

    # project the embeddings using dot products
    dot_products_observed <- rowSums(words_positioned_embeddings * t(replicate(nrow(all_unique_words_we_b), projected_embedding)))
    all_unique_words_freq$dot <- dot_products_observed

    ############
    ######         Comparison distributions for Project embedding
    #############
    # Get df with ALL embedding to randomly draw from (without log transformed, and quartiles)
    words_group1_agg_single_wordembedding_e <- cbind(words_group1b_freq, words_group1_single_wordembedding_b)
    words_group1_agg_single_wordembedding_f <-  words_group1_agg_single_wordembedding_e %>%
      mutate(., n1_e = n)  %>%
      tidyr::uncount(n1_e)

    words_group2_agg_single_wordembedding_e <- cbind(words_group2b_freq, words_group2_single_wordembedding_b)
    words_group2_agg_single_wordembedding_f <-  words_group2_agg_single_wordembedding_e %>%
      mutate(., n1_e = n)  %>%
      tidyr::uncount(n1_e)

    words_group1_2_agg_single_wordembedding_e <- rbind(words_group1_agg_single_wordembedding_f, words_group2_agg_single_wordembedding_f)
    words_group1_2_agg_single_wordembedding_e1 <- dplyr::select(words_group1_2_agg_single_wordembedding_e, dplyr::starts_with("V"))


    # Splitting up the permutations in different loops to avoid memory issues n_per_split=1000
    forloops <- ceiling(Npermutations/n_per_split)
    dot_null_distribution <- list()


    for(i in 1:forloops){
      ### Create new Projected embedding
      #Randomly split word embeddings into two groups: words_group1_2_agg_single_wordembedding_e1
      ind <- sample(c(TRUE, FALSE), nrow(words_group1_2_agg_single_wordembedding_e1), replace=TRUE)
      Aggregated_word_embedding_group1_random <- words_group1_2_agg_single_wordembedding_e1[ind, ]
      Aggregated_word_embedding_group1_random <- textEmbeddingAggregation(Aggregated_word_embedding_group1_random, aggregation = "mean")
      Aggregated_word_embedding_group2_random <- words_group1_2_agg_single_wordembedding_e1[!ind, ]
      Aggregated_word_embedding_group2_random <- textEmbeddingAggregation(Aggregated_word_embedding_group2_random, aggregation = "mean")
      projected_embedding_random <- Aggregated_word_embedding_group2_random - Aggregated_word_embedding_group1_random

      # Select random word embeddings accordings to setting
      indice <- sample(nrow(words_group1_2_agg_single_wordembedding_e1), n_per_split, replace=TRUE)
      random_group2_embedding <- words_group1_2_agg_single_wordembedding_e1[indice, ]

      # Position the embedding; i.e., taking the word embedding substrated with aggregated word embedding
      #version 1: word_new = word_old - ((group(high harmony) + group(low harmony)) / 2)
      words_positioned_embeddings_random <- random_group2_embedding - ((t(replicate(nrow(random_group2_embedding), Aggregated_word_embedding_group2)) +
                                                                          t(replicate(nrow(random_group2_embedding), Aggregated_word_embedding_group1)))/2)
      #      # version 2: word_new = word_old - mean(all_words)  [1x768] x |words|
      #      words_positioned_embeddings_random <- random_group2_embedding - t(replicate(nrow(random_group2_embedding), Aggregated_word_embedding_group1_2a))

      # project the embeddings using dot products
      dot_products_null <- as_tibble(rowSums(words_positioned_embeddings_random * t(replicate(nrow(words_positioned_embeddings_random), projected_embedding_random)))) # _random to test

      dot_null_distribution[i] <- dot_products_null
      dot_null_distribution
    }
    dot_null_distribution <- as_tibble(unlist(dot_null_distribution))

    ### Compare observed dot-product with null
    dot_null_distribution <- dot_null_distribution[complete.cases(dot_null_distribution),]
    p_values_dot_prod <- purrr::map(as.list(as_vector(dot_products_observed)), p_value_comparing_with_Null,
                                    dot_null_distribution,
                                    Npermutations = Npermutations, alternative = "two_sided")
    p_values_dot_prod <- unlist(p_values_dot_prod)
    # Sort out dataframe
    dot_result <- cbind(all_unique_words_freq, dot_products_observed, as_tibble(unlist(p_values_dot_prod)))
    dot_result <- as_tibble(dot_result)
    colnames(dot_result) <- c("words", "n", "dot", "dot2", "p_value_dot")
    dot_result <- select(dot_result,-c(dot2))



    ############
    ######         Cosine based plots
    #############

    ###### WITHIN ANALYSES: Compute cosine semantic similairty score between single words and aggregated word embedding
    # Group1 single words compared with Group2 aggregated word embedding
    group1_words_cosine_within <- cosines(words_group1_single_wordembedding_b, t(replicate(nrow(words_group1_single_wordembedding_b), Aggregated_word_embedding_group1)))
    group1_words_cosine_within_1<- tibble::tibble(words_group1b_freq$words, group1_words_cosine_within)
    # Group2 single words compared with Group1 aggregated word embedding
    group2_words_cosine_within <- cosines(words_group2_single_wordembedding_b, t(replicate(nrow(words_group2_single_wordembedding_b), Aggregated_word_embedding_group2)))
    group2_words_cosine_within_1<- tibble::tibble(words_group2b_freq$words, group2_words_cosine_within)

    # ###### BETWEEN: Compute COSINE between single words and aggregated word embedding view(group1_words_cosine_1)
    # # Group1 single words compared with Group2 aggregated word embedding
    # group1_words_cosine_between <- cosines(words_group1_single_wordembedding_b, t(replicate(nrow(words_group1_single_wordembedding_b), Aggregated_word_embedding_group2)))
    # group1_words_cosine_between_1<- tibble::tibble(words_group1b_freq$words, group1_words_cosine_between)
    # # Group2 single words compared with Group1 aggregated word embedding
    # group2_words_cosine_between <- cosines(words_group2_single_wordembedding_b, t(replicate(nrow(words_group2_single_wordembedding_b), Aggregated_word_embedding_group1)))
    # group2_words_cosine_between_1<- tibble::tibble(words_group2b_freq$words, group2_words_cosine_between)
    #
    # ###### BETWEEN with DIFF-EMBEDDING
    # Aggregated_word_embedding_group2_MINUS_1a <- Aggregated_word_embedding_group2 - Aggregated_word_embedding_group1
    # Aggregated_word_embedding_group1_MINUS_2b <- Aggregated_word_embedding_group1 - Aggregated_word_embedding_group2
    # # Group1 single words compared with Group2 aggregated word embedding
    # group1_words_cosine_between_diff <- cosines(words_group1_single_wordembedding_b, t(replicate(nrow(words_group1_single_wordembedding_b), Aggregated_word_embedding_group2_MINUS_1a)))
    # group1_words_cosine_between_diff_1<- tibble::tibble(words_group1b_freq$words, group1_words_cosine_between_diff)
    # # Group2 single words compared with Group1 aggregated word embedding
    # group2_words_cosine_between_diff <- cosines(words_group2_single_wordembedding_b, t(replicate(nrow(words_group2_single_wordembedding_b), Aggregated_word_embedding_group1_MINUS_2b)))
    # group2_words_cosine_between_diff_1 <- tibble::tibble(words_group2b_freq$words, group2_words_cosine_between_diff)


    ##########
    #####     Create NULL distribution
    ##########

    # Permutate randomly from Group 1 and compute cosine to Group 1 aggregated embedding; Npermutations = 10000
    # Group 1; WITHIN distribution
    Group1_Null_distribution_cosine_permutated_within <- GroupX_cummalative_distribution_cosine(words_group1_single_wordembedding_b,
                                                                                                Aggregated_word_embedding_group1,
                                                                                                Npermutations, n_per_split)
    # Group 2; WITHIN distribution
    Group2_Null_distribution_cosine_permutated_within <- GroupX_cummalative_distribution_cosine(words_group2_single_wordembedding_b,
                                                                                                Aggregated_word_embedding_group2,
                                                                                                Npermutations, n_per_split)

    # Group 1; BETWEEN comparison distribution
    Group1_Null_distribution_cosine_permutated_between <- GroupX_cummalative_distribution_cosine(words_group1_single_wordembedding_b,
                                                                                                 Aggregated_word_embedding_group1,
                                                                                                 Npermutations, n_per_split)
    # Group 2; BETWEEN comparison distribution
    Group2_Null_distribution_cosine_permutated_between <- GroupX_cummalative_distribution_cosine(words_group2_single_wordembedding_b,
                                                                                                 Aggregated_word_embedding_group2,
                                                                                                 Npermutations, n_per_split)

    # Group 1; BETWEEN-DIFFERENCE EMBEDDING comparison distribution
    Group1_Null_distribution_cosine_permutated_between_diff <- GroupX_cummalative_distribution_cosine(words_group1_single_wordembedding_b,
                                                                                                      Aggregated_word_embedding_group2_MINUS_1a, #Perhaps try change here
                                                                                                      Npermutations, n_per_split)
    # Group 2; BETWEEN-DIFFERENCE EMBEDDING comparison distribution
    Group2_Null_distribution_cosine_permutated_between_diff <- GroupX_cummalative_distribution_cosine(words_group2_single_wordembedding_b,
                                                                                                      Aggregated_word_embedding_group1_MINUS_2b, #Perhaps try change here
                                                                                                      Npermutations, n_per_split)




    ##########
    #####         4     Comparing observed with NULL distribution
    ##########
    # Group 1
    # WITHIN 1
    Group1_p_values_within <- purrr::map(as.list(as_vector(group1_words_cosine_within_1[,2])), p_value_comparing_with_Null,
                                         Group1_Null_distribution_cosine_permutated_within,
                                         Npermutations = Npermutations, alternative = "greater")
    # BETWEEN
    Group1_p_values_between <- purrr::map(as.list(as_vector(group1_words_cosine_between_1[,2])), p_value_comparing_with_Null,
                                          Group1_Null_distribution_cosine_permutated_between, # Group2_Null_distribution_cosine_permutated_between
                                          Npermutations = Npermutations, alternative = "two_sided")

    # BETWEEN DIFFERENCE
    Group1_p_values_between_diff <- purrr::map(as.list(as_vector(group1_words_cosine_between_diff_1[,2])), p_value_comparing_with_Null,
                                               Group1_Null_distribution_cosine_permutated_between_diff, # Group1_Null_distribution_cosine_permutated_between_diff
                                               Npermutations = Npermutations, alternative = "two_sided")

    # Sorting out Dataframe
    Group1_result <- cbind(group1_words_cosine_between_1, as_tibble(unlist(Group1_p_values_between)),
                           group1_words_cosine_between_diff_1[,2], as_tibble(unlist(Group1_p_values_between_diff)),
                           group1_words_cosine_within_1[,2], as_tibble(unlist(Group1_p_values_within)))

    Group1_result$forG2 <- rep(NA, nrow(group1_words_cosine_between_1))
    Group1_result$forG2_1 <- rep(NA, nrow(group1_words_cosine_between_1))
    colnames(Group1_result) <- c("words", "cosine_between", "p_values_between",
                                 "cosine_between_diff", "p_values_between_diff",
                                 "cosine_within_g1", "p_values_within_g1",
                                 "cosine_within_g2", "p_values_within_g2")

    Group1_result_n <- dplyr::full_join(Group1_result, words_group1b_freq, by = "words") %>%
      unique()


    # Group 2
    # WITHIN
    Group2_p_values_within <- purrr::map(as.list(as_vector(group2_words_cosine_within_1[,2])), p_value_comparing_with_Null,
                                         Group2_Null_distribution_cosine_permutated_within,
                                         Npermutations = Npermutations, alternative = "greater")
    # BETWEEN
    Group2_p_values_between <- purrr::map(as.list(as_vector(group2_words_cosine_between_1[,2])), p_value_comparing_with_Null,
                                          Group1_Null_distribution_cosine_permutated_between, # Group2_Null_distribution_cosine_permutated
                                          Npermutations = Npermutations, alternative = "two_sided")

    # BETWEEN DIFFERENCE
    Group2_p_values_between_diff <- purrr::map(as.list(as_vector(group2_words_cosine_between_diff_1[,2])), p_value_comparing_with_Null,
                                               Group1_Null_distribution_cosine_permutated_between_diff, # Group2_Null_distribution_cosine_permutated
                                               Npermutations = Npermutations, alternative = "two_sided")

    # Sorting out Dataframe
    forG1 <- rep(NA, nrow(group2_words_cosine_between_1))

    Group2_result <- cbind(group2_words_cosine_between_1, as_tibble(unlist(Group2_p_values_between)),
                           group2_words_cosine_between_diff_1[,2], as_tibble(unlist(Group2_p_values_between_diff)),
                           forG1, forG1,
                           group2_words_cosine_within_1[,2], as_tibble(unlist(Group2_p_values_within)))


    colnames(Group2_result) <- c("words", "cosine_between", "p_values_between",
                                 "cosine_between_diff", "p_values_between_diff",
                                 "cosine_within_g1", "p_values_within_g1",
                                 "cosine_within_g2", "p_values_within_g2")
    head(Group2_result)
    Group2_result_n <- dplyr::full_join(Group2_result, words_group2b_freq, by = "words") %>%
      unique()


    # Sorting out the resuts; make grouping variable
    Group1_result_n$g1_1_g2_2 <- rep(1, nrow(Group1_result_n))
    Group2_result_n$g1_1_g2_2 <- rep(2, nrow(Group2_result_n))

    # Reverse transoform Cosine to be intuitive to plot
    Group1_result_n$cosine_between_rev <- Group1_result_n$cosine_between  # -1
    Group2_result_n$cosine_between_rev <- Group2_result_n$cosine_between *-1 # -1) #* -1

    # Reverse transoform Cosine to be intuitive to plot
    Group1_result_n$cosine_between_diff_rev <- Group1_result_n$cosine_between_diff #*-1 # -1
    Group2_result_n$cosine_between_diff_rev <- Group2_result_n$cosine_between_diff *-1 # -1) #* -1

    # Bind all words together
    Group1_2_result <- dplyr::bind_rows(Group1_result_n, Group2_result_n) # View(Group2_result_n)
    head(Group1_2_result)

    # Merge the within cosine and within p-values to one g1_g2 column each; also make cosine in group -, so they are plotted to the left
    Group1_2_result$cosine_within_g1_g2   <-   coalesce((Group1_2_result$cosine_within_g1*-1), Group1_2_result$cosine_within_g2)
    Group1_2_result$p_value_within_g1_g2  <-   coalesce(Group1_2_result$p_values_within_g1, Group1_2_result$p_values_within_g2)


    #Including dot restuls.
    Group1_2_result_dot <- dplyr::full_join(Group1_2_result, dot_result, by = "words")
    #dput(colnames(Group1_2_result_dot))
    #head(Group1_2_result_dot)

    word_data_list[i_dim] <- list(Group1_2_result_dot)
  }

  # Arranging it to one tibble; accounting for x versus x and y input
  if (is.null(y) == TRUE) {
    word_data_tibble <- word_data_list[[1]]
    colnames(word_data_tibble) <-
      c("words", "cosine_between.x", "p_values_between.x", "cosine_between_diff.x",
        "p_values_between_diff.x", "cosine_within_g1.x", "p_values_within_g1.x",
        "cosine_within_g2.x", "p_values_within_g2.x", "n.x", "n_g1_g2.x", "g1_1_g2_2.x",
        "cosine_between_rev.x", "cosine_between_diff_rev.x", "cosine_within_g1_g2.x",
        "p_value_within_g1_g2.x", "n.y", "dot.x", "dot2.x", "p_value_dot.x")
    #colnames(word_data_tibble) <- c("words", "cosine.x", "p_values_between.x", "cosine_within_g1.x", "p_values_within_g1.x", "cosine_within_g2.x", "p_values_within_g2.x", "n.x", "n_g1g2.x", "g1_1_g2_2.x", "cosine_between_rev.x", "cosine_within_g1_g2.x", "p_values_within_g1_g2.x", "n.dot", "dot", "dot2", "p_value_dot")
  } else {
    word_data_tibble <- dplyr::full_join(word_data_list[[1]], word_data_list[[2]], by = "words")
  }
  return(word_data_tibble)
}
#### End textPlotDataTextDiff
#############



#word_data <- plot_data
#y_axes = NULL
# word_data <- plot_data
# plot_n_words = 25
# title_top = " "
# titles_color = "#61605e"
# x_axes = "cosine_within_g1_g2.x"
# y_axes = NULL
# p_values = "p_values_within_g1_g2.x"
# p_alpha = 0.05
# p_adjust_method = "holm" # c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY",  "fdr", "none")
# x_axes_label = "Semantic Distance"
# y_axes_label = "Semantic Distance"
# scale_x_axes_lim = NULL
# scale_y_axes_lim = NULL
# y_axes_values = element_blank()
# word_font = "Arial"
# colors_words = c("#ff0000", "#ff8080", "white", "#99e699", "#33cc33")
# colors_words_scale = c(-0.1, -0.01, 0, 0.01, 0.1)
# word_size_range = c(3, 8)
# position_jitter_hight = .0
# position_jitter_width = .03
# point_size = 0.5
# arrow_transparency = 0.1


textPlotVizTextDiff <- function(word_data,
                                plot_n_words = 25,
                                title_top = " ",
                                titles_color = "#61605e",
                                x_axes = "cosine_rev.x",
                                y_axes = NULL,
                                p_values = "p_values_within_g1_g2.x",
                                p_alpha = 0.05,
                                p_adjust_method = "none", # c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY",  "fdr", "none")
                                x_axes_label = "Semantic Distance",
                                y_axes_label = "Semantic Distance",
                                scale_x_axes_lim = NULL,
                                scale_y_axes_lim = NULL,
                                y_axes_values = element_blank(),
                                word_font = "Arial",
                                colors_words = c("#ff0000", "#ff8080", "white", "#99e699", "#33cc33"),
                                colors_words_scale = c(-0.1, -0.01, 0, 0.01, 0.1),
                                word_size_range = c(3, 8),
                                position_jitter_hight = .0,
                                position_jitter_width = .03,
                                point_size = 0.5,
                                arrow_transparency = 0.1) {
  set.seed(2020)


  # Select words to plot according to p-value and selected adjustment for multiple comparison (e.g., bonferroni)
  if (is.character(p_adjust_method) == TRUE & is.null(y_axes) == TRUE) {
    # Getting adjusted p-values head(word_data) help(p.adjust)
    word_data$adjusted_p_values.x <- stats::p.adjust(word_data[, p_values], method = p_adjust_method)
    word_data_p <- word_data[word_data$adjusted_p_values.x < p_alpha, ] #View(word_data_p)

  } else if (is.character(p_adjust_method) & is.null(y_axes) == FALSE) {
    # Select significant words when correcting for multiple comparison
    word_data$adjusted_p_values.x <- stats::p.adjust(word_data$p_values.x, method = p_adjust_method)
    word_data$adjusted_p_values.y <- stats::p.adjust(word_data$p_values.y, method = p_adjust_method)
    word_data_p <- word_data[((word_data$adjusted_p_values.x < p_alpha) | (word_data$adjusted_p_values.y < p_alpha)), ]
    # Only using p-value (i.e., now correction)
  } else if (is.null(p_adjust_method)==TRUE & is.null(y_axes) == TRUE) {
    word_data_p <- word_data[(word_data$p_values.x < p_alpha), ]
  } else if (is.null(p_adjust_method) == TRUE & is.null(y_axes) == FALSE) {
    word_data_p <- word_data[((word_data$p_values.x < p_alpha)| (word_data$p_values.y < p_alpha)), ]
  }

  # Remove NAs view(word_data)
  word_data <- word_data_p[!is.na(word_data_p$words), ]

  # Make limits for color gradient so that 0 becomes in the middle;
  color_limit <- max(abs(word_data[, x_axes]), na.rm=TRUE) * c(-1, 1)

  # Select lowest p-values for x or both x and y if needed. help(get)
  if (is.null(y_axes)) {
    # Order data from first according to lowest p-value and then to highest absolut cosine p_values="p_values_within_g1_g2.x"
    data1 <- word_data[with(word_data, order(-n.x, base::get(p_values))), ]
    #data1 <- word_data[with(word_data, order(-n.x, p_values_within_g1_g2.x)), ] # p_values_between, -abs(cosine_rev) view(word_data) view(data1)
    # Selecting the plot_n_words
    data <- data1[1:plot_n_words, ]
    data


  } else {
    # Selecting as above but for both x and y
    data1x <- word_data[with(word_data, order(p_values.x, -abs(cosine_rev.x))), ]
    # Selecting the plot_n_words
    data2x <- data1x[1:plot_n_words, ]
    # Selecting as above but for both x and y
    data1y <- word_data[with(word_data, order(p_values.y, -abs(cosine_rev.y))), ]
    # Selecting the plot_n_words
    data2y <- data1y[1:plot_n_words, ]
    # Combing the words and selecting remove duplicates
    data3 <- rbind(data2x, data2y)
    data <- unique(data3)
  }

  # This solution is because it is not possible to send "0" as a parameter
  if (is.null(y_axes) == TRUE) {
    one_dime <- 0
    y_axes <- "one_dime"
  } else {
    y_axes
  }

  # Plot
  plot <- data %>%

    # construct ggplot; the !!sym( ) is to  turn the strings into symbols.
    ggplot2::ggplot(ggplot2::aes(!!rlang::sym(x_axes), !!rlang::sym(y_axes), label = words)) +

    # Title
    ggplot2::ggtitle(paste0(title_top)) +

    # Help creat possibility to remove y-axes numbers help(scale_y_continuous)
    ggplot2::scale_x_continuous(limits = scale_x_axes_lim) +
    ggplot2::scale_y_continuous(limits = scale_y_axes_lim) +

    # ggrepel geom, make arrows transparent, color by rank, size by n help(geom_text_repel)
    ggrepel::geom_text_repel(
      segment.alpha  = arrow_transparency,
      position = ggplot2::position_jitter(h = position_jitter_hight, w = position_jitter_width),
      ggplot2::aes(color = !!rlang::sym(x_axes), size = n.x, family = word_font) #size = n.x
    ) +

    # Decide size and color of the points
    ggplot2::geom_point(
      size = point_size,
      ggplot2::aes(color = !!rlang::sym(x_axes))
    ) +

    # set color gradient, & customize legend help(guide_colorbar) help(rescale) help(scale_colour_gradientn)
    ggplot2::scale_colour_gradientn(
      colours = colors_words,
      limit = color_limit,
      values = scales::rescale(colors_words_scale), #scales::rescale(colors_words_scale)
      space = "Lab",
      aesthetics = "colour",
      guide = ggplot2::guide_colorbar(
        direction = "horizontal",
        title.position = "top",
        title = "Cosine",
        ggplot2::element_text(color = titles_color)
      )
    ) +

    # set word size range
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

    ggplot2::labs(y = y_axes_label, x = x_axes_label) +

    # minimal theme, and turning off legends
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
###### End textPlotVizTextDiff




