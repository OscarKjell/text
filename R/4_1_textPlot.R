

#DELETE THIS WHEN DONE; EXISTS IN textSimilairty...
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



library(tidyverse)

library(text)
wordembeddings <- wordembeddings4_10
data <- sq_data_tutorial8_10
df_for_plotting <- textPlotData(data$harmonywords, wordembeddings$harmonywords,
  wordembeddings$singlewords_we,
  data$hilstotal, data$swlstotal,
  Bonferroni = TRUE, nrFolds = 2
)
df_for_plotting

library(text)
wordembeddings <- wordembeddings4_10
data <- sq_data_tutorial8_10
words <- data$harmonywords
single_wordembeddings <- wordembeddings$singlewords_we
wordembeddings <- wordembeddings$harmonywords

x <- data$hilstotal
y <- data$swlstotal
Bonferroni = TRUE
nrFolds = 2




# Function that creates semnatic t-test scores for single words for plotting purposes
textPlotDataTextDiff <- function(words,
                                 wordembeddings,
                                 single_wordembeddings = single_wordembeddings_df,
                                 x,
                                 y = NULL,
                                 Bonferroni = TRUE,
                                 nrFolds = 10) {
  if (is.null(y) == TRUE) {
    x <- data.frame(x)
  } else {
    # Combing the dimensions for for-loop
    x <- data.frame(x, y)
  }

  # Creating list for the x and y dimension
  word_data_list <- list()

  # For loop for x and y input; i.e., the two dimensions of the plot i_dim=1 i_dim=2
  for (i_dim in 1:ncol(x)) {

    # Get the word embeddings and Scale/Category
    x1 <- data.frame(words, x[i_dim])
    x1 <- tibble::as_tibble(cbind(x1, wordembeddings))

    # Splitting datasets up to low versus high according to median split
    group1 <- x1[ x1[2] > stats::median(x1[[2]]), ]
    group2 <- x1[ x1[2] < stats::median(x1[[2]]), ]

    # Use function addEqualNrNArows from textTtestscores
    # Function adds rows of NA until group2 and group1 have the same amount of rows.
    # If statement deciding which of group1 or group2 that needs row(s) of NA
    if (nrow(group1) < nrow(group2)) {
      group1 <- addEqualNrNArows(group1, group2)
    } else if (nrow(group1) > nrow(group2)) {
      group2 <- addEqualNrNArows(group2, group1)
    } else {
      group1 <- group1
      group2 <- group2
    }

#    # For loop taking one minus the other to create a
#    # semantic difference representation; using k-fold procedure
#
#    # Creating folds with acret package
    nrF <- nrFolds
    folds <- c(1:nrow(group1))
    folds <- caret::createFolds(folds, k = nrF, list = TRUE, returnTrain = FALSE)
    folds

    semanticTtestscoreslistG1 <- list()
    semanticTtestscoreslistG2 <- list()

    for (i in 1:nrF) {
      ### Summ all semrep in one column and normlise to one SHOULD AVOID HARDCODING 1:7!!!  i=1
      colXsemrep <- colSums(group1[, -c(1:7)][ -folds[[i]], ], na.rm = TRUE)
      # SHOULD WE NORMALISE HERE
      # colXsemrep <- normalizeV(colXsemrep)

      colYsemrep <- colSums(group2[, -c(1:7)][ -folds[[i]], ], na.rm = TRUE)
      # colYsemrep <- normalizeV(colYsemrep)

      # The Semantic Difference Represenation: Taking colX minus colY
      semDifRep <- colXsemrep - colYsemrep

      # Measure the semantic simlarity score between the WORDS of the responses
      # NOT-included-in-the-semDifRep and the semDifRep (i.e., the semantic difference representation)
      # Should I normalise them before(!?)
      # normX <- normalizeV(x[ folds[[i]], ])
      # normY <- normalizeV(y[ folds[[i]], ])

      # Get the words in a column (i=1)
      normG1 <- group1[ folds[[i]], ]
      normG1_words <- data.frame(unlist(strsplit(tolower(normG1$words), " ")))
      normG1_words1 <- tibble::as_tibble(as.character(normG1_words$unlist.strsplit.tolower.normG1.words........))

      normG2 <- group2[ folds[[i]], ]
      normG2_words <- data.frame(unlist(strsplit(tolower(normG2$words), " ")))
      normG2_words1 <- tibble::as_tibble(as.character(normG2_words$unlist.strsplit.tolower.normG2.words........))

      # Get word embeddings for each word; applysemrep is created in 1_textImport
      group1_single1 <- sapply(normG1_words1$value, applysemrep, single_wordembeddings)
      group1_single2 <- tibble::as_tibble(t(group1_single1))

      group2_single1 <- sapply(normG2_words1$value, applysemrep, single_wordembeddings)
      group2_single2 <- tibble::as_tibble(t(group2_single1))

      # Adds the Semantic Difference Representation into a Tibble and then duplicates it
      # to as many rows as it will be compared to with x (nrow(normX))

      # For x: Only get the word embeddings and make them numeric
      group1_single3 <- group1_single2 %>%
        dplyr::mutate_if(is.character, as.numeric)
      # Get as many SemDifRep as group_single3 so that it can be compared
      semDifRep_x <- tibble::as_tibble(t(semDifRep)) %>%
        dplyr::slice(rep(dplyr::row_number(), nrow(group1_single2)))
      # Get Semantic Similairty score between words and SemDifRep
      group1_single4_ss <- cosines(group1_single3, semDifRep_x)
      group1_single4_ss_1 <- tibble::tibble(normG1_words1$value, group1_single4_ss)

      # For y: Only get the word embeddings and make them numeric
      group2_single3 <- group2_single2 %>%
        dplyr::mutate_if(is.character, as.numeric)
      # Get as many SemDifRep as y3 so that it can be compred
      semDifRep_y <- as_tibble(t(semDifRep)) %>%
        dplyr::slice(rep(dplyr::row_number(), nrow(group2_single2)))

      # Get Semantic Similairty score between words and SemDifRep
      group2_single4_ss <- cosines(group2_single3, semDifRep_y)
      group2_single4_ss_1 <- tibble(normG2_words1$value, group2_single4_ss)

      # Lists to save the restuls in
      semanticTtestscoreslistG1[[i]] <- group1_single4_ss_1
      semanticTtestscoreslistG2[[i]] <- group2_single4_ss_1
    }

    # Sorting out a dataframe for the resuts; bind the list of tibbles together to one
    semanticTtestscoreslistG1done <- dplyr::bind_rows(semanticTtestscoreslistG1)
    colnames(semanticTtestscoreslistG1done) <- c("words", "SS")
    semanticTtestscoreslistG1done$g1_1_g2_2 <- rep(1, nrow(semanticTtestscoreslistG1done))

    semanticTtestscoreslistG2done <- dplyr::bind_rows(semanticTtestscoreslistG2)
    colnames(semanticTtestscoreslistG2done) <- c("words", "SS")
    semanticTtestscoreslistG2done$g1_1_g2_2 <- rep(2, nrow(semanticTtestscoreslistG2done))

    # Bind all words together
    semanticTtestscoreslistG1G2done <- dplyr::bind_rows(semanticTtestscoreslistG1done, semanticTtestscoreslistG2done)

    # Getting Descriptives of the words
    # All words SS mean to the semantic comparison representation
    words_mean <- mean(semanticTtestscoreslistG1G2done$SS, na.rm = TRUE)
    # All words SD to the semantic comparison representation
    words_sd <- stats::sd(semanticTtestscoreslistG1G2done$SS, na.rm = TRUE)

    # Each word's mean and sd (i.e., they may have different SS score due to being in different K-folds)
    Words_info <- dplyr::group_by(semanticTtestscoreslistG1G2done, words) %>%
      dplyr::summarize(mean = mean(SS))
    Words_info_sd <- dplyr::group_by(semanticTtestscoreslistG1G2done, words) %>%
      dplyr::summarize(sd = stats::sd(SS))

    semanticTtestscoreslistG1G2done$words
    table(semanticTtestscoreslistG1G2done$words, useNA = "always")
    # The n/frequency of each word
    Words_info$n <- as.numeric(table(semanticTtestscoreslistG1G2done$words, useNA = "ifany"))
    Words_info$sd <- Words_info_sd$sd
    # Add the max SD to the words that do not have a SD
    Words_info$sd[is.na(Words_info$sd)] <- max(Words_info$sd, na.rm = T)

    # To get the p-values even for words only occuring once; I make n = 1 to n = 2.
    # Words_info$n_2 <- Words_info$n
    # Words_info$n_2[Words_info$n_2 == 1] <- 2

    # Computing t-tests: library(BSDA) tsum.test(mean.x=.1,   s.x=.01, n.x=2, mean.y=.136, s.y=.02, n.y=7)
    # Words_info$n_2 adds so it is at least 2 words; and more t-tests can be made.
    n_total_words <- sum(Words_info$n)
    Words_ttest <- data.frame(mapply(BSDA::tsum.test,
                                     mean.x = Words_info$mean, s.x = Words_info$sd, n.x = Words_info$n,
                                     mean.y = words_mean, s.y = words_sd, n.y = n_total_words - Words_info$n,
                                     var.equal = FALSE
    ))
    # Pulling out the t-statistics and p-values from the t-test done above
    p_values <- t(Words_ttest[1:nrow(Words_info)][3, ])
    Words_info$p_values <- unlist(p_values)
    t_statistic <- t(Words_ttest[1:nrow(Words_info)][1, ])
    Words_info$t_statistic <- unlist(t_statistic)

    # Function for Computing Cohen's D (https://www.socscistatistics.com/effectsize/default3.aspx)
    cohens_d <- function(mean_x, sd_x, n_x, mean_y, sd_y, n_y) {
      (mean_x - mean_y) / (sqrt((sd_x^2 + sd_y^2) / 2))
    }
    # Applying Cohens D individually for each word
    cohen_D_df <- data.frame(mapply(cohens_d,
                                    mean_x = Words_info$mean, sd_x = Words_info$sd, n_x = Words_info$n, # Words_info$n_2 adds so it is at least 2 words; and more t-tests can be made.
                                    mean_y = words_mean, sd_y = words_sd, n_y = n_total_words - Words_info$n
    ))
    colnames(cohen_D_df) <- "cohensD"
    Words_info$cohensD <- cohen_D_df$cohensD

    word_data_list[i_dim] <- list(Words_info)
  }
  # Arranging it to one tibble; accounting for x versus x and y input
  if (is.null(y) == TRUE) {
    word_data_tibble <- word_data_list[[1]]
    colnames(word_data_tibble) <- c(
      "words", "mean.x", "n.x", "sd.x",
      "p_values.x", "t_statistic.x", "cohensD.x"
    )
  } else {
    word_data_tibble <- dplyr::full_join(word_data_list[[1]], word_data_list[[2]], by = "words")
  }

  # Bonferroni correction or not
  if (isTRUE(Bonferroni) == TRUE) {
    # If there are no y axes or not.
    if (is.null(y) == TRUE) {
      bonferroni_x <- as.numeric(table(!is.na(word_data_tibble$p_values.x))["TRUE"])
      word_data_tibble_bonf <- word_data_tibble[word_data_tibble$p_values.x < .05 / bonferroni_x, ]
    } else {
      # Counting number of t-test made (i.e, on words more than 1)
      bonferroni_x <- as.numeric(table(!is.na(word_data_tibble$p_values.x))["TRUE"])
      bonferroni_y <- as.numeric(table(!is.na(word_data_tibble$p_values.y))["TRUE"])
      # Select significant words when Bonferroni correcting for multiple comparison
      word_data_tibble_bonf <- word_data_tibble[word_data_tibble$p_values.x < .05 / bonferroni_x | word_data_tibble$p_values.y < .05 / bonferroni_y, ]
    }
    # Remove NAs
    word_data_tibble <- word_data_tibble_bonf[!is.na(word_data_tibble_bonf$words), ]
    word_data_tibble
  } else {
    word_data_tibble
  }
  word_data_tibble
}
