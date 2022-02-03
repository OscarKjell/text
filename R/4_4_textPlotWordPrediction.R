
#### textWordPredixtion ####

#' Function to compute associated mean value of "x" for each word.
#' @param words Variable of words.
#' @param target_word Word to compute mean value of x for.
#' @param x_value Numeric variable associated to words.
#' @param case_insensitive When TRUE all words are made lower case.
#' @return Mean value of x associated to target word.
#' @importFrom stringi stri_count_regex
#' @noRd
wordsMeanValue <- function(words, target_word, x_value, case_insensitive){
  # Number of time word occurs per row
  word_freq_row <- stringi::stri_count_regex(words, target_word,
                                             case_insensitive = case_insensitive)

  mean_value <- sum(word_freq_row * x_value)/sum(word_freq_row)
  return(mean_value)
}

## Test data
#embeddings_test <- textEmbed(Language_based_assessment_data_8[3])
#words <- Language_based_assessment_data_8$harmonywords
#single_word_embeddings <- embeddings_test$singlewords_we
#x <-  Language_based_assessment_data_8$hilstotal
#y <-  Language_based_assessment_data_8$swlstotal


#' Compute predictions based on single words for plotting words. The word embeddings of
#' single words are trained to predict the mean value associated with that word.
#' @param words Word or text variable to be plotted.
# @param word_embeddings Word embeddings from textEmbed for the words to be plotted
# (i.e., the aggregated word embeddings for the "words" parameter).
#' @param single_word_embeddings Word embeddings from textEmbed for individual words
#' (i.e., decontextualized embeddings).
#' @param x Numeric variable that the words should be plotted according to on the x-axes.
#' @param y Numeric variable that the words should be plotted according to on the y-axes (y=NULL).
#' @param seed Set different seed.
#' @param case_insensitive When TRUE all words are made lower case.
#' @param ... Training options from textTrainRegression().
#' @return A dataframe with variables (e.g., including trained (out of sample) predictions, frequencies, p-values)
#' for the individual words that is used for the plotting in the textProjectionPlot function.
#' @examples
#' # Data
#' # Pre-processing data for plotting
#' \dontrun{
#' df_for_plotting <- textWordPrediction(
#'   words = Language_based_assessment_data_8$harmonywords,
#'   single_word_embeddings = word_embeddings_4$singlewords_we,
#'   x = Language_based_assessment_data_8$hilstotal
#' )
#' df_for_plotting
#' }
#' #' @seealso see \code{\link{textProjection}}
#' @importFrom tibble as_tibble_col as_tibble
#' @importFrom dplyr bind_cols
#' @export
textWordPrediction <- function(words,
                               single_word_embeddings = single_word_embeddings_df,
                               x,
                               y = NULL,
                               seed = 1003,
                               case_insensitive = TRUE,
                               ...){
  set.seed(seed)

  # Description to include as a comment in the end of function
  textWordPrediction_descriptions <- paste(
    "type = textWordPrediction",
    "words =", substitute(words),
    "single_word_embeddings =", comment(single_word_embeddings),
    "x =", substitute(x),
    "y =", substitute(y),
    "case_insensitive =", case_insensitive,
    sep = " ", collapse = " "
  )

  # Sort individual words
  words_sorted_1 <- unique_freq_words(words)


  # Get mean value for each word: Apply function over all words
  mean_word_value_x <- unlist(lapply(words_sorted_1$words, wordsMeanValue,
                                   words=words, x_value=x, case_insensitive = case_insensitive))

  mean_word_value_x <- tibble::as_tibble_col(mean_word_value_x, column_name = "word_mean_value_x")

  #words_sorted_2 <- dplyr::bind_cols(words_sorted_1, mean_word_value_x)

  # Adding mean value of y associated with each word
  if (!is.null(y)){
    mean_word_value_y <- unlist(lapply(words_sorted_1$words, wordsMeanValue,
                                     words=words, x_value=y, case_insensitive = case_insensitive))

    mean_word_value_y <- tibble::as_tibble_col(mean_word_value_y, column_name = "word_mean_value_y")

  }

  # Get word embeddings for each word
  uniques_words_all_wordembedding <- sapply(words_sorted_1$words, applysemrep, single_word_embeddings)
  uniques_words_all_wordembedding <- tibble::as_tibble(t(uniques_words_all_wordembedding))

  # Train model
  model_x <- textTrainRegression(uniques_words_all_wordembedding, mean_word_value_x, ...) #, ...
  embedding_prediction_x <- tibble::as_tibble_col(model_x$predictions$predictions, column_name = "embedding_based_prediction_x")

  # Train model for y-axes
  if (!is.null(y)){
    model_y <- textTrainRegression(uniques_words_all_wordembedding, mean_word_value_y, ...) #, ...
    embedding_prediction_y <- tibble::as_tibble_col(model_y$predictions$predictions, column_name = "embedding_based_prediction_y")
  }

  # TO DO: Compute p-values
  p_value_x <- tibble::as_tibble_col(rep(1, nrow(words_sorted_1)), column_name = "p_value_w_pred_x")

  if (!is.null(y)){
    p_value_y <- tibble::as_tibble_col(rep(1, nrow(words_sorted_1)), column_name = "p_value_w_pred_y")

  }

  #### Sorting output ####

  word_data <- dplyr::bind_cols(words_sorted_1, mean_word_value_x, embedding_prediction_x, p_value_x)

  if (!is.null(y)){
    word_data <- dplyr::bind_cols(word_data, mean_word_value_y, embedding_prediction_y, p_value_y)
  }

  if (is.null(y)){
  output <- list(model_x, word_data)
  names(output) <- c("model_x", "word_data")
  }

  if (!is.null(y)){
    output <- list(model_x, model_y, word_data)
    names(output) <- c("model_x", "model_y", "word_data")
  }

  comment(output) <- textWordPrediction_descriptions

  return(output)
}


# Test
#embeddings_L <- textEmbed(Language_based_assessment_data_3_100)
#
#df_for_plotting_y_L <- textWordPrediction(
#  words = Language_based_assessment_data_3_100$harmonywords,
#  single_word_embeddings = embeddings_L$singlewords_we,
#  x = Language_based_assessment_data_3_100$hilstotal
#  ,y = Language_based_assessment_data_3_100$swlstotal
#)
#
#textPlot(df_for_plotting_y_L,
#         y_axes = TRUE)
#
#
#textPlot(df_for_plotting_y_L,
#         y_axes = TRUE,
#         explore_words = c("happy harmony", "hate disharmony suicide"),
#         explore_words_color = c("green", "red"),
#         explore_words_point = c("ALL_1","ALL_2"))
#
## Pre-processing data for plotting
#library(text)
#df_for_plotting_y <- textWordPrediction(
#  words = Language_based_assessment_data_8$harmonywords,
#  single_word_embeddings = word_embeddings_4$singlewords_we,
#  x = Language_based_assessment_data_8$hilstotal
#  ,y = Language_based_assessment_data_8$hilstotal
#)
#
#help(textPlot)
#textPlot(df_for_plotting_y,
#         y_axes = TRUE,
#         explore_words = c("happy divorce", "calm cock"),
#         explore_words_color = c("#ad42f5", "black"),
#         explore_words_point = c("ALL_1","ALL_2"))
##
##
##df_for_projection_y <- textProjection(
##  words = Language_based_assessment_data_8$harmonywords,
##  word_embeddings = word_embeddings_4$harmonywords,
##  single_word_embeddings = word_embeddings_4$singlewords_we,
##  x = Language_based_assessment_data_8$hilstotal
##  ,y = Language_based_assessment_data_8$swlstotal
##)

#textProjectionPlot(df_for_projection_y, y_axes = TRUE)

#embeddings_test <- textEmbed(Language_based_assessment_data_8[3])

#library(tidyverse)
#word_prediction_test <- textWordPrediction(words = Language_based_assessment_data_8[3],
#                                           #embeddings = embeddings_test[1],
#                                           single_word_embeddings = embeddings_test[2],
#                                           x = Language_based_assessment_data_8[5],
#                                           y = NULL,
#                                           seed = 1003)
#word_prediction_test
#
#colnames(word_prediction_test$word_data)[which(names(word_prediction_test$word_data) == "p_value")] <- "p_values_dot.x"
#colnames(word_prediction_test$word_data)[which(names(word_prediction_test$word_data) == "embedding_based_prediction")] <- "dot.x"
#word_prediction_test$word_data
#
#text::textProjectionPlot(word_prediction_test)
#
#
#help(textEmbed)
#$word_data
# words    n    [dot.x = prediction_score]   p_value
# p_value = permutation type test by creating a NULL distribution of prediction scores from the data.
# For example, randomixe words



