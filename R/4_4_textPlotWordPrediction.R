
#### textWordPrediction ####

#' Function to compute associated mean value of "x" for each word.
#' @param words Variable of words.
#' @param target_word Word to compute mean value of x for.
#' @param x_value Numeric variable associated to words.
#' @param case_insensitive When TRUE all words are made lower case.
#' @return Mean value of x associated to target word.
#' @importFrom stringi stri_count_regex
#' @noRd
wordsMeanValue <- function(words, target_word, x_value, case_insensitive) {
  # Number of time word occurs per row
  word_freq_row <- stringi::stri_count_regex(words, target_word,
    case_insensitive = case_insensitive
  )

  mean_value <- sum(word_freq_row * x_value) / sum(word_freq_row)
  return(mean_value)
}


#words = BestVSTop_Tib$allwords
#single_word_embeddings = WE_BestVSTop$singlewords_we
#x = BestVSTop_Tib$dummy
#y = BestVSTop_Tib$dummy
#seed = 1003
#case_insensitive = TRUE




#' Compute predictions based on single words for plotting words. The word embeddings of
#' single words are trained to predict the mean value associated with that word. P-values does
#' NOT work yet.
#' @param words Word or text variable to be plotted.
# @param word_embeddings Word embeddings from textEmbed for the words to be plotted
# (i.e., the aggregated word embeddings for the "words" parameter).
#' @param single_word_embeddings Word embeddings from textEmbed for individual words
#' (i.e., decontextualized embeddings).
#' @param x Numeric variable that the words should be plotted according to on the x-axes.
#' @param y Numeric variable that the words should be plotted according to on the y-axes (y=NULL).
#' @param seed Set different seed.
#' @param case_insensitive When TRUE all words are made lower case.
#' @param text_remove Remove special characters
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
                               text_remove = "[()]",
                               ...) {
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

  text_remove <- "[()]"
  #Remove brackets
  words_sorted_1 <- words_sorted_1[!grepl(pattern = text_remove,
                                          x = words_sorted_1$words)==TRUE,]

  # Get mean value for each word: Apply function over all words
  mean_word_value_x <- unlist(lapply(words_sorted_1$words, wordsMeanValue,
    words = words, x_value = x, case_insensitive = case_insensitive
  ))

  mean_word_value_x <- tibble::as_tibble_col(mean_word_value_x, column_name = "word_mean_value_x")
  words_mean_x <- dplyr::bind_cols(words_sorted_1, mean_word_value_x)
  words_mean_x <- words_mean_x[complete.cases(words_mean_x$word_mean_value_x),]
  # Adding mean value of y associated with each word
  if (!is.null(y)) {


    mean_word_value_y <- unlist(lapply(words_sorted_1$words, wordsMeanValue,
      words = words, x_value = y, case_insensitive = case_insensitive
    ))

    mean_word_value_y <- tibble::as_tibble_col(mean_word_value_y, column_name = "word_mean_value_y")

    words_mean_y <- dplyr::bind_cols(words_sorted_1, mean_word_value_y)
    words_mean_y <- words_mean_y[complete.cases(words_mean_y$word_mean_value_y),]

  }

  # Get word embeddings for each word library(tidyverse)
  uniques_words_all_wordembedding <- sapply(words_mean_x$words, applysemrep, single_word_embeddings)
  uniques_words_all_wordembedding <- tibble::as_tibble(t(uniques_words_all_wordembedding))

  # Train model
  model_x <- textTrainRegression(uniques_words_all_wordembedding,
                                 words_mean_x$word_mean_value_x) #, ...

  embedding_prediction_x <- tibble::as_tibble_col(model_x$predictions$predictions,
                                                  column_name = "embedding_based_prediction_x")

  # Train model for y-axes
  if (!is.null(y)) {
    model_y <- textTrainRegression(uniques_words_all_wordembedding,
                                   words_mean_y$word_mean_value_y) #, ...

    embedding_prediction_y <- tibble::as_tibble_col(model_y$predictions$predictions,
                                                    column_name = "embedding_based_prediction_y")
  }

  # TO DO: Compute p-values
  p_value_x <- tibble::as_tibble_col(rep(1, nrow(words_mean_x)), column_name = "p_value_w_pred_x")

  if (!is.null(y)) {
    p_value_y <- tibble::as_tibble_col(rep(1, nrow(words_mean_y)), column_name = "p_value_w_pred_y")
  }

  #### Sorting output ####
  #words_mean_y
  #word_data <- dplyr::bind_cols(words_sorted_1, mean_word_value_x, embedding_prediction_x, p_value_x)
  word_data <- dplyr::bind_cols(words_mean_x, embedding_prediction_x, p_value_x)
  if (!is.null(y)) {
    word_data <- dplyr::bind_cols(words_mean_y, embedding_prediction_y, p_value_y)
  }

  if (is.null(y)) {
    output <- list(model_x, word_data)
    names(output) <- c("model_x", "word_data")
  }

  if (!is.null(y)) {
    output <- list(model_x, model_y, word_data)
    names(output) <- c("model_x", "model_y", "word_data")
  }

  comment(output) <- textWordPrediction_descriptions

  return(output)
}
