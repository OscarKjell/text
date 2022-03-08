
#' Compute descriptive statistics of character variables.
#'
#' @param words A character variable; if its a tibble or dataframe, all the character variables will be selected.
#' @param compute_total If words is a tibble or dataframe with several character variables are total variable is computed.
#' That is, the text columns are united using dplyr unite function.
#' @param entropy_unit  The unit entropy is measured. The default is to used bits (i.e., log2; see also, "log", "log10").
#' @param na.rm Option to remove NAs when computing mean, median etc (see under return).
#' @return A tibble with descriptive statistics, including
#' variable = the variable names of input "words";
#' w_total = total number of words in the variable;
#' w_mean = mean number of words in each row of the variable;
#' w_median = median number of words in each row of the variable;
#' w_range_min = smallest number of words of all rows;
#' w_range_max = largest number of words of all rows;
#' w_sd = the standard deviation of the number of words of all rows;
#' unique_tokens = the unique number of tokens (using the word_tokenize function from python package nltk)
#' n_token = number of tokens in the variable (using the word_tokenize function from python package nltk)
#' entropy = the entropy of the variable. It is computed as the Shannon entropy H of a discrete random variable
#' from the specified bin frequencies. (see library entropy and specifically
#' the entropy.plugin function)
#' @examples
#' \dontrun{
#' textDescriptives(Language_based_assessment_data_8[1:2])
#' }
#' @seealso see \code{\link{textEmbed}}
#' @importFrom tibble is_tibble as_tibble_col
#' @importFrom stringi stri_count_words stri_split_regex
#' @importFrom dplyr bind_cols
#' @export
textDescriptives <- function(words,
                             compute_total = TRUE,
                             entropy_unit = "log2",
                             na.rm = TRUE){

  if(tibble::is_tibble(words) | is.data.frame(words)){
    # Select all character variables and make them UTF-8 coded.
    words <- select_character_v_utf8(words)

    #create a total variable help(unite)
    if(compute_total){
      total <- tidyr::unite(words, total, sep = " ")
      words <- dplyr::bind_cols(words, total)
    }

    variable_name <- colnames(words)
    variable_name <- tibble::as_tibble_col(variable_name,
                                           "variable")
  }

  # If words is a vector make it into a tibble format
  if (is.vector(words) == TRUE) {
    variable_name <- deparse(substitute(words))
    variable_name <- tibble::as_tibble_col(variable_name,
                                           "variable")
    words <- tibble::as_tibble_col(words)
    # Select all character variables and make them UTF-8 coded.
    words <- select_character_v_utf8(words)
  }

  word_descriptives <- function(words, entropy_unit){

    # Total words
    w_total <- sum(stringi::stri_count_words(words), na.rm = na.rm)

    # Mean, median SD and range words
    w_mean   <- mean(stringi::stri_count_words(words), na.rm = na.rm)
    w_median <- median(stringi::stri_count_words(words), na.rm = na.rm)
    w_sd     <- sd(stringi::stri_count_words(words), na.rm = na.rm)
    w_range_min <- range(stringi::stri_count_words(words), na.rm = na.rm)[1]
    w_range_max <- range(stringi::stri_count_words(words), na.rm = na.rm)[2]

    # Number of tokens and unique tokens using nltk
    n_tokens <- sum(unique_freq_words(words)$n)
    unique_tokens <- nrow(unique_freq_words(words))

    # Information content
    # Get all words in variables to one cell
    collapse_text <- paste(words, collapse = " ")
    collapse_text <- tolower(collapse_text)

    # Split up each word
    # The default interpretation is a regular expression, as described
    # in stringi::stringi-search-regex. Control options with regex().
    collapse_text <- stringi::stri_split_regex(collapse_text, " ")
    collapse_text_df <- as.data.frame(table(collapse_text))

    collapse_text_df_freqs1 <- collapse_text_df$Freq / sum(collapse_text_df$Freq)
    #collapse_text_df_freqs1 <- freqs.empirical(collapse_text_df$Freq)

    # entropy.empirical estimates the Shannon entropy H of the random
    # variable Y from the corresponding observed counts y by plug-in of the empirical frequencies.
    # entropy estimates the Shannon entropy H of the random variable Y from the corresponding observed counts y.
    # adapted from entropy.plugin from entropy
    entropy_plugin_text <- function (freqs, unit = c("log", "log2", "log10")) {
      unit = match.arg(unit)

      freqs = freqs/sum(freqs)

      H = -sum(ifelse(freqs > 0, freqs * log(freqs), 0))

      if (unit == "log2"){
        H = H/log(2)
      }

      if (unit == "log10"){
        H = H/log(10)
      }
      return(H)
    }

    entropy <- entropy_plugin_text(collapse_text_df_freqs1, unit = entropy_unit)

    #Sorting output
    output_list <- tibble::tibble(w_total, w_mean, w_median, w_range_min, w_range_max, w_sd, unique_tokens, n_tokens, entropy)
  }
  output <- apply(words, 2, word_descriptives, entropy_unit = entropy_unit)
  output <- dplyr::bind_rows(output)
  output <- dplyr::bind_cols(variable_name, output)
  output
}


