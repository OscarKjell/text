#' Tokenize and count
#' @param data (string) Language to tokenise and count.
#' @param n_remove_threshold (numeric) Threshold deciding which words to remove
#' @return A word-frequency data frame (can be saved to a model object or compared in textDomainCompare).
#' @examples
#' \dontrun{
#' textTokenizeAndCount(Language_based_assessment_data_8["harmonytexts"])
#' }
#' @seealso see \code{\link{textDomainCompare}}
#' @importFrom dplyr arrange desc
#' @importFrom tibble as_tibble
#' @export
textTokenizeAndCount <- function(
    data,
    n_remove_threshold = 3) {
  # Automatically identify the single column in the tibble
  text_column <- names(data)[1]

  data <- textCleanNonASCII(data)

  # Tokenize and count word frequency
  word_frequency <- data[[text_column]] %>%
    tolower() %>%                                  # Convert text to lowercase
    strsplit(split = "\\s+") %>%                   # Split text by whitespace
    unlist() %>%                                   # Flatten the list to a vector
    table() %>%                                    # Count occurrences of each word
    as.data.frame(stringsAsFactors = FALSE) %>%    # Convert to a data frame
    setNames(c("word", "n")) %>%                   # Rename columns
    dplyr::arrange(dplyr::desc(n)) %>%                    # Sort by frequency
    dplyr::filter(n >= n_remove_threshold) %>%     # Filter based on threshold
    tibble::as_tibble()

  return(word_frequency)
}


#' Compare two language domains
#' @param train_language A word-frequency data frame from textTokenizeAndCount
#' @param assess_language A word-frequency data frame from textTokenizeAndCount
#' @return List with similarity scores: overlapp_percentage, test_recall_percentage and cosine_similarity
#' @examples
#' \dontrun{
#' training_language <- textTokenizeAndCount(Language_based_assessment_data_8["harmonytexts"])
#' assess_language <- textTokenizeAndCount(Language_based_assessment_data_8["satisfactiontexts"])
#' textDomainCompare(training_language, assess_language)
#' }
#' @seealso see \code{\link{textTokenizeAndCount}}
#' @importFrom dplyr full_join mutate
#' @export
textDomainCompare <- function(
    train_language,
    assess_language){

  # Merge the results for comparison
  word_counts <- dplyr::full_join(
    train_language, assess_language,
    by = "word",
    suffix = c("_train", "_assess"))

  # Replace NAs with 0 for counts
  word_counts[is.na(word_counts)] <- 0

  # Calculate total words for each column
  total_words1 <- sum(word_counts$n_train)
  total_words2 <- sum(word_counts$n_assess)

  # Add proportion columns (fixing the column names here)
  combined_words <- word_counts %>%
    dplyr::mutate(
      proportion_train = n_train / total_words1,
      proportion_assess = n_assess / total_words2)

  # Compute binary presence for both columns
  combined_words <- combined_words %>%
    dplyr::mutate(
      present_in_train = ifelse(n_train > 0, 1, 0),
      present_in_assess = ifelse(n_assess > 0, 1, 0))

  ### Compute similarities

  # Compute intersection (words present in both columns)
  intersection <- sum(
    combined_words$present_in_train == 1 & combined_words$present_in_assess == 1)

  # Compute union (words present in either column)
  union <- sum(
    combined_words$present_in_train == 1 | combined_words$present_in_assess == 1)

  assess <- sum(
    combined_words$present_in_assess == 1)

  # Calculate Jaccard distance
  overlapp_percentage <- (intersection / union)

  test_recall_percentage <- (intersection / assess)


  ### Calculate Cosine Similarity
  # Extract the proportion vectors
  proportion_train <- combined_words$proportion_train
  proportion_assess <- combined_words$proportion_assess

  # Calculate cosine similarity
  cosine_similarity <- sum(proportion_train * proportion_assess) /
    (sqrt(sum(proportion_train^2)) * sqrt(sum(proportion_assess^2)))


  ### Calculate Cosine Similarity with Standardization on Raw Counts
  # Extract the raw count vectors
  raw_train <- combined_words$n_train
  raw_assess <- combined_words$n_assess

  # Standardize the raw counts
  standardized_train <- (raw_train - mean(raw_train)) / sd(raw_train)
  standardized_assess <- (raw_assess - mean(raw_assess)) / sd(raw_assess)

  # Calculate cosine similarity using standardized raw counts
  cosine_similarity_standardised <- sum(standardized_train * standardized_assess) /
    (sqrt(sum(standardized_train^2)) * sqrt(sum(standardized_assess^2)))

  # Return results as a list
  return(list(overlapp_percentage = overlapp_percentage,
              test_recall_percentage = test_recall_percentage,
              cosine_similarity = cosine_similarity,
              cosine_similarity_standardised = cosine_similarity_standardised))
}
