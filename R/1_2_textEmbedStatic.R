
#' semanticrepresentation
#' Function to apply an aggregated semantic representation for ALL words in a "CELL";
#' and if there are no words return a vector with NAs. The function is using above
#' applysemrep function.
#' @param x words
#' @param single_word_embeddings2 used to get number of dimensions in embedding/space.
#' @param aggregate Method to aggregate the word embeddings (see mean, min, max and concatenate).
#' @return semantic representations for all words in a cell.
#' @noRd
semanticrepresentation <- function(x, single_word_embeddings2, aggregate = "min", ...) {
  x <- tolower(x)
  # Separates the words in a cell into a character vector with separate words.
  x <- data.frame(unlist(stringi::stri_extract_all(x, regex = "[[:alpha:]]+")))
  colnames(x) <- c("wordsAll1")
  x <- tibble::as_tibble(x)
  x <- as.character(x$wordsAll1)
  # If empty return a "semantic representation" with NA
  if (length(x) == 0) {
    x2 <- data.frame(matrix(ncol = length(single_word_embeddings2 %>%
      dplyr::select(dplyr::starts_with("Dim"))), nrow = 1))
    x2 <- as.numeric(x2)
  } else {
    # Create a matrix with all the semantic representations using the function above
    x1 <- map(x, applysemrep, single_word_embeddings1 = single_word_embeddings2) # , ...
    x2 <- bind_rows(x1)
    # If more than one semrep; Sum all the semantic representations; if not return it as is,
    # so that NAs etc is returned/kept
    x2 <- textEmbeddingAggregation(x2, aggregation = aggregate)
    # If all values are 0 they should be NA instead; otherwise return the semantic representation.
    if (all(x2 == 0 | x2 == Inf | x2 == -Inf | is.nan(x2)) == TRUE) {
      dim_length <- length(single_word_embeddings2 %>%
        dplyr::select(dplyr::starts_with("Dim")))

      x2 <- data.frame(t(rep(NA_real_, dim_length))) # tibble::as_tibble
      colnames(x2) <- paste0("Dim", sep = "", seq_len(dim_length))
      x2
    } else {
      x2 <- x2
      x2
    }
  }
}

#' applysemrep
#' Function to apply the semantic representation (or word embeddings)  to ONE word from
#' a matrix of word embeddings; and return a vector with NA if word is not found.
#' That is, look up word embeddings for each word.
#' @param x A word.
#' @param single_word_embeddings Used to get number of dimensions in embedding/space
#' @return semantic representation (word embedding) from a matrix.
#' @noRd
applysemrep <- function(x, single_word_embeddings1 = single_word_embeddings2) {
  # If semrep is found get it; if not return NA vector of dimensions
  if (sum(single_word_embeddings1$words == x[TRUE]) %in% 1) {
    x <- tolower(x)
    # Get the semantic representation for a word=x
    word1rep <- single_word_embeddings1[single_word_embeddings1$words == x, ]
    # Only get the semantic representation as a vector without the actual word in the first column
    wordrep <- purrr::as_vector(word1rep %>% dplyr::select(dplyr::starts_with("Dim")))
    # If the word does not have a representation return a vector with NAs with the same number
    # of dimensions as columns with Dim
  } else {
    dim_length <- length(single_word_embeddings1 %>%
      dplyr::select(dplyr::starts_with("Dim")))

    wordrep <- data.frame(t(rep(NA_real_, dim_length)))

    colnames(wordrep) <- paste0("Dim", sep = "", seq_len(dim_length))
    wordrep <- purrr::as_vector(wordrep)
  }
}


#' Applies word embeddings from a given decontextualized static space (such as
#' from Latent Semantic Analyses) to all character variables
#'
#' @param df dataframe that at least contains one character column.
#' @param space decontextualized/static space (from textSpace, which is not included in the current text package).
#' @param tk_df default "null"; option to use either the "tk" of "df" space (if using textSpace, which has
#' not been implemented yet).
#' @param aggregate method to aggregate semantic representation when their are more than a single word.
#' (default is "mean"; see also "min" and "max", "concatenate" and "normalize")
#' @return A list with tibbles for each character variable. Each tibble comprises a column with the text, followed by
#' columns representing the semantic representations of the text.
#' The tibbles are called the same as the original variable.
#' @seealso see \code{\link{textEmbed}}
#' @importFrom tibble as_tibble
#' @importFrom dplyr select_if bind_cols
#' @export
textEmbedStatic <- function(df, space, tk_df = "null", aggregate = "mean") {

  # Select the tk or dk matrrix derived from the lsa (svd)
  if (tk_df == "tk") {
    space <- tibble::as_tibble(space$tk, .name_repair = "check_unique")
  } else if (tk_df == "df") {
    space <- tibble::as_tibble(space$df, .name_repair = "check_unique")
  } else if (tk_df == "null") {
    space
  }
  # Select all character variables help(as_tibble)
  df_characters <- dplyr::select_if(df, is.character)

  # Create empty list
  list_semrep <- list()
  # Send the space to Semantic representation function as single_word_embeddings2
  single_word_embeddings2 <- space
  single_word_embeddings1 <- space
  # For loop that apply the semrep to each character variable
  for (i in seq_len(length(df_characters))) {
    # Apply the semantic representation function to all rows; transpose the resulting matrix and making a tibble
    df_output <- data.frame(t(sapply(df_characters[[i]],
      semanticrepresentation,
      single_word_embeddings2,
      aggregate,
      single_word_embeddings1 = single_word_embeddings1
    )))


    list_semrep[[i]] <- df_output %>%
      dplyr::mutate(dplyr::across(dplyr::everything(), unlist)) %>% # dplyr ok?
      tibble::as_tibble(.name_repair = "unique")
  }

  # Add single word embeddings used for plotting
  singlewords <- getUniqueWordsAndFreq(df_characters)
  output_vectors_sw <- map(singlewords$words, applysemrep, single_word_embeddings1)
  names(output_vectors_sw) <- singlewords$words
  output_vectors_sw2 <- dplyr::bind_cols(output_vectors_sw)
  output_vectors_sw3 <- data.frame(t(output_vectors_sw2))
  colnames(output_vectors_sw3) <- paste0("Dim", sep = "", seq_len(ncol(output_vectors_sw3)))

  singlewords_we <- dplyr::bind_cols(singlewords, output_vectors_sw3)

  # Gives the tibbles in the list the same name as the original character variables
  names(list_semrep) <- names(df_characters)
  list_semrep$singlewords_we <- singlewords_we
  list_semrep
}
