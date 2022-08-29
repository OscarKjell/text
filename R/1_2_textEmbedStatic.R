#x <- df_characters[[i]][1]

#' semanticrepresentation
#' Apply an aggregated semantic representation for ALL words in a "CELL";
#' and if there are no words return a vector with NAs. The function is using
#' applysemrep function.
#' @param x (string) Character words
#' @param single_word_embeddings2 used to get number of dimensions in embedding/space.
#' @param aggregation_from_tokens_to_texts Method to aggregate the word embeddings (see mean, min, max and concatenate).
#' @param tolower (boolean) Lower case x.
#' @return semantic representations for all words in a cell.
#' @importFrom purrr map
#' @importFrom dplyr starts_with select
#' @noRd
semanticrepresentation <- function(x,
                                   single_word_embeddings2,
                                   aggregation_from_tokens_to_texts = "min",
                                   tolower = TRUE, ...) {
  if(tolower) x <- tolower(x)
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
    # Create a matrix with all the semantic representations using the applysemrep function
    x1 <- purrr::map(x, applysemrep,
                     single_word_embeddings1 = single_word_embeddings2,
                     tolower = tolower) #
    x2 <- dplyr::bind_rows(x1)
    # If more than one semrep; Sum all the semantic representations; if not return it as is,
    # so that NAs etc is returned/kept
    x2 <- textEmbeddingAggregation(x2, aggregation = aggregation_from_tokens_to_texts)
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

#x = text_tokens[[1]][[1]][[2]]
#x = "adsfdg"
#x = text_tokens[[1]][[1]][[1]]
#single_word_embeddings1 = decontext_space
#decontext_space$words

#' applysemrep
#' Function to apply the semantic representation (or word embeddings)  to ONE word from
#' a matrix of word embeddings; and return a vector with NA if word is not found.
#' That is, look up word embeddings for each word.
#' @param x A word.
#' @param single_word_embeddings Used to get number of dimensions in embedding/space
#' @param tolower (boolean) Lower case input.
#' @return semantic representation (word embedding) from a matrix.
#' @importFrom purrr as_vector
#' @importFrom dplyr select starts_with
#' @noRd
applysemrep <- function(x,
                        single_word_embeddings1 = single_word_embeddings2,
                        tolower = TRUE) {
  # If semrep is found get it; if not return NA vector of dimensions
  if (sum(single_word_embeddings1$words == x[TRUE]) %in% 1) {
    if(tolower) x <- tolower(x)
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


#' applysemrep
#' Function to apply the semantic representation (or word embeddings)  to ONE word from
#' a matrix of word embeddings; and return a vector with NA if word is not found.
#' That is, look up word embeddings for each word.
#' @param word_col A tibble/dataframe column with words.
#' @param space A dataframe with words in the first column and their word
#' embeddings in the rest of the columns.
#' @param tolower (boolean) Lower case input.
#' @return (tibble) Semantic representation (word embeddings) from space for all the words in a column.
#' @importFrom purrr map
#' @importFrom dplyr bind_cols
#' @importFrom tibble as_tibble
#' @noRd
applysemrep_over_words <- function(word_col,
                                   space,
                                   tolower = TRUE){

  output_vectors_sw <- purrr::map(word_col[[1]][[1]],
                                  applysemrep,
                                  single_word_embeddings1 = space,
                                  tolower = tolower)

  names(output_vectors_sw) <- paste("a", seq_len(length(output_vectors_sw)))
  output_vectors_sw2 <- dplyr::bind_cols(output_vectors_sw)
  output_vectors_sw3 <- data.frame(t(output_vectors_sw2))
  colnames(output_vectors_sw3) <- paste0("Dim", sep = "", seq_len(ncol(output_vectors_sw3)))
  output_vectors_sw3 <- tibble::as_tibble(output_vectors_sw3)

  return(output_vectors_sw3)
}




#' Applies word embeddings from a given decontextualized static space (such as
#' from Latent Semantic Analyses) to all character variables
#'
#' @param df dataframe that at least contains one character column.
#' @param space decontextualized/static space with a column called "words" and the semantic
#' representations are in columns called Dim1, Dim2 (or V1, V2, ...) and so on (from textSpace,
#' which is not included in the current text package).
#' @param tk_df default "null"; option to use either the "tk" of "df" space (if using textSpace, which has
#' not been implemented yet).
#' @param aggregation_from_tokens_to_texts method to aggregate semantic representation when their are more than a single word.
#' (default is "mean"; see also "min" and "max", "concatenate" and "normalize")
#' @param dim_name Boolean, if TRUE append the variable name after all variable-names in the output.
#' (This differentiates between word embedding dimension names; e.g., Dim1_text_variable_name)
#' @param tolower (boolean) Lower case input.
#' @return A list with tibbles for each character variable. Each tibble comprises a column with the text, followed by
#' columns representing the semantic representations of the text.
#' The tibbles are called the same as the original variable.
#' @seealso see \code{\link{textEmbed}}
#' @importFrom tibble as_tibble
#' @importFrom dplyr select_if bind_cols
#' @importFrom purrr map
#' @export
textEmbedStatic <- function(df,
                            space,
                            tk_df = "null",
                            aggregation_from_tokens_to_texts = "mean",
                            dim_name = FALSE,
                            tolower = FALSE) {

  # Select the tk or dk matrix derived from the lsa (svd)
  if (tk_df == "tk") {
    # If variable names start with V (as in V1) rename to Dim1
    colnames(space$tk) <- sub("V", "Dim", colnames(space$tk))
    space <- tibble::as_tibble(space$tk, .name_repair = "check_unique")
  } else if (tk_df == "df") {
    # If variable names start with V (as in V1) rename to Dim1
    colnames(space$df) <- sub("V", "Dim", colnames(space$df))
    space <- tibble::as_tibble(space$df, .name_repair = "check_unique")
  } else if (tk_df == "null") {
    space <- space
  }

  # Select all character variables
  df_characters <- dplyr::select_if(df, is.character)

  # Create empty list
  list_semrep <- list()
  # Send the space to Semantic representation function as single_word_embeddings2
  single_word_embeddings2 <- space
  single_word_embeddings1 <- space
  # For loop that apply the semrep to each character variable i = 1 help(sapply)
  for (i in seq_len(length(df_characters))) {
    # Apply the semantic representation function to all rows; transpose the resulting matrix and making a tibble
    df_output <- data.frame(t(sapply(df_characters[[i]],
      semanticrepresentation,
      single_word_embeddings2,
      aggregation_from_tokens_to_texts,
      single_word_embeddings1 = single_word_embeddings1
    )))


    list_semrep[[i]] <- df_output %>%
      dplyr::mutate(dplyr::across(dplyr::everything(), unlist)) %>%
      tibble::as_tibble(.name_repair = "unique")

    if (dim_name == TRUE) {
      colnames(list_semrep[[i]]) <- paste0(
        colnames(list_semrep[[i]]),
        "static",
        "_",
        names(df_characters[i])
      )
    }
  }

  # Add single word embeddings used for plotting
  singlewords <- getUniqueWordsAndFreq(df_characters, hg_tokenizer = FALSE)
  output_vectors_sw3 <- applysemrep_over_words(word_col = list(singlewords),
                                               space =  single_word_embeddings1,
                                               tolower = tolower)
  word_types <- dplyr::bind_cols(singlewords, output_vectors_sw3)

  # Gives the tibbles in the list the same name as the original character variables
  names(list_semrep) <- names(df_characters)
  list_semrep$word_types <- word_types
  list_semrep
}
