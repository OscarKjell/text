

########################################################################
########
########      textImport
########
########################################################################

## HAVE TO LOOK CLOSER SO THAT THE EXTRACTION FROM RBERT MAKES SENSE

# Split up sentences (NOW ONLY 512 tokens!);
# Extract pre-trained BERT-embeddings)
# devtools::document()
#' textImport extracts word embeddings for all text in character variables in dataframe
#'
#' @param x Tibble.
#' @param layer_indexes_RBERT layers to be used from RBERT (default 12; in REBERT it is 1:12).
#' @param batch_size_IBT batch size from RBERT (default 2L)
#' @param token_index_IBT from RBERT
#' @param layer_index_IBT from RBERT
#' @param ... arguments from RBERT function extract_features
#' @return A list with word embeddings.
#' @examples
#' x <- sq_data_tutorial8_10[1:2, 1:2]
#' library(dplyr)
#' wordembeddings <- textImport(x)
#' @seealso see \code{\link{textTrain}}, \code{\link{textTtest}} and \code{\link{textSimilarity}}
#' @importFrom RBERT make_examples_simple
#' @importFrom RBERT make_examples_simple
#' @importFrom dplyr %>%
#' @importFrom tidyr unite
#' @importFrom tokenizers tokenize_words
#' @importFrom stringr str_replace_all
#' @importFrom tibble tibble
#' @importFrom purrr map
#' @importFrom stringi stri_encode
#' @export

#x <- sq_data_tutorial[1:2, 1:2]
#x_test <- textImport(x)
#x_test

# This version is importing the entire cell/response/paragraph in one; but only the 512-first tokens.
# However, should make one that take in individual words without context; and another taking in sentences that are summed up?
textImport <- function(x, layer_indexes_RBERT = 12, batch_size_IBT = 2L, token_index_IBT = 1, layer_index_IBT = 12,  ...){

  # Download pre-trained BERT model. This will go to an appropriate cache
  # directory by default.
  BERT_PRETRAINED_DIR <- RBERT::download_BERT_checkpoint(
    model = "bert_base_uncased")

  # Select all character variables
  x_characters <- dplyr::select_if(x, is.character)
  # This makes sure that all variables are UTF-8 coded, since BERT wants it that way
  x_characters <- tibble::as_tibble(purrr::map(x_characters, stri_encode, "", "UTF-8"))
  # Create lists
  BERT_feats <- list()
  output_vectors <- list()
  tokenized_sentences1 <- list()

  # Loop over character variables to tokenize sentences; create BERT-embeddings and Add them to list
  for (i in 1:length(x_characters)) {
    # Tokenize sentences to list
    tokenized_sentences1[[i]] <- x_characters[[i]]
    # Extract BERT feature
    BERT_feats[[i]] <- RBERT::extract_features(
      examples = RBERT::make_examples_simple(tokenized_sentences1[[i]]),
      ckpt_dir = BERT_PRETRAINED_DIR,
      layer_indexes = layer_indexes_RBERT,
      batch_size = batch_size_IBT, ...) #
    BERT_feats

    # Extract/Sort output vectors for all sentences... These vectors can be used as input features for downstream models.
    # Convenience functions for doing this extraction will be added to the RBERT package in the near future.
    output_vectors[[i]] <- BERT_feats[[i]]$output %>%
      dplyr::filter(token_index == token_index_IBT, layer_index == layer_index_IBT)
    output_vectors
  }
  # Gives the names in the list the same name as the orginal character variables
  names(output_vectors) <- names(x_characters)


  #Single words' word embeddings for semantic plots

  # Get word-embeddings for all individual-words (which is used for the word plot)
  #Unite all text variables into one
  x_characters2 <- tidyr::unite(x_characters, "x_characters2", 1:ncol(x_characters), sep = " ")
  # unite all rows in the column into one cell
  x_characters3 <- paste(x_characters2[1], collapse = ' ')
  #Remove remove all punctuation characters
  x_characters4 <- stringr::str_replace_all(x_characters3, "[[:punct:]]", " ")
  #Remove  \n
  x_characters5 <- gsub("[\r\n]", " ", x_characters4)
  x_characters6 <- gsub("[\n]", " ", x_characters5)
  #Tokenize into single words
  x_characters7 <- tokenizers::tokenize_words(x_characters6, simplify=T)
  #Create datafrema with single words and frequency
  x_characters8 <- data.frame(sort(table(unlist(strsplit(tolower(x_characters7), " ")))))
  singlewords <- tibble(x_characters8$Var1, x_characters8$Freq)
  colnames(singlewords) <- c("words", "n")
  singlewords$words <- as.character(singlewords$words)


  # Tokenize sentences to list
  tokenized_sentences1_sw <- singlewords$words
  # Extract BERT feature
  BERT_feats_sw <- RBERT::extract_features(
    examples = RBERT::make_examples_simple(tokenized_sentences1_sw),
    ckpt_dir = BERT_PRETRAINED_DIR,
    layer_indexes = layer_indexes_RBERT,
    batch_size = batch_size_IBT, ...) #
  BERT_feats_sw

  # Extract/Sort output vectors for all sentences... These vectors can be used as input features for downstream models.
  # Convenience functions for doing this extraction will be added to the RBERT package in the near future.
  output_vectors_sw <- BERT_feats_sw$output %>%
    dplyr::filter(token_index == token_index_IBT, layer_index == layer_index_IBT)
    #Add frequency for each word
  singlewords_we1 <- cbind(singlewords, output_vectors_sw)
  singlewords_we <- tibble::as_tibble(singlewords_we1)
  # Add the single words embeddings
  output_vectors$singlewords_we <- singlewords_we
  output_vectors
}



# Below are two helper functions not exported for users of the package
# embeddings <- wordembeddings4_10[1]
# words <- c("happy", "harmony", "joy", "sad", "cry", "ad", "afdg", "adgh", "asdfg", "age")
# single_wordembeddings_df <- cbind(words, embeddings$harmonywords)
# single_wordembeddings_df <- as_tibble(single_wordembeddings_df)
# devtools::document()
#' applysemrep
#' Function to apply the semantic representation to ONE word from a matrix of semreps; and
#' return vector with NA if word is not found.
#' That is, look up word embeddings for each word in output_vectors_sw
#' @param x A word
#' @param single_wordembeddings Used to get number of dimensions in embedding/space
#' @return semantic representation from a matrix.
#' @noRd
# x="happy"
# single_wordembeddings_df
applysemrep <- function(x, single_wordembeddings1){
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







#applysemrep <- function(x, single_wordembeddings_df, Ndim){
#  #If semrep is found get it; if not return NA vector of dimensions
#  if (sum(single_wordembeddings_df$words == x[TRUE]) %in% 1) {
#    x <- tolower(x)
#    #Get the semantic representation for a word=x
#    word1rep <- single_wordembeddings_df[single_wordembeddings_df$words ==x, ]
#    #Only get the semantic represenation as a vector without the actual word in the first column
#    wordrep <- purrr::as_vector(word1rep[,8:length(word1rep)])
#
#    #If the word does not have a semrep return vector with Ndim (512) dimensions of NA; Ndim=768
#  }else if (x %in% NA) {
#    wordrep <- data.frame(matrix(ncol = Ndim, nrow = 1))
#    class(wordrep)
#    wordrep <- as.numeric(wordrep)
#  } else {
#    wordrep <- data.frame(matrix(ncol = Ndim, nrow = 1))
#    wordrep <- as.numeric(wordrep)
#  }
#}

#  devtools::document()
#' semanticrepresentation
#' Function to apply a common semantic representaion for ALL words in a CELL; and if there are no words return a vector with NAs
#' The function is using above applysemrep function.
#' @param x words
#' @param single_wordembeddings Used to get number of dimensions in embedding/space
#' @return semantic representations for all words in cells.
#' @noRd
semanticrepresentation <- function(x, single_wordembeddings2, ...) {
  x <- tolower(x)
  # Separates the words in a cell into a character vector with separate words.
  x <- data.frame(unlist(stringr::str_extract_all(x, "[[:alpha:]]+")))
  colnames(x) <- c("wordsAll1")
  x <- as_tibble(x)
  x <- as.character(x$wordsAll1)
  # If empty return a "semantic representation" with NA
  if (length(x)== 0) {
    x2 <- data.frame(matrix(ncol = length(single_wordembeddings2 %>% dplyr::select(dplyr::starts_with("V"))), nrow = 1))
    x2 <- as.numeric(x2)
  }else{
    # Create a matrix with all the semantic representations using the function above
    x1 <-  sapply(x, applysemrep, ...)
    # If more than one semrep; Sum all the semantic represenations; if not return it as is so that NA etc is returned/kept
    x2 <- Matrix::rowSums(x1, na.rm=TRUE)
    # If all values are 0 they should be NA instead; otherwise return the semantic representation.
    if (all(x2 == 0)== TRUE){
      x2 <- data.frame(matrix(ncol = length(single_wordembeddings2 %>% dplyr::select(dplyr::starts_with("V"))), nrow = 1))
      x2 <- as.numeric(x2)
    }else{
      x2 <- x2
    }
  }
}




# This function gets DECONTEXTUALISED word embeddings for single words;
# and then adds them together to represent the
# common word embedding for X descriptive words in a cell.

## HAVE TO LOOK CLOSER SO THAT THE EXTRACTION FROM RBERT MAKES SENSE

# devtools::document()
#' textImportWords extracts word embeddings on single words basis and adds them together
#' for all character variables in given tibble
#'
#' @param x Tibble.
#' @param layer_indexes_RBERT layers to be used from RBERT (default 12; in REBERT it is 1:12).
#' @param batch_size_IBT batch size from RBERT (default 2L)
#' @param token_index_IBT from RBERT
#' @param layer_index_IBT from RBERT
#' @param ... arguments from RBERT function extract_features
#' @return A list with word embeddings.
#' @examples
#' x <- sq_data_tutorial8_10[1:2, 1:2]
#' library(dplyr)
#' wordembeddings <- textImportWords(x)
#' @seealso see \code{\link{textImport}}, \code{\link{textTtest}} and \code{\link{textSimilarity}}
#' @importFrom RBERT make_examples_simple
#' @importFrom RBERT make_examples_simple
#' @importFrom dplyr %>%
#' @importFrom tidyr unite
#' @importFrom tokenizers tokenize_words
#' @importFrom stringr str_replace_all
#' @importFrom tibble tibble
#' @importFrom purrr as_vector
#' @importFrom Matrix rowSums
#' @export

# x <- sq_data_tutorial8_10[1:2, 1:2]

# General description. textImportWords is first getting the word embedding for individuals words in a dataset;
# then these word embeddings are attached to the data (i.e., this is to avoid to call for the same word-embeddings
# several time)

textImportWords <- function(x, layer_indexes_RBERT = 12, batch_size_IBT = 2L, token_index_IBT = 1,
                            layer_index_IBT = 12, ...){

  # Download pre-trained BERT model. This will go to an appropriate cache
  # directory by default.
  BERT_PRETRAINED_DIR <- RBERT::download_BERT_checkpoint(
    model = "bert_base_uncased")

  # Select all character variables
  x_characters <- dplyr::select_if(x, is.character)

  #Single words' word embeddings
  # Get word-embeddings for all individual-words

  # Unite all text variables into one
  x_characters2 <- tidyr::unite(x_characters, "x_characters2", 1:ncol(x_characters), sep = " ")
  # Unite all rows in the column into one cell
  x_characters3 <- paste(x_characters2[1], collapse = ' ')
  # Remove all punctuation characters
  x_characters4 <- stringr::str_replace_all(x_characters3, "[[:punct:]]", " ")
  # Remove "\n"
  x_characters5 <- gsub("[\r\n]", " ", x_characters4)
  x_characters6 <- gsub("[\n]", " ", x_characters5)
  # Tokenize into single words
  x_characters7 <- tokenizers::tokenize_words(x_characters6, simplify=T)
  # Create dataframe with single words and frequency
  x_characters8 <- data.frame(sort(table(unlist(strsplit(tolower(x_characters7), " ")))))
  singlewords <- tibble(x_characters8$Var1, x_characters8$Freq)
  colnames(singlewords) <- c("words", "n")
  singlewords$words <- as.character(singlewords$words)

  # Tokenize words to list
  tokenized_sentences1_sw <- singlewords$words
  # Extract BERT feature
  BERT_feats_sw <- RBERT::extract_features(
    examples = RBERT::make_examples_simple(tokenized_sentences1_sw),
    ckpt_dir = BERT_PRETRAINED_DIR,
    layer_indexes = layer_indexes_RBERT,
    batch_size = batch_size_IBT, ...)
  BERT_feats_sw

  # Extract/Sort output vectors for all words.
  output_vectors_sw <- BERT_feats_sw$output %>%
    dplyr::filter(token_index == token_index_IBT, layer_index == layer_index_IBT)
  # Add frequency for each word
  singlewords_we1 <- cbind(singlewords, output_vectors_sw)
  single_wordembeddings_df3 <- tibble::as_tibble(singlewords_we1)
  # Add the single words embeddings
  output_vectors_sw <- list()
  output_vectors_sw$single_wordembeddings_df3 <- single_wordembeddings_df3
  output_vectors_sw

  # The semanticrepresentation function is now looped over all variables in x_characters
  # Creating empty list
  list_semrep <- list()
  # For loop that apply the word embeddings to each character variable
  for (i in 1:length(x_characters)) {
    # Apply the semanticrepresentation function to all rows; transpose the resulting matrix and making a tibble
    list_semrep[[i]] <- as_tibble(t(sapply(x_characters[[i]], semanticrepresentation, single_wordembeddings2 = output_vectors_sw$single_wordembeddings_df3, single_wordembeddings1 = output_vectors_sw$single_wordembeddings_df3, ...)))
  }

  # Gives the tibbles in the list the same name as the orginal character variables
  names(list_semrep) <- names(x_characters)
  list_semrep
}

#library(tidyverse)
#x <- sq_data_tutorial8_10[1:2, 1:2]
#decontext_we3 <- textImportWords(x)
#decontext_we3
# decontext_we <- textImport(x)
#


























