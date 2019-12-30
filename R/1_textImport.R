
### textImport
#library(roxygen2)
########################################################################
########
########      textImport
########
########################################################################
#Importart Tibble so that every character variables get word embeddings
#The function takes all character variables in tibble;
# Split up sentences (NOW ONLY 512 tokens!!!);
# Extract pre-trained BERT-embeddings)
# devtools::load_all()
# devtools::document()
# ?textImport
# Fint tags: https://cran.r-project.org/doc/manuals/R-exts.html#Marking-text
#' textImport extracts word embeddings for all text in character variables in dataframe
#'
#' @param x Tibble.
#' @param layer_indexes_REBERT layers to be used from RBERT (default 12; in REBERT it is 1:12).
#' @param batch_size_IBT batch size from RBERT (default 2L)
#' @param token_index_IBT from RBERT
#' @param layer_index_IBT from RBERT
#' @param ... arguments from RBERT function extract_features
#' @return A list with word embeddings.
#' @examples
#' x <- sq_data_tutorial8_10
#' library(dplyr)
#' wordembeddings <- textImport(x)
#' @seealso see \code{\link{textTrain}}, \code{\link{textTtest}} and \code{\link{textSimilarity}}
#' @importFrom RBERT make_examples_simple
#' @importFrom RBERT make_examples_simple
#' @importFrom dplyr %>%
#' @export


# This version is importing the entire paragraph in one; but only the 512-first tokens.
textImport <- function(x, layer_indexes_REBERT = 12, batch_size_IBT = 2L, token_index_IBT = 1, layer_index_IBT = 12,  ...){

  # Download pre-trained BERT model. This will go to an appropriate cache
  # directory by default.
  BERT_PRETRAINED_DIR <- RBERT::download_BERT_checkpoint(
    model = "bert_base_uncased")

  # Select all character variables
  x_characters <- dplyr::select_if(x, is.character)
  # Create lists
  BERT_feats <- list()
  output_vectors <- list()
  tokenized_sentences1 <- list()

  # Loop over character variables to tokenize sentences; create BERT-embeddings and Add them to list
  for (i in 1:length(x_characters)) {
    # Tokenize sentences to list
    #    tokenized_sentences1[[i]] <- mapply(tokenize_sentences, x_characters[[i]])
    tokenized_sentences1[[i]] <- x_characters[[i]]
    # Extract BERT feature
    BERT_feats[[i]] <- RBERT::extract_features(
      examples = RBERT::make_examples_simple(tokenized_sentences1[[i]]),
      ckpt_dir = BERT_PRETRAINED_DIR,
      layer_indexes = layer_indexes_REBERT,
      batch_size = batch_size_IBT, ...)
    BERT_feats

    # Extract/Sort output vectors for all sentences... These vectors can be used as input features for downstream models.
    # Convenience functions for doing this extraction will be added to the RBERT package in the near future.
    output_vectors[[i]] <- BERT_feats[[i]]$output %>%
      dplyr::filter(token_index == token_index_IBT, layer_index == layer_index_IBT)
    output_vectors
  }
  # Gives the names in the list the same name as the orginal character variables
  names(output_vectors) <- names(x_characters)
  output_vectors
}


# This function SHOULD NOT BE USED as above have been updated; below splits the text up into sentences; and submitts the two first ones to BERT model
textImport2 <- function(x, layer_indexes_IBT = 12, batch_size_IBT = 2L, ...){
  # Download pre-trained BERT model. This will go to an appropriate cache
  # directory by default.
  BERT_PRETRAINED_DIR <- RBERT::download_BERT_checkpoint(
    model = "bert_base_uncased")

  # Select all character variables
  x_characters <- dplyr::select_if(x, is.character)
  #Create lists
  BERT_feats <- list()
  output_vectors <- list()
  tokenized_sentences1 <- list()

  # Loop over character variables to tokenize sentences; create BERT-embeddings and Add them to list
  for (i in 1:length(x_characters)) {
    # Tokenize sentences to list
    tokenized_sentences1[[i]] <- mapply(tokenizers::tokenize_sentences, x_characters[[i]])

    # Extract BERT feature
    BERT_feats[[i]] <- RBERT::extract_features(
      examples = make_examples_simple(tokenized_sentences1[[i]]),
      ckpt_dir = BERT_PRETRAINED_DIR,
      layer_indexes = layer_indexes_IBT,
      batch_size = batch_size_IBT, ...)
    BERT_feats

    # Extract/Sort output vectors for all sentences... These vectors can be used as input features for downstream models.
    # Convenience functions for doing this extraction will be added to the RBERT package in the near future.
    output_vectors[[i]] <- BERT_feats[[i]]$output %>%
      dplyr::filter(token_index == 1, layer_index == 12)
    output_vectors
  }
  # Gives the names in the list the same name as the orginal character variables
  names(output_vectors) <- names(x_characters)
  output_vectors
}








