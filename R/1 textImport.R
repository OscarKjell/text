#usethis::use_package("RBERT")
#usethis::use_package("tokenizers")
#usethis::use_package("dplyr")
#browseVignettes("RBERT")
#vignette("RBERT")
#devtools::use_vignette("text")
#usethis::use_vignette("text")
#devtools::document()
#usethis::use_data()
#CHECK: R CMD check is the name of the command you run from the terminal. I don’t recommend calling it directly. Instead, run devtools::check(), or press Cmd + Shift + E in RStudio. In contrast to R CMD check, devtools::check():

### textImport
#library(roxygen2)
########################################################################
########
########      textImport
########
########################################################################
#Importart Tibble so that every character variables get word embeddings
#The function takes all character variables in tibble;
#Split up sentences (NOW ONLY 512 tokens!!!);
#Extract pre-trained BERT-embeddings)
#devtools::load_all()
#devtools::document()
#?textImport
#Fint tags: https://cran.r-project.org/doc/manuals/R-exts.html#Marking-text
#' textImport extracts word embeddings for all text in character variables in dataframe
#'
#' @param x Tibble.
#' @return A list with word embeddings.
#' @examples
#' wordembeddings <- textImport(x)
#' @seealso see \code{\link{textTrain}}, \code{\link{textTtest}} and \code{\link{textSimilarity}}
#' @export

#This version is importing the entire paragraph in one; but only the 512-first tokens.
textImport <- function(df, layer_indexes_IBT = 1:12, batch_size_IBT = 2L, ...){

  # Download pre-trained BERT model. This will go to an appropriate cache
  # directory by default.
  BERT_PRETRAINED_DIR <- RBERT::download_BERT_checkpoint(
    model = "bert_base_uncased")

  #Select all character variables
  df_characters <- dplyr::select_if(df, is.character)
  #Create lists
  BERT_feats <- list()
  output_vectors <- list()
  tokenized_sentences1 <- list()

  #Loop over character variables to tokenize sentences; create BERT-embeddings and Add them to list
  for (i in 1:length(df_characters)) {
    #Tokenize sentences to list
    #    tokenized_sentences1[[i]] <- mapply(tokenize_sentences, df_characters[[i]])
    tokenized_sentences1[[i]] <- df_characters[[i]]
    #Extract BERT feature
    BERT_feats[[i]] <- RBERT::extract_features(
      examples = RBERT::make_examples_simple(tokenized_sentences1[[i]]),
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
  #Gives the names in the list the same name as the orginal character variables
  names(output_vectors) <- names(df_characters)
  output_vectors
}


#This function splits the text up into sentences; and submitts the two first ones to BERT model
textImport2 <- function(df, layer_indexes_IBT = 1:12, batch_size_IBT = 2L, ...){
  # Download pre-trained BERT model. This will go to an appropriate cache
  # directory by default.
  BERT_PRETRAINED_DIR <- RBERT::download_BERT_checkpoint(
    model = "bert_base_uncased")

  #Select all character variables
  df_characters <- dplyr::select_if(df, is.character)
  #Create lists
  BERT_feats <- list()
  output_vectors <- list()
  tokenized_sentences1 <- list()

  #Loop over character variables to tokenize sentences; create BERT-embeddings and Add them to list
  for (i in 1:length(df_characters)) {
    #Tokenize sentences to list
    tokenized_sentences1[[i]] <- mapply(tokenizers::tokenize_sentences, df_characters[[i]])

    #Extract BERT feature
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
  #Gives the names in the list the same name as the orginal character variables
  names(output_vectors) <- names(df_characters)
  output_vectors
}








