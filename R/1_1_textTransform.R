# .rs.restartR()
#  devtools::document()
#' Select all character variables and make then UTF-8 coded, since BERT wants it that way
#'
#' @param tibble including both text and numeric variabes
#' @return all character variables in UTF-8 format.
#' @importFrom dplyr select_if
#' @importFrom tibble as_tibble
#' @importFrom purrr map
#' @importFrom stringi stri_encode
#' @noRd
select_character_v_utf8 <- function(x){
  # Select all character variables
  x_characters <- dplyr::select_if(x, is.character)
  # This makes sure that all variables are UTF-8 coded, since BERT wants it that way
  x_characters <- tibble::as_tibble(purrr::map(x_characters, stringi::stri_encode, "", "UTF-8"))
}

#  devtools::document()
#' Function to take min, max, mean or the CLS (which comes from BERT models; not Static spaces) from list of vectors
#'
#' @param x word embeddings to be aggregated
#' @param aggregation method to carry out the aggreation
#' @return aggreagated word ambeddings.
#' @importFrom stats complete.cases
#' @noRd
#'
textEmbeddingAggregation <- function(x, aggregation = "min"){
  if(aggregation == "min"){
    min_vector <- unlist(map(x, min, na.rm = TRUE))
  } else if (aggregation == "max") {
    max_vector <- unlist(map(x, max, na.rm = TRUE))
  } else if (aggregation == "mean") {
    mean_vector <- colMeans(x, na.rm = TRUE)
  } else if (aggregation == "CLS"){
    CLS <- x %>%
      dplyr::filter(token_index == 1, layer_index == 1)
  } else if (aggregation == "normalize1") {
    #    norma_vector <- unlist(map(x, norma))
    x2 <- x[complete.cases(x), ]
    x3 <- colSums(x2) # BELOW NEED FIXING; REMOVED FROM CRAN
    # x4 <- ppls::normalize.vector(x3)
  }
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

#  devtools::document()
#' semanticrepresentation
#' Function to apply an aggregated semantic representaion for ALL words in a CELL; and if there are no words return a vector with NAs
#' The function is using above applysemrep function.
#' @param x words
#' @param single_wordembeddings Used to get number of dimensions in embedding/space
#' @return semantic representations for all words in cells.
#' @noRd
semanticrepresentation <- function(x, single_wordembeddings2, aggregate = "min", ...) {
  x <- tolower(x)
  # Separates the words in a cell into a character vector with separate words.
  x <- data.frame(unlist(stringr::str_extract_all(x, "[[:alpha:]]+")))
  colnames(x) <- c("wordsAll1")
  x <- as_tibble(x)
  x <- as.character(x$wordsAll1)
  # If empty return a "semantic representation" with NA
  if (length(x) == 0) {
    x2 <- data.frame(matrix(ncol = length(single_wordembeddings2 %>% dplyr::select(dplyr::starts_with("V"))), nrow = 1))
    x2 <- as.numeric(x2)
  } else {
    # Create a matrix with all the semantic representations using the function above
    x1 <- sapply(x, applysemrep, ...)
    x1 <- tibble::as_tibble(t(x1))  ################## This makes it work for textStaticSpace, but might do something bad with other function?
    # If more than one semrep; Sum all the semantic represenations; if not return it as is so that NA etc is returned/kept
    x2 <- textEmbeddingAggregation(x1, aggregation = aggregate) #aggregate
    # If all values are 0 they should be NA instead; otherwise return the semantic representation.
    if (all(x2 == 0|x2 == Inf|x2 == -Inf | is.nan(x2)) == TRUE){
      x2 <- data.frame(matrix(ncol = length(single_wordembeddings2 %>% dplyr::select(dplyr::starts_with("V"))), nrow = 1))
      #OLD:   x2 <- data.frame(matrix(ncol = length(space)-1, nrow = 1))
      x2 <- as.numeric(x2)
    } else {
      x2 <- x2
    }
  }
}

# devtools::document()
#' getUniqueWordsAndFreq
#' Function unites several text variables and rows to one, where all text is tranformed to lowercase and tokenized.
#' Also give word frequencies.
#' @param x A word
#' @return dataframe with unique words and their frequency.
#' @noRd
getUniqueWordsAndFreq <- function(x_characters){
  # Unite all text variables into one
  x_characters2 <- tidyr::unite(x_characters, "x_characters2", 1:ncol(x_characters), sep = " ")
  # unite all rows in the column into one cell
  x_characters3 <- paste(x_characters2[1], collapse = " ")
  # Remove remove all punctuation characters
  x_characters4 <- stringr::str_replace_all(x_characters3, "[[:punct:]]", " ")
  # Remove  \n
  x_characters5 <- gsub("[\r\n]", " ", x_characters4)
  x_characters6 <- gsub("[\n]", " ", x_characters5)
  # Tokenize into single words
  x_characters7 <- tokenizers::tokenize_words(x_characters6, simplify = T)
  # Create dataframe with single words and frequency
  x_characters8 <- data.frame(sort(table(unlist(strsplit(tolower(x_characters7), " ")))))
  singlewords <- tibble(x_characters8$Var1, x_characters8$Freq)
  colnames(singlewords) <- c("words", "n")
  singlewords$words <- as.character(singlewords$words)
  singlewords
}


# textTransform (Installations needed?)
# if error then install: Error in py_run_file_impl(file, local, convert) :  ModuleNotFoundError: No module named 'transformers'
# Reticulate provides a link to python
###install.packages("reticulate")
########### library(reticulate)
########### reticulate::py_install("PyTorch")
########### reticulate::py_install("transformers")
########### torch <- reticulate::import('torch')
########### transformers <- reticulate::import('transformers')

# not needed?: np <- import('numpy')

# By default, reticulate uses the version of Python found on your PATH (i.e. Sys.which("python")).
# Sys.which("python3")

# Python versions discovered on the system:
# py_config()
# You can also use the py_discover_config() function to see what version of Python will be used without actually loading Python:
#cpy_discover_config()

# Needed? : py_install("anaconda")
# use_condaenv("anaconda3")

# Set the path to the Python executable file
# Needed?: use_python("/opt/anaconda3/bin/python3", required = T)

# Check the version of Python.
#py_config()

# Split up sentences (NOW ONLY 512 tokens!);
# devtools::document()
#' textEmbed extracts word embeddings for all text in character variables in dataframe
#'
#' @param x Tibble.
#' @param pretrained_weights Character specifying pre-trained language model through RBERT. Current options
#' "bert_base_uncased", "bert_base_cased", "bert_large_uncased",
#' "bert_large_cased", "bert_large_uncased_wwm", "bert_large_cased_wwm",
#' "bert_base_multilingual_cased", "bert_base_chinese", "scibert_scivocab_uncased",
#' "scibert_scivocab_cased", "scibert_basevocab_uncased", and "scibert_basevocab_cased".
#' @param tokenizer_class tokenizer that match pretrained_weights
#' @param model_class model class that matches pretrained_weights and tokenizer_class
#' @param layers best to extract 'all', and then remove in other function, when aggregating embeddings
#' @param return_tokens f
#' @return A tibble with tokens, layer identifyer and word embeddings. Note that layer 0 is the input embedding to the transformer
# @examples
# x <- sq_data_tutorial8_10[1:2, 1:2]
# wordembeddings <- textEmbedd(x)
#' @seealso see \code{\link{textTrain}}, \code{\link{textDiff}} and \code{\link{textSimilarity}}
#' @importFrom reticulate source_python
#'@importFrom dplyr %>% bind_rows
#' @importFrom tibble tibble as_tibble
#' @importFrom purrr map
#' @importFrom magrittr set_colnames
#' @export
#data = x
#pretrained_weights = 'bert-base-uncased'
#tokenizer_class = BertTokenizer
#model_class = BertModel
#layers = 'all'  # all or a list of layers to keep
#return_tokens = TRUE
textTransform <- function(data,
                      pretrained_weights = 'bert-base-uncased',
                      tokenizer_class = BertTokenizer,
                      model_class = BertModel,
                      layers = 'all',  # all or a list of layers to keep
                      return_tokens = TRUE) {

  # Run python file with HunggingFace interface to state-of-the-art transformers
  reticulate::source_python("R/huggingface_interface.py")

  # Function from python file
  hg_embeddings <- hgTransformerGetEmbedding(text_strings = data,
                                             pretrained_weights = pretrained_weights,
                                             tokenizer_class = tokenizer_class,
                                             model_class = model_class,
                                             layers = layers,  # all or a list of layers to keep
                                             return_tokens = return_tokens)

  # Tidy-structure tokens and embeddings
  # Loop over the variable
  variable_x <- list()
  for(i_in_varialbe in 1:length(x)){   # i_in_varialbe=1; i_in_varialbe=2
    tokens <- hg_embeddings[[2]][[i_in_varialbe]]
    layers <- hg_embeddings[[1]][[i_in_varialbe]]

    # loop of layers
    layers_list <- list()
    for(i in 1:length(layers[[1]])){
      layers_x <- layers[[1]][[i]] # i=1     i=2     i=3

      layers_x_u <- t(dplyr::bind_cols(layers_x)) %>%
        tibble::as_tibble() %>%
        magrittr::set_colnames(c(paste0("Dim", 1:length(layers_x))))

      tokens_lnumber <- tibble::tibble(tokens, rep(i-1, length(tokens)))
      colnames(tokens_lnumber) <- c("tokens", "layer_number")
      tokens_lnumber_layers <- bind_cols(tokens_lnumber, layers_x_u)

      layers_list[[i]] <- tokens_lnumber_layers
      layers_list
    }

    layers_tibble <- dplyr::bind_rows(layers_list)

    variable_x[[i_in_varialbe]] <- layers_tibble
  }

  variable_x

}
#x <-  c("I am here")
#x <-  c("I am here", "where are you")
#textEmbed(x)

#











