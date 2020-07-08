
# library(text)
# .rs.restartR()
# not needed?: np <- import('numpy')
# textTransform (Installations needed?)
# if error then install: Error in py_run_file_impl(file, local, convert) :  ModuleNotFoundError: No module named 'transformers'
# Reticulate provides a link to python
###install.packages("reticulate")
########### library(reticulate)
########### reticulate::py_install("PyTorch")
########### reticulate::py_install("transformers")
########### torch <- reticulate::import('torch')
########### transformers <- reticulate::import('transformers')

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
# py_config()



#  devtools::document()
#' Testing F1 function from python
#'
#' @return "f one"
#' @export
f1_from_python <- function(){
  f1()
}
# f1_from_python()

#  devtools::document()
#' Select all character variables and make them UTF-8 coded, since BERT wants it that way
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
    wordrep <- purrr::as_vector(word1rep %>% dplyr::select(dplyr::starts_with("Dim")))
    # If the word does not have a semrep return vector with NA the same number of dimensions as columns with Dim
  } else if (x %in% NA) {
    # The length() refers to how many column starts with Dim (i.e., how many dimensions)
    wordrep <- data.frame(matrix(ncol = length(single_wordembeddings1 %>% dplyr::select(dplyr::starts_with("Dim"))), nrow = 1))
    class(wordrep)
    wordrep <- as.numeric(wordrep)
  } else {
    wordrep <- data.frame(matrix(ncol = length(single_wordembeddings1 %>% dplyr::select(dplyr::starts_with("Dim"))), nrow = 1))
    colnames(wordrep) <- paste0("Dim", sep="", 1:length(wordrep))
    #wordrep1 <- is.numeric(wordrep)
    #wordrep <- tibble_cols(wordrep)
    wordrep
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
    x2 <- data.frame(matrix(ncol = length(single_wordembeddings2 %>% dplyr::select(dplyr::starts_with("Dim"))), nrow = 1))
    x2 <- as.numeric(x2)
  } else {
    # Create a matrix with all the semantic representations using the function above
    x1 <- sapply(x, applysemrep, ...)
    x1 <- tibble::as_tibble(t(x1))  ################## This makes it work for textStaticSpace, but might do something bad with other function?
    # If more than one semrep; Sum all the semantic represenations; if not return it as is so that NA etc is returned/kept
    x2 <- textEmbeddingAggregation(x1, aggregation = aggregate) #aggregate
    # If all values are 0 they should be NA instead; otherwise return the semantic representation.
    if (all(x2 == 0|x2 == Inf|x2 == -Inf | is.nan(x2)) == TRUE){
      x2 <- data.frame(matrix(ncol = length(single_wordembeddings2 %>% dplyr::select(dplyr::starts_with("Dim"))), nrow = 1))
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
  # Remove all punctuation characters
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

# devtools::document()
#' sortingLayers
#' This is a function that sorts out the embeddings (and is used again below for decontextualised words)
#' @param x List of layers
#' @param layers The number of layers to get (setting comes from textHuggingFace)
#' @param return_tokens bolean wther tokens have been returened (setting comes from textHuggingFace)
#' @return Layers in tidy tibble format with each dimension column caled Dim1, Dim2 etc.
#' @noRd
sortingLayers <- function(x, layers = layers, return_tokens = return_tokens){
  # If selecting "all" layers, find out number of layers to help indicate layer index later in code
  if(is.character(layers)) {
    layers <- 0:(length(x[[1]][[1]])-1)
  }

  # Find Number Dimensions (where the place differ depending on return_token is TRUE or FALSE)
  if(return_tokens){
    dimensions <- length(x[[1]][[1]][[1]][[1]][[1]])
    participants <- length(x[[1]])
  }else{
    dimensions <- length(x[[1]][[1]][[1]][[1]])
    participants <- length(x)
  }

  # Tidy-structure tokens and embeddings
  # Loop over the cases in the variable
  variable_x <- list()
  for(i_in_variable in 1:participants){   # 3 i_in_variable=1; i_in_variable=2

    if(return_tokens){
      tokens <- x[[2]][[i_in_variable]]
      all_layers <- x[[1]][[i_in_variable]]
    }else{
      tokens <- NULL
      all_layers <- x[[i_in_variable]]
    }

    # Loop of the number of layers
    layers_list <- list()
    for(i_layers in 1:length(all_layers)){ # 2 i_layers = 1
      i_layers_for_tokens <- all_layers[i_layers]

      # Transpose layers and give each column a DimX names library(tidyverse)
      # layers_4_token <- suppressMessages(t(dplyr::bind_cols(i_layers_for_tokens))) %>%
      layers_4_token <- suppressMessages(t(dplyr::bind_cols(i_layers_for_tokens))) %>%
        magrittr::set_colnames(c(paste0("Dim", 1:dimensions))) #%>%
      layers_4_token <- tibble::as_tibble(layers_4_token)

      if(return_tokens){
        tokens_layer_number <- tibble::tibble(tokens, rep(layers[i_layers], length(tokens)))
        colnames(tokens_layer_number) <- c("tokens", "layer_number")
        tokens_lnumber_layers <- bind_cols(tokens_layer_number, layers_4_token)
        #tokens_lnumber_layers_true <- tokens_lnumber_layers
      }else{
        layer_number <- tibble::tibble(rep(layers[i_layers], nrow(layers_4_token))) # 11; 14
        colnames(layer_number) <- c("layer_number")
        tokens_lnumber_layers <- bind_cols(layer_number, layers_4_token)
      }

      layers_list[[i_layers]] <- tokens_lnumber_layers
      layers_list
    }
    layers_tibble <- dplyr::bind_rows(layers_list)

    variable_x[[i_in_variable]] <- layers_tibble
  }
  variable_x
}

# devtools::document()
#' grep_col_by_name_in_list
#' This function finds a column by name even independent on where in the list structure.
#' @param l List
#' @param pattern to find; such as "layers_number" column
#' @return elements in the column called pattern.
#' @noRd
grep_col_by_name_in_list <- function(l, pattern) {
  u <- unlist(l)
  u[grep(pattern, names(u))]
}


# devtools::document()
# setting_up_model_tokenizer_weights
# This function sets up the specific of the models; the parameters for HuggingFace.
# @param model A characther string naming the model according to HuggingFace's way of aning the pretrained weights.
# @return whcih pretrained_weights, tokenizer_class and model_class to use.
# @noRd
#setting_up_model_tokenizer_weights <- function(model)


#library(text)
# Split up sentences (NOW ONLY 512 tokens!); # TODO: Add function in case there are more than 512 tokens it needs to split up.
#x <-  c("harmony", "I'm harmonious.")
#x <- Language_based_assessment_data_8_10[1:2, 1]
#
#contexts = TRUE
#decontexts = TRUE
#pretrained_weights = 'bert-base-uncased'
#tokenizer_class = BertTokenizer
#model_class = BertModel
#layers = 'all'  # all or a list of layers to keep
#return_tokens = TRUE

# devtools::document()
#' Extract layers of hidden states (word embeddings) for all character variables in the dataframe
#'
#' @param x Tibble/dataframe with at least one character variable.
#' @param contexts Provide word embeddings based on word contexts (standard method; default = TRUE).
#' @param decontexts Provide word embeddings of single words as input (embeddings used for plotting; default = TRUE).
#' @param model Character strinng specifying pre-trained language model. Default 'bert-base-uncased'; options "bert-base-multilingual-uncased", "bert-base-multilingual-cased", "openai-gpt",
#' "gpt2", "ctrl", "transfo-xl-wt103", "xlnet-base-cased", "xlm-mlm-enfr-1024", "distilbert-base-uncased", "roberta-base", or "xlm-roberta-base", "xlm-roberta-large". See
#' also https://www.r-text.org/articles/Word_embeddings.html. If specified as NULL, set parameters pretrained_weights, tokenizer_class and model_class.
#' @param pretrained_weights advanced parameter submitted to HuggingFace interface to get models not yet officially incorporated into *text*. Default = NULL. for details see https://huggingface.co/
#' @param tokenizer_class advanced parameter submitted to HuggingFace interface to get models not yet officially incorporated into *text*. Default = NULL. for details see https://huggingface.co/
#' @param model_class advanced parameter submitted to HuggingFace interface to get models not yet officially incorporated into *text*. Default = NULL. for details see https://huggingface.co/
#' @param layers Specify the layers that should be extracted (default 'all'). It is more efficient to only extract the layers
#' that you need (e.g., 11:12). Layer 0 is the decontextualised input layer (i.e., not comprising hidden states) and thus adviced to not use.
#' These layers can then be aggregated in the textLayerAggregation function.
#' @param return_tokens If TRUE, provide the tokens used in the specified transformer model.
#' @return A tibble with tokens, layer identifyer and word embeddings. Note that layer 0 is the input embedding to the transformer, and should
#' normally not be used.
#' @examples
#'\dontrun{
#' x <- Language_based_assessment_data_8_10[1:2, 1:2]
#' wordembeddings <- textHuggingFace(x, layers = 'all')
#'}
#' @seealso see \code{\link{textLayerAggregation}} and \code{\link{textEmbed}}
#' @importFrom reticulate source_python
#' @importFrom dplyr %>% bind_rows
#' @importFrom tibble tibble as_tibble
#' @importFrom magrittr set_colnames
#' @export
textHuggingFace <- function(x,
                            contexts = TRUE,
                            decontexts = TRUE,
                            model = "bert-base-uncased",
                            layers = 'all',
                            return_tokens = TRUE,
                            pretrained_weights = NULL,
                            tokenizer_class = NULL,
                            model_class = NULL) {

  # Run python file with HunggingFace interface to state-of-the-art transformers UPDATING
  # reticulate::source_python("~/inst/python/huggingface_Interface3.py")
  reticulate::source_python(system.file("python", "huggingface_Interface3.py", package="text", mustWork = TRUE))

  # Setting up the specifics of the models; the parameters for HuggingFace.
  if(model == "bert-base-uncased"){
    pretrained_weights = 'bert-base-uncased'
    tokenizer_class = BertTokenizer
    model_class = BertModel
    } else if (model == "bert-base-multilingual-uncased"){
      pretrained_weights = 'bert-base-multilingual-uncased'
      tokenizer_class = BertTokenizer
      model_class = BertModel
    } else if (model == "bert-base-multilingual-cased"){
      pretrained_weights = 'bert-base-multilingual-cased'
      tokenizer_class = BertTokenizer
      model_class = BertModel
      }  else if (model == "openai-gpt"){
    pretrained_weights = 'openai-gpt'
    tokenizer_class = OpenAIGPTTokenizer
    model_class = OpenAIGPTModel
  } else if (model == "gpt2"){
    pretrained_weights = 'GPT2Tokenizer'
    tokenizer_class = OpenAIGPTTokenizer
    model_class = GPT2Model
  } else if (model == "ctrl"){
    pretrained_weights = 'ctrl'
    tokenizer_class = CTRLTokenizer
    model_class = CTRLModel
  } else if (model == "transfo-xl-wt103"){
    pretrained_weights = 'transfo-xl-wt103'
    tokenizer_class = TransfoXLTokenizer
    model_class = TransfoXLModel
  } else if (model == "xlnet-base-cased"){
    pretrained_weights = 'xlnet-base-cased'
    tokenizer_class = XLNetTokenizer
    model_class = XLNetModel
  } else if (model == "xlm-mlm-enfr-1024"){
    pretrained_weights = 'xlm-mlm-enfr-1024'
    tokenizer_class = XLMTokenizer
    model_class = XLMModel
  } else if (model == "distilbert-base-uncased"){
    pretrained_weights = 'distilbert-base-uncased'
    tokenizer_class = DistilBertTokenizer
    model_class = DistilBertModel
  } else if (model == "roberta-base"){
    pretrained_weights = 'roberta-base'
    tokenizer_class = RobertaTokenizer
    model_class = RobertaModel
  } else if (model == "xlm-roberta-base"){
    pretrained_weights = 'xlm-roberta-base'
    tokenizer_class = XLMRobertaTokenizer
    model_class = XLMRobertaModel
  } else if (model == "xlm-roberta-large"){
    pretrained_weights = 'xlm-roberta-large'
    tokenizer_class = XLMRobertaTokenizer
    model_class = XLMRobertaModel
  } else if (is.null(model)){
    pretrained_weights
    tokenizer_class
    model_class
  }

  # Select all character variables and make then UTF-8 coded, since BERT wants it that way
  data_character_variables <- select_character_v_utf8(x)

  # This gives sorted word embeddings based on context (i.e., the entire text is sent to the transformer model)
  if(contexts){
  x <- data_character_variables
  sorted_layers_ALL_variables <- list()
  sorted_layers_ALL_variables$context <- list()
  # Loop over all character variables i_variables = 1
  for (i_variables in 1:length(data_character_variables)){

  # Python file function to HuggingFace # i_variables=2
  hg_embeddings <- hgTransformerGetEmbedding(text_strings = x[[i_variables]],
                                             pretrained_weights = pretrained_weights,
                                             tokenizer_class = tokenizer_class,
                                             model_class = model_class,
                                             layers = layers,
                                             return_tokens = return_tokens)

  #x <- Language_based_assessment_data_8_10[1:5, c(1:2, 5)]
  #hg_embeddings <- hgTransformerGetEmbedding(text_strings = x[[1]],
  #                                           pretrained_weights = 'bert-base-uncased',
  #                                           tokenizer_class = BertTokenizer,
  #                                           model_class = BertModel,
  #                                           layers = 11:12,
  #                                           return_tokens = FALSE)
  #x <- hg_embeddings

  variable_x <- sortingLayers(x = hg_embeddings, layers = layers, return_tokens = return_tokens)

  sorted_layers_ALL_variables$context[[i_variables]] <- variable_x
  names(sorted_layers_ALL_variables$context)[[i_variables]] <- names(x)[i_variables]
  sorted_layers_ALL_variables
    }
  }

  # Deonctextualised embeddings for individual words
  if(decontexts){
    sorted_layers_All_decontexts <- list()
    sorted_layers_All_decontexts$decontext <- list()
    # Get word-embeddings for all individual-words (which is used for the word plot)
    singlewords <- getUniqueWordsAndFreq(data_character_variables)
    list_words <- sapply(singlewords$words, list)
    names(list_words)<-NULL

    #  hg_decontexts_embeddings <- hgTransformerGetEmbedding(text_strings = list_words,
    #                                             pretrained_weights = 'bert-base-uncased',
    #                                             tokenizer_class = BertTokenizer,
    #                                             model_class = BertModel,
    #                                             layers = layers,
    #                                             return_tokens = FALSE)

    hg_decontexts_embeddings <- hgTransformerGetEmbedding(text_strings = list_words,
                                               pretrained_weights = pretrained_weights,
                                               tokenizer_class = tokenizer_class,
                                               model_class = model_class,
                                               layers = layers,
                                               return_tokens = return_tokens)

    # Sort out layers as above
    sorted_layers_All_decontexts$decontext$single_we$single_we <- sortingLayers(x=hg_decontexts_embeddings, layers = layers, return_tokens = return_tokens)
    names(sorted_layers_All_decontexts$decontext$single_we$single_we) <- NULL
    sorted_layers_All_decontexts$decontext$single_words <- singlewords
  }

  #Combine previous list and word list
  word_embeddings_with_layers <- c(sorted_layers_ALL_variables, sorted_layers_All_decontexts)
  word_embeddings_with_layers
}

#x <- Language_based_assessment_data_8_10[1:6, c(1:2, 5)]
#test_context_decontext_true <- textHuggingFace(x, return_tokens = TRUE, layers = "all")
##test_context_decontext_true$context
##test_context_decontext_false <- textHuggingFace(x, return_tokens = FALSE, layers = 1:2)
##test_context_decontext_false
#
#word_embeddings_layers = test_context_decontext_true$decontext
#tokens_select <- NULL
##tokens_deselect <- c("[SEP]", "[CLS]")
#aggregation = "mean"
#layers = "all"

#word_embeddings_layers <- textHuggingFace(tibble("I'm feeling relatedness with other", "hello how are you"))
#test$context[1][1][1]
#word_embeddings_layers
# devtools::document()
#' Select and aggregate layers of hidden states to form a word embeddings.
#'
#' @param word_embeddings_layers Layers outputted from textHuggingFace.
#' @param layers The numbers of the layers to be aggregated (e.g., c(11:12) to aggregate the eleventh and twelth; 'all' is not possible here).
#' Note that layer 0 is the input embedding to the transformer, and should normally not be used.
#' @param aggregation Method to aggregate the dimensions of each selected layer (default "mean"; see also "min" or "max).
#' @param tokens_select Option to only select embeddings linked to specifc token such as [CLS] and [SEP] (default NULL).
#' @param tokens_deselect Option to deselect embeddings linked to specifc token such as [CLS] and [SEP] (default NULL).
#' @return A tibble with word embeddings. Note that layer 0 is the input embedding to the transformer, which is normally not used.
#' @examples
#'\dontrun{
#' wordembeddings <- textLayerAggregation(word_embeddings_layers)
#'}
#' @seealso see \code{\link{textHuggingFace}} and \code{\link{textEmbed}}
#' @importFrom dplyr %>% bind_rows
#' @export
textLayerAggregation <- function(word_embeddings_layers,
                                 layers = 1:12,
                                 aggregation = "mean",
                                 tokens_select=NULL,
                                 tokens_deselect=NULL){

    #word_embeddings_layers <- word_embeddings_layers_1_1
  #word_embeddings_layers <- word_embeddings_layers_1_1_1
  # If selecting "all" layers, find out number of layers to help indicate layer index later in code
  if(is.character(layers)) {
    # Get the first embeddings
    x_layer_unique <- unique(grep_col_by_name_in_list(word_embeddings_layers[[1]][[1]], "layer_number"))
    x_layer_unique
    # Get which layers
    x_layer_unique_numeric <- as.numeric(x_layer_unique)
    # Remove layer 0 becuase it is the input layer for the word embeddings.
    if(x_layer_unique_numeric[1] == 0) {
      layers <- x_layer_unique_numeric[2:length(x_layer_unique_numeric)]
    }
    layers
  }

  # Loop over the list of variables
  selected_layers_aggregated_tibble <- list()
  for (variable_list_i in 1:length(word_embeddings_layers)) {   # variable_list_i =1
  x <- word_embeddings_layers[[variable_list_i]]

  # Go over the lists and select the layers (in case "all" or too many have been retrieved from textHuggingFace)
  selected_layers <- lapply(x, function(x) x[x$layer_number %in% layers,])

  # Go over the lists and select the token (e.g., CLS) tokens_select = NULL tokens_select = "[CLS]"
  if(!is.null(tokens_select)){
    selected_layers <- lapply(selected_layers, function(x) x[x$tokens %in% tokens_select,])
  }

  # Go over the lists and DEselect the token (e.g., CLS) tokens_select = NULL tokens_select = "[CLS]"
  if(!is.null(tokens_deselect)){
    selected_layers <- lapply(selected_layers, function(x) x[!x$tokens %in% tokens_deselect,])
  }

  # Select only dimensions (i.e., remove tokens and layer_number)
  selected_layers <- lapply(selected_layers, function(x) dplyr::select(x, dplyr::starts_with("Dim")))

  # Aggregate (Remove all tokens and layer; but creat a cell with the infomration abt layers, aggregation)
  selected_layers_aggregated <- lapply(selected_layers, textEmbeddingAggregation, aggregation = "max")

  # Sort out output
  selected_layers_aggregated_tibble[[variable_list_i]] <- dplyr::bind_rows(selected_layers_aggregated)
  }
  names(selected_layers_aggregated_tibble) <- names(word_embeddings_layers)
  selected_layers_aggregated_tibble
}

#embeddings_test_context_decontext <- textLayerAggregation(test_context_decontext$context)


# devtools::document()
#' Extract layers and aggregate them to word embeddings, for all character variables in the dataframe.
#'
#' @param x Tibble/dataframe with at least one character variable.
#' @param contexts Provide word embeddings based on word contexts (standard method; default = TRUE).
#' @param decontexts Provide word embeddings of single words as input (embeddings used for plotting; default = TRUE).
#' @param model Character strinng specifying pre-trained language model (default 'bert-base-uncased'; options "openai-gpt",
#' "gpt2", "ctrl", "transfo-xl-wt103", "xlnet-base-cased", "xlm-mlm-enfr-1024", "distilbert-base-cased", "roberta-base", or "xlm-roberta-base".
#' @param layers Specify the layers that should be extracted (default 11:12). It is more efficient to only extract the layers
#' that you need (e.g., 12). Layer 0 is the decontextualised input layer (i.e., not comprising hidden states) and thus adviced to not use.
#' These layers can then be aggregated in the textLayerAggregation function. If you want all layers then use 'all'.
#' @param context_layers Specify the layers that should be aggregated (default 11:12). Layer 0 is the decontextualised input layer (i.e., not comprising hidden states) and thus adviced to not use.
#' @param context_aggregation Method to aggregate the contextualised layers (e.g., "mean", "min" or "max).
#' @param context_tokens_select Option to select word embeddings linked to specific tokens such as [CLS] and [SEP] for the context embeddings.
#' @param context_tokens_deselect Option to deselect embeddings linked to specific tokens such as [CLS] and [SEP] for the context embeddings.
#' @param decontext_layers Layers to aggregate for the decontext embeddings.
#' @param decontext_aggregation Method to aggregate the decontextualised layers (i.e., embeddings from single words; e.g., "mean", "min" or "max).
#' @param decontext_tokens_select Option to select embeddings linked to specific tokens such as [CLS] and [SEP] for the decontext embeddings.
#' @param decontext_tokens_deselect option to deselect embeddings linked to specifc tokens such as [CLS] and [SEP] for the decontext embeddings.
#' @return A tibble with tokens, layer identifyer and word embeddings. Note that layer 0 is the input embedding to the transformer
#' @examples
#'\dontrun{
#' x <- Language_based_assessment_data_8_10[1:2, 1:2]
#' #Example 1
#' wordembeddings <- textEmbed(x, layers = 9:11, context_layers = 11, decontext_layers = 9)
#' #Example 1
#' wordembeddings <- textEmbed(x, layers = 'all', context_layers = 'all', decontext_layers = 'all')
#'}
#' @seealso see \code{\link{textLayerAggregation}} and \code{\link{textHuggingFace}}
#' @export
textEmbed <- function(x,
                      model = 'bert-base-uncased',
                      layers = 11:12,
                      contexts = TRUE,
                      context_layers = 11:12,
                      context_aggregation = "mean",
                      context_tokens_select = NULL,
                      context_tokens_deselect = NULL,
                      decontexts = TRUE,
                      decontext_layers = 11:12,
                      decontext_aggregation = "mean",
                      decontext_tokens_select = NULL,
                      decontext_tokens_deselect = NULL){
  # .rs.restartR()
  #library(text)
  reticulate::source_python("inst/python/huggingface_Interface3.py")

  # Get hiden states/layers for all text; both context and decontext
  all_wanted_layers <- textHuggingFace(x,
                                       contexts = contexts,
                                       decontexts = decontexts,
                                       model = model,
                                       layers = layers,
                                       return_tokens = FALSE)

  # Aggregate context layers
  contextualised_embeddings <- textLayerAggregation(word_embeddings_layers = all_wanted_layers$context,
                                                    layers = context_layers,
                                                    aggregation = context_aggregation,
                                                    tokens_select = NULL,
                                                    tokens_deselect = NULL)
  # Aggregate DEcontext layers (in case they should be added differently from context)
  decontextualised_embeddings <- textLayerAggregation(word_embeddings_layers = all_wanted_layers$decontext$single_we,
                                                      layers = decontext_layers,
                                                      aggregation = decontext_aggregation,
                                                      tokens_select = decontext_tokens_select,
                                                      tokens_deselect = decontext_tokens_deselect)
  # Combine the words for each decontextualised embedding
  decontextualised_embeddings_words <- dplyr::bind_cols(all_wanted_layers$decontext$single_words, decontextualised_embeddings)

  # Adding embeddings to one list
  all_embeddings <- contextualised_embeddings
  all_embeddings$singlewords_we <- decontextualised_embeddings_words
  all_embeddings
}

#x <- Language_based_assessment_data_8_10[1:3, c(1:2, 5)]
#everying <- textEmbed(x)
#everying

