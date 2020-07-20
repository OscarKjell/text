
#  devtools::document()
#' Select all character variables and make them UTF-8 coded (BERT wants it in this format).
#' @param tibble including both text and numeric variables.
#' @return all character variables in UTF-8 format.
#' @importFrom dplyr select_if
#' @importFrom tibble as_tibble
#' @importFrom purrr map
#' @importFrom stringi stri_encode
#' @noRd
select_character_v_utf8 <- function(x) {
  # Select all character variables
  x_characters <- dplyr::select_if(x, is.character)
  # This makes sure that all variables are UTF-8 coded
  x_characters <- tibble::as_tibble(purrr::map(x_characters, stringi::stri_encode, "", "UTF-8"))
}

#  devtools::document()
#' Function to take min, max, mean or the CLS
#' (which comes from BERT models; not Static spaces) from list of vectors
#' @param x word embeddings to be aggregated
#' @param aggregation method to carry out the aggregation
#' @return aggregated word embeddings.
#' @importFrom stats complete.cases
#' @noRd
#'
textEmbeddingAggregation <- function(x, aggregation = "min") {
  if (aggregation == "min") {
    min_vector <- unlist(map(x, min, na.rm = TRUE))
  } else if (aggregation == "max") {
    max_vector <- unlist(map(x, max, na.rm = TRUE))
  } else if (aggregation == "mean") {
    mean_vector <- colMeans(x, na.rm = TRUE)
  } #else if (aggregation == "CLS") {
    #CLS <- x %>%
     # dplyr::filter(token_index == 1, layer_index == 1)
  } # else if (aggregation == "normalize1") {
  #    norma_vector <- unlist(map(x, norma))
  # x2 <- x[complete.cases(x), ]
  # x3 <- colSums(x2) # BELOW NEED FIXING; REMOVED FROM CRAN
  # x4 <- ppls::normalize.vector(x3)
  # }
#}

# devtools::document()
#' applysemrep
#' Function to apply the semantic representation (or word embeddings)  to ONE word from
#' a matrix of semreps; and return a vector with NA if word is not found.
#' That is, look up word embeddings for each word.
#' @param x A word.
#' @param single_wordembeddings Used to get number of dimensions in embedding/space
#' @return semantic representation (word embedding) from a matrix.
#' @noRd
applysemrep <- function(x, single_wordembeddings1) {
  # If semrep is found get it; if not return NA vector of dimensions
  if (sum(single_wordembeddings1$words == x[TRUE]) %in% 1) {
    x <- tolower(x)
    # Get the semantic representation for a word=x
    word1rep <- single_wordembeddings1[single_wordembeddings1$words == x, ]
    # Only get the semantic representation as a vector without the actual word in the first column
    wordrep <- purrr::as_vector(word1rep %>% dplyr::select(dplyr::starts_with("Dim")))
    # If the word does not have a semrep return a vector with NAs with the same number of dimensions as columns with Dim
  } else if (x %in% NA) {
    # The length() refers to how many column starts with Dim (i.e., how many dimensions)
    wordrep <- data.frame(matrix(ncol = length(single_wordembeddings1 %>%
      dplyr::select(dplyr::starts_with("Dim"))), nrow = 1))
    ## class(wordrep)
    wordrep <- as.numeric(wordrep)
  } else {
    wordrep <- data.frame(matrix(ncol = length(single_wordembeddings1 %>%
      dplyr::select(dplyr::starts_with("Dim"))), nrow = 1))
    colnames(wordrep) <- paste0("Dim", sep = "", 1:length(wordrep))
    wordrep
  }
}

# devtools::document()
#' getUniqueWordsAndFreq
#' Function unites several text variables and rows to one,
#' where all text is transformed to lowercase and tokenized.
#' Also give word frequencies.
#' @param x A word
#' @return dataframe with unique words and their frequency.
#' @noRd
getUniqueWordsAndFreq <- function(x_characters) {
  # Unite all text variables into one
  x_characters2 <- tidyr::unite(x_characters, "x_characters2", 1:ncol(x_characters), sep = " ")
  # unite all rows in the column into one cell
  x_characters3 <- paste(x_characters2[1], collapse = " ")
  # Remove all punctuation characters
  x_characters4 <- stringr::str_replace_all(x_characters3, "[[:punct:]]", " ")
  # Remove \n
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
#' This is a function that sorts out the embeddings
#' (and is used again below for decontextualized words).
#' @param x list of layers.
#' @param layers the number of layers to get (setting comes from textHuggingFace).
#' @param return_tokens bolean whether tokens have been returned (setting comes from textHuggingFace).
#' @return Layers in tidy tibble format with each dimension column called Dim1, Dim2 etc.
#' @noRd
sortingLayers <- function(x, layers = layers, return_tokens = return_tokens) {
  # If selecting "all" layers, find out number of layers to help indicate layer index later in code
  if (is.character(layers)) {
    layers <- 0:(length(x[[1]][[1]]) - 1)
  }

  # Find number of dimensions (where the place differ depending on return_token is TRUE or FALSE)
  if (return_tokens) {
    dimensions <- length(x[[1]][[1]][[1]][[1]][[1]])
    participants <- length(x[[1]])
  } else {
    dimensions <- length(x[[1]][[1]][[1]][[1]])
    participants <- length(x)
  }

  # Tidy-structure tokens and embeddings
  # Loop over the cases in the variable
  variable_x <- list()
  for (i_in_variable in 1:participants) {
    if (return_tokens) {
      tokens <- x[[2]][[i_in_variable]]
      all_layers <- x[[1]][[i_in_variable]]
    } else {
      tokens <- NULL
      all_layers <- x[[i_in_variable]]
    }

    # Loop of the number of layers
    layers_list <- list()
    for (i_layers in 1:length(all_layers)) {
      i_layers_for_tokens <- all_layers[i_layers]

      # Transpose layers and give each column a DimX names
      layers_4_token <- suppressMessages(t(dplyr::bind_cols(i_layers_for_tokens))) %>%
        magrittr::set_colnames(c(paste0("Dim", 1:dimensions))) # %>%
      layers_4_token <- tibble::as_tibble(layers_4_token)

      if (return_tokens) {
        tokens_layer_number <- tibble::tibble(tokens, rep(layers[i_layers], length(tokens)))
        colnames(tokens_layer_number) <- c("tokens", "layer_number")
        tokens_lnumber_layers <- bind_cols(tokens_layer_number, layers_4_token)
      } else {
        layer_number <- tibble::tibble(rep(layers[i_layers], nrow(layers_4_token)))
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
#' This function finds a column by name independent on where in the list structure it is.
#' @param l a list.
#' @param pattern what to find; such as the "layers_number" column.
#' @return elements in the column called pattern.
#' @noRd
grep_col_by_name_in_list <- function(l, pattern) {
  u <- unlist(l)
  u[grep(pattern, names(u))]
}

# devtools::document()
#' Extract layers of hidden states (word embeddings) for all character variables in a given dataframe.
#' @param x Tibble/dataframe with at least one character variable.
#' @param contexts Provide word embeddings based on word contexts
#' (standard method; default = TRUE).
#' @param decontexts Provide word embeddings of single words as input
#' (embeddings used for plotting; default = TRUE).
#' @param model Character string specifying pre-trained language model.
#' Default 'bert-base-uncased'; options "bert-base-multilingual-uncased", "bert-base-multilingual-cased", "openai-gpt",
#' "gpt2", "ctrl", "transfo-xl-wt103", "xlnet-base-cased", "xlm-mlm-enfr-1024", "distilbert-base-uncased",
#' "roberta-base", "xlm-roberta-base", "xlm-roberta-large", or "T5Model". See
#' also https://www.r-text.org/articles/Word_embeddings.html.
#' If specified as NULL, set parameters pretrained_weights, tokenizer_class and model_class.
#' @param pretrained_weights Advanced parameter submitted to HuggingFace interface to get models not yet
#' officially incorporated into text. Default = NULL. For alternatives see https://huggingface.co/.
#' @param tokenizer_class Advanced parameter submitted to HuggingFace interface to get models not yet
#' officially incorporated into text. Default = NULL. for alternatives see https://huggingface.co/.
#' @param model_class Advanced parameter submitted to HuggingFace interface to get models not yet officially
#' incorporated into text.
#' Default = NULL. for alternatives see https://huggingface.co/.
#' @param layers Specify the layers that should be extracted (default 11:12). It is more efficient
#' to only extract the layers that you need (e.g., 11:12). You can also extract all by setting this
#' parameter to "all". Layer 0 is the decontextualized input layer (i.e., not comprising hidden states)
#' and thus should normally not be used. These layers can then be aggregated in the textLayerAggregation function.
#' @param return_tokens If TRUE, provide the tokens used in the specified transformer model.
#' @return A tibble with tokens, column specifying layer and word embeddings. Note that layer 0 is the
#' input embedding to the transformer, and should normally not be used.
#' @examples
#' \dontrun{
#' x <- Language_based_assessment_data_8_10[1:2, 1:2]
#' wordembeddings <- textHuggingFace(x, layers = "all")
#' }
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
                            layers = 11:12,
                            return_tokens = TRUE,
                            pretrained_weights = NULL,
                            tokenizer_class = NULL,
                            model_class = NULL) {

  # Run python file with HunggingFace interface to state-of-the-art transformers
  reticulate::source_python(system.file("python", "huggingface_Interface3.py", package = "text", mustWork = TRUE))

  # Setting up the specifics of the models; the parameters for HuggingFace.
  if (model == "bert-base-uncased") {
    pretrained_weights <- "bert-base-uncased"
    tokenizer_class <- BertTokenizer
    model_class <- BertModel
  } else if (model == "bert-base-multilingual-uncased") {
    pretrained_weights <- "bert-base-multilingual-uncased"; tokenizer_class <- BertTokenizer; model_class <- BertModel
  } else if (model == "bert-base-multilingual-cased") {
    pretrained_weights <- "bert-base-multilingual-cased"
    tokenizer_class <- BertTokenizer
    model_class <- BertModel
  } else if (model == "openai-gpt") {
    pretrained_weights <- "openai-gpt"; tokenizer_class <- OpenAIGPTTokenizer; model_class <- OpenAIGPTModel
  } else if (model == "gpt2") {
    pretrained_weights <- "GPT2Tokenizer"; tokenizer_class <- OpenAIGPTTokenizer; model_class <- GPT2Model
  } else if (model == "ctrl") {
    pretrained_weights <- "ctrl"; tokenizer_class <- CTRLTokenizer; model_class <- CTRLModel
  } else if (model == "transfo-xl-wt103") {
    pretrained_weights <- "transfo-xl-wt103"; tokenizer_class <- TransfoXLTokenizer; model_class <- TransfoXLModel
  } else if (model == "xlnet-base-cased") {
    pretrained_weights <- "xlnet-base-cased"; tokenizer_class <- XLNetTokenizer; model_class <- XLNetModel
  } else if (model == "xlm-mlm-enfr-1024") {
    pretrained_weights <- "xlm-mlm-enfr-1024"; tokenizer_class <- XLMTokenizer; model_class <- XLMModel
  } else if (model == "distilbert-base-uncased") {
    pretrained_weights <- "distilbert-base-uncased"; tokenizer_class <- DistilBertTokenizer; model_class <- DistilBertModel
  } else if (model == "roberta-base") {
    pretrained_weights <- "roberta-base"; tokenizer_class <- RobertaTokenizer; model_class <- RobertaModel
  } else if (model == "xlm-roberta-base") {
    pretrained_weights <- "xlm-roberta-base"; tokenizer_class <- XLMRobertaTokenizer; model_class <- XLMRobertaModel
  } else if (model == "xlm-roberta-large") {
    pretrained_weights <- "xlm-roberta-large"; tokenizer_class <- XLMRobertaTokenizer; model_class <- XLMRobertaModel
  } else if (model == "t5-small") {
    pretrained_weights <- "t5-small"; tokenizer_class <- T5Tokenizer; model_class <- T5Model
  } else if (model == "new") {
    pretrained_weights = pretrained_weights; tokenizer_class = tokenizer_class; model_class = model_class
  }

  # Select all character variables and make them UTF-8 coded (e.g., BERT wants it that way).
  data_character_variables <- select_character_v_utf8(x)

  # This gives sorted word embeddings based on context (i.e., the entire text is sent to the transformer model)
  if (contexts) {
    x <- data_character_variables
    sorted_layers_ALL_variables <- list()
    sorted_layers_ALL_variables$context <- list()
    # Loop over all character variables
    for (i_variables in 1:length(data_character_variables)) {

      # Python file function to HuggingFace
      hg_embeddings <- hgTransformerGetEmbedding(
        text_strings = x[[i_variables]],
        pretrained_weights = pretrained_weights,
        tokenizer_class = tokenizer_class,
        model_class = model_class,
        layers = layers,
        return_tokens = return_tokens
      )

      variable_x <- sortingLayers(x = hg_embeddings, layers = layers, return_tokens = return_tokens)

      sorted_layers_ALL_variables$context[[i_variables]] <- variable_x
      names(sorted_layers_ALL_variables$context)[[i_variables]] <- names(x)[i_variables]
      sorted_layers_ALL_variables
    }
  }

  # Decontextualized embeddings for individual words
  if (decontexts) {
    sorted_layers_All_decontexts <- list()
    sorted_layers_All_decontexts$decontext <- list()
    # Get word embeddings for all individual words (which is used for the word plot).
    singlewords <- getUniqueWordsAndFreq(data_character_variables)
    list_words <- sapply(singlewords$words, list)
    names(list_words) <- NULL

    hg_decontexts_embeddings <- hgTransformerGetEmbedding(
      text_strings = list_words,
      pretrained_weights = pretrained_weights,
      tokenizer_class = tokenizer_class,
      model_class = model_class,
      layers = layers,
      return_tokens = return_tokens
    )

    # Sort out layers as above
    sorted_layers_All_decontexts$decontext$single_we$single_we <- sortingLayers(
      x = hg_decontexts_embeddings,
      layers = layers,
      return_tokens = return_tokens
    )
    names(sorted_layers_All_decontexts$decontext$single_we$single_we) <- NULL
    sorted_layers_All_decontexts$decontext$single_words <- singlewords
    sorted_layers_All_decontexts
  }

  # Combine previous list and word list
  if(contexts == TRUE & decontexts == TRUE){
  word_embeddings_with_layers <- c(sorted_layers_ALL_variables, sorted_layers_All_decontexts)
  word_embeddings_with_layers
  } else if(contexts == TRUE & decontexts == FALSE){
    word_embeddings_with_layers <- c(sorted_layers_ALL_variables)
  } else if(contexts == FALSE & decontexts == TRUE){
    word_embeddings_with_layers <- c(sorted_layers_All_decontexts)
  }
  word_embeddings_with_layers
}

# devtools::document()
#' Select and aggregate layers of hidden states to form a word embeddings.
#' @param word_embeddings_layers Layers outputted from textHuggingFace.
#' @param layers The numbers of the layers to be aggregated
#' (e.g., c(11:12) to aggregate the eleventh and twelfth).
#' Note that layer 0 is the input embedding to the transformer, and should normally not be used.
#' Selecting 'all' thus removes layer 0.
#' @param aggregation Method to aggregate the dimensions of each selected layer
#' (default "mean"; see also "min",  "max" and "CLS").
#' @param tokens_select Option to only select embeddings linked to specific tokens
#' such as "[CLS]" and "[SEP]" (default NULL).
#' @param tokens_deselect Option to deselect embeddings linked to specific tokens
#' such as "[CLS]" and "[SEP]" (default NULL).
#' @return A tibble with word embeddings. Note that layer 0 is the input embedding to
#' the transformer, which is normally not used.
#' @examples
#' \dontrun{
#' wordembeddings <- textLayerAggregation(word_embeddings_layers)
#' }
#' @seealso see \code{\link{textHuggingFace}} and \code{\link{textEmbed}}
#' @importFrom dplyr %>% bind_rows
#' @export
textLayerAggregation <- function(word_embeddings_layers,
                                 layers = 1:12,
                                 aggregation = "mean",
                                 tokens_select = NULL,
                                 tokens_deselect = NULL) {

  # If selecting 'all' layers, find out number of layers to help indicate layer index later in code
  if (is.character(layers)) {
    # Get the first embeddings
    x_layer_unique <- unique(grep_col_by_name_in_list(word_embeddings_layers[[1]][[1]], "layer_number"))
    # Get which layers
    x_layer_unique_numeric <- as.numeric(x_layer_unique)
    # Remove layer 0 because it is the input layer for the word embeddings.
    if (x_layer_unique_numeric[1] == 0) {
      layers <- x_layer_unique_numeric[2:length(x_layer_unique_numeric)]
    }
    layers
  }

  # Loop over the list of variables
  selected_layers_aggregated_tibble <- list()
  for (variable_list_i in 1:length(word_embeddings_layers)) {
    x <- word_embeddings_layers[[variable_list_i]]

    # Go over the lists and select the layers
    selected_layers <- lapply(x, function(x) x[x$layer_number %in% layers, ])

    # Go over the lists and select the tokens (e.g., CLS) (tokens_select = NULL tokens_select = "[CLS]")
    if (!is.null(tokens_select)) {
      selected_layers <- lapply(selected_layers, function(x) x[x$tokens %in% tokens_select, ])
    }

    # Go over the lists and DEselect the token (e.g., CLS) (tokens_select = NULL tokens_select = "[CLS]")
    if (!is.null(tokens_deselect)) {
      selected_layers <- lapply(selected_layers, function(x) x[!x$tokens %in% tokens_deselect, ])
    }

    # Select only dimensions (i.e., remove tokens and layer_number)
    selected_layers <- lapply(selected_layers, function(x) dplyr::select(x, dplyr::starts_with("Dim")))

    # Aggregate (Remove all tokens and layers; but create a cell with the information abt layers, aggregation)
    selected_layers_aggregated <- lapply(selected_layers, textEmbeddingAggregation, aggregation = aggregation)

    # Sort output
    selected_layers_aggregated_tibble[[variable_list_i]] <- dplyr::bind_rows(selected_layers_aggregated)
  }
  names(selected_layers_aggregated_tibble) <- names(word_embeddings_layers)
  selected_layers_aggregated_tibble
}

# devtools::document()
#' Extract layers and aggregate them to word embeddings, for all character variables in a given dataframe.
#' @param x Tibble/dataframe with at least one character variable.
#' @param contexts Provide word embeddings based on word contexts
#' (standard method; default = TRUE).
#' @param decontexts Provide word embeddings of single words as input
#' (embeddings, e.g., used for plotting; default = TRUE).
#' @param model Character string specifying pre-trained language model (default 'bert-base-uncased'; options "openai-gpt",
#' "gpt2", "ctrl", "transfo-xl-wt103", "xlnet-base-cased", "xlm-mlm-enfr-1024", "distilbert-base-cased",
#' "roberta-base", or "xlm-roberta-base". Can set this parameter to NULL and then manually
#' specify pretrained_weights, tokenizer_class and model_class.
#' @param layers Specify the layers that should be extracted (default 11:12). It is more efficient to
#' only extract the layers that you need (e.g., 12).
#' Layer 0 is the decontextualized input layer (i.e., not comprising hidden states) and thus advised to not use.
#' These layers can then be aggregated in the textLayerAggregation function. If you want all layers then use 'all'.
#' @param pretrained_weights advanced parameter submitted to the HuggingFace interface to get models not yet officially
#' incorporated into text. Default = NULL. for details see https://huggingface.co/.
#' @param tokenizer_class advanced parameter submitted to the HuggingFace interface to get
#' models not yet officially incorporated into text.
#' Default = NULL. for details see https://huggingface.co/.
#' @param model_class advanced parameter submitted to the HuggingFace interface to get models not yet officially
#' incorporated into text.
#' Default = NULL. for details see https://huggingface.co/.
#' @param context_layers Specify the layers that should be aggregated (default 11:12).
#' Layer 0 is the decontextualized input layer (i.e., not comprising hidden states) and thus advised not to be used.
#' @param context_aggregation Method to aggregate the contextualized layers (e.g., "mean", "min" or "max).
#' @param context_tokens_select Option to select word embeddings linked to specific tokens
#' such as [CLS] and [SEP] for the context embeddings.
#' @param context_tokens_deselect Option to deselect embeddings linked to specific tokens
#' such as [CLS] and [SEP] for the context embeddings.
#' @param decontext_layers Layers to aggregate for the decontext embeddings.
#' @param decontext_aggregation Method to aggregate the decontextualized layers
#' (i.e., embeddings from single words; e.g., "mean", "min", "max or "[CLS]").
#' @param decontext_tokens_select Option to select embeddings linked to specific tokens
#' such as [CLS] and [SEP] for the decontext embeddings.
#' @param decontext_tokens_deselect option to deselect embeddings linked to specific tokens
#' such as [CLS] and [SEP] for the decontext embeddings.
#' @return A tibble with tokens, a column for layer identifier and word embeddings.
#' Note that layer 0 is the input embedding to the transformer
#' @examples
#' \dontrun{
#' x <- Language_based_assessment_data_8_10[1:2, 1:2]
#' # Example 1
#' wordembeddings <- textEmbed(x, layers = 9:11, context_layers = 11, decontext_layers = 9)
#' # Example 1
#' wordembeddings <- textEmbed(x, layers = "all", context_layers = "all", decontext_layers = "all")
#' }
#' @seealso see \code{\link{textLayerAggregation}} and \code{\link{textHuggingFace}}
#' @export
textEmbed <- function(x,
                      model = "bert-base-uncased",
                      layers = 11:12,
                      pretrained_weights = NULL,
                      tokenizer_class = NULL,
                      model_class = NULL,
                      contexts = TRUE,
                      context_layers = 11:12,
                      context_aggregation = "mean",
                      context_tokens_select = NULL,
                      context_tokens_deselect = NULL,
                      decontexts = TRUE,
                      decontext_layers = 11:12,
                      decontext_aggregation = "mean",
                      decontext_tokens_select = NULL,
                      decontext_tokens_deselect = NULL) {
  reticulate::source_python(system.file("python", "huggingface_Interface3.py", package = "text", mustWork = TRUE))

  # Get hidden states/layers for all text; both context and decontext
  all_wanted_layers <- textHuggingFace(x,
    contexts = contexts,
    decontexts = decontexts,
    model = model,
    layers = layers,
    return_tokens = FALSE,
    pretrained_weights = pretrained_weights,
    tokenizer_class = tokenizer_class,
    model_class = model_class
  )

  # Aggregate context layers
  contextualised_embeddings <- textLayerAggregation(
    word_embeddings_layers = all_wanted_layers$context,
    layers = context_layers,
    aggregation = context_aggregation,
    tokens_select = NULL,
    tokens_deselect = NULL
  )

  # Aggregate DEcontext layers (in case they should be added differently from context)
  decontextualised_embeddings <- textLayerAggregation(
    word_embeddings_layers = all_wanted_layers$decontext$single_we,
    layers = decontext_layers,
    aggregation = decontext_aggregation,
    tokens_select = decontext_tokens_select,
    tokens_deselect = decontext_tokens_deselect
  )

  # Combine the words for each decontextualized embedding
  decontextualised_embeddings_words <- dplyr::bind_cols(all_wanted_layers$decontext$single_words, decontextualised_embeddings)

  # Adding embeddings to one list
  all_embeddings <- contextualised_embeddings
  all_embeddings$singlewords_we <- decontextualised_embeddings_words
  all_embeddings
}
