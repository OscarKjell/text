

#' Find encoding type of variable and the set it to UTF-8.
#' @param tibble including both text and numeric variables.
#' @return all character variables in UTF-8 format.
#' @noRd
get_encoding_change <- function(x) {
  Encoding(x) <- Encoding(enc2utf8(x))
  x
}


#' Select all character variables and make them UTF-8 coded (BERT wants it in this format).
#' @param x A character variable or a tibble including both character and numeric variables.
#' @return a tibble containing all character variables in UTF-8 format.
#' @importFrom dplyr select_if
#' @importFrom tibble as_tibble
#' @importFrom purrr map
#' @noRd
select_character_v_utf8 <- function(x) {
  # If a vector is submitted, make it a tibble column.
  if (is.vector(x) == TRUE & is.list(x) == FALSE) {
    # Select variable name to have as column name in the end
    colname_x <- deparse(substitute(x))
    # Remove everything before a "$"
    colname_x <- gsub("^.*\\$", "", colname_x)

    x <- tibble::as_tibble_col(x)
    colnames(x) <- substitute(colname_x)
  }
  # Select all character variables
  x_characters <- dplyr::select_if(x, is.character)
  # This makes sure that all variables are UTF-8 coded
  x_characters <- tibble::as_tibble(purrr::map(x_characters, get_encoding_change))
}


#' Function to normalize the vector to one; to a unit vector.
#'
#' @param x a word embedding
#' @return normalized (unit) vector/word embedding.
#' @noRd
normalizeV <- function(x) {
  magnitude <-
    x / sqrt(sum(x^2, na.rm = TRUE))
}


#' Function to take min, max, mean or the CLS
#' (which comes from BERT models; not Static spaces) from list of vectors
#' @param x word embeddings to be aggregated
#' @param aggregation method to carry out the aggregation, including "min", "max" and "mean" which takes the
#' minimum, maximum or mean across each column; or "concatenate", which links together each word embedding layer
#' to one long row.
#' @return aggregated word embeddings.
#' @importFrom tibble as_tibble_row
#' @importFrom purrr map
#' @noRd
textEmbeddingAggregation <- function(x, aggregation = "min") {
  if (aggregation == "min") {
    min_vector <- unlist(purrr::map(x, min, na.rm = TRUE))
    min_vector
  } else if (aggregation == "max") {
    max_vector <- unlist(purrr::map(x, max, na.rm = TRUE))
    max_vector
  } else if (aggregation == "mean") {
    mean_vector <- colMeans(x, na.rm = TRUE)
    mean_vector
  } else if (aggregation == "concatenate") {
    long_vector <- c(t(x)) %>% tibble::as_tibble_row(.name_repair = "minimal")
    colnames(long_vector) <- paste0("Dim", sep = "", seq_len(length(long_vector)))
    long_vector
  } else if (aggregation == "normalize") {
    sum_vector <- unlist(purrr::map(x, sum, na.rm = TRUE))
    normalized_vector <- normalizeV(sum_vector)
    normalized_vector
  }
}


#' getUniqueWordsAndFreq
#' Function unites several text variables and rows to one,
#' where all text is transformed to lowercase and tokenized.
#' Also give word frequencies.
#' @param x_characters A character column in a tibble.
#' @return A tibble with a unique words column and a column with their respective frequency.
#' @importFrom tibble tibble
#' @importFrom stringi stri_c stri_trans_tolower
# @importFrom stringr str_c str_split stri_split_boundaries
# @importFrom tokenizers tokenize_words
#' @noRd
getUniqueWordsAndFreq <- function(x_characters) {
  # Unite all text variables into one
  x_characters2 <- tidyr::unite(x_characters, "x_characters2", seq_len(ncol(x_characters)), sep = " ")

  # unite all rows in the column into one cell
  x_characters3 <- stringi::stri_c(x_characters2$x_characters2, collapse = " ")
  # Tokenize into single words help(stri_split_boundaries)
  x_characters4a <- stringi::stri_trans_tolower(x_characters3)
  x_characters4b <- stringi::stri_split_boundaries(x_characters4a,
    type = "word",
    skip_word_none = TRUE,
    skip_word_number = FALSE
  )[[1]]

  # Create dataframe with single words and frequency
  x_characters5 <- data.frame(sort(table(unlist(strsplit(tolower(x_characters4b), " ")))))
  if (length(x_characters5) == 1) {
    colnames(x_characters5) <- c("Freq")
    x_characters5 <- tibble::rownames_to_column(x_characters5, "Var1")
  }
  singlewords <- tibble::tibble(x_characters5$Var1, x_characters5$Freq)
  colnames(singlewords) <- c("words", "n")
  singlewords$words <- as.character(singlewords$words)
  singlewords
}


#' This is a function that sorts out (i.e., tidy) the embeddings from the huggingface interface.
#' @param x list of layers.
#' @param layers the number of layers to get (setting comes from textEmbedLayersOutput).
#' @param return_tokens bolean whether tokens have been returned (setting comes from textEmbedLayersOutput).
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
  # Loop over the cases in the variable; i_in_variable = 1
  variable_x <- list()
  for (i_in_variable in 1:participants) {
    if (return_tokens) {
      tokens <- x[[2]][[i_in_variable]]
      token_id <- seq_len(length(tokens))
      all_layers <- x[[1]][[i_in_variable]]
    } else {
      tokens <- NULL
      all_layers <- x[[i_in_variable]]
      # Count number of embeddings within one layer
      token_id <- seq_len(length(all_layers[[1]][[1]]))
    }

    # Loop of the number of layers; i_layers=1
    layers_list <- list()
    for (i_layers in seq_len(length(all_layers))) {
      i_layers_for_tokens <- all_layers[i_layers]

      # Transpose layers and give each column a DimX names
      layers_4_token <- suppressMessages(t(dplyr::bind_cols(i_layers_for_tokens))) %>%
        magrittr::set_colnames(c(paste0("Dim", 1:dimensions))) # %>%
      layers_4_token <- tibble::as_tibble(layers_4_token)

      if (return_tokens) {
        tokens_layer_number <- tibble::tibble(tokens, token_id, rep(layers[i_layers], length(tokens)))
        colnames(tokens_layer_number) <- c("tokens", "token_id", "layer_number")
        tokens_lnumber_layers <- dplyr::bind_cols(tokens_layer_number, layers_4_token)
      } else {
        layer_number <- tibble::tibble(token_id, rep(layers[i_layers], nrow(layers_4_token)))
        colnames(layer_number) <- c("token_id", "layer_number")
        tokens_lnumber_layers <- dplyr::bind_cols(layer_number, layers_4_token)
      }

      layers_list[[i_layers]] <- tokens_lnumber_layers
      layers_list
    }
    layers_tibble <- dplyr::bind_rows(layers_list)

    variable_x[[i_in_variable]] <- layers_tibble
  }
  variable_x
}


#' This is a function that uses the textAggregation to aggreagate the layers
#' @param x list of layers.
#' @param aggregation method to aggregate the layers.
#' @return Aggregated layers in tidy tibble format.
#' @noRd
layer_aggregation_helper <- function(x, aggregation = aggregation) {
  aggregated_layers_saved <- list()
  # Loops over the number of tokens
  for (i_token_id in seq_len(length(unique(x$token_id)))) {
    # Selects all the layers for each token/token_id
    x1 <- x[x$token_id == i_token_id, ]
    # Select only Dimensions
    x2 <- dplyr::select(x1, dplyr::starts_with("Dim"))
    # Aggregate the dimensions
    x3 <- textEmbeddingAggregation(x2, aggregation = aggregation)
    aggregated_layers_saved[[i_token_id]] <- x3
  }
  aggregated_layers_saved1 <- dplyr::bind_rows(aggregated_layers_saved)
  return(aggregated_layers_saved1)
}


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


#' Extract layers of hidden states (word embeddings) for all character variables in a given dataframe.
#' @param x A character variable or a tibble/dataframe with at least one character variable.
#' @param contexts Provide word embeddings based on word contexts
#' (standard method; default = TRUE).
#' @param decontexts Provide word embeddings of single words as input
#' (embeddings used for plotting; default = TRUE).
#' @param model Character string specifying pre-trained language model (default 'bert-base-uncased').
#'  For full list of options see pretrained models at
#'  \href{https://huggingface.co/transformers/pretrained_models.html}{HuggingFace}.
#'  For example use "bert-base-multilingual-cased", "openai-gpt",
#' "gpt2", "ctrl", "transfo-xl-wt103", "xlnet-base-cased", "xlm-mlm-enfr-1024", "distilbert-base-cased",
#' "roberta-base", or "xlm-roberta-base".
#' @param layers Specify the layers that should be extracted (default 11). It is more efficient
#' to only extract the layers that you need (e.g., 11). You can also extract several (e.g., 11:12),
#' or all by setting this parameter to "all". Layer 0 is the decontextualized input layer
#' (i.e., not comprising hidden states) and thus should normally not be used. These layers can then
#'  be aggregated in the textEmbedLayerAggregation function.
#' @param return_tokens If TRUE, provide the tokens used in the specified transformer model.
#' @param device Name of device to use: 'cpu', 'gpu', or 'gpu:k' where k is a specific device number
#' @param print_python_warnings bolean; when TRUE any warnings from python environment are printed
#'  to the console. (Either way warnings are saved in the comment of the embedding)
#' @param tokenizer_parallelism If TRUE this will turn on tokenizer parallelism. Default FALSE.
#' @param model_max_length The maximum length (in number of tokens) for the inputs to the transformer model
#' (default the value stored for the associated model).
#' @return A tibble with tokens, column specifying layer and word embeddings. Note that layer 0 is the
#' input embedding to the transformer, and should normally not be used.
#' @examples
#' \donttest{
#' # x <- Language_based_assessment_data_8[1:2, 1:2]
#' # word_embeddings_with_layers <- textEmbedLayersOutput(x, layers = 11:12)
#' }
#' @seealso see \code{\link{textEmbedLayerAggregation}} and \code{\link{textEmbed}}
#' @importFrom reticulate source_python py_capture_output
#' @importFrom dplyr %>% bind_rows
#' @importFrom tibble tibble as_tibble
#' @importFrom magrittr set_colnames
#' @export
textEmbedLayersOutput <- function(x,
                                  contexts = TRUE,
                                  decontexts = TRUE,
                                  model = "bert-base-uncased",
                                  layers = 11,
                                  return_tokens = TRUE,
                                  device = "cpu",
                                  print_python_warnings = FALSE,
                                  tokenizer_parallelism = FALSE,
                                  model_max_length = NULL) {

  # Run python file with HunggingFace interface to state-of-the-art transformers
  reticulate::source_python(system.file("python",
    "huggingface_Interface3.py",
    # envir = NULL,
    package = "text",
    mustWork = TRUE
  ))

  # Select all character variables and make them UTF-8 coded (e.g., BERT wants it that way).
  data_character_variables <- select_character_v_utf8(x)

  # This gives sorted word embeddings based on context (i.e., the entire text is sent to the transformer model)
  if (contexts) {
    x <- data_character_variables
    sorted_layers_ALL_variables <- list()
    sorted_layers_ALL_variables$context <- list()
    # Loop over all character variables; i_variables = 1 library(tidyverse) help(py_capture_output)
    for (i_variables in seq_len(length(data_character_variables))) {

      # Python file function to HuggingFace
      textrpp_py_warnings_text_context <- reticulate::py_capture_output(
        hg_embeddings <- hgTransformerGetEmbedding(
          text_strings = x[[i_variables]],
          model = model,
          layers = layers,
          return_tokens = return_tokens,
          device = device,
          tokenizer_parallelism = tokenizer_parallelism,
          model_max_length = model_max_length
        ),
        type = "stderr"
      )

      variable_x <- sortingLayers(x = hg_embeddings, layers = layers, return_tokens = return_tokens)

      sorted_layers_ALL_variables$context[[i_variables]] <- variable_x
      names(sorted_layers_ALL_variables$context)[[i_variables]] <- names(x)[[i_variables]]

      # Adding informative comment help(comment)
      layers_string <- paste(as.character(layers), sep = " ", collapse = " ")

      if (!exists("textrpp_py_warnings_text_context")) {
        textrpp_py_warnings_text_context <- "There were no python warnings."
      }

      comment(sorted_layers_ALL_variables$context) <- paste("Information about the embeddings. textEmbedLayersOutput: ",
        "model:", model, "; ",
        "layers:", layers_string, ".",
        "Warnings from python: ", textrpp_py_warnings_text_context,
        collapse = "\n"
      )
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

    textrpp_py_warnings_text_decontext <- reticulate::py_capture_output(
      hg_decontexts_embeddings <- hgTransformerGetEmbedding(
        text_strings = list_words,
        model = model,
        layers = layers,
        return_tokens = return_tokens,
        device = device,
        tokenizer_parallelism = tokenizer_parallelism,
        model_max_length = model_max_length
      )
    )

    # Sort out layers as above
    sorted_layers_All_decontexts$decontext$single_we$single_we <- sortingLayers(
      x = hg_decontexts_embeddings,
      layers = layers,
      return_tokens = return_tokens
    )
    names(sorted_layers_All_decontexts$decontext$single_we$single_we) <- NULL
    sorted_layers_All_decontexts$decontext$single_words <- singlewords

    # Adding informative data
    layers_string <- paste(as.character(layers), sep = " ", collapse = " ")
    comment(sorted_layers_All_decontexts$decontext$single_we) <- c(paste("Information about the embeddings.
                                                                         textEmbedLayersOutput: ",
      "model:", model,
      "layers:", layers_string, ".",
      collapse = "; "
    ))

    comment(sorted_layers_All_decontexts$decontext$single_words) <- c(paste("Information about the embeddings.
                                                                            textEmbedLayersOutput: ",
      "model:", model,
      "layers:", layers_string, ".",
      collapse = "; "
    ))

    sorted_layers_All_decontexts
  }

  if (print_python_warnings == TRUE) {
    if (contexts == TRUE) {
      cat(textrpp_py_warnings_text_context)
    }

    if (decontexts == TRUE) {
      cat(textrpp_py_warnings_text_decontext)
    }
  }

  # Combine previous list and word list
  if (contexts == TRUE & decontexts == TRUE) {
    word_embeddings_with_layers <- c(sorted_layers_ALL_variables, sorted_layers_All_decontexts)
  } else if (contexts == TRUE & decontexts == FALSE) {
    word_embeddings_with_layers <- c(sorted_layers_ALL_variables)
  } else if (contexts == FALSE & decontexts == TRUE) {
    word_embeddings_with_layers <- c(sorted_layers_All_decontexts)
  }
  return(word_embeddings_with_layers)
}


#' Select and aggregate layers of hidden states to form a word embeddings.
#' @param word_embeddings_layers Layers outputted from textEmbedLayersOutput.
#' @param layers The numbers of the layers to be aggregated
#' (e.g., c(11:12) to aggregate the eleventh and twelfth).
#' Note that layer 0 is the input embedding to the transformer, and should normally not be used.
#' Selecting 'all' thus removes layer 0.
#' @param aggregate_layers Method to carry out the aggregation among the layers for each word/token,
#' including "min", "max" and "mean" which takes the minimum, maximum or mean across each column;
#' or "concatenate", which links together each layer of the word embedding to one long row. Default is "concatenate"
#' @param aggregate_tokens Method to carry out the aggregation among the word embeddings for the words/tokens,
#' including "min", "max" and "mean" which takes the minimum, maximum or mean across each column;
#' or "concatenate", which links together each layer of the word embedding to one long row.
#' @param tokens_select Option to only select embeddings linked to specific tokens
#' such as "[CLS]" and "[SEP]" (default NULL).
#' @param tokens_deselect Option to deselect embeddings linked to specific tokens
#' such as "[CLS]" and "[SEP]" (default NULL).
#' @return A tibble with word embeddings. Note that layer 0 is the input embedding to
#' the transformer, which is normally not used.
#' @examples
#' \donttest{
#' # word_embeddings_layers <- textEmbedLayersOutput(Language_based_assessment_data_8$harmonywords[1],
#' # layers = 11:12)
#' # word_embeddings <- textEmbedLayerAggregation(word_embeddings_layers$context, layers = 11)
#' }
#' @seealso see \code{\link{textEmbedLayersOutput}} and \code{\link{textEmbed}}
#' @importFrom dplyr %>% bind_rows
#' @export
textEmbedLayerAggregation <- function(word_embeddings_layers,
                                      layers = 11:12,
                                      aggregate_layers = "concatenate",
                                      aggregate_tokens = "mean",
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

  # Loop over the list of variables; variable_list_i = 1; variable_list_i = 2; remove(variable_list_i)
  selected_layers_aggregated_tibble <- list()
  for (variable_list_i in seq_len(length(word_embeddings_layers))) {
    x <- word_embeddings_layers[[variable_list_i]]

    # Go over the lists and select the layers; [[1]] ok to add below x=
    if ((length(setdiff(layers, unique(x[[1]]$layer_number))) > 0) == TRUE) {
      stop(cat(
        colourise("You are trying to aggregate layers that were not extracted.", fg = "red"),
        colourise("For example, in textEmbed the layers option needs to include all the
                  layers used in context_layers.", fg = "green")
      ))
    }

    selected_layers <- lapply(x, function(x) x[x$layer_number %in% layers, ])

    # Go over the lists and select the tokens (e.g., CLS) (tokens_select = NULL tokens_select = "[CLS]")
    if (!is.null(tokens_select)) {
      selected_layers <- lapply(selected_layers, function(x) x[x$tokens %in% tokens_select, ])
    }

    # Go over the lists and DEselect the token (e.g., CLS) (tokens_select = NULL tokens_select = "[CLS]")
    if (!is.null(tokens_deselect)) {
      selected_layers <- lapply(selected_layers, function(x) x[!x$tokens %in% tokens_deselect, ])

      # If any of the tokens that was removed was "[CLS]", subtract one on token_id so it starts with
      # 1 and works with the layer_aggregation_helper
      if (length(tokens_deselect) == 1 & tokens_deselect == "[CLS]") {
        # Subtract
        selected_layers <- purrr::map(selected_layers, function(x) {
          x$token_id <- x$token_id - 1
          x
        })
      } else if (length(tokens_deselect) > 1) {
        if (table(tokens_deselect %in% "[CLS]")[[2]] > 0) {
          # Subtract
          selected_layers <- purrr::map(selected_layers, function(x) {
            x$token_id <- x$token_id - 1
            x
          })
        }
      }
    }

    ## Aggregate across layers; help(lapply); i_token_id=1
    selected_layers_aggregated <- lapply(selected_layers, layer_aggregation_helper, aggregation = aggregate_layers)

    # Aggregate (Remove all tokens and layers; but create a cell with the information abt layers, aggregation)
    selected_layers_tokens_aggregated <- lapply(selected_layers_aggregated,
                                                textEmbeddingAggregation,
                                                aggregation = aggregate_tokens)

    # Sort output
    selected_layers_aggregated_tibble[[variable_list_i]] <- dplyr::bind_rows(selected_layers_tokens_aggregated)
    # Add informative comments
    original_comment <- comment(word_embeddings_layers)
    layers_string <- paste(as.character(layers), sep = " ", collapse = " ")
    comment(selected_layers_aggregated_tibble[[variable_list_i]]) <- paste(original_comment,
      "textEmbedLayerAggregation: layers = ",
      layers_string,
      "aggregate_layers = ",
      aggregate_layers,
      "aggregate_tokens = ",
      aggregate_tokens,
      "tokens_select = ",
      tokens_select,
      "tokens_deselect = ",
      tokens_deselect,
      collapse = ";"
    )
  }
  names(selected_layers_aggregated_tibble) <- names(word_embeddings_layers)
  selected_layers_aggregated_tibble
}


#' Extract layers and aggregate them to word embeddings, for all character variables in a given dataframe.
#' @param x A character variable or a tibble/dataframe with at least one character variable.
#' @param model Character string specifying pre-trained language model (default 'bert-base-uncased').
#'  For full list of options see pretrained models at
#'  \href{https://huggingface.co/transformers/pretrained_models.html}{HuggingFace}.
#'  For example use "bert-base-multilingual-cased", "openai-gpt",
#' "gpt2", "ctrl", "transfo-xl-wt103", "xlnet-base-cased", "xlm-mlm-enfr-1024", "distilbert-base-cased",
#' "roberta-base", or "xlm-roberta-base".
#' @param layers Specify the layers that should be extracted (default 11:12). It is more efficient to
#' only extract the layers that you need (e.g., 12).
#' Layer 0 is the decontextualized input layer (i.e., not comprising hidden states) and thus advised to not use.
#' These layers can then be aggregated in the textEmbedLayerAggregation function. If you want all layers then use 'all'.
#' @param contexts Provide word embeddings based on word contexts
#' (standard method; default = TRUE).
#' @param decontexts Provide word embeddings of single words as input
#' (embeddings, e.g., used for plotting; default = TRUE).
# @param pretrained_weights advanced parameter submitted to the HuggingFace interface to get models not yet officially
# incorporated into text. Default = NULL. for details see https://huggingface.co/.
#' @param context_layers Specify the layers that should be aggregated (default the number of layers extracted above).
#' Layer 0 is the decontextualized input layer (i.e., not comprising hidden states)
#' and thus advised not to be used.
#' @param context_aggregation_layers Method to aggregate the contextualized layers (e.g., "mean", "min" or
#' "max, which takes the minimum, maximum or mean, respectively, across each column; or "concatenate", which links
#'  together each word embedding layer to one long row.
#' @param context_aggregation_tokens Method to aggregate the contextualized tokens (e.g., "mean", "min" or
#' "max, which takes the minimum, maximum or mean, respectively, across each column; or "concatenate",
#' which links together each word embedding layer to one long row.
#' @param context_tokens_select Option to select word embeddings linked to specific tokens
#' such as [CLS] and [SEP] for the context embeddings.
#' @param context_tokens_deselect Option to deselect embeddings linked to specific tokens
#' such as [CLS] and [SEP] for the context embeddings.
#' @param decontext_layers Layers to aggregate for the decontext embeddings  the number of layers extracted above.
#' @param decontext_aggregation_layers Method to aggregate the decontextualized layers (e.g., "mean", "min" or
#'  "max, which takes the minimum, maximum or mean, respectively, across each column; or "concatenate",
#'  which links together each word embedding layer to one long row.
#' @param decontext_aggregation_tokens Method to aggregate the decontextualized tokens (e.g., "mean", "min" or "max,
#' which takes the minimum, maximum or mean, respectively, across each column; or "concatenate", which links
#' together each word embedding layer to one long row.
#' @param decontext_tokens_select Option to select embeddings linked to specific tokens
#' such as [CLS] and [SEP] for the decontext embeddings.
#' @param decontext_tokens_deselect option to deselect embeddings linked to specific tokens
#' such as [CLS] and [SEP] for the decontext embeddings.
#' @param device Name of device to use: 'cpu', 'gpu', or 'gpu:k' where k is a specific device number
#' @param print_python_warnings bolean; when true any warnings from python environment are printed to the console.
#' @param model_max_length The maximum length (in number of tokens) for the inputs to the transformer model
#' (default the value stored for the associated model).
#' @return A tibble with tokens, a column for layer identifier and word embeddings.
#' Note that layer 0 is the input embedding to the transformer
#' @examples
#' \donttest{
#' # x <- Language_based_assessment_data_8[1:2, 1:2]
#' # Example 1
#' # word_embeddings <- textEmbed(x, layers = 9:11, context_layers = 11, decontext_layers = 9)
#' # Show information that have been saved with the embeddings about how they were constructed
#' # comment(word_embeddings$satisfactionwords)
#' # comment(word_embeddings$singlewords_we)
#' # comment(word_embeddings)
#' # Example 2
#' # word_embeddings <- textEmbed(x, layers = "all", context_layers = "all", decontext_layers = "all")
#' }
#' @seealso see \code{\link{textEmbedLayerAggregation}} and \code{\link{textEmbedLayersOutput}}
#' @export
textEmbed <- function(x,
                      model = "bert-base-uncased",
                      layers = 11:12,
                      contexts = TRUE,
                      context_layers = layers,
                      context_aggregation_layers = "concatenate",
                      context_aggregation_tokens = "mean",
                      context_tokens_select = NULL,
                      context_tokens_deselect = NULL,
                      decontexts = TRUE,
                      decontext_layers = layers,
                      decontext_aggregation_layers = "concatenate",
                      decontext_aggregation_tokens = "mean",
                      decontext_tokens_select = NULL,
                      decontext_tokens_deselect = NULL,
                      device = "cpu",
                      print_python_warnings = FALSE,
                      model_max_length = NULL) {
  T1_textEmbed <- Sys.time()

  reticulate::source_python(system.file("python", "huggingface_Interface3.py", package = "text", mustWork = TRUE))

  # Get hidden states/layers for all text; both context and decontext
  all_wanted_layers <- textEmbedLayersOutput(x,
    contexts = contexts,
    decontexts = decontexts,
    model = model,
    layers = layers,
    return_tokens = FALSE,
    device = device,
    print_python_warnings = print_python_warnings,
    model_max_length = model_max_length
  )

  # Aggregate context layers
  contextualised_embeddings <- textEmbedLayerAggregation(
    word_embeddings_layers = all_wanted_layers$context,
    layers = context_layers,
    aggregate_layers = context_aggregation_layers,
    aggregate_tokens = context_aggregation_tokens,
    tokens_deselect = NULL
  )

  # Aggregate DEcontext layers (in case they should be added differently from context)
  decontextualised_embeddings <- textEmbedLayerAggregation(
    word_embeddings_layers = all_wanted_layers$decontext$single_we,
    layers = decontext_layers,
    aggregate_layers = decontext_aggregation_layers,
    aggregate_tokens = decontext_aggregation_tokens,
    tokens_select = decontext_tokens_select,
    tokens_deselect = decontext_tokens_deselect
  )


  # Combine the words for each decontextualized embedding
  decontextualised_embeddings_words <- dplyr::bind_cols(all_wanted_layers$decontext$single_words,
                                                        decontextualised_embeddings)

  if (decontexts == TRUE) {
    comment(decontextualised_embeddings_words) <- comment(decontextualised_embeddings[[1]])
  }

  T2_textEmbed <- Sys.time()
  Time_textEmbed <- T2_textEmbed - T1_textEmbed
  Time_textEmbed <- sprintf("Duration to embed text: %f %s", Time_textEmbed, units(Time_textEmbed))
  Date_textEmbed <- Sys.time()
  # Adding embeddings to one list
  all_embeddings <- contextualised_embeddings
  all_embeddings$singlewords_we <- decontextualised_embeddings_words

  comment(all_embeddings) <- paste(Time_textEmbed,
    "; Date created: ", Date_textEmbed,
    sep = "",
    collapse = " "
  )
  all_embeddings
}
