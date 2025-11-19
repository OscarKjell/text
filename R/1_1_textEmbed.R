#' Find encoding type of variable and then set it to UTF-8.
#' @param x Tibble including both text and numeric variables.
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
  if (is.vector(x) == TRUE && is.list(x) == FALSE) {
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


#' Detect non-ASCII characters
#'
#' This function to detect non-ASCII characters in a tibble with multiple columns.
#' @param data_tibble A character variable or a tibble including  character variables.
#' @return a tibble containing variable names, row numbers and text including non-acii.
#' @importFrom tibble tibble
#' @export
textFindNonASCII <- function(data_tibble) {

  # Initialize an empty list to store results
  results_list <- list()

  # Iterate over each column in the tibble
  for (col_name in colnames(data_tibble)) {

    # Extract the actual vector from the tibble column, ensuring it's treated as a character vector
    data_column <- as.character(data_tibble[[col_name]])

    # Use sapply to identify any characters outside the ASCII range
    matches <- base::sapply(seq_along(data_column), function(i) {
      text <- data_column[i]
      # Check if the element is a valid string, then convert to a character and check
      if (length(text) == 1 && !base::is.na(text) && !base::is.null(text)) {
        any(base::charToRaw(base::as.character(text)) > base::as.raw(0x7F))
      } else {
        FALSE
      }
    })

    # Get the original row numbers
    row_indices <- base::which(matches)

    # Extract the text entries that have non-ASCII characters
    matching_texts <- data_column[row_indices]

    # If there are any matches, create a tibble and add to the results list
    if (length(row_indices) > 0) {
      results_list[[col_name]] <- tibble::tibble(
        column_name = col_name,
        row_number = base::as.integer(row_indices),
        text = base::as.character(matching_texts)
      )
    }
  }

  # Combine all results into a single tibble
  final_result <- dplyr::bind_rows(results_list)

  return(final_result)
}

#' # Function to clean non-ASCII characters from a single text entry
#' @param data_tibble A character variable.
#' @return a tibble with removed ascii characters
#' @noRd
clean_text <- function(text) {
  #iconv(text, from = "UTF-8", to = "UTF-8", sub = "")
  iconv(text, from = "UTF-8", to = "ASCII", sub = "")
}

#' Clean non-ASCII characters
#'
#' textCleanNonASCII() cleans all text entries with a non-ASCII character in a tibble.
#' @param data_tibble A tibble with character variables.
#' @param problematic_texts (tibble)
#' @param remove_non_ascii (boolean)
#' @return a tibble with removed ascii characters
#' @importFrom dplyr mutate across everything
#' @importFrom purrr map_chr
#' @noRd
textCleanNonASCIIinfo <- function(
    data_tibble,
    problematic_texts = NULL,
    remove_non_ascii
    ) {

  # Combine column_name and row_number for each row
  combined_texts <- apply(problematic_texts[c("column_name", "row_number")], 1, function(x) {
    paste(x, collapse = " ")
  })

  # Merge all combined texts into a single string, separated by ";"
  final_string <- paste(combined_texts, collapse = "; ")

  warning_ascii <- paste("Warning: non-ascii characters were found in:",
                         final_string, "Many large laguage models cannot handle them. \n")

  message(colourise(warning_ascii, "brown"))
  message(colourise("To examine thise text cases use the textNonASCII() function. \n", "green"))

  # remove non-ASCII characters
  if(remove_non_ascii){

    # Apply `clean_text` to each element in the tibble
    cleaned_tibble <- data_tibble %>%
      dplyr::mutate(dplyr::across(dplyr::everything(), ~ purrr::map_chr(., clean_text)))

    message(colourise("Non-ASCII characters has been removed. \n", "green"))

  } else {

    cleaned_tibble <- data_tibble

  }

  return(cleaned_tibble)
}


#' Clean non-ASCII characters
#'
#' textCleanNonASCII() cleans all text entries with a non-ASCII character in a tibble.
#' @param data_tibble A tibble with character variables.
#' @return a tibble with removed ascii characters
#' @importFrom dplyr mutate across everything
#' @importFrom purrr map_chr
#' @export
textCleanNonASCII <- function(data_tibble) {
  # Apply `clean_text` to each element in the tibble
  cleaned_tibble <- data_tibble %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), ~ purrr::map_chr(., clean_text)))

  return(cleaned_tibble)
}



#' Function to normalize the vector to one; to a unit vector.
#'
#' @param x a word embedding
#' @return normalized (unit) vector/word embedding.
#' @noRd
normalizeV <- function(x) {
  magnitude <- x / sqrt(sum(x^2, na.rm = TRUE))
  return(magnitude)
}

#' Function to take min, max, mean or the CLS
#' (which comes from BERT models; not Static spaces) from list of vectors
#' @param x word embeddings to be aggregated
#' @param aggregation method to carry out the aggregation, including "min", "max" and "mean" which takes the
#' minimum, maximum or mean across each column; or "concatenate", which links together each word embedding layer
#' to one long row.
#' @param weights to be used when aggreating using mean (used when batching and aggregating word_types).
#' @return aggregated word embeddings.
#' @importFrom tibble as_tibble_row
#' @importFrom purrr map
#' @noRd
textEmbeddingAggregation <- function(x,
                                     aggregation = "min",
                                     weights = NULL) {
  if (!is.null(weights) && aggregation == "mean") {
    # Weighted mean calculation
    weighted_mean_vector <- colSums(x * weights, na.rm = TRUE) / sum(weights, na.rm = TRUE)
    return(weighted_mean_vector)
  } else if (aggregation == "min") {
    min_vector <- unlist(purrr::map(x, min, na.rm = TRUE))
    return(min_vector)
  } else if (aggregation == "max") {
    max_vector <- unlist(purrr::map(x, max, na.rm = TRUE))
    return(max_vector)
  } else if (aggregation == "mean") {
    mean_vector <- colMeans(x, na.rm = TRUE)
    return(mean_vector)
  } else if (aggregation == "concatenate") {
    long_vector <- c(t(x)) %>% tibble::as_tibble_row(.name_repair = "minimal")
    colnames(long_vector) <- paste0("Dim", seq_len(length(long_vector)))

    variable_name <- names(x)[1]

    # If original name is not just Dim1, then add back Dim1_variable.name
    if (!variable_name == "Dim1") {
      variable_name <- sub(".*Dim1_", "", variable_name)
      colnames(long_vector) <- paste0(names(long_vector), "_", variable_name)
    }
    return(long_vector)
  } else if (aggregation == "normalize") {
    sum_vector <- unlist(purrr::map(x, sum, na.rm = TRUE))
    normalized_vector <- normalizeV(sum_vector)
    return(normalized_vector)
  } else {
    stop("Invalid aggregation method provided.")
  }
}

#' getUniqueWordsAndFreq
#' Function unites several text variables and rows to one,
#' where all text is transformed to lowercase and tokenized.
#' Also give word frequencies.
#' @param x_characters A character column in a tibble.
#' @param hg_tokenizer (boolean) Weather to use textTokenize
#' @param device (string) device to use in textTokenize (e.g., "cpu", "mps:0", "gpu:0").
#' @return A tibble with a unique words column and a column with their respective frequency.
#' @importFrom tibble tibble
#' @importFrom stringi stri_c stri_trans_tolower
# @importFrom stringr str_c str_split stri_split_boundaries
# @importFrom tokenizers tokenize_words
#' @noRd
getUniqueWordsAndFreq <- function(x_characters,
                                  hg_tokenizer = NULL,
                                  device = "cpu",
                                  ...) {
  if (is.null(hg_tokenizer)) {
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
  }

  if (!is.null(hg_tokenizer)) {
    x_characters4b <- lapply(list(x_characters), textTokenize, model = hg_tokenizer, device = device, ...)
    x_characters5 <- data.frame(sort(table(unlist(x_characters4b))))
  }

  if (length(x_characters5) == 1) {
    colnames(x_characters5) <- c("Freq")
    x_characters5 <- tibble::rownames_to_column(x_characters5, "Var1")
  }

  singlewords <- tibble::tibble(x_characters5$Var1, x_characters5$Freq)
  colnames(singlewords) <- c("words", "n")
  singlewords$words <- as.character(singlewords$words)

  return(singlewords)
}


#' This is a function that sorts out (i.e., tidy) the embeddings from the huggingface interface.
#' @param x list of layers.
#' @param layers the number of layers to get (setting comes from textEmbedRawLayers).
#' @param return_tokens boolean whether tokens have been returned (setting comes from textEmbedRawLayers).
#' @return Layers in tidy tibble format with each dimension column called Dim1, Dim2 etc.
#' @noRd
sortingLayers <- function(x,
                          layers = layers,
                          return_tokens = return_tokens) {
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
  # Replace outer loop over i_in_variable with map();
  variable_x <- purrr::map(1:participants, function(i_in_variable) {
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

    # Replace inner loop over i_layers with updated code
    totalTokensNum <- length(tokens)

    tarTb <- numeric(length = totalTokensNum * length(layers) * dimensions)

    tarTb <- reticulate::np_array(tarTb)

    tarTb <- tibble::as_tibble(
      reticulate::py_to_r(
        reticulate::array_reshape(tarTb, c(totalTokensNum * length(layers), dimensions))
      ),
      .name_repair = "minimal"
    )

    colnames(tarTb) <- paste0("Dim", seq_len(dimensions))

    purrr::map(seq_len(totalTokensNum), function(i) {
      purrr::map(seq_len(length(layers)), function(j) {
        k <- j - 1
        tarTb[i + totalTokensNum * k, ] <<- as.list(all_layers[[j]][[1]][[i]])
      })
    })

    # Add tokens, token IDs, and layer numbers to output tibble
    if (return_tokens) {
      tarTb <- cbind(tokens, token_id, layer_number = rep(layers, each = totalTokensNum), tarTb) %>%
        tibble::as_tibble()
    } else {
      tarTb <- cbind(token_id, layer_number = rep(layers, each = totalTokensNum), tarTb) %>%
        tibble::as_tibble()
    }

    tarTb
  })

  variable_x
}

#' This is a function that uses the textAggregation to aggregate the layers
#' @param x list of layers.
#' @param aggregation method to aggregate the layers.
#' @param return_tokens (boolean) returns the tokens as the first column.
#' @return Aggregated layers in tidy tibble format.
#' @noRd
layer_aggregation_helper <- function(x,
                                     aggregation = aggregation,
                                     return_tokens = FALSE) {
  aggregated_layers_saved <- list()

  # Get unique number of token ids in the variable starting with x$token_id ; i_token_id=1
  number_of_ids <- unique(x[, grep("^token_id", names(x))][[1]])

  # Loops over the number of tokens; i_token_id = 2; i_token_id = 3
  for (i_token_id in number_of_ids) { # seq_len(length(number_of_ids))
    # Selects all the layers for each token/token_id
    x1 <- x[x[, grep("^token_id", names(x))][[1]] == i_token_id, ]
    # Select only Dimensions
    x2 <- dplyr::select(x1, dplyr::starts_with("Dim"))
    # Aggregate the dimensions
    x3 <- textEmbeddingAggregation(x2, aggregation = aggregation)

    aggregated_layers_saved[[i_token_id]] <- x3
  }
  aggregated_layers_saved1 <- dplyr::bind_rows(aggregated_layers_saved)

  if (return_tokens) {
    # Number of ids
    number_of_layers <- unique(x[, grep("^layer_number", names(x))][[1]])
    n_layers <- length(number_of_layers)
    tokens <- x$tokens[1:(length(x$tokens) / n_layers)]
    tokens <- as_tibble_col(tokens, column_name = "tokens")
    aggregated_layers_saved1 <- dplyr::bind_cols(tokens, aggregated_layers_saved1)
  }

  return(aggregated_layers_saved1)
}


#' grep_col_by_name_in_list
#' This function finds a column by name independent on where in the list structure it is.
#' @param l a list.
#' @param pattern what to find; such as the "layers_number" column.
#' @return elements in the column called pattern.
#' @noRd
grep_col_by_name_in_list <- function(l,
                                     pattern) {
  u <- unlist(l)
  u[grep(pattern, names(u))]
}


#' Tokenize text-variables
#'
#' textTokenize() tokenizes according to different huggingface transformers
#' @param texts A character variable or a tibble/dataframe with at least one character variable.
#' @param model Character string specifying pre-trained language model (default 'bert-base-uncased').
#'  For full list of options see pretrained models at
#'  \href{https://huggingface.co/transformers/pretrained_models.html}{HuggingFace}.
#'  For example use "bert-base-multilingual-cased", "openai-gpt",
#' "gpt2", "ctrl", "transfo-xl-wt103", "xlnet-base-cased", "xlm-mlm-enfr-1024", "distilbert-base-cased",
#' "roberta-base", or "xlm-roberta-base".
#' @param max_token_to_sentence (numeric) Maximum number of tokens in a string to handle before
#' switching to embedding text sentence by sentence.
#' @param device (character) Name of device to use: 'cpu', 'gpu', 'gpu:k' or 'mps'/'mps:k'
#' for MacOS, where k is a specific device number. (default = "cpu"). Default is NULL, which sets gpu:0/mps:0
#' depending on OS.
#' @param tokenizer_parallelism If TRUE this will turn on tokenizer parallelism. Default FALSE.
#' @param model_max_length The maximum length (in number of tokens) for the inputs to the transformer model
#' (default the value stored for the associated model).
#' @param hg_gated Set to TRUE if the accessed model is gated.
#' @param hg_token The token needed to access the gated model.
#' Create a token from the ['Settings' page](https://huggingface.co/settings/tokens) of
#' the Hugging Face website. An an environment variable HUGGINGFACE_TOKEN can
#' be set to avoid the need to enter the token each time.
#' @param trust_remote_code use a model with custom code on the Huggingface Hub
#' @param logging_level Set the logging level. Default: "warning".
#' Options (ordered from less logging to more logging): critical, error, warning, info, debug
#' @return Returns tokens according to specified huggingface transformer.
#' @examples
#' \donttest{
#' # tokens <- textTokenize("hello are you?")
#' }
#' @seealso see \code{\link{textEmbed}}
#' @importFrom reticulate source_python
#' @importFrom tibble tibble as_tibble
#' @export
textTokenize <- function(texts,
                         model,
                         max_token_to_sentence = 4,
                         device = NULL,
                         tokenizer_parallelism = FALSE,
                         model_max_length = NULL,
                         hg_gated = FALSE,
                         hg_token = Sys.getenv("HUGGINGFACE_TOKEN",
                                                  unset = ""),
                         trust_remote_code = FALSE,
                         logging_level = "error") {

  # Setting device depending on OS
  if(is.null(device)){
    if(!is_osx()){
      device = "gpu:0"
    }
    if(is_osx()){
      device = "mps:0"
    }
  }

  # Run python file with HunggingFace interface to state-of-the-art transformers
  reticulate::source_python(system.file("python",
                                        "huggingface_Interface3.py",
                                        package = "text",
                                        mustWork = TRUE
  ))

  tokens <- hgTokenizerGetTokens(
    text_strings = texts,
    model = model,
    max_token_to_sentence = max_token_to_sentence,
    device = device,
    tokenizer_parallelism = tokenizer_parallelism,
    model_max_length = model_max_length,
    hg_gated = reticulate::r_to_py(hg_gated),
    hg_token = reticulate::r_to_py(hg_token),
    trust_remote_code = trust_remote_code,
    logging_level = logging_level
  )
  tokens1 <- lapply(tokens, tibble::as_tibble_col, column_name = "tokens")

  return(tokens1)
}

#' Extract layers of hidden states
#'
#' textEmbedRawLayers extracts layers of hidden states (word embeddings) for all character variables
#' in a given dataframe.
#' @param texts A character variable or a tibble with at least one character variable.
#' @param model (character) Character string specifying pre-trained language model
#' (default = 'bert-base-uncased'). For full list of options see pretrained models at
#'  \href{https://huggingface.co/transformers/pretrained_models.html}{HuggingFace}.
#'  For example use "bert-base-multilingual-cased", "openai-gpt",
#' "gpt2", "ctrl", "transfo-xl-wt103", "xlnet-base-cased", "xlm-mlm-enfr-1024",
#' "distilbert-base-cased", "roberta-base", or "xlm-roberta-base". Only load models that
#' you trust from HuggingFace; loading a malicious model can execute arbitrary code on
#' your computer).
#' @param layers (character or numeric) Specify the layers that should be extracted
#' (default -2, which give the second to last layer). It is more efficient to only extract the
#' layers that you need (e.g., 11). You can also extract several (e.g., 11:12),
#' or all by setting this parameter to "all". Layer 0 is the decontextualized input layer
#' (i.e., not comprising hidden states) and thus should normally not be used. These layers can then
#'  be aggregated in the textEmbedLayerAggregation function.
#' @param return_tokens (boolean) If TRUE, provide the tokens used in the specified transformer
#' model. (default = TRUE)
#' @param word_type_embeddings (boolean) Wether to provide embeddings for each word/token type.
#' (default = FALSE)
#' @param decontextualize (boolean) Wether to dectonextualise embeddings (i.e., embedding one word
#' at a time). (default = TRUE)
#' @param keep_token_embeddings (boolean) Whether to keep token level embeddings in the output
#' (when using word_types aggregation). (default= TRUE)
#' @param device (character) Name of device to use: 'cpu', 'gpu', 'gpu:k' or 'mps'/'mps:k'
#' for MacOS, where k is a specific device number. (default = "cpu"). Default is NULL, which sets gpu:0/mps:0
#' depending on OS.
#' @param tokenizer_parallelism (boolean) If TRUE this will turn on tokenizer parallelism.
#' (default = FALSE).
#' @param model_max_length The maximum length (in number of tokens) for the inputs to the
#' transformer model (default the value stored for the associated model).
#' @param max_token_to_sentence (numeric) Maximum number of tokens in a string to handle before
#'  switching to embedding text sentence by sentence. (default= 4)
#' @param hg_gated Set to TRUE if the accessed model is gated.
#' @param hg_token The token needed to access the gated model.
#' Create a token from the ['Settings' page](https://huggingface.co/settings/tokens) of
#' the Hugging Face website. An an environment variable HUGGINGFACE_TOKEN can
#' be set to avoid the need to enter the token each time.
#' @param trust_remote_code use a model with custom code on the Huggingface Hub
#' @param logging_level (character) Set the logging level. (default ="error")
#' Options (ordered from less logging to more logging): critical, error, warning, info, debug
#' @param sort (boolean) If TRUE sort the output to tidy format. (default = TRUE)
#' @return The textEmbedRawLayers() takes text as input, and returns the hidden states for
#' each token of the text, including the [CLS] and the [SEP].
#' Note that layer 0 is the input embedding to the transformer, and should normally not be used.
#' @examples
#' # Get hidden states of layer 11 and 12 for "I am fine".
#' \dontrun{
#' imf_embeddings_11_12 <- textEmbedRawLayers(
#'   "I am fine",
#'   layers = 11:12
#' )
#'
#' # Show hidden states of layer 11 and 12.
#' imf_embeddings_11_12
#' }
#' @seealso See \code{\link{textEmbedLayerAggregation}} and \code{\link{textEmbed}}.
#' @importFrom reticulate source_python
#' @importFrom dplyr %>% bind_rows group_split
#' @importFrom tibble tibble as_tibble
#' @importFrom magrittr set_colnames
#' @export
textEmbedRawLayers <- function(
    texts,
    model = 'bert-base-uncased',
    layers = -2,
    return_tokens = TRUE,
    word_type_embeddings = FALSE,
    decontextualize = FALSE,
    keep_token_embeddings = TRUE,
    device = NULL,
    tokenizer_parallelism = FALSE,
    model_max_length = NULL,
    max_token_to_sentence = 4,
    hg_gated = FALSE,
    hg_token = Sys.getenv("HUGGINGFACE_TOKEN",
                          unset = ""),
    trust_remote_code = FALSE,
    logging_level = "error",
    sort = TRUE) {

  # Setting device depending on OS
  if(is.null(device)){
    if(!is_osx()){
      device = "gpu:0"
    }
    if(is_osx()){
      device = "mps:0"
    }
  }


  if (decontextualize == TRUE && word_type_embeddings == FALSE) {
    stop(message(
      colourise("decontextualize = TRUE & word_type_embeddings = FALSE has not been
                implemented in textEmbedRawLayers() at this stage.",
                fg = "red"
      ),
      colourise("When using decontextualize = TRUE  you need to create the word_type_embeddings.
      To create a text embeddings withouth it would take unnecessary time as it would require to
      send the same decontextualised words to a transformer multiple times (whilst getting the same
      results over and over). Consdier using rextEmbed, to get token embeddings as well as text embeddings.",
                fg = "green"
      )
    ))
  }


  # Run python file with HunggingFace interface to state-of-the-art transformers
  reticulate::source_python(system.file("python",
                                        "huggingface_Interface3.py",
                                        package = "text",
                                        mustWork = TRUE
  ))

  if (is.numeric(layers)) {
    if (max(layers) > textModelLayers(model, reticulate::r_to_py(hg_gated), reticulate::r_to_py(hg_token))) {
      stop("You are trying to extract layers that do not exist in this model.")
    }
  }

  if (layers[1] < 0) {
    n <- textModelLayers(model)
    layers <- 1 + n + layers
    layers
  }


  # Select all character variables and make them UTF-8 coded (e.g., BERT wants it that way).
  data_character_variables <- select_character_v_utf8(texts)


  # Context (default): gives sorted word embeddings based on context
  # (i.e., the entire text is sent to the transformer model)
  if (!decontextualize) {
    x <- data_character_variables
    sorted_layers_ALL_variables <- list()
    sorted_layers_ALL_variables$context_tokens <- list()
    # Loop over all character variables; i_variables = 1
    for (i_variables in seq_len(length(data_character_variables))) {
      T1_variable <- Sys.time()

      # Python file function to HuggingFace
      hg_embeddings <- hgTransformerGetEmbedding(
        text_strings = x[[i_variables]],
        model = model,
        layers = layers,
        return_tokens = return_tokens,
        device = reticulate::r_to_py(device),
        tokenizer_parallelism = tokenizer_parallelism,
        model_max_length = model_max_length,
        max_token_to_sentence = max_token_to_sentence,
        hg_gated = reticulate::r_to_py(hg_gated),
        hg_token = reticulate::r_to_py(hg_token),
        trust_remote_code = trust_remote_code,
        logging_level = logging_level
      )

      if (sort) {
        variable_x <- sortingLayers(
          x = hg_embeddings,
          layers = layers,
          return_tokens = return_tokens
        )
      } else {
        variable_x <- hg_embeddings
      }

      sorted_layers_ALL_variables$context_tokens[[i_variables]] <- variable_x
      names(sorted_layers_ALL_variables$context_tokens)[[i_variables]] <- names(x)[[i_variables]]


      # Adding informative comment
      layers_string <- paste(as.character(layers), sep = " ", collapse = " ")

      comment(sorted_layers_ALL_variables$context_tokens) <-
        paste("Information about the embeddings. textEmbedRawLayers: ",
              "model: ", model, " ; ",
              "layers: ", layers_string, " ; ",
              "word_type_embeddings: ", word_type_embeddings, " ; ",
              "max_token_to_sentence: ", max_token_to_sentence, " ; ",
              "text_version: ", packageVersion("text"), ".",
              sep = "",
              collapse = "\n"
        )

      ## Timing
      T2_variable <- Sys.time()
      variable_time <- T2_variable - T1_variable
      variable_time <- sprintf(
        "duration: %f %s).",
        variable_time,
        units(variable_time)
      )

      version_seq <- paste(i_variables, "/", length(data_character_variables), sep = "")

      loop_text <- paste("Completed layers output for ",
                         names(texts)[[i_variables]], " (variable: ",
                         version_seq, ", ",
                         variable_time,
                         "\n",
                         sep = ""
      )

      message(colourise(loop_text, "green"))
    }
  }

  # Word type embeddings based on Context embeddings
  if (word_type_embeddings && !decontextualize) {
    # see stop in the beginning of this function.

    individual_tokens <- list()
    individual_tokens$context_word_type <- list()
    individual_tokens$tokens <- list()

    # 1. Group individual tokens help(bind_rows)
    i_we <- suppressWarnings(dplyr::bind_rows(sorted_layers_ALL_variables$context_tokens))
    i_we2 <- dplyr::group_split(i_we, i_we[, grep("^tokens", names(i_we))][[1]])
    names(i_we2) <- paste(rep("word_type", length(i_we2)), seq_len(length(i_we2)), sep = "")
    individual_tokens$context_word_type <- i_we2

    # Specify which token layers go together and ensure that the token_id starts with 1
    # (for textLayersAggregation to know which layers are linked);
    num_layers <- length(layers)

    # Look over all token list objects to adjust the token_id. i_context = 1
    for (i_context in seq_len(length(individual_tokens$context_word_type))) { # $word_type
      token_id_df <- individual_tokens$context_word_type[[i_context]] # $word_type

      token_id_variable <- token_id_df[, grep("^token_id", names(token_id_df))][[1]]

      num_token <- length(token_id_variable) / num_layers
      token_id <- sort(rep(1:num_token, num_layers))
      individual_tokens$context_word_type[[i_context]][, grep("^token_id", names(token_id_df))][[1]] <- token_id
    }

    # Get first element from each list.
    single_words <- sapply(individual_tokens$context_word_type, function(x) x[[1]][1]) # $word_type
    single_words <- tibble::as_tibble_col(single_words, column_name = "words")

    # n
    n <- sapply(individual_tokens$context_word_type, function(x) length(x[[1]]) / num_layers) # $word_type
    n <- tibble::as_tibble_col(n, column_name = "n")
    single_words_n <- dplyr::bind_cols(single_words, n)
    individual_tokens$tokens <- single_words_n

    sing_text <- c("Completed layers aggregation for word_type_embeddings. \n")
    message(colourise(sing_text, "green"))
  }

  # Decontextualized embeddings for aggregated embeddings and word type embeddings
  if (decontextualize) {
    individual_tokens <- list()
    individual_tokens$decontext <- list()
    # Get word embeddings for all individual tokens/words (which is, e.g., used for the word plot).help(bind_cols)
    data_character_variables1 <- suppressMessages(apply(data_character_variables, 1, bind_cols)) %>%
      bind_rows()

    singlewords <- getUniqueWordsAndFreq(data_character_variables1[[1]],
                                         hg_tokenizer = model,
                                         device = device
    )
    list_words <- sapply(singlewords$words, list)
    names(list_words) <- NULL


    hg_decontexts_embeddings <- hgTransformerGetEmbedding(
      text_strings = list_words,
      model = model,
      layers = layers,
      return_tokens = return_tokens,
      device = reticulate::r_to_py(device),
      tokenizer_parallelism = tokenizer_parallelism,
      model_max_length = model_max_length,
      max_token_to_sentence = max_token_to_sentence,
      hg_gated = reticulate::r_to_py(hg_gated),
      hg_token = reticulate::r_to_py(hg_token),
      logging_level = logging_level
    )

    # Sort out layers as above
    if (sort) {
      individual_tokens$decontext$word_type <- sortingLayers(
        x = hg_decontexts_embeddings,
        layers = layers,
        return_tokens = return_tokens
      )
    } else {
      individual_tokens$decontext$word_type <- hg_decontexts_embeddings
    }

    names(individual_tokens$decontext$word_type) <- NULL
    individual_tokens$decontext$single_words <- singlewords


    # Adding informative data
    layers_string <- paste(as.character(layers), sep = " ", collapse = " ")
    comment(individual_tokens$decontext$word_type) <- c(paste("Information about the embeddings.
                                                                         textEmbedRawLayers: ",
                                                              "model:", model,
                                                              "layers:", layers_string, ".",
                                                              collapse = " ; "
    ))

    comment(individual_tokens$decontext$single_words) <- c(paste("Information about the embeddings.
                                                                            textEmbedRawLayers: ",
                                                                 "model:", model,
                                                                 "layers:", layers_string, ".",
                                                                 collapse = " ; "
    ))

    de_text <- c("Completed layers aggregation for decontexts embeddings. \n")
    message(colourise(de_text, "green"))

    individual_tokens
  }

  # Combine previous list and word list
  if (decontextualize == FALSE && word_type_embeddings == FALSE) {
    word_embeddings_with_layers <- c(sorted_layers_ALL_variables)
    rm(sorted_layers_ALL_variables)
  } else if (decontextualize == FALSE && word_type_embeddings == TRUE && keep_token_embeddings == TRUE) {
    word_embeddings_with_layers <- c(sorted_layers_ALL_variables, individual_tokens)
    rm(sorted_layers_ALL_variables)
    rm(individual_tokens)
  } else if (decontextualize == FALSE && word_type_embeddings == TRUE && keep_token_embeddings == FALSE) {
    word_embeddings_with_layers <- c(individual_tokens)
    rm(sorted_layers_ALL_variables)
    rm(individual_tokens)
  } else if (decontextualize == TRUE && word_type_embeddings == TRUE) {
    word_embeddings_with_layers <- c(individual_tokens)
    rm(individual_tokens)
  }

  return(word_embeddings_with_layers)
}



#' Aggregate layers
#'
#' textEmbedLayerAggregation selects and aggregates layers of hidden states to form a word embedding.
#' @param word_embeddings_layers Layers returned by the textEmbedRawLayers function.
#' @param layers (character or numeric) The numbers of the layers to be aggregated
#' (e.g., c(11:12) to aggregate the eleventh and twelfth).
#' Note that layer 0 is the input embedding to the transformer, and should normally not be used.
#' Selecting 'all' thus removes layer 0 (default = "all")
#' @param aggregation_from_layers_to_tokens (character) Method to carry out the aggregation among
#' the layers for each word/token, including "min", "max" and "mean" which takes the minimum,
#' maximum or mean across each column; or "concatenate", which links together each layer of the
#' word embedding to one long row (default = "concatenate").
#' @param aggregation_from_tokens_to_texts (character) Method to carry out the aggregation among the word embeddings
#' for the words/tokens, including "min", "max" and "mean" which takes the minimum, maximum or mean across each column;
#' or "concatenate", which links together each layer of the word embedding to one long row (default = "mean").
#' @param return_tokens (boolean) If TRUE, provide the tokens used in the specified transformer model (default = FALSE).
#' @param tokens_select (character) Option to only select embeddings linked to specific tokens
#' in the textEmbedLayerAggregation() phase such as "[CLS]" and "[SEP]" (default NULL).
#' @param tokens_deselect (character) Option to deselect embeddings linked to specific tokens in
#'  the textEmbedLayerAggregation() phase such as "[CLS]" and "[SEP]" (default NULL).
#' @return A tibble with word embeddings. Note that layer 0 is the input embedding to
#' the transformer, which is normally not used.
#' @examples
#' # Aggregate the hidden states from textEmbedRawLayers
#' # to create a word embedding representing the entire text.
#' # This is achieved by concatenating layer 11 and 12.
#' \dontrun{
#' word_embedding <- textEmbedLayerAggregation(
#'   imf_embeddings_11_12$context_tokens,
#'   layers = 11:12,
#'   aggregation_from_layers_to_tokens = "concatenate",
#'   aggregation_from_tokens_to_texts = "mean"
#' )
#'
#' # Examine word_embedding
#' word_embedding
#' }
#' @seealso See \code{\link{textEmbedRawLayers}} and \code{\link{textEmbed}}.
#' @importFrom dplyr %>% bind_rows
#' @export
textEmbedLayerAggregation <- function(
    word_embeddings_layers,
    layers = "all",
    aggregation_from_layers_to_tokens = "concatenate",
    aggregation_from_tokens_to_texts = "mean",
    return_tokens = FALSE,
    tokens_select = NULL,
    tokens_deselect = NULL) {
  if (return_tokens == TRUE && !is.null(aggregation_from_tokens_to_texts)) {
    stop(message(
      colourise("return_tokens = TRUE does not work with aggregation_from_tokens_to_texts not being NULL ", fg = "red"),
      colourise("When aggregating tokens to text, it is not possible to have return_token = TRUE.
                To get both token_embeddings and text_embeddings use textEmbed().", fg = "green")
    ))
  }

  # If selecting 'all' layers, find out number of layers to help indicate layer index later in code
  if (is.character(layers)) {
    # Get the first embeddings
    x_layer_unique <- unique(grep_col_by_name_in_list(word_embeddings_layers[[1]], "layer_number"))
    # Get which layers
    layers <- as.numeric(x_layer_unique)
    # Remove layer 0 because it is the input layer for the word embeddings.
    if (layers[1] == 0) {
      layers <- layers[2:length(layers)]
    }
  }

  # Loop over the list of variables; variable_list_i = 1; variable_list_i = 2; remove(variable_list_i)
  selected_layers_aggregated_tibble <- list()
  for (variable_list_i in seq_len(length(word_embeddings_layers))) {
    T1_variable <- Sys.time()

    x <- word_embeddings_layers[[variable_list_i]]

    # This is to ensure x is in a list (this is to make it work for single word embedddings that are contextualised)
    if (tibble::is_tibble(x)) {
      x <- list(x)
    }
    # Go over the lists and select the layers; [[1]] ok to add below x=
    # get number of unique layers in the variable starting with "layer_number"
    number_of_layers <- unique(x[[1]][, grep("^layer_number", names(x[[1]]))][[1]])

    # Check that the right number of levels are selected
    if ((length(setdiff(layers, number_of_layers)) > 0) == TRUE) {
      stop(message(
        colourise("You are trying to aggregate layers that were not extracted.", fg = "red"),
        colourise("For example, in textEmbed the layers option needs to include all the
                  layers used in context_layers.", fg = "green")
      ))
    }

    # Select layers in layers-argument selected from the variable starting with layer_number
    selected_layers <- lapply(x, function(x) {
      x[x[, grep("^layer_number", names(x))][[1]]
        %in% layers, ]
    })

    # Go over the lists and select the tokens (e.g., CLS) (tokens_select = NULL tokens_select = "[CLS]")
    if (!is.null(tokens_select)) {
      selected_layers <- lapply(selected_layers, function(x) {
        x[x[, grep("^tokens", names(x))][[1]]
          %in% tokens_select, ]
      })
    }

    # Go over the lists and DEselect the token (e.g., CLS) (tokens_deselect = NULL tokens_deselect = "[CLS]")
    if (!is.null(tokens_deselect)) {
      selected_layers <- lapply(selected_layers, function(x) {
        x[!x[, grep("^tokens", names(x))][[1]]
          %in% tokens_deselect, ]
      })

      # If any of the tokens that was removed was "[CLS]", subtract one on token_id so it starts with
      # 1 and works with the layer_aggregation_helper
      if (length(tokens_deselect) == 1 && tokens_deselect == "[CLS]") {
        # Subtract
        selected_layers <- purrr::map(selected_layers, function(x) {
          x[, grep("^token_id", names(x))][[1]] <- x[, grep("^token_id", names(x))][[1]] - 1
          x
        })
      } else if (length(tokens_deselect) > 1) {
        if (table(tokens_deselect %in% "[CLS]")[[2]] > 0) {
          # Subtract
          selected_layers <- purrr::map(selected_layers, function(x) {
            # select variable starting with "token_id" and substract 1
            x[, grep("^token_id", names(x))][[1]] <- x[, grep("^token_id", names(x))][[1]] - 1
            x
          })
        }
      }
    }

    ## Aggregate across layers; i_token_id=1 aggregation_from_layers_to_tokens="min"
    selected_layers_aggregated <- lapply(selected_layers,
                                         layer_aggregation_helper,
                                         aggregation = aggregation_from_layers_to_tokens,
                                         return_tokens = return_tokens
    )

    if (is.null(aggregation_from_tokens_to_texts)) {
      # Sort output
      selected_layers_aggregated_tibble[[variable_list_i]] <- selected_layers_aggregated
    }

    # Aggregate across tokens
    if (!is.null(aggregation_from_tokens_to_texts)) {
      selected_layers_tokens_aggregated <- lapply(selected_layers_aggregated,
                                                  textEmbeddingAggregation,
                                                  aggregation = aggregation_from_tokens_to_texts
      )
      # Sort output
      selected_layers_aggregated_tibble[[variable_list_i]] <- dplyr::bind_rows(selected_layers_tokens_aggregated)
    }


    # Add informative comments
    original_comment <- comment(word_embeddings_layers)
    layers_string <- paste(as.character(layers), sep = " ", collapse = " ")
    comment(selected_layers_aggregated_tibble[[variable_list_i]]) <- paste(original_comment,
                                                                           "textEmbedLayerAggregation: layers = ",
                                                                           layers_string,
                                                                           "aggregation_from_layers_to_tokens = ",
                                                                           aggregation_from_layers_to_tokens,
                                                                           "aggregation_from_tokens_to_texts = ",
                                                                           aggregation_from_tokens_to_texts,
                                                                           "tokens_select = ",
                                                                           tokens_select,
                                                                           "tokens_deselect = ",
                                                                           tokens_deselect,
                                                                           collapse = " ; "
    )

    ## Timing
    T2_variable <- Sys.time()
    variable_time <- T2_variable - T1_variable
    variable_time <- sprintf(
      "duration: %f %s).",
      variable_time,
      units(variable_time)
    )

    version_seq <- paste(variable_list_i, "/", length(word_embeddings_layers), sep = "")

    loop_text <- paste("Completed layers aggregation", " (variable ",
                       version_seq, ", ",
                       variable_time,
                       "\n",
                       sep = ""
    )

    message(colourise(loop_text, "blue"))
  }

  names(selected_layers_aggregated_tibble) <- names(word_embeddings_layers)
  selected_layers_aggregated_tibble
}


#' Generate_placement_vector input from textEmbedRawLayers and inserts NA-placeholder vectors for NA values.
#' @param raw_layers Layers returned by the textEmbedRawLayers function with NA values.
#' @return Layers returned by the textEmbedRawLayers with inserted NA-placeholder vectors.
#' @noRd
generate_placement_vector <- function(raw_layers,
                                      texts) {
  # Extract column name, if there is one.
  column_name <- colnames(texts)

  context_tokens <- NULL

  if (!is.null(raw_layers$context_tokens$value)) {
    context_tokens <- raw_layers$context_tokens$value
  }
  # If raw_layers$context_tokens$value is NULL, check if raw_layers$context_tokens$texts is not NULL and use it

  if (!is.null(raw_layers$context_tokens$texts)) {
    context_tokens <- raw_layers$context_tokens$texts
  }

  # Try with column name
  if (!is.null(column_name)) {
    context_tokens <- raw_layers$context_tokens[[column_name]]
  }

  # Check if raw_layers$context_tokens$value is not NULL, and use it
  if (is.null(context_tokens)) {
    stop("Neither raw_layers$context_tokens$value nor raw_layers$context_tokens$texts found or both are NULL.")
  }

  # Loop through the hidden states
  for (i in 1:length(context_tokens)) {
    token_embedding <- context_tokens[[i]]

    # Find the corresponding token of each hidden state
    elements <- context_tokens[[i]][1]

    # Check if "na" or "NA" is represented as a token
    if (any(sapply(elements, function(element) "na" %in% element)) ||
        any(sapply(elements, function(element) "NA" %in% element))) {
      # If so, then check for "NA" or "na" in the token-embedding
      if (any(grepl("na", token_embedding$tokens, ignore.case = TRUE)) ||
          any(grepl("NA", token_embedding$tokens, ignore.case = TRUE))) {
        # Store the dimensions of the token-embedding with NA:s
        dimensions <- dim(context_tokens[[i]])
      }
    }
  }

  # Create a placeholder tibble with NA values of the same shape as the original token embedding
  template_na <- as_tibble(matrix(NA, nrow = dimensions[1], ncol = dimensions[2] - 2))
  colnames(template_na) <- c("tokens", paste0("Dim", 1:(dimensions[2] - 3)))

  # Create a list to store the modified embeddings
  modified_embeddings <- list()

  # Iterate over each context token in the original embedding list
  for (i in 1:length(context_tokens)) {
    token_embedding <- context_tokens[[i]]
    elements <- context_tokens[[i]][1]

    # Check if "na" or "" is present in any element of the list
    if ((((any(sapply(elements, function(element) "na" %in% element)) ||
         any(sapply(elements, function(element) "NA" %in% element))) &&
         nrow(token_embedding) == 3))||
         nrow(token_embedding) == 2){

      # If so, then check for "na" (or "") in the token-embedding
      if (any(grepl("na", token_embedding$tokens, ignore.case = TRUE)) ||
          any(grepl("NA", token_embedding$tokens, ignore.case = TRUE)) ||
          length(token_embedding$tokens) == 2) {
        # Replace only the numerical columns with NA values while keeping the first three columns
        token_embedding[, -(1:3)] <- NA # Exclude the first three columns
      }
    }
    modified_embeddings[[i]] <- token_embedding
  }

  # Replace the original layers with the modified
  if (!is.null(raw_layers$context_tokens$value)) {
    raw_layers$context_tokens$value <- modified_embeddings
  }
  if (!is.null(raw_layers$context_tokens$texts)) {
    raw_layers$context_tokens$texts <- modified_embeddings
  }
  if (!is.null(raw_layers$context_tokens[[column_name]])) {
    raw_layers$context_tokens[[column_name]] <- modified_embeddings
  }

  return(raw_layers)
}

#' The number of layers to retrieve
#' @param layers The number of layers to retrieve.
#' @return The number of layers to us (if -2; i.e., the second to last layer)
#' @noRd
find_layer_number <- function(
    model,
    layers,
    hg_gated,
    hg_token){

  if (layers[1] < 0) {
    n <- textModelLayers(model, reticulate::r_to_py(hg_gated), reticulate::r_to_py(hg_token))
    layers <- 1 + n + layers
  }

  return(layers)
}




#' Helper function for textEmbed
#'
#' textEmbed() extracts layers and aggregate them to word embeddings, for all character variables in a given dataframe.
#' @param texts A character variable or a tibble/dataframe with at least one character variable.
#' @param model Character string specifying pre-trained language model (default 'bert-base-uncased').
#'  For full list of options see pretrained models at
#'  \href{https://huggingface.co/transformers/pretrained_models.html}{HuggingFace}.
#'  For example use "bert-base-multilingual-cased", "openai-gpt",
#' "gpt2", "ctrl", "transfo-xl-wt103", "xlnet-base-cased", "xlm-mlm-enfr-1024", "distilbert-base-cased",
#' "roberta-base", or "xlm-roberta-base". Only load models that you trust from HuggingFace; loading a
#'  malicious model can execute arbitrary code on your computer).
#' @param layers (string or numeric) Specify the layers that should be extracted
#' (default -2 which give the second to last layer). It is more efficient to only extract the layers
#' that you need (e.g., 11). You can also extract several (e.g., 11:12), or all by setting this parameter
#' to "all". Layer 0 is the decontextualized input layer (i.e., not comprising hidden states) and
#'  thus should normally not be used. These layers can then be aggregated in the textEmbedLayerAggregation
#'  function.
#' @param dim_name (boolean) If TRUE append the variable name after all variable-names in the output.
#' (This differentiates between word embedding dimension names; e.g., Dim1_text_variable_name).
#' see \code{\link{textDimName}} to change names back and forth.
#' @param aggregation_from_layers_to_tokens (string) Aggregated layers of each token. Method to aggregate the
#' contextualized layers (e.g., "mean", "min" or "max, which takes the minimum, maximum or mean, respectively,
#' across each column; or "concatenate", which links  together each word embedding layer to one long row.
#' @param aggregation_from_tokens_to_texts (string) Method to carry out the aggregation among the word embeddings
#' for the words/tokens, including "min", "max" and "mean" which takes the minimum, maximum or mean across each column;
#' or "concatenate", which links together each layer of the word embedding to one long row (default = "mean"). If set to NULL, embeddings are not
#' aggregated.
#' @param aggregation_from_tokens_to_word_types (string) Aggregates to the word type (i.e., the individual words)
#'  rather than texts. If set to "individually", then duplicate words are not aggregated, (i.e, the context of individual
#'  is preserved). (default = NULL).
#' @param keep_token_embeddings (boolean) Whether to also keep token embeddings when using texts or word
#' types aggregation.
#' @param remove_non_ascii (bolean) TRUE warns and removes non-ascii (using textFindNonASCII()).
#' @param tokens_select Option to select word embeddings linked to specific tokens
#' such as [CLS] and [SEP] for the context embeddings.
#' @param tokens_deselect Option to deselect embeddings linked to specific tokens
#' such as [CLS] and [SEP] for the context embeddings.
#' @param decontextualize (boolean) Provide word embeddings of single words as input to the model
#' (these embeddings are, e.g., used for plotting; default is to use ). If using this, then set
#' single_context_embeddings to FALSE.
#' @param model_max_length The maximum length (in number of tokens) for the inputs to the transformer model
#' (default the value stored for the associated model).
#' @param max_token_to_sentence (numeric) Maximum number of tokens in a string to handle before
#' switching to embedding text sentence by sentence.
#' @param tokenizer_parallelism (boolean) If TRUE this will turn on tokenizer parallelism. Default FALSE.
#' @param device (character) Name of device to use: 'cpu', 'gpu', 'gpu:k' or 'mps'/'mps:k'
#' for MacOS, where k is a specific device number. (default = "cpu"). Default is NULL, which sets gpu:0/mps:0
#' depending on OS.
#' @param hg_gated Set to TRUE if the accessed model is gated.
#' @param hg_token The token needed to access the gated model.
#' Create a token from the ['Settings' page](https://huggingface.co/settings/tokens) of
#' the Hugging Face website. An an environment variable HUGGINGFACE_TOKEN can
#' be set to avoid the need to enter the token each time.
#' @param logging_level Set the logging level. Default: "warning".
#' Options (ordered from less logging to more logging): critical, error, warning, info, debug
#' @param ... settings from textEmbedRawLayers().
#' @return A tibble with tokens.
#' @importFrom reticulate source_python r_to_py
#' @importFrom tidyr unnest_wider
#' @noRd
text_embed_dlatk <- function(
    texts,
    model,
    layers,
    dim_name,
    #    aggregation_from_layers_to_tokens = aggregation_from_layers_to_tokens,
    aggregation_from_tokens_to_texts = aggregation_from_tokens_to_texts,
    #    aggregation_from_tokens_to_word_types = aggregation_from_tokens_to_word_types,
    #   keep_token_embeddings = keep_token_embeddings,
    remove_non_ascii = remove_non_ascii,
    #    tokens_select = tokens_select,
    #    tokens_deselect = tokens_deselect,
    #    decontextualize = decontextualize,
    model_max_length = model_max_length,
    #    max_token_to_sentence = max_token_to_sentence,
    tokenizer_parallelism = tokenizer_parallelism,
    device = device,
    hg_gated = hg_gated,
    hg_token = hg_token,
    trust_remote_code = trust_remote_code,
    logging_level = logging_level,
    batch_size = batch_size
    ){

  device = "cpu"
  # Keeping comment consistent with original method based on textEmbedRawLayers and textEmbedLayerAggregation (to enable textPredict with text input).
  layers_string <- paste(as.character(layers), sep = " ", collapse = " ")
  word_type_embeddings = FALSE
  max_token_to_sentence = NULL

  original_comment <- paste("Information about the embeddings. implementation: dlatk ; textEmbedRawLayers: ",
          "model: ", model, " ; ",
          "layers: ", layers_string, " ; ",
          "word_type_embeddings: ", word_type_embeddings, " ; ",
          "max_token_to_sentence: ", max_token_to_sentence, " ; ",
          "text_version: ", packageVersion("text"), ".",
          sep = "",
          collapse = "\n")

  aggregation_from_layers_to_tokens = NULL
  tokens_select = NULL
  tokens_deselect = NULL

  comment_to_save <- paste(
    original_comment,
    "textEmbedLayerAggregation: layers = ",
    layers_string,
    "aggregation_from_layers_to_tokens = ",
    aggregation_from_layers_to_tokens,
    "aggregation_from_tokens_to_texts = ",
    aggregation_from_tokens_to_texts,
    "tokens_select = ",
    tokens_select,
    "tokens_deselect = ",
    tokens_deselect,
    collapse = " ; ")

  if (sum(is.na(texts) > 0)) {
    warning("texts contain NA-values.")
  }

  T1_textEmbed <- Sys.time()

  reticulate::source_python(system.file("python",
                                        "huggingface_Interface3.py",
                                        package = "text",
                                        mustWork = TRUE
  ))


  # Number of layers to retrieve (if -2 is given; i.e., getting the second to last layer)
  layers <- find_layer_number(model, layers, hg_gated, hg_token)
  layers <- reticulate::r_to_py(as.integer(layers))

  # Select all character variables and make them UTF-8 coded (e.g., BERT wants it that way).
  data_character_variables <- select_character_v_utf8(texts)

  # Check for ASCII characters
  problematic_texts <- textFindNonASCII(data_character_variables)

  #### Clean ASCII ####
  if(nrow(problematic_texts)>0){
    data_character_variables <- textCleanNonASCIIinfo(
      data_tibble = data_character_variables,
      problematic_texts = problematic_texts,
      remove_non_ascii = remove_non_ascii
    )
  }


  #### Get Layers & Aggregate layers ####
  outcome_list <- list()
  # text_i = 1
  for (text_i in 1:ncol(data_character_variables)) {
    texts <- data_character_variables[[text_i]]

    dlatk_emb <- hgDLATKTransformerGetEmbedding(
      text_strings = texts, # texts,
      #text_ids = NULL,
      #group_ids = NULL,
      model = model,
      layers = layers,
      #    return_tokens = True,
      #    max_token_to_sentence = 4,
       device = device,
       tokenizer_parallelism = tokenizer_parallelism,
       model_max_length = model_max_length,
       hg_gated = hg_gated,
       hg_token = hg_token,
       trust_remote_code = trust_remote_code,
       logging_level = logging_level,
      #    sentence_tokenize = True
       batch_size = 1L, #as.numeric(batch_size),
       aggregations = aggregation_from_tokens_to_texts
    )

    dlatk_emb_message <- dlatk_emb #[[1]] This is only needed if the pyhon function return "return msg_embeddings, cf_embeddings"

    # Extract first embedding from each list item
    dlatk_emb_message <- lapply(dlatk_emb_message, function(x) unlist(x[[1]]))

    # Convert to tibble: 1 row per embedding, 1024 columns
    dlatk_emb_message <- tibble(values = dlatk_emb_message) %>%
      tidyr::unnest_wider(values, names_sep = "_", names_repair = "unique")


    # Rename columns to Dim1, Dim2, ...
    colnames(dlatk_emb_message) <- paste0("Dim", seq_along(dlatk_emb_message))



    T2_textEmbed <- Sys.time()
    Time_textEmbed <- T2_textEmbed - T1_textEmbed
    Time_textEmbed <- sprintf("Duration to embed text: %f %s", Time_textEmbed, units(Time_textEmbed))
    Date_textEmbed <- Sys.time()

    comment(dlatk_emb_message) <- comment_to_save

    outcome_list$texts[[text_i]] <- dlatk_emb_message

    names(outcome_list$texts)[[text_i]] <- names(data_character_variables)[[text_i]]

    if (dim_name == TRUE) {
      outcome_list$texts[text_i] <- textDimName(outcome_list$texts[text_i])
    }
  }

  comment(outcome_list) <- paste(
    Time_textEmbed,
    "; Date created: ", Date_textEmbed,
    "; text_version: ", packageVersion("text"),
    " ; implementation = TRUE", ".",
    sep = "",
    collapse = " ")
  return(outcome_list)
}

#' Helper function for textEmbed
#'
#' textEmbed() extracts layers and aggregate them to word embeddings, for all character variables in a given dataframe.
#' @param texts A character variable or a tibble/dataframe with at least one character variable.
#' @param model Character string specifying pre-trained language model (default 'bert-base-uncased').
#'  For full list of options see pretrained models at
#'  \href{https://huggingface.co/transformers/pretrained_models.html}{HuggingFace}.
#'  For example use "bert-base-multilingual-cased", "openai-gpt",
#' "gpt2", "ctrl", "transfo-xl-wt103", "xlnet-base-cased", "xlm-mlm-enfr-1024", "distilbert-base-cased",
#' "roberta-base", or "xlm-roberta-base". Only load models that you trust from HuggingFace; loading a
#'  malicious model can execute arbitrary code on your computer).
#' @param layers (string or numeric) Specify the layers that should be extracted
#' (default -2 which give the second to last layer). It is more efficient to only extract the layers
#' that you need (e.g., 11). You can also extract several (e.g., 11:12), or all by setting this parameter
#' to "all". Layer 0 is the decontextualized input layer (i.e., not comprising hidden states) and
#'  thus should normally not be used. These layers can then be aggregated in the textEmbedLayerAggregation
#'  function.
#' @param dim_name (boolean) If TRUE append the variable name after all variable-names in the output.
#' (This differentiates between word embedding dimension names; e.g., Dim1_text_variable_name).
#' see \code{\link{textDimName}} to change names back and forth.
#' @param aggregation_from_layers_to_tokens (string) Aggregated layers of each token. Method to aggregate the
#' contextualized layers (e.g., "mean", "min" or "max, which takes the minimum, maximum or mean, respectively,
#' across each column; or "concatenate", which links  together each word embedding layer to one long row.
#' @param aggregation_from_tokens_to_texts (string) Method to carry out the aggregation among the word embeddings
#' for the words/tokens, including "min", "max" and "mean" which takes the minimum, maximum or mean across each column;
#' or "concatenate", which links together each layer of the word embedding to one long row (default = "mean"). If set to NULL, embeddings are not
#' aggregated.
#' @param aggregation_from_tokens_to_word_types (string) Aggregates to the word type (i.e., the individual words)
#'  rather than texts. If set to "individually", then duplicate words are not aggregated, (i.e, the context of individual
#'  is preserved). (default = NULL).
#' @param keep_token_embeddings (boolean) Whether to also keep token embeddings when using texts or word
#' types aggregation.
#' @param remove_non_ascii (bolean) TRUE warns and removes non-ascii (using textFindNonASCII()).
#' @param tokens_select Option to select word embeddings linked to specific tokens
#' such as [CLS] and [SEP] for the context embeddings.
#' @param tokens_deselect Option to deselect embeddings linked to specific tokens
#' such as [CLS] and [SEP] for the context embeddings.
#' @param decontextualize (boolean) Provide word embeddings of single words as input to the model
#' (these embeddings are, e.g., used for plotting; default is to use ). If using this, then set
#' single_context_embeddings to FALSE.
#' @param model_max_length The maximum length (in number of tokens) for the inputs to the transformer model
#' (default the value stored for the associated model).
#' @param max_token_to_sentence (numeric) Maximum number of tokens in a string to handle before
#' switching to embedding text sentence by sentence.
#' @param tokenizer_parallelism (boolean) If TRUE this will turn on tokenizer parallelism. Default FALSE.
#' @param device (character) Name of device to use: 'cpu', 'gpu', 'gpu:k' or 'mps'/'mps:k'
#' for MacOS, where k is a specific device number. (default = "cpu"). Default is NULL, which sets gpu:0/mps:0
#' depending on OS.
#' @param hg_gated Set to TRUE if the accessed model is gated.
#' @param hg_token The token needed to access the gated model.
#' Create a token from the ['Settings' page](https://huggingface.co/settings/tokens) of
#' the Hugging Face website. An an environment variable HUGGINGFACE_TOKEN can
#' be set to avoid the need to enter the token each time.
#' @param logging_level Set the logging level. Default: "warning".
#' Options (ordered from less logging to more logging): critical, error, warning, info, debug
#' @param ... settings from textEmbedRawLayers().
#' @return A tibble with tokens, a column for layer identifier and word embeddings.
#' Note that layer 0 is the input embedding to the transformer.
#' @examples
#' # Automatically transforms the characters in the example dataset:
#' # Language_based_assessment_data_8 (included in text-package), to embeddings.
#' \dontrun{
#' word_embeddings <- textEmbed(Language_based_assessment_data_8[1:2, 1:2],
#'   layers = 10:11,
#'   aggregation_from_layers_to_tokens = "concatenate",
#'   aggregation_from_tokens_to_texts = "mean",
#'   aggregation_from_tokens_to_word_types = "mean"
#' )
#'
#' # Show information about how the embeddings were constructed.
#' comment(word_embeddings$texts$satisfactiontexts)
#' comment(word_embeddings$word_types)
#' comment(word_embeddings$tokens$satisfactiontexts)
#'
#' # See how the word embeddings are structured.
#' word_embeddings
#'
#' # Save the word embeddings to avoid having to embed the text again.
#' saveRDS(word_embeddings, "word_embeddings.rds")
#'
#' # Retrieve the saved word embeddings.
#' word_embeddings <- readRDS("word_embeddings.rds")
#' }
#'
#' @seealso See \code{\link{textEmbedLayerAggregation}}, \code{\link{textEmbedRawLayers}} and
#' \code{\link{textDimName}}.
#' @importFrom reticulate source_python
#' @importFrom utils modifyList
#' @noRd
text_embed <- function(
    texts,
    model,
    layers,
    dim_name,
    aggregation_from_layers_to_tokens,
    aggregation_from_tokens_to_texts,
    aggregation_from_tokens_to_word_types,
    keep_token_embeddings = T,
    remove_non_ascii = TRUE,
    tokens_select = NULL,
    tokens_deselect = NULL,
    decontextualize = FALSE,
    model_max_length = NULL,
    max_token_to_sentence = 4,
    tokenizer_parallelism = FALSE,
    device = "cpu",
    hg_gated = FALSE,
    hg_token = Sys.getenv("HUGGINGFACE_TOKEN",
                          unset = ""),
    trust_remote_code = F,
    sort = T,
    logging_level = "error",
    ...) {

  if (sum(is.na(texts) > 0)) {
    warning("texts contain NA-values.")
  }

  T1_textEmbed <- Sys.time()

  reticulate::source_python(system.file("python",
                                        "huggingface_Interface3.py",
                                        package = "text",
                                        mustWork = TRUE
  ))

  if (
    (decontextualize == TRUE && is.null(aggregation_from_tokens_to_texts)) ||
    (decontextualize == TRUE && is.null(aggregation_from_tokens_to_word_types)) ||
    (decontextualize == TRUE && is.null(aggregation_from_layers_to_tokens))) {
    stop(message(
      colourise("When using decontextualize = TRUE, it is required to set aggregation_from_tokens_to_texts,
                aggregation_from_tokens_to_word_types as well as aggregation_from_tokens_to_word_types",
                fg = "red"
      ),
      colourise("This is because both the token embeddings and text embeddings are
                constrcuted from the word type embeddings.",
                fg = "green"
      )
    ))
  }

  output <- list()

  # Number of layers to retrieve (if -2 is given; i.e., getting the second to last layer)
  layers <- find_layer_number(model, layers, hg_gated, hg_token)

  # Select all character variables and make them UTF-8 coded (e.g., BERT wants it that way).
  data_character_variables <- select_character_v_utf8(texts)

  # Check for ASCII characters
  problematic_texts <- textFindNonASCII(data_character_variables)

  #### Clean ASCII ####
  if(nrow(problematic_texts)>0){
    data_character_variables <- textCleanNonASCIIinfo(
      data_tibble = data_character_variables,
      problematic_texts = problematic_texts,
      remove_non_ascii = remove_non_ascii
      )
  }


  #### Get Layers & Aggregate layers ####
  outcome_list <- list()
  #text_i = 1
  for (text_i in 1:ncol(data_character_variables)) {
    texts <- data_character_variables[text_i]

    # Get hidden states/layers for output 1 and/or output 2 or decontextualized;
    if (!is.null(aggregation_from_layers_to_tokens) ||
        !is.null(aggregation_from_tokens_to_texts) ||
        decontextualize) {

      all_wanted_layers <- textEmbedRawLayers(
        texts = texts,
        model = model,
        layers = layers,
        return_tokens = TRUE,
        word_type_embeddings = TRUE,
        decontextualize = decontextualize,
        keep_token_embeddings = T, ######
        device = device,
        tokenizer_parallelism = tokenizer_parallelism,
        model_max_length = model_max_length,
        max_token_to_sentence = max_token_to_sentence,
        hg_gated = hg_gated,
        hg_token = hg_token,
        logging_level = logging_level,
        trust_remote_code = trust_remote_code,
        sort = sort
      )
    }

    # Generate placement vectors if there are NA:s in texts.
    if (sum(is.na(texts) > 0)) {
      all_wanted_layers <- generate_placement_vector(
        raw_layers = all_wanted_layers,
        texts = texts
      )
    }

    if (!decontextualize) {
      # 1. Get token-level embeddings with aggregated levels
      if (!is.null(aggregation_from_layers_to_tokens) && keep_token_embeddings) {
        token_embeddings <- textEmbedLayerAggregation(
          word_embeddings_layers = all_wanted_layers$context_tokens,
          layers = layers,
          aggregation_from_layers_to_tokens = aggregation_from_layers_to_tokens,
          aggregation_from_tokens_to_texts = NULL,
          return_tokens = TRUE, # need to be TRUE
          tokens_select = tokens_select,
          tokens_deselect = tokens_deselect
        )
        output$tokens <- token_embeddings
      }

      # 2. Get aggregated token layers; aggregation_from_tokens_to_texts = "mean"
      if (!is.null(aggregation_from_tokens_to_texts)) {
        aggregated_token_embeddings <- textEmbedLayerAggregation(
          word_embeddings_layers = all_wanted_layers$context_tokens,
          layers = layers,
          aggregation_from_layers_to_tokens = aggregation_from_layers_to_tokens,
          aggregation_from_tokens_to_texts = aggregation_from_tokens_to_texts,
          return_tokens = FALSE, # need to be FALSE
          tokens_select = tokens_select,
          tokens_deselect = tokens_deselect
        )
        output$texts <- aggregated_token_embeddings
      }
    }


    # 3. Aggregate Word Type (both decontextualised or not)
    if (!is.null(aggregation_from_tokens_to_word_types) || decontextualize) {
      if (!decontextualize) {
        single_context_text <- paste("Embedding single context embeddings.",
                                     "\n",
                                     sep = ""
        )
        message(colourise(single_context_text, "purple"))

        ##############################################################################
        # These are the word_type embeddings with duplicates #########################
        ##############################################################################

        individual_word_embeddings_layers <- all_wanted_layers$context_word_type
        individual_words <- all_wanted_layers$tokens
      }

      if (decontextualize) {
        single_context_text <- paste("Embedding decontextualised embeddings.",
                                     "\n",
                                     sep = ""
        )
        message(colourise(single_context_text, "purple"))

        individual_word_embeddings_layers <- all_wanted_layers$decontext$word_type
        individual_words <- all_wanted_layers$decontext$single_words
      }

      # Temporarily switch aggregation_from_tokens_to_word_types to NULL
      if (aggregation_from_tokens_to_word_types == "individually"){
        original_aggregation_from_tokens_to_texts = aggregation_from_tokens_to_texts
        aggregation_from_tokens_to_texts = NULL
      }

      individual_word_embeddings <- textEmbedLayerAggregation(
        word_embeddings_layers = individual_word_embeddings_layers,
        layers = layers,
        aggregation_from_layers_to_tokens = aggregation_from_layers_to_tokens,
        aggregation_from_tokens_to_texts = aggregation_from_tokens_to_texts,
        return_tokens = FALSE,
        tokens_select = tokens_select,
        tokens_deselect = tokens_deselect
      )

      # Switch back aggregation_from_tokens_to_word_type
      if (aggregation_from_tokens_to_word_types == "individually"){
        aggregation_from_tokens_to_texts = original_aggregation_from_tokens_to_texts
      }

      individual_word_embeddings <- dplyr::bind_rows(individual_word_embeddings)

      # Combine the words for each decontextualized embedding
      # Instead of aggregating word_type embeddings, keep them.
      # In order to do so, the size of "individual_words" must match that of "individual_word_embeddings"
      if (aggregation_from_tokens_to_word_types == "individually"){

        individual_words <- tibble::tibble(individual_words)

        # num rows
        row_indices <- rep(seq_along(individual_words$n), individual_words$n)

        # if a word occurs more than once, then, insert that word n times to match the size of "individual_word_embeddings"
        individual_words <- individual_words[row_indices, ] %>%
          mutate(id = seq_along(n), #id column
                 n = 1)
      }

      # Combine the words for each decontextualized embedding
      individual_word_embeddings_words <- dplyr::bind_cols(
        individual_words, # all_wanted_layers$decontext$single_words,
        individual_word_embeddings
      )

      comment(individual_word_embeddings_words) <- paste(
        comment(all_wanted_layers$context_tokens),
        comment(individual_word_embeddings),
        " ; aggregation_from_tokens_to_word_types = ", aggregation_from_tokens_to_word_types,
        " ; decontextualize = ", decontextualize,
        " ; implementation = FALSE"
      )

      individual_word_embeddings_words <- list(individual_word_embeddings_words)
      names(individual_word_embeddings_words) <- colnames(texts)
      output$word_types <- individual_word_embeddings_words
      message(colourise("Done! \n", "purple"))
    }


    #### Decontextualised tokens and text embeddings (using output from 3 above) ####
    if (decontextualize) {
      decontext_space <- individual_word_embeddings_words

      ### 1. Get token-level embeddings with aggregated levels
      if (!is.null(aggregation_from_layers_to_tokens)) { #  & keep_token_embeddings

        # Tokenize texts
        output <- list()
        token_embeddings_list <- list()
        token_embeddings_list$tokens <- list()
        if (!tibble::is_tibble(texts)) {
          texts <- tibble::as_tibble(texts)
        }

        for (i_variables in seq_len(ncol(texts))) {
          text_tokens <- lapply(texts[[i_variables]], textTokenize,
                                model = model, max_token_to_sentence = max_token_to_sentence
          ) # , ...

          t_embeddings <- lapply(text_tokens, applysemrep_over_words, decontext_space[[1]], tolower = FALSE)

          token_embeddings <- mapply(dplyr::bind_cols, text_tokens, t_embeddings, SIMPLIFY = FALSE)

          token_embeddings_list$tokens[[i_variables]] <- token_embeddings
          names(token_embeddings_list$tokens[[i_variables]]) <- paste(names(texts)[[i_variables]],
                                                                      seq_len(length(token_embeddings)),
                                                                      sep = "_"
          )
        }
        output$tokens <- token_embeddings_list$tokens
      }

      # 2. Get aggregated token layers; aggregation_from_tokens_to_texts = "mean"
      if (!is.null(aggregation_from_tokens_to_texts)) {
        # Function to add dummy variable token_id and layer_number because textEmbedLayerAggregation
        # expect that (and it is missing here because the dectontextualize method is used)
        add_columns <- function(df) {
          df$token_id <- seq_len(nrow(df))
          df$layer_number <- rep(1, nrow(df))
          return(df)
        }
        token_embeddings1 <- lapply(token_embeddings, add_columns)

        text_embeddings <- textEmbedLayerAggregation(token_embeddings1,
                                                     aggregation_from_tokens_to_texts = aggregation_from_tokens_to_texts,
                                                     return_tokens = FALSE
                                                     , ...
        )

        output$texts <- text_embeddings
      }
    }

    T2_textEmbed <- Sys.time()
    Time_textEmbed <- T2_textEmbed - T1_textEmbed
    Time_textEmbed <- sprintf("Duration to embed text: %f %s", Time_textEmbed, units(Time_textEmbed))
    Date_textEmbed <- Sys.time()

    if (dim_name == TRUE && !is.null(aggregation_from_tokens_to_texts)) {
      output$texts <- textDimName(output$texts)
    }

    comment(output) <- paste(Time_textEmbed,
                             "; Date created: ", Date_textEmbed,
                             "; text_version: ", packageVersion("text"), ".",
                             sep = "",
                             collapse = " "
    )
    outcome_list[[text_i]] <- output
  }

  if (ncol(data_character_variables) > 1) {
    output1 <- Reduce(utils::modifyList, outcome_list)
  } else {
    output1 <- outcome_list[[1]]
  }

  return(output1)
}


#' Combine results from batches, accounting for tokens, texts, and word_types.
#' @param batch_results The results from each batch
#' @param aggregation The aggregation method
#' @return All character variables in UTF-8 format.
#' @importFrom stringi stri_match stri_trim
#' @importFrom purrr map2 map_chr
#' @importFrom dplyr group_by summarize across
#' @noRd
combine_textEmbed_results <- function(
    batch_results,
    aggregation = "mean") {

  # Initialize combined structure
  combined_results <- list(tokens = list(), texts = list(), word_types = list())
  total_duration <- 0
  creation_dates <- c()
  text_versions <- c()

  # Combine results for each component
  for (result in batch_results) {
    # Combine tokens while preserving names and comments
    if (!is.null(result$tokens)) {
      for (name in names(result$tokens)) {
        if (is.null(combined_results$tokens[[name]])) {
          combined_results$tokens[[name]] <- result$tokens[[name]]
          comment(combined_results$tokens[[name]]) <- comment(result$tokens[[name]])
        } else {
          combined_results$tokens[[name]] <- c(
            combined_results$tokens[[name]],
            result$tokens[[name]]
          )
          # Concatenate unique comments
          current_comment <- comment(combined_results$tokens[[name]])
          new_comment <- comment(result$tokens[[name]])
          comment(combined_results$tokens[[name]]) <- unique(c(
            current_comment,
            new_comment
          )) %>% paste(collapse = " | ")
        }
      }
    }

    # Combine texts while preserving names and comments
    if (!is.null(result$texts)) {
      for (name in names(result$texts)) {
        if (is.null(combined_results$texts[[name]])) {
          combined_results$texts[[name]] <- result$texts[[name]]
          comment(combined_results$texts[[name]]) <- comment(result$texts[[name]])
        } else {
          combined_results$texts[[name]] <- dplyr::bind_rows(
            combined_results$texts[[name]],
            result$texts[[name]]
          )
          # Concatenate unique comments
          current_comment <- comment(combined_results$texts[[name]])
          new_comment <- comment(result$texts[[name]])
          comment(combined_results$texts[[name]]) <- unique(c(
            current_comment,
            new_comment
          )) %>% paste(collapse = " | ")
        }
      }
    }

    # Combine word_types while preserving names and comments
    if (!is.null(result$word_types)) {
      for (name in names(result$word_types)) {
        if (is.null(combined_results$word_types[[name]])) {
          combined_results$word_types[[name]] <- result$word_types[[name]]
          comment(combined_results$word_types[[name]]) <- comment(result$word_types[[name]])
        } else {
          combined_results$word_types[[name]] <- dplyr::bind_rows(
            combined_results$word_types[[name]],
            result$word_types[[name]]
          )
          # Concatenate unique comments
          current_comment <- comment(combined_results$word_types[[name]])
          new_comment <- comment(result$word_types[[name]])
          comment(combined_results$word_types[[name]]) <- unique(c(
            current_comment,
            new_comment
          )) %>% paste(collapse = " | ")
        }
      }
    }

    # Collect top-level comment details
    if (!is.null(comment(result))) {
      parsed_comment <- stringi::stri_match(
        comment(result),
        regex = "Duration to embed text: ([0-9.]+) secs; Date created: ([^;]+); text_version: ([^;]+)\\."
      )
      if (!is.na(parsed_comment[2])) total_duration <- total_duration + as.numeric(parsed_comment[2])
      if (!is.na(parsed_comment[3])) creation_dates <- c(creation_dates, parsed_comment[3])
      if (!is.na(parsed_comment[4])) text_versions <- c(text_versions, parsed_comment[4])
    }
  }

  # Aggregate word_types to remove duplicates and assign comments
  combined_results$word_types <- purrr::map2(
    combined_results$word_types,
    names(combined_results$word_types),
    function(word_type_tibble, word_type_name) {
      aggregated_tibble <- word_type_tibble %>%
        dplyr::group_by(words) %>%
        dplyr::summarize(
          n = sum(n, na.rm = TRUE),
          dplyr::across(starts_with("Dim"), ~ {
            weights <- word_type_tibble$n[dplyr::cur_group_rows()]
            if (aggregation == "mean" && !is.null(weights)) {
              sum(.x * weights, na.rm = TRUE) / sum(weights, na.rm = TRUE)
            } else {
              textEmbeddingAggregation(as.matrix(.x), aggregation = aggregation)
            }
          }, .names = "{.col}")
        )
      # Assign comment to the aggregated tibble
      original_comments <- purrr::map_chr(batch_results, ~ comment(.x$word_types[[word_type_name]])) %>%
        unique() %>%
        stats::na.omit() %>%
        paste(collapse = " | ")
      comment(aggregated_tibble) <- original_comments
      return(aggregated_tibble)
    }
  )

  # Deduplicate and summarize top-level comments
  unique_dates <- unique(creation_dates)
  unique_versions <- unique(text_versions)
  date_summary <- if (length(unique_dates) == 1) {
    sprintf("Date created: %s", unique_dates)
  } else if (length(unique_dates) > 1) {
    sprintf("Date range: %s to %s", min(unique_dates), max(unique_dates))
  } else {
    ""
  }

  version_summary <- if (length(unique_versions) == 1) {
    sprintf("text_version: %s", unique_versions)
  } else {
    ""
  }

  # Set the summarized top-level comment
  comment(combined_results) <- paste(
    sprintf("Total duration to embed text: %.2f secs", total_duration),
    date_summary,
    version_summary,
    sep = "; "
  ) %>% stringi::stri_trim()

  return(combined_results)
}



#' textEmbed() extracts layers and aggregate them to word embeddings, for all character variables in a given dataframe.
#' @param texts A character variable or a tibble/dataframe with at least one character variable.
#' @param model Character string specifying pre-trained language model (default 'bert-base-uncased').
#'  For full list of options see pretrained models at
#'  \href{https://huggingface.co/transformers/pretrained_models.html}{HuggingFace}.
#'  For example use "bert-base-multilingual-cased", "openai-gpt",
#' "gpt2", "ctrl", "transfo-xl-wt103", "xlnet-base-cased", "xlm-mlm-enfr-1024", "distilbert-base-cased",
#' "roberta-base", or "xlm-roberta-base". Only load models that you trust from HuggingFace; loading a
#'  malicious model can execute arbitrary code on your computer).
#' @param layers (string or numeric) Specify the layers that should be extracted
#' (default -2 which give the second to last layer). It is more efficient to only extract the layers
#' that you need (e.g., 11). You can also extract several (e.g., 11:12), or all by setting this parameter
#' to "all". Layer 0 is the decontextualized input layer (i.e., not comprising hidden states) and
#'  thus should normally not be used. These layers can then be aggregated in the textEmbedLayerAggregation
#'  function.
#' @param dim_name (boolean) If TRUE append the variable name after all variable-names in the output.
#' (This differentiates between word embedding dimension names; e.g., Dim1_text_variable_name).
#' see \code{\link{textDimName}} to change names back and forth.
#' @param aggregation_from_layers_to_tokens (string) Aggregated layers of each token. Method to aggregate the
#' contextualized layers (e.g., "mean", "min" or "max, which takes the minimum, maximum or mean, respectively,
#' across each column; or "concatenate", which links  together each word embedding layer to one long row.
#' @param aggregation_from_tokens_to_texts (string) Method to carry out the aggregation among the word embeddings
#' for the words/tokens, including "min", "max" and "mean" which takes the minimum, maximum or mean across each column;
#' or "concatenate", which links together each layer of the word embedding to one long row (default = "mean"). If set to NULL, embeddings are not
#' aggregated.
#' @param aggregation_from_tokens_to_word_types (string) Aggregates to the word type (i.e., the individual words)
#'  rather than texts. If set to "individually", then duplicate words are not aggregated, (i.e, the context of individual
#'  is preserved). (default = NULL).
#' @param keep_token_embeddings (boolean) Whether to also keep token embeddings when using texts or word
#' types aggregation.
#' @param batch_size Number of rows in each batch
#' @param remove_non_ascii (bolean) TRUE warns and removes non-ascii (using textFindNonASCII()).
#' @param tokens_select Option to select word embeddings linked to specific tokens
#' such as [CLS] and [SEP] for the context embeddings.
#' @param tokens_deselect Option to deselect embeddings linked to specific tokens
#' such as [CLS] and [SEP] for the context embeddings.
#' @param decontextualize (boolean) Provide word embeddings of single words as input to the model
#' (these embeddings are, e.g., used for plotting; default is to use ). If using this, then set
#' single_context_embeddings to FALSE.
#' @param model_max_length The maximum length (in number of tokens) for the inputs to the transformer model
#' (default the value stored for the associated model).
#' @param max_token_to_sentence (numeric) Maximum number of tokens in a string to handle before
#' switching to embedding text sentence by sentence.
#' @param tokenizer_parallelism (boolean) If TRUE this will turn on tokenizer parallelism. Default FALSE.
#' @param device (character) Name of device to use: 'cpu', 'gpu', 'gpu:k' or 'mps'/'mps:k'
#' for MacOS, where k is a specific device number. (default = "cpu"). Default is NULL, which sets gpu:0/mps:0
#' depending on OS.
#' @param hg_gated Set to TRUE if the accessed model is gated.
#' @param hg_token The token needed to access the gated model.
#' Create a token from the ['Settings' page](https://huggingface.co/settings/tokens) of
#' the Hugging Face website. An an environment variable HUGGINGFACE_TOKEN can
#' be set to avoid the need to enter the token each time.
#' @param logging_level Set the logging level. Default: "warning".
#' Options (ordered from less logging to more logging): critical, error, warning, info, debug
#' @param implementation (string; experimental) If "dlatk" the text is split using the DLATK-method; this method
#' is faster and appears better for longer texts (but today it does not
#' return token level word embeddings, nor word_types embeddings).
#' @param trust_remote_code (boolean) use a model with custom code on the Huggingface Hub
#' @param ... settings from textEmbedRawLayers().
#' @return A tibble with tokens, a column for layer identifier and word embeddings.
#' Note that layer 0 is the input embedding to the transformer.
#' @examples
#' # Automatically transforms the characters in the example dataset:
#' # Language_based_assessment_data_8 (included in text-package), to embeddings.
#' \dontrun{
#' word_embeddings <- textEmbed(Language_based_assessment_data_8[1:2, 1:2],
#'   layers = 10:11,
#'   aggregation_from_layers_to_tokens = "concatenate",
#'   aggregation_from_tokens_to_texts = "mean",
#'   aggregation_from_tokens_to_word_types = "mean"
#' )
#'
#' # Show information about how the embeddings were constructed.
#' comment(word_embeddings$texts$satisfactiontexts)
#' comment(word_embeddings$word_types)
#' comment(word_embeddings$tokens$satisfactiontexts)
#'
#' # See how the word embeddings are structured.
#' word_embeddings
#'
#' # Save the word embeddings to avoid having to embed the text again.
#' saveRDS(word_embeddings, "word_embeddings.rds")
#'
#' # Retrieve the saved word embeddings.
#' word_embeddings <- readRDS("word_embeddings.rds")
#' }
#'
#' @seealso See \code{\link{textEmbedLayerAggregation}}, \code{\link{textEmbedRawLayers}} and
#' \code{\link{textDimName}}.
#' @importFrom reticulate source_python
#' @importFrom utils modifyList
#' @export
textEmbed <- function(
    texts,
    model = "bert-base-uncased",
    layers = -2,
    dim_name = TRUE,
    aggregation_from_layers_to_tokens = "concatenate",
    aggregation_from_tokens_to_texts = "mean",
    aggregation_from_tokens_to_word_types = NULL,
    keep_token_embeddings = TRUE,
    batch_size = 100,
    remove_non_ascii = TRUE,
    tokens_select = NULL,
    tokens_deselect = NULL,
    decontextualize = FALSE,
    model_max_length = NULL,
    max_token_to_sentence = 4,
    tokenizer_parallelism = FALSE,
    device = NULL,
    hg_gated = FALSE,
    hg_token = Sys.getenv("HUGGINGFACE_TOKEN",
                          unset = ""),
    logging_level = "error",
    implementation = "original",
    trust_remote_code = FALSE,
    ...) {

  T1 <- Sys.time()

  # Setting device depending on OS
  if(is.null(device)){
    if(!is_osx()){
      device = "gpu:0"
    }
    if(is_osx()){
      device = "mps:0"
    }
  }

  if(!tibble::is_tibble(texts)){
    texts <- tibble::tibble(texts = texts)
  }

  # Split texts into batches
  split_into_batches <- function(data, batch_size) {
    split(data, ceiling(seq_along(1:nrow(data)) / batch_size))
  }

  batches <- split_into_batches(texts, batch_size)

  # Process each batch and store results i=1
  batch_results <- list()
  for (i in seq_along(batches)) {

    batch_message <- sprintf("Processing batch %d/%d\n", i, length(batches))
    message(colourise(batch_message, "blue"))


    batch_texts <- batches[[i]]
    #batch_texts <- batch[["satisfactionwords"]]

    # Process batch with error handling
    if(implementation == "original"){
       batch_result <- tryCatch(
         text_embed(
           texts = batch_texts,
           model = model,
           layers = layers,
           dim_name = dim_name,
           aggregation_from_layers_to_tokens = aggregation_from_layers_to_tokens,
           aggregation_from_tokens_to_texts = aggregation_from_tokens_to_texts,
           aggregation_from_tokens_to_word_types = aggregation_from_tokens_to_word_types,
           keep_token_embeddings = keep_token_embeddings,
           remove_non_ascii = remove_non_ascii,
           tokens_select = tokens_select,
           tokens_deselect = tokens_deselect,
           decontextualize = decontextualize,
           model_max_length = model_max_length,
           max_token_to_sentence = max_token_to_sentence,
           tokenizer_parallelism = tokenizer_parallelism,
           device = device,
           hg_gated = hg_gated,
           hg_token = hg_token,
           logging_level = logging_level
           , ...
           ),

         error = function(e) {
           message(sprintf("Error in batch %d: %s", i, e$message))
           return(NULL)
         }
       )
    }

    if(implementation == "dlatk"){

      # Process batch with error handling
      batch_result <- tryCatch(
        text_embed_dlatk(
          texts = batch_texts,
          model = model,
          layers = layers,
          dim_name = dim_name,
      #    aggregation_from_layers_to_tokens = aggregation_from_layers_to_tokens,
          aggregation_from_tokens_to_texts = aggregation_from_tokens_to_texts,
      #    aggregation_from_tokens_to_word_types = aggregation_from_tokens_to_word_types,
       #   keep_token_embeddings = keep_token_embeddings,
          remove_non_ascii = remove_non_ascii,
      #    tokens_select = tokens_select,
      #    tokens_deselect = tokens_deselect,
      #    decontextualize = decontextualize,
          model_max_length = model_max_length,
      #    max_token_to_sentence = max_token_to_sentence,
          tokenizer_parallelism = tokenizer_parallelism,
          device = device,
          hg_gated = hg_gated,
          hg_token = hg_token,
          trust_remote_code = trust_remote_code,
          logging_level = logging_level,
          batch_size =as.integer(batch_size)
          , ...
        ),

        error = function(e) {
          message(sprintf("Error in batch %d: %s", i, e$message))
          return(NULL)
        }
      )
    }

    batch_results[[i]] <- batch_result

    T2 <- Sys.time()
    time_from_starts <- round(as.numeric(difftime(T2, T1, units = "mins")), 3)
    time_from_message <- paste("Minutes from start: ", time_from_starts)
    message(colourise(time_from_message, "green"))

    batches_left <- length(batches) - i
    mean_time_per_batch <- time_from_starts/i
    estimated_time_left <- mean_time_per_batch * batches_left
    estimation_message <- paste0("Estimated embedding time left = ", estimated_time_left, " minutes")
    message(colourise(estimation_message, "black"))
  }

  final_result <- combine_textEmbed_results(
    batch_results,
    aggregation = aggregation_from_tokens_to_word_types)

  return(final_result)
}

# comment(final_result$texts$texts)

#' Change dimension names
#'
#' textDimName() changes the names of the dimensions in the word embeddings.
#'
#' @param word_embeddings List of word embeddings or a single tibble
#' @param dim_names Logical. If TRUE, the word embedding name or a custom name will be attached to the name of each dimension.
#' If FALSE, the attached part of the name will be removed.
#' @param name Optional character. If provided and dim_names = TRUE, this custom name will be attached after each column (e.g., dim1_name).
#' @return Word embeddings with changed names.
#' @examples
#' \donttest{
#' # Note that dimensions are called Dim1_harmonytexts etc.
#' word_embeddings_4$texts$harmonytexts
#'
#' # Change to just Dim
#' w_e_T <- textDimName(word_embeddings_4$texts["harmonytexts"],
#'   dim_names = FALSE
#' )
#'
#' # Change back to include the original name
#' w_e_F <- textDimName(w_e_T, dim_names = TRUE)
#'
#' # Change and add a custom name
#' w_e_custom <- textDimName(w_e_T, dim_names = TRUE, name = "CustomName")
#' }
#' @seealso \code{\link{textEmbed}}
#' @export
textDimName <- function(
    word_embeddings,
    dim_names = TRUE,
    name = NULL) {

  tokens <- NULL
  word_type <- NULL

  x_is_tibble <- tibble::is_tibble(word_embeddings)
  if (x_is_tibble) word_embeddings <- list(word_embeddings)

  # Remove singlewords_we if it exists
  if (!is.null(word_embeddings$word_type)) {
    word_type <- word_embeddings$word_type
    word_embeddings$word_type <- NULL
  }

  if (!is.null(word_embeddings$tokens)) {
    tokens <- word_embeddings$tokens
    word_embeddings$tokens <- NULL
  }

  if (dim_names) {
    for (i_row in seq_len(length(word_embeddings))) {
      base_names <- names(word_embeddings[[i_row]])
      suffix <- if (!is.null(name)) name else names(word_embeddings)[[i_row]]
      colnames(word_embeddings[[i_row]]) <- paste0(
        base_names,
        "_",
        suffix
      )
    }
  } else {
    for (i_row in seq_len(length(word_embeddings))) {
      target_variables_names <- colnames(word_embeddings[[i_row]])
      # Select everything BEFORE the first _ (i.e., the Dim1, etc.)
      variable_names <- sub("\\_.*", "", target_variables_names)
      colnames(word_embeddings[[i_row]]) <- variable_names
    }
  }

  # Attach word embeddings again
  if (!is.null(word_type)) {
    word_embeddings$word_type <- word_type
  }
  if (!is.null(tokens)) {
    word_embeddings$tokens <- tokens
  }

  # Return tibble if input was a tibble (not a list)
  if (x_is_tibble) word_embeddings <- word_embeddings[[1]]

  return(word_embeddings)
}

