
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

    variable_name <- names(x)[1]

    # If original name is not just Dim1, then add back Dim1_variable.name
    if (!variable_name == "Dim1") {
      variable_name <- sub(".*Dim1_", "", variable_name)

      colnames(long_vector) <- paste0(
        names(long_vector),
        "_",
        variable_name
      )
    }
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
#' @param hg_tokenizer (boolean) Weather to use textTokenize
#' @return A tibble with a unique words column and a column with their respective frequency.
#' @importFrom tibble tibble
#' @importFrom stringi stri_c stri_trans_tolower
# @importFrom stringr str_c str_split stri_split_boundaries
# @importFrom tokenizers tokenize_words
#' @noRd
getUniqueWordsAndFreq <- function(x_characters, hg_tokenizer = NULL, ...) {
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
    x_characters4b <- lapply(list(x_characters), textTokenize, model = hg_tokenizer, ...)
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
#' @param return_tokens bolean whether tokens have been returned (setting comes from textEmbedRawLayers).
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

  # Loops over the number of tokens; i_token_id = 1
  for (i_token_id in seq_len(length(number_of_ids))) {
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
grep_col_by_name_in_list <- function(l, pattern) {
  u <- unlist(l)
  u[grep(pattern, names(u))]
}


#' Tokenize according to different huggingface transformers
#' @param texts A character variable or a tibble/dataframe with at least one character variable.
#' @param model Character string specifying pre-trained language model (default 'bert-base-uncased').
#'  For full list of options see pretrained models at
#'  \href{https://huggingface.co/transformers/pretrained_models.html}{HuggingFace}.
#'  For example use "bert-base-multilingual-cased", "openai-gpt",
#' "gpt2", "ctrl", "transfo-xl-wt103", "xlnet-base-cased", "xlm-mlm-enfr-1024", "distilbert-base-cased",
#' "roberta-base", or "xlm-roberta-base".
#' @param max_token_to_sentence (numeric) Maximum number of tokens in a string to handle before
#' switching to embedding text sentence by sentence.
#' @param device Name of device to use: 'cpu', 'gpu', or 'gpu:k' where k is a specific device number
#' @param tokenizer_parallelism If TRUE this will turn on tokenizer parallelism. Default FALSE.
#' @param model_max_length The maximum length (in number of tokens) for the inputs to the transformer model
#' (default the value stored for the associated model).
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
                         model = "bert-base-uncased",
                         max_token_to_sentence = 4,
                         device = "cpu",
                         tokenizer_parallelism = FALSE,
                         model_max_length = NULL,
                         logging_level = "error") {


  # Run python file with HunggingFace interface to state-of-the-art transformers
  reticulate::source_python(system.file("python",
    "huggingface_Interface3.py",
    # envir = NULL,
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
    logging_level = logging_level
  )
  tokens1 <- lapply(tokens, tibble::as_tibble_col, column_name = "tokens")

  return(tokens1)
}



#' Extract layers of hidden states (word embeddings) for all character variables in a given dataframe.
#' @param texts A character variable or a tibble/dataframe with at least one character variable.
#' @param model Character string specifying pre-trained language model (default 'bert-base-uncased').
#'  For full list of options see pretrained models at
#'  \href{https://huggingface.co/transformers/pretrained_models.html}{HuggingFace}.
#'  For example use "bert-base-multilingual-cased", "openai-gpt",
#' "gpt2", "ctrl", "transfo-xl-wt103", "xlnet-base-cased", "xlm-mlm-enfr-1024", "distilbert-base-cased",
#' "roberta-base", or "xlm-roberta-base".
#' @param layers (string or numeric) Specify the layers that should be extracted
#' (default -2, which give the second to last layer). It is more efficient to only extract the
#' layers that you need (e.g., 11). You can also extract several (e.g., 11:12),
#' or all by setting this parameter to "all". Layer 0 is the decontextualized input layer
#' (i.e., not comprising hidden states) and thus should normally not be used. These layers can then
#'  be aggregated in the textEmbedLayerAggregation function.
#' @param return_tokens If TRUE, provide the tokens used in the specified transformer model.
#' @param word_type_embeddings (boolean) Wether to provide embeddings for each word/token type.
#' @param decontextualize (boolean) Wether to dectonextualise embeddings (i.e., embedding one word at a time).
#' @param keep_token_embeddings (boolean) Whether to keep token level embeddings in the output
#' (when using word_types aggregation)
#' @param device Name of device to use: 'cpu', 'gpu', or 'gpu:k' where k is a specific device number
#' @param tokenizer_parallelism If TRUE this will turn on tokenizer parallelism. Default FALSE.
#' @param model_max_length The maximum length (in number of tokens) for the inputs to the transformer model
#' (default the value stored for the associated model).
#' @param max_token_to_sentence (numeric) Maximum number of tokens in a string to handle before switching to embedding
#' text sentence by sentence.
#' @param logging_level Set the logging level. Default: "warning".
#' Options (ordered from less logging to more logging): critical, error, warning, info, debug
#' @return Returns hiddenstates/layers that can be 1. Can return three different outputA tibble with tokens,
#' column specifying layer and word embeddings. Note that layer 0 is the input embedding to the transformer,
#' and should normally not be used.
#' @examples
#' \donttest{
#' # texts <- Language_based_assessment_data_8[1:2, 1:2]
#' # word_embeddings_with_layers <- textEmbedRawLayers(texts, layers = 11:12)
#' }
#' @seealso see \code{\link{textEmbedLayerAggregation}} and \code{\link{textEmbed}}
#' @importFrom reticulate source_python
#' @importFrom dplyr %>% bind_rows group_split
#' @importFrom tibble tibble as_tibble
#' @importFrom magrittr set_colnames
#' @export
textEmbedRawLayers <- function(texts,
                               model = "bert-base-uncased",
                               layers = -2,
                               return_tokens = TRUE,
                               word_type_embeddings = FALSE,
                               decontextualize = FALSE,
                               keep_token_embeddings = TRUE,
                               device = "cpu",
                               tokenizer_parallelism = FALSE,
                               model_max_length = NULL,
                               max_token_to_sentence = 4,
                               logging_level = "error") {



  if (decontextualize == TRUE & word_type_embeddings == FALSE) {
    stop(cat(
      colourise("decontextualize = TRUE & word_type_embeddings = FALSE has not been implemented in textEmbedRawLayers() at this stage.",
                fg = "red"),
      colourise("When using decontextualize = TRUE  you need to create the word_type_embeddings. To create a text embeddings withouth it would take unnecessary
                time as it would require to send the same decontextualised words to a transformer multiple times (whilst getting the same results over and over).
                Consdier using rextEmbed, to get token embeddings as well as text embeddings.",
                fg = "green")
    ))
  }


  # Run python file with HunggingFace interface to state-of-the-art transformers
  reticulate::source_python(system.file("python",
                                        "huggingface_Interface3.py",
                                        # envir = NULL,
                                        package = "text",
                                        mustWork = TRUE
  ))

  if (is.numeric(layers)) {
    if (max(layers) > textModelLayers(model)) {
      stop("You are trying to extract layers that do not exist in this model.")
    }
  }

  if (layers[1] < 0) {
    n <- textModelLayers("bert-base-uncased")
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
        device = device,
        tokenizer_parallelism = tokenizer_parallelism,
        model_max_length = model_max_length,
        max_token_to_sentence = max_token_to_sentence,
        logging_level = logging_level
      )

      variable_x <- sortingLayers(x = hg_embeddings, layers = layers, return_tokens = return_tokens)

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

      cat(colourise(loop_text, "green"))
    }
  }

  # Word type embeddings based on Context embeddings
  if (word_type_embeddings & !decontextualize) {

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
    cat(colourise(sing_text, "green"))
  }

  # Decontextualized embeddings for aggregated embeddings and word type embeddings
  if (decontextualize) {
    individual_tokens <- list()
    individual_tokens$decontext <- list()
    # Get word embeddings for all individual tokens/words (which is, e.g., used for the word plot).help(bind_cols)
    data_character_variables1 <- suppressMessages(apply(data_character_variables, 1, bind_cols)) %>%
      bind_rows()

    singlewords <- getUniqueWordsAndFreq(data_character_variables1[[1]],
      hg_tokenizer = model
    )
    list_words <- sapply(singlewords$words, list)
    names(list_words) <- NULL


    hg_decontexts_embeddings <- hgTransformerGetEmbedding(
      text_strings = list_words,
      model = model,
      layers = layers,
      return_tokens = return_tokens,
      device = device,
      tokenizer_parallelism = tokenizer_parallelism,
      model_max_length = model_max_length,
      max_token_to_sentence = max_token_to_sentence,
      logging_level = logging_level
    )

    # Sort out layers as above
    individual_tokens$decontext$word_type <- sortingLayers(
      x = hg_decontexts_embeddings,
      layers = layers,
      return_tokens = return_tokens
    )
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
    cat(colourise(de_text, "green"))

    individual_tokens
  }

  # Combine previous list and word list
  if (decontextualize == FALSE & word_type_embeddings == FALSE) {
    word_embeddings_with_layers <- c(sorted_layers_ALL_variables)
    rm(sorted_layers_ALL_variables)
  } else if (decontextualize == FALSE & word_type_embeddings == TRUE & keep_token_embeddings == TRUE) {
    word_embeddings_with_layers <- c(sorted_layers_ALL_variables, individual_tokens)
    rm(sorted_layers_ALL_variables)
    rm(individual_tokens)
  } else if (decontextualize == FALSE & word_type_embeddings == TRUE & keep_token_embeddings == FALSE) {
    word_embeddings_with_layers <- c(individual_tokens)
    rm(sorted_layers_ALL_variables)
    rm(individual_tokens)
  } else if (decontextualize == TRUE & word_type_embeddings == TRUE) {
    word_embeddings_with_layers <- c(individual_tokens)
    rm(individual_tokens)
  }

  return(word_embeddings_with_layers)
}


#' Select and aggregate layers of hidden states to form a word embeddings.
#' @param word_embeddings_layers Layers outputted from textEmbedRawLayers.
#' @param layers The numbers of the layers to be aggregated
#' (e.g., c(11:12) to aggregate the eleventh and twelfth).
#' Note that layer 0 is the input embedding to the transformer, and should normally not be used.
#' Selecting 'all' thus removes layer 0.
#' @param aggregation_from_layers_to_tokens Method to carry out the aggregation among the layers for each word/token,
#' including "min", "max" and "mean" which takes the minimum, maximum or mean across each column;
#' or "concatenate", which links together each layer of the word embedding to one long row. Default is "concatenate"
#' @param aggregation_from_tokens_to_texts Method to carry out the aggregation among the word embeddings
#' for the words/tokens, including "min", "max" and "mean" which takes the minimum, maximum or mean across each column;
#' or "concatenate", which links together each layer of the word embedding to one long row.
#' @param return_tokens If TRUE, provide the tokens used in the specified transformer model.
#' @param tokens_select Option to only select embeddings linked to specific tokens
#' such as "[CLS]" and "[SEP]" (default NULL).
#' @param tokens_deselect Option to deselect embeddings linked to specific tokens
#' such as "[CLS]" and "[SEP]" (default NULL).
#' @return A tibble with word embeddings. Note that layer 0 is the input embedding to
#' the transformer, which is normally not used.
#' @examples
#' \donttest{
#' # word_embeddings_layers <- textEmbedRawLayers(Language_based_assessment_data_8$harmonywords[1],
#' # layers = 11:12)
#' # word_embeddings <- textEmbedLayerAggregation(word_embeddings_layers$context, layers = 11)
#' }
#' @seealso see \code{\link{textEmbedRawLayers}} and \code{\link{textEmbed}}
#' @importFrom dplyr %>% bind_rows
#' @export
textEmbedLayerAggregation <- function(word_embeddings_layers,
                                      layers = "all",
                                      aggregation_from_layers_to_tokens = "concatenate",
                                      aggregation_from_tokens_to_texts = "mean",
                                      return_tokens = FALSE,
                                      tokens_select = NULL,
                                      tokens_deselect = NULL) {
  if (return_tokens == TRUE & !is.null(aggregation_from_tokens_to_texts)) {
    stop(cat(
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
      stop(cat(
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
      if (length(tokens_deselect) == 1 & tokens_deselect == "[CLS]") {
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

    cat(colourise(loop_text, "blue"))
  }

  names(selected_layers_aggregated_tibble) <- names(word_embeddings_layers)
  selected_layers_aggregated_tibble
}


#' Extract layers and aggregate them to word embeddings, for all character variables in a given dataframe.
#' @param texts A character variable or a tibble/dataframe with at least one character variable.
#' @param model Character string specifying pre-trained language model (default 'bert-base-uncased').
#'  For full list of options see pretrained models at
#'  \href{https://huggingface.co/transformers/pretrained_models.html}{HuggingFace}.
#'  For example use "bert-base-multilingual-cased", "openai-gpt",
#' "gpt2", "ctrl", "transfo-xl-wt103", "xlnet-base-cased", "xlm-mlm-enfr-1024", "distilbert-base-cased",
#' "roberta-base", or "xlm-roberta-base".
#' @param layers (string or numeric) Specify the layers that should be extracted
#' (default -2 which give the second to last layer). It is more efficient to only extract the layers
#' that you need (e.g., 11). You can also extract several (e.g., 11:12), or all by setting this parameter
#' to "all". Layer 0 is the decontextualized input layer (i.e., not comprising hidden states) and
#'  thus should normally not be used. These layers can then be aggregated in the textEmbedLayerAggregation
#'  function.
#' @param dim_name Boolean, if TRUE append the variable name after all variable-names in the output.
#' (This differentiates between word embedding dimension names; e.g., Dim1_text_variable_name).
#' see \code{\link{textDimName}} to change names back and forth.
#' @param aggregation_from_layers_to_tokens (string) Aggregated layers of each token. Method to aggregate the
#' contextualized layers (e.g., "mean", "min" or "max, which takes the minimum, maximum or mean, respectively,
#' across each column; or "concatenate", which links  together each word embedding layer to one long row.
#' @param aggregation_from_tokens_to_texts (string)  Aggregates to the individual text (i.e., the aggregation of
#' all tokens/words given to the transformer).
#' @param aggregation_from_tokens_to_word_types (string) Aggregates to the word type (i.e., the individual words)
#'  rather than texts.
#' @param keep_token_embeddings (boolean) Whether to also keep token embeddings when using texts or word
#' types aggregation.
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
#' @param device Name of device to use: 'cpu', 'gpu', or 'gpu:k' where k is a specific device number
#' @param logging_level Set the logging level. Default: "warning".
#' Options (ordered from less logging to more logging): critical, error, warning, info, debug
#' @return A tibble with tokens, a column for layer identifier and word embeddings.
#' Note that layer 0 is the input embedding to the transformer
#' @examples
#' \donttest{
#' # word_embeddings <- textEmbed(Language_based_assessment_data_8[1:2, 1:2],
#' #                             layers = 10:11,
#' #                             aggregation_from_layers_to_tokens = "concatenate",
#' #                             aggregation_from_tokens_to_texts = "mean",
#' #                             aggregation_from_tokens_to_word_types = "mean")
#' ## Show information about how the embeddings were constructed
#' # comment(word_embeddings$texts$satisfactiontexts)
#' # comment(word_embeddings$word_types)
#' # comment(word_embeddings$tokens$satisfactiontexts)
#' }
#' @seealso see \code{\link{textEmbedLayerAggregation}}, \code{\link{textEmbedRawLayers}} and
#' \code{\link{textDimName}}
#' @importFrom reticulate source_python
#' @export
textEmbed <- function(texts,
                      model = "bert-base-uncased",
                      layers = -2,
                      dim_name = TRUE,
                      aggregation_from_layers_to_tokens = "concatenate",
                      aggregation_from_tokens_to_texts = "mean",
                      aggregation_from_tokens_to_word_types = NULL,
                      keep_token_embeddings = TRUE,
                      tokens_select = NULL,
                      tokens_deselect = NULL,
                      decontextualize = FALSE,
                      model_max_length = NULL,
                      max_token_to_sentence = 4,
                      tokenizer_parallelism = FALSE,
                      device = "gpu",
                      logging_level = "error") {


  T1_textEmbed <- Sys.time()

  reticulate::source_python(system.file("python",
                                        "huggingface_Interface3.py",
                                        package = "text",
                                        mustWork = TRUE
  ))

  if (
    (decontextualize == TRUE & is.null(aggregation_from_tokens_to_texts)) |
      (decontextualize == TRUE & is.null(aggregation_from_tokens_to_word_types)) |
      (decontextualize == TRUE & is.null(aggregation_from_layers_to_tokens))) {
    stop(cat(
      colourise("When using decontextualize = TRUE, it is required to set aggregation_from_tokens_to_texts,
                aggregation_from_tokens_to_word_types as well as aggregation_from_tokens_to_word_types",
                fg = "red"),
      colourise("This is because both the token ambeddings and text embeddings are constrcuted from the word type embeddings.",
                fg = "green")
    ))
  }
  output <- list()

  if (layers[1] < 0) {
    n <- textModelLayers("bert-base-uncased")
    layers <- 1 + n + layers
    layers
  }

  # Get hidden states/layers for output 1 and/or output 2 or decontextualsied;
  if (!is.null(aggregation_from_layers_to_tokens) |
    !is.null(aggregation_from_tokens_to_texts) |
    decontextualize) {
    all_wanted_layers <- textEmbedRawLayers(
      texts = texts,
      model = model,
      layers = layers,
      return_tokens = TRUE,
      word_type_embeddings = TRUE,
      decontextualize = decontextualize,
      device = device,
      tokenizer_parallelism = tokenizer_parallelism,
      model_max_length = model_max_length,
      max_token_to_sentence = max_token_to_sentence,
      logging_level = logging_level
    )
  }

  if (!decontextualize) {
    # 1. Get token-level embeddings with aggregated levels
    if (!is.null(aggregation_from_layers_to_tokens) & keep_token_embeddings) {
      token_embeddings <- textEmbedLayerAggregation(
        word_embeddings_layers = all_wanted_layers$context_tokens,
        layers = layers,
        aggregation_from_layers_to_tokens = aggregation_from_layers_to_tokens,
        aggregation_from_tokens_to_texts = NULL,
        return_tokens = TRUE, # need to be TRUE
        tokens_deselect = NULL
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
        tokens_deselect = NULL
      )
      output$texts <- aggregated_token_embeddings
    }
  }


  # 3. Aggregate Word Type (both decontextualised or not) aggregation_from_tokens_to_word_types = "mean"
  if (!is.null(aggregation_from_tokens_to_word_types) | decontextualize) {
    if (!decontextualize) {
      single_context_text <- paste("Embedding single context embeddings.",
        "\n",
        sep = ""
      )
      cat(colourise(single_context_text, "purple"))

      individual_word_embeddings_layers <- all_wanted_layers$context_word_type
      individual_words <- all_wanted_layers$tokens
    }

    if (decontextualize) {
      single_context_text <- paste("Embedding decontextualised embeddings.",
        "\n",
        sep = ""
      )
      cat(colourise(single_context_text, "purple"))

      individual_word_embeddings_layers <- all_wanted_layers$decontext$word_type
      individual_words <- all_wanted_layers$decontext$single_words
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
    individual_word_embeddings <- dplyr::bind_rows(individual_word_embeddings)
    # Combine the words for each decontextualized embedding
    individual_word_embeddings_words <- dplyr::bind_cols(
      individual_words, # all_wanted_layers$decontext$single_words,
      individual_word_embeddings
    )

    comment(individual_word_embeddings_words) <- paste(
      comment(all_wanted_layers$context_tokens),
      comment(individual_word_embeddings),
      " ; aggregation_from_tokens_to_word_types = ", aggregation_from_tokens_to_word_types,
      " ; decontextualize = ", decontextualize
    )
    output$word_types <- individual_word_embeddings_words
    cat(colourise("Done! \n", "purple"))
  }


  #### Decontextualised tokens and text embeddings (using output from 3 above)

  if (decontextualize) {
    decontext_space <- individual_word_embeddings_words

    ### 1. Get token-level embeddings with aggregated levels
    if (!is.null(aggregation_from_layers_to_tokens)) { #  & keep_token_embeddings

      # Tokenize texts
      output <- list()
      token_embeddings_list$tokens <- list()
      for (i_variables in seq_len(ncol(texts))) {
        text_tokens <- lapply(texts[[i_variables]], textTokenize,
                              model = model, max_token_to_sentence = max_token_to_sentence) # , ...

        t_embeddings <- lapply(text_tokens, applysemrep_over_words, decontext_space, tolower = FALSE)

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
      )

      output$texts <- text_embeddings
    }
  }

  T2_textEmbed <- Sys.time()
  Time_textEmbed <- T2_textEmbed - T1_textEmbed
  Time_textEmbed <- sprintf("Duration to embed text: %f %s", Time_textEmbed, units(Time_textEmbed))
  Date_textEmbed <- Sys.time()

  if (dim_name == TRUE & !is.null(aggregation_from_tokens_to_texts)) {
    output$texts <- textDimName(output$texts)
  }

  comment(output) <- paste(Time_textEmbed,
    "; Date created: ", Date_textEmbed,
    "; text_version: ", packageVersion("text"), ".",
    sep = "",
    collapse = " "
  )
  return(output)
}


#' Change the names of the dimensions in the word embeddings.
#' @param word_embeddings List of word embeddings
#' @param dim_names (boolean) If TRUE the word embedding name will be attached to the name of each dimension;
#' is FALSE, the attached part of the name will be removed.
#' @return Word embeddings with changed names.
#' @examples
#' \donttest{
#' # Note that dimensions are called Dim1_harmonytexts etc.
#' word_embeddings_4$texts$harmonytexts
#' # Here they are changed to just Dim
#' w_e_T <- textDimName(word_embeddings_4$texts["harmonytexts"],
#'   dim_names = FALSE
#' )
#' # Here they are changed back
#' w_e_F <- textDimName(w_e_T, dim_names = TRUE)
#' }
#' @seealso see \code{\link{textEmbed}}
#' @export
textDimName <- function(word_embeddings,
                        dim_names = TRUE) {
  tokens <- NULL
  word_type <- NULL

  x_is_tibble <- tibble::is_tibble(word_embeddings)
  if (x_is_tibble) word_embeddings <- list(word_embeddings)

  # Remove singlewords_we if it exist
  if (!is.null(word_embeddings$word_type)) {
    word_type <- word_embeddings$word_type
    word_embeddings$word_type <- NULL
  }

  if (!is.null(word_embeddings$tokens)) {
    tokens <- word_embeddings$tokens
    word_embeddings$tokens <- NULL
  }

  # i_row = 1 dim_name=TRUE
  if (dim_names) {
    for (i_row in seq_len(length(word_embeddings))) {
      colnames(word_embeddings[[i_row]]) <- paste0(
        names(word_embeddings[[i_row]]),
        "_",
        names(word_embeddings)[[i_row]]
      )
    }
  }

  if (!dim_names) {
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
  # Attach word embeddings again
  if (!is.null(tokens)) {
    word_embeddings$tokens <- tokens
  }

  # Return tibble if x is a tibble (and not a list)
  if (x_is_tibble) word_embeddings <- word_embeddings[[1]]

  return(word_embeddings)
}
