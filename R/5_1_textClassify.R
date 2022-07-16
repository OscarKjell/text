
#x = "I like you. I love you"
#model = "distilbert-base-uncased-finetuned-sst-2-english"
#device = "cpu"
#tokenizer_parallelism = FALSE
#logging_level = "error"
#return_incorrect_results = FALSE
#return_all_scores = FALSE
#function_to_apply = "none"


#' Predict label and probability of a text using a pretrained classifier language model. STILL UNDER DEVELOPMENT
#' @param x (string)  A character variable or a tibble/dataframe with at least one character variable.
#' @param model (string)  Character string specifying a pre-trained classifier language model.
#'  For full list of options see pretrained classifier models at
#'  \href{https://huggingface.co/transformers/pretrained_models.html}{HuggingFace}.
#'  For example use "cardiffnlp/twitter-roberta-base-sentiment", "distilbert-base-uncased-finetuned-sst-2-english".
#' @param device (string)  Name of device to use: 'cpu', 'gpu', or 'gpu:k' where k is a specific device number
#' @param tokenizer_parallelism (boolean)  If TRUE this will turn on tokenizer parallelism. Default FALSE.
#' @param logging_level (string)  Set the logging level. Default: "warning".
#' Options (ordered from less logging to more logging): critical, error, warning, info, debug
#' @param return_incorrect_results (boolean)  Many models are not created to be able to provide classifications - this setting
#' stops them from returning incorrect results.
#' @param return_all_scores (boolean)  Whether to return all prediction scores or just the one of the predicted class.
#' @param function_to_apply (string)  The function to apply to the model outputs to retrieve the scores.
#' There are four different values:
#' "default": if the model has a single label, will apply the sigmoid function on the output. If the model has several labels, will apply the softmax function on the output.
#' "sigmoid": Applies the sigmoid function on the output.
#' "softmax": Applies the softmax function on the output.
#' "none": Does not apply any function on the output.
#' @return A tibble with predicted labels and scores for each text variable.
#' The comment of the object show the model-name and computation time.
#' @examples
#' \donttest{
#' classifications <- textClassify(x = Language_based_assessment_data_8[1:2, 1:2])
#' classifications
#' comment(classifications)
#' }
#' @seealso see \code{\link{textEmbedLayerAggregation}} and \code{\link{textEmbed}}
#' @importFrom reticulate source_python
#' @importFrom dplyr bind_cols bind_rows
#' @export
textClassify <- function(x,
                         model = "distilbert-base-uncased-finetuned-sst-2-english",
                         device = "cpu",
                         tokenizer_parallelism = FALSE,
                         logging_level = "error",
                         return_incorrect_results = FALSE,
                         return_all_scores = FALSE,
                         function_to_apply = "none"){

  T1_textSentiment <- Sys.time()

  # Run python file with HunggingFace interface to state-of-the-art transformers
  reticulate::source_python(system.file("python",
                                        "huggingface_Interface3.py",
                                        # envir = NULL,
                                        package = "text",
                                        mustWork = TRUE
  ))

  # Select all character variables and make them UTF-8 coded (e.g., BERT wants it that way).
  data_character_variables <- select_character_v_utf8(x)

  ALL_sentiments <- list()
  # Loop over all character variables; i_variables = 1
  for (i_variables in seq_len(length(data_character_variables))) {
    T1_variable <- Sys.time()
    # Python file function to HuggingFace
    hg_sentiments <- hgTransformerGetSentiment(text_strings = data_character_variables[[i_variables]],
                                               model = model,
                                               device = device,
                                               tokenizer_parallelism = tokenizer_parallelism,
                                               logging_level = logging_level)

    # Sort output into tidy-format
    output1 <- map(hg_sentiments, dplyr::bind_cols) %>%
      dplyr::bind_rows()

    # Add the text variable name to variable names
    x_name <- names(data_character_variables[i_variables])
    names(output1) <- paste0(names(output1),
                             "_",
                             x_name)

    ALL_sentiments[[i_variables]] <- output1
    T2_variable <- Sys.time()
    variable_time <- T2_variable - T1_variable
    variable_time <- sprintf("Duration: %f %s",
                             variable_time,
                             units(variable_time))

    loop_text <- paste(x_name, "completed:",
                       variable_time,
                       "\n",
                       sep = " ")

    cat(colourise(loop_text, "green"))

  }

  ALL_sentiments1 <- dplyr::bind_cols(ALL_sentiments)

  #Time to complete all variables
  T2_textSentiment <- Sys.time()
  all_time <- T2_textSentiment - T1_textSentiment
  all_time <- sprintf("Duration to predict all variables: %f %s",
                      all_time,
                      units(all_time))


    # Adding informative comment help(comment)
    comment(ALL_sentiments1) <- paste("Information about the textSentiment.  ",
                                       "model: ", model, "; ",
                                      "time: ", all_time, ";",
                                       "text_version: ", packageVersion("text"), ".",
                                       sep = "",
                                       collapse = "\n"
    )
    return(ALL_sentiments1)
  }
