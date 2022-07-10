

#
## sen1 <- textSentiment("hello sunshine",
##            model = "distilbert-base-uncased-finetuned-sst-2-english")
## comment(sen1)
# sen2 <- textSentiment(Language_based_assessment_data_8[1:2,1],
#                       model = "bert-base-uncased")
#
# sen3 <- textSentiment(Language_based_assessment_data_8[1:2,1:2],
#                       model = "distilbert-base-uncased-finetuned-sst-2-english")
#

x = "hello sunshine"
model  = "cardiffnlp/twitter-roberta-base-sentiment-latest"
device = "cpu"
tokenizer_parallelism = FALSE
logging_level = "error"

#' Predict label and score of a text using huggingface model
#' @param x A character variable or a tibble/dataframe with at least one character variable.
#' @param model Character string specifying pre-trained language model (default 'bert-base-uncased').
#'  For full list of options see pretrained models at
#'  \href{https://huggingface.co/transformers/pretrained_models.html}{HuggingFace}.
#'  For example use "...".
#' @param device Name of device to use: 'cpu', 'gpu', or 'gpu:k' where k is a specific device number
#' @param tokenizer_parallelism If TRUE this will turn on tokenizer parallelism. Default FALSE.
#' @param logging_level Set the logging level. Default: "warning".
#' Options (ordered from less logging to more logging): critical, error, warning, info, debug
#' @return A tibble with predicted labels and scores for each text variable.
#' The comment of the object show the model-name and computation time.
#' @examples
#' \donttest{
#' # x <- Language_based_assessment_data_8[1:2, 1:2]
#' # word_embeddings_with_layers <- textEmbedLayersOutput(x, layers = 11:12)
#' }
#' @seealso see \code{\link{textEmbedLayerAggregation}} and \code{\link{textEmbed}}
#' @importFrom reticulate source_python
#' @importFrom dplyr bind_cols bind_rows
#' @export
textSentiment <- function(x,
                          model,
                          device = "cpu",
                          tokenizer_parallelism = FALSE,
                          logging_level = "error"){

  #set.seed(seed)
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




