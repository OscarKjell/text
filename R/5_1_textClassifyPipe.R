
#' Predict label and probability of a text using a pretrained classifier language model. (experimental)
#' @param x (string)  A character variable or a tibble/dataframe with at least one character variable.
#' @param model (string)  Specification of a pre-trained classifier language model.
#'  For full list of options see pretrained classifier models at
#'  \href{https://huggingface.co/transformers/pretrained_models.html}{HuggingFace}.
#'  For example use "cardiffnlp/twitter-roberta-base-sentiment", "distilbert-base-uncased-finetuned-sst-2-english".
#' @param device (string)  Device to use: 'cpu', 'gpu', or 'gpu:k' where k is a specific device number.
#' @param tokenizer_parallelism (boolean)  If TRUE this will turn on tokenizer parallelism.
#' @param logging_level (string)  Set the logging level.
#' Options (ordered from less logging to more logging): critical, error, warning, info, debug
#' @param force_return_results (boolean)  Stop returning some incorrectly formatted/structured results.
#' This setting CANNOT evaluate the actual results (whether or not they make sense, exist, etc.).
#' All it does is to ensure the returned results are formatted correctly (e.g., does the question-answering
#' dictionary contain the key "answer", is sentiments from textClassify containing the labels "positive"
#'  and "negative").
#' @param return_all_scores (boolean)  Whether to return all prediction scores or just the one of the predicted class.
#' @param function_to_apply (string)  The function to apply to the model outputs to retrieve the scores.
#' There are four different values:
#' "default": if the model has a single label, will apply the sigmoid function on the output.
#' If the model has several labels,
#' the softmax function will be applied on the output.
#' "sigmoid": Applies the sigmoid function on the output.
#' "softmax": Applies the softmax function on the output.
#' "none": Does not apply any function on the output.
#' @param set_seed (Integer) Set seed.
#' @return A tibble with predicted labels and scores for each text variable.
#' The comment of the object show the model-name and computation time.
#' @examples
#' \donttest{
#' # classifications <- textClassify(x = Language_based_assessment_data_8[1:2, 1:2])
#' # classifications
#' # comment(classifications)
#' }
#' @seealso see \code{\link{textClassify}}, \code{\link{textGeneration}}, \code{\link{textNER}},
#'  \code{\link{textSum}}, \code{\link{textQA}}, \code{\link{textTranslate}}
#' @importFrom reticulate source_python
#' @importFrom dplyr bind_cols bind_rows
#' @noRd
textClassifyPipe <- function(
    x,
    model = "distilbert-base-uncased-finetuned-sst-2-english",
    device = "cpu",
    tokenizer_parallelism = FALSE,
    logging_level = "error",
    force_return_results = TRUE,
    return_all_scores = FALSE,
    function_to_apply = NULL,
    set_seed = 202208) {

  T1_textSentiment <- Sys.time()

  # Run python file with HunggingFace interface to state-of-the-art transformers
  reticulate::source_python(system.file("python",
    "huggingface_Interface3.py",
    package = "text",
    mustWork = TRUE
  ))

  # Select all character variables and make them UTF-8 coded (e.g., BERT wants it that way).
  data_character_variables <- select_character_v_utf8(x)

  # check whether models name exist in a predefined list
  registered_true_false <- registered_model_name(model)

  ALL_sentiments <- list()
  # Loop over all character variables; i_variables = 1
  for (i_variables in seq_len(length(data_character_variables))) {
    T1_variable <- Sys.time()
    # Python file function to HuggingFace
    hg_sentiments <- hgTransformerGetSentiment(
      text_strings = data_character_variables[[i_variables]],
      model = model,
      device = device,
      tokenizer_parallelism = tokenizer_parallelism,
      logging_level = logging_level,
      force_return_results = force_return_results,
      return_all_scores = return_all_scores,
      function_to_apply = function_to_apply,
      set_seed = set_seed
    )

    # Sort output into tidy-format
    output1 <- map(hg_sentiments, dplyr::bind_cols) %>%
      dplyr::bind_rows()

    # Add the text variable name to variable names
    x_name <- names(data_character_variables[i_variables])
    names(output1) <- paste0(
      names(output1),
      "_",
      x_name
    )

    ALL_sentiments[[i_variables]] <- output1
    T2_variable <- Sys.time()
    variable_time <- T2_variable - T1_variable
    variable_time <- sprintf(
      "Duration: %f %s",
      variable_time,
      units(variable_time)
    )

    loop_text <- paste(x_name, "completed:",
      variable_time,
      "\n",
      sep = " "
    )

    cat(colourise(loop_text, "green"))
  }

  ALL_sentiments1 <- dplyr::bind_cols(ALL_sentiments)

  # Time to complete all variables
  T2_textSentiment <- Sys.time()
  all_time <- T2_textSentiment - T1_textSentiment
  all_time <- sprintf(
    "Duration to predict all variables: %f %s",
    all_time,
    units(all_time)
  )


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
