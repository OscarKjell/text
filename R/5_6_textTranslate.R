

#' Translation. (experimental)
#' @param x (string)  The text to be translated.
#' @param source_lang (string)  The input language. Might be needed for multilingual models
#' (it will not have any effect for single pair translation models). using ISO 639-1 Code,
#' such as: "en", "zh", "es", "fr", "de", "it", "sv", "da", "nn".
#' @param target_lang (string)  The desired language output. Might be required for multilingual models
#' (will not have any effect for single pair translation models).
#' @param model (string)  Specify a pre-trained language model that have been fine-tuned on a translation task.
#' @param device (string)  Name of device to use: 'cpu', 'gpu', or 'gpu:k' where k is a specific device number
#' @param tokenizer_parallelism (boolean)  If TRUE this will turn on tokenizer parallelism.
#' @param logging_level (string)  Set the logging level.
#' Options (ordered from less logging to more logging): critical, error, warning, info, debug
#' @param return_incorrect_results (boolean)  Stop returning some incorrectly formatted/structured results.
#' This setting does CANOT evaluate the actual results (whether or not they make sense, exist, etc.).
#' All it does is to ensure the returned results are formatted correctly (e.g., does the question-answering
#' dictionary contain the key "answer", is sentiments from textClassify containing the labels "positive" and "negative").
#' @param return_tensors (boolean)  Whether or not to include the predictions' tensors as token indices in the outputs.
#' @param return_text (boolean)  Whether or not to also output the decoded texts.
#' @param clean_up_tokenization_spaces (boolean)  Whether or not to clean the output from potential extra spaces.
#' @param set_seed (Integer) Set seed.
#' @return A tibble with transalted text.
#' @examples
#' \donttest{
#' # translation_example <- text::textTranslate(
#' #  Language_based_assessment_data_8[1,1:2],
#' #  source_lang = "en",
#' #  target_lang = "fr",
#' #  model = "t5-base")
#' }
#' @seealso see \code{\link{textClassify}}, \code{\link{textGeneration}}, \code{\link{textNER}},
#'  \code{\link{textSum}}, and \code{\link{textQA}}
#' @importFrom reticulate source_python
#' @importFrom tibble as_tibble_col
#' @export
textTranslate <- function(x,
                          source_lang = "",
                          target_lang = "",
                          model = "xlm-roberta-base",
                          device = "cpu",
                          tokenizer_parallelism = FALSE,
                          logging_level = "warning",
                          return_incorrect_results = FALSE,
                          return_tensors = FALSE,
                          return_text = TRUE,
                          clean_up_tokenization_spaces = FALSE,
                          set_seed = 202208L) {
  T1_text_all <- Sys.time()
  # Run python file with HunggingFace interface to state-of-the-art transformers
  reticulate::source_python(system.file("python",
    "huggingface_Interface3.py",
    # envir = NULL,
    package = "text",
    mustWork = TRUE
  ))

  # Select all character variables and make them UTF-8 coded (e.g., BERT wants it that way).
  data_character_variables <- select_character_v_utf8(x)

  ALL_output <- list()
  # Loop over all character variables; i_variables = 1
  for (i_variables in seq_len(length(data_character_variables))) {
    T1_variable <- Sys.time()

    hg_translatiton <- apply(data_character_variables[i_variables], 1,
      hgTransformerGetTranslation,
      source_lang = source_lang,
      target_lang = target_lang,
      model = model,
      device = device,
      tokenizer_parallelism = tokenizer_parallelism,
      logging_level = logging_level,
      return_incorrect_results = return_incorrect_results,
      return_tensors = return_tensors,
      return_text = return_text,
      clean_up_tokenization_spaces = clean_up_tokenization_spaces,
      set_seed = set_seed
    )

    # Sort output into tidy-format
    output1 <- dplyr::bind_rows(hg_translatiton)


    # Add the text variable name to variable names
    x_name <- names(data_character_variables[i_variables])
    names(output1) <- paste0(
      source_lang, "_to_", target_lang,
      "_",
      x_name
    )

    ALL_output[[i_variables]] <- output1
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

  ALL_output1 <- dplyr::bind_cols(ALL_output)

  # Time to complete all variables
  T2_text_all <- Sys.time()
  all_time <- T2_text_all - T1_text_all
  all_time <- sprintf(
    "Duration to predict all variables: %f %s",
    all_time,
    units(all_time)
  )


  # Adding informative comment help(comment)
  comment(ALL_output1) <- paste("Information about the textTranslate.  ",
    "model: ", model, "; ",
    "time: ", all_time, ";",
    "text_version: ", packageVersion("text"), ".",
    sep = "",
    collapse = "\n"
  )
  return(ALL_output1)
}
