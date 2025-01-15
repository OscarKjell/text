x = "The meaning of life is"
model = "gpt2"
device = "cpu"
tokenizer_parallelism = FALSE
logging_level = "warning"
force_return_results = FALSE
return_tensors = TRUE
return_tensors = FALSE
return_full_text = FALSE
clean_up_tokenization_spaces = FALSE
prefix = ""
handle_long_generation = "hole"
set_seed = 22L


#' Text generation
#'
#' textGeneration() predicts the words that will follow a specified text prompt. (experimental)
#' @param x (string)  A variable or a tibble/dataframe with at least one character variable.
#' @param model (string)  Specification of a pre-trained language model that have been trained with an
#' autoregressive language modeling objective, which includes the uni-directional models (e.g., gpt2).
#' @param device (string)  Device to use: 'cpu', 'gpu', or 'gpu:k' where k is a specific device number
#' @param tokenizer_parallelism (boolean)  If TRUE this will turn on tokenizer parallelism.
#' @param max_length (Integer)  The maximum length the generated tokens can have. Corresponds to the length of the input prompt + `max_new_tokens`. Its effect is overridden by `max_new_tokens`, if also set. Defaults to NULL.
#' @param max_new_tokens (Integer)  The maximum numbers of tokens to generate, ignoring the number of tokens in the prompt. The default value is 20.
#' @param min_length (Integer)  The minimum length of the sequence to be generated. Corresponds to the length of the input prompt + `min_new_tokens`. Its effect is overridden by `min_new_tokens`, if also set. The default value is 0.
#' @param min_new_tokens (Integer)  The minimum numbers of tokens to generate, ignoring the number of tokens in the prompt. Default is NULL.
#' @param logging_level (string)  Set the logging level.
#' Options (ordered from less logging to more logging): critical, error, warning, info, debug
#' @param force_return_results (boolean)  Stop returning some incorrectly formatted/structured results.
#' This setting does CANOT evaluate the actual results (whether or not they make sense, exist, etc.).
#' All it does is to ensure the returned results are formatted correctly (e.g., does the question-answering
#' dictionary contain the key "answer", is sentiments from textClassify containing the labels "positive"
#'  and "negative").
#' @param return_tensors (boolean)  Whether or not the output should include the prediction tensors (as token indices).
#' @param return_full_text (boolean) If FALSE only the added text is returned, otherwise the full text is returned.
#'  (This setting is only meaningful if return_text is set to TRUE)
#' @param clean_up_tokenization_spaces (boolean)  Option to clean up the potential extra spaces in the returned text.
#' @param prefix (string) Option to add a prefix to prompt.
#' @param handle_long_generation  By default, this function does not handle long generation
#' (those that exceed the model maximum length).
#' @param set_seed (Integer) Set seed.
#' (more info :https://github.com/huggingface/transformers/issues/14033#issuecomment-948385227).
#' This setting provides some ways to work around the problem:
#' None: default way, where no particular strategy is applied.
#' "hole": Truncates left of input, and leaves a gap that is wide enough to let generation happen.
#' (this might truncate a lot of the prompt and not suitable when generation exceed the model capacity)
#' @return A tibble with generated text.
#' @examples
#' \donttest{
#' # generated_text <- textGeneration("The meaning of life is")
#' # generated_text
#' }
#' @seealso see \code{\link{textClassify}}, \code{\link{textNER}},
#'  \code{\link{textSum}}, \code{\link{textQA}}, \code{\link{textTranslate}}
#' @importFrom reticulate source_python
#' @importFrom tibble as_tibble_col
#' @export
textGeneration <- function(x,
                           model = "gpt2",
                           device = "cpu",
                           tokenizer_parallelism = FALSE,
                           max_length = NULL,
                           max_new_tokens = 20,
                           min_length = 0,
                           min_new_tokens = NULL,
                           logging_level = "warning",
                           force_return_results = FALSE,
                           return_tensors = FALSE,
                           return_full_text = TRUE,
                           clean_up_tokenization_spaces = FALSE,
                           prefix = "",
                           handle_long_generation = NULL,
                           set_seed = 202208L) {
  T1_text_all <- Sys.time()
  # Run python file with HunggingFace interface to state-of-the-art transformers
  reticulate::source_python(system.file("python",
    "huggingface_Interface3.py",
    package = "text",
    mustWork = TRUE
  ))

  # Convert integer arguments explicitly for Python
  ## `max_length` and `max_length` are converted inside the Python function
  if (!is.null(set_seed)) set_seed <- as.integer(set_seed)


  # Select all character variables and make them UTF-8 coded (e.g., BERT wants it that way).
  data_character_variables <- select_character_v_utf8(x)

  ALL_output <- list()
  # Loop over all character variables; i_variables = 1
  for (i_variables in seq_len(length(data_character_variables))) {
    T1_variable <- Sys.time()


    hg_generated <- apply(data_character_variables[i_variables],
      1,
      hgTransformerGetTextGeneration,
      model = model,
      device = device,
      tokenizer_parallelism = tokenizer_parallelism,
      max_length = max_length,
      max_new_tokens = max_new_tokens,
      min_length = min_length,
      min_new_tokens = min_new_tokens,
      logging_level = logging_level,
      force_return_results = force_return_results,
      return_tensors = return_tensors,
      return_full_text = return_full_text,
      clean_up_tokenization_spaces = clean_up_tokenization_spaces,
      prefix = prefix,
      handle_long_generation = handle_long_generation,
      set_seed = set_seed
    )

    # Sort output into tidy-format
    if (return_tensors == FALSE) {
      output1 <- dplyr::bind_rows(hg_generated[[1]][[1]][[1]])
      output1
    }

    if (return_tensors == TRUE) {
      output1 <- hg_generated
    }

    # Add the text variable name to variable names
    x_name <- names(data_character_variables[i_variables])
    names(output1) <- paste0(x_name, "_generated")

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

    message(colourise(loop_text, "green"))
  }

  ALL_output1 <- dplyr::bind_cols(ALL_output)

  # Time to complete all variables
  T2_text_all <- Sys.time()
  all_time <- T2_text_all - T1_text_all
  all_time <- sprintf(
    "Duration to translate all variables: %f %s",
    all_time,
    units(all_time)
  )


  # Adding informative comment help(comment)
  comment(ALL_output1) <- paste("Information about the textGeneration settings.  ",
    "model: ", model, "; ",
    "time: ", all_time, "; ",
    "text_version: ", packageVersion("text"), ".",
    sep = "",
    collapse = "\n"
  )

  return(ALL_output1)
}
