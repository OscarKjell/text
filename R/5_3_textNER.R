#' Named Entity Recognition. (experimental)
#' @param x (string)  A  variable or a tibble/dataframe with at least one character variable.
#' @param model (string)  Specification of a pre-trained language model for token classification
#' that have been fine-tuned on a NER task (e.g., see "dslim/bert-base-NER").
#' Use for predicting the classes of tokens in a sequence: person, organisation, location or miscellaneous).
#' @param device (string)  Device to use: 'cpu', 'gpu', or 'gpu:k' where k is a specific device number
#' @param tokenizer_parallelism (boolean)  If TRUE this will turn on tokenizer parallelism.
#' @param logging_level (string)  Set the logging level.
#' Options (ordered from less logging to more logging): critical, error, warning, info, debug
#' @param force_return_results (boolean)  Stop returning some incorrectly formatted/structured results.
#' This setting does CANOT evaluate the actual results (whether or not they make sense, exist, etc.).
#' All it does is to ensure the returned results are formatted correctly (e.g., does the question-answering
#' dictionary contain the key "answer", is sentiments from textClassify containing the labels "positive"
#' and "negative").
#' @param set_seed (Integer) Set seed.
#' @return A list with tibble(s) with NER classifications for each column.
#' @examples
#' \donttest{
#' # ner_example <- textNER("Arnes plays football with Daniel")
#' # ner_example
#' }
#' @seealso see \code{\link{textClassify}}, \code{\link{textGeneration}}, \code{\link{textNER}},
#'  \code{\link{textSum}}, \code{\link{textQA}}, \code{\link{textTranslate}}
#' @importFrom reticulate source_python
#' @importFrom tibble as_tibble_col
#' @importFrom purrr map
#' @export
textNER <- function(x,
                    model = "dslim/bert-base-NER",
                    device = "cpu",
                    tokenizer_parallelism = FALSE,
                    logging_level = "error",
                    force_return_results = FALSE,
                    set_seed = 202208L) {
  T1_text_all <- Sys.time()

  # Run python file with HunggingFace interface to state-of-the-art transformers
  reticulate::source_python(system.file("python",
    "huggingface_Interface3.py",
    package = "text",
    mustWork = TRUE
  ))

  # Select all character variables and make them UTF-8 coded (e.g., BERT wants it that way).
  data_character_variables <- select_character_v_utf8(x)

  ALL_output <- list()
  # Loop over all character variables; i_variables = 1
  for (i_variables in seq_len(length(data_character_variables))) {
    T1_variable <- Sys.time()

    hg_NER <- purrr::map(data_character_variables[[i_variables]],
      hgTransformerGetNER,
      model = model,
      device = device,
      tokenizer_parallelism = tokenizer_parallelism,
      logging_level = logging_level,
      force_return_results = force_return_results,
      set_seed = set_seed
    )


    ## Sort output into tidy-format
    # Name each hg_NER output to get below to work
    names(hg_NER) <- paste0("variable_row", seq_len(length(hg_NER)))
    # Make each portion of the objects in the list tibbles
    output1 <- purrr::map(hg_NER, dplyr::bind_rows)
    # Combine the tibbles to one tibble (and include NA if the result is empty)
    output <- dplyr::bind_rows(output1, .id = "NamesNer") %>%
      tidyr::complete(NamesNer = names(hg_NER)) %>%
      select(-NamesNer)

    ALL_output[[i_variables]] <- output
    # Add the text variable name to variable names
    x_name <- names(data_character_variables[i_variables])

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

  names(ALL_output) <- paste0(names(data_character_variables), "_NER")

  # Time to complete all variables
  T2_text_all <- Sys.time()
  all_time <- T2_text_all - T1_text_all
  all_time <- sprintf(
    "Duration to predict all variables: %f %s",
    all_time,
    units(all_time)
  )


  # Adding informative comment help(comment)
  comment(ALL_output) <- paste("Information about the textNER.  ",
    "model: ", model, "; ",
    "time: ", all_time, ";",
    "text_version: ", packageVersion("text"), ".",
    sep = "",
    collapse = "\n"
  )
  return(ALL_output)
}
