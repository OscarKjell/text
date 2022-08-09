

#' Question Answering. (experimental)
#' @param question (string)  A question
#' @param context (string)  The context(s) where the model will look for the answer.
#' @param model (string)  HuggingFace name of a pre-trained language model that have been fine-tuned on a question answering task.
#' @param device (string)  Device to use: 'cpu', 'gpu', or 'gpu:k' where k is a specific device number
#' @param tokenizer_parallelism (boolean)  If TRUE this will turn on tokenizer parallelism.
#' @param logging_level (string)  Set the logging level.
#' Options (ordered from less logging to more logging): critical, error, warning, info, debug
#' @param return_incorrect_results (boolean)  Stop returning some incorrectly formatted/structured results. This setting does CANOT evaluate the actual results (whether or not they make sense, exist, etc.).
#' All it does is to ensure the returned results are formatted correctly (e.g., does the question-answering dictionary contain the key "answer", is sentiments from textClassify containing the labels "positive" and "negative").
#' @param top_k (integer) (int)  Indicates number of possible answer span(s) to get from the model output.
#' @param doc_stride (integer)   If the context is too long to fit with the question for the model, it will be split into overlapping chunks.
#' This setting controls the overlap size.
#' @param max_answer_len (integer)  Max answer size to be extracted from the model’s output.
#' @param max_seq_len (integer)  The max total sentence length (context + question) in tokens of each chunk passed to the model.
#'  If needed, the context is split in chunks (using doc_stride as overlap).
#' @param max_question_len (integer)   The max question length after tokenization. It will be truncated if needed.
#' @param handle_impossible_answer (boolean)  Whether or not impossible is accepted as an answer.
#' @param set_seed (Integer) Set seed.
#' @return Answers.
#' @examples
#' \donttest{
#' #   qa_examples <- textQA(question = "Which colour have trees?",
#' #     context = "Trees typically have leaves, are mostly green and like water.")
#' }
#' @seealso see \code{\link{textClassify}}, \code{\link{textGeneration}}, \code{\link{textNER}},
#'  \code{\link{textSum}}, \code{\link{textQA}}, \code{\link{textTranslate}}
#' @importFrom reticulate source_python
#' @importFrom tibble as_tibble_col
#' @export
textQA <- function(question,
                   context,
                   model = '',
                   device = 'cpu',
                   tokenizer_parallelism = FALSE,
                   logging_level = 'warning',
                   return_incorrect_results = FALSE,
                   top_k = 1L,
                   doc_stride = 128L,
                   max_answer_len = 15L,
                   max_seq_len = 384L,
                   max_question_len = 64L,
                   handle_impossible_answer = FALSE,
                   set_seed = 202208L){

  T1_text_all <- Sys.time()

  # Run python file with HunggingFace interface to state-of-the-art transformers
  reticulate::source_python(system.file("python",
                                        "huggingface_Interface3.py",
                                        # envir = NULL,
                                        package = "text",
                                        mustWork = TRUE
  ))

  ALL_output <- list()
  # Loop over all character variables; i_variables = 1
  for (i_variables in seq_len(length(question))) {
    T1_variable <- Sys.time()

    hg_QA <- hgTransformerGetQA(question = question[[i_variables]],
                                context = context[[i_variables]],
                                model = model,
                                device = device,
                                tokenizer_parallelism = tokenizer_parallelism,
                                logging_level = logging_level,
                                return_incorrect_results = return_incorrect_results,
                                top_k = top_k,
                                doc_stride = doc_stride,
                                max_answer_len = max_answer_len,
                                max_seq_len = max_seq_len,
                                max_question_len = max_question_len,
                                handle_impossible_answer = handle_impossible_answer,
                                set_seed = set_seed)[[1]]

    output1 <- dplyr::bind_rows(hg_QA)

    ALL_output[[i_variables]] <- output1

    T2_variable <- Sys.time()
    variable_time <- T2_variable - T1_variable
    variable_time <- sprintf("Duration: %f %s",
                             variable_time,
                             units(variable_time))

    loop_text <- paste(question[[i_variables]], "completed:",
                       variable_time,
                       "\n",
                       sep = " ")

    cat(colourise(loop_text, "green"))

  }

  ALL_output <- dplyr::bind_rows(ALL_output)

  #Time to complete all variables
  T2_text_all <- Sys.time()
  all_time <- T2_text_all - T1_text_all
  all_time <- sprintf("Duration to predict all variables: %f %s",
                      all_time,
                      units(all_time))


  # Adding informative comment help(comment)
  comment(ALL_output) <- paste("Information about the textSum  ",
                                "model: ", model, "; ",
                                "time: ", all_time, ";",
                                "text_version: ", packageVersion("text"), ".",
                                sep = "",
                                collapse = "\n"
  )
  return(ALL_output)
}
