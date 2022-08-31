

#' Zero Shot Classification (Experimental)
#' @param sequences (string)  The sequence(s) to classify (not that they will be truncated if the model input is too large).
#' @param candidate_labels (string) The set of class labels that is possible in the to classification of each sequence.
#' It may be a single label, a string of comma-separated labels, or a list of labels.
#' @param hypothesis_template (string; optional)
#' The template that is used for turning each of the label into an NLI-style hypothesis.
#' This template must include a "{}" or similar syntax so that the candidate label can be inserted into the template.
#' For example, the default template is "This example is {}." With the candidate label "sports",
#' this would be fed into the model like "<cls> sequence to classify <sep> This example is sports . <sep>".
#' The default template works well in many cases, but it may be worthwhile to experiment with different templates
#' depending on the task setting (see https://huggingface.co/docs/transformers/).
#' @param multi_label (boolean; optional) It indicates whether multiple candidate labels can be true. If FALSE, the scores
#' are normalized such that the sum of the label likelihoods for each sequence is 1.
#' If TRUE, the labels are considered independent and probabilities are normalized for each candidate by doing a softmax
#' of the entailment score vs. the contradiction score.
#' @param model (string)  Specify a pre-trained language model that have been fine-tuned on a translation task.
#' @param device (string)  Name of device to use: 'cpu', 'gpu', or 'gpu:k' where k is a specific device number
#' @param tokenizer_parallelism (boolean)  If TRUE this will turn on tokenizer parallelism.
#' @param logging_level (string)  Set the logging level.
#' Options (ordered from less logging to more logging): critical, error, warning, info, debug
#' @param return_incorrect_results (boolean)  Stop returning some incorrectly formatted/structured results.
#' This setting does CANOT evaluate the actual results (whether or not they make sense, exist, etc.).
#' All it does is to ensure the returned results are formatted correctly (e.g., does the question-answering
#' dictionary contain the key "answer", is sentiments from textClassify containing the labels "positive" and "negative").
#' @param set_seed (Integer) Set seed.
#' @return A tibble with the result with the following keys:
#' sequence (string) The imputed sequence.
#' labels (string) The labels sorted in the order of likelihood.
#' scores (numeric) The probabilities for each of the labels.
#' @examples
#' \donttest{
#' # ZeroShot_example <- text::textZeroShot(sequences = c("I play football",
#' # "The forest is wonderful"),
#' # candidate_labels = c("sport", "nature", "research"),
#' # model = "facebook/bart-large-mnli")
#' }
#' @seealso see \code{\link{textClassify}}, \code{\link{textGeneration}}, \code{\link{textNER}},
#'  \code{\link{textSum}}, \code{\link{textQA}}, \code{\link{textTranslate}}
#' @importFrom reticulate source_python
#' @importFrom tibble as_tibble_col
#' @export
textZeroShot <- function(sequences,
                         candidate_labels,
                         hypothesis_template = "This example is {}.",
                         multi_label = FALSE,
                         model = "",
                         device = "cpu",
                         tokenizer_parallelism = FALSE,
                         logging_level = "error",
                         return_incorrect_results = FALSE,
                         set_seed = 202208L) {
  T1_text_all <- Sys.time()
  # Run python file with HunggingFace interface to state-of-the-art transformers
  reticulate::source_python(system.file("python",
    "huggingface_Interface3.py",
    # envir = NULL,
    package = "text",
    mustWork = TRUE
  ))

  hg_zeroshot <- hgTransformerGetZeroShot(sequences = sequences,
                                          candidate_labels = candidate_labels,
                                          hypothesis_template = hypothesis_template,
                                          multi_label = multi_label,
                                          model = model,
                                          device = device,
                                          tokenizer_parallelism = tokenizer_parallelism,
                                          logging_level = logging_level,
                                          return_incorrect_results = return_incorrect_results,
                                          set_seed = set_seed)

  T1_text_all <- Sys.time()

  hg_zeroshot1 <- hg_zeroshot %>%
    dplyr::bind_rows() %>%
    dplyr::group_by(sequence) %>%
    dplyr::mutate(
      no = dplyr::row_number()
    ) %>%
    dplyr::ungroup() %>%
    tidyr::pivot_wider(
      sequence,
      names_from = no,
      names_prefix = "x_",
      values_from = c(labels, scores),
      names_sort=TRUE
    )

  # Re-arrange to mix order of labels and scores.
  hg_zeroshot1 <- cbind(hg_zeroshot1[1],
                        hg_zeroshot1[c(matrix(names(hg_zeroshot1)[-1], 2,
                                              byrow = TRUE))])


  # Time to complete all variables
  T2_text_all <- Sys.time()
  all_time <- T2_text_all - T1_text_all
  all_time <- sprintf(
    "Duration to categorise all variables: %f %s",
    all_time,
    units(all_time)
  )


  # Adding informative comment help(comment)
  comment(hg_zeroshot1) <- paste("Information about the textTranslate.  ",
    "model: ", model, "; ",
    "time: ", all_time, ";",
    "text_version: ", packageVersion("text"), ".",
    sep = "",
    collapse = "\n"
  )
  return(hg_zeroshot1)
}




