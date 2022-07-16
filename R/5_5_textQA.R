



#' Named Entity Recognition STILL UNDER DEVELOPMENT
#' Uses any Model for token classification.
#' for predicting the classes of tokens in a sequence: person, organisation, location or miscellaneous).
#' @param x (string)  A character variable or a tibble/dataframe with at least one character variable.
#' @param model (string)  Specify a pre-trained language model that have been fine-tuned on a token classification task
#' (e.g., see "dslim/bert-base-NER").
#' @param device (string)  Name of device to use: 'cpu', 'gpu', or 'gpu:k' where k is a specific device number
#' @param tokenizer_parallelism (boolean)  If TRUE this will turn on tokenizer parallelism. Default FALSE.
#' @param logging_level (string)  Set the logging level. Default: "warning".
#' Options (ordered from less logging to more logging): critical, error, warning, info, debug
#' @param return_incorrect_results (boolean)  Many models are not created to be able to provide classifications - this setting
#' stops them from returning incorrect results.
#' @return A tibble with.
#' @examples
#' \donttest{
#' ner_example <- textQA(question = "why are the ")
#' ner_example
#' }
#' @seealso see \code{\link{textEmbedLayerAggregation}} and \code{\link{textEmbed}}
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
                   topk = 1,
                   doc_stride = 128,
                   max_answer_len = 15,
                   max_seq_len = 384,
                   max_question_len = 64,
                   handle_impossible_answer = FALSE){

  # Run python file with HunggingFace interface to state-of-the-art transformers
  reticulate::source_python(system.file("python",
                                        "huggingface_Interface3.py",
                                        # envir = NULL,
                                        package = "text",
                                        mustWork = TRUE
  ))

  # Select all character variables and make them UTF-8 coded (e.g., BERT wants it that way).
  data_character_variables <- select_character_v_utf8(x)


  hg_QA <- hgTransformerGetQA(question,
                              context,
                              model = '',
                              device = 'cpu',
                              tokenizer_parallelism = False,
                              logging_level = 'warning',
                              return_incorrect_results = False,
                              topk = 1,
                              doc_stride = 128,
                              max_answer_len = 15,
                              max_seq_len = 384,
                              max_question_len = 64,
                              handle_impossible_answer = False)
  return(hg_QA)
}
