

#context = "Oh, Yeah, For Now, But The Beeper's Gonna Be Making A Comeback. Technology's Cyclical."
#question = "Who is Liz Lemon?"
#model = ''
#device = 'cpu'
#tokenizer_parallelism = FALSE
#logging_level = 'warning'
#return_incorrect_results = FALSE
#topk = 1
#doc_stride = 128
#max_answer_len = 15
#max_seq_len = 384
#max_question_len = 64
#handle_impossible_answer = FALSE
#set_seed = 202208

#' Question Answering STILL UNDER DEVELOPMENT
#' @param question (string)  A question
#' @param context (string)  The context(s) where the model will look for the answer.
#' @param model (string)  HuggingFace name of a pre-trained language model that have been fine-tuned on a question answering task.
#' @param device (string)  Device to use: 'cpu', 'gpu', or 'gpu:k' where k is a specific device number
#' @param tokenizer_parallelism (boolean)  If TRUE this will turn on tokenizer parallelism.
#' @param logging_level (string)  Set the logging level.
#' Options (ordered from less logging to more logging): critical, error, warning, info, debug
#' @param return_incorrect_results (boolean)  Many models are not created to be able to provide question -- answering, so this setting
#' stops them from returning incorrect results.
#' @param topk (integer) (int)  Indicates number of possible answer span(s) to get from the model output.
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
                   topk = 1,
                   doc_stride = 128,
                   max_answer_len = 15,
                   max_seq_len = 384,
                   max_question_len = 64,
                   handle_impossible_answer = FALSE,
                   set_seed = 202208){

  # Run python file with HunggingFace interface to state-of-the-art transformers
  reticulate::source_python(system.file("python",
                                        "huggingface_Interface3.py",
                                        # envir = NULL,
                                        package = "text",
                                        mustWork = TRUE
  ))

  hg_QA <- hgTransformerGetQA(question = question,
                              context = context,
                              model = model,
                              device = device,
                              tokenizer_parallelism = tokenizer_parallelism,
                              logging_level = logging_level,
                              return_incorrect_results = return_incorrect_results,
                             # topk = topk,
                             # doc_stride = doc_stride,
                              max_answer_len = max_answer_len,
                             # max_seq_len = max_seq_len,
                              max_question_len = max_question_len,
                              handle_impossible_answer = handle_impossible_answer,
                              set_seed = set_seed)
  return(hg_QA)
}
