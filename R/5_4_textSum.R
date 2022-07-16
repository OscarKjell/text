#x = "Happiness is something that people seek to find, yet what defines happiness can vary from one person to the next. When most people talk about happiness, they might be talking about how they feel in the present moment, or they might be referring to a more general sense of how they feel about life overall."
#model = 't5-small'
#device = 'cpu'
#tokenizer_parallelism = FALSE
#logging_level = 'warning'
#return_incorrect_results = FALSE
#return_text = TRUE
#return_tensors = FALSE
#clean_up_tokenization_spaces = FALSE

#' Summarize text such as news articles.
#' @param x (string)  A character variable or a tibble/dataframe with at least one character variable.
#' @param model (string)  Specify a pre-trained language model that have been fine-tuned on a summarization task,
#' such as ’bart-large-cnn’, ’t5-small’, ’t5-base’, ’t5-large’, ’t5-3b’, ’t5-11b’.
#' @param device (string)  Name of device to use: 'cpu', 'gpu', or 'gpu:k' where k is a specific device number
#' @param tokenizer_parallelism (boolean)  If TRUE this will turn on tokenizer parallelism. Default FALSE.
#' @param logging_level (string)  Set the logging level. Default: "warning".
#' Options (ordered from less logging to more logging): critical, error, warning, info, debug
#' @param return_incorrect_results (boolean)  Many models are not created to be able to provide classifications - this setting
#' stops them from returning incorrect results.
#' @param return_text (boolean)  Whether or not the outputs should include the decoded text.
#' @param return_tensors (boolean)  Whether or not the output should include the prediction tensors (as token indices).
#' @param clean_up_tokenization_spaces (boolean)  Option to clean up the potential extra spaces in the returned text.
#' @return A tibble with.
#' @examples
#' \donttest{
#' # sum_example <- textSum("The meaning of life is")
#' }
#' @seealso see \code{\link{textEmbedLayerAggregation}} and \code{\link{textEmbed}}
#' @importFrom reticulate source_python
#' @importFrom tibble as_tibble_col
#' @export
textSum <- function(x,
                    model = '',
                    device = 'cpu',
                    tokenizer_parallelism = FALSE,
                    logging_level = 'warning',
                    return_incorrect_results = FALSE,
                    return_text = TRUE,
                    return_tensors = FALSE,
                    clean_up_tokenization_spaces = FALSE){

  # Run python file with HunggingFace interface to state-of-the-art transformers
  reticulate::source_python(system.file("python",
                                        "huggingface_Interface3.py",
                                        # envir = NULL,
                                        package = "text",
                                        mustWork = TRUE
  ))

  # Select all character variables and make them UTF-8 coded (e.g., BERT wants it that way).
  data_character_variables <- select_character_v_utf8(x)


  hg_sum <- hgTransformerGetSummarization(text_strings = data_character_variables[[1]],
                                model = model,
                                device = device,
                                tokenizer_parallelism = tokenizer_parallelism,
                                logging_level = logging_level,
                                return_incorrect_results = return_incorrect_results,
                                return_text = return_text,
                                return_tensors = return_tensors,
                                clean_up_tokenization_spaces = clean_up_tokenization_spaces)
  hg_sum

  return(hg_sum)
  }
