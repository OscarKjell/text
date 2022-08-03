
# x = "Oscar plays football"
# model = "dslim/bert-base-NER"
# device = 'cpu'
# tokenizer_parallelism = FALSE
# logging_level = 'warning'
# return_incorrect_results = FALSE


#' Named Entity Recognition STILL UNDER DEVELOPMENT
#' @param x (string)  A  variable or a tibble/dataframe with at least one character variable.
#' @param model (string)  Specification of a pre-trained language model for token classification that have been fine-tuned on a NER task
#' (e.g., see "dslim/bert-base-NER"). Use for predicting the classes of tokens in a sequence: person, organisation, location or miscellaneous).
#' @param device (string)  Device to use: 'cpu', 'gpu', or 'gpu:k' where k is a specific device number
#' @param tokenizer_parallelism (boolean)  If TRUE this will turn on tokenizer parallelism.
#' @param logging_level (string)  Set the logging level.
#' Options (ordered from less logging to more logging): critical, error, warning, info, debug
#' @param return_incorrect_results (boolean)  Many models are not created to be able to provide NER classifications - this setting
#' stops them from returning incorrect results.
#' @param set_seed (Integer) Set seed.
#' @return A tibble with NER classifications.
#' @examples
#' \donttest{
#' # ner_example <- textNER("Arnes plays football with Daniel")
#' # ner_example
#' }
#' @seealso see \code{\link{textClassify}}, \code{\link{textGeneration}}, \code{\link{textNER}},
#'  \code{\link{textSum}}, \code{\link{textQA}}, \code{\link{textTranslate}}
#' @importFrom reticulate source_python
#' @importFrom tibble as_tibble_col
#' @export
textNER <- function(x,
                    model = "dslim/bert-base-NER",
                    device = 'cpu',
                    tokenizer_parallelism = FALSE,
                    logging_level = 'warning',
                    return_incorrect_results = FALSE,
                    set_seed = 202208) {

  # Run python file with HunggingFace interface to state-of-the-art transformers
  reticulate::source_python(system.file("python",
                                        "huggingface_Interface3.py",
                                        # envir = NULL,
                                        package = "text",
                                        mustWork = TRUE
  ))

  # Select all character variables and make them UTF-8 coded (e.g., BERT wants it that way).
  data_character_variables <- select_character_v_utf8(x)

  hg_NER <- hgTransformerGetNER(text_strings = data_character_variables[[1]],
                                model = model,
                                device = device,
                                tokenizer_parallelism = tokenizer_parallelism,
                                logging_level = logging_level,
                                return_incorrect_results = return_incorrect_results,
                                set_seed = set_seed)


  hg_NER1 <- hg_NER %>%
    bind_rows()
  hg_NER1

  return(hg_NER1)
  }
