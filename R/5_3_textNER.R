



x = "Oscar plays football"
model = "dslim/bert-base-NER"
device = 'cpu'
tokenizer_parallelism = FALSE
logging_level = 'warning'
return_incorrect_results = FALSE


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
#' ner_example <- textNER("The meaning of life is")
#' ner_example
#' }
#' @seealso see \code{\link{textEmbedLayerAggregation}} and \code{\link{textEmbed}}
#' @importFrom reticulate source_python
#' @importFrom tibble as_tibble_col
#' @export
textNER <- function(x,
                    model = "dslim/bert-base-NER",
                    device = 'cpu',
                    tokenizer_parallelism = FALSE,
                    logging_level = 'warning',
                    return_incorrect_results = FALSE) {

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
                                return_incorrect_results = return_incorrect_results)


  hg_NER1 <- hg_NER %>%
    bind_rows()
  hg_NER1

  return(hg_NER1)
  }
