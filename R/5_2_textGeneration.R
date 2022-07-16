
# x = "meaning in life is"
# model = 'gpt2'
# device = 'cpu'
# tokenizer_parallelism = FALSE
# logging_level = 'warning'
# return_incorrect_results = FALSE
# return_tensors = FALSE
# return_text = TRUE
# return_full_text = TRUE
# clean_up_tokenization_spaces = FALSE
# prefix = ''
# handle_long_generation = "None"

#' Predicts the words that will follow a specified text prompt. STILL UNDER DEVELOPMENT
#' @param x (string)  A character variable or a tibble/dataframe with at least one character variable.
#' @param model (string)  Specify a pre-trained language model that have been trained with an autoregressive language modeling
#' objective, which includes the uni-directional models (e.g., gpt2).
#' @param device (string)  Name of device to use: 'cpu', 'gpu', or 'gpu:k' where k is a specific device number
#' @param tokenizer_parallelism (boolean)  If TRUE this will turn on tokenizer parallelism. Default FALSE.
#' @param logging_level (string)  Set the logging level. Default: "warning".
#' Options (ordered from less logging to more logging): critical, error, warning, info, debug
#' @param return_incorrect_results (boolean)  Many models are not created to be able to provide classifications - this setting
#' stops them from returning incorrect results.
#' @param return_tensors (boolean)  Whether or not the output should include the prediction tensors (as token indices).
#' @param return_text (boolean)  Whether or not the outputs should include the decoded text.
#' @param return_full_text (boolean) If FALSE only the added text is returned, otherwise the full text is returned.
#'  (This setting is only meaningful if return_text is set to TRUE)
#' @param clean_up_tokenization_spaces (boolean)  Option to clean up the potential extra spaces in the returned text.
#' @param prefix (string) Option to add a prefix to prompt.
# @param handle_long_generation  By default, this function does not handle long generation (those that exceed the model maximum length).
# (more info :https://github.com/huggingface/transformers/issues/14033#issuecomment-948385227).
# This setting provides some ways to work around the problem:
# None: default way, where no particular strategy is applied.
# "hole": Truncates left of input, and leaves a gap that is wide enough to let generation happen.
# (this might truncate a lot of the prompt and not suitable when generation exceed the model capacity)
#' @return A tibble with generated text.
#' @examples
#' \donttest{
#' # generated_text <- textGeneration("The meaning of life is")
#' # generated_text
#' }
#' @seealso see \code{\link{textEmbedLayerAggregation}} and \code{\link{textEmbed}}
#' @importFrom reticulate source_python
#' @importFrom tibble as_tibble_col
#' @export
textGeneration <- function(x,
                           model = 'gpt2',
                           device = 'cpu',
                           tokenizer_parallelism = FALSE,
                           logging_level = 'warning',
                           return_incorrect_results = FALSE,
                           return_tensors = FALSE,
                           return_text = TRUE,
                           return_full_text = TRUE,
                           clean_up_tokenization_spaces = FALSE,
                           prefix = ''
                           #handle_long_generation = "hole"
                           ){

  # Run python file with HunggingFace interface to state-of-the-art transformers
  reticulate::source_python(system.file("python",
                                        "huggingface_Interface3.py",
                                        # envir = NULL,
                                        package = "text",
                                        mustWork = TRUE
  ))

  # Select all character variables and make them UTF-8 coded (e.g., BERT wants it that way).
  data_character_variables <- select_character_v_utf8(x)


  hg_generated <- hgTransformerGetTextGeneration(text_strings = data_character_variables[[1]],
                                 model = model,
                                 device = device,
                                 tokenizer_parallelism = tokenizer_parallelism,
                                 logging_level = logging_level,
                                 return_incorrect_results = return_incorrect_results,
                                 return_tensors = return_tensors,
                                 return_text = return_text,
                                 return_full_text = return_full_text,
                                 clean_up_tokenization_spaces = clean_up_tokenization_spaces,
                                 prefix = prefix
                                 #handle_long_generation = handle_long_generation
                                 )

  hg_generated1 <- tibble::as_tibble_col(hg_generated[[1]]$generated_text,
                                    column_name = "generatedtext")
  return(hg_generated1)
}
