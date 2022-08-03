# x = "Happiness is something that people seek to find, yet what defines happiness can vary from one person to the next. When most people talk about happiness, they might be talking about how they feel in the present moment, or they might be referring to a more general sense of how they feel about life overall."
# x = Language_based_assessment_data_8[1:2,1:2]
# model = 't5-small'
# device = 'cpu'
# tokenizer_parallelism = FALSE
# logging_level = 'warning'
# return_incorrect_results = FALSE
# return_text = TRUE
# return_tensors = FALSE
# clean_up_tokenization_spaces = FALSE

#' Summarize texts STILL UNDER DEVELOPMENT
#' @param x (string)  A variable or a tibble/dataframe with at least one character variable.
#' @param min_length (explicit integer; e.g., 10L)  The minimum number of tokens in the summed output.
#' @param max_length (explicit integer higher than min_length; e.g., 20L)  The maximum number of tokens
#' in the summed output.
#' @param model (string)  Specififcation of a pre-trained language model that have been fine-tuned on a summarization task,
#' such as ’bart-large-cnn’, ’t5-small’, ’t5-base’, ’t5-large’, ’t5-3b’, ’t5-11b’.
#' @param device (string)  Device to use: 'cpu', 'gpu', or 'gpu:k' where k is a specific device number.
#' @param tokenizer_parallelism (boolean)  If TRUE this will turn on tokenizer parallelism.
#' @param logging_level (string)  Set the logging level.
#' Options (ordered from less logging to more logging): critical, error, warning, info, debug
#' @param return_incorrect_results (boolean)  Many models are not created to be able to provide summerization - this setting
#' stops them from returning incorrect results.
#' @param return_text (boolean)  Whether or not the outputs should include the decoded text.
#' @param return_tensors (boolean)  Whether or not the output should include the prediction tensors (as token indices).
#' @param clean_up_tokenization_spaces (boolean)  Option to clean up the potential extra spaces in the returned text.
#' @param set_seed (Integer) Set seed.
#' @return A tibble with summed text(s).
#' @examples
#' \donttest{
#'   # sum_examples <- textSum(Language_based_assessment_data_8[1:2,1:2],
#'   # min_length = 5L,
#'   # max_length = 10L)
#' }
#' @seealso see \code{\link{textClassify}}, \code{\link{textGeneration}}, \code{\link{textNER}},
#'  \code{\link{textSum}}, \code{\link{textQA}}, \code{\link{textTranslate}}
#' @importFrom reticulate source_python
#' @importFrom tibble as_tibble_col
#' @export
textSum <- function(x,
                    min_length = 10L,
                    max_length = 20L,
                    model = 't5-small',
                    device = 'cpu',
                    tokenizer_parallelism = FALSE,
                    logging_level = 'warning',
                    return_incorrect_results = FALSE,
                    return_text = TRUE,
                    return_tensors = FALSE,
                    clean_up_tokenization_spaces = FALSE,
                    set_seed = 202208){

  T1_text_all <- Sys.time()

  # Run python file with HunggingFace interface to state-of-the-art transformers
  reticulate::source_python(system.file("python",
                                        "huggingface_Interface3.py",
                                        # envir = NULL,
                                        package = "text",
                                        mustWork = TRUE
  ))

  # Select all character variables and make them UTF-8 coded (e.g., BERT wants it that way).
  data_character_variables <- select_character_v_utf8(x)

  ALL_output <- list()
  # Loop over all character variables; i_variables = 1
  for (i_variables in seq_len(length(data_character_variables))) {
    T1_variable <- Sys.time()


     hg_sum <- hgTransformerGetSummarization(text_strings = data_character_variables[[i_variables]],
                                   model = model,
                                   device = device,
                                   tokenizer_parallelism = tokenizer_parallelism,
                                   logging_level = logging_level,
                                   return_incorrect_results = return_incorrect_results,
                                   return_text = return_text,
                                   return_tensors = return_tensors,
                                   clean_up_tokenization_spaces = clean_up_tokenization_spaces,
                                   min_length = min_length,
                                   max_length = max_length,
                                   set_seed =  set_seed)

     output1 <- dplyr::bind_rows(hg_sum)

      # Add the text variable name to variable names
      x_name <- names(data_character_variables[i_variables])
      names(output1) <- paste0("sum_",
                               x_name)

      ALL_output[[i_variables]] <- output1

      T2_variable <- Sys.time()
      variable_time <- T2_variable - T1_variable
      variable_time <- sprintf("Duration: %f %s",
                               variable_time,
                               units(variable_time))

      loop_text <- paste(x_name, "completed:",
                         variable_time,
                         "\n",
                         sep = " ")

      cat(colourise(loop_text, "green"))

  }

  ALL_output1 <- dplyr::bind_cols(ALL_output)

  #Time to complete all variables
  T2_text_all <- Sys.time()
  all_time <- T2_text_all - T1_text_all
  all_time <- sprintf("Duration to predict all variables: %f %s",
                      all_time,
                      units(all_time))


  # Adding informative comment help(comment)
  comment(ALL_output1) <- paste("Information about the textSum  ",
                                "model: ", model, "; ",
                                "time: ", all_time, ";",
                                "text_version: ", packageVersion("text"), ".",
                                sep = "",
                                collapse = "\n"
  )
  return(ALL_output1)

  }
