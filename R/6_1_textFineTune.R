# .rs.restartR()
# library(text)
# library(tidyverse)
# set_seed - Result DOES NOT replicate.

# Decide which arguments to include.
# where to default save output? (cld include it in .cash -- get() )
# argument to name model

# OK: Stratify sampling!

#textEmbed("hello",
#          model = "roberta-base")
#help("textEmbed")
#textrpp_install(rpp_version = c("torch==1.11.0", "transformers==4.22.0", #give version number version torch == 1.11.0 transformers==4.22.0
#                                "numpy==1.23.1", "nltk",
#                                "datasets", "evaluate",
#                                "scipy==1.9.3",
#                                "scikit-learn"))
#textrpp_initialize()
#reticulate::source_python("/Users/oscarkjell/Desktop/1 Projects/0 Research/0 text r-package/text/inst/python/huggingface_Interface4.py")



#' Task Adapted Pre-Training (experimental)
#' @param text_outcome_data A dataframe, where the first column contain text data,
#' and the second column the to-be-predicted variable (numeric or categorical).
#' @param model_name_or_path (string) Path to foundation/pretrained model or model identifier from huggingface.co/models
#' @param output_dir (string) Path to the output directory.
#' @param validation_proportion (Numeric) Proportion of the text_outcome_data to be used for validation.
#' @param evaluation_proportion (Numeric) Proportion of the text_outcome_data to be used for evaluation.
#' @param is_regression (Boolean) TRUE for regression tasks, FALSE for classification.
#' @param config_name (String) Pretrained config name or path if not the same as model_name.
#' @param tokenizer_name (String) Pretrained tokenizer name or path if not the same as model_name
#' @param max_seq_length (Numeric) The maximum total input sequence length after tokenization. Sequences longer
#' than this will be truncated, sequences shorter will be padded.
#' @param evaluation_strategy (String or IntervalStrategy) — The evaluation strategy to adopt during training.
#' Possible values are:
#' "no": No evaluation is done during training.
#' "steps": Evaluation is done (and logged) every eval_steps.
#' "epoch": Evaluation is done at the end of each epoch.
#' @param eval_accumulation_steps (Integer) Number of predictions steps to accumulate the output tensors for,
#' before moving the results to the CPU. If left unset, the whole predictions are accumulated on GPU/TPU
#' before being moved to the CPU (faster but requires more memory).
#' @param num_train_epochs (Numeric) Total number of training epochs to perform
#' (if not an integer, will perform the decimal part percents of the last epoch before stopping training).
#' @param past_index (Numeric, defaults to -1) Some models like TransformerXL or XLNet can make use of the past hidden states
#' for their predictions. If this argument is set to a positive int, the Trainer will use the corresponding output
#' (usually index 2) as the past state and feed it to the model at the next training step under the keyword argument mems.
#' @param set_seed (Numeric) Set the seed
#' @param ... Parameters related to the fine tuning set in text-package file inst/python/arg2.json.
#' @return A folder containing the pretrained model and output data. The model can then be used, for example, by
#' textEmbed() by providing the model parameter with a the path to the output folder.
#' @examples
#' \donttest{
#' textFineTuneTask(text_outcome_data)
#' }
#' @seealso see \code{\link{textEmbed}}, \code{\link{textEmbed}}
#' @details Information about more parameters see inst/python/args2.json (https://github.com/OscarKjell/text/tree/master/inst/python/args2.json).
# Descriptions of settings can be found in inst/python/task_finetune.py under "class ModelArguments" and class DataTrainingArguments as well as
# online at https://huggingface.co/docs/transformers/main_classes/trainer.
#' @export
textFineTuneTask <- function(text_outcome_data,
                             model_name_or_path = "bert-base-uncased", # Also how to find my previously created one?
                             output_dir = "./runs/trial",
                             validation_proportion = 0.10,
                             evaluation_proportion = 0.10,
                             is_regression = TRUE,
                             config_name = NULL,
                             tokenizer_name = NULL,
                             max_seq_length = 128L,
                             evaluation_strategy = "epoch",
                             eval_accumulation_steps = NULL,
                             num_train_epochs = 3,
                             past_index = -1,
                             set_seed = 2022,
                             ...
                             ){

  T1 <- Sys.time()
  set.seed(set_seed)
  # Run python file with HunggingFace interface to state-of-the-art transformers
  reticulate::source_python(system.file("python",
                                        "huggingface_Interface4.py",
                                        # envir = NULL,
                                        package = "text",
                                        mustWork = TRUE
  ))

  colnames(text_outcome_data) <-  c("text", "label")

  # Data set partitioning
  props <- sample(c("train", "validation", "evaluation"),
               size=nrow(text_outcome_data),
               replace=TRUE,
               prob = c(
                 1 - validation_proportion - evaluation_proportion,
                 validation_proportion,
                 evaluation_proportion
                 )
               )

  text_outcome_df      = text_outcome_data[props=="train", ]
  text_outcome_df_val  = text_outcome_data[props=="validation", ]
  text_outcome_df_test = text_outcome_data[props=="evaluation", ]

  hgTransformerFineTune(json_path = "inst/python/args2.json",
                        model_name_or_path = model_name_or_path,
                        output_dir = output_dir,
                        text_outcome_df = text_outcome_df,
                        text_outcome_df_val = text_outcome_df_val,
                        text_outcome_df_test = text_outcome_df_test,
                        is_regression = is_regression,
                        config_name = config_name,
                        tokenizer_name = tokenizer_name,
                        max_seq_length = max_seq_length,
                        evaluation_strategy = evaluation_strategy,
                        eval_accumulation_steps = eval_accumulation_steps,
                        num_train_epochs = num_train_epochs,
                        #past_index = past_index,
                        ...)

  T2 <- Sys.time()
  T2-T1

}

# DAPT
