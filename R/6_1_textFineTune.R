
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
#' @param label_names label name in case of classification.
#' @param ... Parameters related to the fine tuning, which can be seen in the text-package file inst/python/arg2.json.
#' @return A folder containing the pretrained model and output data. The model can then be used, for example, by
#' textEmbed() by providing the model parameter with a the path to the output folder.
#' @examples
#' \dontrun{
#' textFineTuneTask(text_outcome_data)
#' }
#' @seealso see \code{\link{textEmbed}}, \code{\link{textEmbed}}
#' @details Information about more parameters see inst/python/args2.json (https://github.com/OscarKjell/text/tree/master/inst/python/args2.json).
#' Descriptions of settings can be found in inst/python/task_finetune.py under "class ModelArguments" and "class DataTrainingArguments" as well as
#' online at https://huggingface.co/docs/transformers/main_classes/trainer.
#' @export
textFineTuneTask <- function(text_outcome_data,
                             model_name_or_path = "bert-base-uncased", # Also how to find my previously created one?
                             output_dir = "./runs",
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
                             label_names = NULL,
                             ...
                             ){

  T1 <- Sys.time()
  set.seed(set_seed)

  text_path <- system.file("python", package = "text")
  # Setting path in python -- so it finds task_finetune module/file
  reticulate::py_run_string(paste0("import sys; sys.path.append('", text_path, "')"))
  reticulate::source_python(system.file("python",
                                        "huggingface_Interface4.py",
                                        # envir = NULL,
                                        package = "text",
                                        mustWork = TRUE
  ))

  if(ncol(text_outcome_data)>2){
    stop("Please only input a text and label column")
  }


  colnames(text_outcome_data) <-  c("text", "label")
  text_outcome_data$idx <- 1:nrow(text_outcome_data)
  text_outcome_data <- text_outcome_data[, c(3, 1, 2)]

  # Ensuring label variable has appropriate type
  if(is_regression){
    text_outcome_data$label <- as.numeric(text_outcome_data$label)
  }
  if(!is_regression){
    text_outcome_data$label <- as.character(text_outcome_data$label)
  }

  # Only include complete cases
  n_befor <- nrow(text_outcome_data)
  text_outcome_data <- text_outcome_data[complete.cases(text_outcome_data),]
  n_after <- nrow(text_outcome_data)

  if(n_befor>n_after){
    print(paste("Removed incomplete cases. Only using", n_after, "complete cases."))
  }

  # Data set partitioning
  train_proportion = 1 - validation_proportion - evaluation_proportion
  total_size = nrow(text_outcome_data)
  props <- c(rep("train",      ceiling(train_proportion*total_size)),
             rep("validation", ceiling(validation_proportion*total_size)),
             rep("evaluation", ceiling(evaluation_proportion*total_size)))
  props <- props[1:total_size]

  train_data1 <-  tibble::as_tibble(text_outcome_data[props=="train", ])
  val_data1   <-  tibble::as_tibble(text_outcome_data[props=="validation", ])
  test_data1  <-  tibble::as_tibble(text_outcome_data[props=="evaluation", ])

  # Setting file to fine-tuning arguments in python
  json_path1 <- paste0(text_path, "/args2.json")

  hgTransformerFineTune(json_path = json_path1,
                        model_name_or_path = model_name_or_path,
                        output_dir = output_dir,
                        text_outcome_df = train_data1,
                        text_outcome_df_val = val_data1,
                        text_outcome_df_test = test_data1,
                        is_regression = is_regression,
                        config_name = config_name,
                        tokenizer_name = tokenizer_name,
                        max_seq_length = max_seq_length,
                        evaluation_strategy = evaluation_strategy,
                        eval_accumulation_steps = eval_accumulation_steps,
                        num_train_epochs = num_train_epochs,
                        past_index = past_index,
                        label_names = label_names,
                        ...)


  # Return all datasets

  T2 <- Sys.time()
  T2-T1
  colourise("Completed",
            fg = "green", bg = NULL
  )

}

