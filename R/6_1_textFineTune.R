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


# Task Adapted Pre-Training
#' @param text_outcome_data A dataframe, where the first column contain text data,
#' and the second column the to-be-predicted variable (numeric or categorical).
#' @param model (string) Path to the foundation model to use.
#' @param output_dir (string) Path to the output directory.
#' @param validation_proportion (Numeric) Proportion of the text_outcome_data to be used for validation.
#' @param evaluation_proportion (Numeric) Proportion of the text_outcome_data to be used for evaluation.
#' @param is_regression (Boolean) TRUE for regression tasks, FALSE for classification.
#' @param set_seed (Numeric) Set the seed
#' @return A folder containing the pretrained model and output data. The model can then be used, for example, by
#' textEmbed() by providing the model parameter with a the path to the output folder.
#' @examples
#' \donttest{
#' textFineTuneTask(text_outcome_data)
#' }
#' @seealso see \code{\link{textEmbed}}, \code{\link{textEmbed}}
#' @export
textFineTuneTask <- function(text_outcome_data,
                             model = "bert-base-uncased", # Also how to find my previously created one?
                             output_dir = "./runs/trial",
                             validation_proportion = 0.10,
                             evaluation_proportion = 0.10,
                             is_regression = TRUE,
                             set_seed = 2022
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

  # Data set partitioning help(sample)
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

  # /Users/oscarkjell/Desktop/1 Projects/0 Research/0 text r-package/text/
  hgTransformerFineTune(json_path = "inst/python/args2.json",
                        text_outcome_df = text_outcome_df,
                        text_outcome_df_val = text_outcome_df_val,
                        text_outcome_df_test = text_outcome_df_test,
                        is_regression = is_regression)


  T2 <- Sys.time()
  T2-T1

}

# DAPT
