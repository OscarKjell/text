## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----textPredict_examples, eval = FALSE, echo=TRUE----------------------------
# library(text)
# 
# # Example calling a model using the URL
# textPredict(
#   model_info = "valence_facebook_mxbai23_eijsbroek2024",
#   texts = "what is the valence of this text?"
# )
# 
# 
# # Example calling a model having an abbreviation
# textClassify(
#   model_info = "implicitpower_roberta23_nilsson2024",
#   texts = "It looks like they have problems collaborating."
# )

## ----models_table, eval = TRUE, echo=FALSE------------------------------------
library("reactable")
# see vignette: https://glin.github.io/reactable/articles/examples.html#custom-rendering

model_data <- read.csv(system.file("extdata",
                                   "The_L-BAM_Library.csv",
                                   package = "text"),
                       header = TRUE, 
                       skip = 6)

reactable::reactable(
  data = model_data,
  filterable = TRUE,
  defaultPageSize = 20,
  highlight = TRUE, 
  resizable = TRUE,
  theme = reactableTheme(
    borderColor = "#1f7a1f",
    #  stripedColor = "#e6ffe6",
    highlightColor = "#ebfaeb",
    cellPadding = "8px 12px",
    style = list(fontFamily = "-apple-system, BlinkMacSystemFont, Segoe UI, Helvetica, Arial, sans-serif")
  ),
  columns = list(
    Name = colDef(minWidth = 480),
    Construct_Concept_Behaviours	= colDef(minWidth = 280),
    Use_case_binary	= colDef(minWidth = 280),
    Use_case_detailed	= colDef(minWidth = 280),
    Reference	= colDef(minWidth = 280),
    Contact_details	= colDef(minWidth = 280),
    Outcome	= colDef(minWidth = 280),
    Language= colDef(minWidth = 280),
    Language_type= colDef(minWidth = 280),
    Level	= colDef(minWidth = 280),
    N_training= colDef(minWidth = 280),
    N_evaluation_or_N_generalisation= colDef(minWidth = 280),
    Source	= colDef(minWidth = 280),
    Participants_training	= colDef(minWidth = 1680),
    Participants_evaluation_or_generalisation	= colDef(minWidth = 1680),
    Label_types		= colDef(minWidth = 280),
    Language_domain_distribution 		= colDef(minWidth = 280),
    Open_data		= colDef(minWidth = 480),
    Open_code		= colDef(minWidth = 480),
    Model_type		= colDef(minWidth = 280),
    Features		= colDef(minWidth = 280),
    Validation_metric1		= colDef(minWidth = 280),
    N_fold_cv_accuracy_1_not_in_generalisation	= colDef(minWidth = 280),
    Held_out_accuracy_1		= colDef(minWidth = 280),
    Held_out_accuracy_2		= colDef(minWidth = 280),
    SEMP_accuracy_1		= colDef(minWidth = 280),
    Other_metrics_n_fold_cv		= colDef(minWidth = 280),
    Other_metrics_held_out_1		= colDef(minWidth = 280),
    Other_metrics_held_out_2		= colDef(minWidth = 280),
    Other_metrics_SEMP		= colDef(minWidth = 280),
    Other_evaluation		= colDef(minWidth = 280),
    Ethical_approval		= colDef(minWidth = 280),
    Ethical_statement		= colDef(minWidth = 1680),
    Date		= colDef(minWidth = 280),
    License		= colDef(minWidth = 280),
    Original		= colDef(minWidth = 280),
    Miscellaneous		= colDef(minWidth = 280),
    Command_info		= colDef(minWidth = 280),
    Name_description		= colDef(minWidth = 1280),
    Path		= colDef(minWidth = 480),
    Model_Type	= colDef(minWidth = 280)
  ), 
  showPageSizeOptions = TRUE,
  groupBy = "Construct_Concept_Behaviours"
)

