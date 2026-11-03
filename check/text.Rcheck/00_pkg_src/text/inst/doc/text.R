## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
evaluate = FALSE

## ----setup, eval = evaluate, warning=FALSE, message=FALSE---------------------
# 
# library(text)
# 
# # View example data including both text and numerical variables
# Language_based_assessment_data_8
# 
# # Transform the text data to BERT word embeddings
# word_embeddings <- textEmbed(
#   texts = Language_based_assessment_data_8[3],
#   model = "bert-base-uncased",
#   layers = -2,
#   aggregation_from_tokens_to_texts = "mean",
#   aggregation_from_tokens_to_word_types = "mean",
#   keep_token_embeddings = FALSE)
# 
# # See how word embeddings are structured
# word_embeddings
# 
# # Save the word embeddings to avoid having to import the text every time. (i.e., remove the ##)
# ## saveRDS(word_embeddings, "word_embeddings.rds")
# 
# # Get the word embeddings again (i.e., remove the ##)
# ## word_embeddings <- readRDS("_YOURPATH_/word_embeddings.rds")

## ----eval = evaluate,  warning=FALSE, message=FALSE---------------------------
# library(text)
# 
# # Examine the relationship between harmonytext word embeddings and the harmony in life rating scale
# model_htext_hils <- textTrain(word_embeddings$texts$harmonywords,
#                               Language_based_assessment_data_8$hilstotal)
# 
# # Examine the correlation between predicted and observed Harmony in life scale scores
# model_htext_hils$results
# 

## ----eval = evaluate, warning=FALSE, message=FALSE----------------------------
# library(text)
# 
# # Pre-process data
# projection_results <- textProjection(
#   words = Language_based_assessment_data_8$harmonywords,
#   word_embeddings = word_embeddings$texts,
#   word_types_embeddings = word_embeddings$word_types,
#   x = Language_based_assessment_data_8$hilstotal,
#   y = Language_based_assessment_data_8$age
# )
# projection_results$word_data
# 

## ----eval = FALSE, warning=FALSE, message=FALSE-------------------------------
# # Participant-permutation null: every word is compared with its own null distribution
# projection_results_participant <- textProjection(
#   words = Language_based_assessment_data_8$harmonywords,
#   word_embeddings = word_embeddings$texts,
#   word_types_embeddings = word_embeddings$word_types,
#   x = Language_based_assessment_data_8$hilstotal,
#   null_method = "participant",
#   Npermutations = 10000
# )

## ----DPP_plot, message=FALSE, warning=FALSE-----------------------------------
library(text) 
# Use data (DP_projections_HILS_SWLS_100) that have been pre-processed with the textProjectionData function; the preprocessed test-data included in the package is called: DP_projections_HILS_SWLS_100
plot_projection <- textProjectionPlot(
  word_data = DP_projections_HILS_SWLS_100,
  y_axes = TRUE,
  title_top = " Supervised Bicentroid Projection of Harmony in life words",
  x_axes_label = "Low vs. High HILS score",
  y_axes_label = "Low vs. High SWLS score",
  position_jitter_hight = 0.5,
  position_jitter_width = 0.8
)
plot_projection$final_plot


