# Global settings; this is to avoid the
# note (warning) "no visible binding for global variable ‘token_index’
# https://community.rstudio.com/t/how-to-solve-no-visible-binding-for-global-variable-note/28887
utils::globalVariables(c(
  "token_index", "layer_index",
  "SS", "p_values.x", "p_values.y", "words", "n.x",
  "addEqualNrNArows", "applysemrep", "single_wordembeddings_df", "Ndim",
  "semanticrepresentation",
  "guide_legend", "V1",
  "textTrainRandomForest", "textTrainCVpredictions", "textTrainCVpredictionsRF",
  "penalty", "mixture",
  "id1", "y.y", "id",
  ".pred", ".", "analysis", "assessment", "df3_glmn_grid"
))
