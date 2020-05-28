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
  ".pred", ".", "analysis", "assessment", "df3_glmn_grid",
  "n", "n1", "n1_e", "dot2", "adjusted_p_values.x", "dot.x", "plot_n_word_extreme",
  "plot_n_word_frequency", "plot_n_words_middle", "n_all.x", "adjusted_p_values.y", "dot.y",
  "n_all.y", "colour_categories", "scale_color_identity", "group", "x", "y", "fill"
))
