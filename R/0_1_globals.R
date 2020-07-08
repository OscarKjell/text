# devtools::document()
#Global settings; this is to avoid the
# note (warning) "no visible binding for global variable ‘token_index’
# https://community.rstudio.com/t/how-to-solve-no-visible-binding-for-global-variable-note/28887
utils::globalVariables(c(
  "token_index", "layer_index",
  "SS", "p_values.x", "p_values.y", "words", "n.x", "n.y",
  "addEqualNrNArows", "applysemrep", "single_wordembeddings_df", "Ndim",
  "semanticrepresentation",
  "guide_legend", "Dim1",
  "textTrainRandomForest", "textTrainCVpredictions", "textTrainCVpredictionsRF",
  "textCentralityData", "textCentralityPlot", "textEmbed", "textHuggingFace",
  "textLayerAggregation", "textProjectionData", "textProjectionPlot", "textSimilarity",
  "penalty", "mixture",
  "id1", "y.y", "id",
  ".pred", ".", "analysis", "assessment", "df3_glmn_grid",
  "n", "n1", "n1_e", "dot2", "adjusted_p_values.x", "dot.x", "plot_n_word_extreme",
  "plot_n_word_frequency", "plot_n_words_middle", "n_all.x", "adjusted_p_values.y", "dot.y",
  "n_all.y", "colour_categories", "scale_color_identity", "group", "x", "y", "fill",
  "check_p_square", "check_p_x", "check_extreme_max_x", "check_extreme_min_x", "check_extreme_frequency_x",
  "check_middle_x", "check_p_y", "check_extreme_max_y", "check_extreme_min_y", "check_extreme_frequency_y",
  "check_middle_y", "extremes_all_x", "extremes_all_y", "BertTokenizer", "BertModel",
  "hgTransformerGetEmbedding", "central_cosine",
  "CTRLModel", "CTRLTokenizer", "DistilBertModel", "DistilBertTokenizer", "GPT2Model",
  "OpenAIGPTModel", "OpenAIGPTTokenizer", "RobertaModel", "RobertaTokenizer",
  "TransfoXLModel", "TransfoXLTokenizer", "XLMModel", "XLMRobertaModel",
  "XLMRobertaTokenizer", "XLMTokenizer", "XLNetModel", "XLNetTokenizer"
))