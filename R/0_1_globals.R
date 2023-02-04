
# Global settings (these avoids the note (warning) "no visible binding for global variable ‘XXX’
# https://community.rstudio.com/t/how-to-solve-no-visible-binding-for-global-variable-note/28887
utils::globalVariables(c(
  # GENERAL
  "text_version_nr",

  # EXPORTED FUNCTIONS
  "textEmbedLayerAggreation", "textProjection", "textProjectionPlot", "textSimilarity",
  "textTrainRandomForest", "textCentrality", "textCentralityPlot", "textEmbed", "textEmbedLayersOutput",

  # HELPER FUNCTIONS
  "addEqualNrNArows", "applysemrep",

  # PYTHON FUNCTIONS
  "hgTransformerGetEmbedding", "hgTransformerGetSentiment", "hgTransformerGetTextGeneration",
  "hgTransformerGetNER", "hgTransformerGetSummarization", "hgTransformerGetQA",
  "hgTransformerGetTranslation", "hgTokenizerGetTokens", "hgTransformerGetZeroShot",
  "textModelsPy", "textModelsRMPy", "get_number_of_hidden_layers",


  # EMBEDDINGS OBJECTS
  "layer_index", "token_index",

  # TRAINING OBJECTS
  "id_nr",
  "mod_spec", "wf", "xy", "xy1",
  "penalty", "mixture", "trees",
  "id1", "y.y", "id",
  ".pred", ".", "analysis", "assessment", "df3_glmn_grid",
  ".pred_class", "RMSE", "estimate", "inside_folds", "multisession", "outside_folds",
  "predictions", "truth", "value", ".pred_1", ".pred_2",
  "data_train",
  "textTrainCVpredictions", "textTrainCVpredictionsRF",

  # Language Tasks
  "NamesNer",
  "no", "scores", # in textZeropShot

  # PLOTTING OBJECTS
  "n", "n1", "n1_e", "dot2", "adjusted_p_values.x", "dot.x", "plot_n_word_extreme",
  "plot_n_word_frequency", "plot_n_words_middle", "n_all.x", "adjusted_p_values.y", "dot.y",
  "n_all.y", "colour_categories", "scale_color_identity", "group", "x", "y", "fill",
  "check_p_square", "check_p_x_neg", "check_p_x_pos", "check_extreme_max_x",
  "check_extreme_min_x", "check_extreme_frequency_x",
  "check_middle_x", "check_p_y_neg", "check_p_y_pos", "check_extreme_max_y",
  "check_extreme_min_y", "check_extreme_frequency_y",
  "check_middle_y", "extremes_all_x", "extremes_all_y",
  "central_cosine",
  "SS", "p_values.x", "p_values.y", "words", "n.x", "n.y",
  "word_types_embeddings_df", "Ndim",
  "semanticrepresentation", "single_word_embeddings2",
  "guide_legend", "Dim1",
  "Dim_PC1", "Dim_PC2", "check_extreme_frequency", "check_extreme_max_PC1",
  "check_extreme_max_PC2", "check_extreme_min_PC1", "check_extreme_min_PC2",
  "check_middle_PC1", "check_middle_PC2",
  "corr_y1", "corr_y2", "splits", "word_data1", "word_data1_middle_x",
  "word_data_extrem_max_PC1", "word_data_extrem_max_PC2",
  "word_data_extrem_min_PC1", "word_data_extrem_min_PC2", "word_data_frequency", "n_g1.x", "n_g2.x",
  "centrality", "projection", "superviced",
  "x_plotted", "y_plotted", "mixture_mode", "penalty_mode",
  "tokens"
))
