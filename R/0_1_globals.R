# Global settings (these avoids the note (warning) "no visible binding for global variable ‘XXX’
# https://community.rstudio.com/t/how-to-solve-no-visible-binding-for-global-variable-note/28887
utils::globalVariables(c(
  # GENERAL
  "text_version_nr", "find_textrpp",
  # EXPORTED FUNCTIONS
  "textEmbedLayerAggreation", "textProjection", "textProjectionPlot", "textSimilarity",
  "textTrainRandomForest", "textCentrality", "textCentralityPlot", "textEmbed", "textEmbedLayersOutput",

  # HELPER FUNCTIONS
  "addEqualNrNArows", "applysemrep",

  # PYTHON FUNCTIONS
  "hgTransformerGetEmbedding", "hgDLATKTransformerGetEmbedding", "hgTransformerGetSentiment", "hgTransformerGetTextGeneration",
  "hgTransformerGetNER", "hgTransformerGetSummarization", "hgTransformerGetQA",
  "hgTransformerGetTranslation", "hgTokenizerGetTokens", "hgTransformerGetZeroShot",
  "textModelsPy", "textModelsRMPy", "get_number_of_hidden_layers", "hgTransformerFineTune",
  "remove_pii", "values",


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
  "strata",
  "data_prepared_with_recipe",
  "bestParametersFunction", "eval_result",
  "fold", "fold_prop", "overall_n", "overall_prop",
  "weights_in_text",

  # textTrainNPlot
  "sample_size", "percent", "std", "ymin", "ymax",

  # textTrainExamples
  "category", "distance_to_mean", "error", "language", "topic",
  "height", "save_dir", "width", "y_axes_1",
  "check_random_x", "check_random_y", "color_categories", "ranking_criteria",
  "variable", "x_variable_grouped_three",


  # textPredcitTest
  "stats_on_bootstrap",

  # textPredict
  ":=", "predicted_class", "osf_download", "osf_retrieve_file",
  "Construct_Concept_Behaviours",

  # implicit motives
  "resid",

  # textTokenizeAndCount and textDomainComapre
  "n_assess", "n_train",

  # Similarity
  "dist",

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
  "tokens", "check_extreme_xy", "extremity_score", "square_categories",

  # Finetuning
  "hgTransformerMLM",

  ### BertTopic
  # create_plots:
  "Word", "phi", "scale_size_area", "create_bertopic_model", "name_cols_with_vocab",
  "topics_corr_grouping", "extract_topic_stats_corr",
  "adjust.p_value", "mean_group_1", "mean_group_2", "sort_stats_tibble", "label_1", "top_terms",
  "topics_t_test_grouping", "extract_topic_stats_cate", "topic_name", "p.value",
  "adjusted_p.value", "cohen_d", "prevalence", "coherence", "log_sample_size", "reduce_topics",
  "get_topic_tree",

  ## L-BAM table
  "Model_Type", "Name", "Path",

  ## textTopics
  "preds", "train_data"

))
