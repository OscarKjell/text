library(testthat)
library(tibble)
library(text)

context("Big analyses flow (with dlatk embedding method")

test_that("Testing DLATK textEmbed as well as train", {
  skip_on_cran()

#  textrpp_initialize(refresh_settings = T,
#                     save_profile = T)

  if (Sys.getenv("GITHUB_ACTIONS") == "true") {
    multi_cores = FALSE
  } else {
    multi_cores = "multi_cores_sys_default"
  }

  harmony_word_embeddings1 <- text::textEmbed(
    texts = Language_based_assessment_data_8[1:20, 1:2],
    model = "bert-base-uncased",
    dim_name = TRUE,
    layers = c(11:12),
    aggregation_from_layers_to_tokens = "concatenate",
    aggregation_from_tokens_to_texts = "mean",
    aggregation_from_tokens_to_word_types = "mean",
    implementation = "dlatk",
    batch_size = 5L
  )
  original_comment <- paste0(
    "Information about the embeddings. implementation: dlatk ; textEmbedRawLayers: model: bert-base-uncased ; layers: 11 12 ; word_type_embeddings: FALSE ; max_token_to_sentence:  ; text_version: ",
    packageVersion("text"),
    ". textEmbedLayerAggregation: layers =  11 12 aggregation_from_layers_to_tokens =   aggregation_from_tokens_to_texts =  mean tokens_select =   tokens_deselect =  ")
  new_comment <- comment(harmony_word_embeddings1$texts$satisfactiontexts)

  expect_equal(original_comment, new_comment)

#  textModelsRemove("bert-base-uncased")
  expect_equal(harmony_word_embeddings1$texts$satisfactiontexts[[1]][1], 0.2391714, tolerance = 0.0001)
  expect_equal(harmony_word_embeddings1$texts$satisfactiontexts[[1]][2], 0.02277972, tolerance = 0.00001)

#  dim1for1 <- harmony_word_embeddings1$word_types$harmonytexts[[3]][harmony_word_embeddings1$word_types$harmonytexts$words == "you"]
#  expect_equal(dim1for1, -0.2809616, tolerance = 0.00001)

#  dim1for1 <- harmony_word_embeddings1$word_types$satisfactiontexts[[3]][harmony_word_embeddings1$word_types$harmonytexts$words == "you"]
#  expect_equal(dim1for1, 0.1417716, tolerance = 0.00001)

#  dim1for1 <- harmony_word_embeddings1$word_types$harmonytexts[[3]][harmony_word_embeddings1$word_types$harmonytexts$words == "-"]
#  expect_equal(dim1for1, 0.5637481, tolerance = 0.00001)


  text_train_results_size <- text::textTrainRegression(
    x = harmony_word_embeddings1$texts["satisfactiontexts"],
    y = Language_based_assessment_data_8["hilstotal"][1:20,],
    penalty = 1e-16,
    multi_cores = multi_cores
  )

  # Function to examine the size of an object when saved.
  saveSize <- function(object) {
    tf <- tempfile(fileext = ".rds")
    on.exit(unlink(tf))
    saveRDS(object, tf)
    file.info(tf)$size
  }
  saved_size <- saveSize(text_train_results_size)
  saved_size
  testthat::expect_equal(saved_size[[1]], 2778186, tolerance = 1000)

  testthat::expect_equal(text_train_results_size$results$estimate[[1]], .4527607, tolerance = 0.001)
  testthat::expect_equal(text_train_results_size$results$p.value[[1]], 0.02250343, tolerance = 0.001)

  text_train_results1 <- text::textTrainRegression(
    x = harmony_word_embeddings1$texts["satisfactiontexts"],
    y = Language_based_assessment_data_8["hilstotal"][1:20, ],
    cv_method = "cv_folds",
    outside_folds = 2,
    inside_folds = 2,
    outside_strata_y = NULL,
    inside_strata_y = NULL,
    # preprocess_PCA = c(0.20),
    preprocess_PCA = NA,
    penalty = 1e-16,
    multi_cores = multi_cores
  )

  expect_that(text_train_results1$results$estimate[1], is_a("numeric"))
  expect_gt(text_train_results1$results$estimate[1], .3)
  expect_equal(text_train_results1$results$estimate[[1]], .4079596, tolerance = 0.00001)


  hils_predicted_scores1 <- text::textPredict(
    model_info = text_train_results1,
    word_embeddings = harmony_word_embeddings1$texts["satisfactiontexts"],
    dim_names = TRUE
  )

  expect_that(hils_predicted_scores1[[1]], is_a("numeric"))
  expect_equal(hils_predicted_scores1[[1]][1], 11.88353, tolerance = 0.0001)
  expect_equal(hils_predicted_scores1[[1]][2], 26.02205, tolerance = 0.0001)

#### testing testing
  # Train with x_variable
  train_x_append <- text::textTrainRegression(
    x = harmony_word_embeddings1$texts["satisfactiontexts"],
    x_append = Language_based_assessment_data_8[1:20, 6:7],
    y = Language_based_assessment_data_8["hilstotal"][1:20, ],
    language_distribution = Language_based_assessment_data_8["satisfactiontexts"],
    save_aggregated_word_embedding = TRUE,
    cv_method = "cv_folds",
    outside_folds = 2,
    inside_folds = 2,
    outside_strata = FALSE,
    inside_strata = FALSE,
    # preprocess_PCA = c(0.20),
    preprocess_PCA = NA,
    penalty = 1e-16,
    multi_cores = multi_cores
  )
  expect_that(train_x_append$language_distribution, is_a("tbl_df"))

  # Predict
  hils_predicted_scores2 <- text::textPredict(
    model_info = train_x_append,
    x_append = Language_based_assessment_data_8[1:20, 6:7],
    word_embeddings = harmony_word_embeddings1$texts["satisfactiontexts"],
    language_distribution = Language_based_assessment_data_8[1:20, 1],
    dim_names = TRUE,
    append_first = FALSE
  )

  expect_that(hils_predicted_scores2, is_a("list"))
  expect_equal(hils_predicted_scores2[[1]]$ss_min[[1]], 0.9459751, tolerance = 0.1)
  expect_equal(hils_predicted_scores2[[5]]$test_recall_percentage, 1, tolerance = 0.1)
  expect_equal(hils_predicted_scores2[[5]]$cosine_similarity, 0.986325, tolerance = 0.1)
  expect_equal(hils_predicted_scores2[[5]]$cosine_similarity_standardised, 0.9846371, tolerance = 0.1)

  expect_equal(hils_predicted_scores2[[6]]$satisfactiontexts_swlstotal_age_hilstotalpred[[1]], 12.40038, tolerance = 0.1)
  expect_equal(hils_predicted_scores2[[6]]$satisfactiontexts_swlstotal_age_hilstotalpred[[2]], 25.89001, tolerance = 0.1)


  # Trying with text
  # Predict help(textPredict)
  hils_predicted_scores3 <- text::textPredict(
    model_info = train_x_append,
    x_append = Language_based_assessment_data_8[1:20, 6:7],
    texts = Language_based_assessment_data_8[1:20,"satisfactiontexts"],
    dim_names = TRUE,
    append_first = FALSE
  )
  hist(hils_predicted_scores3$predictions$satisfactiontexts_swlstotal_age_hilstotalpred)
  expect_that(hils_predicted_scores3, is_a("list"))
  expect_equal(hils_predicted_scores3$predictions[[1]][1], 12.19023, tolerance = 0.1)
  expect_equal(hils_predicted_scores3$similarity_scores$overlap_percentage[[1]], 0.5915493, tolerance = 0.1)


  # Same as above with different input
  hils_predicted_scores1b <- textPredict(
    model_info = text_train_results1,
    word_embeddings = harmony_word_embeddings1$texts,
    dim_names = TRUE
  )
  expect_that(hils_predicted_scores1b[[1]], is_a("numeric"))
  expect_equal(hils_predicted_scores1b[[1]][1], 11.89219, tolerance = 0.1)

  # Including x_append
  hils_predicted_scores1 <- textPredict(
    model_info = train_x_append,
    word_embeddings = harmony_word_embeddings1$texts,
    # x_append = Language_based_assessment_data_8[1:20, ], # sending all give same as below: 12.40038
    # x_append = Language_based_assessment_data_8[1:20, 6:7], # sending only swlstotal and age: 12.40038
    x_append = Language_based_assessment_data_8[1:20, c(7, 6)], # sending "wrong" order give: 12.40038
    append_first = FALSE,
    # x_append = Language_based_assessment_data_8[1:20, c(5,6) ], # missing one throws error
    dim_names = TRUE
  )
  expect_equal(hils_predicted_scores1[[1]][1], 12.19023, tolerance = 0.01)


  # Predict ALL
  models_1_2 <- list(text_train_results1, train_x_append)
  all_predictions <- textPredictAll(
    models = models_1_2,
    word_embeddings = harmony_word_embeddings1$texts,
    x_append = Language_based_assessment_data_8[1:20, 5:8],
    append_first = FALSE
  )

  expect_equal(all_predictions[[1]][1], 11.88353, tolerance = 0.1)
  expect_equal(all_predictions[[2]][1], 12.19023, tolerance = 0.01)


#  proj <- textProjection(
#    words = Language_based_assessment_data_8[1],
#    word_embeddings = harmony_word_embeddings1$texts$satisfactiontexts,
#    word_types_embeddings = harmony_word_embeddings1$word_type$satisfactiontexts,
#    x = Language_based_assessment_data_8$hilstotal,
#    y = Language_based_assessment_data_8$swlstotal,
#    pca = NULL,
#    aggregation = "mean",
#    split = "quartile",
#    word_weight_power = 1,
#    min_freq_words_test = 0,
#    Npermutations = 10,
#    n_per_split = 5,
#    seed = 1003
#  )
#
#  expect_that(proj[[1]][[1]][[1]][[1]], is_a("numeric"))
#  expect_equal(proj[[1]][[1]][[1]][[1]], 0.2018889, tolerance = 0.00001)

  # for decontexts = TRUE expect_equal(proj[[1]][[1]][[1]][[1]], -0.402433, tolerance = 0.0000001)

#  plot_proj <- text::textProjectionPlot(
#    word_data = proj,
#    explore_words = c("happy"),
#    y_axes = TRUE
#  )
#  plot_proj$processed_word_data$n[1]
#  expect_that(plot_proj$processed_word_data$n[1], is_a("numeric"))
#  # expect_equal(plot_proj$processed_word_data$n[1], 2)
#  # expect_equal(plot_proj$processed_word_data$n[1], 1)
#
#  one_character <- plot_proj$processed_word_data %>%
#    dplyr::filter(words == "-")
#  expect_equal(one_character$n, 1)
  #  pred_word <- textWordPrediction(words = Language_based_assessment_data_8[1],
  #                                  single_word_embeddings = harmony_word_embeddings1$singlewords_we,
  #                                  x = Language_based_assessment_data_8$hilstotal)
  #
  #  plot_pred <- textPlot(pred_word,
  #                        explore_words = c("happy"),
  #                        y_axes = FALSE)
  #

  text_train_results <- text::textTrain(
    x = harmony_word_embeddings1$texts$satisfactiontexts,
    y = Language_based_assessment_data_8$hilstotal[1:20],
    cv_method = "cv_folds",
    outside_folds = 2,
    inside_folds = 2,
    outside_strata_y = NULL,
    inside_strata_y = NULL,
    # preprocess_PCA = c(0.20),
    preprocess_PCA = NA,
    penalty = 1e-16,
    multi_cores = multi_cores
  )

  expect_that(text_train_results$results$estimate[1], is_a("numeric"))
  expect_gt(text_train_results$results$estimate[1], 0.3)
  expect_equal(text_train_results$results$estimate[[1]], 0.4079596, tolerance = 0.0001)


  # Predict
  hils_predicted_scores1 <- textPredict(
    model_info = text_train_results,
    word_embeddings = harmony_word_embeddings1$texts
  )

  expect_that(hils_predicted_scores1[[1]][1], is_a("numeric"))
  expect_equal(hils_predicted_scores1[[1]][1], 11.88353, tolerance = 0.000001)

  rm(harmony_word_embeddings1)
  })

##test_that("Testing textEmbedReduce as well as train", {
##  skip_on_cran()
##  embedding_roberta <- textEmbed(
##    Language_based_assessment_data_3_100[1, 1],
##    model = "roberta-base",
##    layer = 11,
##    aggregation_from_tokens_to_word_types = "mean"
##  )
##
###  textModelsRemove("roberta-base")
##
##  pca5 <- text::textEmbedReduce(
##    embeddings = embedding_roberta,
##    n_dim = 5
##  )
##
##  testthat::expect_equal(pca5$texts$harmonywords[[1]], -9.569476, tolerance = 0.0001)
##  testthat::expect_equal(pca5$word_types$harmonywords$Dim1[[1]], -0.0542851, tolerance = 0.0001)
##
##  unlink("./inst/extdata/rpca_roberta_768_D_20.csv")
##  unlink("./inst/extdata/scalar.csv")
##
##  # Trying to add this to see whether the two above lines are executed properly.
##  x = 5
##
##})
