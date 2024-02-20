library(testthat)
library(tibble)
library(text)

context("Big analyses flow")

test_that("Testing textEmbed as well as train", {
  skip_on_cran()

  descr1 <- textDescriptives(Language_based_assessment_data_8[1])
  expect_that(descr1[[1]], is_a("character"))
  descr2 <- textDescriptives(Language_based_assessment_data_8[1:2])
  expect_equal(descr2[[2]][[1]], 2482)
  expect_equal(descr2[[3]][[1]], 62.05)

  harmony_word_embeddings <- text::textEmbed(Language_based_assessment_data_8[1:20, 1:2],
    model = "bert-base-uncased",
    dim_name = TRUE,
    layers = c(11:12),
    aggregation_from_layers_to_tokens = "concatenate",
    aggregation_from_tokens_to_texts = "mean",
    aggregation_from_tokens_to_word_types = "mean"
  )

#  textModelsRemove("bert-base-uncased")
  expect_equal(harmony_word_embeddings$texts$satisfactiontexts[[1]][1], 0.3403273, tolerance = 0.0001)
  expect_equal(harmony_word_embeddings$texts$satisfactiontexts[[1]][2], 0.1531016, tolerance = 0.00001)

  dim1for1 <- harmony_word_embeddings$word_types$harmonytexts[[3]][harmony_word_embeddings$word_types$harmonytexts$words == "you"]
  expect_equal(dim1for1, -0.2809616, tolerance = 0.00001)

  dim1for1 <- harmony_word_embeddings$word_types$satisfactiontexts[[3]][harmony_word_embeddings$word_types$harmonytexts$words == "you"]
  expect_equal(dim1for1, 0.1417716, tolerance = 0.00001)

  dim1for1 <- harmony_word_embeddings$word_types$harmonytexts[[3]][harmony_word_embeddings$word_types$harmonytexts$words == "-"]
  expect_equal(dim1for1, 0.5637481, tolerance = 0.00001)

  text_train_results1 <- textTrainRegression(
    x = harmony_word_embeddings$texts["satisfactiontexts"],
    y = Language_based_assessment_data_8["hilstotal"][1:20, ],
    cv_method = "cv_folds",
    outside_folds = 2,
    inside_folds = 2,
    outside_strata_y = NULL,
    inside_strata_y = NULL,
    # preprocess_PCA = c(0.20),
    preprocess_PCA = NA,
    penalty = 1e-16,
    multi_cores = "multi_cores_sys_default"
  )

  expect_that(text_train_results1$results$estimate[1], is_a("numeric"))
  expect_gt(text_train_results1$results$estimate[1], .3)
  expect_equal(text_train_results1$results$estimate[[1]], 0.3273128, tolerance = 0.00001)


  hils_predicted_scores1 <- text::textPredict(
    model_info = text_train_results1,
    word_embeddings = harmony_word_embeddings$texts["satisfactiontexts"],
    dim_names = TRUE
  )

  expect_that(hils_predicted_scores1[[1]], is_a("numeric"))
  expect_equal(hils_predicted_scores1[[1]][1], 11.89219, tolerance = 0.0001)
  expect_equal(hils_predicted_scores1[[1]][2], 25.90329, tolerance = 0.0001)


  # Train with x_variable
  train_x_append <- text::textTrainRegression(
    x = harmony_word_embeddings$texts["satisfactiontexts"],
    x_append = Language_based_assessment_data_8[1:20, 6:7],
    y = Language_based_assessment_data_8["hilstotal"][1:20, ],
    cv_method = "cv_folds",
    outside_folds = 2,
    inside_folds = 2,
    outside_strata = FALSE,
    inside_strata = FALSE,
    # preprocess_PCA = c(0.20),
    preprocess_PCA = NA,
    penalty = 1e-16,
    multi_cores = "multi_cores_sys_default"
  )

  # Predict
  hils_predicted_scores2 <- text::textPredict(
    model_info = train_x_append,
    x_append = Language_based_assessment_data_8[1:20, 6:7],
    append_first = TRUE,
    word_embeddings = harmony_word_embeddings$texts["satisfactiontexts"],
    dim_names = TRUE
  )

  expect_that(hils_predicted_scores2[[1]], is_a("numeric"))
  expect_equal(hils_predicted_scores2[[1]][1], 12.40038, tolerance = 0.1)
  expect_equal(hils_predicted_scores2[[1]][2], 25.89001, tolerance = 0.1)

  # Same as above with different input
  hils_predicted_scores1b <- textPredict(
    model_info = text_train_results1,
    word_embeddings = harmony_word_embeddings$texts,
    dim_names = TRUE
  )
  expect_that(hils_predicted_scores1b[[1]], is_a("numeric"))
  expect_equal(hils_predicted_scores1b[[1]][1], 11.89219, tolerance = 0.1)

  # Including x_append
  hils_predicted_scores1 <- textPredict(
    model_info = train_x_append,
    word_embeddings = harmony_word_embeddings$texts,
    # x_append = Language_based_assessment_data_8[1:20, ], # sending all give same as below: 12.40038
    # x_append = Language_based_assessment_data_8[1:20, 6:7], # sending only swlstotal and age: 12.40038
    x_append = Language_based_assessment_data_8[1:20, c(7, 6)], # sending "wrong" order give: 12.40038
    append_first = FALSE,
    # x_append = Language_based_assessment_data_8[1:20, c(5,6) ], # missing one throws error
    dim_names = TRUE
  )
  expect_equal(hils_predicted_scores1[[1]][1], 12.40038, tolerance = 0.01)


  # Predict ALL
  models_1_2 <- list(text_train_results1, train_x_append)
  all_predictions <- textPredictAll(
    models = models_1_2,
    word_embeddings = harmony_word_embeddings$texts,
    x_append = Language_based_assessment_data_8[1:20, 5:8],
    append_first = FALSE
  )

  expect_equal(all_predictions[[1]][1], 11.89, tolerance = 0.1)
  expect_equal(all_predictions[[2]][1], 12.40038, tolerance = 0.01)


  proj <- textProjection(
    words = Language_based_assessment_data_8[1],
    word_embeddings = harmony_word_embeddings$texts$satisfactiontexts,
    word_types_embeddings = harmony_word_embeddings$word_type$satisfactiontexts,
    x = Language_based_assessment_data_8$hilstotal,
    y = Language_based_assessment_data_8$swlstotal,
    pca = NULL,
    aggregation = "mean",
    split = "quartile",
    word_weight_power = 1,
    min_freq_words_test = 0,
    Npermutations = 10,
    n_per_split = 5,
    seed = 1003
  )

  expect_that(proj[[1]][[1]][[1]][[1]], is_a("numeric"))
  expect_equal(proj[[1]][[1]][[1]][[1]], 0.2018889, tolerance = 0.00001)

  # for decontexts = TRUE expect_equal(proj[[1]][[1]][[1]][[1]], -0.402433, tolerance = 0.0000001)

  plot_proj <- textProjectionPlot(
    word_data = proj,
    explore_words = c("happy"),
    y_axes = TRUE
  )
  plot_proj$processed_word_data$n[1]
  expect_that(plot_proj$processed_word_data$n[1], is_a("numeric"))
  # expect_equal(plot_proj$processed_word_data$n[1], 2)
  # expect_equal(plot_proj$processed_word_data$n[1], 1)

  one_character <- plot_proj$processed_word_data %>%
    dplyr::filter(words == "-")
  expect_equal(one_character$n, 1)
  #  pred_word <- textWordPrediction(words = Language_based_assessment_data_8[1],
  #                                  single_word_embeddings = harmony_word_embeddings$singlewords_we,
  #                                  x = Language_based_assessment_data_8$hilstotal)
  #
  #  plot_pred <- textPlot(pred_word,
  #                        explore_words = c("happy"),
  #                        y_axes = FALSE)
  #


  text_train_results <- textTrain(
    x = harmony_word_embeddings$texts$satisfactiontexts,
    y = Language_based_assessment_data_8$hilstotal[1:20],
    cv_method = "cv_folds",
    outside_folds = 2,
    inside_folds = 2,
    outside_strata_y = NULL,
    inside_strata_y = NULL,
    # preprocess_PCA = c(0.20),
    preprocess_PCA = NA,
    penalty = 1e-16,
    multi_cores = "multi_cores_sys_default"
  )

  expect_that(text_train_results$results$estimate[1], is_a("numeric"))
  expect_gt(text_train_results$results$estimate[1], 0.3)
  expect_equal(text_train_results$results$estimate[[1]], 0.3273128, tolerance = 0.0001)


  # Predict
  hils_predicted_scores1 <- textPredict(
    model_info = text_train_results,
    word_embeddings = harmony_word_embeddings$texts
  )

  expect_that(hils_predicted_scores1[[1]][1], is_a("numeric"))
  expect_equal(hils_predicted_scores1[[1]][1], 11.89219, tolerance = 0.000001)
})

test_that("Testing textEmbedReduce as well as train", {
  skip_on_cran()
  embedding_roberta <- textEmbed(Language_based_assessment_data_3_100[1, 1],
    model = "roberta-base",
    layer = 11,
    aggregation_from_tokens_to_word_types = "mean"
  )

#  textModelsRemove("roberta-base")

  pca5 <- text::textEmbedReduce(
    embeddings = embedding_roberta,
    n_dim = 5
  )

  testthat::expect_equal(pca5$texts$harmonywords[[1]], -9.569476, tolerance = 0.0001)
  testthat::expect_equal(pca5$word_types$harmonywords$Dim1[[1]], -0.0542851, tolerance = 0.0001)

  unlink("inst/extdata/rpca_roberta_768_D_20.csv")
  unlink("inst/extdata/scalar.csv")

})
