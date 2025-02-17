

library(testthat)
library(tibble)
library(text)
library(topics)
context("Testing tasks")


test_that("textClassify tests", {
  skip_on_cran()

  # Prepare the dataset
  # Combine depression and worry language
  Language <- c(
    dep_wor_data$Depphrase,
    dep_wor_data$Worphrase)

  # Create indicator for depression language (1) and worry language (2)
  DepWor <- c(
    as.numeric(rep(1, length(dep_wor_data$Depphrase))),
    as.numeric(rep(2, length(dep_wor_data$Worphrase))))

  # Add corresponding depression scores, age, and gender
  PHQ <- as.numeric(rep(dep_wor_data$PHQ9tot, 2))
  GAD <- as.numeric(rep(dep_wor_data$GAD7tot, 2))
  Age <- as.numeric(rep(dep_wor_data$Age, 2))
  Gender <- as.numeric(rep(dep_wor_data$Gender, 2))

  # Combine language, indicators, rating scales, age, and gender
  dep_wor_language <- tibble(
    Language, DepWor, PHQ, GAD, Age, Gender)

  # Transform language into word embeddings
  # Apply pre-trained word embeddings to transform the depression language into word embeddings
  # Extract text embeddings and word type embeddings
  embeddings <- textEmbed(
    texts = dep_wor_language["Language"],
    aggregation_from_tokens_to_texts = "mean",      # text embeddings
    aggregation_from_tokens_to_word_types = "mean", # word type embeddings
    keep_token_embeddings = FALSE)                  # token embeddings
  # Code box 5. - Highest predictive responses of the phrases

  # Train word embeddings to assess depression
  # Examine the relationship between the embeddings and the depression scores
  dep_model <- textTrain(
    x = embeddings$texts,      # text embeddings as predictor
    y = dep_wor_language$PHQ)  # depression scores as target


  # 1 get ansewrs with specific words
  # 2 show highest loewst absolut_error error_call


  #text = dep_wor_language$Language
  #target = dep_model$predictions$y
  #selection = "min_max"
  #selection_method = "predictions"
  #predictions = dep_model$predictions$predictions
  #n_examples = 5
  #include_words = NULL

  examples <- text::textTrainExamples(
    text = dep_wor_language$Language,
    target = dep_model$predictions$y,
    predictions = dep_model$predictions$predictions,
    n_examples = 5,
    selection_method = "min_max", # "min", "max", "min_max", "min_mean_max", "quintiles"
    selection_variable = "predictions", # "predictions", "error", or "targets"
    filter_words = NULL,
    target_color = "darkgreen",
    predictions_color = "darkblue",
    error_color = "darkred",
    distribution_color = c("darkgreen", "gray",  "darkred")
  )

  examples$error_plot
  examples$histogram_plot
  examples$scatter_plot
  testthat::expect_equal(examples$examples$id[1:5], c(303, 819, 11, 674, 89), tolerance = 1)
  #

  examples <- textTrainExamples(
    text = dep_wor_language$Language,
    target = dep_model$predictions$y,
    predictions = dep_model$predictions$predictions,
    n_examples = 3,
    selection_method = "min_mean_max", # "min", "max", "min_max", "min_mean_max", "quintiles"
    selection_variable = "predictions", # "predictions", "error", or "targets"
    include_words = NULL,
    target_color = "darkgreen",
    predictions_color = "darkblue",
    error_color = "darkred",
    distribution_color = c("darkred", "darkgreen", "gray"),
    figure_format = "svg",
    scatter_legend_dot_size = 6,
    scatter_legend_bg_dot_size = 3,
    scatter_legend_n = c(3,3,3),
    scatter_show_axis_values = TRUE,
    grid_legend_x_axes_label = "x",
    grid_legend_y_axes_label = "y",
    seed = 42
  )


})



