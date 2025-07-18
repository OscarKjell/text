library(text)
library(tibble)
library(testthat)

context("Testing PM tutorial")

test_that("PM tutorial", {
  skip_on_cran()

  if (Sys.getenv("GITHUB_ACTIONS") == "true") {
    multi_cores = FALSE
  } else {
    multi_cores = "multi_cores_sys_default"
  }

  # Example text
  texts <- c("I am feeling relatedness with others", "That's great!")

  # Defaults
  embeddings <- textEmbed(texts)

  # Output
  expect_equal(embeddings$tokens$texts[[1]][1][[1]][2], "i")
  expect_equal(embeddings$tokens$texts[[1]][2][[1]][2], 0.46768242, tolerance = 0.00001)

  # # # # # # # # # # # # # # # # # # # # # # # # # # # #

  # Transform the text/word data to word embeddings (see help(textEmbed) to see the default settings).
  word_embeddings <- text::textEmbed(
    Language_based_assessment_data_8,
    model = "bert-base-uncased",
    aggregation_from_layers_to_tokens = "concatenate",
    aggregation_from_tokens_to_texts = "mean",
    keep_token_embeddings = FALSE)

  # See how the word embeddings are structured
  #word_embeddings

  # Save the word embeddings to avoid having to embed the text again. It is good practice to save output from analyses that take a lot of time to compute, which is often the case when analyzing text data.
  # saveRDS(word_embeddings, "word_embeddings.rds")

  # Get the saved word embeddings (again)
### word_embeddings <- readRDS("/Users/oscarkjell/Desktop/1 Projects/0 Research/0 text r-package/1 article/text_tutorial/word_embeddings.rds")

  # # # # # # # # # # # # # # # # # # # # # # # # # # # #

  # Get hidden states for "I am fine"
  imf_embeddings_11_12 <- textEmbedRawLayers("I am fine",
                                             layers = 11:12)
  imf_embeddings_11_12

  #OUTPUT
  expect_equal(imf_embeddings_11_12$context_tokens$texts[[1]][[1]][1][[1]], "[CLS]")
  expect_equal(imf_embeddings_11_12$context_tokens$texts[[1]][[4]][1], 0.2663715, tolerance = 0.00001)


  # # # # # # # # # # # # # # # # # # # # # # # # # # # #


  # 1. Concatenate layers(results in 1,536 dimensions).
  agg1 <- textEmbedLayerAggregation(imf_embeddings_11_12$context_tokens,
                            layers = 11:12,
                            aggregation_from_layers_to_tokens = "concatenate",
                            aggregation_from_tokens_to_texts = "mean")
  # OUTPUT
  expect_equal(agg1$texts[[1]], 0.3110354, tolerance = 0.00001)

  # # # # # # # # # # # # # # # # # # # # # # # # # # # #

  # 2. Aggregate layers using mean (results in 768).
  agg2 <- textEmbedLayerAggregation(imf_embeddings_11_12$context_tokens,
                            layers = 11,
                            aggregation_from_tokens_to_texts = "mean")

  # OUTPUT
  expect_equal(agg2$texts[[1]], 0.3110354, tolerance = 0.00001)

  # # # # # # # # # # # # # # # # # # # # # # # # # # # #


  # Examine the relationship between satisfactiontext and the corresponding rating scale
  model_satisfactiontext_swls <- text::textTrain(
    x = word_embeddings$texts$satisfactiontexts, # the predictor variables (i.e., the word embeddings)
    y = Language_based_assessment_data_8$swlstotal, # the criterion variable (i.e.,the rating scale score.
    multi_cores = multi_cores,
    model_description = "author(s): Kjell, Giorgi, & Schwartz; data: N=40, population =  Online, Mechanical Turk; publication: title = Example for demo; description: swls = the satisfaction with life scale")

  # Examine the correlation between predicted and observed Harmony in life scale scores
  #model_satisfactiontext_swls$results

  #original
  #expect_equal(model_satisfactiontext_swls$results[[4]][[1]], 0.5385082, tolerance = 0.00001)
  expect_equal(model_satisfactiontext_swls$results[[4]][[1]], 0.5845689, tolerance = 0.00001)

  # OUTPUT:

  # # # # # # # # # # # # # # # # # # # # # # # # # # # #

  # Save the mode
  #saveRDS(model_satisfactiontext_swls, "model_satisfactiontext_swls.rds")
  # Read the model
  #model_satisfactiontext_swls <- readRDS("model_satisfactiontext_swls.rds")

  # Examine the names in the object returned from training
  #names(model_satisfactiontext_swls)

  #OUTPUT:

  # # # # # # # # # # # # # # # # # # # # # # # # # # # #


#  # Predicting several outcomes from several word embeddings
#  models_words_ratings <- textTrainLists(word_embeddings$texts[1:2],
#                                         Language_based_assessment_data_8[5:6])
#
#  # See results
#  models_words_ratings$results

  # OUTPUT


  # Save model
  ## saveRDS(models_words_ratings, "models_words_ratings.rds")
  # Read model
  ## models_words_ratings <- readRDS("models_words_ratings.rds")


  # # # # # # # # # # # # # # # # # # # # # # # # # # # #

  # Read a valence trained prediction model (download it from https://osf.io/dgczt/)
  ## valence_Warriner_L11 <- readRDS("valence_Warriner_L11.rds")

  # Examine the model
  # valence_Warriner_L11

  # PART OF THE OUTPUT

  # # # # # # # # # # # # # # # # # # # # # # # # # # # #

  # Apply the model to the satisfaction text
#   satisfaction_text_valence <- textPredict(valence_Warriner_L11,
#                                           word_embeddings$texts$satisfactiontexts,
#                                           dim_names = FALSE)
#
#  # Examine the correlation between the predicted valence and the Satisfaction with life scale score
#  psych::corr.test(satisfaction_text_valence$word_embeddings__ypred, Language_based_assessment_data_8$swlstotal)


  # OUTPUT

  # # # # # # # # # # # # # # # # # # # # # # # # # # # #



  # Compute semantic similarity scores between two text columns, using the previously created word_embeddings.
  semantic_similarity_scores <- textSimilarity(word_embeddings$texts$harmonytexts,
                                               word_embeddings$texts$satisfactiontexts)
  # Look at the first scores
  #head(semantic_similarity_scores)
  expect_equal(semantic_similarity_scores[[1]], 0.9281080, tolerance = 0.00001)

  # OUTPUT
  # # # # # # # # # # # # # # # # # # # # # # # # # # # #


  # Read word norms text (later we will use these for the semantic centrality plot)
  # word_norms <- read.csv("Word_Norms_Mental_Health_Kjell2018_text.csv")

  # Read the word embeddings for the word norms
  # word_norms_embeddings <- readRDS("Word_Norms_Mental_Health_Kjell2018_text_embedding_L11.rds")

  # Examine which word norms there are.
  # names(word_norms_embeddings$texts)

  # OUTPUT
  # # # # # # # # # # # # # # # # # # # # # # # # # # # #


  # Compute semantic similarity score between the harmony answers and the harmony norm
  # Note that the descriptive word answers are used instead of text answers to correspond with how the word norm was created.
  # norm_similarity_scores_harmony <- textSimilarityNorm(word_embeddings$texts$harmonywords,
  #                                                     word_norms_embeddings$texts$harmonynorm)

  # Correlating the semantic measure with the corresponding rating scale
  # psych::corr.test(norm_similarity_scores_harmony, Language_based_assessment_data_8$hilstotal)

  # OUTPUT
  # # # # # # # # # # # # # # # # # # # # # # # # # # # #


  # Extract word type embeddings and text embeddings for harmony words
  harmony_words_embeddings <- text::textEmbed(
    texts = Language_based_assessment_data_8["harmonywords"],
    aggregation_from_layers_to_tokens = "concatenate",
    aggregation_from_tokens_to_texts = "mean",
    aggregation_from_tokens_to_word_types = "mean",
    keep_token_embeddings = FALSE)


  # Pre-processing data for plotting
  projection_results <- text::textProjection(words = Language_based_assessment_data_8$harmonywords,
                                       word_embeddings = harmony_words_embeddings$texts,
                                       word_types_embeddings = harmony_words_embeddings$word_types,
                                       x = Language_based_assessment_data_8$hilstotal,
                                       y = Language_based_assessment_data_8$age)

  projection_results$word_data
  expect_equal(projection_results$word_data[[1]][[1]], "Group1*")
  expect_equal(projection_results$word_data$dot.x[[1]], -15.11895, tolerance = 0.00001)
  # To avoid warnings -- and that words do not get plotted, first increase the max.overlaps for the entire session:
  #options(ggrepel.max.overlaps = 1000)

  # Plot
#  plot_projection <- textPlot(projection_results,
#                              min_freq_words_plot = 1,
#                              plot_n_word_extreme = 10,
#                              plot_n_word_frequency = 5,
#                              plot_n_words_middle = 5,
#                              y_axes = FALSE,
#                              p_alpha = 0.05,
#                              p_adjust_method = "fdr",
#                              title_top = "Harmony Words Responses (Supervised Dimension Projection)",
#                              x_axes_label = "Low to High Harmony in Life Scale Score",
#                              y_axes_label = "",
#                              bivariate_color_codes = c("#FFFFFF", "#FFFFFF", "#FFFFFF",
#                                                        "#E07f6a", "#EAEAEA", "#85DB8E",
#                                                        "#FFFFFF", "#FFFFFF", "#FFFFFF"
#                              )
#  )
#  # View plot
#
#  plot_projection$final_plot
#

  # # # # # # # # # # # # # # # # # # # # # # # # # # # #

  # Plot
#  plot_projection_2D <- textPlot(projection_results,
#                                 min_freq_words_plot = 1,
#                                 plot_n_word_extreme = 10,
#                                 plot_n_word_frequency = 5,
#                                 plot_n_words_middle = 5,
#                                 y_axes = TRUE, # Change to TRUE
#                                 p_alpha = 0.05,
#                                 p_adjust_method = "fdr",
#                                 title_top = "Harmony Words Responses (Supervised Dimension Projection)",
#                                 x_axes_label = "Low vs. High Harmony in Life Scale Score",
#                                 y_axes_label = "Low vs.High Age", # Added
#                                 bivariate_color_codes = c("#E07f6b", "#60A1F7", "#85DB8D",
#                                                           "#FF0000", "#EAEAEA", "#5dc688",
#                                                           "#E07f6a", "#60A1F7", "#85DB8E"
#                                 )
#  )
#  # View plot
#  plot_projection_2D$final_plot

  # # # # # # # # # # # # # # # # # # # # # # # # # # # #


#  # Computing words' centrality (semantic similarity) score to the aggregated embedding of all words
#  centrality_results <- textCentrality(words = word_norms$satisfactionnorm,
#                                       word_embeddings = word_norms_embeddings$texts$satisfactionnorm,
#                                       word_types_embeddings = word_norms_embeddings$word_types)
# # options(ggrepel.max.overlaps = 1000)
#  centrality_plot <- textCentralityPlot(word_data = centrality_results,
#                                        min_freq_words_test = 2,
#                                        plot_n_word_extreme = 10,
#                                        plot_n_word_frequency = 5,
#                                        plot_n_words_middle = 5,
#                                        title_top = "Satisfaction with life word norm: Semantic Centrality Plot",
#                                        x_axes_label = "Satisfaction with Life Semantic Centrality")
#
#  centrality_plot$final_plot
#
  # OUTPUT

  # # # # # # # # # # # # # # # # # # # # # # # # # # # #


  # Supplementary

  # PCA results to be plotted help(textPCA)
#  textPCA_results <- textPCA(words = Language_based_assessment_data_8$satisfactionwords,
#                             word_types_embeddings = harmony_words_embeddings$word_types)
#
#
#  # Plotting the PCA results
#  plot_PCA <- textPCAPlot(
#    word_data = textPCA_results,
#    min_freq_words_test = 2,
#    plot_n_word_extreme = 5,
#    plot_n_word_frequency = 5,
#    plot_n_words_middle = 5
#  )
#  plot_PCA$final_plot


})

