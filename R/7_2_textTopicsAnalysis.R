#' Wrapper for topicsTest function from the topics package
#' @param model (list) The trained model
#' @param x_variable (string) The x variable name to be predicted, and to be plotted (only needed for regression or correlation)
#' @param y_variable (string) The y variable name to be predicted, and to be plotted (only needed for regression or correlation)
# @param group_var (string) The variable to group by (only needed for t-test)
#' @param controls (vector) The control variables (not supported yet)
#' @param test_method (string) The test method to use, either "correlation","t-test", "linear_regression","logistic_regression", or "ridge_regression"
# @param p_alpha (numeric) Threshold of p value set by the user for visualising significant topics
#' @param p_adjust_method (character) Method to adjust/correct p-values for multiple comparisons
#' (default = "none"; see also "holm", "hochberg", "hommel", "bonferroni", "BH", "BY",  "fdr").
# @param load_dir (string) The directory to load the test from, if NULL, the test will not be loaded
# @param save_dir (string) The directory to save the test, if NULL, the test will not be saved
#' @param ... Parameter settings from topicsTest in the topics-package.
#' @return A list of the test results, test method, and prediction variable
#' @importFrom topics topicsTest
#' @export
textTopicsTest <- function(
    model,
    x_variable = NULL,
    y_variable = NULL,
    controls = c(),
    test_method = "default",
    p_adjust_method = "fdr",
    ...) {

  # Extract components from the model
  data1 <- model$train_data %>% tibble::as_tibble()
  preds <- model$preds %>% tibble::as_tibble()
  seed <- model$seed
  save_dir <- model$save_dir
  ngrams <- model$ngrams
  model_summary <- list()
  model_summary$summary <- model$model[[2]] %>% tibble::as_tibble()

  # Adding dummy for prevalence and coherence to fit the topics package
  model_summary$summary$prevalence <- NA
  model_summary$summary$coherence <- NA

  # Call topicsTest from the topics package
  results <- topics::topicsTest(
    data = data1,
    model = model_summary,
    preds = preds,
    ngrams = ngrams,
    x_variable = x_variable,
    y_variable = y_variable,
    controls = controls,
    test_method = test_method,
    p_adjust_method = p_adjust_method,
    seed = seed
 #   , ...
  )
  results
  return(results)
}

#' Plot word clouds
#'
#' This function create word clouds and topic fugures
#' @param model (list) A trained topics model. For examples from topicsModel(). Should be NULL if plotting ngrams.
#' @param ngrams (list) The output from the the topicsGram() function . Should be NULL if plotting topics.
#' @param test (list) The test results; if plotting according to dimension(s) include the object from topicsTest() function.
# @param p_threshold (integer) The p-value threshold to use for significance testing.
# @param color_scheme (string 'default' or vector) The color scheme.
#
# For plots not including a test, the color_scheme should in clude 2 colours (1 gradient pair), such as:
#
# c("lightgray", "darkblue)
#
# For 1 dimensional plots of n-grams it should contain 4 colours (2 gradient pairs), such as:
#
# c(
# "#EAEAEA", "darkred", # negative ngrams colors
#
# "#EAEAEA", "darkgreen" # positve ngrams colors)
#
#
#
# For 1-dimension plots of topics, it should contain 6 colours (3 gradient pairs), such as
#
#  c(
# "#EAEAEA", "darkred",     # negative topics colors
#
# "#EAEAEA", "darkgray",     # colours of topics not significantly associated
#
# "#EAEAEA", "darkgreen"     # positve topics colors)
#
#
#
#  For 2-dimensional plots of topics, the color scheme should contain 18 colours (9 gradient pairs), such as:
#
#  c(
#   "lightgray", "#398CF9",     # quadrant 1 (upper left corner)
#
#   "lightgray", "#60A1F7",     # quadrant 2
#
#   "lightgray", "#5dc688",     # quadrant 3 (upper right corner)
#
#   "lightgray", "#e07f6a",     # quadrant 4
#
#   "lightgray", "darkgray",     # quadrant 5 (middle square)
#
#   "lightgray", "#40DD52",     # quadrant 6
#
#   "lightgray", "#FF0000",     # quadrant 7 (bottom left corner)
#
#   "lightgray", "#EA7467",     # quadrant 8
#
#   "lightgray", "#85DB8E")     # quadrant 9 (bottom right corner)
#
#
# @param scale_size (logical) Whether to scale the size of the words.
# @param plot_topics_idx (vector)  The index or indeces of the topics to plot
# (e.g., look in the model-object for the indices; can for example, be c(1, 3:5) to plot topic t_1, t_3, t_4 and t_5) (optional).
# @param save_dir (string) The directory to save the plots.
# @param figure_format (string) Set the figure format, e.g., ".svg", or ".png".
# @param width (integer) The width of the topic (units = "in").
# @param height (integer) The width of the topic (units = "in").
# @param max_size (integer) The max size of the words.
#' @param seed (integer) The seed to set for reproducibility; need to be the same seed number as in in
# @param scatter_legend_dot_size (integer) The size of dots in the scatter legend.
# @param scatter_legend_bg_dot_size (integer) The size of background dots in the scatter legend.
# @param scatter_legend_n (numeric or vector) A vector determining the number of dots to emphasis in each quadrant of the scatter legend.
# For example: c(1,1,1,1,0,1,1,1,1) result in one dot in each quadrant except for the middle quadrant.
# @param scatter_legend_method (string) The method to filter topics to be emphasised in the scatter legend.
# Can be either "mean", "max_x", or "max_y"
# @param scatter_legend_specified_topics (vector) Specify which topic(s) to be emphasised in the scatter legend.
# For example c("t_1", "t_2"). If set, scatter_legend_method will have no effect.
# @param scatter_legend_topic_n (boolean) Allow showing the topic number or not in the scatter legend
# @param grid_legend_title The title of grid topic plot.
# @param grid_legend_title_size The size of the title of the plot.
# @param grid_legend_title_color The color of the legend title.
# @param grid_legend_x_axes_label The label of the x axes.
# @param grid_legend_y_axes_label The label of the y axes.
# @param grid_legend_number_color The color in the text in the legend.
# @param grid_legend_number_size The color in the text in the legend.
#' @param ... Parameters from the topicsPlot() function in the topics package.
#' @return The function saves figures in the save_dir.
#' @importFrom topics topicsPlot
#' @export
textTopicsWordcloud <- function(
    model = NULL,
    ngrams = NULL,
    test = NULL,
    seed = 2024,
    ...) {


  if(!is.null(test)){
    message(colourise("The textTopics function does not return topics-prevalence, so all topics are assigned 1 in the visualisation.", "blue"))
    test$test$prevalence <- rep(1, length(test$test$prevalence))
  }

  # Assign pseudo-phi values as descending importance scores (normalized from 1 to N)
   message(colourise("Note: BERTopic does not provide true 'phi' values (i.e., P(word | topic)) as in probabilistic models like LDA. Instead, we assign descending normalized values to approximate word importance.", "blue"))


  if(is.null(test)){
    plot_topics_idx = "textTopics"
  } else {
    plot_topics_idx = NULL
  }

  plots <- topics::topicsPlot(
    model = model,
    ngrams = NULL,
    test = test,
    seed = seed,
    plot_topics_idx = plot_topics_idx
    , ...
  )

  #completion_text <- paste0("The plots (p< p_alpha) are saved in ", save_dir, "/seed_", seed, "/wordclouds")
  #message(colourise(completion_text, "green"))

  return(plots)
}

