#' Wrapper for topicsTest function from the topics package
#' @param model (list) The trained model
#' @param pred_var_x (string) The x variable name to be predicted, and to be plotted (only needed for regression or correlation)
#' @param pred_var_y (string) The y variable name to be predicted, and to be plotted (only needed for regression or correlation)
#' @param group_var (string) The variable to group by (only needed for t-test)
#' @param control_vars (vector) The control variables (not supported yet)
#' @param test_method (string) The test method to use, either "correlation","t-test", "linear_regression","logistic_regression", or "ridge_regression"
#' @param p_alpha (numeric) Threshold of p value set by the user for visualising significant topics
#' @param p_adjust_method (character) Method to adjust/correct p-values for multiple comparisons
#' (default = "none"; see also "holm", "hochberg", "hommel", "bonferroni", "BH", "BY",  "fdr").
#' @param load_dir (string) The directory to load the test from, if NULL, the test will not be loaded
# @param save_dir (string) The directory to save the test, if NULL, the test will not be saved
#' @return A list of the test results, test method, and prediction variable
#' @importFrom topics topicsTest
#' @export
textTopicsTest <- function(
    model,
    pred_var_x = NULL,
    pred_var_y = NULL,
    group_var = NULL, # only one in the case of t-test
    control_vars = c(),
    test_method = "linear_regression",
    p_alpha = 0.05,
    p_adjust_method = "fdr",
    load_dir = NULL) {

  # Extract components from the model
  data <- model$train_data %>% tibble::as_tibble()
  preds <- model$preds %>% tibble::as_tibble()
  seed <- model$seed
  save_dir <- model$save_dir
  ngrams <- model$ngrams
  model_summary <- model$model

  # Call topicsTest from the topics package
  results <- topics::topicsTest(
    data = data,
    model = model_summary,
    preds = preds,
    ngrams = ngrams,
    pred_var_x = pred_var_x,
    pred_var_y = pred_var_y,
    group_var = group_var,
    control_vars = control_vars,
    test_method = test_method,
    p_alpha = p_alpha,
    p_adjust_method = p_adjust_method,
    seed = seed,
    # load_dir = NULL,
    save_dir = save_dir
  )

  return(results)
}

#' Plot word clouds
#'
#' This function create word clouds and topic fugures
#' @param model (list) A trained topics model. For examples from topicsModel(). Should be NULL if plotting ngrams.
#' @param ngrams (list) The output from the the topicsGram() function . Should be NULL if plotting topics.
#' @param test (list) The test results; if plotting according to dimension(s) include the object from topicsTest() function.
#' @param p_threshold (integer) The p-value threshold to use for significance testing.
#' @param color_scheme (string 'default' or vector) The color scheme.
#'
#' For plots not including a test, the color_scheme should in clude 2 colours (1 gradient pair), such as:
#'
#' c("lightgray", "darkblue)
#'
#' For 1 dimensional plots of n-grams it should contain 4 colours (2 gradient pairs), such as:
#'
#' c(
#' "#EAEAEA", "darkred", # negative ngrams colors
#'
#' "#EAEAEA", "darkgreen" # positve ngrams colors)
#'
#'
#'
#' For 1-dimension plots of topics, it should contain 6 colours (3 gradient pairs), such as
#'
#'  c(
#' "#EAEAEA", "darkred",     # negative topics colors
#'
#' "#EAEAEA", "darkgray",     # colours of topics not significantly associated
#'
#' "#EAEAEA", "darkgreen"     # positve topics colors)
#'
#'
#'
#'  For 2-dimensional plots of topics, the color scheme should contain 18 colours (9 gradient pairs), such as:
#'
#'  c(
#'   "lightgray", "#398CF9",     # quadrant 1 (upper left corner)
#'
#'   "lightgray", "#60A1F7",     # quadrant 2
#'
#'   "lightgray", "#5dc688",     # quadrant 3 (upper right corner)
#'
#'   "lightgray", "#e07f6a",     # quadrant 4
#'
#'   "lightgray", "darkgray",     # quadrant 5 (middle square)
#'
#'   "lightgray", "#40DD52",     # quadrant 6
#'
#'   "lightgray", "#FF0000",     # quadrant 7 (bottom left corner)
#'
#'   "lightgray", "#EA7467",     # quadrant 8
#'
#'   "lightgray", "#85DB8E")     # quadrant 9 (bottom right corner)
#'
#'
#' @param scale_size (logical) Whether to scale the size of the words.
#' @param plot_topics_idx (vector)  The index or indeces of the topics to plot
#' (e.g., look in the model-object for the indices; can for example, be c(1, 3:5) to plot topic t_1, t_3, t_4 and t_5) (optional).
#' @param save_dir (string) The directory to save the plots.
#' @param figure_format (string) Set the figure format, e.g., ".svg", or ".png".
#' @param width (integer) The width of the topic (units = "in").
#' @param height (integer) The width of the topic (units = "in").
#' @param max_size (integer) The max size of the words.
#' @param seed (integer) The seed to set for reproducibility; need to be the same seed number as in in
#' @param scatter_legend_dot_size (integer) The size of dots in the scatter legend.
#' @param scatter_legend_bg_dot_size (integer) The size of background dots in the scatter legend.
#' @param scatter_legend_n (numeric or vector) A vector determining the number of dots to emphasis in each quadrant of the scatter legend.
#' For example: c(1,1,1,1,0,1,1,1,1) result in one dot in each quadrant except for the middle quadrant.
#' @param scatter_legend_method (string) The method to filter topics to be emphasised in the scatter legend.
#' Can be either "mean", "max_x", or "max_y"
#' @param scatter_legend_specified_topics (vector) Specify which topic(s) to be emphasised in the scatter legend.
#' For example c("t_1", "t_2"). If set, scatter_legend_method will have no effect.
#' @param scatter_legend_topic_n (boolean) Allow showing the topic number or not in the scatter legend
#' @param grid_legend_title The title of grid topic plot.
#' @param grid_legend_title_size The size of the title of the plot.
#' @param grid_legend_title_color The color of the legend title.
#' @param grid_legend_x_axes_label The label of the x axes.
#' @param grid_legend_y_axes_label The label of the y axes.
#' @param grid_legend_number_color The color in the text in the legend.
#' @param grid_legend_number_size The color in the text in the legend.
#' @return The function saves figures in the save_dir.
#' @importFrom topics topicsPlot
#' @export
textTopicsWordcloud <- function(
    model = NULL,
    ngrams = NULL,
    test = NULL,
    p_threshold = 0.05, # Why is this set here since the test[[3]]$test$color_categories is determnied in in testTopics test?
    color_scheme = "default",
    scale_size = FALSE,
    plot_topics_idx = NULL,
    save_dir,
    figure_format = "svg",
    width = 10,
    height = 8,
    max_size = 10,
    seed = 42,
    scatter_legend_dot_size = 15,
    scatter_legend_bg_dot_size = 9,
    scatter_legend_n = c(1,1,1,1,0,1,1,1,1),
    scatter_legend_method = c("mean"),
    scatter_legend_specified_topics = NULL,
    scatter_legend_topic_n = FALSE,
    grid_legend_title = "legend_title",
    grid_legend_title_size = 5,
    grid_legend_title_color = 'black',
    grid_legend_x_axes_label = "legend_x_axes_label",
    grid_legend_y_axes_label = "legend_y_axes_label",
    grid_legend_number_color = 'black',
    grid_legend_number_size = 5) {


  topics::topicsPlot(
    model = model,
    ngrams = NULL,
    test = test,
    p_threshold = p_threshold,
    color_scheme = color_scheme,
    scale_size = scale_size,
    plot_topics_idx = plot_topics_idx,
    save_dir = save_dir,
    figure_format = figure_format,
    width = width,
    height = height,
    max_size = max_size,
    seed = seed,
    scatter_legend_dot_size = scatter_legend_dot_size,
    scatter_legend_bg_dot_size = scatter_legend_bg_dot_size,
    scatter_legend_n = scatter_legend_n,
    scatter_legend_method = scatter_legend_method,
    scatter_legend_specified_topics = scatter_legend_specified_topics,
    scatter_legend_topic_n = scatter_legend_topic_n,
    grid_legend_title = grid_legend_title,
    grid_legend_title_size = grid_legend_title_size,
    grid_legend_title_color = grid_legend_title_color,
    grid_legend_x_axes_label = grid_legend_x_axes_label,
    grid_legend_y_axes_label = grid_legend_y_axes_label,
    grid_legend_number_color = grid_legend_number_color,
    grid_legend_number_size = grid_legend_number_size
  )

  completion_text <- paste0("The plots (p<", p_threshold, ") are saved in ", save_dir, "/seed_", seed, "/wordclouds")
  message(colourise(completion_text, "green"))

}


