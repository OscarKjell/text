
# Examine if parsnip 1.3 can be fixed <-  or if it is that models need to be retrained.

#' Add random noise to variables
#'
#' This function is needed because adding jitter des not work to the ggplot objective
#' from the topicsScatterLegend() function.
#' @param data (dataframe) Dataframe including the varaibles that should get jitter.
#' @param x_col (string) name of the column that should get jitter.
#' @param y_col (string) name of the column that should get jitter.
#' @param jitter_factor (numeric) Percentage of jitter to be added.
#' @param seed (numeric)
#' @noRd
dynamic_jitter_data <- function(
    data,
    x_col,
    y_col = NULL,
    jitter_factor = 0.05,
    seed = 2025) {

  # Check if x_col exists in the data
  if (!x_col %in% colnames(data)) {
    stop(paste("Column", x_col, "not found in the data."))
  }

  # Calculate jitter for x
  jitter_width <- diff(range(data[[x_col]], na.rm = TRUE)) * jitter_factor
  data <- data %>%
    mutate(
      x_jittered = .data[[x_col]] + runif(n(), -jitter_width, jitter_width)
    )

  # If y_col is specified, calculate jitter for y
  if (!is.null(y_col)) {
    if (!y_col %in% colnames(data)) {
      stop(paste("Column", y_col, "not found in the data."))
    }

    jitter_height <- diff(range(data[[y_col]], na.rm = TRUE)) * jitter_factor
    data <- data %>%
      mutate(
        y_jittered = .data[[y_col]] + runif(n(), -jitter_height, jitter_height)
      )
  }

  return(data)
}


#' Reorder columns in a given dataframe
#'
#' Target column and desired position
#' from the topicsScatterLegend() function.
#' @param df (dataframe) Dataframe including the varibles that should get jitter.
#' @param target_col (string) name of the column that should have specified position.
#' @param desired_position (numeric) Position of the specified column.
#' @noRd
reorder_columns <- function(
    df,
    target_col,
    desired_position
){
  # Get all column names
  all_cols <- colnames(df)

  # Remove the target column
  all_cols <- all_cols[all_cols != target_col]

  # Insert the target column at the desired position
  new_order <- append(all_cols, target_col, after = desired_position - 1)

  # Reorder the dataframe
  df <- df %>%
    dplyr::select(all_of(new_order))

  return(df)
}


#' Show language examples (Experimental)
#'
#' This function selects language examples that been used in the textTrain() or textAssess() functions.
#' @param text (string) the language that was used for prediction/assessment/classification.
#' @param x_variable (numeric) the variable used for training (y).
#' @param y_variable (numeric) the outcome from the model (i.e., y_hat).
#' @param type (string) if set to "prediction_errors", two extra plots is provided: distribution of scores and absolute error.
#' @param n_tile (integer) the n tile to split the data in (to show the most extreme tiles in different colours).
#' @param n_examples (integer) the number of language examples to show.
#' @param jitter (integer) the percentage of jitter to add to the data for the scatter plot.
# @param x_variable (string) selection method: "predictions", "error", or "targets".
# @param selection_method (string) specification of which examples to select "min", "max", "min_max", "min_mean_max", "quintiles".
#' @param filter_words (character vector) words required to be included in examples.
#' @param target_color (string)
#' @param predictions_color  (string) = "darkblue",
#' @param error_color =  (string) "darkred",
#' @param distribution_color  (string) colors of the distribution plot
#' @param figure_format  (string) file format of the figures.
#' @param scatter_legend_dot_size (integer) The size of dots in the scatter legend.
#' @param scatter_legend_bg_dot_size (integer) The size of background dots in the scatter legend.
# @param scatter_legend_n (numeric or vector) A vector determining the number of dots to emphasize in each quadrant of the scatter legend.
#' For example: c(1,0,1) result in one dot in each quadrant except for the middle quadrant.
# @param scatter_legend_method (string) The method to filter topics to be emphasized in the scatter legend; either "mean", "max_x", or "max_y".
# @param scatter_legend_specified_topics (vector) Specify which topic(s) to emphasize in the scatter legend.
# For example, c("t_1", "t_2"). If set, scatter_legend_method will have no effect.
# @param scatter_legend_topic_n (boolean) If TRUE, the topic numbers are shown in the scatter legend.
#' @param scatter_show_axis_values (boolean) If TRUE, the estimate values are shown on the distribution plot axes.
#' @param x_axis_range (numeric vector) range of x axis (e.g., c(1, 100)).
#' @param y_axis_range (numeric vector) range of y axis (e.g., c(1, 100)).
#' @param grid_legend_x_axes_label x-axis label of the grid topic plot.
#' @param grid_legend_y_axes_label y-axis label of the grid topic plot.
#' @param seed (integer) The seed to set for reproducibility.
#' @returns A tibble including examples with descriptive variables.
#' @importFrom dplyr filter select arrange slice group_by summarize mutate rename
#' @importFrom stringi stri_detect_fixed
#' @importFrom purrr map_lgl
#' @export
textTrainExamples <- function(
    text,
    x_variable,
    y_variable = NULL,
    type = "default",
    n_tile = 4,
    n_examples = 5,
    jitter = NULL,
    filter_words = NULL,
    target_color = "darkgreen",
    predictions_color = "darkblue",
    error_color = "darkred",
    distribution_color = c(
      "#00508c",  # quadrant 1 (upper left corner)
      "#805259",  # quadrant 2
      "#a71200",  # quadrant 3 (upper right corner)
      "#0a6882",  # quadrant 4
      "#a4a4a4",  # quadrant 5 (middle square)
      "#e04b39",  # quadrant 6
      "#19956e",  # quadrant 7 (bottom left corner)
      "#22a567",  # quadrant 8
      "#5c8a59"), # quadrant 9 (bottom right corner)
    figure_format = "svg",
    scatter_legend_dot_size = 3,
    scatter_legend_bg_dot_size = 2,
  #  scatter_legend_n = c(3, 3, 3),
    scatter_show_axis_values = TRUE,
    x_axis_range = NULL,
    y_axis_range = NULL,
    grid_legend_x_axes_label = NULL,
    grid_legend_y_axes_label = NULL,
    seed = 42
    ){

  set.seed(seed)
  hist_plot = NULL
  error_plot = NULL
  if(!is.null(y_variable)){
    y_axes_1 = 2
  } else {
    y_axes_1 = 1
  }


  if(is.data.frame(x_variable)){
    grid_legend_x_axes_label <- colnames(x_variable)
    x_variable <- x_variable[[1]]
  }

  if(is.data.frame(y_variable)){
    grid_legend_y_axes_label <- colnames(y_variable)
    y_variable <- y_variable[[1]]
  }


  #### Create "color codes" for each square based on quantiles or quartiles ####
  df <- tibble::tibble(
    topic = 1:length(text),
    text,
    x_variable,
    y_variable,
  )

  # Compute grouping columns
  df <- df %>%
    mutate(
      x_variable_grouped = ntile(x_variable, n_tile),
      y_variable_grouped = if (!is.null(y_variable)) ntile(y_variable, n_tile) else NA_integer_
    ) %>%
    mutate(
      x_variable_grouped_three = case_when(
        x_variable_grouped == 1 ~ 1,
        x_variable_grouped == n_tile ~ 3,
        TRUE ~ 2
      ),
      y_variable_grouped_three = case_when(
        !is.null(y_variable) & y_variable_grouped == 1 ~ 1,
        !is.null(y_variable) & y_variable_grouped == n_tile ~ 3,
        !is.null(y_variable) ~ 2,
        TRUE ~ NA_integer_
      )
    ) %>%
    mutate(
      combined_category_three = if (!is.null(y_variable)) {
        (x_variable_grouped_three - 1) * 3 + y_variable_grouped_three
      } else {
        x_variable_grouped_three  # One-dimensional case
      }
    )

  # Handle square ranking criteria separately for 1D and 2D cases #
  if (!is.null(y_variable)) {
    df <- df %>%
      mutate(
        ranking_criteria = case_when(
          combined_category_three %in% c(1, 3, 7, 9) ~ abs((x_variable + y_variable) / 2 - mean((x_variable + y_variable) / 2, na.rm = TRUE)), # Extreme based on both
          combined_category_three %in% c(2, 8) ~ abs(y_variable - mean(y_variable, na.rm = TRUE)),  # Extreme based on y_variable
          combined_category_three %in% c(4, 6) ~ abs(x_variable - mean(x_variable, na.rm = TRUE)),  # Extreme based on x_variable
          combined_category_three == 5 ~ -abs((x_variable + y_variable) / 2 - mean((x_variable + y_variable) / 2, na.rm = TRUE)), # Closest to mean
          TRUE ~ 0
        )
      )
  } else {
    df <- df %>%
      mutate(
        ranking_criteria = case_when(
          combined_category_three == 1 ~ abs(x_variable - mean(x_variable, na.rm = TRUE)),  # Extreme low
          combined_category_three == 3 ~ abs(x_variable - mean(x_variable, na.rm = TRUE)),  # Extreme high
          combined_category_three == 2 ~ -abs(x_variable - mean(x_variable, na.rm = TRUE)), # Closest to mean
          TRUE ~ 0
        )
      )
  }
  # Save data for plotting the distribution
  df_for_distribution <- df


  #### Filter sentences/cases to show as examples ####
  # Filter rows to include only those with specified words in the language variable ####
  if (!is.null(filter_words)) {
    df <- df %>%
      dplyr::filter(
        purrr::map_lgl(language, ~ all(
          stringi::stri_detect_fixed(.x, filter_words)))
      )
  }

  # Select N most extreme per category
  df_examples <- df %>%
    group_by(combined_category_three) %>%
    arrange(desc(ranking_criteria)) %>%
    slice_head(n = n_examples) %>%
    ungroup()


  #### Histogram of predictions and targets using the entire dataset ####

  if (type == "prediction_errors") {

    # Reshape the data to long format for ggplot2
    df_long <- df_for_distribution %>%
      tidyr::pivot_longer(
        cols = c(x_variable, y_variable),
        names_to = "variable",
        values_to = "value"
      )

    # Create the histogram with legend
    hist_plot <- ggplot2::ggplot(df_long, ggplot2::aes(x = value, fill = variable)) +
      ggplot2::geom_histogram(
        bins = 30,
        alpha = 0.5,
        position = "identity",
        color = "black"  # Set border color for better contrast
      ) +
      ggplot2::scale_fill_manual(
        values = c("x_variable" = predictions_color, "y_variable" = target_color),
        labels = c("Predictions", "Targets")
      ) +
      ggplot2::labs(
        title = paste("Histogram of", grid_legend_x_axes_label, "and", grid_legend_y_axes_label),
        x = "Value",
        y = "Count",
        fill = "Variable"
      ) +
      ggplot2::theme_minimal()

    df_for_distribution$error <- df_for_distribution$x_variable - df_for_distribution$y_variable

    #### Plot for the distribution of errors (absolute_error)
    error_plot <- ggplot2::ggplot(df_for_distribution) +
      ggplot2::geom_histogram(
        ggplot2::aes(x = error),
        bins = 30,
        fill = error_color,
        color = "black",
        alpha = 0.7
      ) +
      ggplot2::labs(
        title = "Histogram of the absolute errors",
        x = "Absolute error",
        y = "Count"
      ) +
      ggplot2::theme_minimal()
  }


  #### Make the Scatter plot ####

  # Add jitter
  if(!is.null(jitter)){
    df_for_distribution <-  dynamic_jitter_data(
      data = df_for_distribution,
      x_col = "x_variable",
      if(!is.null(y_variable)) y_col = "y_variable",
      jitter_factor = jitter,
      seed = seed)

    df_for_distribution$x_variable <- df_for_distribution$x_jittered
    if(!is.null(y_variable)) df_for_distribution$y_variable <- df_for_distribution$y_jittered
  }

  if(!is.null(y_variable)){
    df_for_distribution <- reorder_columns(df_for_distribution, "y_variable", 9)
  }
  # Reorder columns to be plotted in the scatter plot to satisfy topicsScatterLegend()
  df_for_distribution <- reorder_columns(df_for_distribution, "x_variable", 5)




  # Ensure we have a column called color_categories for the topicsScatterLegend()
  df_for_distribution$color_categories <- df_for_distribution$combined_category_three

  scatter_plot <- topics::topicsScatterLegend(
    bivariate_color_codes = distribution_color,
    filtered_test = df_for_distribution,
  #  num_popout = scatter_legend_n,
    way_popout_topics = "mean",
    user_spec_topics = df_examples$topic,
    allow_topic_num_legend = FALSE,
    scatter_show_axis_values = scatter_show_axis_values,
    y_axes_1 = y_axes_1,
    cor_var = "",
    label_x_name = grid_legend_x_axes_label,
    label_y_name = grid_legend_y_axes_label,
    save_dir = NULL,
    figure_format = figure_format,
    scatter_popout_dot_size = scatter_legend_dot_size,
    scatter_bg_dot_size = scatter_legend_bg_dot_size,
    width = width,
    height = height,
    seed = seed
  )

  # Adjusing the x and y dimensions
  if(is.null(x_axis_range)){
    x_axis_range <- c(
      min(df_for_distribution$x_variable),
      max(df_for_distribution$x_variable))
  }
  if(!is.null(y_variable)){
    if(is.null(y_axis_range)){
      y_axis_range <- c(
        min(df_for_distribution$y_variable),
        max(df_for_distribution$y_variable))
    }
  }


  suppressMessages(
  scatter_plot$legend <- scatter_plot$legend +
    ggplot2::scale_x_continuous(limits = x_axis_range) +
    if (!is.null(y_variable)) {
      ggplot2::scale_y_continuous(limits = y_axis_range)
    } else {
      NULL
    }
  )

  #### Sorting output ####
  # Renaming variable
  df_examples <- df_examples %>%
    dplyr::rename(id = topic)

  # Initialize an empty list
  results <- list()

  # Conditionally add elements
  if (!is.null(error_plot)) {
    results$error_plot <- error_plot
  }
  if (!is.null(hist_plot)) {
    results$histogram_plot <- hist_plot
  }

  # Always add these elements
  results$examples <- df_examples
  results$scatter_plot <- scatter_plot$legend

  # Return the filtered and selected examples
  return(results)
}

# Alias functions
#' @rdname textTrainExamples
#' @export
textPredictExamples <- textTrainExamples




###if(selection_method == "min_max" |
###   selection_method == "min" |
###   selection_method == "max"){
###  df <- df %>%
###    dplyr::left_join(df_short %>% dplyr::select(topic, category), by = "topic") %>%
###    dplyr::mutate(
###      color_categories = dplyr::case_when(
###        category == "min" ~ 1,
###        category == "max" ~ 3,
###        TRUE ~ 2
###      )
###    ) %>%
###    dplyr::select(-category)
###
###  num_popout = c(n_examples, 0, n_examples)
###  user_spec_topics <- paste0(df_short$topic)
###}
###if(selection_method == "min_mean_max"){
###
###  df <- df %>%
###    dplyr::left_join(df_short %>% dplyr::select(topic, category), by = "topic") %>%
###    dplyr::mutate(
###      color_categories = dplyr::case_when(
###        category == "min" ~ 1,
###        category == "mean" ~ 2,
###        category == "max" ~ 3,
###        TRUE ~ 2
###      )
###    ) %>%
###    dplyr::select(-category) # Optionally remove the temporary `category` column
###
###  num_popout = c(n_examples, n_examples, n_examples)
###  user_spec_topics <- paste0(df_short$topic)
###}
###
###
#### The current way of selecting colours are off in topcsScatterLegend
###if(selection_method %in% c("min_mean_max", "min_max")){
###  distribution_color = distribution_color[c(3,2,1)]
###}
###if(selection_method %in% c("min", "max")){
###  distribution_color = distribution_color[c(1, 2)]
###}
####table(df$color_categories)

