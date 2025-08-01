
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
#' @importFrom stats runif
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
    dplyr::mutate(
      x_jittered = .data[[x_col]] + stats::runif(n(), -jitter_width, jitter_width)
    )

  # If y_col is specified, calculate jitter for y
  if (!is.null(y_col)) {
    if (!y_col %in% colnames(data)) {
      stop(paste("Column", y_col, "not found in the data."))
    }

    jitter_height <- diff(range(data[[y_col]], na.rm = TRUE)) * jitter_factor
    data <- data %>%
      dplyr::mutate(
        y_jittered = .data[[y_col]] + stats::runif(n(), -jitter_height, jitter_height)
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


#' Identify language examples.
#'
#' This function identifies examples based on the frequency of use of n-grams (see the topics-pacakge),
#' estimated topic prevalence (see the topics-pacakge), or  assessment scores from textTrain() or textPredict().
#' @param text (string) the language that was used for prediction/assessment/classification.
#' @param x_variable (numeric) the variable used for training (y).
#' @param y_variable (numeric) the outcome from the model (i.e., y_hat).
#' @param type (string) If you are plotting errors between predicted and targeted scores, you can set the type to "prediction_errors",
#' to produce two extra plots: distribution of scores and absolute error.
#' @param n_tile (integer) the n tile to split the data in (to show the most extreme tiles in different colours).
#' @param n_examples (integer) the number of language examples to show/select in each quadrant.
#' When providing both x_variable and y_variable, each example is categorized into one of nine bivariate quadrants
#' based on its position in the scatterplot (e.g., low–low, high–high, center).
#' Within each quadrant, the function selects the most extreme examples by computing the distance to that quadrant’s corner:
#'	Corner quadrants (1, 3, 7, 9):
#' Examples closest to the corner points (e.g., min x & max y) are selected using Euclidean distance.
#' Edge quadrants (2, 4, 6, 8):
#' Examples furthest along the relevant axis (x or y) are selected.
#' Center quadrant (5):
#' Examples closest to the mean of both x and y are selected.
#' @param jitter (integer) the percentage of jitter to add to the data for the scatter plot.
# @param x_variable (string) selection method: "predictions", "error", or "targets".
# @param selection_method (string) specification of which examples to select "min", "max", "min_max", "min_mean_max", "quintiles".
#' @param filter_words (character vector) words required to be included in examples.
#' @param target_color (string)
#' @param predictions_color  (string) = "darkblue",
#' @param error_color =  (string) "darkred",
#' @param distribution_color  (string) colors of the distribution plot
#' @param figure_format  (string) file format of the figures.
#' @param scatter_legend_dot_size (integer) The size of highlighted dots in the scatter legend.
#' @param scatter_legend_bg_dot_size (integer) The size of background dots in the scatter legend.
#' @param scatter_legend_dots_alpha (numeric) The transparency alphe level of the dots.
#' @param scatter_legend_bg_dots_alpha (numeric) The transparency alphe level of the background dots.
# @param scatter_legend_n (numeric or vector) A vector determining the number of dots to emphasize in each quadrant of the scatter legend.
#' For example: c(1,0,1) result in one dot in each quadrant except for the middle quadrant.
# @param scatter_legend_method (string) The method to filter topics to be emphasized in the scatter legend; either "mean", "max_x", or "max_y".
# @param scatter_legend_specified_topics (vector) Specify which topic(s) to emphasize in the scatter legend.
# For example, c("t_1", "t_2"). If set, scatter_legend_method will have no effect.
# @param scatter_legend_topic_n (boolean) If TRUE, the topic numbers are shown in the scatter legend.
#' @param scatter_show_axis_values (boolean) If TRUE, the estimate values are shown on the distribution plot axes.
#' @param scatter_legend_regression_line_colour (string) If a colour string is added, a regression line will be plotted.
#' @param x_axis_range (numeric vector) range of x axis (e.g., c(1, 100)).
#' @param y_axis_range (numeric vector) range of y axis (e.g., c(1, 100)).
#' @param grid_legend_x_axes_label (string) x-axis label of the grid topic plot.
#' @param grid_legend_y_axes_label (string) y-axis label of the grid topic plot.
#' @param grid_legend_title (string)
#' @param grid_legend_number_size (integer)
#' @param grid_legend_number_color (string)
#' @param grid_legend_title_color (string)
#' @param grid_legend_title_size (integer)
#' @param seed (integer) The seed to set for reproducibility.
#' @returns A tibble including examples with descriptive variables.
#' @importFrom dplyr filter select arrange slice slice_head group_by summarize mutate rename ntile case_when
#' @importFrom stringi stri_detect_fixed
#' @importFrom purrr map_lgl
#' @importFrom tidyr pivot_longer
#' @importFrom ggplot2 geom_histogram aes labs theme_minimal scale_fill_manual geom_smooth
#' @importFrom topics topicsScatterLegend
#' @export
textExamples <- function(
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
    scatter_legend_dots_alpha = .80,
    scatter_legend_bg_dots_alpha = .20,
  #  scatter_legend_n = c(3, 3, 3),
    scatter_show_axis_values = TRUE,
    scatter_legend_regression_line_colour = NULL,
    x_axis_range = NULL,
    y_axis_range = NULL,
    grid_legend_x_axes_label = NULL,
    grid_legend_y_axes_label = NULL,
    grid_legend_title = NULL,
    grid_legend_number_size = 8,
    grid_legend_number_color = "white",
    grid_legend_title_color = "black",
    grid_legend_title_size = 0,
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

  if(is.data.frame(text)){
    n_cases <- nrow(text)
  } else {
    n_cases <- length(text)
  }

  #### Create "color codes" for each square based on quantiles or quartiles ####
  df <- tibble::tibble(
    topic = 1:n_cases,
    text,
    x_variable,
    y_variable,
  )


#######################################################
##### Compute grouping columns and color categories####
#######################################################

  # Conditional grouping and categorization
  df <- df %>%
    dplyr::mutate(
      # Split into quantiles
      x_variable_grouped = dplyr::ntile(x_variable, n_tile),
      y_variable_grouped = if (!is.null(y_variable)) dplyr::ntile(y_variable, n_tile) else NA_integer_,

      # Recategorize into 3 groups: Low (1), Medium (2), High (3)
      x_variable_grouped_three = dplyr::case_when(
        x_variable_grouped == 1 ~ 1,  # Low
        x_variable_grouped == n_tile ~ 3,  # High
        TRUE ~ 2  # Medium
      ),
      y_variable_grouped_three = dplyr::case_when(
        !is.null(y_variable) & y_variable_grouped == 1 ~ 1,  # Low
        !is.null(y_variable) & y_variable_grouped == n_tile ~ 3,  # High
        !is.null(y_variable) ~ 2,
        TRUE ~ NA_integer_
      ),

      # Combine x and y groupings to form color categories
      color_categories = if (!is.null(y_variable)) {
        dplyr::case_when(
          x_variable_grouped_three == 1 & y_variable_grouped_three == 3 ~ 1,
          x_variable_grouped_three == 2 & y_variable_grouped_three == 3 ~ 2,
          x_variable_grouped_three == 3 & y_variable_grouped_three == 3 ~ 3,
          x_variable_grouped_three == 1 & y_variable_grouped_three == 2 ~ 4,
          x_variable_grouped_three == 2 & y_variable_grouped_three == 2 ~ 5,
          x_variable_grouped_three == 3 & y_variable_grouped_three == 2 ~ 6,
          x_variable_grouped_three == 1 & y_variable_grouped_three == 1 ~ 7,
          x_variable_grouped_three == 2 & y_variable_grouped_three == 1 ~ 8,
          x_variable_grouped_three == 3 & y_variable_grouped_three == 1 ~ 9,
          TRUE ~ NA_real_
        )
      } else {
        # If y_variable is NULL, only categorize based on x_variable
        x_variable_grouped_three
      }
    ) %>%
    # Convert to string first, then factor with explicit levels
    dplyr::mutate(color_categories = as.character(color_categories),
           color_categories = factor(color_categories, levels = as.character(1:9))
    )

  # Handle square ranking criteria to get text examples separately for 1D and 2D cases #
  if (!is.null(y_variable)) {
    #df <- df %>%
    #  dplyr::mutate(
    #    ranking_criteria = dplyr::case_when(
    #      color_categories %in% c(1, 3, 7, 9) ~ abs((x_variable + y_variable) / 2 - mean((x_variable + y_variable) / 2, na.rm = TRUE)), # Extreme based on both
    #      color_categories %in% c(2, 8) ~ abs(y_variable - mean(y_variable, na.rm = TRUE)),  # Extreme based on y_variable
    #      color_categories %in% c(4, 6) ~ abs(x_variable - mean(x_variable, na.rm = TRUE)),  # Extreme based on x_variable
    #      color_categories == 5 ~ -abs((x_variable + y_variable) / 2 - mean((x_variable + y_variable) / 2, na.rm = TRUE)), # Closest to mean
    #      TRUE ~ 0
    #    )
    #  )

      df <- df %>%
        mutate(
          ranking_criteria = case_when(
            color_categories == "1" ~ sqrt((x_variable - min(x_variable, na.rm = TRUE))^2 + (y_variable - max(y_variable, na.rm = TRUE))^2),
            color_categories == "2" ~ abs(y_variable - max(y_variable, na.rm = TRUE)),
            color_categories == "3" ~ sqrt((x_variable - max(x_variable, na.rm = TRUE))^2 + (y_variable - max(y_variable, na.rm = TRUE))^2),
            color_categories == "4" ~ abs(x_variable - min(x_variable, na.rm = TRUE)),
            color_categories == "5" ~ sqrt((x_variable - mean(x_variable, na.rm = TRUE))^2 + (y_variable - mean(y_variable, na.rm = TRUE))^2),
            color_categories == "6" ~ abs(x_variable - max(x_variable, na.rm = TRUE)),
            color_categories == "7" ~ sqrt((x_variable - min(x_variable, na.rm = TRUE))^2 + (y_variable - min(y_variable, na.rm = TRUE))^2),
            color_categories == "8" ~ abs(y_variable - min(y_variable, na.rm = TRUE)),
            color_categories == "9" ~ sqrt((x_variable - max(x_variable, na.rm = TRUE))^2 + (y_variable - min(y_variable, na.rm = TRUE))^2),
            TRUE ~ NA_real_
          )
        )


  } else {
    df <- df %>%
      mutate(
        ranking_criteria = case_when(
          color_categories == "1" ~ abs(x_variable - min(x_variable, na.rm = TRUE)),  # Far left (low)
          color_categories == "3" ~ abs(x_variable - max(x_variable, na.rm = TRUE)),  # Far right (high)
          color_categories == "2" ~ abs(x_variable - mean(x_variable, na.rm = TRUE)), # Center (closest to mean)
          TRUE ~ NA_real_
        )
      )
  }
  # Save data for plotting the distribution
  df_for_distribution <- df


  #### Filter sentences/cases to show as examples ####
  # Filter rows to include only those with specified words in the language variable ####
  if (!is.null(filter_words)) {
    df_examples <- df %>%
      dplyr::filter(
        purrr::map_lgl(
          text,
          ~ all(stringi::stri_detect_regex(.x, paste0("\\b", filter_words, "\\b")))
        )
      )

    df_examples <- df_examples %>%
      dplyr::group_by(color_categories) %>%
      dplyr::arrange(ranking_criteria) %>% # minimal distance = most representative extreme
      dplyr::slice_head(n = n_examples) %>%
      dplyr::ungroup()


  } else {
    # Select N most extreme per category
    df_examples <- df %>%
      dplyr::group_by(color_categories) %>%
      dplyr::arrange(ranking_criteria) %>%
      dplyr::slice_head(n = n_examples) %>%
      dplyr::ungroup()

  }



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



  # This is because topicsScatterLegend expect a prevalence variable to scale the dots
  # since we do not have any difference in prevalence we set it o one.
  df_for_distribution$prevalence <- rep(1, nrow(df_for_distribution))


  # Reorder columns to be plotted in the scatter plot to satisfy topicsScatterLegend()
  df_for_distribution <- reorder_columns(df_for_distribution, "color_categories", ncol(df_for_distribution))

  if(!is.null(y_variable)){
    df_for_distribution <- reorder_columns(df_for_distribution, "y_variable", 9)
  }
  # Reorder columns to be plotted in the scatter plot to satisfy topicsScatterLegend()
  df_for_distribution <- reorder_columns(df_for_distribution, "x_variable", 5)


  # Ensure we have a column called color_categories for the topicsScatterLegend()
  # Define color codes with explicit naming
  distribution_color <- c(
      "1" = distribution_color[1],
      "2" = distribution_color[2],
      "3" = distribution_color[3],
      "4" = distribution_color[4],
      "5" = distribution_color[5],
      "6" = distribution_color[6],
      "7" = distribution_color[7],
      "8" = distribution_color[8],
      "9" = distribution_color[9]
    )

  scatter_legend_dot_size = c(scatter_legend_dot_size, scatter_legend_dot_size)
  scatter_legend_bg_dot_size = c(scatter_legend_bg_dot_size, scatter_legend_bg_dot_size)

  #### TESTING
  df_for_distribution$color_categories

  table(df_for_distribution$color_categories)

  #####

  # Adding dot distribution scatter legened
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
    scatter_legend_dots_alpha = scatter_legend_dots_alpha,
    scatter_legend_bg_dots_alpha = scatter_legend_bg_dots_alpha,
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

  # Add regression/correlation line
  if(!is.null(scatter_legend_regression_line_colour) & !is.null(y_variable)) {
    scatter_plot$legend <- scatter_plot$legend +
      ggplot2::geom_smooth(aes(x = df_for_distribution$x_variable,
                      y = df_for_distribution$y_variable),
                  method = "lm",
                  se = FALSE,
                  color = scatter_legend_regression_line_colour,
                  size = 0.5)
  }

  # Adding matrix legened
  legend <- topics::topicsGridLegend(
    bivariate_color_codes = distribution_color,
    filtered_test = df_for_distribution,
 #   cor_var = df_for_distribution,
    save_dir = NULL,
    figure_format = figure_format,
    seed = seed,
    y_axes_1 = y_axes_1,
    legend_title = grid_legend_title,
   legend_title_size = grid_legend_title_size,
    titles_color = grid_legend_title_color,
    legend_x_axes_label = grid_legend_x_axes_label,
    legend_y_axes_label = grid_legend_y_axes_label,
    topic_data_all = df_for_distribution,
    legend_number_color = grid_legend_number_color,
    legend_number_size = grid_legend_number_size
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
  results$legend <- legend
  results$scatter_plot <- scatter_plot$legend

  # Return the filtered and selected examples
  return(results)
}


