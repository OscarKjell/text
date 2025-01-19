#Sys.setenv(OMP_NUM_THREADS = "1") #Limit the number of threads to prevent conflicts.
#
#Sys.setenv(OMP_MAX_ACTIVE_LEVELS = "1")
#
## If above does not work, you can also try this; although this solution might have some risks assocaited with it (for more information see https://github.com/dmlc/xgboost/issues/1715)
#Sys.setenv(KMP_DUPLICATE_LIB_OK = "TRUE") #Temporarily allows execution despite duplicate OpenMP libraries.




#' Show language examples (Experimental)
#'
#' This function selects language examples that been used in the textTrain() or textAssess() functions.
#' @param text (string) the language that was used for prediction/assessment/classification.
#' @param target (numeric) the variable used for training (y).
#' @param predictions (numeric) the outcome from the model (i.e., y_hat).
#' @param n_examples (integer) the number of language examples to show.
#' @param selection_variable (string) selection method: "predictions", "error", or "targets".
#' @param selection_method (string) specification of which examples to select "min", "max", "min_max", "min_mean_max", "quintiles".
#' @param include_words (character vector) words required to be included in examples.
#' @param target_color (string)
#' @param predictions_color  (string)= "darkblue",
#' @param error_color =  (string)"darkred",
#' @param distribution_color  (string) colors of the distribution plot
#' @param figure_format  (string) file format of the figures.
#' @returns A tibble including examples with descriptive variables.
#' @importFrom dplyr filter select arrange slice group_by summarize mutate
#' @importFrom stringi stri_detect_fixed
#' @importFrom purrr map_lgl
#' @export
textTrainExamples <- function(
    text,
    target,
    predictions,
    n_examples = 5,
    selection_method = "min_max", # "min", "max", "min_max", "min_mean_max", "quintiles"
    selection_variable = "predictions", # "predictions", "error", or "targets"
    include_words = NULL,
    target_color = "darkgreen",
    predictions_color = "darkblue",
    error_color = "darkred",
    distribution_color = c("darkgreen", "gray", "darkred"),
    figure_format = "svg"
    ){

  # Combine responses with predictions and target scores
  df <- tibble::tibble(
    topic = 1:length(text),
    language = text,
    target = target,
    predictions = predictions,
    error = target - predictions,
    absolute_error = abs(target - predictions)
  )

  # Filter rows to include only those with specified words in the language variable
  if (!is.null(include_words)) {
    df <- df %>%
      dplyr::filter(
        purrr::map_lgl(language, ~ all(stringi::stri_detect_fixed(.x, include_words)))
      )
  }

  # Variable to base the selection on
  selection_var <- switch(
    selection_variable,
    "predictions" = df$predictions,
    "error" = df$absolute_error,
    "targets" = df$target,
    stop("Invalid selection_variable. Choose 'predictions', 'error', or 'targets'.")
  )

  #### Select examples based on the selection parameter ####
  df_short <- switch(
    selection_method,
    "min" = {
      df %>%
        dplyr::arrange(selection_var) %>%
        dplyr::slice(1:n_examples) %>%
        dplyr::mutate(category = "min")
    },
    "max" = {
      df %>%
        dplyr::arrange(desc(selection_var)) %>%
        dplyr::slice(1:n_examples) %>%
        dplyr::mutate(category = "max")
    },
    "min_max" = {
      dplyr::bind_rows(
        df %>%
          dplyr::arrange(selection_var) %>%
          dplyr::slice(1:n_examples) %>%
          dplyr::mutate(category = "min"),
        df %>%
          dplyr::arrange(desc(selection_var)) %>%
          dplyr::slice(1:n_examples) %>%
          dplyr::mutate(category = "max")
      )
    },
    "min_mean_max" = {
      mean_value <- mean(selection_var, na.rm = TRUE)
      df_mean <- df %>%
        dplyr::mutate(distance_to_mean = abs(selection_var - mean_value)) %>%
        dplyr::arrange(distance_to_mean) %>%
        dplyr::slice(1:n_examples) %>%
        dplyr::mutate(category = "mean")

      dplyr::bind_rows(
        df %>%
          dplyr::arrange(selection_var) %>%
          dplyr::slice(1:n_examples) %>%
          dplyr::mutate(category = "min"),
        df %>%
          dplyr::arrange(desc(selection_var)) %>%
          dplyr::slice(1:n_examples) %>%
          dplyr::mutate(category = "max"),
        df_mean
      )
    },
    "quintiles" = {
      quintile_bins <- dplyr::ntile(selection_var, 5)
      dplyr::bind_rows(
        df %>%
          dplyr::filter(quintile_bins == 1) %>%
          dplyr::slice(1:n_examples) %>%
          dplyr::mutate(category = "quintile1"),
        df %>%
          dplyr::filter(quintile_bins == 5) %>%
          dplyr::slice(1:n_examples) %>%
          dplyr::mutate(category = "quintile5")
      )
    },
    stop("Invalid selection. Choose 'min', 'max', 'min_max', 'min_mean_max', or 'quintiles'.")
  )

  #### Histogram of predictions and targets using the entire dataset ####
  hist_plot <- ggplot2::ggplot(df) +
    ggplot2::geom_histogram(
      ggplot2::aes(x = predictions, fill = "Predictions"),
      bins = 30,
      alpha = 0.5,
      position = "identity",
      color = predictions_color,  # Set border color to red for predictions
      fill = predictions_color    # Set fill color to red for predictions
    ) +
    ggplot2::geom_histogram(
      ggplot2::aes(x = target, fill = "Targets"),
      bins = 30,
      alpha = 0.5,
      position = "identity",
      color = target_color, # Set border color to blue for targets
      fill = target_color   # Set fill color to blue for targets
    ) +
    ggplot2::labs(
      title = "Histogram of Predictions and Targets",
      x = "Value",
      y = "Count",
      fill = "Variable"
    ) +
    ggplot2::theme_minimal()

  #### Plot for the distribution of errors (absolute_error) ####
  error_plot <- ggplot2::ggplot(df) +
    ggplot2::geom_histogram(
      ggplot2::aes(x = error),
      bins = 30,
      fill = error_color,
      color = "black",
      alpha = 0.7
    ) +
    ggplot2::labs(
      title = "Histogram of Absolute Errors",
      x = "Absolute Error",
      y = "Count"
    ) +
    ggplot2::theme_minimal()

  if(selection_method == "min_max"){
  df <- df %>%
    dplyr::left_join(df_short %>% dplyr::select(topic, category), by = "topic") %>%
    dplyr::mutate(
      color_categories = dplyr::case_when(
        category == "min" ~ 1,
        category == "max" ~ 3,
        TRUE ~ 2
      )
    ) %>%
    dplyr::select(-category)

  num_popout = c(n_examples, 0, n_examples)

  }

  if(selection_method == "min_mean_max"){

    df <- df %>%
      dplyr::left_join(df_short %>% dplyr::select(topic, category), by = "topic") %>%
      mutate(
        color_categories = dplyr::case_when(
          category == "min" ~ 1,
          category == "mean" ~ 2,
          category == "max" ~ 3,
          TRUE ~ 2
        )
      ) %>%
      dplyr::select(-category) # Optionally remove the temporary `category` column

    distribution_color = distribution_color[c(3, 2, 1)]

    num_popout = c(n_examples, n_examples, n_examples)

    user_spec_topics <- paste0("t_", df_short$topic)
    #user_spec_topics <- df_short$topic
  }

  #table(df$color_categories)

  # Dynamically move a column to the fifth position
  df <- df %>%
    dplyr::relocate(all_of(selection_variable), .before = 6)


  scatter_plot <- topics::topicsScatterLegend(
    bivariate_color_codes = distribution_color,
    filtered_test = df,
    num_popout = num_popout,
    way_popout_topics = "mean",
    user_spec_topics = user_spec_topics,
    allow_topic_num_legend = FALSE,
    scatter_show_axis_values = TRUE,
    y_axes_1 = 1,
    cor_var = "",
    label_x_name = "x",
    label_y_name = "y",
    save_dir = NULL,
    figure_format = figure_format,
    scatter_popout_dot_size = 8,
    scatter_bg_dot_size = 4,
    width = 10,
    height = 8,
    seed = 42
  )
  scatter_plot

  results <- list(
    error_plot = error_plot,
    histogram_plot = hist_plot,
    examples = df_short,
    scatter_plot = scatter_plot$legend)

  # Return the filtered and selected examples
  return(results)
}

# Alias functions
#' @rdname textTrainExamples
#' @export
textPredictExamples <- textTrainExamples

