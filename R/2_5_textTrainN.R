
##### indexing function #### 

#' Creates a nested list of element indexes. These elements will be used in the training process of textTrainN. 
#' @param n_cross_val Number of cross-validations.  
#' @param sample_percents Numeric vector of sample percent sizes, e.g. (25,50,75,100). 
#' @param len Length of the dataset (i.e, number of datapoints).
#' @param seed Set seed. 
#' @return Nested list of elements. 
#' @noRd
indexing <- function(n_cross_val = 1, sample_percents, len, seed) {
  
  nested_list <- list()
  n_tests <- length(sample_percents)
  set.seed(seed)
  
  for (check in 1:n_cross_val) {
    # Create a new sub_list for each check
    sub_list <- list() 
    for (idx in 1:n_tests) {
      # Calculate sample size as a percentage of the total number of data points
      sample_size <- round(sample_percents[idx] / 100 * len, 0)
      
      # Check for a minimum sample size due to K-fold cross-validation in textTrain function. 
      if (sample_size < 10) { 
        error_msg <- paste("The number of data points must exceed 10 due to K-fold cross-validation in textTrain")
        stop(error_msg)
      }
      
      # index is a numeric vector with random indexes of elements to train. 
      index <- sample(1:len, size = sample_size, replace = FALSE)
      index <- list(index)
      
      sub_list <- append(sub_list, index)
    }
    # Add sub_list to nested_list
    nested_list <- append(nested_list, list(sub_list)) 
  }
  return(nested_list)
}


#### textTrainN function ####

#' Compute cross-validated correlations for different sample-sizes of a data set. 
#' The cross-validation process can be repeated several times to enhance the reliability of the evaluation.
#' @param x Word embeddings from textEmbed (or textEmbedLayerAggregation). 
#' If several word embedding are provided in a list they will be concatenated.
#' @param y Numeric variable to predict.
#' @param sample_percents (numeric) Numeric vector that specifies the percentages of the total number of data points to include in each sample (default = c(25,50,75,100), i.e., correlations are evaluated for 25%%,50%%,75%% and 100%% of 
#' the datapoints). The datapoints in each sample are chosen randomly for each new sample.  
#' @param n_cross_val (numeric) Value that determines the number of times to repeat the cross-validation.
#' (default = 1, i.e., cross-validation is only performed once). 
#' @param seed (numeric) Set different seed (default = 2023).
#' @return A tibble containing correlations for each sample. If n_cross_val > 1, correlations for each new cross-validation, 
#' along with standard-deviation and mean correlation is included in the tibble. The information in the tibble is 
#' visualised via the textTrainNPlot function. 
#' @examples
#' # Compute correlations for 25%, 50%, 75% and 100% of the data in word_embeddings and perform 
#' # cross-validation thrice. 
#' 
#' \dontrun{
#' tibble_to_plot <- textTrainN(
#'       x = word_embeddings_4$texts$harmonytext,
#'       y = Language_based_assessment_data_8$hilstotal,
#'       sample_percents = c(25,50,75,100),
#'       n_cross_val = 3,
#' )
#' 
#' # tibble_to_plot contains correlation-coefficients for each cross_validation and 
#' # standard deviation and mean value for each sample. The tibble can be plotted 
#' # using the testTrainNPlot function.
#' 
#' # Examine tibble
#' tibble_to_plot
#' }
#' @seealso See \code{\link{textTrainNPlot}}. 
#' @importFrom tibble tibble
#' @export
textTrainN <- function(
    x = word_embeddings_4$texts$harmonytext,
    y = Language_based_assessment_data_8$hilstotal,
    sample_percents = c(25,50,75,100),
    n_cross_val = 1,
    seed = 2023
) {
  # number of tests (i.e., number of samples for each cross-validation)
  n_tests = length(sample_percents)
  
  # number of elements 
  len <- length(y)
  
  # instantiate dataframe 
  results_df <- data.frame()
  
  # vector´that will contain the quantity of elements in each sample.  
  sample_sizes <- c()
  
  # Add "percent" column to dataframe 
  for (test in 1:n_tests){
    num_samples <- sample_percents[test]/100 * len
    results_df[test, "percent"] <- sample_percents[test]
    sample_sizes <- c(sample_sizes, num_samples)
  }
  
  # Add sample_size column to dataframe
  results_df["sample_size"] <- sample_sizes  
  
  # Set seed
  set.seed(seed)
  
  # Call the indexing function to generate indexes
  indexes <- indexing(n_cross_val, sample_percents, len, seed)
  
  # Vector containing column names for each cross-validation. E.g., if n_cross_val = 3, 
  # columns_to_calculate will be "Test1", "Test2", "Test3"
  columns_to_calculate <- c()
  
  #### Training & Cross-Validation #### 
  
  # Number of cross-validation
  for (check in 1:n_cross_val) {
    # Adds column names for each cross validation to columns_to_calculate
    columns_to_calculate <- c(columns_to_calculate, sprintf("Test%s", as.character(check)))
   
    #Performs each cross-validation. 
    for (idx in 1:n_tests) {
      # Trains model with data in each sample (i.e., with elements in each nested list in indexes)
      trained <- text::textTrain(
        x = x[indexes[[check]][[idx]],],
        y = y[indexes[[check]][[idx]]]
      )
      
      # Extract the correlation-coefficient
      value_to_insert <- trained$results[4]
      
      # Assign the correlation-coefficient to results_df
      column_name <- sprintf("Test%s", as.character(check))
      results_df[idx, column_name] <- value_to_insert
    }
  }
  
  # Calculate the mean and standard deviation for each row in results_df. 
  results_df["mean"] <- apply(results_df[columns_to_calculate], 1, mean)
  results_df["std"] <- apply(results_df[columns_to_calculate], 1, sd)
  
  # Convert to dataframe to tibble 
  results_df <- as_tibble(results_df)
  return(results_df)
}

#### textTrainNPlot function #### 

#' Plot cross-validated correlation coefficients across different sample-sizes from the object
#' returned by the textTrainN function. If the number of cross-validations exceed one, then 
#' error-bars will be included in the plot. 
#' @param tibble (tibble) Object returned by the function textTrainN.
#' @param sample_percents (numeric) Vector containing the percents of the total number of datapoints that is 
#' included in each sample (default = c(25,50,75,100)).   
#' @param n_cross_val (numeric) Value of the number of times cross-validation has been repeated (default = 1, 
#' i.e., cross-validation has only been applied once). 
#' @param x_unit (character, "percent" or "quantity") Determines whether the x-axis-values should represent the number of elements in each sample,
#' or the number of percent of the total data they represent (default = "percent"). 
#' @param y_range (numeric) Optional. Determines the y_range. E.g, y_range = c(1,2) sets the y_range from 1 to 2 (default = NULL). 
#' @param title (character) Determine plot title (default = "Cross-validated correlation coefficients across different sample sizes").
#' @param x_axes_label (character) Determine x-axis-label (default = "Sample Size (percent)").
#' @param y_axes_label (character) Determine y-axis-label (default = "Correlation Coefficient (r)").
#' @param point_color (character, (Hex color codes)) Determine point color (default = "#5dc688"). 
#' @param bar_color (character, (Hex color codes)) Determine error-bar color (default = "#60A1F7").
#' @param line_color (character, (Hex color codes)) Determine line color (default = "grey").
#' @param bar_width (numeric) Determine bar-width (default = 1). 
#' @param bar_size (numeric) Determine bar-size (default = 1). 
#' @param line_size (numeric) Determine line-size (default = 1).
#' @param line_type (character, either "straight" or "smooth") Determine line-type (default = "straight"). 
#' @param point_size (numeric) Determine points size (default = 1). 
#' @return A plot with correlation coefficient on y-axis and sample size in quantity or percent on x axis. 
#' If number och cross-validations exceed 1, then error bars measuring standard deviations will be plotted. 
#' @section Plot Example: Example of a plot created by textTrainNPlot. 
#' \if{html}{\figure{textTrainNPlot.image.png}{options: width=100\%}}
#' @examples
#' # Plot cross-validated correlation coefficients across different sample-sizes from the object
#' # returned by the textTrainN function.
#' 
#' \dontrun{
#' plot_object <- textTrainNPlot(
#'                     tibble = tibble_to_plot, 
#'                     n_cross_val = 3, 
#'                     x_unit = "quantity"
#' )
#' 
#' # Visualize plot 
#' plot_object
#' }
#' @seealso See \code{\link{textTrainN}}. 
#' @importFrom tibble tibble
#' @export
textTrainNPlot <- function(
    tibble,
    sample_percents = c(25, 50, 75, 100),
    n_cross_val = 1,
    x_unit = "percent", 
    y_range = NULL, 
    title = "Cross-validated correlation coefficients across different sample sizes",
    x_axes_label = "Sample Size (percent)",
    y_axes_label = "Correlation Coefficient (r)",
    point_color = "#5dc688",
    bar_color = "#60A1F7",
    line_color = "grey",
    bar_width = 1,
    bar_size = 0.8, 
    line_size = 0.6, 
    line_type = "straight",
    point_size = 3
) {
  sample_sizes <- tibble$sample_size
  
  if (n_cross_val == 1) {
    # Create the ggplot object and specify aesthetics
    TrainNPlot <- ggplot2::ggplot(data = tibble, aes(x = if (x_unit == "quantity") sample_size else percent, y = mean)) +
      ggplot2::geom_point(color = point_color, size = point_size) +
      ggplot2::geom_errorbar(
        aes(ymin = mean - std, ymax = mean + std),
        size = bar_size, 
        width = bar_width,
        color = bar_color
      ) + 
      # determines whether to plot a smooth or straight line
      (if (line_type == "smooth") {
        ggplot2::geom_smooth(size = line_size, color = line_color, method = "auto")
      } else {
        ggplot2::geom_line(size = line_size, color = line_color)
      }) +
      # set axis-labels 
      ggplot2::labs(
        title = title,
        x = if (x_unit == "quantity") "Sample Size (quantity)" else x_axes_label,
        y = y_axes_label
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        axis.text = ggplot2::element_text(size = 12),
        axis.title = ggplot2::element_text(size = 14),
        plot.title = ggplot2::element_text(size = 16),
        legend.position = "none"
      )
    
    # changes y limit if not set to NULL
    if (!is.null(y_range)){
      TrainNPlot <- TrainNPlot +
        ggplot2::ylim(y_range[1], y_range[2])
    }
    
    # Set x-axis breaks
    if (x_unit == "quantity") {
      TrainNPlot <- TrainNPlot +
        ggplot2::scale_x_continuous(breaks = sample_sizes)
    } else {
      TrainNPlot <- TrainNPlot +
        ggplot2::scale_x_continuous(breaks = sample_percents)
    }
    
    return(TrainNPlot)
  }
  
  if (n_cross_val > 1) {
    # Create the ggplot object and specify aesthetics
    TrainNPlot <- ggplot2::ggplot(data = tibble, aes(x = if (x_unit == "quantity") sample_size else percent, y = mean)) +
      ggplot2::geom_point(color = point_color, size = point_size) +
      ggplot2::geom_errorbar(
        aes(ymin = mean - std, ymax = mean + std),
        size = bar_size, 
        width = bar_width,
        color = bar_color
      ) + 
      # determines whether to plot a smooth or straight line
      (if (line_type == "smooth") {
        ggplot2::geom_smooth(size = line_size, color = line_color, method = "auto")
      } else {
        ggplot2::geom_line(size = line_size, color = line_color)
      }) +
      # set axis-labels 
      ggplot2::labs(
        title = title,
        x = if (x_unit == "quantity") "Sample Size (quantity)" else x_axes_label,
        y = y_axes_label
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        axis.text = ggplot2::element_text(size = 12),
        axis.title = ggplot2::element_text(size = 14),
        plot.title = ggplot2::element_text(size = 16),
        legend.position = "none"
      )
    
    # changes y limit if not set to NULL
    if (!is.null(y_range)){
      TrainNPlot <- TrainNPlot +
        ggplot2::ylim(y_range[1], y_range[2])
    }
    
    # Set x-axis breaks
    if (x_unit == "quantity") {
      TrainNPlot <- TrainNPlot +
        ggplot2::scale_x_continuous(breaks = sample_sizes)
    } else {
      TrainNPlot <- TrainNPlot +
        ggplot2::scale_x_continuous(breaks = sample_percents)
    }
    
    return(TrainNPlot)
  }
}
