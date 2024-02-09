
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

#' (experimental) Compute cross-validated correlations for different sample-sizes of a data set. 
#' The cross-validation process can be repeated several times to enhance the reliability of the evaluation.
#' @param x Word embeddings from textEmbed (or textEmbedLayerAggregation). 
#' If several word embedding are provided in a list they will be concatenated.
#' @param y Numeric variable to predict.
#' @param sample_percents (numeric) Numeric vector that specifies the percentages of the total number of data points to include in each sample (default = c(25,50,75,100), i.e., correlations are evaluated for 25%%,50%%,75%% and 100%% of 
#' the datapoints). The datapoints in each sample are chosen randomly for each new sample.  
#' @param handle_word_embeddings Determine whether to use a list of word embeddings or an individual word_embedding (default = "individually", also "concatenate"). If a list of word embeddings are 
#' provided, then they will be concatenated. 
#' @param n_cross_val (numeric) Value that determines the number of times to repeat the cross-validation.
#' (default = 1, i.e., cross-validation is only performed once). Warning: The training process gets proportionately slower to the number of cross-validations, 
#' resulting in a time complexity that increases with a factor of n (n cross-validations).
#' @param x_append (optional) Variables to be appended after the word embeddings (x); if wanting to preappend them before
#' the word embeddings use the option first = TRUE. If not wanting to train with word embeddings, set x = NULL (default = NULL).
#' @param append_first (boolean) Option to add variables before or after all word embeddings (default = False).
#' @param cv_method (character) Cross-validation method to use within a pipeline of nested outer and inner loops
#' of folds (see nested_cv in rsample). Default is using cv_folds in the outside folds and "validation_split"
#' using rsample::validation_split in the inner loop to achieve a development and assessment set (note that
#' for validation_split the inside_folds should be a proportion, e.g., inside_folds = 3/4); whereas "cv_folds"
#' uses rsample::vfold_cv to achieve n-folds in both the outer and inner loops.
#' @param outside_folds (numeric) Number of folds for the outer folds (default = 10).
#' @param inside_folds (numeric) The proportion of data to be used for modeling/analysis; (default proportion = 3/4).
#' For more information see validation_split in rsample.
#' @param strata (string or tibble; default "y") Variable to stratify according; if a string the variable needs to be in the
#' training set - if you want to stratify according to another variable you can include it as a tibble (please note you
#' can only add 1 variable to stratify according). Can set it to NULL.
#' @param outside_strata (boolean) Whether to stratify the outside folds.
#' @param outside_breaks (numeric) The number of bins wanted to stratify a numeric stratification variable in the
#' outer cross-validation loop (default = 4).
#' @param inside_strata Whether to stratify the outside folds.
#' @param inside_breaks The number of bins wanted to stratify a numeric stratification variable in the inner
#' cross-validation loop (default = 4).
#' @param model Type of model. Default is "regression"; see also "logistic" and "multinomial" for classification.
#' @param eval_measure (character) Type of evaluative measure to select models from. Default = "rmse" for regression and
#' "bal_accuracy" for logistic. For regression use "rsq" or "rmse"; and for classification use "accuracy",
#'  "bal_accuracy", "sens", "spec", "precision", "kappa", "f_measure", or "roc_auc",(for more details see
#'  the yardstick package).
#' @param preprocess_step_center (boolean) Normalizes dimensions to have a mean of zero; default is set to TRUE.
#' For more info see (step_center in recipes).
#' @param preprocess_step_scale (boolean) Normalize dimensions to have a standard deviation of one; default is set to TRUE.
#' For more info see (step_scale in recipes).
#' @param preprocess_PCA Pre-processing threshold for PCA (to skip this step set it to NA).
#' Can select amount of variance to retain (e.g., .90 or as a grid c(0.80, 0.90)); or
#' number of components to select (e.g., 10). Default is "min_halving", which is a function
#' that selects the number of PCA components based on number  of participants and feature (word embedding dimensions)
#' in the data. The formula is:
#' preprocess_PCA = round(max(min(number_features/2), number_participants/2), min(50, number_features))).
#' @param penalty (numeric) Hyper parameter that is tuned (default = 10^seq(-16,16)).
#' @param mixture A number between 0 and 1 (inclusive) that reflects the proportion of L1 regularization
#' (i.e. lasso) in the model (for more information see the linear_reg-function in the parsnip-package).
#' When mixture = 1, it is a pure lasso model while mixture = 0 indicates that ridge regression is being
#' used (specific engines only).
#' @param first_n_predictors By default this setting is turned off (i.e., NA). To use this method,
#' set it to the highest number of predictors you want to test. Then the X first dimensions are used in training,
#' using a sequence from Kjell et al., 2019 paper in Psychological Methods. Adding 1,
#' then multiplying by 1.3 and finally rounding to the nearest integer (e.g., 1, 3, 5, 8).
#' This option is currently only possible for one embedding at the time.
#' @param method_cor Type of correlation used in evaluation (default "pearson";
#' can set to "spearman" or "kendall").
#' @param impute_missing Default FALSE (can be set to TRUE if something else than word_embeddings are trained).
#' @param model_description (character) Text to describe your model (optional; good when sharing the model with others).
#' @param multi_cores If TRUE it enables the use of multiple cores if the computer system allows for it
#'  (i.e., only on unix, not windows). Hence it makes the analyses considerably faster to run. Default is
#'  "multi_cores_sys_default", where it automatically uses TRUE for Mac and Linux and FALSE for Windows.
#' @param save_output (character) Option not to save all output; default = "all". see also "only_results"
#'  and "only_results_predictions".
#' @param simulate.p.value (Boolean) From fisher.test: a logical indicating whether to compute p-values by Monte Carlo simulation,
#' in larger than 2 × 2 tables.
#' @param seed (numeric) Set different seed (default = 2024).
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
#'       n_cross_val = 3
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
    handle_word_embeddings = "individually",
    n_cross_val = 1,
    x_append = NULL,
    append_first = FALSE,
    cv_method = "validation_split",
    outside_folds = 10,
    inside_folds = 3 / 4,
    strata = "y",
    outside_strata = TRUE,
    outside_breaks = 4,
    inside_strata = TRUE,
    inside_breaks = 4,
    model = "regression",
    eval_measure = "default",
    preprocess_step_center = TRUE,
    preprocess_step_scale = TRUE,
    preprocess_PCA = NA,
    penalty = 10^seq(-16, 16),
    mixture = c(0),
    first_n_predictors = NA,
    impute_missing = FALSE,
    method_cor = "pearson",
    model_description = "Consider writing a description of your model here",
    multi_cores = "multi_cores_sys_default",
    save_output = "all",
    simulate.p.value = FALSE,
    seed = 2024
) {
  set.seed(seed)

  # number of tests (i.e., number of samples for each cross-validation)
  n_tests = length(sample_percents)
  
  # number of elements 
  len <- length(y)
  
  # dataframe 
  results_df <- data.frame()
  
  # vector´that will contain the quantity of elements in each sample.  
  sample_sizes <- c()
  
  # Add "percent" column to dataframe 
  for (test in 1:n_tests){
    num_samples <- round((sample_percents[test] / 100) * len)
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
  x_vectors <- list()
  y_vectors <- list()
  
  # Number of cross-validation
  for (check in 1:n_cross_val) {
    # Initialize empty vectors for each cross-validation fold
    x_vectors[[check]] <- list()
    y_vectors[[check]] <- list()
    
    # Adds column names for each cross-validation to columns_to_calculate
    columns_to_calculate <- c(columns_to_calculate, sprintf("Test%s", as.character(check)))
    
    # Performs each cross-validation. 
    for (idx in 1:n_tests) {
      # Trains model with data in each sample (i.e., with elements in each nested list in indexes)
      if (handle_word_embeddings == "individually"){
        trained <- textTrainRegression(
          x = x[indexes[[check]][[idx]],],
          y = y[indexes[[check]][[idx]]],
          x_append = x_append,
          append_first = append_first,
          cv_method = cv_method,
          outside_folds = outside_folds,
          inside_folds = inside_folds,
          strata = strata,
          outside_strata = outside_strata,
          outside_breaks = outside_breaks,
          inside_strata = inside_strata,
          inside_breaks = inside_breaks,
          model = model,
          eval_measure = eval_measure,
          preprocess_step_center = preprocess_step_center,
          preprocess_step_scale = preprocess_step_scale,
          preprocess_PCA = preprocess_PCA,
          penalty = penalty,
          mixture = mixture,
          first_n_predictors = first_n_predictors,
          impute_missing = impute_missing,
          method_cor = method_cor,
          model_description = model_description,
          multi_cores = multi_cores,
          save_output = save_output,
          simulate.p.value = simulate.p.value,
          seed = seed
        )
      }
      if (handle_word_embeddings == "concatenate"){
        trained <- textTrainRegression(
          x = lapply(x, function(x) x[indexes[[check]][[idx]], ]), 
          y = y[indexes[[check]][[idx]]],
          x_append = x_append,
          append_first = append_first,
          cv_method = cv_method,
          outside_folds = outside_folds,
          inside_folds = inside_folds,
          strata = strata,
          outside_strata = outside_strata,
          outside_breaks = outside_breaks,
          inside_strata = inside_strata,
          inside_breaks = inside_breaks,
          model = model,
          eval_measure = eval_measure,
          preprocess_step_center = preprocess_step_center,
          preprocess_step_scale = preprocess_step_scale,
          preprocess_PCA = preprocess_PCA,
          penalty = penalty,
          mixture = mixture,
          first_n_predictors = first_n_predictors,
          impute_missing = impute_missing,
          method_cor = method_cor,
          model_description = model_description,
          multi_cores = multi_cores,
          save_output = save_output,
          simulate.p.value = simulate.p.value,
          seed = seed
        )
      }
      
      # Extract the correlation-coefficient or AUC and assign it to results_df
      if (model == "logistic" || model == "multinomial"){
        value_to_insert <- trained$results[8,3]
      } else {
        value_to_insert <- trained$results[4]
      }
      column_name <- sprintf("Test%s", as.character(check))
      results_df[idx, column_name] <- value_to_insert
    }
  }
  
  # Calculate the mean and standard deviation for each row in results_df. 
  results_df["mean"] <- apply(results_df[columns_to_calculate], 1, mean)
  results_df["std"] <- apply(results_df[columns_to_calculate], 1, sd)
  
  # Convert to dataframe to tibble 
  results_df <- tibble::as_tibble(results_df)
  
  return(results_df)
}

#### textTrainNPlot function #### 

#' (experimental) Plot cross-validated correlation coefficients across different sample-sizes from the object
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
    TrainNPlot <- ggplot2::ggplot(data = tibble, ggplot2::aes(x = if (x_unit == "quantity") sample_size else percent, y = mean)) +
      ggplot2::geom_point(color = point_color, size = point_size) +
      ggplot2::geom_errorbar(
        ggplot2::aes(ymin = mean - std, ymax = mean + std),
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
    TrainNPlot <- ggplot2::ggplot(data = tibble, ggplot2::aes(x = if (x_unit == "quantity") sample_size else percent, y = mean)) +
      ggplot2::geom_point(color = point_color, size = point_size) +
      ggplot2::geom_errorbar(
        ggplot2::aes(ymin = mean - std, ymax = mean + std),
        size = bar_size, 
        width = bar_width,
        color = bar_color
      ) + 
      # determines whether to plot a smooth or straight line
      (if (line_type == "smooth") {
        ggplot2::geom_smooth(linewidth = line_size, color = line_color, method = "auto")
      } else {
        ggplot2::geom_line(linewidth = line_size, color = line_color)
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
