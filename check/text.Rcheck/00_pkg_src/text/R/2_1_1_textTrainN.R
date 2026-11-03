##### indexing function ####

#' Creates a nested list of element indexes. These elements will be used in the training process of textTrainN.
#' @param n_cross_val Number of cross-validations.
#' @param sample_percents Numeric vector of sample percent sizes, e.g. (25,50,75,100).
#' @param len Length of the dataset (i.e, number of datapoints).
#' @param seed Set seed.
#' @return Nested list of elements.
#' @noRd
indexing_random <- function(n_cross_val = 1,
                     sample_percents,
                     len,
                     seed) {
  nested_list <- list()
  n_tests <- length(sample_percents)
  set.seed(seed)

  for (check in 1:n_cross_val) {
    # Create a new sub_list for each check
    sub_list <- list()
    for (idx in 1:n_tests) {
      # Calculate sample size as a percentage of the total number of data points
      sample_size <- round(sample_percents[idx] / 100 * len,
                           0)

      # Check for a minimum sample size due to K-fold cross-validation in textTrain function.
      if (sample_size < 10) {
        error_msg <- paste("The number of data points must exceed 10 due to K-fold cross-validation in textTrain")
        stop(error_msg)
      }

      # index is a numeric vector with random indexes of elements to train.
      index <- sample(1:len,
                      size = sample_size,
                      replace = FALSE)
      index <- list(index)

      sub_list <- append(sub_list, index)
    }
    # Add sub_list to nested_list
    nested_list <- append(nested_list, list(sub_list))
  }
  return(nested_list)
}

#' Creates a nested list of element indexes. These elements will be used in the
#' training process of textTrainN. Here we use a subset of cases, so that each larger batch
#' of indexes comprise the smaller ones.
#' @param n_cross_val Number of cross-validations.
#' @param sample_percents Numeric vector of sample percent sizes, e.g. (25,50,75,100).
#' @param len Length of the dataset (i.e, number of datapoints).
#' @param seed Set seed.
#' @return Nested list of elements.
#' @noRd
indexing_subsets_no100 <- function(
    n_cross_val = 1,
    sample_percents,
    len,
    seed) {

  nested_list <- list()
  set.seed(seed)

  # Remove 100% from sample_percents to determine the max base percentage
  sample_percents_no_100 <- sample_percents[sample_percents < 100]

  if(length(sample_percents_no_100) == 0){
    stop("No valid percentages less than 100 provided")
  }

  max_percent <- max(sample_percents_no_100)
  max_sample_size <- round(max_percent / 100 * len, 0)

  for (check in 1:n_cross_val) {
    sub_list <- list()

    # Generate the max percentage sample and shuffle it
    resample_max <- sample(1:len,
                           size = max_sample_size,
                           replace = FALSE)

    shuffled_resample_max <- sample(resample_max)

    for (percent in sort(sample_percents_no_100, decreasing = FALSE)) {
      # Calculate the sample size for the current percentage
      sample_size <- round(percent / 100 * len, 0)

      # Check for a minimum sample size due to K-fold cross-validation in textTrain function.
      if (sample_size < 10) {
        error_msg <- paste("The number of data points must exceed 10 due to K-fold cross-validation in textTrain")
        stop(error_msg)
      }

      # Take the first 'sample_size' elements from the shuffled max percentage sample
      index <- shuffled_resample_max[1:sample_size]
      sub_list <- append(sub_list, list(index))
    }

    nested_list <- append(nested_list, list(sub_list))
  }

  return(nested_list)
}

#### textTrainN function ####
#' Cross-validated accuracies across sample-sizes
#'
#' textTrainN() computes cross-validated correlations for different sample-sizes of a data set.
#' The cross-validation process can be repeated several times to enhance the reliability of the evaluation.
#' @param x Word embeddings from textEmbed (or textEmbedLayerAggregation).
#' If several word embedding are provided in a list they will be concatenated.
#' @param y Numeric variable to predict.
#' @param sample_percents (numeric) Numeric vector that specifies the percentages of the total number of
#' data points to include in each sample (default = c(25,50,75,100), i.e., correlations are evaluated
#' for 25%%,50%%,75%% and 100%% of the datapoints). The datapoints in each sample are chosen randomly for
#' each new sample.
#' @param handle_word_embeddings Determine whether to use a list of word embeddings or an individual
#' word_embedding (default = "individually", also "concatenate"). If a list of word embeddings are
#' provided, then they will be concatenated.
#' @param n_cross_val (numeric) Value that determines the number of times to repeat the cross-validation (i.e., number of tests).
#' (default = 1, i.e., cross-validation is only performed once). Warning: The training process gets
#' proportionately slower to the number of cross-validations, resulting in a time complexity that increases
#' with a factor of n (n cross-validations).
#' @param sampling_strategy Sample a "random" sample for each subset from all data or sample a "subset" from the
#' larger subsets (i.e., each subset contain the same data).
#' @param use_same_penalty_mixture If TRUE it only searches the penalty and mixture search grid once, and then use the same
#' thereafter; if FALSE, it searches the grid every time.
# @param x_append (optional) Variables to be appended after the word embeddings (x); if wanting to preappend
# them before the word embeddings use the option first = TRUE. If not wanting to train with word embeddings,
# set x = NULL (default = NULL).
# @param append_first (boolean) Option to add variables before or after all word embeddings (default = False).
# @param cv_method (character) Cross-validation method to use within a pipeline of nested outer and inner loops
# of folds (see nested_cv in rsample). Default is using cv_folds in the outside folds and "validation_split"
# using rsample::validation_split in the inner loop to achieve a development and assessment set (note that
# for validation_split the inside_folds should be a proportion, e.g., inside_folds = 3/4); whereas "cv_folds"
# uses rsample::vfold_cv to achieve n-folds in both the outer and inner loops.
# @param outside_folds (numeric) Number of folds for the outer folds (default = 10).
# @param inside_folds (numeric) The proportion of data to be used for modeling/analysis; (default proportion = 3/4).
# For more information see validation_split in rsample.
# @param strata (string or tibble; default "y") Variable to stratify according; if a string the variable
# needs to be in the training set - if you want to stratify according to another variable you can include
# it as a tibble (please note you can only add 1 variable to stratify according). Can set it to NULL.
# @param outside_strata (boolean) Whether to stratify the outside folds.
# @param outside_breaks (numeric) The number of bins wanted to stratify a numeric stratification variable in the
# outer cross-validation loop (default = 4).
# @param inside_strata Whether to stratify the outside folds.
# @param inside_breaks The number of bins wanted to stratify a numeric stratification variable in the inner
# cross-validation loop (default = 4).
#' @param model Type of model. Default is "regression"; see also "logistic" and "multinomial" for classification.
# @param eval_measure (character) Type of evaluative measure to select models from. Default = "rmse" for regression and
# "bal_accuracy" for logistic. For regression use "rsq" or "rmse"; and for classification use "accuracy",
#  "bal_accuracy", "sens", "spec", "precision", "kappa", "f_measure", or "roc_auc",(for more details see
#  the yardstick package).
# @param preprocess_step_center (boolean) Normalizes dimensions to have a mean of zero; default is set to TRUE.
# For more info see (step_center in recipes).
# @param preprocess_step_scale (boolean) Normalize dimensions to have a standard deviation of one;
# default is set to TRUE. For more info see (step_scale in recipes).
# @param preprocess_PCA Pre-processing threshold for PCA (to skip this step set it to NA).
# Can select amount of variance to retain (e.g., .90 or as a grid c(0.80, 0.90)); or
# number of components to select (e.g., 10). Default is "min_halving", which is a function
# that selects the number of PCA components based on number  of participants and feature (word embedding dimensions)
# in the data. The formula is:
# preprocess_PCA = round(max(min(number_features/2), number_participants/2), min(50, number_features))).
#' @param penalty (numeric) Hyper parameter that is tuned (default = 10^seq(-16,16)).
#' @param mixture A number between 0 and 1 (inclusive) that reflects the proportion of L1 regularization
#' (i.e. lasso) in the model (for more information see the linear_reg-function in the parsnip-package).
#' When mixture = 1, it is a pure lasso model while mixture = 0 indicates that ridge regression is being
#' used (specific engines only).
# @param first_n_predictors By default this setting is turned off (i.e., NA). To use this method,
# set it to the highest number of predictors you want to test. Then the X first dimensions are used in training,
# using a sequence from Kjell et al., 2019 paper in Psychological Methods. Adding 1,
# then multiplying by 1.3 and finally rounding to the nearest integer (e.g., 1, 3, 5, 8).
# This option is currently only possible for one embedding at the time.
# @param method_cor Type of correlation used in evaluation (default "pearson";
# can set to "spearman" or "kendall").
# @param impute_missing Default FALSE (can be set to TRUE if something else than word_embeddings are trained).
# @param model_description (character) Text to describe your model (optional; good when sharing the model with others).
# @param multi_cores If TRUE it enables the use of multiple cores if the computer system allows for it
#  (i.e., only on unix, not windows). Hence it makes the analyses considerably faster to run. Default is
#  "multi_cores_sys_default", where it automatically uses TRUE for Mac and Linux and FALSE for Windows.
# @param save_output (character) Option not to save all output; default = "all". see also "only_results"
#  and "only_results_predictions".
# @param simulate.p.value (Boolean) From fisher.test: a logical indicating whether to compute p-values
# by Monte Carlo simulation, in larger than 2 × 2 tables.
#' @param seed (numeric) Set different seed (default = 2024).
#' @param ... Additional parameters from textTrainRegression.
#' @return A tibble containing correlations for each sample. If n_cross_val > 1, correlations for
#'  each new cross-validation, along with standard-deviation, mean and standard error of correlation is included in the
#'  tibble. The information in the tibble is visualised via the textTrainNPlot function.
#' @examples
#' # Compute correlations for 25%, 50%, 75% and 100% of the data in word_embeddings and perform
#' # cross-validation thrice.
#'
#' \dontrun{
#' tibble_to_plot <- textTrainN(
#'   x = word_embeddings_4$texts$harmonytext,
#'   y = Language_based_assessment_data_8$hilstotal,
#'   sample_percents = c(25, 50, 75, 100),
#'   n_cross_val = 3
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
    x,
    y,
    sample_percents = c(25, 50, 75, 100),
    handle_word_embeddings = "individually",
    n_cross_val = 1,
    sampling_strategy = "subsets",
    use_same_penalty_mixture = TRUE,
    model = "regression",
    penalty = 10^seq(-16, 16),
    mixture = c(0),
    seed = 2024,
    ...) {

  set.seed(seed)

  # number of tests (i.e., number of samples for each cross-validation)
  n_tests <- length(sample_percents)

  # number of elements
  len <- length(y)

  # dataframe
  results_df <- data.frame()

  # vector that will contain the quantity of cases in each sample.
  sample_sizes <- c()

  # Add "percent" column to data frame
  for (test in 1:n_tests) {
    num_samples <- round((sample_percents[test] / 100) * len)
    results_df[test, "percent"] <- sample_percents[test]
    sample_sizes <- c(sample_sizes, num_samples)
  }

  # Add sample_size column to data frame
  results_df["sample_size"] <- sample_sizes

  # Set seed
  set.seed(seed)

  # Call the indexing function to generate indexes
  if(sampling_strategy == "random"){
    indexes <- indexing_random(
      n_cross_val,
      sample_percents,
      len,
      seed)
  }

  if(sampling_strategy == "subsets"){
    indexes <- indexing_subsets_no100(
      n_cross_val,
      sample_percents,
      len,
      seed
      )

    if(max(sample_percents) == 100){
      n_tests = n_tests-1
    }
  }
  # Vector containing column names for each cross-validation. E.g., if n_cross_val = 3,
  # columns_to_calculate will be "Test1", "Test2", "Test3"
  columns_to_calculate <- c()

  ## Training & Cross-Validation ##
  x_vectors <- list()
  y_vectors <- list()
  check_time_0 <- Sys.time()

  # Number of cross-validation
  for (check in 1:n_cross_val) {
    check_time_1 <- Sys.time()

    # Initialize empty vectors for each cross-validation fold
    x_vectors[[check]] <- list()
    y_vectors[[check]] <- list()

    # Adds column names for each cross-validation to columns_to_calculate
    columns_to_calculate <- c(columns_to_calculate,
                              sprintf("Test%s", as.character(check)))

    # Performs each cross-validation.
    saving_models <- list()

    # idx = 1
    for (idx in 1:n_tests) {

      if(check == 1){
        penalty_change <- penalty
        mixture_change <- mixture
      }

      if(check > 1 & use_same_penalty_mixture == TRUE) {
        penalty_change <- results_df[idx, "penalty"]
        mixture_change <- results_df[idx, "mixture"]
      }

      # Trains model with data in each sample (i.e., with elements in each nested list in indexes)
      if (handle_word_embeddings == "individually") {
        trained <- textTrainRegression(
          x = x[indexes[[check]][[idx]], ],
          y = y[indexes[[check]][[idx]]],
          penalty = penalty_change,
          mixture = mixture_change,
          seed = seed,
          ...
        )
      }
      if (handle_word_embeddings == "concatenate") {
        trained <- textTrainRegression(
          x = lapply(x, function(x) x[indexes[[check]][[idx]], ]),
          y = y[indexes[[check]][[idx]]],
          penalty = penalty_change,
          mixture = mixture_change,
          seed = seed,
          ...
        )
      }
      saving_models[check][[idx]] <- trained


      message_check <- paste("check:", check)
      message_idx <- paste("idx:", idx)

      message(colourise(message_check, "blue"))
      message(colourise(message_idx, "blue"))

      # Extract the correlation-coefficient or AUC and assign it to results_df
      if (model == "logistic" || model == "multinomial") {
        value_to_insert <- trained$results[8, 3]
      } else {
        value_to_insert <- trained$results[4]
      }
      column_name <- sprintf("Test%s", as.character(check))
      results_df[idx, column_name] <- value_to_insert

      # Save the penalty and mixture for forthcoming rounds
      # (to avoid searching the entire grid over and over again)
          penalty_change <- extract_comment(comment = trained$model_description,
                                            part = "penalty_in_final_model")

          results_df[idx, "penalty"] <- penalty_change

          mixture_change <- extract_comment(comment = trained$model_description,
                                                    part = "mixture_in_final_model")
          results_df[idx, "mixture"] <- mixture_change

        check_time_2 <- Sys.time()
        check_time_start <- check_time_2 - check_time_0
        check_time <- check_time_2 - check_time_1
        # Print progress

        message(colourise(results_df, "blue"))
        mes2 <- paste0("Computing time from the start: ",
                     round(check_time_start, 2),
                     units(check_time_start))

        message(colourise(mes2, "blue"))
        mes3 <- paste0("Computing time since starting the current test column: ",
                     round(check_time, 2),
                     units(check_time))
        message(colourise(mes3, "blue"))
      }

    }
  # Running the entire sample (this is outside the loop so that it is not run multiple times)
  if(max(sample_percents) == 100){

      penalty_change <- penalty
      mixture_change <- mixture

    # Trains model with data in each sample (i.e., with elements in each nested list in indexes)
    if (handle_word_embeddings == "individually") {
      trained <- textTrainRegression(
        x = x,
        y = y,
        penalty = penalty_change,
        mixture = mixture_change,
        seed = seed,
        ...
      )
    }

    if (handle_word_embeddings == "concatenate") {
      trained <- textTrainRegression(
        x = lapply(x, function(x) x),
        y = y,
        penalty = penalty_change,
        mixture = mixture_change,
        seed = seed,
        ...
      )
    }
     saving_models[check+1][[1]] <- trained

    # Extract the correlation-coefficient or AUC and assign it to results_df
    if (model == "logistic" || model == "multinomial") {
      value_to_insert <- trained$results[8, 3]
    } else {
      value_to_insert <- trained$results[4]
    }
    #column_name <- sprintf("Test%s", as.character(check))
    results_df[is.na(results_df)] <- as.numeric(value_to_insert)

    # Save the penalty and mixture for forthcoming rounds
    penalty_change <- extract_comment(
      comment = trained$model_description,
      part = "penalty_in_final_model")

    results_df[nrow(results_df), "penalty"] <- penalty_change

    mixture_change <- extract_comment(
      comment = trained$model_description,
      part = "mixture_in_final_model")

    results_df[nrow(results_df), "mixture"] <- mixture_change
    }

  # Calculate the mean and standard deviation for each row in results_df.
  #results_df1 <- tibble::as_tibble(results_df[1:3, columns_to_calculate])

  results_df["mean"] <- apply(
    results_df[columns_to_calculate], 1, mean)

  results_df["std"] <- apply(
    results_df[columns_to_calculate], 1, sd)

  # std-err (std/sqrt(resamples))
  results_df["std_err"] <- results_df["std"]/sqrt(results_df["sample_size"])

  # Convert to data frame to tibble
  results_df <- tibble::as_tibble(results_df)

  # Print progress
  message(colourise(results_df, "blue"))

  check_time_3 <- Sys.time()
  check_time_full <- check_time_3 - check_time_0

  mes4 <- message("The full computing time: ",
               round(check_time_full, 2),
               units(check_time_full))
  message(colourise(mes4, "blue"))

  return(list(models = saving_models,
              results = results_df))
}


#### textTrainNPlot function ####

#' Plot cross-validated accuracies across sample sizes
#'
#' textTrainNPlot() plots cross-validated correlation coefficients across different
#' sample-sizes from the object returned by the textTrainN function. If the number
#' of cross-validations exceed one, then error-bars will be included in the plot.
#' @param results_data (list) One or several objects returned by the function textTrainN
#' as a list (e.g, list(object1, object2)). Also, if several models are provided,
#' then one can add a vector c() with settings (i.e the parameters below) for each model
#' (make sure to add the settings in the order as the models are ordered,
#' if you look to keep the original settings then write "").
#' @param breaks (numeric) Vector containing the percents of the total number of data points that is
#' included in each sample (default = NULL, which takes the breaks from the percentages).
#' If several models are provided, then one can add a vector c() with settings for each model
#' (make sure to add the settings in the order as the models are ordered).
#' @param x_unit (character, "percent" or "quantity") Determines whether the x-axis-values should represent
#'  the number of elements in each sample, or the number of percent of the total data they represent
#'  (default = "percent").
#' @param y_range (numeric) Optional. Determines the y_range. E.g, y_range = c(1,2) sets the y_range from
#' 1 to 2 (default = NULL).
#' @param title (character) Determine plot title (default = "Cross-validated correlation coefficients
#'  across different sample sizes").
#' @param x_axes_label (character) Determine x-axis-label (default = "Sample Size (percent)").
#' @param y_axes_label (character) Determine y-axis-label (default = "Correlation Coefficient (r)").
#' @param point_color (character, (Hex color codes)) Determine point color (default = "#5dc688"). Can set a vector
#' if several results_data are provided.
#' @param error_bar Default "std_err"; see also "std", NULL. Can set a vector
#' if several results_data are provided.
#' @param bar_color (character, (Hex color codes)) Determine error-bar color (default = "#60A1F7"). Can set a vector
#' if several results_data are provided.
#' @param line_color (character, (Hex color codes)) Determine line color (default = "grey"). Can set a vector
#' if several results_data are provided.
#' @param bar_width (numeric) Determine bar-width (default = 1). Can set a vector
#' if several results_data are provided.
#' @param bar_size (numeric) Determine bar-size (default = 1). Can set a vector
#' if several results_data are provided.
#' @param line_size (numeric) Determine line-size (default = 1). Can set a vector
#' if several results_data are provided.
#' @param line_type (character, either "straight" or "smooth") Determine line-type (default = "straight").
#' Can set a vector if several results_data are provided.
#' @param point_size (numeric) Determine points size (default = 1). Can set a vector if several results_data are provided.
#' @param log_transform_x (boolean) Determine wether to log-transform x in case of displaying number of samples
#' (default = FALSE).
#' @return A plot with correlation coefficient on y-axis and sample size in quantity or percent on x axis.
#' If number och cross-validations exceed 1, then error bars measuring standard deviations will be plotted.
#' @examples
#' # Plot cross-validated correlation coefficients across different sample-sizes from the object
#' # returned by the textTrainN function.
#'
#' \dontrun{
#' # Plot the performance of a single model across different sample sizes
#' plot_object1 <- textTrainNPlot(
#'   train_data = tibble_to_plot,
#'   n_cross_val = 3,
#'   x_unit = "quantity"
#' )
#'
#' # Visualize plot
#' plot_object1
#'
#' # Plot the performance of several models across different sample sizes.
#' plot_object2 <- textTrainNPlot(train_data = list(object1, object2, object3),
#'                                n_cross_val = c(2,1,1),
#'                                line_color = c("","","#0000FF")) # "" gives the default settings.
#' # Visualize plot
#' plot_object2
#' }
#' @seealso See \code{\link{textTrainN}}.
#' @importFrom tibble tibble is_tibble
#' @importFrom ggplot2 ggplot geom_point
#' @importFrom purrr map
#' @export
textTrainNPlot <- function(
    results_data,
    breaks = NULL,
    x_unit = "percent",
    y_range = NULL,
    title = "Cross-validated correlation coefficients across sample sizes",
    x_axes_label = "Sample Size (percent)",
    y_axes_label = "Correlation Coefficient (r)",
    point_color = "#5dc688",
    error_bar = "std_err",
    bar_color = "#60A1F7",
    line_color = "grey",
    bar_width = 1,
    bar_size = 0.8,
    line_size = 0.6,
    line_type = "straight",
    point_size = 3,
    log_transform_x = FALSE) {

  # ensure input is in list form
  if(tibble::is_tibble(results_data)){
    results_data <- list(results_data)
  }

  if(is.null(breaks)){
    breaks <- results_data[[1]]$percent
  }


  if(length(point_color) == 1){
    point_color = rep(point_color, length(results_data))
  }
  if(length(bar_color) == 1){
    bar_color = rep(bar_color, length(results_data))
  }
  if(length(line_color) == 1){
    line_color = rep(line_color, length(results_data))
  }
  if(length(bar_width) == 1){
    bar_width = rep(bar_width, length(results_data))
  }
  if(length(bar_size) == 1){
    bar_size = rep(bar_size, length(results_data))
  }
  if(length(line_size) == 1){
    line_size = rep(line_size, length(results_data))
  }
  if(length(line_type) == 1){
    line_type = rep(line_type, length(results_data))
  }
  if(length(point_size) == 1){
    point_size = rep(point_size, length(results_data))
  }


  if(isTRUE(log_transform_x)){
    results_data <- lapply(results_data, function(df) {
      df$log_sample_size <- round(log(df$sample_size),2)
      return(df)
    })
  }

  # replace empty strings with the default color
  default_color <- "grey"
  line_color <- sapply(line_color,
                       function(color) if(color == "") default_color
                       else color)


  # Determine x mapping based on conditions
  if (x_unit == "quantity" && !log_transform_x) {
    x_mapping <- "sample_size"
  } else if (x_unit == "quantity" && log_transform_x) {
    x_mapping <- "log_sample_size"
  } else {
    x_mapping <- "percent"
  }

  # Determine ymin and ymax mapping based on conditions
  if(error_bar == "std" | error_bar ==  "std_err"){

    # Apply transformations over the list of tibbles
    results_data <- purrr::map(
      results_data, ~ .x %>%
        mutate(
          ymin = if (error_bar == "std") {
            mean - std
            } else if (error_bar == "std_err") {
              mean - std_err
              } else {
                stop("Invalid error_bar value")
                },
          ymax = if (error_bar == "std") {
            mean + std
            } else if (error_bar == "std_err") {
              mean + std_err
              } else {
                stop("Invalid error_bar value")
                }
          )
    )
  }

  # initialize the ggplot object
  TrainNPlot <- ggplot2::ggplot()

  # iterate over each model i = 1 i = 2
  for (i in seq_along(results_data)) {
    results_data_i <- results_data[[i]]

    TrainNPlot <- TrainNPlot +
      ggplot2::geom_point(
        data = results_data_i,
        mapping = ggplot2::aes(x = if (x_unit == "quantity" && isFALSE(log_transform_x)) {
          sample_size
        } else if (x_unit == "quantity" && isTRUE(log_transform_x)) {
          log_sample_size
        } else {
          percent
        },
        y = mean),
        color = point_color[i], size = point_size[i])

    # add error bars if n_cross_val > 1 for the current tibble
    if (error_bar == "std" | error_bar ==  "std_err") {

      # Add geom_errorbar to the plot using tidy evaluation
      TrainNPlot <- TrainNPlot +
      #  ggplot2::ggplot() +
        ggplot2::geom_errorbar(
          data = results_data_i,
          mapping = ggplot2::aes(
            x = !!sym(x_mapping),
            ymin = ymin,
            ymax = ymax
          ),
          width = bar_width[i],
          color = bar_color[i],
          linewidth = bar_size[i]
        )
    }


    if (line_type[i] == "smooth") {
      TrainNPlot <- TrainNPlot +
        ggplot2::geom_smooth(
          data = results_data_i,
          mapping = ggplot2::aes(x = if (x_unit == "quantity" && isFALSE(log_transform_x)) {
            sample_size
          } else if (x_unit == "quantity" && isTRUE(log_transform_x)) {
            log_sample_size
          } else {
            percent
          },
          y = mean),
          linewidth = line_size[i],
          color = line_color[i],
          method = "auto")
    } else {
      TrainNPlot <- TrainNPlot +
        ggplot2::geom_line(
          data = results_data_i,
          mapping = ggplot2::aes(x = if (x_unit == "quantity" && isFALSE(log_transform_x)) {
            sample_size
          } else if (x_unit == "quantity" && isTRUE(log_transform_x)) {
            log_sample_size
          } else {
            percent
          },
          y = mean),
          linewidth = line_size[i], color = line_color[i])
    }
  }



  # (title, axes labels, theme)
  TrainNPlot <- TrainNPlot +
    ggplot2::labs(title = title, x = x_axes_label, y = y_axes_label) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text = ggplot2::element_text(size = 12),
                   axis.title = ggplot2::element_text(size = 14),
                   plot.title = ggplot2::element_text(size = 16),
                   legend.position = "none")

  #  y-axis limits and x-axis breaks
  if (!is.null(y_range)) {
    TrainNPlot <- TrainNPlot +
      ggplot2::ylim(y_range[1], y_range[2])

  }
  if (x_unit == "quantity" && isFALSE(log_transform_x)) {
    TrainNPlot <- TrainNPlot +
      ggplot2::scale_x_continuous(
        breaks = unique(unlist(lapply(results_data,
                                      function(t) t$sample_size))))

  } else if (x_unit == "quantity" && isTRUE(log_transform_x)) {
    TrainNPlot <- TrainNPlot +
      ggplot2::scale_x_continuous(
        breaks = unique(unlist(lapply(results_data,
                                      function(t) t$log_sample_size)))) +
      theme(axis.text.x = element_text(angle = 60, hjust = 1))

  } else {
    TrainNPlot <- TrainNPlot + ggplot2::scale_x_continuous(
      breaks = breaks)

  }

  return(TrainNPlot)
}

