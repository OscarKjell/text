#x <- wordembeddings4_10$harmonywords
#y <- Language_based_assessment_data_8_10$hilstotal
#library(magrittr)
# wordembeddings <- wordembeddings4_10
# ratings_data <- Language_based_assessment_data_8_10
# x <- wordembeddings$harmonytext
# y <- ratings_data$hilstotal
# outside_folds = 10
# outside_strata_y = "y"
# inside_folds = 10
# inside_strata_y = "y"
# preprocess_PCA_thresh = 0.95
# method_cor = "pearson"
# model_description = "Consider writing a description here"
# multi_cores = TRUE


#results <- textTrain(wordembeddings$harmonytext, ratings_data$hilstotal,
#  nrFolds_k = 2, strata_y = NULL

# devtools::document()
#' Train word embeddings to a numeric variable.
#'
#' @param x Word embeddings from textEmbed (or textLayerAggregation).
#' @param y Numeric variable to predict.
# @param outside_folds Number of folds for the outer folds.
#' @param outside_strata_y Variable to stratify according (default y; can set to NULL).
# @param inside_folds Number of folds for the inner folds.
#' @param inside_strata_y Variable to stratify according (default y; can set to NULL).
#' @param preprocess_PCA_thresh Pre-processing threshold for amount of variance to retain (default 0.95).
#' @param penalty hyper parameter that is tuned
#' @param mixture hyper parameter that is tuned default = 0 (hence a pure ridge regression).
#' @param method_cor Type of correlation used in evaluation (default "pearson";
#' can set to "spearman" or "kendall").
#' @param model_description Text to describe your model (optional; good when sharing the model with others).
#' @param multi_cores If TRUE enables the use of multiple cores if computer/system allows for it (hence it can
#' make the analyses considerably faster to run).
#' @return A correlation between predicted and observed values; as well as a tibble of predicted values.
#' @examples
#' wordembeddings <- wordembeddings4_10
#' ratings_data <- Language_based_assessment_data_8_10
#'
#' results <- textTrainRegression(wordembeddings$harmonytext,
#' ratings_data$hilstotal,
#' outside_strata_y = NULL,
#' inside_strata_y = NULL,
#' multi_cores = FALSE #this is FALSE due to CRAN testing.
#' )
#' @seealso see \code{\link{textLayerAggregation}} \code{\link{textTrainLists}}
#' \code{\link{textTrainRandomForest}} \code{\link{textDiff}}
#' @importFrom stats cor.test na.omit
#' @importFrom dplyr select starts_with filter
#' @importFrom recipes recipe step_naomit step_center step_scale step_pca all_predictors
#' @importFrom rsample vfold_cv
#' @importFrom parsnip linear_reg set_engine
#' @importFrom tune tune control_grid tune_grid select_best collect_predictions
#' @importFrom magrittr %>%
#' @importFrom future plan multisession
#' @importFrom furrr future_map
#' @export
textTrainRegression <- function(x,
                                y,
#                                outside_folds = 10, # is commented out due to a bug in rsample; when bug is resolved these will work.
                                outside_strata_y = "y",
#                                inside_folds = 10, # is commented out due to a bug in rsample; when bug is resolved these will work.
                                inside_strata_y = "y",
                                preprocess_PCA_thresh = 0.95,
                                penalty = 10^seq(-16, 16),
                                mixture = c(0),
                                method_cor = "pearson",
                                model_description = "Consider writing a description of your model here",
                                multi_cores = TRUE) {
  set.seed(2020)
  x1 <- dplyr::select(x, dplyr::starts_with("Dim"))
  xy <- cbind(x1, y)

  results_nested_resampling <- rsample::nested_cv(xy,
                                                  outside = rsample::vfold_cv(v = 3, #outside_folds,
                                                                              repeats = 1,
                                                                              strata_y = outside_strata_y),
                                                  inside  = rsample::vfold_cv(v = 3, #inside_folds,
                                                                              repeats = 1,
                                                                              strata_y = inside_strata_y))

  # Function to fit a model and compute RMSE
  # object: an rsplit object (from results_nested_resampling tibble)
  # object = results_nested_resampling$splits[[1]]
  fit_model_rmse <- function(object, penalty = 1, mixture = 0) {

    xy_recipe <- rsample::analysis(object) %>%
      recipes::recipe(y ~ .) %>%
      # recipes::step_BoxCox(all_predictors()) %>%
      recipes::step_naomit(Dim1, skip = TRUE) %>%
      recipes::step_center(recipes::all_predictors()) %>%
      recipes::step_scale(recipes::all_predictors()) %>%
      recipes::step_pca(recipes::all_predictors(), threshold = preprocess_PCA_thresh) %>% #  num_comp = tune()
      recipes::prep()

    # To load the prepared training data into a variable juice() is used.
    # It extracts the data from the xy_recipe object.
    xy_training <- recipes::juice(xy_recipe)

    # Create and fit model; penalty=0.1 mixture = 0
    mod <-
      parsnip::linear_reg(penalty = penalty, mixture = mixture) %>%
      parsnip::set_engine("glmnet") %>%
      parsnip::fit(y ~ ., data = xy_training) #analysis(object)

    #Prepare the test data according to the recipe
    xy_testing <- xy_recipe %>%
      recipes::bake(rsample::assessment(object))
    # look at xy_testing: glimpse(xy_testing)

    # Apply model on new data
    holdout_pred <-
      stats::predict(mod, xy_testing %>% dplyr::select(-y)) %>%
      dplyr::bind_cols(rsample::assessment(object) %>% dplyr::select(y))

    # Get RMSE
    rmse_val <- yardstick::rmse(holdout_pred, truth = y, estimate = .pred)$.estimate
    # Sort output of RMSE, predictions and truth (observed y)
    output <- list(list(rmse_val), list(holdout_pred$.pred), list(holdout_pred$y))
    names(output) <- c("rmse", "predictions", "y")
    output
  }

  # testing
  # fit_model_rmse(object=results_nested_resampling$inner_resamples[[2]]$splits[[1]])

  # In some situations, we want to parameterize the function over the tuning parameter:
  fit_model_rmse_wrapper <- function(penalty, mixture, object) fit_model_rmse(object, penalty, mixture)

  # For the nested resampling, a model needs to be fit for each tuning parameter and each INNER split.
  # object:  an rsplit object from the INNER samples
  # object=results_nested_resampling$inner_resamples[[1]]$splits[[1]]
  tune_over_cost <- function(object) {

    grid_inner <- base::expand.grid(
      penalty = penalty,
      mixture = mixture)

    # Test models with the different hyperparameters for the inner samples
    tune_results <- purrr::map2(grid_inner$penalty,
                                grid_inner$mixture,
                                fit_model_rmse_wrapper,
                                object = object)

    # Sort the output to separate the rmse, predictions and truth
    tune_outputlist <- tune_results %>%
      dplyr::bind_rows() %>%
      split.default(names(.)) %>%
      purrr::map(na.omit)

    # Extract the RMSE
    tune_rmse <- unlist(tune_outputlist$rmse$rmse)

    # Add RMSE to the grid
    grid_inner_RMSE <- grid_inner %>%
      dplyr::mutate(RMSE = tune_rmse)

    grid_inner_RMSE
  }
  # testing
  #tune_over_cost(object=results_nested_resampling$inner_resamples[[1]]$splits[[1]])

  # object = results_nested_resampling$inner_resamples[[1]]
  # Since this will be called across the set of OUTER cross-validation splits, another wrapper is required:
  # object: an `rsplit` object (from results_nested_resampling$inner_resamples)
  summarize_tune_results <- function(object) {

    # Return row-bound tibble containing the INNER results
    purrr::map_df(object$splits, tune_over_cost) %>%

      # For each value of the tuning parameter, compute the help(summarize)
      # average RMSE which is the INNER estimate.
      dplyr::group_by(penalty) %>%
      dplyr::summarize(mixture = mixture,
                       mean_RMSE = mean(RMSE, na.rm = TRUE),
                       n = length(RMSE),
                       .groups = "drop_last")
  }

  if (multi_cores == FALSE){
    tuning_results <- purrr::map(results_nested_resampling$inner_resamples, summarize_tune_results)
  } else if(multi_cores == TRUE) {
    # The multisession plan uses the local cores to process the inner resampling loop. help(multisession)
    #library(future)
    future::plan(multisession)
    # The object tuning_results is a list of data frames for each of the OUTER resamples.
    tuning_results <- furrr::future_map(results_nested_resampling$inner_resamples, summarize_tune_results)
  }

  # Function to get the lowest mean_RMSE
  bestParameters <- function(dat) dat[which.min(dat$mean_RMSE),]

  # Determine the best parameter estimate from each INNER sample to be used
  # for each of the outer resampling iterations:
  hyper_parameter_vals <-
    tuning_results %>%
    purrr::map_df(bestParameters) %>%
    dplyr::select(c(penalty, mixture))

  # Bind best results
  results_split_parameter <-
    dplyr::bind_cols(results_nested_resampling, hyper_parameter_vals)

  # Compute the outer resampling results for each of the
  # splits using the corresponding tuning parameter value from results_split_parameter.
  # fit_model_rmse(results_split_parameter$splits[[1]])
  results_outer <- purrr::pmap(list(object=results_nested_resampling$splits,
                                    results_split_parameter$penalty,
                                    results_split_parameter$mixture),
                               fit_model_rmse)

  # Separate RMSE, predictions and observed y
  outputlist_results_outer <- results_outer %>%
    dplyr::bind_rows() %>%
    split.default(names(.)) %>%
    purrr::map(na.omit)

  # Unnest predictions and y
  predy_y <- tibble::tibble(tidyr::unnest(outputlist_results_outer$predictions, cols = c(predictions)),
                            tidyr::unnest(outputlist_results_outer$y, cols = c(y)))

  # Correlate predictions and observed
  correlation <- stats::cor.test(predy_y$predictions, predy_y$y, method = method_cor)

  # Construct final model to be saved and applied on other data
  final_predictive_model <-
    parsnip::linear_reg(penalty = mean(results_split_parameter$penalty), mixture = mean(results_split_parameter$mixture)) %>%
    parsnip::set_engine("glmnet") %>%
    parsnip::fit(y ~ ., data = xy)

  # Describe model; adding user's-description + the name of the x and y
  # model_description_detail <- c(model_description, paste(names(x)), paste(names(y)))
  model_description_detail <- c(deparse(substitute(x)), deparse(substitute(y)), model_description)


  final_results <- list(predy_y, final_predictive_model, model_description_detail, correlation)
  final_results
  names(final_results) <- c("predictions", "final_model", "model_description", "correlation")
  final_results
}
########
# textTrainRegression
