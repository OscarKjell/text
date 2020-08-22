#library(tidyverse)
#library(text)
#wordembeddings <- wordembeddings4
#ratings_data <- Language_based_assessment_data_8
#####
#results_low <- textTrainRegression(wordembeddings$harmonytext,
#                               ratings_data$hilstotal,
#                               penalty = c(1, 100),
#                               mixture = c(0),
#                               preprocess_PCA = c(0.4, 0.5),
#                               outside_strata_y = NULL,
#                               inside_strata_y = NULL,
#                               multi_cores = FALSE #this is FALSE due to CRAN testing.
#)

#' Function to find the mode
#' @param x vector with numbers
#' @return  Mode value
#' @noRd
statisticalMode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

#  devtools::document()
#' Function to fit a model and compute RMSE.
#'
#' @param object An rsplit object (from results_nested_resampling tibble)
#' object = results_nested_resampling$splits[[1]]
#' @param penalty hyperparameter for ridge regression.
#' @param mixture hyperparameter for ridge regression.
#' @param preprocess_PCA threshold for pca; preprocess_PCA = NA
#' @return  RMSE.
#' @noRd
fit_model_rmse <- function(object, penalty = 1, mixture = 0, preprocess_PCA = 1) {

  xy_recipe <- rsample::analysis(object) %>%
    recipes::recipe(y ~ .) %>%
    # recipes::step_BoxCox(all_predictors()) %>%  preprocess_PCA = NULL, preprocess_PCA = 0.9 preprocess_PCA = 2
    recipes::step_naomit(Dim1, skip = TRUE) %>%
    recipes::step_center(recipes::all_predictors()) %>%
    recipes::step_scale(recipes::all_predictors()) %>%
    # If preprocess_PCA is not NULL add PCA step with number of component of % of variance to retain specification
    {if(!is.na(preprocess_PCA))
      {if(preprocess_PCA >= 1) recipes::step_pca(., recipes::all_predictors(), num_comp = preprocess_PCA)
      else if(preprocess_PCA < 1) recipes::step_pca(., recipes::all_predictors(), threshold = preprocess_PCA)
      else . } else .} %>%
    recipes::prep()

  # To load the prepared training data into a variable juice() is used.
  # It extracts the data from the xy_recipe object.
  xy_training <- recipes::juice(xy_recipe)

  # Ridge and/or Lasso
  if(length(xy_training)>2){
  # Create and fit model; penalty=NULL mixture = NULL
  mod <-
    parsnip::linear_reg(penalty = penalty, mixture = mixture) %>%
    parsnip::set_engine("glmnet") %>%
    parsnip::fit(y ~ ., data = xy_training)

  # Standard regression
  } else if (length(xy_training) == 2) {
    mod <-
      parsnip::linear_reg(mode = "regression") %>%
      parsnip::set_engine("lm") %>%
      parsnip::fit(y ~ ., data = xy_training) #analysis(object)
  }
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
  output <- list(list(rmse_val), list(holdout_pred$.pred), list(holdout_pred$y), list(preprocess_PCA))
  names(output) <- c("rmse", "predictions", "y", "preprocess_PCA")
  output
}

#' In some situations, we want to parameterize the function over the tuning parameter:
#' Function to fit a model and compute RMSE.
#'
#' @param object An rsplit object (from results_nested_resampling tibble)
#' object = results_nested_resampling$splits[[1]]
#' @param penalty hyperparameter for ridge regression.
#' @param mixture hyperparameter for ridge regression.
#' @param preprocess_PCA threshold for pca
#' @return RMSE.
#' @noRd
fit_model_rmse_wrapper <- function(penalty=penalty, mixture=mixture, object, preprocess_PCA = preprocess_PCA) fit_model_rmse(object, penalty, mixture, preprocess_PCA = preprocess_PCA)

#' For the nested resampling, a model needs to be fit for each tuning parameter and each INNER split.
#'
#' @param object an rsplit object from the INNER samples
#' object=results_nested_resampling$inner_resamples[[1]]$splits[[1]]
#' @param penalty hyperparameter for ridge regression.
#' @param mixture hyperparameter for ridge regression.
#' @param preprocess_PCA threshold for pca
#' @return RMSE.
#' @noRd
tune_over_cost <- function(object, penalty, mixture, preprocess_PCA = preprocess_PCA) {

  # Number of components or percent of variance to attain; min_halving; preprocess_PCA = NULL
  if(!is.na(preprocess_PCA[1])){

    if(preprocess_PCA[1] == "min_halving"){
      num_features = length(rsample::analysis(object)) - 1
      num_users = nrow(rsample::analysis(object))
      preprocess_PCA_value = round(max(min(num_features/2, num_users/1.5), min(50, num_features)))
      preprocess_PCA_value
    } else if(preprocess_PCA[1] >= 1){
      preprocess_PCA_value <- preprocess_PCA
    } else if (preprocess_PCA[1] < 1){
      preprocess_PCA_value <- preprocess_PCA
    }else {
    preprocess_PCA_value = NA
    }
  }

  if(is.na(preprocess_PCA[1])){
    preprocess_PCA_value = NA
  }

  grid_inner <- base::expand.grid(
  penalty = penalty,
  mixture = mixture,
  preprocess_PCA = preprocess_PCA_value)

  # Test models with the different hyperparameters for the inner samples help(map2)
  tune_results <- purrr::pmap(list(grid_inner$penalty,
                              grid_inner$mixture,
                              grid_inner$preprocess_PCA),
                              fit_model_rmse_wrapper,
                              object = object
                              )

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
# dev <- tune_over_cost(object=results_nested_resampling$inner_resamples[[1]]$splits[[1]])
# dev
# n_cross2 <- tune_over_cost(object=results_nested_resampling$inner_resamples[[1]]$splits[[1]])
# n_cross2


#' # Since this will be called across the set of OUTER cross-validation splits, another wrapper is required:
#'
#' @param object An rsplit object from the INNER samples
#' object = results_nested_resampling$inner_resamples[[1]]
#' @param penalty hyperparameter for ridge regression.
#' @param mixture hyperparameter for ridge regression.
#' @param preprocess_PCA threshold for pca
#' @return RMSE with corresponding penalty, mixture and preprocess_PCA.
#' @noRd
summarize_tune_results <- function(object, penalty, mixture, preprocess_PCA = preprocess_PCA) {

  # Return row-bound tibble containing the INNER results
  purrr::map_df(.x = object$splits, .f = tune_over_cost,
                penalty = penalty, mixture = mixture, preprocess_PCA = preprocess_PCA) #%>%

    # For each value of the tuning parameter, compute the
    # average RMSE which is the INNER estimate.
#   dplyr::group_by(penalty) %>%
#   dplyr::summarize(mixture = mixture,
#                    preprocess_PCA = preprocess_PCA,
#                    #mean_RMSE = mean(RMSE, na.rm = TRUE),
#                    RMSE = RMSE,
#                    n = length(RMSE),
#                    .groups = "drop_last")
}


#x <- wordembeddings4$harmonytext
#y <- Language_based_assessment_data_8$hilstotal
##                                outside_folds = 10, # is commented out due to a bug in rsample; when bug is resolved these will work.
#outside_strata_y = NULL
##                                inside_folds = 10, # is commented out due to a bug in rsample; when bug is resolved these will work.
#inside_strata_y = NULL
#preprocess_PCA = c(.94, 0.95, .96)
##preprocess_PCA = NA
#penalty = c(1, 2) #10^seq(-16, 16)
#mixture = c(0)
#method_cor = "pearson"
#model_description = "Consider writing a description of your model here"
#multi_cores = TRUE
#

# devtools::document()
#' Train word embeddings to a numeric variable.
#'
#' @param x Word embeddings from textEmbed (or textLayerAggregation).
#' @param y Numeric variable to predict.
# @param outside_folds Number of folds for the outer folds.
# @param outside_strata_y Variable to stratify according (default y; can set to NULL).
# @param inside_folds Number of folds for the inner folds.
# @param inside_strata_y Variable to stratify according (default y; can set to NULL).
#' @param preprocess_PCA Pre-processing threshold for PCA (to skip this step set it to NA).
#' Can select amount of variance to retain (e.g., .90 or as a grid c(0.80, 0.90)); or
#' number of components to select (e.g., 10). Default is "min_halving", which is a function
#' that selects the number of PCA components based on number  of participants and feature (word embedding dimensions)
#' in the data. The formula is:
#' preprocess_PCA = round(max(min(number_features/2), number_participants/2), min(50, number_features))).
#' @param penalty hyper parameter that is tuned
#' @param mixture hyper parameter that is tuned default = 0 (hence a pure ridge regression).
#' @param method_cor Type of correlation used in evaluation (default "pearson";
#' can set to "spearman" or "kendall").
#' @param model_description Text to describe your model (optional; good when sharing the model with others).
#' @param multi_cores If TRUE enables the use of multiple cores if computer/system allows for it (hence it can
#' make the analyses considerably faster to run).
#' @return A correlation between predicted and observed values; tibble of predicted values, as well as information
#' about the model (preprossing_recipe, final_model and model_description).
#' @examples
#' wordembeddings <- wordembeddings4
#' ratings_data <- Language_based_assessment_data_8
#'
#' results <- textTrainRegression(wordembeddings$harmonytext,
#' ratings_data$hilstotal,
#' multi_cores = FALSE #this is set to FALSE due to CRAN testing.
#' )
#' @seealso see \code{\link{textLayerAggregation}} \code{\link{textTrainLists}}
#' \code{\link{textTrainRandomForest}} \code{\link{textDiff}}
#' @importFrom stats cor.test na.omit lm
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
#                               outside_folds = 10, # is commented out due to a bug in rsample; when bug is resolved these will work.
#                               outside_strata_y = "y",
#                               inside_folds = 10, # is commented out due to a bug in rsample; when bug is resolved these will work.
#                               inside_strata_y = "y",
                                preprocess_PCA = "min_halving",
                                penalty = 10^seq(-16, 16),
                                mixture = c(0),
                                method_cor = "pearson",
                                model_description = "Consider writing a description of your model here",
                                multi_cores = TRUE) {
  set.seed(2020)
  x1 <- dplyr::select(x, dplyr::starts_with("Dim"))
  xy <- cbind(x1, y)

  results_nested_resampling <- rsample::nested_cv(xy,
                                                  outside = rsample::vfold_cv(v = 10, #outside_folds,
                                                                              repeats = 1,
                                                                              strata = "y",
                                                                              breaks = 2), #outside_strata_y
                                                  inside  = rsample::validation_split(prop = 3/4,
                                                                                      strata = "y", #inside_strata_y
                                                                                      breaks=1))
  #results_nested_resampling$inner_resamples
  # results_nested_resampling$inner_resamples[[1]]$splits[[1]]

   #dev_fit <- fit_model_rmse(object=results_nested_resampling$inner_resamples[[1]]$splits[[1]])
   #dev_fit
   #n_cross2_fit <- fit_model_rmse(object=results_nested_resampling$inner_resamples[[1]]$splits[[1]])
   #n_cross2_fit

   #dev_tune <- tune_over_cost(object=results_nested_resampling$inner_resamples[[1]]$splits[[1]])
   #dev_tune
   #n_cross2_tune <- tune_over_cost(object=results_nested_resampling$inner_resamples[[1]]$splits[[1]])
   #n_cross2_tune


  if (multi_cores == FALSE){
    tuning_results <- purrr::map(.x = results_nested_resampling$inner_resamples,
                                 .f = summarize_tune_results,
                                 penalty = penalty,
                                 mixture = mixture,
                                 preprocess_PCA = preprocess_PCA)
  } else if(multi_cores == TRUE) {
    # The multisession plan uses the local cores to process the inner resampling loop. help(multisession)
    # library(future) warnings()
    future::plan(future::multisession)
    # The object tuning_results is a list of data frames for each of the OUTER resamples.
    tuning_results <- furrr::future_map(.x = results_nested_resampling$inner_resamples,
                                        .f = summarize_tune_results,
                                        penalty = penalty,
                                        mixture = mixture,
                                        preprocess_PCA = preprocess_PCA)
  }

  # Function to get the lowest mean_RMSE
  bestParameters <- function(dat) dat[which.min(dat$RMSE),]

  # Determine the best parameter estimate from each INNER sample to be used
  # for each of the outer resampling iterations:
  hyper_parameter_vals <-
    tuning_results %>%
    purrr::map_df(bestParameters) %>%
    dplyr::select(c(penalty, mixture, preprocess_PCA))

  # Bind best results
  results_split_parameter <-
    dplyr::bind_cols(results_nested_resampling, hyper_parameter_vals)

  # Compute the outer resampling results for each of the
  # splits using the corresponding tuning parameter value from results_split_parameter.
  # fit_model_rmse(results_split_parameter$splits[[1]])
  results_outer <- purrr::pmap(list(object  = results_nested_resampling$splits,
                                    penalty = results_split_parameter$penalty,
                                    mixture = results_split_parameter$mixture,
                                    preprocess_PCA = results_split_parameter$preprocess_PCA),
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

#####
  # Construct final model to be saved and applied on other data
  is.list(xy)
  tibble::is_tibble(xy)
  is.data.frame(xy)

  final_recipe <- xy %>%
    recipes::recipe(y ~ .) %>%
    # recipes::step_BoxCox(all_predictors()) %>%
    recipes::step_naomit(Dim1, skip = TRUE) %>%
    recipes::step_center(recipes::all_predictors()) %>%
    recipes::step_scale(recipes::all_predictors()) %>%
    {if(!is.na(preprocess_PCA[1]))
    {if(preprocess_PCA[1] >= 1) recipes::step_pca(., recipes::all_predictors(), num_comp = statisticalMode(results_split_parameter$preprocess_PCA))
      else if(preprocess_PCA[1] < 1) recipes::step_pca(., recipes::all_predictors(), threshold = statisticalMode(results_split_parameter$preprocess_PCA))
      else . } else .}

#  if(preprocess_PCA[1] >= 1){
#  final_recipe <- xy %>%
#    recipes::recipe(y ~ .) %>%
#    # recipes::step_BoxCox(all_predictors()) %>%
#    recipes::step_naomit(Dim1, skip = TRUE) %>%
#    recipes::step_center(recipes::all_predictors()) %>%
#    recipes::step_scale(recipes::all_predictors()) %>%
#    recipes::step_pca(recipes::all_predictors(), num_comp = statisticalMode(results_split_parameter$preprocess_PCA)) #%>%
#    #recipes::prep()
#  }else if(preprocess_PCA[1] < 1){
#    final_recipe <- xy %>%
#      recipes::recipe(y ~ .) %>%
#      # recipes::step_BoxCox(all_predictors()) %>%
#      recipes::step_naomit(Dim1, skip = TRUE) %>%
#      recipes::step_center(recipes::all_predictors()) %>%
#      recipes::step_scale(recipes::all_predictors()) %>%
#      recipes::step_pca(recipes::all_predictors(), threshold = statisticalMode(results_split_parameter$preprocess_PCA)) #%>%
#    #recipes::prep()
#  }

  preprocessing_recipe <- recipes::prep(final_recipe)

  # To load the prepared training data into a variable juice() is used.
  # It extracts the data from the xy_recipe object.
  xy_final <- recipes::juice(preprocessing_recipe)



  if(length(xy_final)>2){
    # Create and fit model; penalty=NULL mixture = NULL
    final_predictive_model <-
      parsnip::linear_reg(penalty = statisticalMode(results_split_parameter$penalty), mixture = statisticalMode(results_split_parameter$mixture)) %>%
      parsnip::set_engine("glmnet") %>%
      parsnip::fit(y ~ ., data = xy_final)

    # Standard regression
  } else if (length(xy_final) == 2) {
    final_predictive_model <-
      parsnip::linear_reg(mode = "regression") %>%
      parsnip::set_engine("lm") %>%
      parsnip::fit(y ~ ., data = xy_final)
  }

#  final_predictive_model <-
#    parsnip::linear_reg(penalty = statisticalMode(results_split_parameter$penalty), mixture = statisticalMode(results_split_parameter$mixture)) %>%
#    parsnip::set_engine("glmnet") %>%
#    parsnip::fit(y ~ ., data = xy_final)
#####

  # Saving the final mtry and min_n used for the final model.
  penalty_description = paste("penalty = ", deparse(statisticalMode(results_split_parameter$penalty)))
  penalty_fold_description = paste("penalty in each fold = ", deparse(results_split_parameter$penalty))

  mixture_description = paste("mixture = ", deparse(statisticalMode(results_split_parameter$mixture)))
  mixture_fold_description = paste("mixture in each fold = ", deparse(results_split_parameter$mixture))

  preprocess_PCA_description = paste("preprocess_PCA = ", deparse(statisticalMode(results_split_parameter$preprocess_PCA)))
  preprocess_PCA_fold_description = paste("preprocess_PCA in each fold = ", deparse(results_split_parameter$preprocess_PCA))



  # Describe model; adding user's-description + the name of the x and y
  model_description_detail <- c(deparse(substitute(x)),
                                deparse(substitute(y)),
                                penalty_description,
                                penalty_fold_description,
                                mixture_description,
                                mixture_fold_description,
                                preprocess_PCA_description,
                                preprocess_PCA_fold_description,
                                model_description)

  final_results <- list(predy_y, preprocessing_recipe, final_predictive_model, model_description_detail, correlation)
  final_results
  names(final_results) <- c("predictions", "preprocessing_recipe", "final_model", "model_description", "correlation")
  final_results
}
########
# textTrainRegression



