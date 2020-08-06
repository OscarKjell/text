#x <- wordembeddings4_10[1]$harmonywords
#y <- Language_based_assessment_data_8_10[8]$gender

#x <- wordembeddings_large$harmonywords
#y <- rs$gender

#' Train word embeddings to a categorical variable using random forrest.
#'
#' @param x Word embeddings from textEmbed.
#' @param y Categorical variable to predict.
# @param outside_folds Number of folds for the outer folds.
#' @param outside_strata_y Variable to stratify according (default "y"; can also set to NULL).
# @param inside_folds Number of folds for the inner folds.
#' @param inside_strata_y Variable to stratify according (default "y"; can also set to NULL).
#' @param mtry hyper parameter that may be tuned;  default:c(1, 20, 40),
#' @param min_n hyper parameter that may be tuned; default: c(1, 20, 40)
#' @param trees Number of trees to use (default 1000).
#' @param model_description Text to describe your model (optional; good when sharing the model with others).
#' @param multi_cores If TRUE enables the use of multiple cores if computer/system allows for it (hence it can
#' make the analyses considerably faster to run).
#' @return A chi-square result between predicted and observed values; as well as predicted and true values.
#' @examples
#' wordembeddings <- wordembeddings4_10
#' example_categories <- as.factor(c(1, 2, 1, 2, 1, 2, 1, 2, 1, 2))
#' results <- textTrainRandomForest(wordembeddings$harmonywords,
#'                                  example_categories,
#'                                  mtry  = c(1),       # this is short because of testing
#'                                  min_n = c(1),       # this is short because of testing
#'                                  multi_cores = FALSE # This is FALSE due to CRAN testing.
#' )
#' @seealso see \code{\link{textTrainLists}} \code{\link{textDiff}}
#' @importFrom stats cor.test na.omit chisq.test fisher.test complete.cases
#' @importFrom dplyr select starts_with filter arrange rename
#' @importFrom recipes recipe step_naomit step_center step_scale step_pca
#' @importFrom rsample vfold_cv
#' @importFrom parsnip linear_reg set_engine rand_forest
#' @importFrom tune control_grid tune_grid select_best collect_predictions control_resamples
#' @importFrom workflows workflow add_model add_recipe
#' @importFrom magrittr %>%
#' @importFrom future plan multisession
#' @importFrom furrr future_map
#' @importFrom yardstick accuracy bal_accuracy sens spec precision kap
#' @export
textTrainRandomForest <- function(x,
                                  y,
                                  #outside_folds = 10,
                                  outside_strata_y = "y",
                                  #inside_folds = 10,
                                  inside_strata_y = "y",
                                  mtry = c(1, 5, 10, 15, 30, 40),
                                  min_n = c(1, 5, 10, 15, 30, 40),
                                  trees = 1000,
                                  model_description = "Consider writing a description of your model here",
                                  multi_cores = TRUE) {

  x1 <- dplyr::select(x, dplyr::starts_with("Dim"))
  y1 <- as.factor(y)
  xy <- cbind(x1, y1)

  xy$id1 <- c(seq_len(nrow(xy)))
  xy_formergingNA <- tibble::tibble(xy$id1, xy$y)
  colnames(xy_formergingNA) <- c("id1", "y")
  xy1 <- tibble::as_tibble(xy[stats::complete.cases(xy), ])

  results_nested_resampling <- rsample::nested_cv(xy1,
                                                  outside = rsample::vfold_cv(v = 5, #outside_folds,
                                                                              repeats = 1,
                                                                              strata_y = outside_strata_y),
                                                  inside  = rsample::vfold_cv(v = 5, #inside_folds,
                                                                              repeats = 1,
                                                                              strata_y = inside_strata_y))

  # Function to fit a model and compute RMSE
  # object: an rsplit object (from results_nested_resampling tibble)
  # object = results_nested_resampling$splits[[5]]; mtry = 1  min_n = 1
  fit_model_accuracy <- function(object, mtry = 1, min_n = 1) {

    # Recipe: Pre-processing by removing na and normalizing variables. library(magrittr)
    xy_recipe <- rsample::analysis(object) %>%
      recipes::recipe(y1 ~ .) %>%
      recipes::update_role(id1, new_role = "id variable") %>%
      #recipes::update_role(-id1, new_role = "predictor") %>%
      recipes::update_role(y1, new_role = "outcome") %>%
      recipes::step_naomit(Dim1, skip = FALSE) %>% # Does this not work here?
      recipes::step_center(recipes::all_predictors()) %>%
      recipes::step_scale(recipes::all_predictors()) %>%
      recipes::step_BoxCox(recipes::all_predictors()) %>%
      recipes::prep()

    # To load the prepared training data into a variable juice() is used.
    # It extracts the data from the xy_recipe object.
    xy_training <- recipes::juice(xy_recipe)

    # Create and fit model.
    mod <-
      parsnip::rand_forest(trees = trees, mode = "classification", mtry = mtry, min_n = min_n) %>%
      # set_engine("ranger")
      parsnip::set_engine("randomForest") %>%
      parsnip::fit(y1 ~ ., data = xy_training) #analysis(object)

    #Prepare the test data according to the recipe
    xy_testing <- xy_recipe %>%
      recipes::bake(rsample::assessment(object))
    # look at xy_testing: glimpse(xy_testing)

    # Apply model on new data
    holdout_pred <-
      stats::predict(mod, xy_testing %>% dplyr::select(-y1)) %>%
      dplyr::bind_cols(rsample::assessment(object) %>% dplyr::select(y1))

    # Get accuracy help(accuracy)
    accuracy_val <- yardstick::accuracy(holdout_pred, truth = y1, estimate = .pred_class)#$.estimate
    # Sort output of RMSE, predictions and truth (observed y)
    output <- list(list(accuracy_val), list(holdout_pred$.pred_class), list(holdout_pred$y1))
    names(output) <- c("accuracy_prop", "estimate", "truth")
    output
  }

  # testing
  # fit_model_accuracy(object=results_nested_resampling$inner_resamples[[2]]$splits[[1]])
  # fit_model_accuracy(object = results_nested_resampling$splits[[1]], mtry = 1, min_n = 1)

  # In some situations, we want to parameterize the function over the tuning parameter:
  fit_model_accuracy_wrapper <- function(mtry, min_n, object) fit_model_accuracy(object, mtry, min_n)

  # For the nested resampling, a model needs to be fit for each tuning parameter and each INNER split.
  # object:  an rsplit object from the INNER samples
  # object=results_nested_resampling$inner_resamples[[1]]$splits[[1]]
  tune_over_cost <- function(object) {

    grid_inner <- base::expand.grid(
      mtry = mtry,
      min_n = min_n,
      trees = trees)

    # Test models with the different hyperparameters for the inner samples
    tune_results <- purrr::map2(grid_inner$mtry,
                                grid_inner$min_n,
                                fit_model_accuracy_wrapper,
                                object = object)

    # Sort the output to separate the rmse, predictions and truth
    tune_outputlist <- tune_results %>%
      dplyr::bind_rows() %>%
      split.default(names(.)) %>%
      purrr::map(na.omit)

    # Extract the Accuracy
    tune_accuracy <- (dplyr::bind_rows(tune_outputlist$accuracy_prop$accuracy_prop))$.estimate
    # Add accuracy to the grid
    grid_inner_accuracy <- grid_inner %>%
      dplyr::mutate(accuracy = tune_accuracy)

    grid_inner_accuracy
  }
  # testing
  #tune_over_cost(object=results_nested_resampling$inner_resamples[[1]]$splits[[1]])

  # object = results_nested_resampling$inner_resamples[[1]]
  # Since this will be called across the set of OUTER cross-validation splits, another wrapper is required:
  # object: an `rsplit` object (from results$inner_resamples)
  summarize_tune_results <- function(object) {

    # Return row-bound tibble containing the INNER results
    purrr::map_df(object$splits, tune_over_cost) %>%

      # For each value of the tuning parameter, compute the
      # average RMSE which is the INNER estimate.
      dplyr::group_by(mtry) %>%
      dplyr::summarize(min_n = min_n,
                       mean_accuracy = mean(accuracy, na.rm = TRUE),
                       n = length(accuracy),
                       .groups = "drop_last")
  }

  # Tuning inner resamples library(magrittr)
  if (multi_cores == FALSE){
    tuning_results <- purrr::map(results_nested_resampling$inner_resamples, summarize_tune_results)
  } else {
    # The multisession plan uses the local cores to process the inner resampling loop.
    #library(future)
    future::plan(multisession)
    # The object tuning_results is a list of data frames for each of the OUTER resamples.
    tuning_results <- furrr::future_map(results_nested_resampling$inner_resamples, summarize_tune_results)
  }

  # Function to get the lowest mean_accuracy
  bestParameters <- function(dat) dat[which.min(dat$mean_accuracy),]

  # Determine the best parameter estimate from each INNER sample to be used
  # for each of the outer resampling iterations: library(magrittr)
  hyper_parameter_vals <-
    tuning_results %>%
    purrr::map_df(bestParameters) %>%
    dplyr::select(c(mtry, min_n))

  # Bind best results
  results_split_parameter <-
    dplyr::bind_cols(results_nested_resampling, hyper_parameter_vals)

  # Compute the outer resampling results for each of the
  # splits using the corresponding tuning parameter value from results_split_parameter.
  # fit_model_rmse(results_split_parameter$splits[[1]])
  results_outer <- purrr::pmap(list(object=results_nested_resampling$splits,
                                    results_split_parameter$mtry,
                                    results_split_parameter$min_n),
                               fit_model_accuracy)

  # Separate RMSE, predictions and observed y
  outputlist_results_outer <- results_outer %>%
    dplyr::bind_rows() %>%
    split.default(names(.)) %>%
    purrr::map(na.omit)

  # Unnest predictions and y
  predy_y <- tibble::tibble(tidyr::unnest(outputlist_results_outer$truth, cols = c(truth)),
                            tidyr::unnest(outputlist_results_outer$estimate, cols = c(estimate)))

  # Correlate predictions and observed
  chisq        <- chisq.test(table(predy_y$truth, predy_y$estimate))

  fisher <- stats::fisher.test(predy_y$truth, predy_y$estimate)

  accuracy     <- yardstick::accuracy(predy_y, truth, estimate)
  bal_accuracy <- yardstick::bal_accuracy(predy_y, truth, estimate)
  sens         <- yardstick::sens(predy_y, truth, estimate)
  spec         <- yardstick::spec(predy_y, truth, estimate)
  precision    <- yardstick::precision(predy_y, truth, estimate)
  kappa        <- yardstick::kap(predy_y, truth, estimate)

  results_collected <- dplyr::bind_rows(accuracy,
                                        bal_accuracy,
                                        sens,
                                        spec,
                                        precision,
                                        kappa)
  results_collected


  # Construct final model to be saved and applied on other data
  final_predictive_model <-
    parsnip::rand_forest(trees = trees, mode = "classification",
                         mtry = mean(results_split_parameter$mtry),
                         min_n = mean(results_split_parameter$min_n)) %>%
    # set_engine("ranger")
    parsnip::set_engine("randomForest") %>%
    parsnip::fit(y1 ~ ., data = xy) #analysis(object)

  # Describe model; adding user's-description + the name of the x and y
  # model_description_detail <- c(model_description, paste(names(x)), paste(names(y)))
  model_description_detail <- c(deparse(substitute(x)), deparse(substitute(y)), model_description)


  final_results <- list(predy_y, final_predictive_model, model_description_detail, fisher, chisq, results_collected)
  names(final_results) <- c("truth_predictions", "final_model", "model_description", "fisher_test", "chisq", "results")
  final_results
}
#
#library(text)
#wordembeddings <- wordembeddings4_10
#ratings_data <- Language_based_assessment_data_8_10
#x <- wordembeddings$harmonytext
#y = ratings_data$gender
#t1 <- Sys.time()
#results <- textTrainRandomForest(wordembeddings$harmonytext,
#  ratings_data$gender,
#outside_folds = 10,
#outside_strata_y = "y",
#inside_folds = 10,
#inside_strata_y = "y"
#)
#results
#t2 <- Sys.time()
#t2-t1
