#rlang::last_error()
#rlang::last_trace()
#example_categories <- Language_based_assessment_data_8[8]$gender
###
#results2 <- textTrainRandomForest(wordembeddings4$harmonywords,
#                                 example_categories,
#                                 eval_measure = "bal_accuracy", # f_measure bal_accuracy accuracy
#                                 mtry  = c(1, 5),
#                                 min_n = c(1, 5),
#                                 trees = c(100, 1000),
#                                 multi_cores = TRUE)
#results2$roc_curve_plot
# library(tidyverse)

#  devtools::document()
#' Function to fit a model and compute accuracy
#'
#' @param object An rsplit object (from results_nested_resampling tibble)
#' object = results_nested_resampling$splits[[1]]
#' @param extremely_randomised_splitrule default: "extratrees", see also: "gini" or "hellinger"
#' @param mtry hyperparameter for random forest.
#' @param min_n hyperparameter for random forest.
#' @param trees number of trees
#' @param eval_measure Measure used to evaluate and select models default: "roc_auc", see also
#' "accuracy", "bal_accuracy", "sens", "spec", "precision", "kappa", "f_measure".
#' @return  Accuracy
#' @noRd
fit_model_accuracy_rf <- function(object,
                                  mtry = 1,
                                  min_n = 1,
                                  trees = 1000,
                                  preprocess_PCA_thresh = 0.95,
                                  eval_measure = "f_measure",
                                  extremely_randomised_splitrule = NULL) {

  # Recipe: Pre-processing by removing na and normalizing variables. library(magrittr)
  xy_recipe <- rsample::analysis(object) %>%
    recipes::recipe(y ~ .) %>%
    recipes::update_role(id1, new_role = "id variable") %>%
    #recipes::update_role(-id1, new_role = "predictor") %>%
    recipes::update_role(y, new_role = "outcome") %>%
    recipes::step_naomit(Dim1, skip = FALSE) %>% # Does this not work here?
    recipes::step_center(recipes::all_predictors()) %>%
    recipes::step_scale(recipes::all_predictors()) %>%
    recipes::step_BoxCox(recipes::all_predictors()) %>%
    recipes::step_pca(recipes::all_predictors(), threshold = preprocess_PCA_thresh) %>%
    recipes::prep()

  # To load the prepared training data into a variable juice() is used.
  # It extracts the data from the xy_recipe object.
  xy_training <- recipes::juice(xy_recipe)

  # Create and fit model. help(rand_forest)
  if(is.null(extremely_randomised_splitrule)){
  mod <-
    parsnip::rand_forest(trees = trees, mode = "classification", mtry = mtry, min_n = min_n) %>%
    #parsnip::set_engine("ranger")
    parsnip::set_engine("randomForest") %>%
    parsnip::fit(y ~ ., data = xy_training) #analysis(object)
  } else if(is.character(extremely_randomised_splitrule)){
    mod <-
      parsnip::rand_forest() %>%
      parsnip::set_engine("ranger", splitrule = extremely_randomised_splitrule) %>% # extremely_randomised_splitrule = "extratrees" = "gini" "extratrees" "hellinger" tune()
      parsnip::set_mode("classification") %>%
      parsnip::fit(y ~ ., data = xy_training) # ADDED line; and: install.packages("ranger")
  }

  #Prepare the test data according to the recipe
  xy_testing <- xy_recipe %>%
    recipes::bake(rsample::assessment(object))
  # look at xy_testing: glimpse(xy_testing)

  # Apply model on new data help(predict)
  holdout_pred_class <-
    stats::predict(mod, xy_testing %>% dplyr::select(-y), type= c("class")) %>%
    dplyr::bind_cols(rsample::assessment(object) %>% dplyr::select(y))
  holdout_pred_prob <-
    stats::predict(mod, xy_testing %>% dplyr::select(-y), type= c("prob")) %>%
    dplyr::bind_cols(rsample::assessment(object) %>% dplyr::select(y))

  holdout_pred_prob$.pred_class <- holdout_pred_class$.pred_class
  class <- colnames(holdout_pred_prob[1])
  # Get accuracy help(accuracy)
  if (eval_measure == "accuracy"){
    eval_measure_val           <- yardstick::accuracy(holdout_pred_prob, truth = y, estimate = .pred_class)
  } else if (eval_measure == "bal_accuracy"){
    eval_measure_val  <- yardstick::bal_accuracy(holdout_pred_prob, truth = y, estimate = .pred_class)
  } else if (eval_measure == "sens"){
    eval_measure_val          <- yardstick::sens(holdout_pred_prob, truth = y, estimate = .pred_class)
  } else if (eval_measure == "spec"){
    eval_measure_val          <- yardstick::spec(holdout_pred_prob, truth = y, estimate = .pred_class)
  } else if (eval_measure == "precision"){
    eval_measure_val          <- yardstick::precision(holdout_pred_prob, truth = y, estimate = .pred_class)
  } else if (eval_measure == "kappa"){
    eval_measure_val          <- yardstick::kap(holdout_pred_prob, truth = y, estimate = .pred_class)
  } else if (eval_measure == "f_measure"){
    eval_measure_val          <- yardstick::f_meas(holdout_pred_prob, truth = y, estimate = .pred_class)
  } else if (eval_measure == "roc_auc"){
    eval_measure_val          <- yardstick::roc_auc(holdout_pred_prob, truth = y, eval(class)) #
  }

  # Sort output of RMSE, predictions and truth (observed y) help(accuracy)
  output <- list(list(eval_measure_val),
                 list(holdout_pred_prob$.pred_class),
                 list(holdout_pred_prob$y),
                 list(holdout_pred_prob[1]),
                 list(holdout_pred_prob[2]),
                 list(preprocess_PCA_thresh))
  names(output) <- c("eval_measure_val",
                     "estimate",
                     "truth",
                     ".pred_1",
                     ".pred_2",
                     "preprocess_PCA_thresh")
  output
}

# object: an rsplit object (from results_nested_resampling tibble)
# object = results_nested_resampling$splits[[5]]; mtry = 1  min_n = 1
# testing
#fit_model_accuracy_rf(object = results_nested_resampling$splits[[2]], mtry = 1, min_n = 1, eval_measure="roc_auc")

# In some situations, we want to parameterize the function over the tuning parameter:
#' Function to fit a model and compute RMSE.
#'
#' @param object An rsplit object (from results_nested_resampling tibble)
#' object = results_nested_resampling$splits[[1]]
#' @param extremely_randomised_splitrule default: "extratrees", see also: "gini" or "hellinger"
#' @param mtry hyperparameter for random forest.
#' @param min_n hyperparameter for random forest.
#' @param trees number of trees
#' @param eval_measure Measure used to evaluate and select models default: "roc_auc", see also
#' "accuracy", "bal_accuracy", "sens", "spec", "precision", "kappa", "f_measure".
#' @return Accuracy
#' @noRd
fit_model_accuracy_wrapper_rf <- function(mtry,
                                          min_n,
                                          object,
                                          trees,
                                          preprocess_PCA_thresh,
                                          eval_measure,
                                          extremely_randomised_splitrule) fit_model_accuracy_rf(object,
                                                                              mtry,
                                                                              min_n,
                                                                              trees,
                                                                              preprocess_PCA_thresh,
                                                                              eval_measure,
                                                                              extremely_randomised_splitrule)

#fit_model_accuracy_wrapper_rf(object = results_nested_resampling$splits[[1]], mtry=1, min_n=1, trees=10, preprocess_PCA_thresh=0.95, eval_measure = "roc_auc")

#' For the nested resampling, a model needs to be fit for each tuning parameter and each INNER split.
#'
#' @param object an rsplit object from the INNER samples
#' object=results_nested_resampling$inner_resamples[[1]]$splits[[1]]
#' @param extremely_randomised_splitrule default: "extratrees", see also: "gini" or "hellinger"
#' @param mtry hyperparameter for random forest.
#' @param min_n hyperparameter for random forest.
#' @param trees number of trees
#' @return Accuracy
#' @noRd
tune_over_cost_rf <- function(object,
                              mtry,
                              min_n,
                              trees,
                              preprocess_PCA_thresh,
                              eval_measure,
                              extremely_randomised_splitrule) {


grid_inner <- base::expand.grid(
  mtry = mtry,
  min_n = min_n,
  trees = trees,
  preprocess_PCA_thresh = preprocess_PCA_thresh)

  # Test models with the different hyperparameters for the inner samples
  tune_results <- purrr::pmap(list(grid_inner$mtry,
                              grid_inner$min_n,
                              grid_inner$trees,
                              grid_inner$preprocess_PCA_thresh),
                              fit_model_accuracy_wrapper_rf,
                              object = object,
                              eval_measure = eval_measure,
                              extremely_randomised_splitrule)

  # Sort the output to separate the accuracy, predictions and truth
  tune_outputlist <- tune_results %>%
    dplyr::bind_rows() %>%
    split.default(names(.)) %>%
    purrr::map(na.omit)

  # Extract the Accuracy
  tune_accuracy <- (dplyr::bind_rows(tune_outputlist$eval_measure_val$eval_measure_val))$.estimate
  # Add accuracy to the grid
  grid_inner_accuracy <- grid_inner %>%
    dplyr::mutate(eval_measure = tune_accuracy)

  grid_inner_accuracy
}
# testing help(yardstick)
#tune_over_cost_rf(object=results_nested_resampling$inner_resamples[[1]]$splits[[1]])

# object = results_nested_resampling$inner_resamples[[1]]
# Since this will be called across the set of OUTER cross-validation splits, another wrapper is required:
# object: an `rsplit` object (from results$inner_resamples)
#' Since this will be called across the set of OUTER cross-validation splits, another wrapper is required:
#'
#' @param object An rsplit object from the INNER samples
#' object = results_nested_resampling$inner_resamples[[1]]
#' @param extremely_randomised_splitrule default: "extratrees", see also: "gini" or "hellinger"
#' @param mtry hyperparameter for random forest.
#' @param min_n hyperparameter for random forest.
#' @param trees number of trees
#' @return Accuracy
#' @noRd
summarize_tune_results_rf <- function(object,
                                      mtry,
                                      min_n,
                                      trees,
                                      preprocess_PCA_thresh,
                                      eval_measure,
                                      extremely_randomised_splitrule) {

  # Return row-bound tibble containing the INNER results
  purrr::map_df(.x = object$splits,
                .f = tune_over_cost_rf,
                mtry=mtry,
                min_n=min_n,
                trees = trees,
                preprocess_PCA_thresh = preprocess_PCA_thresh,
                eval_measure = eval_measure,
                extremely_randomised_splitrule = extremely_randomised_splitrule) %>%

    # For each value of the tuning parameter, compute the
    # average RMSE which is the INNER estimate.
    dplyr::group_by(mtry) %>%
    dplyr::summarize(min_n = min_n,
                     mean_eval_measure = mean(eval_measure, na.rm = TRUE), #TODO should it be mean or mode here?
                     trees = trees,
                     preprocess_PCA_thresh = preprocess_PCA_thresh,
                     n = length(eval_measure),
                     .groups = "drop_last")
}

#x <- wordembeddings4[1]$harmonywords
#y <- Language_based_assessment_data_8[8]$gender#
#outside_strata_y = "y"#
#inside_strata_y = "y"
#mtry = c(1, 2)
#min_n = c(1, 2)
#preprocess_PCA_thresh = c(0.85, 0.95)
#trees = c(1000, 1550)
#model_description = "Consider writing a description of your model here"
#multi_cores = TRUE
#eval_measure = "bal_accuracy" # "roc_auc" #"accuracy" #
#extremely_randomised_splitrule = NULL
#library(magrittr)

#' Train word embeddings to a categorical variable using random forrest.
#'
#' @param x Word embeddings from textEmbed.
#' @param y Categorical variable to predict.
# @param outside_folds Number of folds for the outer folds.
# @param outside_strata_y Variable to stratify according (default "y"; can also set to NULL).
# @param inside_folds Number of folds for the inner folds.
# @param inside_strata_y Variable to stratify according (default "y"; can also set to NULL).
#' @param preprocess_PCA_thresh Pre-processing threshold for amount of variance to retain (default 0.95).
#' @param extremely_randomised_splitrule default: NULL, which thus implement a random forest; can also select: "extratrees", "gini" or "hellinger"; if these are selected
#' your mtry settings will be overridden (see Geurts et al. (2006) Extremely randomized trees for details; and see the ranger r-package
#' for details on implementations).
#' @param mtry hyper parameter that may be tuned;  default:c(1, 20, 40),
#' @param min_n hyper parameter that may be tuned; default: c(1, 20, 40)
#' @param trees Number of trees to use (default 1000).
#' @param eval_measure Measure to evaluate the models in order to select the best hyperparameters default "roc_auc";
#' see also "accuracy", "bal_accuracy", "sens", "spec", "precision", "kappa", "f_measure".
#' @param model_description Text to describe your model (optional; good when sharing the model with others).
#' @param multi_cores If TRUE enables the use of multiple cores if computer/system allows for it (hence it can
#' make the analyses considerably faster to run).
#' @return A list with roc_curve_data, roc_curve_plot, truth and predictions, preprocessing_recipe, final_model, model_description
#' chisq and fishers test as well as evaluation measures, e.g., including accuracy, f_meas and roc_auc (for details on
#' these measures see the yardstick r-package documentation).
#' @examples
#' wordembeddings <- wordembeddings4
#' example_categories <- as.factor(c(1, 2, 1, 2, 1, 2, 1, 2, 1, 2))
#' results <- textTrainRandomForest(wordembeddings$harmonywords,
#'                                  example_categories,
#'                                  trees = c(1000, 1500),
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
#' @importFrom yardstick accuracy bal_accuracy sens spec precision kap f_meas
#' @export
textTrainRandomForest <- function(x,
                                  y,
                                  #outside_folds = 10,
                                  #outside_strata_y = "y",
                                  #inside_folds = 10,
                                  #inside_strata_y = "y",
                                  preprocess_PCA_thresh = c(0.75, 0.85, 0.95),
                                  extremely_randomised_splitrule = NULL,
                                  mtry = c(1, 5, 10, 15, 30, 40),
                                  min_n = c(1, 5, 10, 15, 30, 40),
                                  trees = c(1000, 1500),
                                  eval_measure = "roc_auc",
                                  model_description = "Consider writing a description of your model here",
                                  multi_cores = TRUE) {

  x1 <- dplyr::select(x, dplyr::starts_with("Dim"))
  y <- as.factor(y)
  xy <- cbind(x1, y)

  xy$id1 <- c(seq_len(nrow(xy)))
  #xy_formergingNA <- tibble::tibble(xy$id1, xy$y)
  #colnames(xy_formergingNA) <- c("id1", "y")
  xy1 <- tibble::as_tibble(xy[stats::complete.cases(xy), ])

  results_nested_resampling <- rsample::nested_cv(xy1,
                                                  outside = rsample::vfold_cv(v = 5, #outside_folds,
                                                                              repeats = 1,
                                                                              strata = "y"), #outside_strata_y
                                                  inside  = rsample::validation_split(prop = 3/4,
                                                                                      strata = "y", #inside_strata_y
                                                                                      breaks=1))

  # Tuning inner resamples library(magrittr) warnings()
  if (multi_cores == FALSE){
    tuning_results <- purrr::map(.x = results_nested_resampling$inner_resamples,
                                 .f = summarize_tune_results_rf,
                                 mtry = mtry,
                                 min_n = min_n,
                                 trees = trees,
                                 preprocess_PCA_thresh = preprocess_PCA_thresh,
                                 eval_measure = eval_measure,
                                 extremely_randomised_splitrule = extremely_randomised_splitrule)
  } else {
    # The multisession plan uses the local cores to process the inner resampling loop.
    #library(future)
    future::plan(future::multisession)
    # The object tuning_results is a list of data frames for each of the OUTER resamples.
    tuning_results <- furrr::future_map(.x = results_nested_resampling$inner_resamples,
                                        .f = summarize_tune_results_rf,
                                        mtry = mtry,
                                        min_n = min_n,
                                        trees = trees,
                                        preprocess_PCA_thresh = preprocess_PCA_thresh,
                                        eval_measure = eval_measure,
                                        extremely_randomised_splitrule = extremely_randomised_splitrule)
  }

  # Function to get the lowest eval_measure_val
  bestParameters <- function(dat) dat[which.min(dat$mean_eval_measure),]

  # Determine the best parameter estimate from each INNER sample to be used
  # for each of the outer resampling iterations:
  hyper_parameter_vals <-
    tuning_results %>%
    purrr::map_df(bestParameters) #%>%
    #dplyr::select(c(mtry, min_n, trees, preprocess_PCA_thresh))

  # Bind best results
  results_split_parameter <-
    dplyr::bind_cols(results_nested_resampling, hyper_parameter_vals)

  # Compute the outer resampling results for each of the
  # splits using the corresponding tuning parameter value from results_split_parameter.
  # fit_model_rmse(results_split_parameter$splits[[1]])
  results_outer <- purrr::pmap(list(object=results_nested_resampling$splits,
                                    results_split_parameter$mtry,
                                    results_split_parameter$min_n,
                                    trees = results_split_parameter$trees,
                                    preprocess_PCA_thresh = results_split_parameter$preprocess_PCA_thresh),
                               fit_model_accuracy_rf
                               )

  # Separate RMSE, predictions and observed y
  outputlist_results_outer <- results_outer %>%
    dplyr::bind_rows() %>%
    split.default(names(.)) %>%
    purrr::map(na.omit)

  # Unnest predictions and y
  predy_y <- tibble::tibble(tidyr::unnest(outputlist_results_outer$truth, cols = c(truth)),
                            tidyr::unnest(outputlist_results_outer$estimate, cols = c(estimate)),
                            tidyr::unnest(outputlist_results_outer$.pred_1, cols = c(.pred_1)),
                            tidyr::unnest(outputlist_results_outer$.pred_2, cols = c(.pred_2)))

  # Correlate predictions and observed help(all_of)
  chisq        <- suppressWarnings(chisq.test(table(predy_y$truth, predy_y$estimate)))

  fisher <- stats::fisher.test(predy_y$truth, predy_y$estimate)



  accuracy     <- yardstick::accuracy(predy_y, truth, estimate)
  bal_accuracy <- yardstick::bal_accuracy(predy_y, truth, estimate)
  sens         <- yardstick::sens(predy_y, truth, estimate)
  spec         <- yardstick::spec(predy_y, truth, estimate)
  precision    <- yardstick::precision(predy_y, truth, estimate)
  kappa        <- yardstick::kap(predy_y, truth, estimate)
  f_measure    <- yardstick::f_meas(predy_y, truth, estimate)

  #eval_class <- colnames(predy_y[3])
  roc_auc      <- yardstick::roc_auc(predy_y, truth, colnames(predy_y[3])) # OK tidyselect::all_of(eval_class) dplyr::
  roc_curve_data    <- yardstick::roc_curve(predy_y, truth, colnames(predy_y[3]))
  roc_curve_plot <- ggplot2::autoplot(yardstick::roc_curve(predy_y, truth, colnames(predy_y[3])))

  results_collected <- dplyr::bind_rows(accuracy,
                                        bal_accuracy,
                                        sens,
                                        spec,
                                        precision,
                                        kappa,
                                        f_measure,
                                        roc_auc)
  results_collected


  # Construct final model to be saved and applied on other data
  # Recipe: Pre-processing by removing na and normalizing variables. library(magrittr)
  final_recipe <- xy %>%
    recipes::recipe(y ~ .) %>%
    recipes::update_role(id1, new_role = "id variable") %>%
    #recipes::update_role(-id1, new_role = "predictor") %>%
    recipes::update_role(y, new_role = "outcome") %>%
    recipes::step_naomit(Dim1, skip = FALSE) %>% # Does this not work here?
    recipes::step_center(recipes::all_predictors()) %>%
    recipes::step_scale(recipes::all_predictors()) %>%
    recipes::step_BoxCox(recipes::all_predictors()) %>%
    recipes::step_pca(recipes::all_predictors(), threshold = statisticalMode(results_split_parameter$preprocess_PCA_thresh)) #%>%
    #recipes::prep()

  preprocessing_recipe <- recipes::prep(final_recipe)

  # To load the prepared training data into a variable juice() is used.
  # It extracts the data from the xy_recipe object.
  xy_final <- recipes::juice(preprocessing_recipe)

  final_predictive_model <-
    parsnip::rand_forest(trees = statisticalMode(results_split_parameter$trees), mode = "classification",
                         mtry = statisticalMode(results_split_parameter$mtry),
                         min_n = statisticalMode(results_split_parameter$min_n)) %>%
    # set_engine("ranger")
    parsnip::set_engine("randomForest") %>%
    parsnip::fit(y ~ ., data = xy_final) #analysis(object)

  # Saving the final mtry and min_n used for the final model.
  if(is.null(extremely_randomised_splitrule)){
    mtry_description = paste("mtry =", deparse(statisticalMode(results_split_parameter$mtry)))
    min_n_description = paste("min_n =", deparse(statisticalMode(results_split_parameter$min_n)))
  }else{
    mtry_description <- c("-")
    min_n_description <- c("-")
  }
  trees_description = paste("trees =", deparse(statisticalMode(results_split_parameter$trees)))
  preprocess_PCA_thresh_description = paste("preprocess_PCA_thresh = ", deparse(statisticalMode(results_split_parameter$preprocess_PCA_thresh)))
  eval_measure = paste("eval_measure = ", deparse(eval_measure))

  if(is.character(extremely_randomised_splitrule)){
  extremely_randomised_splitrule  = paste("extremely_randomised_splitrule = ", deparse(extremely_randomised_splitrule))
  }else{
  extremely_randomised_splitrule <- c("-")
  }
  # Describe model; adding user's-description + the name of the x and y and mtry and min_n
  model_description_detail <- c(deparse(substitute(x)),
                                deparse(substitute(y)),
                                preprocess_PCA_thresh_description,
                                extremely_randomised_splitrule,
                                eval_measure,
                                mtry_description,
                                min_n_description,
                                trees_description,
                                model_description)

  final_results <- list(roc_curve_data, predy_y, preprocessing_recipe,  final_predictive_model, roc_curve_plot, model_description_detail, fisher, chisq, results_collected)
  names(final_results) <- c("roc_curve_data", "truth_predictions", "preprocessing_recipe", "final_model", "roc_curve_plot", "model_description", "fisher_test", "chisq", "results")
  final_results
}
#warnings()
#library(text)
#wordembeddings <- wordembeddings4
#ratings_data <- Language_based_assessment_data_8
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
