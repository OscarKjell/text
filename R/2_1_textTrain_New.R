

wordembeddings <- wordembeddings4_10
ratings_data <- Language_based_assessment_data_8_10
x <- wordembeddings$harmonytext
y <- ratings_data$hilstotal
outside_folds = 10
outside_strata_y = "y"
inside_folds = 10
inside_strata_y = "y"
preprocess_PCA_thresh = 0.95
method_cor = "pearson"
model_description = "Consider writing a description here"
multi_cores = TRUE


#results <- textTrain(wordembeddings$harmonytext, ratings_data$hilstotal,
#  nrFolds_k = 2, strata_y = NULL


textTrainRegression <- function(x,
                      y,
                      outside_folds = 10,
                      outside_strata_y = "y",
                      inside_folds = 10,
                      inside_strata_y = "y",
                      preprocess_PCA_thresh = 0.95,
                      method_cor = "pearson",
                      model_description = "Consider writing a description of your model here",
                      multi_cores = TRUE) {

  x1 <- dplyr::select(x, dplyr::starts_with("Dim"))
  xy <- cbind(x1, y)

  results_nested_resampling <- rsample::nested_cv(xy,
                                                  outside = rsample::vfold_cv(v = outside_folds,
                                                                              repeats = 1,
                                                                              strata_y = outside_strata_y),
                                                  inside  = rsample::vfold_cv(v = inside_folds,
                                                                              repeats = 1,
                                                                              strata_y = inside_strata_y))

  # Function to fit a model and compute RMSE
  # object: an rsplit object (from results_nested_resampling tibble)
  # object = results_nested_resampling$splits[[1]]
  fit_model_rmse <- function(object, penalty = 1, mixture = 0) {

    xy_recipe <- analysis(object) %>%
      recipes::recipe(y ~ .) %>%
      # recipes::step_BoxCox(all_predictors()) %>%
      recipes::step_naomit(Dim1, skip = TRUE) %>%
      recipes::step_center(all_predictors()) %>%
      recipes::step_scale(all_predictors()) %>%
      recipes::step_pca(all_predictors(), threshold = preprocess_PCA_thresh) %>% #  num_comp = tune()
      prep()

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
      bake(assessment(object))
    # look at xy_testing: glimpse(xy_testing)

    # Apply model on new data
    holdout_pred <-
      stats::predict(mod, xy_testing %>% dplyr::select(-y)) %>%
      dplyr::bind_cols(assessment(object) %>% dplyr::select(y))

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
  # object=results_nested_resampling$inner_resamples[[5]]$splits[[1]]
  tune_over_cost <- function(object) {

    grid_inner <- base::expand.grid(
      penalty = c(0.01, 0.1),
      mixture = c(0, 0.5, 1))

    #  grid_inner <- base::expand.grid(
    #    penalty = 10^seq(-3, -1, length = 20),
    #    mixture = (0:5) / 5)

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

  # object = results_nested_resampling$inner_resamples
  # Since this will be called across the set of OUTER cross-validation splits, another wrapper is required:
  # object: an `rsplit` object (from results$inner_resamples)
  summarize_tune_results <- function(object) {

    # Return row-bound tibble containing the INNER results
    purrr::map_df(object$splits, tune_over_cost) %>%

      # For each value of the tuning parameter, compute the
      # average RMSE which is the INNER estimate.
      dplyr::group_by(penalty) %>%
      dplyr::summarize(mixture = mixture,
                       mean_RMSE = mean(RMSE, na.rm = TRUE),
                       n = length(RMSE))
  }

  if (multi_cores == FALSE){
  tuning_results <- map(results_nested_resampling$inner_resamples, summarize_tune_results)
  } else {
  # The multisession plan uses the local cores to process the inner resampling loop.
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
  predy_y <- tibble::tibble(unnest(outputlist_results_outer$predictions, cols = c(predictions)),
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
#
wordembeddings <- wordembeddings4_10
ratings_data <- Language_based_assessment_data_8_10


test_smal <- textTrainRegression(x = wordembeddings$harmonytext,
          y = ratings_data$hilstotal,
          outside_folds = 10,
          outside_strata_y = "y",
          inside_folds = 10,
          inside_strata_y = "y",
          preprocess_PCA_thresh = 0.95,
          method_cor = "pearson",
          model_description = "Consider writing a description here",
          multi_cores = TRUE)

# Is it better having only 2 outside folds (as default)?
# should the number of PCA be based on number of participants; perhaps it is possible to find a functions for this?

# Trying with more data
large_data_ex <- readRDS("/Users/oscarkjell/Desktop/1 Projects/0 Research/0 text r-package/text_data_examples/solmini_no_na_raw_600_.rda")
large_data_ex <- large_data_ex[, c(1, 6)]

#large_data_ex_embeddings <- textEmbed(large_data_ex)

T1 <- Sys.time()
textTrainRegression(x = large_data_ex_embeddings$harmonywords,
          y = large_data_ex$hilstotal,
          outside_folds = 10,
          outside_strata_y = "y",
          inside_folds = 10,
          inside_strata_y = "y",
          preprocess_PCA_thresh = 0.95,
          method_cor = "pearson",
          model_description = "Consider writing a description here",
          multi_cores = TRUE)
T2 <- Sys.time()
T2-T1



final_results[[2]]









library(data.table)
# devtools::document()
#' Individually trains word embeddings from several text variables to several numeric/categorical variables.
#' @param x List of lists comprising several word embeddings from textEmbed.
#' (NB need to remove any word embeddings from the decontextualized single words).
#' @param y Tibble with numeric variables to predict.
#' @param trainMethod Method to train word embeddings (default "regression"; see also "randomForest").
#' @param nrFolds_k Number of folds to use.
#' @param preProcessPCAthresh Pre-processing threshold for amount of variance to retain (default 0.95).
#' @param strata_y Variable to stratify according (default y; can set to NULL).
#' @param methodCor Type of correlation used in evaluation (default "pearson"; can set to "spearman" or "kendall").
#' @param trees Number of trees used in the random forest.
#' @param ... Arguments for the textTrain function.
#' @return Correlations between predicted and observed values.
#' @examples
#' wordembeddings <- wordembeddings4_10[1:2]
#' ratings_data <- Language_based_assessment_data_8_10[5:6]
#' results <- textTrainLists(wordembeddings, ratings_data, nrFolds_k = 2)
#' @seealso see \code{\link{textTrain}}
#' @importFrom stats cor.test
#' @importFrom tibble as_tibble
#' @importFrom magrittr %>%
#' @importFrom dplyr arrange
#' @importFrom data.table %like%
#' @export
textTrainLists_new <- function(x,
                           y,
                           trainMethod = "regression",

                           outside_folds = 10,
                           outside_strata_y = "y",
                           inside_folds = 10,
                           inside_strata_y = "y",
                           preprocess_PCA_thresh = 0.95,
                           method_cor = "pearson",
                           model_description = "Consider writing a description here",
                           multi_cores = TRUE,


                           trees = 500, ...) {
  #  , trees=500 method="regression"  method= "textTrainCVpredictions";  method= "textTrainCVpredictionsRF"

  # Get variable names in the list of outcomes.
  variables <- names(y)
  # Duplicate variable names to as many different word embeddings there are in x.
  variables <- rep(variables, length(x))
  # Create data frame with duplicated variables.
  y1 <- y[c(variables)]
  # Order columns alphabetically.
  y1 <- y1[, order(colnames(y1))]

  # Creating descriptions of which variables are used in training, which is  added to the output.
  descriptions <- paste(rep(names(x), length(y)), "_", names(y1), sep = "")

  if (trainMethod == "regression") {
    # Using mapply to loop over the word embeddings and the outcome variables.
    output <- mapply(textTrainRegression, x, y1,
                     SIMPLIFY = FALSE,
                     MoreArgs = list(
                       outside_folds = outside_folds,
                       outside_strata_y = outside_strata_y,
                       inside_folds = inside_folds,
                       inside_strata_y = inside_strata_y,
                       preprocess_PCA_thresh = preprocess_PCA_thresh,
                       method_cor = method_cor,
                       model_description = model_description,
                       multi_cores = multi_cores, ...
                     )
    )

    output_t <- t(as.data.frame(lapply(output, function(output) unlist(output$correlation)[[1]][c(1)])))
    output_df <-t(as.data.frame(lapply(output, function(output) unlist(output$correlation)[[2]][c(1)])))
    output_p <- t(as.data.frame(lapply(output, function(output) unlist(output$correlation)[[3]][c(1)])))
    output_r <- t(as.data.frame(lapply(output, function(output) unlist(output$correlation)[[4]][c(1)])))
    output_a <- t(as.data.frame(lapply(output, function(output) unlist(output$correlation)[[6]][c(1)])))

    # Add Outcomes and Descriptions together; name the columns; and remove the row names.
    output_ordered_named <- data.frame(cbind(descriptions, output_r, output_df, output_p, output_t, output_a))
    colnames(output_ordered_named) <- c("descriptions", "correlation", "df", "p_value", "t_statistics", "alternative")
    rownames(output_ordered_named) <- NULL

    output_predscore <- as.data.frame(lapply(output, function(output) unlist(output$predictions)))
    output_predscore_reg <- output_predscore[rownames(output_predscore) %like% "predictions", ]
    colnames(output_predscore_reg) <- c(paste(descriptions, "_pred", sep = ""))
    results <- list(output_predscore_reg, output_ordered_named)
    names(results) <- c("predictions", "results")
    results
  } else if (trainMethod == "randomForest") { #
    # Apply textTrainRandomForest function between each list element and sort outcome.
    output <- mapply(textTrainRandomForest, x, y1,
                     SIMPLIFY = FALSE,
                     MoreArgs = list(trees = trees, nrFolds_k = nrFolds_k, strata_y = strata_y)
    )
    output_chi <- t(as.data.frame(lapply(output, function(output) unlist(output$results)[[1]][[1]])))
    output_df <- t(as.data.frame(lapply(output, function(output) unlist(output$results)[[2]][[1]])))
    output_p <- t(as.data.frame(lapply(output, function(output) unlist(output$results)[[3]][[1]])))
    output_p_r <- tibble(output_chi, output_df, output_p)

    # Add Outcomes and Descriptions together; name the columns; and remove the row names.
    output_ordered_named <- data.frame(cbind(descriptions, output_p_r))
    colnames(output_ordered_named) <- c("descriptions", "chi2", "df", "p_value")
    output_ordered_named

    # Get and sort the Prediction scores
    output_predscore1 <- lapply(output, "[[", "predictions")
    names(output_predscore1) <- descriptions
    output_predscore <- do.call(cbind, output_predscore1) %>%
      tibble::as_tibble() %>%
      dplyr::arrange()

    # Combine output
    results <- list(output_predscore, output_ordered_named)
    names(results) <- c("predictions", "results")
    results
  }
}

wordembeddings_list <- wordembeddings4_10[1:2]
ratings_data_list <- Language_based_assessment_data_8_10[5:6]
#results <- textTrainLists(wordembeddings, ratings_data, nrFolds_k = 2)

list_restuls <- textTrainLists_new(x = wordembeddings_list,
                               y = ratings_data_list,
                               trainMethod = "regression",

                               outside_folds = 10,
                               outside_strata_y = "y",
                               inside_folds = 10,
                               inside_strata_y = "y",
                               preprocess_PCA_thresh = 0.95,
                               method_cor = "pearson",
                               model_description = "Consider writing a description here",
                               multi_cores = TRUE,


                               trees = 500)


