

#' Function to find the mode
#' @param x vector with numbers
#' @return  Mode value
#' @noRd
statisticalMode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}



#' Function to fit a model and compute RMSE.
#'
#' @param object An rsplit object (from results_nested_resampling tibble)
#' object = results_nested_resampling$splits[[1]] OR results_nested_resampling$splits[[1]][[1]]
#' @param penalty hyperparameter for ridge regression.
#' @param mixture hyperparameter for ridge regression.
#' @param preprocess_PCA threshold for pca; preprocess_PCA = NA
#' @param variable_name_index_pca variable with names to know how to keep variables
#' from same word embedding together in separate pcas
#' @return  RMSE.
#' @noRd
fit_model_rmse <- function(object, model = "regression", eval_measure = "rmse", penalty = 1, mixture = 0, preprocess_PCA = NA, variable_name_index_pca = NA) {

  # Recipe for one embedding input
  if (colnames(rsample::analysis(object)[1]) == "Dim1") {
    xy_recipe <- rsample::analysis(object) %>%
      recipes::recipe(y ~ .) %>%
      # recipes::step_BoxCox(all_predictors()) %>%  preprocess_PCA = NULL, preprocess_PCA = 0.9 preprocess_PCA = 2
      recipes::update_role(id_nr, new_role = "id variable") %>%
      recipes::update_role(-id_nr, new_role = "predictor") %>%
      recipes::update_role(y, new_role = "outcome") %>%
      recipes::step_naomit(Dim1, skip = TRUE) %>%
      recipes::step_center(recipes::all_predictors()) %>%
      recipes::step_scale(recipes::all_predictors()) %>%
      # If preprocess_PCA is not NULL add PCA step with number of component of % of variance to retain specification
      {
        if (!is.na(preprocess_PCA)) {
          if (preprocess_PCA >= 1) {
            recipes::step_pca(., recipes::all_predictors(), num_comp = preprocess_PCA)
          } else if (preprocess_PCA < 1) {
            recipes::step_pca(., recipes::all_predictors(), threshold = preprocess_PCA)
          } else {
            .
          }
        } else {
          .
        }
      } %>%
      recipes::prep()

    # Recipe for multiple word embedding input (with possibility of separate PCAs)
  } else {
    V1 <- colnames(rsample::analysis(object)[1])
    xy_recipe <- rsample::analysis(object) %>%
      recipes::recipe(y ~ .) %>%
      # recipes::step_BoxCox(all_predictors()) %>%  preprocess_PCA = NULL, preprocess_PCA = 0.9 preprocess_PCA = 2
      recipes::update_role(id_nr, new_role = "id variable") %>%
      recipes::update_role(-id_nr, new_role = "predictor") %>%
      recipes::update_role(y, new_role = "outcome") %>%
      recipes::step_naomit(dplyr::all_of(V1), skip = TRUE) %>%
      recipes::step_center(recipes::all_predictors()) %>%
      recipes::step_scale(recipes::all_predictors())

    # Adding a PCA in each loop; first selecting all variables starting with i="Dim_we1"; and then "Dim_we2" etc
    if (!is.na(preprocess_PCA)) {
      if (preprocess_PCA >= 1) {
        for (i in variable_name_index_pca) {
          xy_recipe <-
            xy_recipe %>%
            # !! slices the current name into the `matches()` function.
            # We use a custom prefix so there are no name collisions for the
            # results of each PCA step.
            recipes::step_pca(dplyr::matches(!!i), num_comp = preprocess_PCA, prefix = paste("PCA_", i, "_"))
        }
      } else if (preprocess_PCA < 1) {
        for (i in variable_name_index_pca) {
          xy_recipe <-
            xy_recipe %>%
            recipes::step_pca(dplyr::matches(!!i), threshold = preprocess_PCA, prefix = paste("PCA_", i, "_"))
        }
      }
    }
    xy_recipe <- recipes::prep(xy_recipe)
  }


  # To load the prepared training data into a variable juice() is used.
  # It extracts the data from the xy_recipe object.
  xy_training <- recipes::juice(xy_recipe)

  # Ridge and/or Lasso
  if (length(xy_training) > 3) {
    # Create and fit model
    mod <-
      {
        if (model == "regression") {
          parsnip::linear_reg(penalty = penalty, mixture = mixture)
        } else if (model == "logistic") parsnip::logistic_reg(mode = "classification", penalty = penalty, mixture = mixture)
      } %>%
      parsnip::set_engine("glmnet") %>%
      parsnip::fit(y ~ ., data = xy_training)

    # Standard regression
  } else if (length(xy_training) == 3) {
    mod <-
      {
        if (model == "regression") {
          parsnip::linear_reg(mode = "regression") %>%
            parsnip::set_engine("lm")
        } else if (model == "logistic") {
          parsnip::logistic_reg(mode = "classification") %>%
            parsnip::set_engine("glm")
        }
      } %>%
      parsnip::fit(y ~ ., data = xy_training)
  }
  # Prepare the test data according to the recipe
  xy_testing <- xy_recipe %>%
    recipes::bake(rsample::assessment(object))
  # look at xy_testing: glimpse(xy_testing)

  if (model == "regression") {
    # Apply model on new data; penalty
    holdout_pred <-
      stats::predict(mod, xy_testing %>% dplyr::select(-y)) %>%
      dplyr::bind_cols(rsample::assessment(object) %>% dplyr::select(y, id_nr))

    # Get RMSE; eval_measure = "rmse"
    eval_result <- select_eval_measure_val(eval_measure, holdout_pred = holdout_pred, truth = y, estimate = .pred)$.estimate
    # Sort output of RMSE, predictions and truth (observed y)
    output <- list(list(eval_result), list(holdout_pred$.pred), list(holdout_pred$y), list(preprocess_PCA), list(holdout_pred$id_nr))
    names(output) <- c("eval_result", "predictions", "y", "preprocess_PCA", "id_nr")
  } else if (model == "logistic") {
    holdout_pred_class <-
      stats::predict(mod, xy_testing %>% dplyr::select(-y), type = c("class")) %>%
      dplyr::bind_cols(rsample::assessment(object) %>% dplyr::select(y, id_nr))
    holdout_pred <-
      stats::predict(mod, xy_testing %>% dplyr::select(-y), type = c("prob")) %>%
      dplyr::bind_cols(rsample::assessment(object) %>% dplyr::select(y, id_nr))

    holdout_pred$.pred_class <- holdout_pred_class$.pred_class
    class <- colnames(holdout_pred[1])

    # Get RMSE; eval_measure = "rmse"
    eval_result <- select_eval_measure_val(eval_measure, holdout_pred = holdout_pred, truth = y, estimate = .pred_class)$.estimate
    # Sort output of RMSE, predictions and truth (observed y)
    output <- list(
      list(eval_result),
      list(holdout_pred$.pred_class),
      list(holdout_pred$y),
      list(holdout_pred[1]),
      list(holdout_pred[2]),
      list(preprocess_PCA),
      list(holdout_pred$id_nr)
    )
    names(output) <- c(
      "eval_result",
      "estimate",
      "truth",
      ".pred_1",
      ".pred_2",
      "preprocess_PCA",
      "id_nr"
    )
  }
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
#' @param variable_name_index_pca variable with names to know how to keep variables
#' from same word embedding together in separate pcas.
#' @return RMSE.
#' @noRd
fit_model_rmse_wrapper <- function(penalty = penalty,
                                   mixture = mixture,
                                   object,
                                   model,
                                   eval_measure,
                                   preprocess_PCA = preprocess_PCA,
                                   variable_name_index_pca = variable_name_index_pca) {
  fit_model_rmse(object,
    model,
    eval_measure,
    penalty,
    mixture,
    preprocess_PCA = preprocess_PCA,
    variable_name_index_pca = variable_name_index_pca
  )
}

#' For the nested resampling, a model needs to be fit for each tuning parameter and each INNER split.
#'
#' @param object an rsplit object from the INNER samples
#' object=results_nested_resampling$inner_resamples[[1]]$splits[[1]]
#' @param penalty hyperparameter for ridge regression.
#' @param mixture hyperparameter for ridge regression.
#' @param preprocess_PCA threshold for pca
#' @param variable_name_index_pca variable with names to know how to keep variables
#' from same word embedding together in separate pcas
#' @return RMSE.
#' @noRd
tune_over_cost <- function(object, model, eval_measure, penalty, mixture, preprocess_PCA = preprocess_PCA, variable_name_index_pca = variable_name_index_pca) {

  # Number of components or percent of variance to attain; min_halving; preprocess_PCA = NULL
  if (!is.na(preprocess_PCA[1])) {
    if (preprocess_PCA[1] == "min_halving") {
      num_features <- length(rsample::analysis(object)) - 1
      num_users <- nrow(rsample::analysis(object))
      preprocess_PCA_value <- round(max(min(num_features / 2, num_users / 1.5), min(50, num_features)))
      preprocess_PCA_value
    } else if (preprocess_PCA[1] >= 1) {
      preprocess_PCA_value <- preprocess_PCA
    } else if (preprocess_PCA[1] < 1) {
      preprocess_PCA_value <- preprocess_PCA
    } else {
      preprocess_PCA_value <- NA
    }
  }

  if (is.na(preprocess_PCA[1])) {
    preprocess_PCA_value <- NA
  }

  grid_inner <- base::expand.grid(
    penalty = penalty,
    mixture = mixture,
    preprocess_PCA = preprocess_PCA_value
  )

  # Test models with the different hyperparameters for the inner samples
  tune_results <- purrr::pmap(list(
    grid_inner$penalty,
    grid_inner$mixture,
    grid_inner$preprocess_PCA
  ),
  fit_model_rmse_wrapper,
  object = object,
  model = model,
  eval_measure = eval_measure,
  variable_name_index_pca = variable_name_index_pca
  )

  # Sort the output to separate the rmse, predictions and truth
  tune_outputlist <- tune_results %>%
    dplyr::bind_rows() %>%
    split.default(names(.)) %>%
    purrr::map(na.omit)

  # Extract the RMSE
  tune_eval_result <- unlist(tune_outputlist$eval_result$eval_result)

  # Add RMSE to the grid
  grid_inner_eval_result <- grid_inner %>%
    dplyr::mutate(eval_result = tune_eval_result)

  grid_inner_eval_result
}


#' # Since this will be called across the set of OUTER cross-validation splits, another wrapper is required:
#'
#' @param object An rsplit object from the INNER samples
#' object = results_nested_resampling$inner_resamples[[1]]
#' @param penalty hyperparameter for ridge regression.
#' @param mixture hyperparameter for ridge regression.
#' @param preprocess_PCA threshold for pca
#' @param variable_name_index_pca variable with names to know how to keep variables
#' from same word embedding together in separate pcas
#' @return RMSE with corresponding penalty, mixture and preprocess_PCA.
#' @noRd
summarize_tune_results <- function(object,
                                   model,
                                   eval_measure,
                                   penalty,
                                   mixture,
                                   preprocess_PCA = preprocess_PCA,
                                   variable_name_index_pca = variable_name_index_pca) {

  # Return row-bound tibble containing the INNER results
  purrr::map_df(
    .x = object$splits,
    .f = tune_over_cost,
    penalty = penalty,
    mixture = mixture,
    preprocess_PCA = preprocess_PCA,
    variable_name_index_pca = variable_name_index_pca,
    model = model,
    eval_measure = eval_measure
  )
}



# devtools::document()
#' Train word embeddings to a numeric variable.
#'
#' @param x Word embeddings from textEmbed (or textEmbedLayerAggreation).
#' @param y Numeric variable to predict.
#' @param model Type of model. Default is "regression"; see also "logistic" for classification.
#' @param outside_folds_v Number of folds for the outer folds (default = 10).
#' @param outside_strata_y Variable to stratify according (default y; can set to NULL).
#' @param inside_folds_prop Number of folds for the inner folds (default proportion = 3/4).
#' @param inside_strata_y Variable to stratify according (default y; can set to NULL).
#' @param eval_measure Type of evaluative measure to select models from; default =  "rmse" for regression and "bal_accuracy"
#' for logistic.
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
#' @param multi_cores If TRUE it enables the use of multiple cores if the computer system allows for it (i.e., only on unix, not windows). Hence it
#' makes the analyses considerably faster to run. Default is "multi_cores_sys_default", where it automatically uses TRUE for Mac and Linux and FALSE for Windows.
#' @param save_output Option not to save all output; default "all". see also "only_results" and "only_results_predictions".
#' @param seed Set different seed.
#' @return A (one-sided) correlation test between predicted and observed values; tibble of predicted values, as well as information
#' about the model (preprossing_recipe, final_model and model_description).
#' @examples
#' \dontrun{
#' wordembeddings <- wordembeddings4
#' ratings_data <- Language_based_assessment_data_8
#'
#' results <- textTrainRegression(
#'   wordembeddings$harmonytext,
#'   ratings_data$hilstotal
#' )
#' }
#' @seealso see \code{\link{textEmbedLayerAggreation}} \code{\link{textTrainLists}}
#' \code{\link{textTrainRandomForest}} \code{\link{textSimilarityTest}}
#' @importFrom stats cor.test na.omit lm
#' @importFrom dplyr select starts_with filter all_of
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
                                outside_folds_v = 10,
                                outside_strata_y = "y",
                                inside_folds_prop = 3 / 4,
                                inside_strata_y = "y",
                                model = "regression",
                                eval_measure = "default",
                                preprocess_PCA = NA,
                                penalty = 10^seq(-16, 16),
                                mixture = c(0),
                                method_cor = "pearson",
                                model_description = "Consider writing a description of your model here",
                                multi_cores = "multi_cores_sys_default",
                                save_output = "all",
                                seed = 2020) {
  T1_textTrainRegression <- Sys.time()

  set.seed(seed)

  variable_name_index_pca <- NA

  # Select correct eval_measure depending on model when default
  if (model == "regression" & eval_measure == "default") {
    eval_measure <- "rmse"
  } else if (model == "logistic" & eval_measure == "default") {
    eval_measure <- "bal_accuracy"
  }

  # In case one embedding is in list form get the tibble form
  if (!tibble::is_tibble(x) & length(x) == 1) {
    x1 <- x[[1]]
    # Get names for description
    x_name <- names(x)
    embedding_description <- comment(x[[1]])
    # In case there are several embeddings in list form get the x_names and embedding description for model description
  } else if (!tibble::is_tibble(x) & length(x) > 1) {
    x_name <- names(x)
    x_name <- paste(x_name, sep = " ", collapse = " & ")
    x_name <- paste("input:", x_name, sep = " ", collapse = " ")

    embedding_description <- comment(x[[1]])
    # In case it is just one word embedding as tibble
  } else {
    x1 <- x
    x_name <- deparse(substitute(x))
    embedding_description <- comment(x)
  }


  if (tibble::is_tibble(y) | is.data.frame(y)) {
    y_name <- colnames(y)
    y <- y[[1]]
  } else {
    y_name <- deparse(substitute(y))
    y <- y
  }


  ############ Arranging word embeddings to be concatenated from different texts ############
  ##################################################

  if (!tibble::is_tibble(x) & length(x) > 1) {

    # Select all variables that starts with Dim in each dataframe of the list.
    xlist <- lapply(x, function(X) {
      X <- dplyr::select(X, dplyr::starts_with("Dim"))
    })

    Nword_variables <- length(xlist)
    # Give each column specific names with indexes so that they can be handled separately in the PCAs
    for (i in 1:Nword_variables) {
      colnames(xlist[[i]]) <- paste("Dim_we", i, ".", names(xlist[i]), colnames(xlist[[i]]), sep = "")
    }

    # Make vector with each index so that we can allocate them separately for the PCAs
    variable_name_index_pca <- list()
    for (i in 1:Nword_variables) {
      variable_name_index_pca[i] <- paste("Dim_we", i, sep = "")
    }

    # Make one df rather then list.
    x1 <- dplyr::bind_cols(xlist)

    # Get the name of the first variable; which is used to exclude NA (i.e., word embedding have NA in all columns)
    V1 <- colnames(x)[1]
  }

  ############ End for multiple word embeddings ############
  ##########################################################

  x2 <- dplyr::select(x1, dplyr::starts_with("Dim"))
  xy <- cbind(x2, y)
  xy <- tibble::as_tibble(xy)
  xy$id_nr <- c(seq_len(nrow(xy)))

  results_nested_resampling <- rlang::expr(rsample::nested_cv(xy,
    outside = rsample::vfold_cv(
      v = !!outside_folds_v,
      repeats = 1,
      strata = !!outside_strata_y,
      breaks = 2
    ), #
    inside = rsample::validation_split(
      prop = !!inside_folds_prop,
      strata = !!inside_strata_y, #
      breaks = 1
    )
  ))
  results_nested_resampling <- rlang::eval_tidy(results_nested_resampling)

  # Deciding whether to use multicores depending on system and settings.
  if (multi_cores == "multi_cores_sys_default") {
    if (.Platform$OS.type == "unix") {
      multi_cores_use <- TRUE
    } else if (.Platform$OS.type == "windows") {
      multi_cores_use <- FALSE
    }
  } else if (multi_cores == TRUE) {
    multi_cores_use <- TRUE
  } else if (multi_cores == FALSE) {
    multi_cores_use <- FALSE
  }



  if (multi_cores_use == FALSE) {
    tuning_results <- purrr::map(
      .x = results_nested_resampling$inner_resamples,
      .f = summarize_tune_results,
      model = model,
      eval_measure = eval_measure,
      penalty = penalty,
      mixture = mixture,
      preprocess_PCA = preprocess_PCA,
      variable_name_index_pca = variable_name_index_pca
    )
  } else if (multi_cores_use == TRUE) {
    # The multisession plan uses the local cores to process the inner resampling loop. help(multisession)
    future::plan(future::multisession)
    # The object tuning_results is a list of data frames for each of the OUTER resamples.
    tuning_results <- furrr::future_map(
      .x = results_nested_resampling$inner_resamples,
      .f = summarize_tune_results,
      model = model,
      eval_measure = eval_measure,
      penalty = penalty,
      mixture = mixture,
      preprocess_PCA = preprocess_PCA,
      variable_name_index_pca = variable_name_index_pca
    )
  }

  # Function to get the lowest mean_RMSE
  bestParameters <- function(dat) dat[which.min(dat$eval_result), ]

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
  results_outer <- purrr::pmap(
    list(
      object = results_nested_resampling$splits,
      penalty = results_split_parameter$penalty,
      mixture = results_split_parameter$mixture,
      preprocess_PCA = results_split_parameter$preprocess_PCA,
      variable_name_index_pca = list(variable_name_index_pca),
      model = model,
      eval_measure = eval_measure
    ),
    fit_model_rmse
  )

  # Separate RMSE, predictions and observed y
  outputlist_results_outer <- results_outer %>%
    dplyr::bind_rows() %>%
    split.default(names(.)) %>%
    purrr::map(na.omit)


  # Get overall evaluation measure between predicted and observed values
  if (model == "regression") {
    # Unnest predictions and y
    predy_y <- tibble::tibble(
      tidyr::unnest(outputlist_results_outer$predictions, cols = c(predictions)),
      tidyr::unnest(outputlist_results_outer$y, cols = c(y)),
      tidyr::unnest(outputlist_results_outer$id_nr, cols = c(id_nr))
    )
    predy_y <- predy_y %>% dplyr::arrange(id_nr)
    # Correlate predictions and observed correlation
    collected_results <- stats::cor.test(predy_y$predictions, predy_y$y, method = method_cor, alternative = "greater")


    collected_results <- list(predy_y, collected_results)
  } else if (model == "logistic") {
    collected_results <- classification_results(outputlist_results_outer = outputlist_results_outer)

    #  Save predictions outside list to make similar structure as model == regression output.
    predy_y <- collected_results$predy_y
    # Remove the predictions from list
    collected_results[[1]] <- NULL
  }


  ##### Construct final model to be saved and applied on other data  ########
  ############################################################################

  xy_short <- xy

  ######### One word embedding as input
  if (colnames(xy_short[1]) == "Dim1") {
    # [0,] is added to just get the col names (and avoid saving all the data with the receipt)
    final_recipe <- # xy %>%
      recipes::recipe(y ~ ., xy_short[0, ]) %>%
      recipes::update_role(id_nr, new_role = "id variable") %>%
      recipes::update_role(-id_nr, new_role = "predictor") %>%
      recipes::update_role(y, new_role = "outcome") %>%
      # recipes::step_BoxCox(all_predictors()) %>%
      recipes::step_naomit(Dim1, skip = TRUE) %>%
      recipes::step_center(recipes::all_predictors()) %>%
      recipes::step_scale(recipes::all_predictors()) %>%
      {
        if (!is.na(preprocess_PCA[1])) {
          if (preprocess_PCA[1] >= 1) {
            recipes::step_pca(., recipes::all_predictors(), num_comp = statisticalMode(results_split_parameter$preprocess_PCA))
          } else if (preprocess_PCA[1] < 1) {
            recipes::step_pca(., recipes::all_predictors(), threshold = statisticalMode(results_split_parameter$preprocess_PCA))
          } else {
            .
          }
        } else {
          .
        }
      }

    ######### More than one word embeddings as input help(all_of)
  } else {
    V1 <- colnames(xy_short[1])

    final_recipe <- recipes::recipe(y ~ ., xy_short[0, ]) %>%
      recipes::update_role(id_nr, new_role = "id variable") %>%
      recipes::update_role(-id_nr, new_role = "predictor") %>%
      recipes::update_role(y, new_role = "outcome") %>%
      recipes::step_naomit(dplyr::all_of(V1), skip = TRUE) %>%
      recipes::step_center(recipes::all_predictors()) %>%
      recipes::step_scale(recipes::all_predictors())

    # Adding a PCA in each loop; first selecting all variables starting with i="Dim_we1"; and then "Dim_we2" etc
    if (!is.na(preprocess_PCA)) {
      if (preprocess_PCA >= 1) {
        for (i in variable_name_index_pca) {
          final_recipe <-
            final_recipe %>%
            # !! slices the current name into the `matches()` function.
            # We use a custom prefix so there are no name collisions for the
            # results of each PCA step.
            recipes::step_pca(dplyr::matches(!!i), num_comp = preprocess_PCA, prefix = paste("PCA_", i, "_"))
        }
        # }
      } else if (preprocess_PCA < 1) {
        for (i in variable_name_index_pca) {
          final_recipe <-
            final_recipe %>%
            # recipes::step_pca(., recipes::all_predictors(), threshold = preprocess_PCA)
            recipes::step_pca(dplyr::matches(!!i), threshold = preprocess_PCA, prefix = paste("PCA_", i, "_"))
        }
      }
    }
  }

  #
  preprocessing_recipe_save <- suppressWarnings(recipes::prep(final_recipe, xy_short, retain = FALSE))
  preprocessing_recipe_use <- recipes::prep(final_recipe, xy_short)
  # To load the prepared training data into a variable juice() is used.
  # It extracts the data from the xy_recipe object.
  xy_final <- recipes::juice(preprocessing_recipe_use)

  if (length(xy_final) > 3) {
    # Create and fit model; penalty=NULL mixture = NULL
    final_predictive_model <-
      {
        if (model == "regression") {
          parsnip::linear_reg(penalty = statisticalMode(results_split_parameter$penalty), mixture = statisticalMode(results_split_parameter$mixture))
        } else if (model == "logistic") parsnip::logistic_reg(mode = "classification", penalty = statisticalMode(results_split_parameter$penalty), mixture = statisticalMode(results_split_parameter$mixture))
      } %>%
      parsnip::set_engine("glmnet") %>%
      parsnip::fit(y ~ ., data = xy_final)

    # Standard regression
  } else if (length(xy_final) == 3) {
    final_predictive_model <-
      {
        if (model == "regression") {
          parsnip::linear_reg(mode = "regression") %>%
            parsnip::set_engine("lm")
        } else if (model == "logistic") {
          parsnip::logistic_reg(mode = "classification") %>%
            parsnip::set_engine("glm")
        }
      } %>%
      parsnip::fit(y ~ ., data = xy_final)
  }



  ##########  DESCRIBING THE MODEL  ##########
  ############################################

  outside_folds_v_description <- paste("outside_folds_v = ", deparse(outside_folds_v))
  outside_strata_y_description <- paste("outside_strata_y = ", deparse(outside_strata_y))
  inside_folds_prop_description <- paste("inside_folds_prop = ", deparse(inside_folds_prop))
  inside_strata_y_description <- paste("inside_strata_y = ", deparse(inside_strata_y))

  penalty_setting <- paste("penalty_setting = ", deparse(penalty))
  mixture_setting <- paste("mixture_setting = ", deparse(mixture))
  preprocess_PCA_setting <- paste("preprocess_PCA_setting = ", deparse(preprocess_PCA))

  # Saving the final mtry and min_n used for the final model.
  penalty_description <- paste("penalty in final model = ", deparse(statisticalMode(results_split_parameter$penalty)))
  penalty_fold_description <- paste("penalty in each fold = ", deparse(results_split_parameter$penalty))

  mixture_description <- paste("mixture in final model = ", deparse(statisticalMode(results_split_parameter$mixture)))
  mixture_fold_description <- paste("mixture in each fold = ", deparse(results_split_parameter$mixture))

  preprocess_PCA_description <- paste("preprocess_PCA in final model = ", deparse(statisticalMode(results_split_parameter$preprocess_PCA)))
  preprocess_PCA_fold_description <- paste("preprocess_PCA in each fold = ", deparse(results_split_parameter$preprocess_PCA))

  # Getting time and date
  T2_textTrainRegression <- Sys.time()
  Time_textTrainRegression <- T2_textTrainRegression - T1_textTrainRegression
  Time_textTrainRegression <- sprintf("Duration to train text: %f %s", Time_textTrainRegression, units(Time_textTrainRegression))
  Date_textTrainRegression <- Sys.time()
  time_date <- paste(Time_textTrainRegression,
    "; Date created: ", Date_textTrainRegression,
    sep = "",
    collapse = " "
  )

  # Describe model; adding user's-description + the name of the x and y
  model_description_detail <- c(
    x_name,
    y_name,
    outside_folds_v_description,
    outside_strata_y_description,
    inside_folds_prop_description,
    inside_strata_y_description,
    penalty_setting,
    penalty_description,
    penalty_fold_description,
    mixture_setting,
    mixture_description,
    mixture_fold_description,
    preprocess_PCA_setting,
    preprocess_PCA_description,
    preprocess_PCA_fold_description,
    embedding_description,
    model_description,
    time_date
  )

  ###### Saving and arranging output ######
  ##########################################

  if (model == "regression") {
    if (save_output == "all") {
      final_results <- list(
        predy_y, preprocessing_recipe_save, final_predictive_model, model_description_detail,
        collected_results[[2]]
      )
      names(final_results) <- c(
        "predictions", "final_recipe", "final_model", "model_description",
        "results"
      )
    } else if (save_output == "only_results_predictions") {
      final_results <- list(
        predy_y, model_description_detail,
        collected_results[[2]]
      )
      names(final_results) <- c("predictions", "model_description", "results")
    } else if (save_output == "only_results") {
      final_results <- list(
        model_description_detail,
        collected_results[[2]]
      )
      names(final_results) <- c("model_description", "results")
    }
  } else if (model == "logistic") {
    if (save_output == "all") {
      final_results <- list(
        predy_y, preprocessing_recipe_save, final_predictive_model, model_description_detail,
        collected_results$roc_curve_data, collected_results$roc_curve_plot, collected_results$fisher,
        collected_results$chisq, collected_results$results_collected
      )
      names(final_results) <- c(
        "predictions", "final_recipe", "final_model", "model_description",
        "roc_curve_data", "roc_curve_plot", "fisher", "chisq", "results_metrics"
      )
      final_results
    } else if (save_output == "only_results_predictions") {
      final_results <- list(
        predy_y, model_description_detail,
        collected_results$roc_curve_data, collected_results$roc_curve_plot, collected_results$fisher,
        collected_results$chisq, collected_results$results_collected
      )
      names(final_results) <- c(
        "predictions", "model_description",
        "roc_curve_data", "roc_curve_plot", "fisher", "chisq", "results_metrics"
      )
      final_results
    } else if (save_output == "only_results") {
      final_results <- list(
        model_description_detail,
        collected_results$roc_curve_data, collected_results$roc_curve_plot, collected_results$fisher,
        collected_results$chisq, collected_results$results_collected
      )
      names(final_results) <- c(
        "model_description",
        "roc_curve_data", "roc_curve_plot", "fisher", "chisq", "results_metrics"
      )
      final_results
    }
    final_results
  }
  final_results
}
