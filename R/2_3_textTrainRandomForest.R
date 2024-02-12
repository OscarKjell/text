#' Select evaluation measure and compute it (also used in logistic regression)
#'
#' @param eval_measure Measure used to evaluate and select models default: "roc_auc", see also
#' "accuracy", "bal_accuracy", "sens", "spec", "precision", "kappa", "f_measure".
#' @param holdout_pred data with observed (truth) and predicted
#' @param truth name of the truth column
#' @param estimate name of the predicted estimate
#' @return  tibble with .metric column (i.e., type of evaluation measure), .estimator (e.g., binary)
#' and .estimate (i.e., the resulting value).
#' @importFrom  dplyr all_of
#' @importFrom  yardstick accuracy bal_accuracy sens spec precision kap f_meas roc_auc rmse rsq
#' @importFrom  tibble tibble
#' @importFrom  stats cor.test
#' @noRd
select_eval_measure_val <- function(eval_measure = "bal_accuracy",
                                    holdout_pred = NULL,
                                    truth = y,
                                    estimate = .pred_class, class,
                                    ...) {
  # Get accuracy
  if (eval_measure == "accuracy") {
    eval_measure_val <- yardstick::accuracy(holdout_pred, truth = y, estimate = .pred_class)
  } else if (eval_measure == "bal_accuracy") {
    eval_measure_val <- yardstick::bal_accuracy(holdout_pred, truth = y, estimate = .pred_class)
  } else if (eval_measure == "sens") {
    eval_measure_val <- yardstick::sens(holdout_pred, truth = y, estimate = .pred_class)
  } else if (eval_measure == "spec") {
    eval_measure_val <- yardstick::spec(holdout_pred, truth = y, estimate = .pred_class)
  } else if (eval_measure == "precision") {
    eval_measure_val <- yardstick::precision(holdout_pred, truth = y, estimate = .pred_class)
  } else if (eval_measure == "kappa") {
    eval_measure_val <- yardstick::kap(holdout_pred, truth = y, estimate = .pred_class)
  } else if (eval_measure == "f_measure") {
    eval_measure_val <- yardstick::f_meas(holdout_pred, truth = y, estimate = .pred_class)
  } else if (eval_measure == "roc_auc") {
    class1_name <- eval(class)
    eval_measure_val <- yardstick::roc_auc(holdout_pred, truth = y, dplyr::all_of(class1_name))
  } else if (eval_measure == "rmse") {
    eval_measure_val <- yardstick::rmse(holdout_pred, truth = y, estimate = .pred)
  } else if (eval_measure == "rsq") {
    eval_measure_val <- yardstick::rsq(holdout_pred, truth = y, estimate = .pred)
  } else if (eval_measure == "cor_test") {
    cor_testing <- stats::cor.test(holdout_pred$y, holdout_pred$.pred, na.action = na.omit)
    estimate1 <- cor_testing[[4]][[1]]
    metric <- "cor_test"
    estimator <- "standard"
    eval_measure_val <- tibble::tibble(metric, estimator, as.numeric(estimate1))
    colnames(eval_measure_val) <- c(".metric", ".estimator", ".estimate")
  }
  eval_measure_val
}

#' Select evaluation measure and compute it (also used in logistic regression)
#'
#' @param outputlist_results_outer Results from outer predictions.
#' @param id_nr ID numbers
#' @param simulate.p.value (Boolean) From fisher.test: a logical indicating whether to
#' compute p-values by Monte Carlo simulation, in larger than 2 × 2 tables.
#' @return returns sorted predictions and truth, chi-square test, fisher test, and all evaluation metrics/measures
#' @importFrom  tidyr unnest
#' @importFrom  tibble is_tibble
#' @importFrom  dplyr full_join arrange bind_rows
#' @importFrom  stats fisher.test
#' @importFrom  yardstick accuracy bal_accuracy sens spec precision kap f_meas roc_auc rmse rsq
#' @importFrom  ggplot2 autoplot
#' @noRd
classification_results <- function(outputlist_results_outer,
                                   id_nr = NA,
                                   simulate.p.value = simulate.p.value,
                                   ...) {
  # Unnest predictions and y
  predy_y <- tibble::tibble(
    tidyr::unnest(outputlist_results_outer$truth, cols = c(truth)),
    tidyr::unnest(outputlist_results_outer$estimate, cols = c(estimate)),
    tidyr::unnest(outputlist_results_outer$.pred_1, cols = c(.pred_1)),
    tidyr::unnest(outputlist_results_outer$.pred_2, cols = c(.pred_2)),
    tidyr::unnest(outputlist_results_outer$id_nr, cols = c(id_nr))
  )

  if (tibble::is_tibble(id_nr)) {
    predy_y <- predy_y %>% dplyr::full_join(id_nr, by = "id_nr")
  }

  predy_y <- predy_y %>% dplyr::arrange(id_nr)
  # Correlate predictions and observed help(all_of)
  chisq <- suppressWarnings(chisq.test(table(predy_y$truth, predy_y$estimate)))

  fisher <- stats::fisher.test(predy_y$truth,
    predy_y$estimate,
    simulate.p.value = simulate.p.value
  )


  accuracy <- yardstick::accuracy(predy_y, truth, estimate, ...)
  bal_accuracy <- yardstick::bal_accuracy(predy_y, truth, estimate, ...)
  sens <- yardstick::sens(predy_y, truth, estimate, ...)
  spec <- yardstick::spec(predy_y, truth, estimate, ...)
  precision <- yardstick::precision(predy_y, truth, estimate, ...)
  kappa <- yardstick::kap(predy_y, truth, estimate, ...)
  f_measure <- yardstick::f_meas(predy_y, truth, estimate, ...)

  roc_auc <- yardstick::roc_auc(predy_y, truth, colnames(predy_y[3]))
  roc_curve_data <- yardstick::roc_curve(predy_y, truth, colnames(predy_y[3]))

  roc_curve_plot <- ggplot2::autoplot(yardstick::roc_curve(predy_y, truth, colnames(predy_y[3])))

  results_collected <- dplyr::bind_rows(
    accuracy,
    bal_accuracy,
    sens,
    spec,
    precision,
    kappa,
    f_measure,
    roc_auc
  )

  output <- list(predy_y, roc_curve_data, roc_curve_plot, fisher, chisq, results_collected)
  names(output) <- c("predy_y", "roc_curve_data", "roc_curve_plot", "fisher", "chisq", "results_collected")
  output
}

#' Select evaluation measure and compute it (also used in logistic regression)
#'
#' @param outputlist_results_outer Results from outer predictions.
#' @param id_nr ID numbers
#' @param simulate.p.value (Boolean) From fisher.test: a logical indicating whether to
#'  compute p-values by Monte Carlo simulation, in larger than 2 × 2 tables.
#' @return returns sorted predictions and truth, chi-square test, fisher test, and all evaluation metrics/measures
#' @importFrom  tidyr unnest
#' @importFrom  tibble is_tibble
#' @importFrom  dplyr full_join arrange bind_rows
#' @importFrom  stats fisher.test
#' @importFrom  yardstick accuracy bal_accuracy sens spec precision kap f_meas roc_auc rmse rsq
#' @importFrom  ggplot2 autoplot
#' @noRd
classification_results_multi <- function(outputlist_results_outer,
                                         id_nr = NA,
                                         simulate.p.value = FALSE,
                                         ...) {
  predy_y <- tibble::tibble(
    tidyr::unnest(outputlist_results_outer$truth, cols = c(truth)),
    tidyr::unnest(outputlist_results_outer$estimate, cols = c(estimate)),
    tidyr::unnest(outputlist_results_outer$id_nr, cols = c(id_nr))
  )
  for (i in 1:length(unique(levels(outputlist_results_outer$truth$truth[[1]]))))
  {
    predy_y[3 + i] <- tibble::tibble(tidyr::unnest(outputlist_results_outer[[i]], cols = c(paste(".pred_", i, sep = ""))))
  }

  if (tibble::is_tibble(id_nr)) {
    predy_y <- predy_y %>% dplyr::full_join(id_nr, by = "id_nr")
  }

  predy_y <- predy_y %>% dplyr::arrange(id_nr)
  # Correlate predictions and observed help(all_of)

  chisq <- suppressWarnings(chisq.test(table(predy_y$truth, predy_y$estimate)))

  fisher <- stats::fisher.test(predy_y$truth,
    predy_y$estimate,
    simulate.p.value = simulate.p.value
  )


  accuracy <- yardstick::accuracy(predy_y, truth, estimate, ...)
  bal_accuracy <- yardstick::bal_accuracy(predy_y, truth, estimate, ...)
  sens <- yardstick::sens(predy_y, truth, estimate, ...)
  spec <- yardstick::spec(predy_y, truth, estimate, ...)
  precision <- yardstick::precision(predy_y, truth, estimate, ...)
  kappa <- yardstick::kap(predy_y, truth, estimate, ...)
  f_measure <- yardstick::f_meas(predy_y, truth, estimate, ...)

  roc_auc <- yardstick::roc_auc(predy_y, truth, colnames(predy_y[4:(length(predy_y))]))
  roc_curve_data <- yardstick::roc_curve(predy_y, truth, colnames(predy_y[4:(length(predy_y))]))

  roc_curve_plot <- ggplot2::autoplot(yardstick::roc_curve(predy_y, truth, colnames(predy_y[4:(length(predy_y))])))

  results_collected <- dplyr::bind_rows(
    accuracy,
    bal_accuracy,
    sens,
    spec,
    precision,
    kappa,
    f_measure,
    roc_auc
  )

  output <- list(predy_y, roc_curve_data, roc_curve_plot, fisher, chisq, results_collected)
  names(output) <- c("predy_y", "roc_curve_data", "roc_curve_plot", "fisher", "chisq", "results_collected")
  output
}

#' Function to fit a model and compute accuracy
#'
#' @param object An rsplit object (from results_nested_resampling tibble)
#' object = results_nested_resampling$splits[[1]]
#' @param mode_rf default "classification"; see also "unknown" and "regression".
#' @param extremely_randomised_splitrule default: "extratrees", see also: "gini" or "hellinger"
#' @param mtry hyperparameter for random forest.
#' @param min_n hyperparameter for random forest.
#' @param trees number of trees
#' @param eval_measure Measure used to evaluate and select models default: "roc_auc", see also
#' "accuracy", "bal_accuracy", "sens", "spec", "precision", "kappa", "f_measure".
#' @return  Accuracy
#' @noRd
fit_model_accuracy_rf <- function(object,
                                  mode_rf = "classification",
                                  mtry = 1,
                                  min_n = 1,
                                  trees = 1000,
                                  preprocess_step_center = TRUE,
                                  preprocess_scale_center = TRUE,
                                  preprocess_PCA = NA,
                                  variable_name_index_pca = NA,
                                  eval_measure = "bal_accuracy",
                                  extremely_randomised_splitrule = NULL) {
  data_train <- rsample::analysis(object)
  data_train <- tibble::as_tibble(data_train)

  # Get number of embeddings provided
  n_embeddings <- as.numeric(comment(eval_measure))

  # Recipe for one embedding input
  if (n_embeddings == 1) {
    xy_recipe <- rsample::analysis(object) %>%
      recipes::recipe(y ~ .) %>%
      recipes::update_role(id_nr, new_role = "id variable") %>% # New
      # recipes::update_role(-id_nr, new_role = "predictor") %>% # New
      recipes::update_role(y, new_role = "outcome") # %>% # New

    if ("strata" %in% colnames(data_train)) {
      xy_recipe <- xy_recipe %>%
        recipes::update_role(strata, new_role = "strata")
    }

    xy_recipe <- xy_recipe %>%
      recipes::step_naomit(recipes::all_predictors(), skip = FALSE) # %>%
    # recipes::step_BoxCox(recipes::all_predictors()) %>%

    if (preprocess_step_center) {
      xy_recipe <- recipes::step_center(xy_recipe, recipes::all_predictors())
    }
    if (preprocess_scale_center) {
      xy_recipe <- recipes::step_scale(xy_recipe, recipes::all_predictors())
    }

    # If preprocess_PCA is not NULL add PCA step with number of component of % of variance to retain specification
    xy_recipe <- xy_recipe %>%
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
      }
    xy_recipe_prep <- recipes::prep(xy_recipe)

    # Recipe for multiple word embedding input (with possibility of separate PCAs)
  } else {
    xy_recipe <- rsample::analysis(object) %>%
      recipes::recipe(y ~ .) %>%
      # recipes::step_BoxCox(all_predictors()) %>%  preprocess_PCA = NULL, preprocess_PCA = 0.9 preprocess_PCA = 2
      recipes::update_role(id_nr, new_role = "id variable") %>%
      # recipes::update_role(-id_nr, new_role = "predictor") %>%
      recipes::update_role(y, new_role = "outcome") # %>%

    if ("strata" %in% colnames(data_train)) {
      xy_recipe <- xy_recipe %>%
        recipes::update_role(strata, new_role = "strata")
    }

    xy_recipe <- xy_recipe %>%
      recipes::step_naomit(recipes::all_predictors(), skip = FALSE) %>%
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
            recipes::step_pca(dplyr::matches(!!i), num_comp = preprocess_PCA, prefix = paste("PCA", i, sep = "_"))
        }
      } else if (preprocess_PCA < 1) {
        for (i in variable_name_index_pca) {
          xy_recipe <-
            xy_recipe %>%
            recipes::step_pca(dplyr::matches(!!i), threshold = preprocess_PCA, prefix = paste("PCA", i, sep = "_"))
        }
      }
    }
    xy_recipe_prep <- recipes::prep(xy_recipe)
    xy_recipe_prep
  }

  # To load the prepared training data into a variable juice() is used.
  # It extracts the data from the xy_recipe object.

  # Number of predictors (if more than one should use standard linear regression)

  # Create and fit model. help(rand_forest)
  if (is.null(extremely_randomised_splitrule)) {
    mod_spec <-
      parsnip::rand_forest(mode = mode_rf, trees = trees, mtry = mtry, min_n = min_n) %>%
      parsnip::set_engine("randomForest")

    # Create Workflow (to know variable roles from recipes)
    wf <- workflows::workflow() %>%
      workflows::add_model(mod_spec) %>%
      workflows::add_recipe(xy_recipe)

    # Fit model
    mod <- parsnip::fit(wf, data = data_train)
  } else if (is.character(extremely_randomised_splitrule)) {
    mod_spec <-
      parsnip::rand_forest(mode = mode_rf) %>%
      parsnip::set_engine("ranger", splitrule = extremely_randomised_splitrule) %>%
      parsnip::set_mode("classification")

    # Create Workflow (to know variable roles from recipes)
    wf <- workflows::workflow() %>%
      workflows::add_model(mod_spec) %>%
      workflows::add_recipe(xy_recipe)

    # Fit model
    mod <- parsnip::fit(wf, data = data_train)
  }

  # Prepare the test data according to the recipe
  xy_testing <- rsample::assessment(object)
  # look at xy_testing: glimpse(xy_testing)

  # Apply model on new data
  holdout_pred_class <-
    stats::predict(mod, xy_testing %>%
                     dplyr::select(-y), type = c("class")) %>%
    dplyr::bind_cols(rsample::assessment(object) %>% dplyr::select(y, id_nr))

  holdout_pred <-
    stats::predict(mod, xy_testing %>%
                     dplyr::select(-y), type = c("prob")) %>%
    dplyr::bind_cols(rsample::assessment(object) %>% dplyr::select(y, id_nr))

  holdout_pred$.pred_class <- holdout_pred_class$.pred_class
  class <- colnames(holdout_pred[2])

  eval_measure_val <- select_eval_measure_val(
    eval_measure = eval_measure,
    holdout_pred = holdout_pred,
    truth = y,
    estimate = .pred_class,
    class = class
  )

  # Sort output of RMSE, predictions and truth (observed y)
  output <- list(
    list(eval_measure_val),
    list(holdout_pred$.pred_class),
    list(holdout_pred$y),
    list(holdout_pred[1]),
    list(holdout_pred[2]),
    list(preprocess_PCA),
    list(holdout_pred$id_nr)
  ) # New
  names(output) <- c(
    "eval_measure_val",
    "estimate",
    "truth",
    ".pred_1",
    ".pred_2",
    "preprocess_PCA",
    "id_nr"
  ) # New
  output
}


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
                                          mode_rf,
                                          trees,
                                          preprocess_step_center,
                                          preprocess_scale_center,
                                          preprocess_PCA,
                                          variable_name_index_pca,
                                          eval_measure,
                                          extremely_randomised_splitrule) {
  fit_model_accuracy_rf(
    object,
    mode_rf,
    mtry,
    min_n,
    trees,
    preprocess_step_center,
    preprocess_scale_center,
    preprocess_PCA,
    variable_name_index_pca,
    eval_measure,
    extremely_randomised_splitrule
  )
}


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
                              mode_rf,
                              mtry,
                              min_n,
                              trees,
                              preprocess_step_center,
                              preprocess_scale_center,
                              preprocess_PCA,
                              variable_name_index_pca,
                              eval_measure,
                              extremely_randomised_splitrule) {
  T1 <- Sys.time()


  if (!is.na(preprocess_PCA[1])) {
    # Number of components or percent of variance to attain; min_halving;
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
    mtry = mtry,
    min_n = min_n,
    trees = trees,
    preprocess_PCA = preprocess_PCA_value
  )

  # Test models with the different hyperparameters for the inner samples
  tune_results <- purrr::pmap(
    list(
      grid_inner$mtry,
      grid_inner$min_n,
      grid_inner$trees,
      grid_inner$preprocess_PCA
    ),
    fit_model_accuracy_wrapper_rf,
    object = object,
    mode_rf = mode_rf,
    variable_name_index_pca = variable_name_index_pca,
    eval_measure = eval_measure,
    preprocess_step_center = preprocess_step_center,
    preprocess_scale_center = preprocess_scale_center,
    extremely_randomised_splitrule
  )

  # Sort the output to separate the accuracy, predictions and truth
  tune_outputlist <- tune_results %>%
    dplyr::bind_rows() %>%
    split.default(names(.)) %>%
    purrr::map(na.omit)

  # Extract the Accuracy
  tune_accuracy <- (dplyr::bind_rows(tune_outputlist$eval_measure_val$eval_measure_val))$.estimate
  # Add accuracy to the grid
  grid_inner_accuracy <- grid_inner %>%
    dplyr::mutate(eval_results = tune_accuracy)

  # Progression output
  best_eval <- bestParameters(
    data = grid_inner_accuracy,
    eval_measure = eval_measure
  )

  T2 <- Sys.time()
  time <- T2 - T1
  variable_time <- sprintf(
    "(duration: %s %s).",
    round(time, digits = 3),
    units(time)
  )

  description_text <- paste(
    "Fold:", eval_measure,
    round(best_eval$eval_result, digits = 3),
    variable_time,
    "\n"
  )

  cat(colourise(description_text, "green"))

  return(grid_inner_accuracy)
}


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
                                      mode_rf,
                                      mtry,
                                      min_n,
                                      trees,
                                      preprocess_step_center = preprocess_step_center,
                                      preprocess_scale_center = preprocess_scale_center,
                                      preprocess_PCA,
                                      variable_name_index_pca,
                                      eval_measure,
                                      extremely_randomised_splitrule) {
  # Return row-bound tibble containing the INNER results
  results <- purrr::map_df(
    .x = object$splits,
    .f = tune_over_cost_rf,
    mode_rf = mode_rf,
    mtry = mtry,
    min_n = min_n,
    trees = trees,
    preprocess_step_center = preprocess_step_center,
    preprocess_scale_center = preprocess_scale_center,
    preprocess_PCA = preprocess_PCA,
    variable_name_index_pca = variable_name_index_pca,
    eval_measure = eval_measure,
    extremely_randomised_splitrule = extremely_randomised_splitrule
  )


  return(results)
}




#' Train word embeddings to a categorical variable using random forest.
#'
#' @param x Word embeddings from textEmbed.
#' @param y Categorical variable to predict.
#' @param x_append (optional) Variables to be appended after the word embeddings (x);
#' if wanting to preappend them before the word embeddings use the option
#' first = TRUE.  If not wanting to train with word embeddings, set x_append = NULL (default = null).
#' @param append_first (boolean) Option to add variables before or after all word embeddings (default = FALSE).
#' @param cv_method (character) Cross-validation method to use within a pipeline of nested outer and
#' inner loops of folds (see nested_cv in rsample). Default is using cv_folds in the
#' outside folds and "validation_split" using rsample::validation_split in the inner loop to
#' achieve a development and assessment set (note that for validation_split the inside_folds
#' should be a proportion, e.g., inside_folds = 3/4); whereas "cv_folds" uses rsample::vfold_cv
#' to achieve n-folds in both the outer and inner loops.
#' @param outside_folds (numeric) Number of folds for the outer folds (default = 10).
#' @param inside_folds (numeric) Number of folds for the inner folds (default = 3/4).
#' @param strata (string or tibble; default "y") Variable to stratify according; if a string the
#' variable needs to be in the training set - if you want to stratify according to another variable
#' you can include it as a tibble (please note you can only add 1 variable to stratify according).
#' Can set it to NULL.
#' @param outside_strata (boolean) Whether to stratify the outside folds.
#' @param outside_breaks (numeric) The number of bins wanted to stratify a numeric stratification variable
#' in the outer cross-validation loop (default = 4).
#' @param inside_strata (boolean) Whether to stratify the outside folds.
#' @param inside_breaks The number of bins wanted to stratify a numeric stratification variable
#' in the inner cross-validation loop (default = 4).
#' @param mode_rf Default is "classification" ("regression" is not supported yet).
#' @param preprocess_step_center (boolean) Normalizes dimensions to have a mean of zero; default is set to FALSE
#' For more info see (step_center in recipes).
#' @param preprocess_scale_center (boolean) Normalizes dimensions to have a standard deviation of one;
#' default is set to FALSE. For more info see (step_scale in recipes).
#' @param preprocess_PCA Pre-processing threshold for PCA. Can select amount of variance to
#' retain (e.g., .90 or as a grid c(0.80, 0.90)); or
#' number of components to select (e.g., 10). (To skip this step, set preprocess_PCA to NA) Default is "min_halving",
#' which is a function that selects the number of PCA components based on number of participants and feature
#' (word embedding dimensions) in the data. The formula is:
#' preprocess_PCA = round(max(min(number_features/2), number_participants/2), min(50, number_features))).
#' @param extremely_randomised_splitrule Default is "extratrees", which thus implement a random forest;
#' can also select: NULL, "gini" or "hellinger"; if these are selected your mtry settings will
#'  be overridden (see Geurts et al. (2006) Extremely randomized trees for details; and see the ranger r-package
#' for details on implementations).
#' @param mtry Hyper parameter that may be tuned; default: c(1, 20, 40),
#' @param min_n Hyper parameter that may be tuned; default: c(1, 20, 40)
#' @param trees Number of trees to use (default 1000).
#' @param eval_measure (character) Measure to evaluate the models in order to select the best
#' hyperparameters default "roc_auc"; see also "accuracy", "bal_accuracy", "sens", "spec", "precision",
#' "kappa", "f_measure".
#' @param model_description (character) Text to describe your model (optional; good when sharing the model with others).
#' @param multi_cores If TRUE it enables the use of multiple cores if the computer system allows for it (i.e.,
#'  only on unix, not windows). Hence it makes the analyses considerably faster to run. Default is
#'   "multi_cores_sys_default", where it automatically uses TRUE for Mac and Linux and FALSE for Windows.
#' @param save_output (character) Option not to save all output; default "all". See also "only_results" and
#' "only_results_predictions".
#' @param simulate.p.value (Boolean) From fisher.test: a logical indicating whether to compute p-values
#' by Monte Carlo simulation, in larger than 2 × 2 tables.
#' @param seed (numeric) Set different seed (default = 2020).
#' @param ... For example settings in yardstick::accuracy to set event_level (e.g., event_level = "second").
#' @return A list with roc_curve_data, roc_curve_plot, truth and predictions, preprocessing_recipe,
#' final_model, model_description chisq and fishers test as well as evaluation measures, e.g., including accuracy,
#' f_meas and roc_auc (for details on these measures see the yardstick r-package documentation).
#' @examples
#' # Examines how well the embeddings from column "harmonywords" in
#' # Language_based_assessment_data_8 can binarily classify gender.
#'
#' \dontrun{
#' trained_model <- textTrainRandomForest(
#'   x = word_embeddings_4$texts$harmonywords,
#'   y = as.factor(Language_based_assessment_data_8$gender),
#'   trees = c(1000, 1500),
#'   mtry = c(1), # this is short because of testing
#'   min_n = c(1), # this is short because of testing
#'   multi_cores = FALSE # This is FALSE due to CRAN testing and Windows machines.
#' )
#'
#'
#' # Examine results (t-value, degree of freedom (df), p-value,
#' # alternative-hypothesis, confidence interval, correlation coefficient).
#'
#' trained_model$results
#' }
#' @seealso See \code{\link{textEmbedLayerAggregation}}, \code{\link{textTrainLists}} and
#' \code{\link{textTrainRegression}}.
#' @importFrom stats cor.test na.omit chisq.test fisher.test complete.cases
#' @importFrom dplyr select bind_cols starts_with filter arrange rename
#' @importFrom tibble as_tibble
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
                                  mode_rf = "classification",
                                  preprocess_step_center = FALSE,
                                  preprocess_scale_center = FALSE,
                                  preprocess_PCA = NA,
                                  extremely_randomised_splitrule = "extratrees",
                                  mtry = c(1, 10, 20, 40),
                                  min_n = c(1, 10, 20, 40),
                                  trees = c(1000),
                                  eval_measure = "bal_accuracy",
                                  model_description = "Consider writing a description of your model here",
                                  multi_cores = "multi_cores_sys_default",
                                  save_output = "all",
                                  simulate.p.value = FALSE,
                                  seed = 2020,
                                  ...) {
  T1_textTrainRandomForest <- Sys.time()
  set.seed(seed)

  # Sorting out y
  if (tibble::is_tibble(y) || is.data.frame(y)) {
    y_name <- colnames(y)
    y <- tibble::as_tibble_col(y[[1]], column_name = "y")
  } else {
    y_name <- deparse(substitute(y))
    y <- tibble::as_tibble_col(y, column_name = "y")
  }

  # The fit_model_rmse function need to number of word embeddings -- instead of
  # sending a separate parameter number of embeddings are give as a comment in "model"
  if (tibble::is_tibble(x)) {
    comment(eval_measure) <- "1"
  } else {
    comment(eval_measure) <- paste(length(x))
  }

  # sorting out x's
  variables_and_names <- sorting_xs_and_x_append(
    x = x,
    x_append = x_append,
    append_first = append_first,
    ...
  )
  x1 <- variables_and_names$x1
  x_name <- variables_and_names$x_name
  embedding_description <- variables_and_names$embedding_description
  x_append_names <- variables_and_names$x_append_names
  variable_name_index_pca <- variables_and_names$variable_name_index_pca
  rm(variables_and_names)


  if (!mode_rf == "regression") {
    y$y <- as.factor(y[[1]])
  }
  xy <- dplyr::bind_cols(x1, y)
  xy <- tibble::as_tibble(xy)


  xy$id_nr <- c(seq_len(nrow(xy)))
  id_nr <- tibble::as_tibble_col(c(seq_len(nrow(xy))), column_name = "id_nr")
  xy <- tibble::as_tibble(xy[stats::complete.cases(xy), ])

  # Adding strata variable to the data set -- and then calling it "outside_strata"
  # Which is then called in strata variable, and also made into not a predictor downstream
  outside_strata_y <- NULL
  inside_strata_y <- NULL
  strata_name <- NULL

  if (!is.null(strata)) {
    if (tibble::is_tibble(strata)) {
      strata_name <- colnames(strata)
      colnames(strata) <- "strata"
      xy <- dplyr::bind_cols(xy, strata)

      if (inside_strata) {
        outside_strata_y <- "strata"
      }
      if (outside_strata) {
        inside_strata_y <- "strata"
      }
    }

    if (strata[[1]][[1]] == "y") {
      strata_name <- "y"
      if (inside_strata) {
        outside_strata_y <- "y"
      }
      if (outside_strata) {
        inside_strata_y <- "y"
      }
    }
  }


  # Cross-Validation help(nested_cv) help(vfold_cv) help(validation_split)
  if (cv_method == "cv_folds") {
    results_nested_resampling <- rlang::expr(rsample::nested_cv(xy,
      outside = rsample::vfold_cv(
        v = !!outside_folds,
        repeats = 1,
        strata = !!outside_strata_y,
        breaks = !!outside_breaks
      ), #
      inside = rsample::vfold_cv(
        v = !!inside_folds,
        repeats = 1,
        strata = !!inside_strata_y,
        breaks = !!inside_breaks
      )
    ))
  }
  if (cv_method == "validation_split") {
    results_nested_resampling <- rlang::expr(rsample::nested_cv(xy,
      outside = rsample::vfold_cv(
        v = !!outside_folds,
        repeats = 1,
        strata = !!outside_strata_y,
        breaks = !!outside_breaks
      ), #
      inside = rsample::validation_split(
        prop = !!inside_folds,
        strata = !!inside_strata_y,
        breaks = !!inside_breaks
      )
    ))
  }

  results_nested_resampling <- rlang::eval_tidy(results_nested_resampling)

  # Deciding whether to use multicorese depending on system and settings.
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


  # Tuning inner resamples
  if (multi_cores_use == FALSE) {
    tuning_results <- purrr::map(
      .x = results_nested_resampling$inner_resamples,
      .f = summarize_tune_results_rf,
      mode_rf = mode_rf,
      mtry = mtry,
      min_n = min_n,
      trees = trees,
      preprocess_step_center = preprocess_step_center,
      preprocess_scale_center = preprocess_scale_center,
      preprocess_PCA = preprocess_PCA,
      variable_name_index_pca = variable_name_index_pca,
      eval_measure = eval_measure,
      extremely_randomised_splitrule = extremely_randomised_splitrule
    )
  } else if (multi_cores_use == TRUE) {
    # The multisession plan uses the local cores to process the inner resampling loop.
    future::plan(future::multisession)
    # The object tuning_results is a list of data frames for each of the OUTER resamples.
    tuning_results <- furrr::future_map(
      .x = results_nested_resampling$inner_resamples,
      .f = summarize_tune_results_rf,
      mode_rf = mode_rf,
      mtry = mtry,
      min_n = min_n,
      trees = trees,
      preprocess_step_center = preprocess_step_center,
      preprocess_scale_center = preprocess_scale_center,
      preprocess_PCA = preprocess_PCA,
      variable_name_index_pca = variable_name_index_pca,
      eval_measure = eval_measure,
      extremely_randomised_splitrule = extremely_randomised_splitrule
    )
  }


  # Function to get the lowest eval_measure_val library(tidyverse)
  # Determine the best parameter estimate from each INNER sample to be used
  # for each of the outer resampling iterations:
  hyper_parameter_vals <-
    tuning_results %>%
    purrr::map_df(bestParameters, eval_measure)

  # Bind best results
  results_split_parameter <-
    dplyr::bind_cols(results_nested_resampling, hyper_parameter_vals)

  # Compute the outer resampling results for each of the
  # splits using the corresponding tuning parameter value from results_split_parameter.
  results_outer <- purrr::pmap(
    list(
      object = results_nested_resampling$splits,
      mode_rf = mode_rf,
      results_split_parameter$mtry,
      results_split_parameter$min_n,
      trees = results_split_parameter$trees,
      preprocess_PCA = results_split_parameter$preprocess_PCA,
      variable_name_index_pca = list(variable_name_index_pca),
      eval_measure = list(eval_measure)
    ),
    fit_model_accuracy_rf
  )

  # Separate RMSE, predictions and observed y
  outputlist_results_outer <- results_outer %>%
    dplyr::bind_rows() %>%
    split.default(names(.)) %>%
    purrr::map(na.omit)

  # Get  predictions and evaluation results help(full_join)
  results_collected <- classification_results(
    outputlist_results_outer = outputlist_results_outer,
    id_nr,
    simulate.p.value = simulate.p.value,
    ...
  )
  names(results_collected) <- c("predy_y", "roc_curve_data", "roc_curve_plot", "fisher", "chisq", "results_collected")


  ##### Construct final model to be saved and applied on other data  ########
  ############################################################################

  if ("strata" %in% colnames(xy)) {
    xy_all <- xy %>%
      dplyr::select(-strata)
  } else {
    xy_all <- xy
  }
  ######### One word embedding as input
  n_embbeddings <- as.numeric(comment(eval_measure))

  if (n_embbeddings == 1) {
    final_recipe <- # xy %>%
      recipes::recipe(y ~ ., xy_all[0, ]) %>%
      recipes::update_role(id_nr, new_role = "id variable") %>%
      recipes::update_role(y, new_role = "outcome")

    final_recipe <- final_recipe %>%
      recipes::step_naomit(recipes::all_predictors(), skip = FALSE) # %>%
    # recipes::step_BoxCox(recipes::all_predictors()) %>%

    if (preprocess_step_center) {
      final_recipe <- recipes::step_center(final_recipe, recipes::all_predictors())
    }
    if (preprocess_scale_center) {
      final_recipe <- recipes::step_scale(final_recipe, recipes::all_predictors())
    }

    # If preprocess_PCA is not NULL add PCA step with number of component of % of variance to retain specification
    final_recipe <- final_recipe %>%
      {
        if (!is.na(preprocess_PCA[1])) {
          if (preprocess_PCA[1] >= 1) {
            recipes::step_pca(., recipes::all_predictors(),
              num_comp = statisticalMode(results_split_parameter$preprocess_PCA)
            )
          } else if (preprocess_PCA[1] < 1) {
            recipes::step_pca(., recipes::all_predictors(),
              threshold = statisticalMode(results_split_parameter$preprocess_PCA)
            )
          } else {
            .
          }
        } else {
          .
        }
      }
    ######### More than one word embeddings as input
  } else {
    final_recipe <- recipes::recipe(y ~ ., xy_all[0, ]) %>%
      recipes::update_role(id_nr, new_role = "id variable") %>%
      # recipes::update_role(-id_nr, new_role = "predictor") %>%
      recipes::update_role(y, new_role = "outcome") %>%
      recipes::step_naomit(recipes::all_predictors(), skip = TRUE) # %>%
    # recipes::step_BoxCox(recipes::all_predictors()) %>%


    if (preprocess_step_center) {
      final_recipe <- recipes::step_center(final_recipe, recipes::all_predictors())
    }
    if (preprocess_scale_center) {
      final_recipe <- recipes::step_scale(final_recipe, recipes::all_predictors())
    }

    # Adding a PCA in each loop; first selecting all variables starting with i="DimWs1"; and then "DimWs2" etc
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
            recipes::step_pca(dplyr::matches(!!i), threshold = preprocess_PCA, prefix = paste("PCA_", i, "_"))
        }
      }
    }
  }


  # Creating new environment to keep saving size down
  # Creating recipe in another environment sto avoid saving unnecessarily large parts of the environment
  # when saving the object to rda, rds or Rdata.
  # http://r.789695.n4.nabble.com/Model-object-when-generated-
  # in-a-function-saves-entire-environment-when-saved-td4723192.html
  recipe_save_small_size <- function(final_recipe, xy_all) {
    env_final_recipe <- new.env(parent = globalenv())
    env_final_recipe$xy_all <- xy_all
    env_final_recipe$final_recipe <- final_recipe

    with(env_final_recipe, preprocessing_recipe_save <- suppressWarnings(recipes::prep(final_recipe,
                                                                                       xy_all,
                                                                                       retain = FALSE
    )))
  }
  preprocessing_recipe_save <- recipe_save_small_size(final_recipe = final_recipe, xy_all = xy_all)

  # To load the prepared training data into a variable juice() is used.
  # It extracts the data from the xy_recipe object.

  model_save_small_size <- function(xy_all, final_recipe, results_split_parameter, mode_rf) {
    env_final_model <- new.env(parent = globalenv())
    env_final_model$xy_all <- xy_all
    env_final_model$final_recipe <- final_recipe

    env_final_model$trees_mode <- statisticalMode(results_split_parameter$trees)
    env_final_model$mtry_mode <- statisticalMode(results_split_parameter$mtry)
    env_final_model$min_n_mode <- statisticalMode(results_split_parameter$min_n)

    env_final_model$mode_rf <- mode_rf
    env_final_model$statisticalMode <- statisticalMode
    env_final_model$`%>%` <- `%>%`

    with(
      env_final_model,
      final_predictive_model_spec <-
        parsnip::rand_forest(
          mode = mode_rf,
          trees = trees_mode,
          mtry = mtry_mode,
          min_n = min_n_mode
        ) %>%
        parsnip::set_engine("randomForest")
    )

    with(
      env_final_model,
      wf_final <- workflows::workflow() %>%
        workflows::add_model(final_predictive_model_spec) %>%
        workflows::add_recipe(final_recipe)
    )

    with(
      env_final_model,
      # Fit model
      final_predictive_model <- parsnip::fit(wf_final, data = xy_all)
    )
  }

  final_predictive_model <- model_save_small_size(xy_all, final_recipe, results_split_parameter, mode_rf)



  ##########  DESCRIBING THE MODEL  ##########
  ############################################

  # Saving the final mtry and min_n used for the final model.
  if (is.null(extremely_randomised_splitrule)) {
    mtry_description <- paste("mtry  in final model =", deparse(statisticalMode(results_split_parameter$mtry)))
    mtry_fold_description <- paste("mtry in each fold =", deparse(results_split_parameter$mtry))

    min_n_description <- paste("min_n  in final model =", deparse(statisticalMode(results_split_parameter$min_n)))
    min_n_fold_description <- paste("min_n in each fold =", deparse(results_split_parameter$min_n))
  } else {
    mtry_description <- c("NA")
    mtry_fold_description <- c("NA")
    min_n_description <- c("NA")
    min_n_fold_description <- c("NA")
  }
  mode_rf_description <- paste("mode =", mode_rf)
  trees_description <- paste("trees in final model =", statisticalMode(results_split_parameter$trees))
  trees_fold_description <- paste("trees in each fold =", deparse(results_split_parameter$trees))
  preprocess_PCA_description <- paste(
    "preprocess_PCA in final model = ",
    statisticalMode(results_split_parameter$preprocess_PCA)
  )
  preprocess_PCA_fold_description <- paste(
    "preprocess_PCA in each fold = ",
    deparse(results_split_parameter$preprocess_PCA)
  )
  eval_measure <- paste("eval_measure = ", eval_measure)

  preprocess_step_center <- paste("preprocess_step_center_setting = ", deparse(preprocess_step_center))
  preprocess_scale_center <- paste("preprocess_scale_center_setting = ", deparse(preprocess_scale_center))

  if (is.character(extremely_randomised_splitrule)) {
    extremely_randomised_splitrule <- paste("extremely_randomised_splitrule = ", extremely_randomised_splitrule)
  } else {
    extremely_randomised_splitrule <- c("NA")
  }

  cv_method_description <- paste("cv_method = ", deparse(cv_method))
  strata_description <- paste("strata = ", strata_name)
  outside_folds_description <- paste("outside_folds = ", deparse(outside_folds))
  outside_strata_y_description <- paste("outside_strata_y = ", deparse(outside_strata_y))
  inside_folds_description <- paste("inside_folds = ", deparse(inside_folds))
  inside_strata_y_description <- paste("inside_strata_y = ", deparse(inside_strata_y))

  preprocess_PCA_setting <- paste("preprocess_PCA_setting = ", deparse(preprocess_PCA))

  mtry_setting <- paste("mtry_setting = ", deparse(mtry))
  min_n_setting <- paste("min_n_setting = ", deparse(min_n))
  trees_setting <- paste("trees_setting = ", deparse(trees))

  # Getting time and date
  T2_textTrainRandomForest <- Sys.time()
  Time_textTrainRandomForest <- T2_textTrainRandomForest - T1_textTrainRandomForest
  Time_textTrainRandomForest <- sprintf(
    "Duration to train text: %f %s",
    Time_textTrainRandomForest,
    units(Time_textTrainRandomForest)
  )
  Date_textTrainRandomForest <- Sys.time()
  time_date <- paste(Time_textTrainRandomForest,
    "; Date created: ", Date_textTrainRandomForest,
    sep = "",
    collapse = " "
  )


  # Describe model; adding user's-description + the name of the x and y and mtry and min_n
  model_description_detail <- c(
    x_name,
    y_name,
    cv_method_description,
    outside_folds_description,
    inside_folds_description,
    strata_description,
    outside_strata_y_description,
    inside_strata_y_description,
    mode_rf_description,
    preprocess_step_center,
    preprocess_scale_center,
    preprocess_PCA_setting,
    preprocess_PCA_description,
    preprocess_PCA_fold_description,
    extremely_randomised_splitrule,
    eval_measure,
    mtry_setting,
    mtry_description,
    mtry_fold_description,
    min_n_setting,
    min_n_description,
    min_n_fold_description,
    trees_setting,
    trees_description,
    trees_fold_description,
    embedding_description,
    model_description,
    time_date
  )

  ###### Saving and arranging output ######
  ##########################################

  if (save_output == "all") {
    final_results <- list(
      results_collected$roc_curve_data,
      results_collected$predy_y,
      preprocessing_recipe_save,
      final_predictive_model,
      results_collected$roc_curve_plot,
      model_description_detail,
      results_collected$fisher,
      results_collected$chisq,
      results_collected$results_collected
    )
    names(final_results) <- c(
      "roc_curve_data",
      "truth_predictions",
      "final_recipe",
      "final_model",
      "roc_curve_plot",
      "model_description",
      "fisher_test",
      "chisq",
      "results"
    )
  } else if (save_output == "only_results_predictions") {
    final_results <- list(
      results_collected$roc_curve_data,
      results_collected$predy_y,
      results_collected$roc_curve_plot,
      model_description_detail,
      results_collected$fisher,
      results_collected$chisq,
      results_collected$results_collected
    )
    names(final_results) <- c(
      "roc_curve_data",
      "truth_predictions",
      "roc_curve_plot",
      "model_description",
      "fisher_test",
      "chisq",
      "results"
    )
  } else if (save_output == "only_results") {
    final_results <- list(
      results_collected$roc_curve_data,
      results_collected$roc_curve_plot,
      model_description_detail,
      results_collected$fisher,
      results_collected$chisq,
      results_collected$results_collected
    )
    names(final_results) <- c(
      "roc_curve_data",
      "roc_curve_plot",
      "model_description",
      "fisher_test",
      "chisq",
      "results"
    )
  }
  # Remove object to minimize model size when saved to rds; use this to
  # check sizes: sort( sapply(ls(),function(x) {object.size(get(x))}))
  remove(x)
  remove(x1)
  remove(y)
  remove(xy)
  remove(id_nr)

  remove(preprocessing_recipe_save)
  remove(final_predictive_model)
  remove(model_description_detail)
  remove(results_nested_resampling)
  remove(tuning_results)
  remove(hyper_parameter_vals)
  remove(results_split_parameter)
  remove(results_outer)
  remove(outputlist_results_outer)
  remove(xy_all)
  remove(final_recipe)

  final_results
}
