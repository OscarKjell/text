

#  devtools::document()
#' Select evaluation measure and compute it (also used in logistic regression)
#'
#' @param eval_measure Measure used to evaluate and select models default: "roc_auc", see also
#' "accuracy", "bal_accuracy", "sens", "spec", "precision", "kappa", "f_measure".
#' @param holdout_pred data with observed (truth) and predicted
#' @param truth name of the truth column
#' @param estimate name of the predicted estimate
#' @return  tibble with .metric column (i.e., type of evaluation measure), .estimator (e.g., binary)
#' and .estimate (i.e., the resulting value).
#' @noRd
select_eval_measure_val <- function(eval_measure = "bal_accuracy", holdout_pred = NULL, truth = y, estimate = .pred_class, class) {
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
    eval_measure_val <- yardstick::roc_auc(holdout_pred, truth = y, class1_name) # all_of(class1_name)
  } else if (eval_measure == "rmse") {
    eval_measure_val <- yardstick::rmse(holdout_pred, truth = y, estimate = .pred)
  }
  eval_measure_val
}


#  devtools::document()
#' Select evaluation measure and compute it (also used in logistic regression)
#'
#' @param outputlist_results_outer Results from outer predictions.
#' @return returns sorted predictions and truth, chi-square test, fisher test, and all evaluation metrics/measures
#' @noRd
classification_results <- function(outputlist_results_outer, ...) {
  # Unnest predictions and y
  predy_y <- tibble::tibble(
    tidyr::unnest(outputlist_results_outer$truth, cols = c(truth)),
    tidyr::unnest(outputlist_results_outer$estimate, cols = c(estimate)),
    tidyr::unnest(outputlist_results_outer$.pred_1, cols = c(.pred_1)),
    tidyr::unnest(outputlist_results_outer$.pred_2, cols = c(.pred_2)),
    tidyr::unnest(outputlist_results_outer$id_nr, cols = c(id_nr))
  )
  predy_y <- predy_y %>% dplyr::arrange(id_nr)
  # Correlate predictions and observed help(all_of)
  chisq <- suppressWarnings(chisq.test(table(predy_y$truth, predy_y$estimate)))

  fisher <- stats::fisher.test(predy_y$truth, predy_y$estimate)


  accuracy <- yardstick::accuracy(predy_y, truth, estimate, ...)
  bal_accuracy <- yardstick::bal_accuracy(predy_y, truth, estimate, ...)
  sens <- yardstick::sens(predy_y, truth, estimate, ...)
  spec <- yardstick::spec(predy_y, truth, estimate, ...)
  precision <- yardstick::precision(predy_y, truth, estimate, ...)
  kappa <- yardstick::kap(predy_y, truth, estimate, ...)
  f_measure <- yardstick::f_meas(predy_y, truth, estimate, ...)

  # eval_class <- colnames(predy_y[3])
  roc_auc <- yardstick::roc_auc(predy_y, truth, colnames(predy_y[3])) # OK tidyselect::all_of(eval_class) dplyr::
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

#  devtools::document()
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
                                  preprocess_PCA = NA,
                                  variable_name_index_pca = NA,
                                  eval_measure = "bal_accuracy",
                                  extremely_randomised_splitrule = NULL) {

  # Recipe for one embedding input
  if(colnames(rsample::analysis(object)[1]) == "Dim1"){

    xy_recipe <- rsample::analysis(object) %>%
    recipes::recipe(y ~ .) %>%
    #    recipes::update_role(id1, new_role = "id variable") %>%
    #    #recipes::update_role(-id1, new_role = "predictor") %>%
    #    recipes::update_role(y, new_role = "outcome") %>%
    recipes::update_role(id_nr, new_role = "id variable") %>% # New
    recipes::update_role(-id_nr, new_role = "predictor") %>% # New
    recipes::update_role(y, new_role = "outcome") %>% # New
    recipes::step_naomit(Dim1, skip = FALSE) %>%
    recipes::step_center(recipes::all_predictors()) %>%
    recipes::step_scale(recipes::all_predictors()) %>%
    recipes::step_BoxCox(recipes::all_predictors()) %>%
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
               #recipes::step_pca(., recipes::all_predictors(), threshold = preprocess_PCA)
               recipes::step_pca(dplyr::matches(!!i), threshold = preprocess_PCA, prefix = paste("PCA_", i, "_"))
           }
         }
       }
       xy_recipe <- recipes::prep(xy_recipe)
     }


  # To load the prepared training data into a variable juice() is used.
  # It extracts the data from the xy_recipe object.
  xy_training <- recipes::juice(xy_recipe)

  # Create and fit model. help(rand_forest)
  if (is.null(extremely_randomised_splitrule)) {
    mod <-
      parsnip::rand_forest(mode = mode_rf, trees = trees, mtry = mtry, min_n = min_n) %>%
      # parsnip::set_engine("ranger")
      parsnip::set_engine("randomForest") %>%
      parsnip::fit(y ~ ., data = xy_training) # analysis(object)
  } else if (is.character(extremely_randomised_splitrule)) {
    mod <-
      parsnip::rand_forest(mode = mode_rf) %>%
      parsnip::set_engine("ranger", splitrule = extremely_randomised_splitrule) %>% # extremely_randomised_splitrule = "extratrees" = "gini" "extratrees" "hellinger" tune()
      # parsnip::set_mode("classification") %>%
      parsnip::fit(y ~ ., data = xy_training) # ADDED line; and: install.packages("ranger")
  }

  # Prepare the test data according to the recipe
  xy_testing <- xy_recipe %>%
    recipes::bake(rsample::assessment(object))
  # look at xy_testing: glimpse(xy_testing)

  # Apply model on new data help(predict)
  holdout_pred_class <-
    stats::predict(mod, xy_testing %>% dplyr::select(-y), type = c("class")) %>%
    dplyr::bind_cols(rsample::assessment(object) %>% dplyr::select(y, id_nr))

  holdout_pred <-
    stats::predict(mod, xy_testing %>% dplyr::select(-y), type = c("prob")) %>%
    dplyr::bind_cols(rsample::assessment(object) %>% dplyr::select(y, id_nr))

  holdout_pred$.pred_class <- holdout_pred_class$.pred_class
  class <- colnames(holdout_pred[1])

  eval_measure_val <- select_eval_measure_val(eval_measure = eval_measure, holdout_pred = holdout_pred, truth = y, estimate = .pred_class, class = class)

  # Sort output of RMSE, predictions and truth (observed y) help(sens)
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

# object: an rsplit object (from results_nested_resampling tibble)
# object = results_nested_resampling$splits[[5]]; mtry = 1  min_n = 1
# testing
# fit_model_accuracy_rf(object = results_nested_resampling$splits[[2]], mtry = 1, min_n = 1, eval_measure="roc_auc")

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
    preprocess_PCA,
    variable_name_index_pca,
    eval_measure,
    extremely_randomised_splitrule
  )
}

# fit_model_accuracy_wrapper_rf(object = results_nested_resampling$splits[[1]], mtry=1, min_n=1, trees=10, preprocess_PCA=0.95, eval_measure = "roc_auc")

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
                              preprocess_PCA,
                              variable_name_index_pca,
                              eval_measure,
                              extremely_randomised_splitrule) {
  if (!is.na(preprocess_PCA[1])) {
    # Number of components or percent of variance to attain; min_halving; preprocess_PCA = c(0.9, 0.3); preprocess_PCA = NA
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
  tune_results <- purrr::pmap(list(
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
    dplyr::mutate(eval_measure = tune_accuracy)

  grid_inner_accuracy
}
# testing help(yardstick)
# tune_over_cost_rf(object=results_nested_resampling$inner_resamples[[1]]$splits[[1]])

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
                                      mode_rf,
                                      mtry,
                                      min_n,
                                      trees,
                                      preprocess_PCA,
                                      variable_name_index_pca,
                                      eval_measure,
                                      extremely_randomised_splitrule) {

  # Return row-bound tibble containing the INNER results
  purrr::map_df(
    .x = object$splits,
    .f = tune_over_cost_rf,
    mode_rf = mode_rf,
    mtry = mtry,
    min_n = min_n,
    trees = trees,
    preprocess_PCA = preprocess_PCA,
    variable_name_index_pca = variable_name_index_pca,
    eval_measure = eval_measure,
    extremely_randomised_splitrule = extremely_randomised_splitrule
  )
}




#x <- wordembeddings4[1:2]
#y <- as.factor(c(1, 2, 1, 2, 1, 2, 1, 2, 1, 2))
#
#mode_rf = "classification"
#preprocess_PCA = NA
#preprocess_PCA = 0.9
#preprocess_PCA = 3
#extremely_randomised_splitrule = NULL
#mtry = c(1)
#min_n = c(1)
#trees = c(1000)
#eval_measure = "bal_accuracy"
#model_description = "Consider writing a description of your model here"
#multi_cores = TRUE
#save_output = "all"
#
#textTrainRandomForest(wordembeddings4[1:2],
#                      y,
#                     # preprocess_PCA = NA
#                      preprocess_PCA = 0.9
#                     #preprocess_PCA = 3
#                      )

#' Train word embeddings to a categorical variable using random forrest.
#'
#' @param x Word embeddings from textEmbed.
#' @param y Categorical variable to predict.
# @param outside_folds Number of folds for the outer folds.
# @param outside_strata_y Variable to stratify according (default "y"; can also set to NULL).
# @param inside_folds Number of folds for the inner folds.
# @param inside_strata_y Variable to stratify according (default "y"; can also set to NULL).
#' @param mode_rf Default is "classification" ("regression" is not supported yet).
#' @param preprocess_PCA Pre-processing threshold for PCA. Can select amount of variance to retain (e.g., .90 or as a grid c(0.80, 0.90)); or
#' number of components to select (e.g., 10). Default is "min_halving", which is a function that selects the number of PCA components based on number
#' of participants and feature (word embedding dimensions) in the data. The formula is:
#' preprocess_PCA = round(max(min(number_features/2), number_participants/2), min(50, number_features))).
#' @param extremely_randomised_splitrule default: NULL, which thus implement a random forest; can also select: "extratrees", "gini" or "hellinger"; if these are selected
#' your mtry settings will be overridden (see Geurts et al. (2006) Extremely randomized trees for details; and see the ranger r-package
#' for details on implementations).
#' @param mtry hyper parameter that may be tuned;  default:c(1, 20, 40),
#' @param min_n hyper parameter that may be tuned; default: c(1, 20, 40)
#' @param trees Number of trees to use (default 1000).
#' @param eval_measure Measure to evaluate the models in order to select the best hyperparameters default "roc_auc";
#' see also "accuracy", "bal_accuracy", "sens", "spec", "precision", "kappa", "f_measure".
#' @param model_description Text to describe your model (optional; good when sharing the model with others).
#' @param multi_cores If TRUE it enables the use of multiple cores if the computer system allows for it (i.e., only on unix, not windows). Hence it
#' makes the analyses considerably faster to run. Default is "multi_cores_sys_default", where it automatically uses TRUE for Mac and Linux and FALSE for Windows.
#' @param save_output Option not to save all output; default "all". see also "only_results" and "only_results_predictions".
#' @param ... For example settings in yardstick::accuracy to set event_level (e.g., event_level = "second").
#' @return A list with roc_curve_data, roc_curve_plot, truth and predictions, preprocessing_recipe, final_model, model_description
#' chisq and fishers test as well as evaluation measures, e.g., including accuracy, f_meas and roc_auc (for details on
#' these measures see the yardstick r-package documentation).
#' @examples
#' wordembeddings <- wordembeddings4
#' example_categories <- as.factor(c(1, 2, 1, 2, 1, 2, 1, 2, 1, 2))
#' results <- textTrainRandomForest(wordembeddings$harmonywords,
#'   example_categories,
#'   trees = c(1000, 1500),
#'   mtry  = c(1), # this is short because of testing
#'   min_n = c(1), # this is short because of testing
#'   multi_cores = FALSE # This is FALSE due to CRAN testing and Windows machines.
#' )
#' @seealso see \code{\link{textTrainLists}} \code{\link{textSimilarityTest}}
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
                                  # outside_folds = 10,
                                  # outside_strata_y = "y",
                                  # inside_folds = 10,
                                  # inside_strata_y = "y",
                                  mode_rf = "classification",
                                  preprocess_PCA = NA,
                                  extremely_randomised_splitrule = NULL,
                                  mtry = c(1, 10, 20, 40),
                                  min_n = c(1, 10, 20, 40),
                                  trees = c(1000),
                                  eval_measure = "bal_accuracy",
                                  model_description = "Consider writing a description of your model here",
                                  multi_cores = "multi_cores_sys_default",
                                  save_output = "all",
                                  ...) {
  T1_textTrainRandomForest <- Sys.time()
  set.seed(2020)

  variable_name_index_pca <- NA

  # In case the embedding is in list form get the tibble form
  if (!tibble::is_tibble(x) & length(x) == 1) {
    x1 <- x[[1]]
    # Get names for description
    x_name <- names(x)
    # Get embedding info to save for model description
    embedding_description <- comment(x[[1]])
    # In case there are several embeddings in list form get the x_names and embedding description for model description
  } else if (!tibble::is_tibble(x) & length(x) > 1) {
    x_name <- names(x)
    x_name <- paste(x_name, sep=" ", collapse = " & ")
    x_name <- paste("input:", x_name, sep=" ", collapse = " ")

    embedding_description <- comment(x[[1]])
    # In case it is just ane word embedding as tibble
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

  if(!tibble::is_tibble(x) & length(x)>1){

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
    #V1 <- colnames(x)[1]

  }

  ############ End for multiple word embeddings ############
  ##########################################################


  x1 <- dplyr::select(x1, dplyr::starts_with("Dim"))

  if (!mode_rf == "regression") {
    y <- as.factor(y)
  }
  xy <- cbind(x1, y)

  #  xy$id1 <- c(seq_len(nrow(xy)))
  xy$id_nr <- c(seq_len(nrow(xy))) # New
  # xy_formergingNA <- tibble::tibble(xy$id1, xy$y)
  # colnames(xy_formergingNA) <- c("id1", "y")
  xy1 <- tibble::as_tibble(xy[stats::complete.cases(xy), ])

  results_nested_resampling <- rsample::nested_cv(xy1,
    outside = rsample::vfold_cv(
      v = 5, # outside_folds,
      repeats = 1,
      strata = "y"
    ), # outside_strata_y
    inside = rsample::validation_split(
      prop = 3 / 4,
      strata = "y", # inside_strata_y
      breaks = 1
    )
  )


  # Deciding whether to use multicorese depending on system and settings.
  if (multi_cores == "multi_cores_sys_default"){
    if (.Platform$OS.type == "unix"){
      multi_cores_use <- TRUE
    } else if (.Platform$OS.type == "windows"){
      multi_cores_use <- FALSE
    }
  } else if (multi_cores == TRUE) {
    multi_cores_use <- TRUE
  } else if (multi_cores == FALSE){
    multi_cores_use <- FALSE
  }


  # Tuning inner resamples library(magrittr) warnings()
  if (multi_cores_use == FALSE) {
    tuning_results <- purrr::map(
      .x = results_nested_resampling$inner_resamples,
      .f = summarize_tune_results_rf,
      mode_rf = mode_rf,
      mtry = mtry,
      min_n = min_n,
      trees = trees,
      preprocess_PCA = preprocess_PCA,
      variable_name_index_pca = variable_name_index_pca,
      eval_measure = eval_measure, # eval_measure = "bal_accuracy"; eval_measure = "roc_auc"
      extremely_randomised_splitrule = extremely_randomised_splitrule
    )
  } else if (multi_cores_use == TRUE){
    # The multisession plan uses the local cores to process the inner resampling loop.
    # library(future)
    future::plan(future::multisession)
    # The object tuning_results is a list of data frames for each of the OUTER resamples.
    tuning_results <- furrr::future_map(
      .x = results_nested_resampling$inner_resamples,
      .f = summarize_tune_results_rf,
      mode_rf = mode_rf,
      mtry = mtry,
      min_n = min_n,
      trees = trees,
      preprocess_PCA = preprocess_PCA,
      variable_name_index_pca = variable_name_index_pca,
      eval_measure = eval_measure,
      extremely_randomised_splitrule = extremely_randomised_splitrule
    )
  }

  # Function to get the lowest eval_measure_val
  bestParameters <- function(dat) dat[which.min(dat$eval_measure), ]

  # Determine the best parameter estimate from each INNER sample to be used
  # for each of the outer resampling iterations:
  hyper_parameter_vals <-
    tuning_results %>%
    purrr::map_df(bestParameters) # %>%
  # dplyr::select(c(mtry, min_n, trees, preprocess_PCA))

  # Bind best results
  results_split_parameter <-
    dplyr::bind_cols(results_nested_resampling, hyper_parameter_vals)

  # Compute the outer resampling results for each of the
  # splits using the corresponding tuning parameter value from results_split_parameter.
  # fit_model_rmse(results_split_parameter$splits[[1]])
  results_outer <- purrr::pmap(
    list(
      object = results_nested_resampling$splits,
      mode_rf = mode_rf,
      results_split_parameter$mtry,
      results_split_parameter$min_n,
      trees = results_split_parameter$trees,
      preprocess_PCA = results_split_parameter$preprocess_PCA,
      variable_name_index_pca = list(variable_name_index_pca)
    ),
    fit_model_accuracy_rf
  )

  # Separate RMSE, predictions and observed y
  outputlist_results_outer <- results_outer %>%
    dplyr::bind_rows() %>%
    split.default(names(.)) %>%
    purrr::map(na.omit)

  # Get  predictions and evaluation results
  results_collected <- classification_results(outputlist_results_outer = outputlist_results_outer, ...)
  names(results_collected) <- c("predy_y", "roc_curve_data", "roc_curve_plot", "fisher", "chisq", "results_collected")


  # Construct final model to be saved and applied on other data
  # Recipe: Pre-processing by removing na and normalizing variables. library(magrittr)
  xy_short <- xy %>% dplyr::select(-id_nr)


  ######### One word embedding as input
  if(colnames(xy_short[1]) == "Dim1"){
  final_recipe <- # xy %>%
    recipes::recipe(y ~ ., xy_short[0, ]) %>%
    recipes::step_naomit(Dim1, skip = FALSE) %>% # Does this not work here?
    recipes::step_center(recipes::all_predictors()) %>%
    recipes::step_scale(recipes::all_predictors()) %>%
    recipes::step_BoxCox(recipes::all_predictors()) %>%
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
  ######### More than one word embeddings as input
  } else {

    V1 <- colnames(xy_short[1])

    final_recipe <- recipes::recipe(y ~ ., xy_short[0, ]) %>%
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
            #recipes::step_pca(., recipes::all_predictors(), threshold = preprocess_PCA)
            recipes::step_pca(dplyr::matches(!!i), threshold = preprocess_PCA, prefix = paste("PCA_", i, "_"))
        }
      }
    }
  }


  preprocessing_recipe_save <- recipes::prep(final_recipe, xy_short, retain = FALSE)
  preprocessing_recipe_use <- recipes::prep(final_recipe, xy_short)

  # preprocessing_recipe <- recipes::prep(final_recipe)

  # To load the prepared training data into a variable juice() is used.
  # It extracts the data from the xy_recipe object.
  xy_final <- recipes::juice(preprocessing_recipe_use)

  final_predictive_model <-
    parsnip::rand_forest(
      mode = mode_rf,
      trees = statisticalMode(results_split_parameter$trees),
      mtry = statisticalMode(results_split_parameter$mtry),
      min_n = statisticalMode(results_split_parameter$min_n)
    ) %>%
    # set_engine("ranger")
    parsnip::set_engine("randomForest") %>%
    parsnip::fit(y ~ ., data = xy_final) # analysis(object)

  # Saving the final mtry and min_n used for the final model.
  if (is.null(extremely_randomised_splitrule)) {
    mtry_description <- paste("mtry =", deparse(statisticalMode(results_split_parameter$mtry)))
    mtry_fold_description <- paste("mtry in each fold =", deparse(results_split_parameter$mtry))

    min_n_description <- paste("min_n =", deparse(statisticalMode(results_split_parameter$min_n)))
    min_n_fold_description <- paste("min_n in each fold =", deparse(results_split_parameter$min_n))
  } else {
    mtry_description <- c("-")
    mtry_fold_description <- c("-")
    min_n_description <- c("-")
    min_n_fold_description <- c("-")
  }
  mode_rf_description <- paste("mode =", mode_rf)
  trees_description <- paste("trees =", statisticalMode(results_split_parameter$trees))
  trees_fold_description <- paste("trees in each fold =", deparse(results_split_parameter$trees))
  preprocess_PCA_description <- paste("preprocess_PCA = ", statisticalMode(results_split_parameter$preprocess_PCA))
  preprocess_PCA_fold_description <- paste("preprocess_PCA = ", deparse(results_split_parameter$preprocess_PCA))
  eval_measure <- paste("eval_measure = ", eval_measure)


  if (is.character(extremely_randomised_splitrule)) {
    extremely_randomised_splitrule <- paste("extremely_randomised_splitrule = ", extremely_randomised_splitrule)
  } else {
    extremely_randomised_splitrule <- c("-")
  }


  # Getting time and date
  T2_textTrainRandomForest <- Sys.time()
  Time_textTrainRandomForest <- T2_textTrainRandomForest-T1_textTrainRandomForest
  Time_textTrainRandomForest <- sprintf('Duration to train text: %f %s', Time_textTrainRandomForest, units(Time_textTrainRandomForest))
  Date_textTrainRandomForest <- Sys.time()
  time_date <- paste(Time_textTrainRandomForest,
                     "; Date created: ", Date_textTrainRandomForest,
                     sep = "",
                     collapse = " ")


  # Describe model; adding user's-description + the name of the x and y and mtry and min_n
  model_description_detail <- c(
    x_name,
    y_name,
    mode_rf_description,
    preprocess_PCA_description,
    preprocess_PCA_fold_description,
    extremely_randomised_splitrule,
    eval_measure,
    mtry_description,
    mtry_fold_description,
    min_n_description,
    min_n_fold_description,
    trees_description,
    trees_fold_description,
    embedding_description,
    model_description,
    time_date
  )

  if (save_output == "all") {
    final_results <- list(results_collected$roc_curve_data, results_collected$predy_y, preprocessing_recipe_save, final_predictive_model, results_collected$roc_curve_plot, model_description_detail, results_collected$fisher, results_collected$chisq, results_collected$results_collected)
    names(final_results) <- c("roc_curve_data", "truth_predictions", "final_recipe", "final_model", "roc_curve_plot", "model_description", "fisher_test", "chisq", "results")
  } else if (save_output == "only_results_predictions") {
    final_results <- list(results_collected$roc_curve_data, results_collected$predy_y, results_collected$roc_curve_plot, model_description_detail, results_collected$fisher, results_collected$chisq, results_collected$results_collected)
    names(final_results) <- c("roc_curve_data", "truth_predictions", "roc_curve_plot", "model_description", "fisher_test", "chisq", "results")
  } else if (save_output == "only_results") {
    final_results <- list(results_collected$roc_curve_data, results_collected$roc_curve_plot, model_description_detail, results_collected$fisher, results_collected$chisq, results_collected$results_collected)
    names(final_results) <- c("roc_curve_data", "roc_curve_plot", "model_description", "fisher_test", "chisq", "results")
  }

  final_results
}
