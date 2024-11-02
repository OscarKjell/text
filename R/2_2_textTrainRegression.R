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
#' object = results_nested_resampling$inner_resamples[[5]][[1]][[1]]
#' @param penalty hyperparameter for ridge regression.
#' @param mixture hyperparameter for ridge regression.
#' @param preprocess_PCA threshold for pca; preprocess_PCA = NA
#' @param variable_name_index_pca variable with names to know how to keep variables
#' from same word embedding together in separate pca:s
#' @return  RMSE.
#' @importFrom rsample analysis assessment
#' @importFrom recipes recipe update_role step_naomit step_impute_knn step_center
#' step_scale step_pca prep juice
#' @importFrom dplyr matches select
#' @importFrom parsnip linear_reg logistic_reg multinom_reg set_engine fit
#' @importFrom workflows workflow add_model add_recipe
#' @noRd
fit_model_rmse <- function(object,
                           model = "regression",
                           eval_measure = "rmse",
                           penalty = 1,
                           mixture = 0,
                           preprocess_PCA = NA,
                           variable_name_index_pca = NA,
                           first_n_predictors = NA,
                           preprocess_step_center = TRUE,
                           preprocess_step_scale = TRUE,
                           impute_missing = FALSE) {

  data_train <- rsample::analysis(object)
  data_train <- tibble::as_tibble(data_train)

  # If testing N first predictors help(step_scale) first_n_predictors = 3
  if (!is.na(first_n_predictors)) {
    # Select y and id
    Nvariable_totals <- length(data_train)
    variable_names <- colnames(data_train[(first_n_predictors + 1):(Nvariable_totals - 2)])
  } else {
    if ("strata" %in% colnames(data_train)) {
      variable_names <- c("id_nr", "strata")
    } else {
      variable_names <- c("id_nr")
    }
  }

  # Get number of embeddings provided
  n_embeddings <- as.numeric(comment(eval_measure))


  # Recipe for one embedding input summary(xy_recipe) help(all_of) library(tidyverse) help(step_naomit)
  if (n_embeddings == 1) {
    xy_recipe <- data_train %>%
      recipes::recipe(y ~ .) %>%
      recipes::update_role(dplyr::all_of(variable_names), new_role = "Not_predictors") %>%
      recipes::update_role(id_nr, new_role = "id variable") %>%
      recipes::update_role(y, new_role = "outcome") # %>%

    if (!impute_missing) {
      xy_recipe <- recipes::step_naomit(xy_recipe, recipes::all_predictors(), skip = TRUE)
    } else if (impute_missing) {
      xy_recipe <- recipes::step_impute_knn(xy_recipe, recipes::all_predictors(), neighbors = 10)
    }

    if (preprocess_step_center) {
      xy_recipe <- recipes::step_center(xy_recipe, recipes::all_predictors())
    }
    if (preprocess_step_scale) {
      xy_recipe <- recipes::step_scale(xy_recipe, recipes::all_predictors())
    }

    # If preprocess_PCA is not NULL add PCA step with number of component of % of variance to retain specification
    xy_recipe <- xy_recipe %>%
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
    xy_recipe <- data_train %>%
      recipes::recipe(y ~ .) %>%
      # recipes::step_BoxCox(all_predictors()) %>%  preprocess_PCA = NULL, preprocess_PCA = 0.9 preprocess_PCA = 2
      recipes::update_role(id_nr, new_role = "id variable") %>%
      #  recipes::update_role(-id_nr, new_role = "predictor") %>%
      recipes::update_role(y, new_role = "outcome")


    if ("strata" %in% colnames(data_train)) {
      xy_recipe <- xy_recipe %>%
        recipes::update_role(strata, new_role = "strata")
    }

    if (!impute_missing) {
      xy_recipe <- recipes::step_naomit(xy_recipe, recipes::all_predictors(), skip = TRUE)
    } else if (impute_missing) {
      xy_recipe <- recipes::step_impute_knn(xy_recipe, recipes::all_predictors(), neighbors = 10)
    }

    if (preprocess_step_center) {
      xy_recipe <- recipes::step_center(xy_recipe, recipes::all_predictors())
    }
    if (preprocess_step_scale) {
      xy_recipe <- recipes::step_scale(xy_recipe, recipes::all_predictors())
    }

    # If preprocess_PCA is not NULL add PCA step with number of component of % of variance to retain specification
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
    xy_recipe_prep <- recipes::prep(xy_recipe)
  }

  # Figure out how many predictors to know whether to use simple or multiple regression, which
  # depend on number of of PCA components that are retrived and/or whether first_n_predictors is used
  if (!is.na(first_n_predictors) && is.na(preprocess_PCA)) {
    # Get number of predictors from receipe
    nr_predictors <- table(xy_recipe_prep[[1]]$role)[["predictor"]]
  } else if (!is.na(preprocess_PCA)) {
    # To load the prepared training data into a variable juice() is used.
    # It extracts the data from the xy_recipe object.
    nr_predictors <- recipes::juice(xy_recipe_prep)
    # Count number of PCAs
    nr_predictors <- length(grep(x = colnames(nr_predictors), pattern = "PC"))
  } else if (is.na(preprocess_PCA) && is.na(first_n_predictors)) {
    nr_predictors <- recipes::juice(xy_recipe_prep)
    nr_predictors <- length(nr_predictors) - 2
  }

  # Ridge and/or Lasso
  if (nr_predictors > 1) {
    # Create and fit model help(linear_reg)
    mod_spec <-
      {
        if (model == "regression") {
          parsnip::linear_reg(penalty = penalty, mixture = mixture)
        } else if (model == "logistic") {
          parsnip::logistic_reg(
            mode = "classification",
            penalty = penalty,
            mixture = mixture
          )
        } else if (model == "multinomial") {
          parsnip::multinom_reg(
            mode = "classification",
            penalty = penalty,
            mixture = mixture
          )
        }
      } %>%
      parsnip::set_engine("glmnet")

    # Create Workflow (to know variable roles from recipes) help(workflow)
    wf <- workflows::workflow() %>%
      workflows::add_model(mod_spec) %>%
      workflows::add_recipe(xy_recipe)

    # Fit model
    mod <- parsnip::fit(wf, data = data_train)

    # Standard regression
  } else if (nr_predictors == 1) {
    mod_spec <- {
      if (model == "regression") {
        parsnip::linear_reg(mode = "regression") %>%
          parsnip::set_engine("lm")
      } else if (model == "logistic") {
        parsnip::logistic_reg(mode = "classification") %>%
          parsnip::set_engine("glm")
      } else if (model == "multinomial") {
        parsnip::multinom_reg(mode = "classification") %>%
          parsnip::set_engine("glmnet")
      }
    }

    # Create Workflow (to know variable roles from recipes) help(workflow)
    wf <- workflows::workflow() %>%
      workflows::add_model(mod_spec) %>%
      workflows::add_recipe(xy_recipe)

    # Fit model
    mod <- parsnip::fit(wf, data = data_train)
  }

  # Prepare the test data; remove y and according to the recipe
  xy_testing <- rsample::assessment(object) %>%
    dplyr::select(-y)

  if (model == "regression") {
    # Apply model on new data; penalty
    holdout_pred <-
      stats::predict(mod, xy_testing) %>%
      dplyr::bind_cols(rsample::assessment(object) %>%
                         dplyr::select(y, id_nr))

    # Get RMSE; eval_measure = "rmse" library(tidyverse)
    eval_result <- select_eval_measure_val(eval_measure,
                                           holdout_pred = holdout_pred,
                                           truth = y, estimate = .pred
    )$.estimate
    # Sort output of RMSE, predictions and truth (observed y)
    output <- list(
      list(eval_result), list(holdout_pred$.pred), list(holdout_pred$y), list(preprocess_PCA),
      list(holdout_pred$id_nr)
    )
    names(output) <- c("eval_result", "predictions", "y", "preprocess_PCA", "id_nr")
  } else if (model == "logistic") {
    holdout_pred_class <-
      stats::predict(mod, xy_testing, type = c("class")) %>%
      dplyr::bind_cols(rsample::assessment(object) %>%
                         dplyr::select(y, id_nr))


    holdout_pred <-
      stats::predict(mod, xy_testing, type = c("prob")) %>%
      dplyr::bind_cols(rsample::assessment(object) %>%
                         dplyr::select(y, id_nr))

    holdout_pred$.pred_class <- holdout_pred_class$.pred_class

    # Get RMSE; eval_measure = "rmse"
    eval_result <- select_eval_measure_val(eval_measure,
                                           holdout_pred = holdout_pred,
                                           truth = y, estimate = .pred_class
    )$.estimate
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
  } else if (model == "multinomial") {
    holdout_pred_class <-
      stats::predict(mod, xy_testing, type = c("class")) %>%
      dplyr::bind_cols(rsample::assessment(object) %>%
                         dplyr::select(y, id_nr))


    holdout_pred <-
      stats::predict(mod, xy_testing, type = c("prob")) %>%
      dplyr::bind_cols(rsample::assessment(object) %>%
                         dplyr::select(y, id_nr))

    holdout_pred$.pred_class <- holdout_pred_class$.pred_class

    # Get RMSE; eval_measure = "rmse"
    eval_result <- select_eval_measure_val(eval_measure,
                                           holdout_pred = holdout_pred,
                                           truth = y, estimate = .pred_class
    )$.estimate
    # Sort output of RMSE, predictions and truth (observed y)
    output <- list(
      list(eval_result),
      list(holdout_pred$.pred_class),
      list(holdout_pred$y)
    )
    for (i in 1:length(unique(levels(holdout_pred$y))))
    {
      output[[3 + i]] <- list(holdout_pred[i])
    }
    output[[length(output) + 1]] <- list(preprocess_PCA)
    output[[length(output) + 1]] <- list(holdout_pred$id_nr)

    pred_names <- list()
    for (i in 1:length(unique(levels(holdout_pred$y))))
    {
      pred_names[i] <- paste(".pred_", i, sep = "")
    }
    names(output) <- c(
      "eval_result",
      "estimate",
      "truth",
      pred_names,
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
#' from same word embedding together in separate pca:s.
#' @return RMSE.
#' @noRd
fit_model_rmse_wrapper <- function(penalty = penalty,
                                   mixture = mixture,
                                   object,
                                   model,
                                   eval_measure,
                                   preprocess_PCA = preprocess_PCA,
                                   variable_name_index_pca = variable_name_index_pca,
                                   first_n_predictors = first_n_predictors,
                                   preprocess_step_center = preprocess_step_center,
                                   preprocess_step_scale = preprocess_step_scale,
                                   impute_missing = impute_missing) {
  fit_model_rmse(object,
                 model,
                 eval_measure,
                 penalty,
                 mixture,
                 preprocess_PCA = preprocess_PCA,
                 variable_name_index_pca = variable_name_index_pca,
                 first_n_predictors = first_n_predictors,
                 preprocess_step_center = preprocess_step_center,
                 preprocess_step_scale = preprocess_step_scale,
                 impute_missing = impute_missing
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
tune_over_cost <- function(object,
                           model,
                           eval_measure,
                           penalty,
                           mixture,
                           preprocess_PCA = preprocess_PCA,
                           variable_name_index_pca = variable_name_index_pca,
                           first_n_predictors = first_n_predictors,
                           preprocess_step_center = preprocess_step_center,
                           preprocess_step_scale = preprocess_step_scale,
                           impute_missing = impute_missing,
                           parameter_selection_method = parameter_selection_method) {
  T1 <- Sys.time()

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

  ## Sequence to select dimensions from the semreps. SM-article state:
  # "Adding 1, then multiplying by 1.3 and finally rounding to the nearest
  # integer (e.g., 1, 3, 5, 8, where the next number of dimensions to be tested are the first 12;
  # in other words ([8 +􏰄 1*] 􏱡 1.3)
  if (!is.na(first_n_predictors)) {
    stop <- first_n_predictors
    new_num <- 1
    selection_vector <- 1
    while (new_num < stop) {
      new_num <- round((new_num + 1) * 1.3)
      selection_vector <- c(selection_vector, new_num)
    }
    # Changing the last number to the maximum number of dimensions
    selection_vector[length(selection_vector)] <- first_n_predictors
    first_n_predictors <- selection_vector
    first_n_predictors
  }


  grid_inner <- base::expand.grid(
    penalty = penalty,
    mixture = mixture,
    preprocess_PCA = preprocess_PCA_value,
    first_n_predictors = first_n_predictors
  )

  #  Test models with the different hyperparameters for the inner samples
  tune_results <- purrr::pmap(
    list(
      grid_inner$penalty,
      grid_inner$mixture,
      grid_inner$preprocess_PCA,
      grid_inner$first_n_predictors
    ),
    fit_model_rmse_wrapper,
    object = object,
    model = model,
    eval_measure = eval_measure,
    variable_name_index_pca = variable_name_index_pca,
    preprocess_step_center = preprocess_step_center,
    preprocess_step_scale = preprocess_step_scale,
    impute_missing = impute_missing
  )

  # Sort the output to separate the rmse, predictions and truth
  tune_outputlist <- tune_results %>%
    dplyr::bind_rows() %>%
    split.default(names(.)) %>%
    purrr::map(na.omit)

  # Extract the RMSE.
  tune_eval_result <- unlist(tune_outputlist$eval_result$eval_result)

  # Add RMSE to the grid
  grid_inner_eval_result <- grid_inner %>%
    dplyr::mutate(eval_result = tune_eval_result)


  # Progression output
  best_eval <- bestParameters(
    data = grid_inner_eval_result,
    eval_measure = eval_measure,
    parameter_selection_method = parameter_selection_method
  )

  T2 <- Sys.time()
  time <- round(T2 - T1, digits = 2)

  variable_time <- sprintf(
    "(duration: %s %s).",
    time,
    units(time)
  )

  description_text <- paste(
    "Fold:", eval_measure,
    round(best_eval$eval_result, digits = 3),
    variable_time, "\n"
  )

  cat(colourise(description_text, "green"))


  return(grid_inner_eval_result)
}



#' Function to get the lowest eval_measure_val
#' @param data the data with parameters
#' @param eval_measure the evaluation measure which decide if min or max value should be selected
#' @param parameter_selection_method If several results are tied for different parameters (i.e., penalty or mixture),
#' then select the "first" or the "median" order.
#' @return The row with the best evaluation measure.
#' @noRd
bestParameters <- function(data,
                           eval_measure,
                           parameter_selection_method) {

  if (eval_measure %in% c(
    "accuracy", "bal_accuracy", "sens", "spec",
    "precision", "kappa", "f_measure", "roc_auc",
    "rsq", "cor_test")) {

      if (parameter_selection_method == "first") {
        bestParametersFunction <- function(data) data[which.max(data$eval_result), ]
      }

      if (parameter_selection_method == "median") {

        bestParametersFunction <- function(data) {
          max_value <- max(data$eval_result)
          tied_rows <- data %>%
            dplyr::filter(eval_result == max_value)

          median_index <- base::floor(nrow(tied_rows) / 2)
          return(tied_rows[median_index, ])
        }
      }
  }

  if (eval_measure == "rmse") {

    if (parameter_selection_method == "first") {
      bestParametersFunction <- function(data) data[which.min(data$eval_result), ]
    }

    if (parameter_selection_method == "median") {

      bestParametersFunction <- function(data){
          min_value <- min(data$eval_result)
          tied_rows <- data %>%
            dplyr::filter(eval_result == min_value)

          median_index <- base::floor(nrow(tied_rows) / 2)
          return(tied_rows[median_index, ])
      }
    }
  }
  #data = tuning_results[[10]]; hyper_parameter_vals
  results <- bestParametersFunction(data)
  return(results)
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
                                   variable_name_index_pca = variable_name_index_pca,
                                   first_n_predictors = first_n_predictors,
                                   preprocess_step_center = preprocess_step_center,
                                   preprocess_step_scale = preprocess_step_scale,
                                   impute_missing = impute_missing,
                                   parameter_selection_method = parameter_selection_method) {
  # Return row-bound tibble containing the INNER results
  results <- purrr::map_df(
    .x = object$splits,
    .f = tune_over_cost,
    penalty = penalty,
    mixture = mixture,
    preprocess_PCA = preprocess_PCA,
    variable_name_index_pca = variable_name_index_pca,
    model = model,
    eval_measure = eval_measure,
    first_n_predictors = first_n_predictors,
    preprocess_step_center = preprocess_step_center,
    preprocess_step_scale = preprocess_step_scale,
    impute_missing = impute_missing,
    parameter_selection_method = parameter_selection_method
  )
  return(results)
}


#' Train word embeddings to a numeric variable.
#'
#' textTrainRegression() trains word embeddings to a numeric or a factor variable.
#' @param x Word embeddings from textEmbed (or textEmbedLayerAggregation). If several word embedding are
#' provided in a list they will be concatenated.
#' @param y Numeric variable to predict.
#' @param x_append (optional) Variables to be appended after the word embeddings (x);
#' if wanting to preappend them before the word embeddings use the option first = TRUE.
#' If not wanting to train with word embeddings, set x = NULL (default = NULL).
#' @param append_first (boolean) Option to add variables before or after all word embeddings (default = False).
#' @param cv_method (character) Cross-validation method to use within a pipeline of nested outer and inner loops
#' of folds (see nested_cv in rsample). Default is using cv_folds in the outside folds and "validation_split"
#' using rsample::validation_split in the inner loop to achieve a development and assessment set (note that
#' for validation_split the inside_folds should be a proportion, e.g., inside_folds = 3/4); whereas "cv_folds"
#' uses rsample::vfold_cv to achieve n-folds in both the outer and inner loops.
#' @param outside_folds (numeric) Number of folds for the outer folds (default = 10).
#' @param inside_folds (numeric) The proportion of data to be used for modeling/analysis; (default proportion = 3/4).
#' For more information see validation_split in rsample.
#' @param strata (string or tibble; default "y") Variable to stratify according;
#' if a string the variable needs to be in the training set - if you want to stratify
#' according to another variable you can include it as a tibble (please note you
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
#' @param language_distribution (Character column) If you provide the raw language data used for making the embeddings,
#' the language distribution (i.e., a word and frequency table) will be saved to the model object. This enables
#' calculating similarity scores when the model is being applied to new language domains.
#' Note that this saves the individual words, which, if you are analyzing sensitive data, can be problematic from a
#' privacy perspective; to some extent this can be mitigated by increasing the number of words needed to be saved.
#' @param language_distribution_min_words (numeric) Minimum number a words need to occur in the data set to be saved to the
#' language distribution.
#' @param preprocess_step_center (boolean) Normalizes dimensions to have a mean of zero; default is set to TRUE.
#' For more info see (step_center in recipes).
#' @param preprocess_step_scale (boolean) Normalize dimensions to have a standard deviation of one;
#'  default is set to TRUE. For more info see (step_scale in recipes).
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
#' @param parameter_selection_method If several results are tied for different parameters (i.e., penalty or mixture),
#' then select the "first" or the "median" order.
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
#' @param simulate.p.value (Boolean) From fisher.test: a logical indicating whether to compute p-values by
#' Monte Carlo simulation, in larger than 2 × 2 tables.
#' @param seed (numeric) Set different seed (default = 2020).
#' @param ... For example settings in yardstick::accuracy to set event_level (e.g., event_level = "second").
#' @details
#' By default, NAs are treated as follows:
#'    1. rows with NAs in word embeddings are removed.
#'    2. rows with NAs in y are removed
#'    3. rows with NAs in  x_append are removed; if impute_missing is set to
#'       TRUE, missing values will be imputed using k-nearest neighbours.
#'    When rows are omitted, the user will get a warning.
#'    The CV predictions will include NAs with the same length as the input.
#' @return A (one-sided) correlation test between predicted and observed values; tibble
#' of predicted values (t-value, degree of freedom (df), p-value,
#'  alternative-hypothesis, confidence interval, correlation coefficient), as well as information about
#'  the model (preprossing_recipe, final_model and model_description).
#' @examples
#' # Examines how well the embeddings from the column "harmonytext" can
#' # predict the numerical values in the column "hilstotal".
#'
#' \dontrun{
#' trained_model <- textTrainRegression(
#'   x = word_embeddings_4$texts$harmonytext,
#'   y = Language_based_assessment_data_8$hilstotal,
#'   multi_cores = FALSE # This is FALSE due to CRAN testing and Windows machines.
#' )
#'
#' # Examine results (t-value, degree of freedom (df), p-value, alternative-hypothesis,
#' # confidence interval, correlation coefficient).
#'
#' trained_model$results
#' }
#' @seealso See \code{\link{textEmbedLayerAggregation}}, \code{\link{textTrainLists}} and
#' \code{\link{textTrainRandomForest}}.
#' @importFrom stats cor.test na.omit lm
#' @importFrom dplyr bind_cols select starts_with filter all_of add_row
#' @importFrom recipes recipe step_naomit step_center step_scale step_pca all_predictors
#' @importFrom rsample vfold_cv
#' @importFrom parsnip linear_reg set_engine multinom_reg
#' @importFrom tune tune control_grid tune_grid select_best collect_predictions
#' @importFrom magrittr %>%
#' @importFrom future plan multisession
#' @importFrom furrr future_map
#' @importFrom workflows workflow add_model add_recipe
#' @export
textTrainRegression <- function(x,
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
                                model = "regression", # model = "multinomial"
                                eval_measure = "default",
                                language_distribution = NULL,
                                language_distribution_min_words = 3,
                                preprocess_step_center = TRUE,
                                preprocess_step_scale = TRUE,
                                preprocess_PCA = NA,
                                penalty = 10^seq(-6, 6),
                                parameter_selection_method = "first",
                                mixture = c(0),
                                first_n_predictors = NA,
                                impute_missing = FALSE,
                                method_cor = "pearson",
                                model_description = "Consider writing a description of your model here",
                                multi_cores = "multi_cores_sys_default",
                                save_output = "all",
                                simulate.p.value = FALSE,
                                seed = 2020,
                                ...) {
  T1_textTrainRegression <- Sys.time()
  set.seed(seed)


  # Select correct eval_measure depending on model when default
  if (model == "regression" && eval_measure == "default") {
    eval_measure <- "rmse"
  } else if (model == "logistic" || model == "multinomial" && eval_measure == "default") {
    eval_measure <- "bal_accuracy"
  }

  # The fit_model_rmse function need to number of word embeddings -- instead of
  # sending a separate parameter number of embeddings are give as a comment in "model"
  if (tibble::is_tibble(x)) {
    comment(eval_measure) <- "1"
  } else {
    comment(eval_measure) <- paste(length(x))
  }

  # display warnings if x_append, x or y contain NA-values.
  if (sum(is.na(x_append)) > 0){
    warning("NAs in x_append have been omitted.")
  } else if (sum(is.na(x)) > 0){
    warning("NAs in x have been omitted.")
  } else if (sum(is.na(y)) > 0){
    warning("NAs in y have been omitted.")
  }

  # save for later use
  y_original <- y

  # Search and remove NA-values in y
  if (sum(is.na(y)) > 0){
    # find indexes of NA elements in y
    na_idx <- which(is.na(y) == TRUE)
    # remove rows with NA values in x and y
    y <- y[-c(na_idx)]
    x <- x[-c(na_idx),]
    x_append <- x_append[-c(na_idx),]
  }

  # Sorting out y
  if (tibble::is_tibble(y) || is.data.frame(y)) {
    y_name <- colnames(y)

    y <- tibble::as_tibble_col(y[[1]], column_name = "y")
  } else {
    y_name <- deparse(substitute(y))
    y <- tibble::as_tibble_col(y, column_name = "y")
  }

  if (model == "logistic" && anyNA(y[1])) {
    stop("In logistic regression you cannot currently have any NA(s) in y.")
  }

  # Sorting out x's
  variables_and_names <- sorting_xs_and_x_append(
    x = x,
    x_append = x_append,
    append_first = append_first, ...
  )
  x2 <- variables_and_names$x1
  x_name <- variables_and_names$x_name
  embedding_description <- variables_and_names$embedding_description
  x_append_names <- variables_and_names$x_append_names
  variable_name_index_pca <- variables_and_names$variable_name_index_pca
  rm(variables_and_names)

  xy <- dplyr::bind_cols(x2, y)

  xy$id_nr <- c(seq_len(nrow(xy)))

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

  # complete.cases is not neccassary
  # Cross-Validation inside_folds = 3/4; results_nested_resampling[[1]][[1]][[1]]
  if (cv_method == "cv_folds") {
    results_nested_resampling <- rlang::expr(rsample::nested_cv(
      xy,
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
    results_nested_resampling <- rlang::expr(rsample::nested_cv(
      xy,
      outside = rsample::vfold_cv(
        v = !!outside_folds,
        repeats = 1,
        strata = !!outside_strata_y,
        breaks = !!outside_breaks
      ),
      inside = rsample::validation_split(
        prop = !!inside_folds,
        strata = !!inside_strata_y,
        breaks = !!inside_breaks
      )
    ))
  }

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
      variable_name_index_pca = variable_name_index_pca,
      first_n_predictors = first_n_predictors,
      preprocess_step_center = preprocess_step_center,
      preprocess_step_scale = preprocess_step_scale,
      impute_missing = impute_missing,
      parameter_selection_method = parameter_selection_method
    )
  } else if (multi_cores_use == TRUE) {
    # The multisession plan uses the local cores to process the inner resampling loop.
    future::plan(future::multisession)
    # The object tuning_results is a list of data frames for each of the OUTER resamples.
    tuning_results <- furrr::future_map(
      .options = furrr::furrr_options(seed = seed),
      .x = results_nested_resampling$inner_resamples,
      .f = summarize_tune_results,
      model = model,
      eval_measure = eval_measure,
      penalty = penalty,
      mixture = mixture,
      preprocess_PCA = preprocess_PCA,
      variable_name_index_pca = variable_name_index_pca,
      first_n_predictors = first_n_predictors,
      preprocess_step_center = preprocess_step_center,
      preprocess_step_scale = preprocess_step_scale,
      impute_missing = impute_missing,
      parameter_selection_method = parameter_selection_method
    )
  }

  # Function to get the lowest eval_measure_val
  # Determine the best parameter estimate from each INNER sample to be used
  # for each of the outer resampling iterations:
  hyper_parameter_vals <-
    tuning_results %>%
    purrr::map_df(bestParameters, eval_measure, parameter_selection_method) %>%
    dplyr::select(c(penalty, mixture, preprocess_PCA, first_n_predictors))

  # Bind best results
  results_split_parameter <-
    dplyr::bind_cols(results_nested_resampling, hyper_parameter_vals)

  # Compute the outer re-sampling results for each of the comment(model)
  # splits using the corresponding tuning parameter value from results_split_parameter.
  results_outer <- purrr::pmap(
    list(
      object = results_nested_resampling$splits,
      penalty = results_split_parameter$penalty,
      mixture = results_split_parameter$mixture,
      preprocess_PCA = results_split_parameter$preprocess_PCA,
      first_n_predictors = results_split_parameter$first_n_predictors,
      variable_name_index_pca = list(variable_name_index_pca),
      model = model,
      eval_measure = list(eval_measure),
      preprocess_step_center = preprocess_step_center,
      preprocess_step_scale = preprocess_step_scale,
      impute_missing = impute_missing
    ),
    # this is THE function for the regression models
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
    collected_results <- classification_results(outputlist_results_outer = outputlist_results_outer, ...)

    #  Save predictions outside list to make similar structure as model == regression output.
    predy_y <- collected_results$predy_y
    # Remove the predictions from list
    collected_results[[1]] <- NULL
  } else if (model == "multinomial") {
    collected_results <- classification_results_multi(
      outputlist_results_outer = outputlist_results_outer,
      simulate.p.value = simulate.p.value, ...
    )

    #  Save predictions outside list to make similar structure as model == regression output.
    predy_y <- collected_results$predy_y
    # Remove the predictions from list
    collected_results[[1]] <- NULL
  }

  # Correct for NA-values in y
  # Insert NA's into predictions component of the model object at
  # the sae indexes as ehere there was NA-values in y
  if (sum(is.na(y_original)) > 0){
    for (idx in seq_along(1:length(y_original))){
      if (idx %in% na_idx){
        # create row with NA-values and insert into predy_y
        predy_y <- dplyr::add_row(.before = c(idx), .data = predy_y)
      }
    }
    predy_y$id_nr <- c(1:length(y_original))
  }

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
    # If testing N first predictors help(step_scale) first_n_predictors = 3
    if (!is.na(first_n_predictors)) {
      # Select y and id
      Nvariable_totals <- length(xy_all)
      variable_names <- colnames(xy_all[(first_n_predictors + 1):(Nvariable_totals - 2)])
    } else {
      variable_names <- c("id_nr")
    }

    # [0,] is added to just get the col names (and avoid saving all the data with the receipt) help(step_naomit)
    final_recipe <- # xy %>%
      recipes::recipe(y ~ ., xy_all[0, ]) %>%
      recipes::update_role(all_of(variable_names), new_role = "Not_predictors") %>%
      recipes::update_role(id_nr, new_role = "id variable") %>%
      recipes::update_role(y, new_role = "outcome")

    if (!impute_missing) {
      final_recipe <- recipes::step_naomit(final_recipe, recipes::all_predictors(), skip = TRUE)
    } else if (impute_missing) {
      final_recipe <- recipes::step_impute_knn(final_recipe, recipes::all_predictors(), neighbors = 10)
    }

    if (preprocess_step_center) {
      final_recipe <- recipes::step_center(final_recipe, recipes::all_predictors())
    }
    if (preprocess_step_scale) {
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
      recipes::update_role(-id_nr, new_role = "predictor") %>%
      recipes::update_role(y, new_role = "outcome")

    if (!impute_missing) {
      final_recipe <- recipes::step_naomit(final_recipe, recipes::all_predictors(), skip = TRUE)
    } else if (impute_missing) {
      final_recipe <- recipes::step_impute_knn(final_recipe, recipes::all_predictors(), neighbors = 10)
    }

    if (preprocess_step_center) {
      final_recipe <- recipes::step_center(final_recipe, recipes::all_predictors())
    }
    if (preprocess_step_scale) {
      final_recipe <- recipes::step_scale(final_recipe, recipes::all_predictors())
    }

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
            recipes::step_pca(dplyr::matches(!!i), threshold = preprocess_PCA, prefix = paste("PCA_", i, "_"))
        }
      }
    }
  }


  # Creating recipe in another environment to avoid saving unnecessarily large parts of the environment
  # when saving the object to rda, rds or Rdata.
  # http://r.789695.n4.nabble.com/Model-object-when-generated-in-a-function-saves-
  # entire-environment-when-saved-td4723192.html
  recipe_save_small_size <- function(final_recipe, xy_all) {
    env_final_recipe <- new.env(parent = globalenv())
    env_final_recipe$xy_all <- xy_all
    env_final_recipe$final_recipe <- final_recipe


    preprocessing_recipe <- with(
      env_final_recipe, final_recipe
    )

    # Optionally remove xy_all if you are concerned about memory and it's no longer needed
    remove("xy_all", envir = env_final_recipe)
    remove("final_recipe", envir = env_final_recipe)

    return(list(preprocessing_recipe)) # preprocessing_recipe_save_trained
  }

  preprocessing_recipe_save <- recipe_save_small_size(
    final_recipe = final_recipe,
    xy_all = xy_all
  )

  # Check number of predictors (to later decide standard or multiple regression)
  # To load the prepared training data into a variable juice() is used.
  # It extracts the data from the xy_recipe object.
  preprocessing_recipe_prep <- recipes::prep(final_recipe, xy_all)

  nr_predictors <- recipes::juice(preprocessing_recipe_prep)

  # This is so that nr_predictors > 3 and nr_predictors == 3 should work
  if ("strata" %in% colnames(nr_predictors)) {
    nr_predictors <- nr_predictors %>%
      select(-strata)
  }
  nr_predictors <- length(nr_predictors)

  ####### NEW ENVIRONMENT
  model_save_small_size <- function(xy_all, final_recipe, penalty, mixture, model, nr_predictors) {
    env_final_model <- new.env(parent = globalenv())
    env_final_model$xy_all <- xy_all
    env_final_model$final_recipe <- final_recipe
    env_final_model$penalty_mode <- statisticalMode(penalty)
    env_final_model$mixture_mode <- statisticalMode(mixture)
    env_final_model$model <- model
    env_final_model$nr_predictors <- nr_predictors
    env_final_model$statisticalMode <- statisticalMode
    env_final_model$`%>%` <- `%>%`

    final_predictive_model <- with(env_final_model, {
      if (nr_predictors > 3) {
        final_predictive_model_spec <-
          if (model == "regression") {
            parsnip::linear_reg(penalty = penalty_mode, mixture = mixture_mode)
          } else if (model == "logistic") {
            parsnip::logistic_reg(mode = "classification", penalty = penalty_mode, mixture = mixture_mode)
          } else if (model == "multinomial") {
            parsnip::multinom_reg(mode = "classification", penalty = penalty_mode, mixture = mixture_mode)
          }

        final_predictive_model_spec <- final_predictive_model_spec %>%
          parsnip::set_engine("glmnet")

        # Create Workflow (to know variable roles from recipes) help(workflow)
        wf_final <- workflows::workflow() %>%
          workflows::add_model(final_predictive_model_spec) %>%
          workflows::add_recipe(final_recipe[[1]])

        parsnip::fit(wf_final, data = xy_all)
      } else if (nr_predictors == 3) {
        final_predictive_model_spec <-
          if (model == "regression") {
            parsnip::linear_reg(mode = "regression") %>%
              parsnip::set_engine("lm")
          } else if (model == "logistic") {
            parsnip::logistic_reg(mode = "classification") %>%
              parsnip::set_engine("glm")
          } else if (model == "multinomial") {
            parsnip::multinom_reg(mode = "classification") %>%
              parsnip::set_engine("glmnet")
          }

        wf_final <- workflows::workflow() %>%
          workflows::add_model(final_predictive_model_spec) %>%
          workflows::add_recipe(final_recipe[[1]])

        parsnip::fit(wf_final, data = xy_all)
      }
    })
    remove("final_recipe", envir = env_final_model)
    remove("xy_all", envir = env_final_model)
    return(final_predictive_model)
  }

  final_predictive_model <- model_save_small_size(
    xy_all,
    preprocessing_recipe_save,
    results_split_parameter$penalty,
    results_split_parameter$mixture,
    model,
    nr_predictors
  )

  # Removing parts of the model not needed for prediction (primarily removing training data)
  final_predictive_model$pre$mold$predictors <- NULL
  final_predictive_model$pre$mold$outcomes <- NULL
  final_predictive_model$pre$mold$extras <- NULL


  ##### NEW ENVIRONMENT END

  ##########  DESCRIBING THE MODEL  ##########
  ############################################

  x_name_description <- paste("x word_embeddings = ", x_name)
  x_append_names_description <- paste("x_append = ", x_append_names)
  y_name_description <- paste("y = ", y_name)
  cv_method_description <- paste("cv_method = ", deparse(cv_method))
  strata_description <- paste("strata = ", strata_name)
  outside_folds_description <- paste("outside_folds = ", deparse(outside_folds))
  outside_strata_y_description <- paste("outside_strata = ", deparse(outside_strata))
  inside_folds_description <- paste("inside_folds = ", deparse(inside_folds))
  inside_strata_y_description <- paste("inside_strata = ", deparse(inside_strata))

  penalty_setting <- paste("penalty_setting = ", deparse(penalty))
  mixture_setting <- paste("mixture_setting = ", deparse(mixture))
  preprocess_PCA_setting <- paste("preprocess_PCA_setting = ", deparse(preprocess_PCA))
  first_n_predictors_setting <- paste("first_n_predictors_setting = ", deparse(first_n_predictors))

  # Saving the final mtry and min_n used for the final model.
  penalty_description <- paste("penalty in final model = ", deparse(statisticalMode(results_split_parameter$penalty)))
  penalty_fold_description <- paste("penalty in each fold = ", deparse(results_split_parameter$penalty))

  mixture_description <- paste("mixture in final model = ", deparse(statisticalMode(results_split_parameter$mixture)))
  mixture_fold_description <- paste("mixture in each fold = ", deparse(results_split_parameter$mixture))

  preprocess_PCA_description <- paste(
    "preprocess_PCA in final model = ",
    deparse(statisticalMode(results_split_parameter$preprocess_PCA))
  )
  preprocess_PCA_fold_description <- paste(
    "preprocess_PCA in each fold = ",
    deparse(results_split_parameter$preprocess_PCA)
  )

  first_n_predictors_description <- paste(
    "first_n_predictors in final model = ",
    deparse(statisticalMode(results_split_parameter$first_n_predictors))
  )
  first_n_predictors_fold_description <- paste(
    "first_n_predictors in each fold = ",
    deparse(results_split_parameter$first_n_predictors)
  )

  preprocess_step_center <- paste("preprocess_step_center_setting = ", deparse(preprocess_step_center))
  preprocess_step_scale <- paste("preprocess_step_scale_setting = ", deparse(preprocess_step_scale))

  impute_missing <- paste("impute_missing_setting = ", deparse(impute_missing))

  # Getting time and date
  T2_textTrainRegression <- Sys.time()
  Time_textTrainRegression <- T2_textTrainRegression - T1_textTrainRegression
  Time_textTrainRegression <- sprintf(
    "Duration to train text: %f %s",
    Time_textTrainRegression,
    units(Time_textTrainRegression)
  )
  Date_textTrainRegression <- Sys.time()
  time_date <- paste(Time_textTrainRegression,
                     "; Date created: ", Date_textTrainRegression,
                     sep = "",
                     collapse = " "
  )

  text_version <- paste("; text_version: ", packageVersion("text"), ".", sep = "")

  # Describe model; adding user's-description + the name of the x and y
  model_description_detail <- c(
    x_name_description,
    x_append_names_description,
    y_name_description,
    cv_method_description,
    strata_description,
    outside_folds_description,
    outside_strata_y_description,
    inside_folds_description,
    inside_strata_y_description,
    penalty_setting,
    penalty_description,
    penalty_fold_description,
    mixture_setting,
    mixture_description,
    mixture_fold_description,
    preprocess_step_center,
    preprocess_step_scale,
    preprocess_PCA_setting,
    preprocess_PCA_description,
    preprocess_PCA_fold_description,
    first_n_predictors_setting,
    first_n_predictors_description,
    first_n_predictors_fold_description,
    impute_missing,
    embedding_description,
    model_description,
    time_date,
    text_version
  )

  ###### Saving and arranging output ######
  ##########################################

  if(!is.null(language_distribution)){

    language_distribution_res <- textTokenizeAndCount(
      data = language_distribution,
      n_remove_threshold = language_distribution_min_words)
  } else {
    language_distribution_res = paste0("No language distribution have been saved; ",
    "if you want to attach a language distribution use textTokenizeAndCount(). ",
    "Add the distribution under the language_distribution name so that it can be found by the text ",
    "prediction functions.")
  }

  if (model == "regression") {
    if (save_output == "all") {
      final_results <- list(
        predy_y, final_predictive_model, model_description_detail, #final_recipe <- preprocessing_recipe_save[[1]],
        collected_results[[2]]
      )
      names(final_results) <- c(
        "predictions",  "final_model", "model_description", #"final_recipe",
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
        predy_y, final_predictive_model, model_description_detail, # preprocessing_recipe_save
        collected_results$roc_curve_data, collected_results$roc_curve_plot, collected_results$fisher,
        collected_results$chisq, collected_results$results_collected
      )
      names(final_results) <- c(
        "predictions",  "final_model", "model_description", #"final_recipe",
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
  } else if (model == "multinomial") {
    if (save_output == "all") {
      final_results <- list(
        predy_y, final_predictive_model, model_description_detail, # preprocessing_recipe_save,
        collected_results$roc_curve_data, collected_results$roc_curve_plot, collected_results$fisher,
        collected_results$chisq, collected_results$results_collected
      )
      names(final_results) <- c(
        "predictions", "final_model", "model_description",  # "final_recipe",
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

  # Remove object to minimize model size when saved to rds; use this to
  # check sizes: sort(sapply(ls(),function(x) {object.size(get(x))}))
  remove(x)
  remove(x_append)
  remove(y)
  remove(x2)
  remove(xy)

  remove(predy_y)
  remove(preprocessing_recipe_save)
  remove(final_predictive_model)
  remove(collected_results)
  remove(model_description_detail)
  remove(results_nested_resampling)
  remove(tuning_results)
  remove(hyper_parameter_vals)
  remove(results_split_parameter)
  remove(results_outer)
  remove(outputlist_results_outer)
  remove(xy_all)
  remove(final_recipe)

  # Remove objects to further minimize model size when saved to rds
  remove(embedding_description)
  remove(x_append_names_description)
  remove(x_append_names)
  remove(variable_name_index_pca)
  remove(preprocessing_recipe_prep)
  remove(nr_predictors)
  remove(language_distribution)

  final_results <- c(
    list(language_distribution = language_distribution_res),
    final_results)

  return(final_results)
}
