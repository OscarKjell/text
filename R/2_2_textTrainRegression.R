
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
#' @noRd
fit_model_rmse <- function(object, model = "regression", eval_measure = "rmse", penalty = 1, mixture = 0,
                           preprocess_PCA = NA, variable_name_index_pca = NA, first_n_predictors = NA,
                           preprocess_step_center = TRUE, preprocess_step_scale = TRUE, impute_missing = FALSE) {


  data_train <- rsample::analysis(object)   # mdata_train <- rsample::assessment(object)
  data_train <- tibble::as_tibble(data_train)

  # If testing N first predictors help(step_scale) first_n_predictors = 3
  if (!is.na(first_n_predictors)) {
      #Select y and id
      Nvariable_totals <- length(data_train)
      variable_names <- colnames(data_train[(first_n_predictors+1):(Nvariable_totals-2)])
  }else {
    variable_names = "id_nr"
      }

  # Recipe for one embedding input summary(xy_recipe) help(all_of) library(tidyverse) help(step_naomit)
  if (colnames(data_train[1]) == "Dim1") {
    xy_recipe <- data_train %>%
      recipes::recipe(y ~ .) %>%
      recipes::update_role(dplyr::all_of(variable_names), new_role = "Not_predictors") %>%
      recipes::update_role(id_nr, new_role = "id variable") %>%
      recipes::update_role(y, new_role = "outcome") #%>%

  if (!impute_missing){
    xy_recipe <- recipes::step_naomit(xy_recipe, recipes::all_predictors(), skip = TRUE)
  } else if(impute_missing){
    xy_recipe <- recipes::step_knnimpute(xy_recipe, recipes::all_predictors(), neighbors = 10) #, skip = TRUE
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
      } #%>%
      #recipes::prep()
    xy_recipe_prep <- recipes::prep(xy_recipe)


  # Recipe for multiple word embedding input (with possibility of separate PCAs)
  } else {

    xy_recipe <- data_train %>%
      recipes::recipe(y ~ .) %>%
      # recipes::step_BoxCox(all_predictors()) %>%  preprocess_PCA = NULL, preprocess_PCA = 0.9 preprocess_PCA = 2
      recipes::update_role(id_nr, new_role = "id variable") %>%
      recipes::update_role(-id_nr, new_role = "predictor") %>%
      recipes::update_role(y, new_role = "outcome")

    if (!impute_missing){
      xy_recipe <- recipes::step_naomit(xy_recipe, recipes::all_predictors(), skip = TRUE)
    } else if(impute_missing){
      xy_recipe <- recipes::step_knnimpute(xy_recipe, recipes::all_predictors(), neighbors = 10) #, skip = TRUE
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
  if(!is.na(first_n_predictors) & is.na(preprocess_PCA)){
    # Get number of predictors from receipe
    nr_predictors <- table(xy_recipe_prep[[1]]$role)[["predictor"]]

  } else if (!is.na(preprocess_PCA)) {
    # To load the prepared training data into a variable juice() is used.
    # It extracts the data from the xy_recipe object.
    nr_predictors <- recipes::juice(xy_recipe_prep)
    #Count number of PCAs
    nr_predictors <- length(grep(x = colnames(nr_predictors), pattern = "PC"))
    } else if (is.na(preprocess_PCA) & is.na(first_n_predictors)) {
      nr_predictors <- recipes::juice(xy_recipe_prep)
      nr_predictors <- length(nr_predictors) - 2
    }

  # Ridge and/or Lasso
  if (nr_predictors > 1) {
    # Create and fit model
    mod_spec <-
      {
        if (model == "regression") {
          parsnip::linear_reg(penalty = penalty, mixture = mixture)
        } else if (model == "logistic") parsnip::logistic_reg(mode = "classification", penalty = penalty, mixture = mixture)
      } %>%
      parsnip::set_engine("glmnet") #%>%
      #parsnip::fit(y ~ ., data = xy_training)

    # Create Workflow (to know variable roles from recipes) help(workflow)
    wf <- workflows::workflow() %>%
      workflows::add_model(mod_spec) %>%
      workflows::add_recipe(xy_recipe)

    # Fit model
    mod <-  parsnip::fit(wf, data = data_train)

    # Standard regression
  } else if (nr_predictors == 1) {
    mod_spec <-
      {
        if (model == "regression") {
          parsnip::linear_reg(mode = "regression") %>%
            parsnip::set_engine("lm")
        } else if (model == "logistic") {
          parsnip::logistic_reg(mode = "classification") %>%
            parsnip::set_engine("glm")
        }
      } #%>%
      #parsnip::fit(y ~ ., data = xy_training)

    # Create Workflow (to know variable roles from recipes) help(workflow)
    wf <- workflows::workflow() %>%
      workflows::add_model(mod_spec) %>%
      workflows::add_recipe(xy_recipe)

    # Fit model
    mod <-  parsnip::fit(wf, data = data_train)
  }

  # Prepare the test data; remove y and according to the recipe
  xy_testing <- rsample::assessment(object) %>%
    dplyr::select(-y)

  if (model == "regression") {
    # Apply model on new data; penalty
    holdout_pred <-
      stats::predict(mod, xy_testing)  %>%
      dplyr::bind_cols(rsample::assessment(object) %>%
      dplyr::select(y, id_nr))

    # Get RMSE; eval_measure = "rmse" library(tidyverse)
    eval_result <- select_eval_measure_val(eval_measure, holdout_pred = holdout_pred, truth = y, estimate = .pred)$.estimate
    # Sort output of RMSE, predictions and truth (observed y)
    output <- list(list(eval_result), list(holdout_pred$.pred), list(holdout_pred$y), list(preprocess_PCA), list(holdout_pred$id_nr))
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
    class <- colnames(holdout_pred[2])

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
                           impute_missing = impute_missing) {

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

  ## Sequence to select dimensions from the semreps. SM-article state: "Adding 1, then multiplying by 1.3 and finally rounding to the nearest integer (e.g., 1, 3, 5, 8, where the next number of dimensions to be tested are the first 12; in other words ([8 +􏰄 1*] 􏱡 1.3)
  if(!is.na(first_n_predictors)){
    stop = first_n_predictors
   new_num = 1
   selection_vector = 1
   while(new_num < stop) {
     new_num  <-  round((new_num + 1) * 1.3)
     selection_vector  <-  c(selection_vector, new_num)
   }
   #Changing the last number to the maximum number of dimensions
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

  # Test models with the different hyperparameters for the inner samples
  tune_results <- purrr::pmap(list(
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
                                   variable_name_index_pca = variable_name_index_pca,
                                   first_n_predictors = first_n_predictors,
                                   preprocess_step_center = preprocess_step_center,
                                   preprocess_step_scale = preprocess_step_scale,
                                   impute_missing = impute_missing) {

  # Return row-bound tibble containing the INNER results
  purrr::map_df(
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
    impute_missing = impute_missing
  )
}


#' Train word embeddings to a numeric variable.
#'
#' @param x Word embeddings from textEmbed (or textEmbedLayerAggregation).
#' @param y Numeric variable to predict.
#' @param model Type of model. Default is "regression"; see also "logistic" for classification.
#' @param cv_method Cross-validation method to use within a pipeline of nested outer and inner loops of folds (see nested_cv in rsample).
#' Default is using cv_folds in the outside folds and "validation_split" using rsample::validation_split in the inner loop to achieve a development and assessment
#' set (note that for validation_split the inside_folds should be a proportion, e.g., inside_folds = 3/4); whereas "cv_folds" uses rsample::vfold_cv to achieve n-folds
#' in both the outer and inner loops.
#' @param outside_folds Number of folds for the outer folds (default = 10).
#' @param outside_strata_y Variable to stratify according (default y; can set to NULL).
#' @param outside_breaks The number of bins wanted to stratify a numeric stratification variable in the outer cross-validation loop.
#' @param inside_folds The proportion of data to be used for modeling/analysis; (default proportion = 3/4). For more information
#' see validation_split in rsample.
#' @param inside_strata_y Variable to stratify according (default y; can set to NULL).
#' @param inside_breaks The number of bins wanted to stratify a numeric stratification variable in the inner cross-validation loop.
#' @param eval_measure Type of evaluative measure to select models from. Default = "rmse" for regression and "bal_accuracy"
#' for logistic. For regression use "rsq" or "rmse"; and for classification use "accuracy", "bal_accuracy", "sens", "spec", "precision", "kappa", "f_measure",
#' or "roc_auc",(for more details see the yardstick package).
#' @param preprocess_step_center normalizes dimensions to have a mean of zero; default is set to TRUE. For more info see (step_center in recipes).
#' @param preprocess_step_scale  normalize dimensions to have a standard deviation of one. For more info see (step_scale in recipes).
#' @param preprocess_PCA Pre-processing threshold for PCA (to skip this step set it to NA).
#' Can select amount of variance to retain (e.g., .90 or as a grid c(0.80, 0.90)); or
#' number of components to select (e.g., 10). Default is "min_halving", which is a function
#' that selects the number of PCA components based on number  of participants and feature (word embedding dimensions)
#' in the data. The formula is:
#' preprocess_PCA = round(max(min(number_features/2), number_participants/2), min(50, number_features))).
#' @param penalty hyper parameter that is tuned
#' @param mixture hyper parameter that is tuned default = 0 (hence a pure ridge regression).
#' @param first_n_predictors by default this setting is turned off (i.e., NA). To use this method, set it to the highest number of predictors you want to test.
#' Then the X first dimensions are used in training, using a sequence from Kjell et al., 2019 paper in Psychological Methods.
#' Adding 1, then multiplying by 1.3 and finally rounding to the nearest integer (e.g., 1, 3, 5, 8).
#' This option is currently only possible for one embedding at the time.
#' @param method_cor Type of correlation used in evaluation (default "pearson";
#' can set to "spearman" or "kendall").
#' @param impute_missing default FALSE (can be set to TRUE if something else than word_embeddings are trained).
#' @param model_description Text to describe your model (optional; good when sharing the model with others).
#' @param multi_cores If TRUE it enables the use of multiple cores if the computer system allows for it (i.e., only on unix, not windows). Hence it
#' makes the analyses considerably faster to run. Default is "multi_cores_sys_default", where it automatically uses TRUE for Mac and Linux and FALSE for Windows.
#' @param save_output Option not to save all output; default "all". see also "only_results" and "only_results_predictions".
#' @param seed Set different seed.
#' @param ... For example settings in yardstick::accuracy to set event_level (e.g., event_level = "second").
#' @return A (one-sided) correlation test between predicted and observed values; tibble of predicted values, as well as information
#' about the model (preprossing_recipe, final_model and model_description).
#' @examples
#' \donttest{
#' results <- textTrainRegression(
#'   word_embeddings_4$harmonytext,
#'   Language_based_assessment_data_8$hilstotal,
#'   multi_cores = FALSE # This is FALSE due to CRAN testing and Windows machines.
#' )
#' }
#' @seealso see \code{\link{textEmbedLayerAggregation}} \code{\link{textTrainLists}}
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
#' @importFrom workflows workflow add_model add_recipe
#' @export
textTrainRegression <- function(x,
                                y,
                                cv_method = "validation_split",
                                outside_folds = 10,
                                outside_strata_y = "y",
                                outside_breaks = 4,
                                inside_folds = 3/4,
                                inside_strata_y = "y",
                                inside_breaks = 4,
                                model = "regression",
                                eval_measure = "default",
                                preprocess_step_center = TRUE,
                                preprocess_step_scale = TRUE,
                                preprocess_PCA = NA,
                                penalty = 10^seq(-16, 16),
                                mixture = c(0),
                                first_n_predictors = NA,
                                impute_missing = FALSE,
                                method_cor = "pearson",
                                model_description = "Consider writing a description of your model here",
                                multi_cores = "multi_cores_sys_default",
                                save_output = "all",
                                seed = 2020,
                                ...) {

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

      # Get the name of the first column for the PCA
      #VV1[i] <- colnames(xlist[[i]])[[1]]
    }
    # V1 <- unlist(VV1)

    # Make vector with each index so that we can allocate them separately for the PCAs
    variable_name_index_pca <- list()
    for (i in 1:Nword_variables) {
      variable_name_index_pca[i] <- paste("Dim_we", i, sep = "")
    }

    # Make one df rather then list.
    x1 <- dplyr::bind_cols(xlist)
  }
  ############ End for multiple word embeddings ############
  ##########################################################

  x2 <- dplyr::select(x1, dplyr::starts_with("Dim"))
  xy <- cbind(x2, y)
  xy <- tibble::as_tibble(xy)
  xy$id_nr <- c(seq_len(nrow(xy)))

  # complete.cases is not neccassary
  #xy <- tibble::as_tibble(xy[stats::complete.cases(xy), ])

  # Cross-Validation help(nested_cv) help(vfold_cv) help(validation_split) inside_folds = 3/4
  if(cv_method == "cv_folds") {
    results_nested_resampling <- rlang::expr(rsample::nested_cv(xy,
                                                                outside = rsample::vfold_cv(
                                                                  v       = !!outside_folds,
                                                                  repeats = 1,
                                                                  strata  = !!outside_strata_y,
                                                                  breaks  = !!outside_breaks
                                                                ), #
                                                                inside = rsample::vfold_cv(
                                                                  v       = !!inside_folds,
                                                                  repeats = 1,
                                                                  strata  = !!inside_strata_y,
                                                                  breaks  = !!inside_breaks
                                                                )
    ))
  }
  if(cv_method == "validation_split") {
    results_nested_resampling <- rlang::expr(rsample::nested_cv(xy,
                                                                outside = rsample::vfold_cv(
                                                                  v       = !!outside_folds,
                                                                  repeats = 1,
                                                                  strata  = !!outside_strata_y,
                                                                  breaks  = !!outside_breaks
                                                                ),
                                                                inside = rsample::validation_split(
                                                                  prop   = !!inside_folds,
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
      impute_missing = impute_missing
    )
  } else if (multi_cores_use == TRUE) {
    # The multisession plan uses the local cores to process the inner resampling loop.
    # help(multisession) help(plan)
    future::plan(future::multisession)
    # The object tuning_results is a list of data frames for each of the OUTER resamples.
    # help(future_map)
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
      impute_missing = impute_missing
    )
  }

  # Function to get the lowest eval_measure_val
  if(eval_measure %in% c("accuracy" , "bal_accuracy", "sens", "spec", "precision", "kappa", "f_measure", "roc_auc", "rsq", "cor_test")) {
    bestParameters <- function(dat) dat[which.max(dat$eval_result), ]
  } else if (eval_measure == "rmse"){
    bestParameters <- function(dat) dat[which.min(dat$eval_result), ]
  }

  # Determine the best parameter estimate from each INNER sample to be used
  # for each of the outer resampling iterations:
  hyper_parameter_vals <-
    tuning_results %>%
    purrr::map_df(bestParameters) %>%
    dplyr::select(c(penalty, mixture, preprocess_PCA, first_n_predictors))

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
      first_n_predictors = results_split_parameter$first_n_predictors,
      variable_name_index_pca = list(variable_name_index_pca),
      model = model,
      eval_measure = eval_measure,
      preprocess_step_center = preprocess_step_center,
      preprocess_step_scale = preprocess_step_scale,
      impute_missing = impute_missing
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
    collected_results <- classification_results(outputlist_results_outer = outputlist_results_outer, ...)

    #  Save predictions outside list to make similar structure as model == regression output.
    predy_y <- collected_results$predy_y
    # Remove the predictions from list
    collected_results[[1]] <- NULL
  }

  ##### Construct final model to be saved and applied on other data  ########
  ############################################################################
  xy_all <- xy

  ######### One word embedding as input
  if (colnames(xy_all[1]) == "Dim1") {

    # If testing N first predictors help(step_scale) first_n_predictors = 3
    if (!is.na(first_n_predictors)) {
      #Select y and id
      Nvariable_totals <- length(xy_all)
      variable_names <- colnames(xy_all[(first_n_predictors+1):(Nvariable_totals-2)])
    }else {
      variable_names = "id_nr"
    }

    # [0,] is added to just get the col names (and avoid saving all the data with the receipt) help(step_naomit)
    final_recipe <- # xy %>%
      recipes::recipe(y ~ ., xy_all[0, ]) %>%
      recipes::update_role(all_of(variable_names), new_role = "Not_predictors") %>%
      recipes::update_role(id_nr, new_role = "id variable") %>%
     # recipes::update_role(-id_nr, new_role = "predictor") %>%
      recipes::update_role(y, new_role = "outcome") #%>%
      # recipes::step_BoxCox(all_predictors())

    if (!impute_missing){
      final_recipe <- recipes::step_naomit(final_recipe, recipes::all_predictors(), skip = TRUE)
    } else if(impute_missing){
      final_recipe <- recipes::step_knnimpute(final_recipe, recipes::all_predictors(), neighbors = 10)
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

    final_recipe <- recipes::recipe(y ~ ., xy_all[0, ]) %>%
      recipes::update_role(id_nr, new_role = "id variable") %>%
      recipes::update_role(-id_nr, new_role = "predictor") %>%
      recipes::update_role(y, new_role = "outcome")

    if (!impute_missing){
      final_recipe <- recipes::step_naomit(final_recipe, recipes::all_predictors(), skip = TRUE)
    } else if(impute_missing){
      final_recipe <- recipes::step_knnimpute(final_recipe, recipes::all_predictors(), neighbors = 10)
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
            # recipes::step_pca(., recipes::all_predictors(), threshold = preprocess_PCA)
            recipes::step_pca(dplyr::matches(!!i), threshold = preprocess_PCA, prefix = paste("PCA_", i, "_"))
        }
      }
    }
  }


  # Creating recipe in another environment to avoid saving unnecessarily large parts of the environment
  # when saving the object to rda, rds or Rdata.
  # http://r.789695.n4.nabble.com/Model-object-when-generated-in-a-function-saves-entire-environment-when-saved-td4723192.html
  recipe_save_small_size <- function(final_recipe, xy_all){

    env_final_recipe <- new.env(parent = globalenv())
    env_final_recipe$xy_all <- xy_all
    env_final_recipe$final_recipe <- final_recipe

    with(env_final_recipe, preprocessing_recipe_save <- suppressWarnings(recipes::prep(final_recipe,
                                                                                       xy_all,
                                                                                       retain = FALSE)))
  }

  preprocessing_recipe_save <- recipe_save_small_size(final_recipe = final_recipe, xy_all = xy_all)

  #Check number of predictors (to later decide standard or multiple regression)
  # To load the prepared training data into a variable juice() is used.
  # It extracts the data from the xy_recipe object.
  preprocessing_recipe_prep <- recipes::prep(final_recipe, xy_all)
  nr_predictors <- recipes::juice(preprocessing_recipe_prep)
  nr_predictors <- length(nr_predictors)

  ####### NEW ENVIRONMENT
  model_save_small_size <- function(xy_all, final_recipe, results_split_parameter, model, nr_predictors){
    env_final_model <- new.env(parent = globalenv())
    env_final_model$xy_all       <- xy_all
    env_final_model$final_recipe <- final_recipe
    env_final_model$penalty_mode <- statisticalMode(results_split_parameter$penalty)
    env_final_model$mixture_mode <- statisticalMode(results_split_parameter$mixture)
    env_final_model$model        <- model
    env_final_model$nr_predictors        <- nr_predictors
    env_final_model$statisticalMode <- statisticalMode
    env_final_model$`%>%`  <-  `%>%`

    with(env_final_model,
  if (nr_predictors > 3) {
    # Create and fit model; penalty=NULL mixture = NULL
    final_predictive_model_spec <-
      {
        if (model == "regression") {
          parsnip::linear_reg(penalty = penalty_mode, mixture = mixture_mode)
        } else if (model == "logistic") parsnip::logistic_reg(mode = "classification", penalty = penalty_mode, mixture = mixture_mode)
      } %>%
      parsnip::set_engine("glmnet") #%>%
      #parsnip::fit(y ~ ., data = xy_final)

      # Create Workflow (to know variable roles from recipes) help(workflow)
      wf_final <- workflows::workflow() %>%
        workflows::add_model(final_predictive_model_spec) %>%
        workflows::add_recipe(final_recipe)

      # Fit model
      final_predictive_model <-  parsnip::fit(wf_final, data = xy_all)

    # Standard regression
  } else if (nr_predictors == 3) {
    final_predictive_model_spec <-
      {
        if (model == "regression") {
          parsnip::linear_reg(mode = "regression") %>%
            parsnip::set_engine("lm")
        } else if (model == "logistic") {
          parsnip::logistic_reg(mode = "classification") %>%
            parsnip::set_engine("glm")
        }
      } #%>%
      #parsnip::fit(y ~ ., data = xy_all)

    # Create Workflow (to know variable roles from recipes) help(workflow)
    wf_final <- workflows::workflow() %>%
      workflows::add_model(final_predictive_model_spec) %>%
      workflows::add_recipe(final_recipe)

    # Fit model
    final_predictive_model <-  parsnip::fit(wf_final, data = xy_all)

  }
)
  }

  final_predictive_model <- model_save_small_size(xy_all, final_recipe, results_split_parameter, model, nr_predictors)


  ##### NEW ENVIRONMENT END

  ##########  DESCRIBING THE MODEL  ##########
  ############################################

  cv_method_description <- paste("cv_method = ", deparse(cv_method))
  outside_folds_description <- paste("outside_folds = ", deparse(outside_folds))
  outside_strata_y_description <- paste("outside_strata_y = ", deparse(outside_strata_y))
  inside_folds_description <- paste("inside_folds = ", deparse(inside_folds))
  inside_strata_y_description <- paste("inside_strata_y = ", deparse(inside_strata_y))

  penalty_setting <- paste("penalty_setting = ", deparse(penalty))
  mixture_setting <- paste("mixture_setting = ", deparse(mixture))
  preprocess_PCA_setting <- paste("preprocess_PCA_setting = ", deparse(preprocess_PCA))
  first_n_predictors_setting <- paste("first_n_predictors_setting = ", deparse(first_n_predictors))

  # Saving the final mtry and min_n used for the final model.
  penalty_description <- paste("penalty in final model = ", deparse(statisticalMode(results_split_parameter$penalty)))
  penalty_fold_description <- paste("penalty in each fold = ", deparse(results_split_parameter$penalty))

  mixture_description <- paste("mixture in final model = ", deparse(statisticalMode(results_split_parameter$mixture)))
  mixture_fold_description <- paste("mixture in each fold = ", deparse(results_split_parameter$mixture))

  preprocess_PCA_description <- paste("preprocess_PCA in final model = ", deparse(statisticalMode(results_split_parameter$preprocess_PCA)))
  preprocess_PCA_fold_description <- paste("preprocess_PCA in each fold = ", deparse(results_split_parameter$preprocess_PCA))

  first_n_predictors_description <- paste("first_n_predictors in final model = ", deparse(statisticalMode(results_split_parameter$first_n_predictors)))
  first_n_predictors_fold_description <- paste("first_n_predictors in each fold = ", deparse(results_split_parameter$first_n_predictors))

  preprocess_step_center <- paste("preprocess_step_center_setting = ", deparse(preprocess_step_center))
  preprocess_step_scale <- paste("preprocess_step_scale_setting = ", deparse(preprocess_step_scale))

  impute_missing <- paste("impute_missing_setting = ", deparse(impute_missing))

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
    cv_method_description,
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
  # Remove object to minimize model size when saved to rds; use this to check sizes: sort( sapply(ls(),function(x){object.size(get(x))}))
  remove(x)
  remove(x1)
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

  final_results
}

