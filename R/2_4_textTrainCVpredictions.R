

################
#### Training Cross-validated predicted data
################
# x = Tibble with predictor variables (e.g., cross-validated predictions from textTrainLists, other rating scales etc)
# y = the outcome variable

# 1. Fold split 1:  Split data into c_one_1 - c_one_10, and leave out c_one_1.
# 2. Fold split 2. Split c_one_2 - c_one_10, to c_two_1 - c_two_10
# 3. Use c_two_1 - c_two_10 in cross-validation to tune PCA-components and Ridge parameters;
#      where this tuned model is applied on c_one_1 to predict y (i.e., get y_pred_1).
# 4. Repeat step 1-3 to predict y_pred_1 - y_pred_10.
# 5 And now we can train y_preds to predict y using same folds as before, without being afraid of data leakage.



#  devtools::document()
#' textTrainCVpredictions Function training text to numeric using 2 cross-validations using linear regression.
#' @param x wordembeddings from textImport.
#' @param y numeric variables to predict.
#' @param nrFolds_k Number of folds to use.
#' @param nrFolds_k_out Number of outer folds (only for textTrainCVpredictions textTrainCVpredictionsRF).
#' @param preProcessPCAthresh Preprocessing threshold.
#' @param strata_y variables to stratify according; default y, can set to NULL
#' @return predictions
#' @importFrom dplyr select_if select %>% arrange bind_rows bind_cols
#' @importFrom tibble as_tibble
#' @importFrom rsample vfold_cv
#' @importFrom stringi stri_encode
#' @importFrom recipes recipe step_naomit step_center step_scale step_pca all_predictors update_role
#' @importFrom parsnip linear_reg set_engine fit
#' @importFrom tune tune control_grid tune_grid select_best collect_predictions finalize_workflow
#' @importFrom workflows workflow add_recipe add_model
#' @importFrom stats predict
#' @importFrom yardstick metric_set rmse
#' @noRd
########### help(finalize_workflow)
textTrainCVpredictions <- function(x, y, nrFolds_k = 10, nrFolds_k_out=10, preProcessPCAthresh = 0.95, strata_y = "y"){
  x1 <- dplyr::select(x, dplyr::starts_with("V"))
  df2 <- cbind(x1, y)
  nrow(df2)

  df3 <- df2 #[complete.cases(df2),]
  #nrow(df3)

  #df3f <- cbind(x, y)
  #nrow(df3f)
  #df3f <- df3f[ complete.cases(df3f),]
  df3f <- tibble::as_tibble(df3)
  df3f$id <- c(1:nrow(df3f))
  # Take out 10 % to be used to get prediction used for the final step
  set.seed(42)

  # Outer Folds, where 10% is held out for predictions to be used for the final training of predicted values
  df3f_cv_folds <- rsample::vfold_cv(df3f, v = nrFolds_k_out, repeats = 1, strata = strata_y) # , ... ,  breaks = 4
  cv_predictions_list <- list()
  # Loop to achieve cross-validated scores that was totally left out
  for (i in 1:nrFolds_k_out) { # i=1

    # Get model data help(analysis)
    model_data <- df3f_cv_folds$splits[[i]] %>% rsample::analysis()
    model_data
    # Get data to predict from
    holdout_data <- df3f_cv_folds$splits[[i]] %>% rsample::assessment()
    holdout_data

    # Inner fold
    # set.seed(42)
    model_data_inner_folds <- rsample::vfold_cv(model_data, v = nrFolds_k, repeats = 1, strata = y)

    # Recipes
    df3f_recipe <-
      recipes::recipe(y ~ .,
                      data = model_data) %>%
      recipes::update_role(id, new_role = "id variable") %>%
      recipes::update_role(-id, new_role = "predictor") %>%
      recipes::update_role(y, new_role = "outcome") %>%
      #  step_BoxCox(all_predictors()) %>%
      step_naomit(V1, skip = TRUE) %>%
      recipes::step_center(all_predictors()) %>%
      recipes::step_scale(all_predictors()) %>%
      recipes::step_pca(all_predictors(), threshold = preProcessPCAthresh)


    # Model
    ## Define a regularized regression and explicitly leave the tuning parameters
    ## empty for later tuning.
    df3f_model <-
      parsnip::linear_reg(penalty = tune::tune(), mixture = tune::tune()) %>%
      #parsnip::logistic_reg(mode = "classification", penalty = tune(), mixture = tune()) %>%
      parsnip::set_engine("glmnet")

    # Tuning; parameters; grid for ridge regression. https://rstudio-conf-2020.github.io/applied-ml/Part_5.html#26
    df3f_glmn_grid <- base::expand.grid(
      penalty = 10 ^ seq(-3, -1, length = 20),
      mixture = (0:5) / 5
    )

    ctrl <- tune::control_grid(save_pred = TRUE)

    ## Construct a workflow that combines the recipe and the model
    df3_wflow <-
      workflows::workflow() %>%
      workflows::add_recipe(df3f_recipe) %>%
      workflows::add_model(df3f_model)

    # Find best tuned model
    various_models <-
      df3_wflow %>%
      tune::tune_grid(resamples = model_data_inner_folds,
                      grid = df3_glmn_grid,
                      metrics = yardstick::metric_set(yardstick::rmse))

    # Select best parameters (i.e., best penelty and mixture)
    best_params <-
      various_models %>%
      tune::select_best(metric = "rmse", maximize = FALSE)

    # Refit using the entire training data
    tuned_model <-
      df3_wflow %>%
      tune::finalize_workflow(best_params) %>%
      parsnip::fit(data = model_data)

    # Predict on assessment data that was hold out: https://www.r-bloggers.com/the-simplest-tidy-machine-learning-workflow/

    cv_predictions <- stats::predict(tuned_model, new_data = holdout_data) %>%
      bind_cols(holdout_data, .) %>%
      select(y, .pred, id)

    cv_predictions_list[i] <- list(cv_predictions)
    cv_predictions_list
  }

  cv_predictions_tibble <- bind_rows(cv_predictions_list)
  cv_predictions_tibble <- cv_predictions_tibble %>% dplyr::arrange(id)
  cv_predictions_tibble
}
######################
########### END textTrainCVpredictions
######################
# testingCV <- textTrainCVpredictions(x, y)
# cor.test(testingCV$y, testingCV$.pred)

# x <- x$harmonywords
# y <- y$catgender1
# y[1] <- 3

#  devtools::document()
#' textTrainCVpredictionsRF Function training text to categories using 2 cross-validations using random forest.
#' @param x wordembeddings from textImport.
#' @param y categorical variable to predict.
#' @param nrFolds_k number of folds to use.
#' @param nrFolds_k_out number of outer folds (only for textTrainCVpredictions textTrainCVpredictionsRF).
#' @param strata_y variables to stratify according; default y, can set to NULL
#' @param trees number of trees
#' @return predictions
#' @importFrom dplyr select %>% arrange bind_rows bind_cols
#' @importFrom tibble as_tibble
#' @importFrom rsample vfold_cv
#' @importFrom recipes recipe step_naomit step_center step_scale step_pca all_predictors update_role
#' @importFrom parsnip linear_reg set_engine fit
#' @importFrom tune tune control_grid tune_grid select_best collect_predictions finalize_workflow
#' @importFrom workflows workflow add_recipe add_model
#' @noRd
########### Function training text to numeric using 2 cross-validations using Random Forrest and categorical outcomes.
textTrainCVpredictionsRF <- function(x, y, trees = 500, nrFolds_k_out = 10, nrFolds_k = 10, strata_y = "y"){
  x1 <- dplyr::select(x, dplyr::starts_with("V"))
  y <- as.factor(y)
  df2 <- cbind(x1, y)
  nrow(df2)
  df2$id <- c(1:nrow(df2))

  df3 <- df2[complete.cases(df2),]
  df3f <- tibble::as_tibble(df3)

  # Take out 10 % to be used to get prediction used for the final step
  set.seed(2020)

  # Outer Folds, where 10% is held out for predictions to be used for the final training of predicted values
  df3f_cv_folds <- rsample::vfold_cv(df3f, v = nrFolds_k_out, repeats = 1, strata = strata_y) # , ... ,  breaks = 4
  cv_predictions_list <- list()
  # Loop to achieve cross-validated scores that was totally left out
  for (i in 1:nrFolds_k_out) { # i=1

    # Get model data
    model_data <- df3f_cv_folds$splits[[i]] %>% analysis()
    # Get data to predict from
    holdout_data <- df3f_cv_folds$splits[[i]] %>% assessment()

    # Inner fold
    set.seed(2020)
    model_data_inner_folds <- rsample::vfold_cv(model_data, v = nrFolds_k, repeats = 1, strata = strata_y)

    # Recipes
    df3_recipe <-
      recipes::recipe(y ~ .,
                      data = model_data) %>%
      recipes::update_role(id, new_role = "id variable") %>%
      recipes::update_role(-id, new_role = "predictor") %>%
      recipes::update_role(y, new_role = "outcome") %>%
      # step_BoxCox(all_predictors()) %>%
      #step_naomit(V1, skip = TRUE) %>%
      recipes::step_center(all_predictors()) %>%
      recipes::step_scale(all_predictors()) #%>%
      #recipes::step_pca(all_predictors(), threshold = .95)

    # Model: random forrest
    df3_model <- rand_forest(trees = trees, mode = "classification") %>%
      set_engine("ranger")
      #set_engine("randomForest")

    ## Construct a workflow that combines the recipe and the model
    df3_workflow <-
      workflows::workflow() %>%
      workflows::add_model(df3_model) %>%
      workflows::add_recipe(df3_recipe)

    # Applying add data
    tuned_model <- fit(df3_workflow, data = model_data)

    cv_predictions <- predict(tuned_model, new_data = holdout_data, type = "prob") %>%
      bind_cols(holdout_data, .) %>%
      select(-starts_with("V"))

    pred_class <- predict(tuned_model, new_data = holdout_data) %>%
     bind_cols(cv_predictions)

    cv_predictions_list[i] <- list(pred_class)
    cv_predictions_list
  }

  cv_predictions_tibble <- bind_rows(cv_predictions_list)
  cv_predictions_tibble <- cv_predictions_tibble %>% dplyr::arrange(id)

  # Combine with original in order to impute the NAs help(merge)
  df2_bind <- tibble(df2$y, df2$id)
  colnames(df2_bind) <- c("y", "id")
  cv_predictions_tibble_na <- merge(df2_bind, cv_predictions_tibble, by = "id", all=TRUE)
  cv_predictions_tibble_na
}
######################
########### END textTrainCVpredictionsRF
######################
# textTrainCVpredictionsRF(x, y)

#  devtools::document()
#' Function to train (cross-validated predicted) values with other values (/rating scales)
#' @param x tibble with predicted values.
#' @param y variable to predict.
#' @param nrFolds_k number of folds to use.
#' @param strata_y variables to stratify according; default y, can set to NULL
#' @return result and predictions
#' @importFrom dplyr select %>% arrange bind_rows bind_cols
#' @importFrom stats complete.cases
#' @importFrom tibble as_tibble
#' @importFrom rsample vfold_cv
#' @importFrom recipes recipe step_naomit step_center step_scale step_pca all_predictors update_role
#' @importFrom parsnip linear_reg set_engine fit logistic_reg
#' @importFrom tune tune control_grid tune_grid select_best collect_predictions finalize_workflow
#' @noRd
###########
trainCVpredictions <- function(x, y, nrFolds_k = 10, strata_y = "y") { #, pca_threshold = .95

  df3 <- cbind(x, y)
  df3 <- tibble::as_tibble(df3)

  # Recipe: Preprocessing
  df3_recipe <-
    recipes::recipe(y ~ .,
                    data = df3) %>%
    recipes::step_BoxCox(all_predictors()) %>%
    recipes::step_knnimpute(all_predictors(), neighbors=round(sqrt(nrow(df3[stats::complete.cases(df3),])))) %>%
    recipes::step_naomit(all_predictors(), skip = TRUE) %>%
    recipes::step_center(all_predictors()) %>%
    recipes::step_scale(all_predictors()) #%>%
  #recipes::step_pca(all_predictors(), threshold = pca_threshold) #%>%

  # Cross-validation
  set.seed(42)
  df3_cv_splits <- rsample::vfold_cv(df3, v = nrFolds_k, repeats = 1, strata = strata_y) # , ... ,  breaks = 4

  # Model
  df3_model <-
    #parsnip::linear_reg(penalty = tune(), mixture = tune()) %>% # tune() uses the grid
    parsnip::logistic_reg(mode = "classification", penalty = tune(), mixture = tune()) %>%
    parsnip::set_engine("glmnet")

  # Tuning; parameters; grid for ridge regression.
  df3_glmn_grid <- base::expand.grid(
    penalty = 10 ^ seq(-3, -1, length = 20),
    mixture = (0:5) / 5
  )

  ctrl <- control_grid(save_pred = TRUE)

  # tune_grid
  df3_glmn_tune <- tune_grid(
    df3_recipe,
    model = df3_model,
    resamples = df3_cv_splits,
    grid = df3_glmn_grid,
    control = ctrl
  )

  # Accuracy could also be roc_auc, or metric = "rmse": NEED TO PUT IN IF STATEMENT ABOVE help(select_best)
  best_glmn <-
    select_best(df3_glmn_tune, metric = "accuracy", maximize = TRUE)

  # Get predictions and observed (https://rstudio-conf-2020.github.io/applied-ml/Part_5.html#32)
  df3_predictions <- tune::collect_predictions(df3_glmn_tune) %>%
    filter(penalty == best_glmn$penalty, mixture == best_glmn$mixture)

  df3_predictions <- df3_predictions %>% dplyr::arrange(.row)

  # Evaluating the predictions using correlation
  # result <- cor.test(df3_predictions$y, df3_predictions$.pred)

  #For categorical NEED TO PUT IN IF STATEMENT ABOVE
  result <- chisq.test(df3_predictions$y, df3_predictions$.pred_class)

  output <- list(df3_predictions, result)
  names(output) <- c("predictions", "results")
  output
}
######################
########### END trainCVpredictions
######################
# outputCVpred <- trainCVpredictions(x, y)




