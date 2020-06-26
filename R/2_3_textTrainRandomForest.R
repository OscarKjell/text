


################
#### Random Forest: Training Cross-validated data
################

# x = solmini_sd300_tk_mean$movement[1:40,1:5]
# y = solmini$minidep_diagnose[1:40]
# y = solmini$minidiagnose_cat[1:40]
# y
#df3_data <- as_tibble(df3)
#colnames(df3_data) <- c(paste("Dim", 1:(ncol(df3_data)-1), sep=""), "y")

# devtools::document()
#' textTrainRandomForest trains word embeddings to a categorical variable using random forrest.
#'
#' @param x Wordembeddings from textImport.
#' @param y The categorical variable to predict.
#' @param nrFolds_k Number of folds to use (default 10).
#' @param trees Number of trees to use default 500).
#' @param strata_y variables to stratify according; default y, can set to NULL
#' @param describe_model Input text to describe your model.
#' @return A correlation between predicted and observed values; as well as predicted values.
#' @examples
#' wordembeddings <- wordembeddings4_10
#' ratings_data <- sq_data_tutorial4_10
#' wordembeddings <- textTrain(wordembeddings$harmonytext, ratings_data$hilstotal, nrFolds_k = 2)
#' @seealso see \code{\link{textTrainLists}} \code{\link{textDiff}}
#' @importFrom stats cor.test na.omit chisq.test
#' @importFrom dplyr select starts_with filter arrange rename
#' @importFrom recipes recipe step_naomit step_center step_scale step_pca
#' @importFrom rsample vfold_cv
#' @importFrom parsnip linear_reg set_engine rand_forest
#' @importFrom tune control_grid tune_grid select_best collect_predictions control_resamples
#' @importFrom workflows workflow add_model add_recipe
#' @export
textTrainRandomForest <- function(x, y, trees=500, nrFolds_k = 10, strata_y = "y", describe_model = "Describe the model further and share it with others"){

  set.seed(2020)
  y <- as.factor(y)
  x1 <- dplyr::select(x, dplyr::starts_with("Dim"))
  df2 <- cbind(x1, y)
  df2$id1 <- c(1:nrow(df2))
  df2_formergingNA <- tibble(df2$id1, df2$y)
  colnames(df2_formergingNA) <- c("id1", "y")
  df3_data <- as_tibble(df2[complete.cases(df2),])
  nrow(df3_data)

  # Recipe: Preprocessing by removing na and normalising variables help(step_naomit)
  df3_recipe <-
    recipes::recipe(y ~ .,
                  data = df3_data) %>%
    recipes::update_role(id1, new_role = "id variable") %>%
    recipes::update_role(-id1, new_role = "predictor") %>%
    recipes::update_role(y, new_role = "outcome") %>%
    recipes::step_naomit(V1, skip = FALSE) %>% # Does this not work here?
    recipes::step_center(all_predictors()) %>%
    recipes::step_scale(all_predictors()) %>%
    recipes::step_BoxCox(all_predictors())

  # Cross-validation
  df3_cv_splits <- rsample::vfold_cv(df3_data, v = nrFolds_k, repeats = 1, strata = strata_y) # , ... ,  breaks = 4

  # Model: random forrest help(rand_forest)
  df3_model <- parsnip::rand_forest(trees = trees, mode = "classification") %>%
    #set_engine("ranger")
    set_engine("randomForest")

  # help("workflow")
  df3_workflow <-
    workflows::workflow() %>%
    workflows::add_model(df3_model) %>%
    workflows::add_recipe(df3_recipe)

  # Applying the resampling;
  df3_easy_eval <- tune::fit_resamples(df3_workflow, resamples = df3_cv_splits,  control = tune::control_resamples(save_pred = TRUE))
  # See warning above!

  # Get prediction output
  df3_predictions <- tune::collect_predictions(df3_easy_eval)
  # Sort predictions and merge with the dataframe including NA (this is good for the merging of output in textTrainList)
  df3_predictions1 <- df3_predictions %>%
    dplyr::rename(y2=y) %>%
    dplyr::rename(id2=id) %>%
    dplyr::arrange(.row) %>%
    cbind(df3_data)

  df3_data_preds <- merge(df2_formergingNA, df3_predictions1, by.x = "id1", by.y = "id1", all = TRUE)
  #Chi2 test
  chi2 <- stats::chisq.test(df3_data_preds$.pred_class, df3_data_preds$y.x)
  #Getting all predictions
  outcome_variables <- c("id1", paste(".pred_", levels(y), sep=""), ".pred_class", "y.y")
  df3_predictions_save <- df3_data_preds %>%
    select(outcome_variables) %>%
    rename(y=y.y)

  # Describe model; adding user's-description + the name of the x and y
  # describe_model_detail <- c(describe_model, paste(names(x)), paste(names(y)))
  describe_model_detail <- c(deparse(substitute(x)), deparse(substitute(y)), describe_model)

  output_random_forest <- list(df3_predictions_save, describe_model_detail, chi2)
  names(output_random_forest) <- c("predictions", "model description", "results")
  output_random_forest
}

######################
########### END textTrainRandomForest
######################
# library(tidymodels)
# x <- wordembeddings1_100$harmonytexts
# y <- sq_data_tutorial4_100$gender
# testing_outcome<- textTrainRandomForest(x, sq_data_tutorial4_100$gender, trees=50, nrFolds_k = 10, strata_y = "y")
# testing_outcome



