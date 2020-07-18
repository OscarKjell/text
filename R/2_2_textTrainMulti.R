
####### textTrainMultiText

# devtools::document()
#' Combine several word embeddings from several text variables and train to a numeric variable.
#' Takes several text variables; applies separate PCAs to each text's word embeddings separately;
#' concatenate the PCA components to then be trained to predict one outcome with ridge regression.
#' @param xlist List of word embeddings from textEmbed.
#' @param y Numeric variable to predict.
#' @param nrFolds_k Number of folds to use.
#' @param preProcessThresh Pre-processing threshold for amount of variance to retain (default 0.95).
#' @param strata_y Variable to stratify according (default y; can set to NULL).
#' @param methodCor Type of correlation used in evaluation (default "pearson"; can set to "spearman" or "kendall").
#' @param describe_model Text to describe your model.
#' @return A correlation between predicted and observed values; as well as predicted values.
#' @description Concatenate word embeddings from several text variables to predict an outcome.
#' The word embeddings for each text variable go through separate PCAs, where the PCA components are
#' concatenated and used in a ridge regression.
#' @examples
#' wordembeddings <- wordembeddings4_10[1:4]
#' ratings_data <- Language_based_assessment_data_8_10$hilstotal
#' results <- textTrainMultiTexts(wordembeddings, ratings_data, nrFolds_k = 2)
#' @seealso see \code{\link{textTrainLists}} \code{\link{textDiff}}
#' @importFrom stats cor.test na.omit
#' @importFrom dplyr select starts_with filter bind_cols matches
#' @importFrom recipes recipe step_naomit step_center step_scale step_pca
#' @importFrom rsample vfold_cv
#' @importFrom parsnip linear_reg set_engine
#' @importFrom tune control_grid tune_grid select_best collect_predictions
#' @export

# load("/Users/oscar/Desktop/0 Studies/5 R statistical semantics package/text_data_examples/sq_data_tutorial4_100.rda")
# load("/Users/oscar/Desktop/0 Studies/5 R statistical semantics package/text_data_examples/wordembeddings4_100.rda")
# y <- sq_data_tutorial4_100$hilstotal
# xlist <- wordembeddings4_100[1:4]
# preProcessThresh = 0.95
# nrFolds_k = 10
# strata_y = "y"
# methodCor = "pearson"
# describe_model = "Describe the model further and share it with others"

textTrainMultiTexts <- function(xlist,
                                y,
                                preProcessThresh = 0.95,
                                nrFolds_k = 10,
                                strata_y = "y",
                                methodCor = "pearson",
                                describe_model = "Describe the model further and share it with others") {

  # Select all variables that starts with Dim in each dataframe of the list.
  xlist <- lapply(xlist, function(X) {
    X <- dplyr::select(X, dplyr::starts_with("Dim"))
  })

  set.seed(2020)
  Nword_variables <- length(xlist)
  # Give each column specific names with indexes so that they can be handled separately in the PCAs
  for (i in 1:Nword_variables) {
    colnames(xlist[[i]]) <- paste("V_text", i, ".", names(xlist[i]), colnames(xlist[[i]]), sep = "")
  }

  # Make vector with each index so that we can allocate them separately for the PCAs
  variable_index_vec <- list()
  for (i in 1:Nword_variables) {
    variable_index_vec[i] <- paste("V_text", i, sep = "")
  }

  # Make one df rather then list.
  df1 <- dplyr::bind_cols(xlist)

  # Get the name of the first variable; which is used to exclude NA (i.e., word embedding have NA in all columns)
  V1 <- colnames(df1)[1]

  # x1 <- dplyr::select(x, dplyr::starts_with("Dim"))
  df2 <- cbind(df1, y)
  df3 <- df2[complete.cases(df2), ]
  df3 <- as_tibble(df3)

  df3_recipe <-
    recipes::recipe(y ~ .,
      data = df3
    ) %>%
    # recipes::step_BoxCox(all_predictors()) %>%
    recipes::step_naomit(V1, skip = TRUE) %>%
    recipes::step_center(all_predictors()) %>%
    recipes::step_scale(all_predictors())

  # Adding a PCA in each loop; first selecting all variables starting with V_text1; and then V_text2 etc
  for (i in variable_index_vec) {
    df3_recipe <-
      df3_recipe %>%
      # !! slices the current name into the `matches()` function.
      # We use a custom prefix so there are no name collisions for the
      # results of each PCA step.
      step_pca(dplyr::matches(!!i), threshold = preProcessThresh, prefix = paste("PCA_", i, "_"))
  }

  # Cross-validation
  df3_cv_splits <- rsample::vfold_cv(df3, v = nrFolds_k, repeats = 1, strata = strata_y)

  # Model
  df3_model <-
    parsnip::linear_reg(penalty = tune(), mixture = tune()) %>%
    # parsnip::logistic_reg(mode = "classification", penalty = tune(), mixture = tune()) %>%
    parsnip::set_engine("glmnet")

  # Tuning; parameters; grid for ridge regression.
  # https://rstudio-conf-2020.github.io/applied-ml/Part_5.html#26
  df3_glmn_grid <- base::expand.grid(
    penalty = 10^seq(-3, -1, length = 20),
    mixture = (0:5) / 5
  )

  ctrl <- control_grid(save_pred = TRUE, verbose = TRUE)

  # Tune grid; df3_glmn_tune$.predictions
  df3_glmn_tune <- tune_grid(
    df3_recipe,
    model = df3_model,
    resamples = df3_cv_splits,
    grid = df3_glmn_grid,
    control = ctrl
  )

  # Select the best penelty and mixture based on rmse; df3_glmn_tune$.metrics; accuracy
  best_glmn <- select_best(df3_glmn_tune, metric = "rmse", maximize = FALSE)

  # Get predictions and observed (https://rstudio-conf-2020.github.io/applied-ml/Part_5.html#32)
  predictions <- tune::collect_predictions(df3_glmn_tune) %>%
    filter(penalty == best_glmn$penalty, mixture == best_glmn$mixture)

  # For linear regression: Evaluating the predictions using correlation
  results <- cor.test(predictions$y, predictions$.pred, method = methodCor)

  # For logistic regression: chisq.test
  # results <- stats::chisq.test(predictions$y, predictions$.pred_class)

  # Describe model; adding user's-description + the name of the x and y
  # describe_model_detail <- c(describe_model, paste(names(x)), paste(names(y)))
  describe_model_detail <- c(deparse(substitute(x)), deparse(substitute(y)), describe_model)

  output <- list(predictions, describe_model_detail, results)
  names(output) <- c("predictions", "model description", "results")
  output
}
######################
########### End of textTrain function
######################
# solmini_1 <- read_csv("/Users/oscar/Desktop/0 Studies/13 OnlineMini/combinedSOL_SM.csv")
# solmini_sd300_tk_mean1 <- read_rds("/Users/oscar/Desktop/0 Studies/5 R statistical semantics package/spaces/spaceDomain_solminidata_solmini_sd300_tk_mean.rds")
# xlist <- solmini_sd300_tk_mean1[1:2]
# y <- solmini_1$phq_tot

# testMulti <- textTrainMultiTexts(xlist, y)
