

# devtools::document()
#' Train word embeddings to a numeric variable.
#'
#' @param x Word embeddings from textEmbed (or textLayerAggregation).
#' @param y Numeric variable to predict.
#' @param nrFolds_k Number of folds to use.
#' @param preProcessPCAthresh Pre-processing threshold for amount of variance to retain (default 0.95).
#' @param strata_y Variable to stratify according (default y; can set to NULL).
#' @param methodCor Type of correlation used in evaluation (default "pearson";
#' can set to "spearman" or "kendall").
#' @param model_description Text to describe your model (optional; good when sharing the model with others).
#' @return A correlation between predicted and observed values; as well as a tibble of predicted values.
#' @examples
#' wordembeddings <- wordembeddings4_10
#' ratings_data <- Language_based_assessment_data_8_10
#' wordembeddings <- textTrain(wordembeddings$harmonytext, ratings_data$hilstotal,
#'   nrFolds_k = 2, strata_y = NULL
#' )
#' @seealso see \code{\link{textLayerAggregation}} \code{\link{textTrainLists}}
#' \code{\link{textTrainMultiTexts}} \code{\link{textTrainRandomForest}} \code{\link{textDiff}}
#' @importFrom stats cor.test na.omit
#' @importFrom dplyr select starts_with filter
#' @importFrom recipes recipe step_naomit step_center step_scale step_pca all_predictors
#' @importFrom rsample vfold_cv
#' @importFrom parsnip linear_reg set_engine
#' @importFrom tune tune control_grid tune_grid select_best collect_predictions
#' @export
textTrain <- function(x,
                      y,
                      nrFolds_k = 10,
                      preProcessPCAthresh = 0.95,
                      strata_y = "y",
                      methodCor = "pearson",
                      model_description = "Consider writing a description here") {
  x1 <- dplyr::select(x, dplyr::starts_with("Dim"))
  df3 <- cbind(x1, y)

  # Recipe: Pre-processing with pca, see options: ls("package:recipes", pattern = "^step_")
  df3_recipe <-
    recipes::recipe(y ~ .,
      data = df3
    ) %>%
    # recipes::step_BoxCox(all_predictors()) %>%
    recipes::step_naomit(Dim1, skip = TRUE) %>%
    recipes::step_center(all_predictors()) %>%
    recipes::step_scale(all_predictors()) %>%
    recipes::step_pca(all_predictors(), threshold = preProcessPCAthresh) # %>% num_comp = tune()

  # Cross-validation
  set.seed(2020)
  df3_cv_splits <- rsample::vfold_cv(df3, v = nrFolds_k, strata = strata_y) # , ... ,  breaks = 4

  # Model
  df3_model <-
    parsnip::linear_reg(penalty = tune(), mixture = tune()) %>% # tune() uses the grid
    parsnip::set_engine("glmnet")

  # Tuning; parameters; grid for ridge regression. https://rstudio-conf-2020.github.io/applied-ml/Part_5.html#26
  df3_glmn_grid <- base::expand.grid(
    penalty = 10^seq(-3, -1, length = 20),
    mixture = (0:5) / 5
  )

  ctrl <- tune::control_grid(save_pred = TRUE)

  # Tune_grid help(tune_grid)
  df3_glmn_tune <- tune::tune_grid(
    object = df3_model,
    df3_recipe,
    resamples = df3_cv_splits,
    grid = df3_glmn_grid,
    control = ctrl
  )

  # Select the best penalty and mixture based on rmse
  best_glmn <- tune::select_best(df3_glmn_tune, metric = "rmse") # , maximize = TRUE

  # Get predictions and observed (https://rstudio-conf-2020.github.io/applied-ml/Part_5.html#32)
  df3_predictions <- tune::collect_predictions(df3_glmn_tune) %>%
    dplyr::filter(penalty == best_glmn$penalty, mixture == best_glmn$mixture)

  # Evaluating the predictions using correlation
  correlation <- stats::cor.test(df3_predictions$y, df3_predictions$.pred, method = methodCor)

  # Describe model; adding user's-description + the name of the x and y
  # model_description_detail <- c(model_description, paste(names(x)), paste(names(y)))
  model_description_detail <- c(deparse(substitute(x)), deparse(substitute(y)), model_description)

  output <- list(df3_predictions, model_description_detail, correlation)
  names(output) <- c("predictions", "model description", "correlation")
  output
}
######################
########### End of textTrain function
######################
# wordembeddings <- wordembeddings4_10
# ratings_data <- Language_based_assessment_data_8_10
# wordembeddings <- textTrain(wordembeddings$harmonytext, ratings_data$hilstotal, nrFolds_k = 2)


# x <- solmini_text_11
# y <- solmini_n4[4]
# colnames(y) <- c("hils_tot")
#
# method = "textTrainCVpredictions"
#
#  x <- list(solmini_sd300_tk_mean1$dep_all[1:100,], solmini_sd300_tk_mean1$movement[1:100,]) # , solmini_sd300_tk_mean1$sleep[1:150,]
#  y <- tibble(solmini$phq_tot[1:100], solmini$gad_tot[1:100]) #, solmini$hils_tot[1:150]
#
#  y <- tibble(as.factor(solmini$minidep_diagnose[1:100]), as.factor(solmini$miniGAD_diagnose[1:100]))
#
#  names(x) <- c("dep_all", "movement") #, "sleep"
#  names(y) <- c("phq", "gad") # , "hils"
#  names(y) <- c("minidep", "minigad")

####### textTrainList analyse list of text variables and tibble of numeric variables
# x = list of word-embeddings
# y = tibble with variables to be predicted


# devtools::document()
#' Individually trains word embeddings from several text variables to several numeric variables.
#' @param x List of lists comprising several word embeddings from textEmbed.
#' (NB need to remove any word embeddings from the decontextualized single words).
#' @param y Tibble with numeric variables to predict.
#' @param trainMethod Method to train word embeddings (default "regression"; see also "randomForest").
#' @param nrFolds_k Number of folds to use.
#' @param nrFolds_k_out Number of outer folds (only for textTrainCVpredictions textTrainCVpredictionsRF,
#' which is not yet fully implemented).
#' @param preProcessPCAthresh Pre-processing threshold for amount of variance to retain (default 0.95).
#' @param strata_y Variable to stratify according (default y; can set to NULL).
#' @param methodCor Type of correlation used in evaluation (default "pearson"; can set to "spearman" or "kendall").
#' @param trees Number of trees used in the random forest.
#' @param ... Arguments for the textTrain function.
#' @return Correlations between predicted and observed values.
#' @examples
#' wordembeddings <- wordembeddings4_10[1:2]
#' ratings_data <- Language_based_assessment_data_8_10[5:6]
#' wordembeddings <- textTrainLists(wordembeddings, ratings_data, nrFolds_k = 2)
#' @seealso see \code{\link{textTrain}}
#' @importFrom stats cor.test
#' @importFrom tibble as_tibble
#' @importFrom magrittr %>%
#' @importFrom dplyr arrange
#' @importFrom data.table %like%
#' @export
textTrainLists <- function(x,
                           y,
                           trainMethod = "regression",
                           nrFolds_k = 10,
                           preProcessPCAthresh = 0.95,
                           strata_y = "y",
                           methodCor = "pearson",
                           trees = 500,
                           nrFolds_k_out = 10, ...) {
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
    output <- mapply(textTrain, x, y1,
      SIMPLIFY = FALSE,
      MoreArgs = list(
        nrFolds_k = nrFolds_k, preProcessPCAthresh = preProcessPCAthresh,
        strata_y = strata_y, methodCor = methodCor, ...
      )
    ) # , preProcessPCAthresh=preProcessPCAthresh, strata_y = strata_y, methodCor = methodCor, MoreArgs = list(nrFolds_k = nrFolds_k, methodTrain = methodTrain, preProcessTrain = preProcessTrain, preProcessThresh = preProcessThresh, methodCor = methodCor, ...),

    output_t <- t(as.data.frame(lapply(output, function(output) unlist(output$correlation)[[1]][c(1)])))
    output_df <- t(as.data.frame(lapply(output, function(output) unlist(output$correlation)[[2]][c(1)])))
    output_p <- t(as.data.frame(lapply(output, function(output) unlist(output$correlation)[[3]][c(1)])))
    output_r <- t(as.data.frame(lapply(output, function(output) unlist(output$correlation)[[4]][c(1)])))
    output_a <- t(as.data.frame(lapply(output, function(output) unlist(output$correlation)[[6]][c(1)])))

    # Add Outcomes and Descriptions together; name the columns; and remove the row names.
    output_ordered_named <- data.frame(cbind(descriptions, output_r, output_df, output_p, output_t, output_a))
    colnames(output_ordered_named) <- c("descriptions", "correlation", "df", "p_value", "t_statistics", "alternative")
    rownames(output_ordered_named) <- NULL

    output_predscore <- as.data.frame(lapply(output, function(output) unlist(output$predictions)))
    output_predscore_reg <- output_predscore[rownames(output_predscore) %like% ".pred", ] # like comes from data.table
    colnames(output_predscore_reg) <- c(paste(descriptions, "_pred", sep = ""))
    results <- list(output_predscore_reg, output_ordered_named)
    names(results) <- c("predscores", "results")
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
    names(results) <- c("predscores", "results")
    results
  } else if (trainMethod == "textTrainCVpredictions") {
    output <- mapply(textTrainCVpredictions, x, y1,
      SIMPLIFY = FALSE,
      MoreArgs = list(nrFolds_k = nrFolds_k, nrFolds_k_out = nrFolds_k_out, preProcessPCAthresh = preProcessPCAthresh, strata_y = strata_y)
    )

    output_predscore <- as.data.frame(lapply(output, function(output) unlist(output)))
    output_predscore_reg <- output_predscore[rownames(output_predscore) %like% ".pred", ] # like comes from data.table
    colnames(output_predscore_reg) <- c(paste(descriptions, "_pred", sep = ""))
    results <- list(output_predscore_reg)
    names(results) <- c("CVpredscores")
    results
  } else if (trainMethod == "textTrainCVpredictionsRF") {
    output <- mapply(textTrainCVpredictionsRF, x, y1,
      SIMPLIFY = FALSE,
      MoreArgs = list(trees = trees, nrFolds_k_out = nrFolds_k_out, nrFolds_k = nrFolds_k, strata_y = strata_y)
    )

    # Get and sort the prediction scores
    names(output) <- descriptions
    output_predscore <- do.call(cbind, output) %>%
      tibble::as_tibble() %>%
      dplyr::arrange()

    results <- list(output_predscore)
    names(results) <- c("CVpredscores")
    results
  }
}
######################
###########  End of textTrainList function
######################
# wordembeddings <- wordembeddings1_100[1]
# ratings_data <- sq_data_tutorial4_100[4]
# y <- sq_data_tutorial4_100$gender
#
#
# wordembeddings <- wordembeddings4_10[1:2]
# ratings_data <- Language_based_assessment_data_8_10[7]
# wordembeddings <- textTrainLists(wordembeddings, ratings_data, trainMethod = "regression", nrFolds_k = 2) # randomForest , nrFolds_k = 2)
#
# # Categorical
# catgender1 <- sq_data_tutorial4_100$gender
# catgender2 <- sq_data_tutorial4_100$gender
# catgender3 <- sq_data_tutorial4_100$gender
# categ <- tibble(catgender1, catgender2, catgender3)
# x <- wordembeddings
# y <- categ
# wordembeddings <- textTrainLists(x, y, trainMethod = "randomForest", trees = 5)
# wordembeddings


# solmini_1 <- read_csv("/Users/oscar/Desktop/0 Studies/13 OnlineMini/combinedSOL_SM.csv")
# solmini_sd300_tk_mean1 <- read_rds("/Users/oscar/Desktop/0 Studies/5 R statistical semantics package/spaces/spaceDomain_solminidata_solmini_sd300_tk_mean.rds")
# x <- solmini_sd300_tk_mean1[1:2]
# y <- as_tibble(solmini_1$phq_tot)
# colnames(y) <- c("phq")
# train_lists_test_reg <- textTrainLists(x, y, trainMethod = "regression", nrFolds_k = 3) #, nrFolds_k = 2
# train_lists_test_reg

# y <- as_tibble(solmini_1$minidiagnose_cat)
# colnames(y) <- c("diagnose")
# train_lists_test <- textTrainLists(x, y, trainMethod = "randomForest", trees = 5, nrFolds_k = 2, strata_y = "y")
# train_lists_test




# train_lists_test <- textTrainLists(x, y, trainMethod = "textTrainCVpredictions", nrFolds_k_out=2)


# train_lists_test <- textTrainLists(x, y, trainMethod = "textTrainCVpredictionsRF", trees=50)
# train_lists_test
# colnames(train_lists_test$CVpredscores)
