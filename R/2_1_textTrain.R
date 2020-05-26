
#library(tidymodels)
# Basic example: https://rviews.rstudio.com/2019/06/19/a-gentle-intro-to-tidymodels/
# recipies: https://tidymodels.github.io/recipes/articles/Simple_Example.html
# rsample: https://tidymodels.github.io/rsample/articles/Basics.html
# parsnip: https://tidymodels.github.io/parsnip/articles/parsnip_Intro.html
# yardstick: https://tidymodels.github.io/yardstick/reference/index.html
# Two-day workshop: https://github.com/tidymodels/aml-training/tree/master/two%20day
# .rs.restartR()

# https://rstudio-conf-2020.github.io/applied-ml/Part_5.html#29
#library(doParallel)
#help(doParallel)
#cl <- makeCluster(6)
#registerDoParallel(cl)
#stopCluster(cl)

# library(text)
# library(tidymodels)
# library(rlang)
# library(magrittr)
# library(recipes)
# library(workflows)
# library(parsnip)
# library(tune)
# library(data.table)
# library(stats)
# library(tidyselect)
# library(tidyverse)

# test data
# x = solmini_sd300_tk_mean$movement[1:50,]
# y = solmini$phq_tot[1:50]



# textTrain using Tidymodels

# devtools::document()
#' textTrain trains word embeddings to a numeric variable.
#'
#' @param x Wordembeddings from textImport.
#' @param y The numeric variable to predict.
#' @param nrFolds_k Number of folds to use.
#' @param preProcessPCAthresh Preprocessing threshold.
#' @param strata_y variables to stratify according; default y, can set to NULL
#' @param methodCor Type of correlation used in evaluation; default pearson (see also "spearman", "kendall").
#' @param describe_model Input text to describe your model.
#' @return A correlation between predicted and observed values; as well as predicted values.
#' @examples
#' wordembeddings <- wordembeddings4_10
#' ratings_data <- sq_data_tutorial4_10
#' wordembeddings <- textTrain(wordembeddings$harmonytext, ratings_data$hilstotal,
#' nrFolds_k = 2, strata_y = NULL)
#' @seealso see \code{\link{textTrainLists}} \code{\link{textDiff}}
#' @importFrom stats cor.test na.omit
#' @importFrom dplyr select starts_with filter
#' @importFrom recipes recipe step_naomit step_center step_scale step_pca all_predictors
#' @importFrom rsample vfold_cv
#' @importFrom parsnip linear_reg set_engine
#' @importFrom tune tune control_grid tune_grid select_best collect_predictions
#' @export
textTrain <- function(x, y, nrFolds_k = 10, preProcessPCAthresh = 0.95, strata_y = "y", methodCor = "pearson", describe_model = "Describe the model further and share it with others"){
  x1 <- dplyr::select(x, dplyr::starts_with("V"))
  df2 <- cbind(x1, y)
  df3 <- df2 #[complete.cases(df2),]

  # Recipe: Preprocessing with pca, see options: ls("package:recipes", pattern = "^step_")
  df3_recipe <-
    recipes::recipe(y ~ .,
                    data = df3) %>%
    #  recipes::step_BoxCox(all_predictors()) %>%
    recipes::step_naomit(V1, skip = TRUE) %>%
    recipes::step_center(all_predictors()) %>%
    recipes::step_scale(all_predictors()) %>%
    recipes::step_pca(all_predictors(), threshold = preProcessPCAthresh) #%>% num_comp = tune()

  # Cross-validation help(vfold_cv)
  set.seed(42)
  df3_cv_splits <- rsample::vfold_cv(df3, v = nrFolds_k, strata = strata_y) # , ... ,  breaks = 4

  # Model
  df3_model <-
    parsnip::linear_reg(penalty = tune(), mixture = tune()) %>% # tune() uses the grid
    parsnip::set_engine("glmnet")

  # Tuning; parameters; grid for ridge regression. https://rstudio-conf-2020.github.io/applied-ml/Part_5.html#26
  df3_glmn_grid <- base::expand.grid(
    penalty = 10 ^ seq(-3, -1, length = 20),
    mixture = (0:5) / 5
    )

  ctrl <- tune::control_grid(save_pred = TRUE)

  # Tune_grid
  df3_glmn_tune <- tune::tune_grid(
    df3_recipe,
    model = df3_model,
    resamples = df3_cv_splits,
    grid = df3_glmn_grid,
    control = ctrl
    )

  # Select the best penelty and mixture based on rmse help(select_best)
  best_glmn <- tune::select_best(df3_glmn_tune, metric = "rmse", maximize = TRUE)

  # Get predictions and observed (https://rstudio-conf-2020.github.io/applied-ml/Part_5.html#32)
  df3_predictions <- tune::collect_predictions(df3_glmn_tune) %>%
    dplyr::filter(penalty == best_glmn$penalty, mixture == best_glmn$mixture)

  # Evaluating the predictions using correlation help(filter)
  correlation <- stats::cor.test(df3_predictions$y, df3_predictions$.pred, method = methodCor)

  # Describe model; adding user's-description + the name of the x and y
  # describe_model_detail <- c(describe_model, paste(names(x)), paste(names(y)))
  describe_model_detail <- c(deparse(substitute(x)), deparse(substitute(y)), describe_model)

  output <- list(df3_predictions, describe_model_detail, correlation)
  names(output) <- c("predictions", "model description", "correlation")
  output
}
######################
########### End of textTrain function
######################
# wordembeddings <- wordembeddings4_10
# ratings_data <- sq_data_tutorial4_10
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
#' textTrainLists trains word embeddings from several text variable to several numeric variable.
#'
#' @param x List of several Wordembeddings with same length from textImport (NB remove single_word_we).
#' @param y A Tibble with numeric variables to predict.
#' @param trainMethod Method to train wordembeddings, default "regression", or "randomForest"
#' @param nrFolds_k Number of folds to use.
#' @param nrFolds_k_out Number of outer folds (only for textTrainCVpredictions textTrainCVpredictionsRF).
#' @param preProcessPCAthresh Preprocessing threshold.
#' @param strata_y variables to stratify according; default y, can set to NULL
#' @param methodCor Type of correlation used in evaluation; default pearson (see also "spearman", "kendall").
#' @param trees Number of trees used in random forest.
#' @param ... Arguments from textTrain.
#' @return A correlation between predicted and observed values; as well as predicted values.
#' @examples
#' wordembeddings <- wordembeddings4_10[1:2]
#' ratings_data <- sq_data_tutorial4_10[1:2]
#' wordembeddings <- textTrainLists(wordembeddings, ratings_data, nrFolds_k = 2)
#' @seealso see \code{\link{textTrain}}
#' @importFrom stats cor.test
#' @importFrom tibble as_tibble
#' @importFrom magrittr %>%
#' @importFrom dplyr arrange
#' @importFrom data.table %like%
#' @export
textTrainLists <- function(x, y, trainMethod = "regression", nrFolds_k = 10, preProcessPCAthresh = 0.95, strata_y = "y", methodCor = "pearson", trees=500, nrFolds_k_out = 10, ...) { #  , trees=500 method="regression"  method= "textTrainCVpredictions";  method= "textTrainCVpredictionsRF"

  # Get variable names in the list of outcomes
  variables <- names(y)
  # Duplicate variable names to as many different wordembeddings there are in x
  variables <- rep(variables, length(x))
  # Create data frame with duplicated variables
  y1 <- y[c(variables)]
  # Order columns alphabatically
  y1 <- y1[, order(colnames(y1))]

  # Creating descriptions of which variabeles are used in training, which is  added to the output help(mapply)
  descriptions <- paste(rep(names(x), length(y)), "_", names(y1), sep = "")

  if (trainMethod == "regression"){
    # Using mapply to loop over the word embeddings and the outcome variables help(mapply)
    output <- mapply(textTrain, x, y1, SIMPLIFY = FALSE, MoreArgs = list(nrFolds_k = nrFolds_k, preProcessPCAthresh = preProcessPCAthresh, strata_y = strata_y, methodCor = methodCor, ...))   #, preProcessPCAthresh=preProcessPCAthresh, strata_y = strata_y, methodCor = methodCor, MoreArgs = list(nrFolds_k = nrFolds_k, methodTrain = methodTrain, preProcessTrain = preProcessTrain, preProcessThresh = preProcessThresh, methodCor = methodCor, ...),

    output_t  <- t(as.data.frame(lapply(output, function(output) unlist(output$correlation)[[1]][c(1)])))
    output_df <- t(as.data.frame(lapply(output, function(output) unlist(output$correlation)[[2]][c(1)])))
    output_p  <- t(as.data.frame(lapply(output, function(output) unlist(output$correlation)[[3]][c(1)])))
    output_r  <- t(as.data.frame(lapply(output, function(output) unlist(output$correlation)[[4]][c(1)])))
    output_a  <- t(as.data.frame(lapply(output, function(output) unlist(output$correlation)[[6]][c(1)])))

    # Add Outcomes and Descriptions together; name the columns; and remove the rownames.
    output_ordered_named <- data.frame(cbind(descriptions, output_r, output_df, output_p, output_t, output_a))
    colnames(output_ordered_named) <- c("descriptions", "correlation", "df", "p_value", "t_statistics", "alternative")
    rownames(output_ordered_named) <- NULL

    output_predscore <- as.data.frame(lapply(output, function(output) unlist(output$predictions)))
    output_predscore_reg <- output_predscore[rownames(output_predscore) %like% ".pred", ] # like comes from data.table
    colnames(output_predscore_reg) <- c(paste(descriptions, "_pred", sep=""))
    results <- list(output_predscore_reg, output_ordered_named)
    names(results) <- c("predscores", "results")
    results

  } else if (trainMethod == "randomForest"){ #
    # Apply textTrainRandomForest function between each list element and sort outcome
    output <- mapply(textTrainRandomForest, x, y1, SIMPLIFY = FALSE, MoreArgs = list(trees = trees, nrFolds_k = nrFolds_k, strata_y = strata_y))
    output_chi <- t(as.data.frame(lapply(output, function(output) unlist(output$results)[[1]][[1]])))
    output_df <- t(as.data.frame(lapply(output, function(output) unlist(output$results)[[2]][[1]])))
    output_p <- t(as.data.frame(lapply(output, function(output) unlist(output$results)[[3]][[1]])))
    output_p_r <- tibble(output_chi, output_df, output_p)

    # Add Outcomes and Descriptions together; name the columns; and remove the rownames.
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

    } else if (trainMethod == "textTrainCVpredictions"){
      output <- mapply(textTrainCVpredictions, x, y1, SIMPLIFY = FALSE, MoreArgs = list(nrFolds_k = nrFolds_k, nrFolds_k_out=nrFolds_k_out, preProcessPCAthresh = preProcessPCAthresh, strata_y = strata_y))

      output_predscore <- as.data.frame(lapply(output, function(output) unlist(output)))
      output_predscore_reg <- output_predscore[rownames(output_predscore) %like% ".pred", ] # like comes from data.table
      colnames(output_predscore_reg) <- c(paste(descriptions, "_pred", sep=""))
      results <- list(output_predscore_reg)
      names(results) <- c("CVpredscores")
      results

    } else if (trainMethod == "textTrainCVpredictionsRF"){
      output <- mapply(textTrainCVpredictionsRF, x, y1, SIMPLIFY = FALSE,  MoreArgs = list(trees = trees, nrFolds_k_out = nrFolds_k_out, nrFolds_k = nrFolds_k, strata_y = strata_y))

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
# ratings_data <- sq_data_tutorial4_10[3]
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


#solmini_1 <- read_csv("/Users/oscar/Desktop/0 Studies/13 OnlineMini/combinedSOL_SM.csv")
#solmini_sd300_tk_mean1 <- read_rds("/Users/oscar/Desktop/0 Studies/5 R statistical semantics package/spaces/spaceDomain_solminidata_solmini_sd300_tk_mean.rds")
#x <- solmini_sd300_tk_mean1[1:2]
#y <- as_tibble(solmini_1$phq_tot)
#colnames(y) <- c("phq")
#train_lists_test_reg <- textTrainLists(x, y, trainMethod = "regression", nrFolds_k = 3) #, nrFolds_k = 2
#train_lists_test_reg

#y <- as_tibble(solmini_1$minidiagnose_cat)
#colnames(y) <- c("diagnose")
#train_lists_test <- textTrainLists(x, y, trainMethod = "randomForest", trees = 5, nrFolds_k = 2, strata_y = "y")
#train_lists_test




# train_lists_test <- textTrainLists(x, y, trainMethod = "textTrainCVpredictions", nrFolds_k_out=2)


#train_lists_test <- textTrainLists(x, y, trainMethod = "textTrainCVpredictionsRF", trees=50)
#train_lists_test
#colnames(train_lists_test$CVpredscores)

