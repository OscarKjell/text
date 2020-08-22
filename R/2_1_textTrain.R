
#x <- wordembeddings4$harmonytext
#y <- Language_based_assessment_data_8$gender
#
#outside_folds = 10
#outside_strata_y = "y"
#inside_folds = 10
#inside_strata_y = "y"
#trees = 5
#model_description = "Consider writing a description of your model here"
#multi_cores = TRUE


#wordembeddings <- wordembeddings4
#ratings_data <- Language_based_assessment_data_8
#
#results <- textTrain(wordembeddings$harmonytext,
#                      ratings_data$hilstotal,
#                      penalty = 1, # 1 due to computational constraints for the example context
#                      multi_cores = FALSE
# )

# devtools::document()
#' Train word embeddings to a numeric (ridge regression) or categorical (random forest) variable.
#'
#' @param x Word embeddings from textEmbed (or textLayerAggregation).
#' @param y Numeric variable to predict.
#' @param force_train_method default is "automatic", so if y is a factor random_forest is used, and if y is numeric ridge regression
#' is used. This can be overridden using "regression" or "random_forest".
#' @param ... Arguments from textTrainRegression or textTrainRandomForest the textTrain function.
#' @return A correlation between predicted and observed values; as well as a tibble of predicted values.
#' @examples
#' wordembeddings <- wordembeddings4
#' ratings_data <- Language_based_assessment_data_8
#' results <- textTrain(wordembeddings$harmonytext,
#'                      ratings_data$hilstotal,
#'                      penalty = 1, # 1 due to computational constraints for the example context
#'                      multi_cores = FALSE
#' )
#' @seealso \code{\link{textTrainRegression}} \code{\link{textTrainRandomForest}}
#' \code{\link{textTrainLists}} \code{\link{textDiff}}
#' @export
textTrain <- function(x,
                      y,
                      force_train_method = "automatic",
                      ...){

  if (is.numeric(y) == TRUE & force_train_method == "automatic") {
    train_method = "regression"
  } else if (force_train_method == "regression"){
    train_method = "regression"
  } else if (is.factor(y) == TRUE & force_train_method =="automatic"){
    train_method = "random_forest"
  } else if (force_train_method == "random_forest"){
    train_method = "random_forest"
  }

  if (train_method == "regression"){
    repression_output <- textTrainRegression(x=x,
                        y=y,
                        ...)
    repression_output

  } else if (train_method == "random_forest"){
   random_forest_output <-  textTrainRandomForest(x=x,
                          y=y,
                          ...)
   random_forest_output
  }
  }


#x <- wordembeddings4[1:2]
##y <- ratings_data
#y1 <- Language_based_assessment_data_8[8]$gender
#y2 <- Language_based_assessment_data_8[8]$gender
#y <- tibble(y1, y2)

# library(data.table)
# devtools::document()
#' Individually trains word embeddings from several text variables to several numeric/categorical variables.
#' @param x Word embeddings from textEmbed (or textLayerAggregation).
#' @param y Numeric variable to predict.
#' @param force_train_method default is automatic; see also "regression" and "random_forest".
#' @param ... Arguments from textTrainRegression or textTrainRandomForest the textTrain function.
#' @return Correlations between predicted and observed values.
#' @examples
#' wordembeddings <- wordembeddings4[1:2]
#' ratings_data <- Language_based_assessment_data_8[5:6]
#' results <- textTrainLists(wordembeddings,
#' ratings_data,
#' multi_cores = FALSE)
#' @seealso see \code{\link{textTrain}}  \code{\link{textTrainRegression}}  \code{\link{textTrainRandomForest}}
#' @importFrom stats cor.test
#' @importFrom tibble as_tibble
#' @importFrom magrittr %>%
#' @importFrom dplyr arrange
#' @export
textTrainLists <- function(x,
                           y,
                           force_train_method = "automatic",
                           ...) {

  # Force or decide regression or random forest.
  if (is.numeric(y) == TRUE & force_train_method == "automatic") {
    train_method = "regression"
  } else if (force_train_method == "regression"){
    train_method = "regression"
  } else if (is.factor(y) == TRUE & force_train_method =="automatic"){
    train_method = "random_forest"
  } else if (force_train_method == "random_forest"){
    train_method = "random_forest"
  }

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

  if (force_train_method == "regression") {
    # Using mapply to loop over the word embeddings and the outcome variables. help(mapply)
    output <- mapply(textTrainRegression, x, y1, SIMPLIFY = FALSE, ...)

    output_t <- t(as.data.frame(lapply(output, function(output) unlist(output$correlation)[[1]][c(1)])))
    output_df <-t(as.data.frame(lapply(output, function(output) unlist(output$correlation)[[2]][c(1)])))
    output_p <- t(as.data.frame(lapply(output, function(output) unlist(output$correlation)[[3]][c(1)])))
    output_r <- t(as.data.frame(lapply(output, function(output) unlist(output$correlation)[[4]][c(1)])))
    output_a <- t(as.data.frame(lapply(output, function(output) unlist(output$correlation)[[6]][c(1)])))

    # Add Outcomes and Descriptions together; name the columns; and remove the row names.
    output_ordered_named <- data.frame(cbind(descriptions, output_r, output_df, output_p, output_t, output_a))
    colnames(output_ordered_named) <- c("descriptions", "correlation", "df", "p_value", "t_statistics", "alternative")
    rownames(output_ordered_named) <- NULL

    output_predscore <- as.data.frame(lapply(output, function(output) unlist(output$predictions)))
    output_predscore_reg <- output_predscore[grep("predictions", rownames(output_predscore)), ]
    colnames(output_predscore_reg) <- c(paste(descriptions, "_pred", sep = ""))

    names(output) <- descriptions
    #Remove predictions from output since they are saved together
    output1 <- purrr::map(output, ~purrr::discard(.x, names(.x) == 'predictions'))


    results <- list(output1, output_predscore_reg, output_ordered_named) #
    names(results) <- c("all_output", "predictions", "results") #
    results
  } else if (force_train_method == "random_forest") { #

    # Apply textTrainRandomForest function between each list element and sort outcome.
    output <- mapply(textTrainRandomForest, x, y1, SIMPLIFY = FALSE, ...)

    output_chi <- t(as.data.frame(lapply(output, function(output) unlist(output$chisq)[[1]][[1]])))
    output_df <- t(as.data.frame(lapply(output, function(output) unlist(output$chisq)[[2]][[1]])))
    output_p <- t(as.data.frame(lapply(output, function(output) unlist(output$chisq)[[3]][[1]])))
    output_p_r <- tibble::tibble(output_chi, output_df, output_p)

    # Add Outcomes and Descriptions together; name the columns; and remove the row names.
    output_ordered_named <- data.frame(cbind(descriptions, output_p_r))
    colnames(output_ordered_named) <- c("descriptions", "chi2", "df", "p_value")
    output_ordered_named

    output_eval_measures <- t(as.data.frame(lapply(output, function(output) unlist(output$results$.estimate))))
    output_eval_measures_names <- t(as.data.frame(lapply(output, function(output) unlist(output$results$.metric))))
    colnames(output_eval_measures) <- c(output_eval_measures_names[1,])
    output_eval_measures

    output_ordered_named1 <- cbind(output_ordered_named, output_eval_measures)
    rownames(output_ordered_named1) <- NULL

    # Get and sort the Prediction scores
    output_predscore1 <- lapply(output, "[[", "truth_predictions")
    names(output_predscore1) <- descriptions
    output_predscore <- do.call(cbind, output_predscore1) %>%
      tibble::as_tibble() %>%
      dplyr::arrange()

    names(output) <- descriptions
    output1 <- purrr::map(output, ~purrr::discard(.x, names(.x) == 'predictions'))

    # Combine output
    results <- list(output1, output_predscore, output_ordered_named1)
    names(results) <- c("all_output", "predictions", "results")
    results # results$all_output$satisfactionwords_y2$model_description[3]
  }
}

# Regression
#wordembeddings_list <- wordembeddings4[1:2]
#ratings_data_list <- Language_based_assessment_data_8[5:6]
##results <- textTrainLists(wordembeddings, ratings_data)
#
#list_restuls <- textTrainLists(x = wordembeddings_list,
#                               y = ratings_data_list,
#                               trainMethod = "regression",
#
#                               outside_folds = 10,
#                               outside_strata_y = "y",
#                               inside_folds = 10,
#                               inside_strata_y = "y",
#                               preprocess_PCA = 0.95,
#                               method_cor = "pearson",
#                               model_description = "Consider writing a description here",
#                               multi_cores = TRUE,
#
#
#                               trees = 500)

# Random Forest
##wordembeddings_list <- wordembeddings4[1:2]
##ratings_data_list <- Language_based_assessment_data_8[8]
##ratings_data_list2 <- ratings_data_list
##colnames(ratings_data_list2) <-  "gender2"
##ratings_data_list <- tibble::tibble(ratings_data_list, ratings_data_list2)
##
###results <- textTrainLists(wordembeddings, ratings_data)
##
##list_restuls <- textTrainLists(x = wordembeddings_list,
##                               y = ratings_data_list,
##                               trainMethod = "random_forest",
##
##                               outside_folds = 10,
##                               outside_strata_y = "y",
##                               inside_folds = 10,
##                               inside_strata_y = "y",
##                               preprocess_PCA = 0.95,
##                               method_cor = "pearson",
##                               model_description = "Consider writing a description here",
##                               multi_cores = TRUE,
##
##
##                               trees = 500)
##list_restuls





# devtools::document()
#' Predict scores or classification from, e.g., textTrain.
#'
#' @param model_info model info (e.g., saved output from textTrain, textTrainRegression or textRandomForest).
#' @param new_data Word embeddings from new data to be predicted from.
#' @return Predicted scores from word embeddings.
#' @examples
#' wordembeddings <- wordembeddings4
#' ratings_data <- Language_based_assessment_data_8
#'
#' @seealso see \code{\link{textLayerAggregation}} \code{\link{textTrainLists}}
#' \code{\link{textTrainRandomForest}} \code{\link{textDiff}}
#' @importFrom recipes prep bake
#' @importFrom stats predict
#' @export
textPredict <- function(model_info = NULL, new_data=NULL){

  # Load prepared_with_recipe
  data_prepared_with_recipe <- recipes::bake(model_info$preprocessing_recipe, new_data)

  # Get scores
  predicted_scores <- stats::predict(model_info$final_model, data_prepared_with_recipe)
  predicted_scores
}




## TESTING
#trained <- textTrain(wordembeddings4$harmonytext,
#                     Language_based_assessment_data_8$hilstotal,
#                     #outside_strata_y = NULL,
#                     #inside_strata_y = NULL,
#                     penalty = c(1),
#                     mixture = c(0),
#                     trees = c(1000),
#                     preprocess_PCA = c(0.95),
#                     multi_cores = FALSE
#)
#
#
#
#test_data <- c("happy", "sad unhappy")
#
#test_data_we <- textEmbed(test_data)
#
#hils_predicted_scores1 <- textPredict(model = trained,
#                                     new_data = test_data_we$harmonywords)
#hils_predicted_scores1





