


# devtools::document()
#' Train word embeddings to a numeric (ridge regression) or categorical (random forest) variable.
#'
#' @param x Word embeddings from textEmbed (or textEmbedLayerAggregation). Can analyze several variables at the same time; but if training to several
#' outcomes at the same time use a tibble within the list as input rather than just a tibble input (i.e., keep the name of the wordembedding).
#' @param y Numeric variable to predict. Can be several; although then make sure to have them within a tibble (this is required
#' even if it is only one outcome but several word embeddings variables).
#' @param force_train_method default is "automatic", so if y is a factor random_forest is used, and if y is numeric ridge regression
#' is used. This can be overridden using "regression" or "random_forest".
#' @param ... Arguments from textTrainRegression or textTrainRandomForest the textTrain function.
#' @return A correlation between predicted and observed values; as well as a tibble of predicted values.
#' @examples
#' \donttest{
#' wordembeddings <- wordembeddings4
#' ratings_data <- Language_based_assessment_data_8
#' results <- textTrain(
#'   wordembeddings$harmonytext,
#'   ratings_data$hilstotal
#' )
#' }
#' @seealso \code{\link{textTrainRegression}} \code{\link{textTrainRandomForest}}
#' \code{\link{textTrainLists}} \code{\link{textSimilarityTest}}
#' @importFrom tibble is_tibble
#' @importFrom dplyr select_if
#' @export
textTrain <- function(x,
                      y,
                      force_train_method = "automatic",
                      ...) {

  # Figure out which train_method to use (textTrainRegression or textTrainRandomForest)
  if (is.numeric(y) == TRUE & force_train_method == "automatic") {
    train_method <- "regression"
  } else if (is.factor(y) == TRUE & force_train_method == "automatic") {
    train_method <- "random_forest"
  } else if ((tibble::is_tibble(y) | is.data.frame(y) & length(y) > 1) & force_train_method == "automatic") {

    # Create a dataframe with only one type (numeric or categorical) depending on most frequent type
    # Select all numeric variables
    y_n <- dplyr::select_if(y, is.numeric)
    # Select all categorical variables
    y_f <- dplyr::select_if(y, is.factor)
    # Select most frequent type as y
    if (length(y_n) >= length(y_f)) {
      y <- y_n
      train_method <- "regression"
    } else if (length(y_n) < length(y_f)) {
      y <- y_f
      train_method <- "random_forest"
    }
  } else if (((tibble::is_tibble(y) | is.data.frame(y)) & length(y) > 1) & force_train_method == "regression") {
    y <- dplyr::select_if(y, is.numeric)
    train_method <- "regression"
  } else if (((tibble::is_tibble(y) | is.data.frame(y)) & length(y) > 1) & force_train_method == "random_forest") {
    y <- dplyr::select_if(y, is.factor)
    train_method <- "random_forest"
  } else if (force_train_method == "regression") {
    train_method <- "regression"
  } else if (force_train_method == "random_forest") {
    train_method <- "random_forest"
  }


  # Analyze according to train_method decided above.
  if (train_method == "regression") {
    # textTrainLists x; if more than one wordembedding list; or more than one column of numeric/categorical variable
    if ((!tibble::is_tibble(x) & length(x) > 1) | ((tibble::is_tibble(y) | is.data.frame(y)) & length(y) > 1)) {
      repression_output <- textTrainLists(
        x = x,
        y = y,
        force_train_method = "regression",
        ...
      )
      repression_output
    } else {
      repression_output <- textTrainRegression(
        x = x,
        y = y,
        ...
      )
      repression_output
    }
  } else if (train_method == "random_forest") {
    if ((!tibble::is_tibble(x) & length(x) > 1) | (tibble::is_tibble(y) | is.data.frame(y) & length(y) > 1)) {
      random_forest_output <- textTrainLists(
        x = x,
        y = y,
        force_train_method = "random_forest",
        ...
      )
      random_forest_output
    } else {
      random_forest_output <- textTrainRandomForest(
        x = x,
        y = y,
        ...
      )
      random_forest_output
    }
  }
}



# devtools::document()
#' Sorts out the output from a regression model for the list format.
#'
#' @param output output from mapply of textTrainRegression or textTrainRandomForest
#' @param method_cor type of measure as output; default is pearson, see also spearman, kendall.
#' @param save_output including "all", "only_restuls_predictions" or "only_results".
#' @return A list with result output depending on save_output setting.
#' @noRd
sort_regression_output_list <- function(output, method_cor, save_output, descriptions, ...) {
  # Sort out the summary results depending on type of correlation method used
  if (method_cor == "pearson") {
    output_t <- t(as.data.frame(lapply(output, function(output) unlist(output$results)[[1]][c(1)])))
    output_df <- t(as.data.frame(lapply(output, function(output) unlist(output$results)[[2]][c(1)])))
    output_p <- t(as.data.frame(lapply(output, function(output) unlist(output$results)[[3]][c(1)])))
    output_r <- t(as.data.frame(lapply(output, function(output) unlist(output$results)[[4]][c(1)])))
    output_a <- t(as.data.frame(lapply(output, function(output) unlist(output$results)[[6]][c(1)])))

    # Add Outcomes and Descriptions together; name the columns; and remove the row names.
    output_ordered_named <- data.frame(cbind(descriptions, output_r, output_df, output_p, output_t, output_a))
    colnames(output_ordered_named) <- c("descriptions", "correlation", "df", "p_value", "t_statistics", "alternative")
    rownames(output_ordered_named) <- NULL
  } else if (method_cor == "spearman" | method_cor == "kendall") {
    output_S <- t(as.data.frame(lapply(output, function(output) unlist(output$results)[[1]][c(1)])))
    output_p <- t(as.data.frame(lapply(output, function(output) unlist(output$results)[[2]][c(1)])))
    output_r <- t(as.data.frame(lapply(output, function(output) unlist(output$results)[[3]][c(1)])))
    output_a <- t(as.data.frame(lapply(output, function(output) unlist(output$results)[[5]][c(1)])))

    # Add Outcomes and Descriptions together; name the columns; and remove the row names.
    output_ordered_named <- data.frame(cbind(descriptions, output_r, output_p, output_S, output_a))
    if (method_cor == "spearman") {
      colnames(output_ordered_named) <- c("descriptions", "rho_correlation", "p_value", "S_statistics", "alternative")
    } else if (method_cor == "kendall") {
      colnames(output_ordered_named) <- c("descriptions", "tau_correlation", "p_value", "z_statistics", "alternative")
    }
    rownames(output_ordered_named) <- NULL
  }

  names(output) <- descriptions
  # Remove predictions from output since they are saved together
  output1 <- purrr::map(output, ~ purrr::discard(.x, names(.x) == "predictions"))

  if (save_output == "all" | save_output == "only_results_predictions") {
    output_predscore <- as.data.frame(lapply(output, function(output) unlist(output$predictions)))
    output_predscore_reg <- output_predscore[grep("predictions", rownames(output_predscore)), ]
    colnames(output_predscore_reg) <- c(paste(descriptions, "_pred", sep = ""))

    output_predscore_reg$id_nr <- output[[1]]$predictions$id_nr

    results <- list(output1, output_predscore_reg, output_ordered_named) #
    names(results) <- c("all_output", "predictions", "results") #
  } else if (save_output == "only_results") {
    results <- list(output1, output_ordered_named) #
    names(results) <- c("all_output", "results") #
  }
  results
}

# devtools::document()
#' Sorts out the output from a classification for the list format.
#' This is a function because it is needed in both regression for logistic and for
#' Random forest
#'
#' @param output output from mapply of textTrainRegression or textTrainRandomForest
#' @param save_output including "all", "only_restuls_predictions" or "only_results".
#' @return A list with result output depending on save_output setting.
#' @importFrom purrr reduce
#' @importFrom dplyr full_join
#' @noRd
sort_classification_output_list <- function(output, save_output, descriptions, ...) {
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
  colnames(output_eval_measures) <- c(output_eval_measures_names[1, ])
  output_eval_measures

  output_ordered_named1 <- cbind(output_ordered_named, output_eval_measures)
  rownames(output_ordered_named1) <- NULL

  # Remove predictions since it will be collated together
  names(output) <- descriptions
  output1 <- purrr::map(output, ~ purrr::discard(.x, names(.x) == "predictions"))


  # Get and sort the Prediction scores
  if (save_output == "all" | save_output == "only_results_predictions") {
    output_predscore <- lapply(output, "[[", "truth_predictions")

    # Append dataframe name to each of its columns within a list of dataframes
    output_predscore <- purrr::imap(output_predscore, ~ dplyr::rename_with(.x, function(x) paste(.y, x, sep = "_")))

    # Renaming the last column of each dataframe to id_nr so that they can be joint later.
    output_predscore <- lapply(output_predscore, function(x) {
      names(x)[ncol(x)] <- "id_nr"
      x
    })

    output_predscore <- output_predscore %>%
      purrr::reduce(dplyr::full_join, "id_nr") %>%
      dplyr::arrange(id_nr)

    # colnames(output_predscore_reg) <- c(paste(descriptions, "_pred", sep = ""))

    # output_predscore1 <- lapply(output, "[[", "truth_predictions")
    #
    # help(do.call)
    # output_predscore <- do.call(cbind, output_predscore1) %>%
    #   tibble::as_tibble() %>%
    #   dplyr::arrange()

    results <- list(output1, output_predscore, output_ordered_named1)
    names(results) <- c("all_output", "predictions", "results")
  } else if (save_output == "only_results") {
    results <- list(output1, output_ordered_named1) #
    names(results) <- c("all_output", "results") #
  }
  results
}


# library(data.table)
# devtools::document()
#' Individually trains word embeddings from several text variables to several numeric or categorical variables. It is possible
#' to have  word embeddings from one text variable and several numeric/categprical variables; or vice verse, word embeddings from
#' several text variables to one numeric/categorical variable. It is not possible to mix numeric and categorical variables.
#' @param x Word embeddings from textEmbed (or textEmbedLayerAggregation).
#' @param y Tibble with several numeric or categorical variables to predict. Please note that you cannot mix numeric and
#' categorical variables.
#' @param force_train_method default is automatic; see also "regression" and "random_forest".
#' @param save_output Option not to save all output; default "all". see also "only_results" and "only_results_predictions".
#' @param method_cor  "pearson",
#' @param model  type of model to use in regression; default is "regression"; see also "logistic".
#' (To set different random forest algorithms see extremely_randomised_splitrule parameter in textTrainRandomForest)
#' @param eval_measure  Type of evaluative measure to assess models on.
#' @param p_adjust_method Method to adjust/correct p-values for multiple comparisons
#' (default = "holm"; see also "none", "hochberg", "hommel", "bonferroni", "BH", "BY",  "fdr").
#' @param ... Arguments from textTrainRegression or textTrainRandomForest the textTrain function.
#' @return Correlations between predicted and observed values.
#' @examples
#' \donttest{
#' wordembeddings <- wordembeddings4[1:2]
#' ratings_data <- Language_based_assessment_data_8[5:6]
#' results <- textTrainLists(
#'   wordembeddings,
#'   ratings_data
#' )
#' results
#' comment(results)
#' }
#' @seealso see \code{\link{textTrain}}  \code{\link{textTrainRegression}}  \code{\link{textTrainRandomForest}}
#' @importFrom stats cor.test
#' @importFrom tibble as_tibble
#' @importFrom magrittr %>%
#' @importFrom dplyr arrange
#' @export
textTrainLists <- function(x,
                           y,
                           force_train_method = "automatic",
                           save_output = "all",
                           method_cor = "pearson",
                           model = "regression",
                           eval_measure = "rmse",
                           p_adjust_method = "holm",
                           ...) {
  T1_textTrainLists <- Sys.time()

  # If y is a vector make it into a tibble format
  if (is.vector(y) == TRUE) {
    y <- tibble::as_tibble_col(y)
  }

  # If x is a tibble (and not a list with tibble(s)), make it into a list with tibble format
  if (tibble::is_tibble(x) == TRUE) {
    x_name <- deparse(substitute(x))
    x <- list(x)
    names(x) <- x_name
  }

    if ((tibble::is_tibble(y) | is.data.frame(y) & length(y) > 1) & force_train_method == "automatic") {

    # Create a dataframe only comprising numeric or categorical depending on most frequent type
    # Select all numeric variables
    y_n <- dplyr::select_if(y, is.numeric)
    # Select all categorical variables
    y_f <- dplyr::select_if(y, is.factor)
    # Select most frequent type as y
    if (length(y_n) >= length(y_f)) {
      y <- y_n
      train_method <- "regression"
    } else if (length(y_n) < length(y_f)) {
      y <- y_f
      train_method <- "random_forest"
    }
  } else if ((tibble::is_tibble(y) | is.data.frame(y) & length(y) > 1) & force_train_method == "regression") {
    y <- dplyr::select_if(y, is.numeric)
    train_method <- "regression"
  } else if ((tibble::is_tibble(y) | is.data.frame(y) & length(y) > 1) & force_train_method == "random_forest") {
    y <- dplyr::select_if(y, is.factor)
    train_method <- "random_forest"
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

  if (train_method == "regression") {
    # Using mapply to loop over the word embeddings and the outcome variables to train the different combinations

    output <- mapply(textTrainRegression, x, y1, MoreArgs = list(
      method_cor = method_cor,
      save_output = save_output,
      model = model,
      ...
    ), SIMPLIFY = FALSE)

    if (model == "regression") {
      results <- sort_regression_output_list(output, method_cor = method_cor, save_output = save_output, descriptions = descriptions)
    } else if (model == "logistic") {
      results <- sort_classification_output_list(output = output, save_output = save_output, descriptions = descriptions)
    }
  } else if (train_method == "random_forest") { #

    # Apply textTrainRandomForest function between each list element and sort outcome.
    output <- mapply(textTrainRandomForest, x, y1, MoreArgs = list(save_output = save_output, ...), SIMPLIFY = FALSE)

    results <- sort_classification_output_list(output = output, save_output = save_output, descriptions = descriptions)
    results$results$p_value_corrected <- stats::p.adjust(as.numeric(results$results$p_value), method = p_adjust_method)
  }

  # Time
  T2_textTrainLists <- Sys.time()
  Time_textTrainLists <- T2_textTrainLists - T1_textTrainLists
  Time_textTrainLists <- sprintf("Duration to train text: %f %s", Time_textTrainLists, units(Time_textTrainLists))
  Date_textTrainLists <- Sys.time()
  time_date <- paste(Time_textTrainLists,
    "; Date created: ", Date_textTrainLists,
    sep = "",
    collapse = " "
  )
  results$results$p_value_corrected <- stats::p.adjust(results$results$p_value, method = p_adjust_method)
  comment(results) <- time_date
  results
}


# devtools::document()
#' Predict scores or classification from, e.g., textTrain.
#'
#' @param model_info Model info (e.g., saved output from textTrain, textTrainRegression or textRandomForest).
#' @param new_data Word embeddings from new data to be predicted from.
#' @param ... From predict
#' @return Predicted scores from word embeddings.
#' @examples
#' wordembeddings <- wordembeddings4
#' ratings_data <- Language_based_assessment_data_8
#' @seealso see \code{\link{textEmbedLayerAggregation}} \code{\link{textTrainLists}}
#' \code{\link{textTrainRandomForest}} \code{\link{textSimilarityTest}}
#' @importFrom recipes prep bake
#' @importFrom stats predict
#' @importFrom tibble is_tibble as_tibble_col
#' @export
textPredict <- function(model_info, new_data, ...) {

  # In case the embedding is in list form get the tibble form
  if (!tibble::is_tibble(new_data) & length(new_data) == 1) {
    new_data1 <- new_data[[1]]
    # Get original columns names, to remove these column from output
    original_colnames <- colnames(new_data1)

    new_data1$id_nr <- c(1:nrow(new_data1))
    new_data_id_nr_col <- tibble::as_tibble_col(1:nrow(new_data1), column_name = "id_nr")

    # In case there are several different word embeddings (from different responses)
  } else if (!tibble::is_tibble(new_data) & length(new_data) > 1) {
    new_datalist <- lapply(new_data, function(X) {
      X <- dplyr::select(X, dplyr::starts_with("Dim"))
    })

    Nword_variables <- length(new_datalist)
    # Give each column specific names with indexes so that they can be handled separately in the PCAs
    for (i in 1:Nword_variables) {
      colnames(new_datalist[[i]]) <- paste("Dim_we", i, ".", names(new_datalist[i]), colnames(new_datalist[[i]]), sep = "")
    }

    # Make vector with each index so that we can allocate them separately for the PCAs
    variable_name_index_pca <- list()
    for (i in 1:Nword_variables) {
      variable_name_index_pca[i] <- paste("Dim_we", i, sep = "")
    }

    # Make one df rather then list.
    new_data1 <- dplyr::bind_cols(new_datalist)

    # Get original columns names, to remove these column from output
    original_colnames <- colnames(new_data1)

    # Add id
    new_data1$id_nr <- c(1:nrow(new_data1))
    new_data_id_nr_col <- tibble::as_tibble_col(1:nrow(new_data1), column_name = "id_nr")

    # Get the name of the first variable; which is used to exclude NA (i.e., word embedding have NA in all columns)
    V1 <- colnames(new_data1)[1]

    # Removing NAs for the analyses (here we could potentially impute missing values; e.g., mean of each dimension)
    new_data1 <- new_data1[complete.cases(new_data1), ]
  } else {
    new_data1 <- new_data
    # Get original columns names, to remove these column from output
    original_colnames <- colnames(new_data1)

    new_data1$id_nr <- c(1:nrow(new_data1))
    new_data_id_nr_col <- tibble::as_tibble_col(1:nrow(new_data1), column_name = "id_nr")
  }

  # Load prepared_with_recipe
  data_prepared_with_recipe <- recipes::bake(model_info$final_recipe, new_data1)

  # Get column names to be removed
  colnames_to_b_removed <- colnames(data_prepared_with_recipe)
  colnames_to_b_removed <- colnames_to_b_removed[!colnames_to_b_removed == "id_nr"]

  # Get scores
  predicted_scores <- data_prepared_with_recipe %>%
    bind_cols(predict(model_info$final_model, new_data = data_prepared_with_recipe, ...)) %>% #
    select(-!!colnames_to_b_removed) %>%
    full_join(new_data_id_nr_col, by = "id_nr") %>%
    arrange(id_nr) %>%
    select(-id_nr)

  predicted_scores
}
