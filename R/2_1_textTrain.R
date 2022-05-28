
#' Train word embeddings to a numeric (ridge regression) or categorical (random forest) variable.
#'
#' @param x Word embeddings from textEmbed (or textEmbedLayerAggreation).
#' Can analyze several variables at the same time; but if training to several
#' outcomes at the same time use a tibble within the list as input rather than just a
#' tibble input (i.e., keep the name of the wordembedding).
#' @param y Numeric variable to predict. Can be several; although then make
#' sure to have them within a tibble (this is required
#' even if it is only one outcome but several word embeddings variables).
#' @param force_train_method default is "automatic", so if y is a factor
#' random_forest is used, and if y is numeric ridge regression
#' is used. This can be overridden using "regression" or "random_forest".
#' @param ... Arguments from textTrainRegression or textTrainRandomForest
#' the textTrain function.
#' @return A correlation between predicted and observed values; as well as a
#'  tibble of predicted values.
#' @examples
#' \dontrun{
#' word_embeddings <- word_embeddings_4
#' ratings_data <- Language_based_assessment_data_8
#' results <- textTrain(
#'   word_embeddings$harmonytext,
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
    train_method <- "logistic"
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
      train_method <- "logistic"
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
  if (train_method == "regression" | train_method == "logistic") {
    # textTrainLists x; if more than one wordembedding list; or more than one column of numeric/categorical variable
    if ((!tibble::is_tibble(x) & length(x) > 1) | ((tibble::is_tibble(y) | is.data.frame(y)) & length(y) > 1)) {
      repression_output <- textTrainLists(
        x = x,
        y = y,
        force_train_method = train_method,
        ...
      )
      repression_output
    } else {
      repression_output <- textTrainRegression(
        x = x,
        y = y,
        model = train_method,
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

#' Sorts out the output from a regression model for the list format.
#'
#' @param output output from mapply of textTrainRegression or textTrainRandomForest
#' @param method_cor type of measure as output; default is pearson, see also spearman, kendall.
#' @param save_output including "all", "only_restuls_predictions" or "only_results".
#' @param descriptions description.
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

#' Sorts out the output from a classification for the list format.
#' This is a function because it is needed in both regression for logistic and for
#' Random forest
#'
#' @param output output from mapply of textTrainRegression or textTrainRandomForest
#' @param save_output including "all", "only_restuls_predictions" or "only_results".
#' @param train_method method used to train the models.
#' @return A list with result output depending on save_output setting.
#' @importFrom purrr reduce
#' @importFrom dplyr full_join
#' @noRd
sort_classification_output_list <- function(output, save_output, descriptions, train_method, ...) {
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


  # Get and sort the Prediction scores; names(output$harmonywords_factors2)
  if (save_output == "all" | save_output == "only_results_predictions") {
    if (train_method == "random_forest") output_predscore <- lapply(output, "[[", "truth_predictions")
    if (train_method == "logistic") output_predscore <- lapply(output, "[[", "predictions")
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


    results <- list(output1, output_predscore, output_ordered_named1)
    names(results) <- c("all_output", "predictions", "results")
  } else if (save_output == "only_results") {
    results <- list(output1, output_ordered_named1) #
    names(results) <- c("all_output", "results") #
  }
  results
}

#' Individually trains word embeddings from several text variables to several numeric or categorical variables.
#' It is possible to have  word embeddings from one text variable and several numeric/categprical variables;
#' or vice verse, word embeddings from several text variables to one numeric/categorical variable.
#' It is not possible to mix numeric and categorical variables.
#' @param x Word embeddings from textEmbed (or textEmbedLayerAggreation).
#' @param y Tibble with several numeric or categorical variables to predict. Please note that you cannot mix numeric and
#' categorical variables.
#' @param force_train_method Default is "automatic"; see also "regression" and "random_forest".
#' @param save_output Option not to save all output; default "all". see also "only_results"
#' and "only_results_predictions".
#' @param method_cor  A character string describing type of correlation (default "Pearson").
#' @param eval_measure  Type of evaluative measure to assess models on.
#' @param p_adjust_method Method to adjust/correct p-values for multiple comparisons
#' (default = "holm"; see also "none", "hochberg", "hommel", "bonferroni", "BH", "BY",  "fdr").
#' @param ... Arguments from textTrainRegression or textTrainRandomForest the textTrain function.
#' @return Correlations between predicted and observed values.
#' @examples
#' \dontrun{
#' word_embeddings <- word_embeddings_4[1:2]
#' ratings_data <- Language_based_assessment_data_8[5:6]
#' results <- textTrainLists(
#'   word_embeddings,
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
                           # model = "regression",
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

  # Force or decide regression or random forest (and select only categorical or numeric variables for multiple input).
  if (is.numeric(y) == TRUE & force_train_method == "automatic") {
    train_method <- "regression"
  } else if (force_train_method == "regression") {
    train_method <- "regression"
  } else if (is.factor(y) == TRUE & force_train_method == "automatic") {
    train_method <- "logistic"
  } else if (force_train_method == "random_forest") {
    train_method <- "random_forest"
  } else if ((tibble::is_tibble(y) | is.data.frame(y) & length(y) > 1) & force_train_method == "automatic") {

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
      train_method <- "logistic"
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

  if (train_method == "regression" | train_method == "logistic") {
    # Using mapply to loop over the word embeddings and the outcome variables to train the different combinations

    output <- mapply(textTrainRegression, x, y1, MoreArgs = list(
      method_cor = method_cor,
      save_output = save_output,
      model = train_method,
      ...
    ), SIMPLIFY = FALSE)

    if (train_method == "regression") {
      results <- sort_regression_output_list(output,
        method_cor = method_cor,
        save_output = save_output,
        descriptions = descriptions
      )
    } else if (train_method == "logistic") {
      results <- sort_classification_output_list(
        output = output,
        save_output = save_output,
        descriptions = descriptions,
        train_method = train_method
      )
    }
  } else if (train_method == "random_forest") {

    # Apply textTrainRandomForest function between each list element and sort outcome.
    output <- mapply(textTrainRandomForest, x, y1,
      MoreArgs = list(
        save_output = save_output,
        ...
      ),
      SIMPLIFY = FALSE
    )

    results <- sort_classification_output_list(
      output = output,
      save_output = save_output,
      descriptions = descriptions,
      train_method = train_method
    )
    results$results$p_value_corrected <- stats::p.adjust(as.numeric(results$results$p_value), method = p_adjust_method)
    # Combine output
    #
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

#' Predict scores or classification from, e.g., textTrain.
#'
#' @param model_info Model info (e.g., saved output from textTrain, textTrainRegression or textRandomForest).
#' @param new_data Word embeddings from new data to be predicted from.
#' @param  type Type of prediction; e.g., "prob", "class".
#' @param ... Setting trom stats::predict can be called.
#' @return Predicted scores from word embeddings.
#' @examples
#' word_embeddings <- word_embeddings_4
#' ratings_data <- Language_based_assessment_data_8
#' @seealso see \code{\link{textTrain}} \code{\link{textTrainLists}}
#' \code{\link{textTrainRandomForest}} \code{\link{textSimilarityTest}}
#' @importFrom recipes prep bake
#' @importFrom stats predict
#' @importFrom tibble is_tibble as_tibble_col
#' @export
textPredict <- function(model_info,
                        new_data,
                        type = NULL, ...) {

  # In case the embedding is in list form get the tibble form
  if (!tibble::is_tibble(new_data) & length(new_data) == 1) {
    new_data1 <- new_data[[1]]

    new_data1$id_nr <- c(seq_len(nrow(new_data1)))
    new_data_id_nr_col <- tibble::as_tibble_col(seq_len(nrow(new_data1)), column_name = "id_nr")

    # In case there are several different word embeddings (from different responses)
  } else if (!tibble::is_tibble(new_data) & length(new_data) > 1) {
    new_datalist <- lapply(new_data, function(X) {
      X <- dplyr::select(X, dplyr::starts_with("Dim"))
    })

    Nword_variables <- length(new_datalist)
    # Give each column specific names with indexes so that they can be
    # handled separately in the PCAs
    for (i in 1:Nword_variables) {
      colnames(new_datalist[[i]]) <- paste("Dim_we", i, ".", names(new_datalist[i]),
        colnames(new_datalist[[i]]),
        sep = ""
      )
    }

    # Make vector with each index so that we can allocate them separately for the PCAs
    variable_name_index_pca <- list()
    for (i in 1:Nword_variables) {
      variable_name_index_pca[i] <- paste("Dim_we", i, sep = "")
    }

    # Make one df rather than list.
    new_data1 <- dplyr::bind_cols(new_datalist)


    # Add ID
    new_data1$id_nr <- c(seq_len(nrow(new_data1)))
    new_data_id_nr_col <- tibble::as_tibble_col(seq_len(nrow(new_data1)), column_name = "id_nr")

    # Removing NAs for the analyses (here we could potentially impute missing values; e.g., mean of each dimension)
    new_data1 <- new_data1[complete.cases(new_data1), ]
  } else {
    new_data1 <- new_data

    new_data1$id_nr <- c(seq_len(nrow(new_data1)))
    new_data_id_nr_col <- tibble::as_tibble_col(seq_len(nrow(new_data1)), column_name = "id_nr")
  }

  # Load prepared_with_recipe
  data_prepared_with_recipe <- recipes::bake(model_info$final_recipe, new_data1)

  # Get column names to be removed
  colnames_to_b_removed <- colnames(data_prepared_with_recipe)
  colnames_to_b_removed <- colnames_to_b_removed[!colnames_to_b_removed == "id_nr"]

  # Get Prediction scores help(predict)
  predicted_scores2 <- data_prepared_with_recipe %>%
    bind_cols(stats::predict(model_info$final_model, new_data = new_data1, type = type), ...) %>%
    select(-!!colnames_to_b_removed) %>%
    full_join(new_data_id_nr_col, by = "id_nr") %>%
    arrange(id_nr) %>%
    select(-id_nr)
}


#' Significance testing correlations
#' If only y1 is provided a t-test is computed, between the absolute error from yhat1-y1 and yhat2-y1.
#'
#' If y2 is provided a bootstrapped procedure is used to compare the correlations between y1 and yhat1 versus
#' y2 and yhat2. This is achieved by creating two distributions of correlations using bootstrapping; and then
#' finally compute the distributions overlap.
#'
#' @param y1 The observed scores (i.e., what was used to predict when training a model).
#' @param y2 The second observed scores (default = NULL; i.e., for when comparing models that are predicting different
#' outcomes. In this case a bootstrap procedure is used to create two distributions of correlations that are
#' compared (see description above).
#' @param yhat1 The predicted scores from model 1.
#' @param yhat2 The predicted scores from model 2 that will be compared with model 1.
#' @param paired Paired test or not in stats::t.test (default TRUE).
#' @param bootstraps_times Number of bootstraps (when providing y2).
#' @param seed Set different seed.
#' @param ... Settings from stats::t.test or overlapping::overlap (e.g., plot = TRUE).
#' @return Comparison of correlations either a t-test or the overlap of a bootstrapped procedure (see $OV).
#' @examples
#' # Example random data
#' y1 <- runif(10)
#' yhat1 <- runif(10)
#' y2 <- runif(10)
#' yhat2 <- runif(10)
#'
#' boot_test <- textPredictTest(y1, yhat1, y2, yhat2, bootstraps_times = 10)
#' @seealso see \code{\link{textTrain}} \code{\link{textPredict}}
#' @importFrom stats t.test cor
#' @importFrom tibble is_tibble as_tibble_col
#' @importFrom tidyr unnest
#' @importFrom dplyr select mutate
#' @importFrom overlapping overlap
#' @importFrom rsample analysis bootstraps
#' @export
textPredictTest <- function(y1,
                            y2 = NULL,
                            yhat1,
                            yhat2,
                            paired = TRUE,
                            bootstraps_times = 10000,
                            seed = 6134,
                            ...) {

  ## If comparing predictions from models that predict the SAME outcome
  if (is.null(y2)) {
    yhat1_absolut_error <- abs(yhat1 - y1)
    yhat1_absolut_error_mean <- mean(yhat1_absolut_error)
    yhat1_absolut_error_sd <- sd(yhat1_absolut_error)

    yhat2_absolut_error <- abs(yhat2 - y1)
    yhat2_absolut_error_mean <- mean(yhat2_absolut_error)
    yhat2_absolut_error_sd <- sd(yhat2_absolut_error)

    # T-test
    t_test_results <- stats::t.test(yhat1_absolut_error,
      yhat2_absolut_error,
      paired = paired, ...
    ) # , ... Double check
    # Effect size
    cohensD <- cohens_d(
      yhat1_absolut_error,
      yhat2_absolut_error
    )
    # Descriptive
    descriptives <- tibble::tibble(
      yhat1_absolut_error_mean, yhat1_absolut_error_sd,
      yhat2_absolut_error_mean, yhat2_absolut_error_sd
    )
    # Outputs
    output <- list(descriptives, cohensD, t_test_results)
    names(output) <- c("Descriptives", "Effect_size", "Test")
  }

  ## If comparing predictions from models that predict DIFFERENT outcomes

  if (!is.null(y2)) {
    set.seed(seed)
    # Bootstrap data to create distribution of correlations; help(bootstraps)

    # Correlation function
    corr_on_bootstrap <- function(split) {
      stats::cor(rsample::analysis(split)[[1]], rsample::analysis(split)[[2]])
    }

    # Creating correlation distribution for y1 and yhat1
    y_yhat1_df <- tibble::tibble(y1, yhat1)
    boots_y1 <- rsample::bootstraps(y_yhat1_df, times = bootstraps_times, apparent = FALSE)

    boot_corrss_y1 <- boots_y1 %>%
      dplyr::mutate(corr_y1 = purrr::map(splits, corr_on_bootstrap))

    boot_y1_distribution <- boot_corrss_y1 %>%
      tidyr::unnest(corr_y1) %>%
      dplyr::select(corr_y1)

    # Creating correlation distribution for y2 and yhat2
    y_yhat2_df <- tibble::tibble(y2, yhat2)
    boots_y2 <- rsample::bootstraps(y_yhat2_df, times = bootstraps_times, apparent = FALSE)

    boot_corrss_y2 <- boots_y2 %>%
      dplyr::mutate(corr_y2 = purrr::map(splits, corr_on_bootstrap))

    boot_y2_distribution <- boot_corrss_y2 %>%
      tidyr::unnest(corr_y2) %>%
      dplyr::select(corr_y2)


    ### Examining the overlap
    x_list_dist <- list(boot_y1_distribution$corr_y1, boot_y2_distribution$corr_y2)

    output <- overlapping::overlap(x_list_dist, ...)
    output <- list(output$OV[[1]])
    names(output) <- "overlapp_p_value"
  }
  output
}
