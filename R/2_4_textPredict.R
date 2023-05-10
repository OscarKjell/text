
#' Predict scores or classification from, e.g., textTrain.
#'
#' @param model_info (model object) Model info (e.g., saved output from textTrain,
#' textTrainRegression or textRandomForest).
#' @param word_embeddings (tibble) Word embeddings
#' @param x_append (tibble) Variables to be appended after the word embeddings (x).
#' @param dim_names (boolean) Account for specific dimension names from textEmbed()
#' (rather than generic names including Dim1, Dim2 etc.). If FALSE the models need to have been trained on
#' word embeddings created with dim_names FALSE, so that embeddings were only called Dim1, Dim2 etc.
#' @param type (string) Type of prediction; e.g., "prob", "class".
#' @param ...  Setting from stats::predict can be called.
#' @return Predicted scores from word embeddings.
#' @examples
#' word_embeddings <- word_embeddings_4
#' ratings_data <- Language_based_assessment_data_8
#' @seealso see \code{\link{textTrain}} \code{\link{textTrainLists}}
#' \code{\link{textTrainRandomForest}}
#' @importFrom recipes prep bake
#' @importFrom stats predict
#' @importFrom tibble is_tibble as_tibble_col
#' @importFrom dplyr bind_cols select full_join arrange
#' @export
textPredict <- function(model_info,
                        word_embeddings,
                        x_append = NULL,
                        type = NULL,
                        dim_names = TRUE,
                        ...) {

  # Get the right word embeddings
  if (dim_names == TRUE) {
    # Select the predictor variables needed for the prediction
    target_variables_names <- model_info$final_recipe$var_info$variable[model_info$final_recipe$var_info$role == "predictor"]

    ## Get Word Embedding Names
    # remove those starting with Dim0
    We_names1 <- target_variables_names[!grepl("^Dim0", target_variables_names)]

    # Get everything after "_"
    We_names1_v_colnames <- substring(We_names1, regexpr("_", We_names1) + 1)
    # Get unique (keep the order the same)
    word_embeddings_names <- unique(We_names1_v_colnames)

    # Select the word embeddings
    word_embeddings <- word_embeddings[word_embeddings_names]
  } else {
    # Remove specific names in the word embeddings
    word_embeddings <- textDimName(word_embeddings,
      dim_names = FALSE
    )

    word_embeddings_names <- "word_embeddings"
  }

  if (!is.null(x_append)) {
    ### Sort a_append: select all Dim0 (i.e., x_append variables)
    dims0 <- target_variables_names[grep(
      "^Dim0",
      target_variables_names
    )]

    # select everything after the first "_".
    variable_names <- substring(dims0, regexpr("_", dims0) + 1)

    # Select those names from the "data"
    x_append_target <- x_append %>% dplyr::select(dplyr::all_of(variable_names))
  } else {
    variable_names <- NULL
    x_append_target <- NULL
  }

  # Adding embeddings and x_append (if any)
  new_data1 <- sorting_xs_and_x_append(
    x = word_embeddings,
    x_append = x_append_target, ...
  )
  new_data1 <- new_data1$x1

  # Dealing with NAs
  new_data1$id_nr <- c(seq_len(nrow(new_data1)))
  new_data1 <- new_data1[complete.cases(new_data1), ]
  new_data_id_nr_col <- tibble::as_tibble_col(seq_len(nrow(new_data1)), column_name = "id_nr")

  #### Load prepared_with_recipe
  data_prepared_with_recipe <- recipes::bake(model_info$final_recipe, new_data1)

  # Get column names to be removed
  colnames_to_b_removed <- colnames(data_prepared_with_recipe)
  colnames_to_b_removed <- colnames_to_b_removed[!colnames_to_b_removed == "id_nr"]

  # Get Prediction scores help(arrange)
  predicted_scores2 <- data_prepared_with_recipe %>%
    dplyr::bind_cols(stats::predict(model_info$final_model, new_data = new_data1, type = type)) %>% # , ...
    dplyr::select(-!!colnames_to_b_removed) %>%
    dplyr::full_join(new_data_id_nr_col, by = "id_nr") %>%
    dplyr::arrange(id_nr) %>%
    dplyr::select(-id_nr)

  we_names <- paste(word_embeddings_names, collapse = "_", sep = "")
  v_names <- paste(variable_names, collapse = "_", sep = "")

  y_name <- model_info$model_description[3]
  y_name <- gsub("[[:space:]]", "", y_name)
  y_name <- gsub("y=", "", y_name)

  colnames(predicted_scores2) <- paste(we_names, "_", v_names, "_", y_name, "pred", sep = "")

  return(predicted_scores2)
}


#' Predict from several models, selecting the correct input
#' @param models Object containing several models.
#' @param word_embeddings List of word embeddings (if using word embeddings from more than one
#' text-variable use dim_names = TRUE throughout the pipeline).
#' @param x_append A tibble/dataframe with additional variables used in the training of the models (optional).
#' @param ...  Settings from textPredict.
#' @return A tibble with predictions.
#' @examples
#' \donttest{
#' # x <- Language_based_assessment_data_8[1:2, 1:2]
#' # word_embeddings_with_layers <- textEmbedLayersOutput(x, layers = 11:12)
#' }
#' @seealso see \code{\link{textPredict}} and \code{\link{textTrain}}
#' @importFrom dplyr bind_cols select all_of
#' @export
textPredictAll <- function(models,
                           word_embeddings,
                           x_append = NULL,
                           ...) {
  output_predictions <- list()

  # If textTrain has created many models at the same time, select them from "all_output".
  if (!is.null(models$all_output)) {
    models <- models$all_output
  }

  # Remove singlewords_we if it exist
  if (!is.null(word_embeddings$singlewords_we)) {
    word_embeddings$singlewords_we <- NULL
  }

  # i = 1
  for (i in seq_len(length(models))) {
    preds <- textPredict(
      models[[i]],
      word_embeddings,
      x_append, ...
    )

    output_predictions[[i]] <- preds
  }
  output_predictions1 <- dplyr::bind_cols(output_predictions)
  return(output_predictions1)
}




textPredictTest()
test
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
#' @param method Set "t-test" if comparing predictions from models that predict the SAME outcome.
#' Set "bootstrap" if comparing predictions from models that predict DIFFERENT outcomes or comparison from logistic regression computing AUC distributions.
#' @param statistic If method is set to bootstrap choose "correlation or "auc" depending model training.
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
#boot_test <- textPredictTest(y1, y2, yhat1, yhat2, statistic = auc, paired = TRUE, bootstraps_times = 10)
#' @seealso see \code{\link{textTrain}} \code{\link{textPredict}}
#' @importFrom stats t.test cor
#' @importFrom tibble is_tibble as_tibble_col
#' @importFrom tidyr unnest
#' @importFrom dplyr select mutate
#' @importFrom overlapping overlap
#' @importFrom rsample analysis bootstraps
#' @importFrom yardstick auc
#' @export
textPredictTest <- function(y1,
                            y2,
                            yhat1,
                            yhat2,
                            method = "bootstrap",
                            statistic = "auc",
                            paired = TRUE,
                            bootstraps_times = 1000,
                            seed = 6134,
                            ...) {

  ## If comparing predictions from models that predict the SAME outcome
  if (method == "t-test") {
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

  if (method == "bootstrap") {
    set.seed(seed)
    # Bootstrap data to create distribution of correlations; help(bootstraps)

    # Correlation function
    if(statistic == "correlation"){
      stats_on_bootstrap <- function(split) {
        stats::cor(rsample::analysis(split)[[1]],
                   rsample::analysis(split)[[2]])
      }
    }


    #auc function
    if(statistic == "auc"){

      stats_on_bootstrap <- function(split) {
        yardstick::roc_auc_vec(as_factor(rsample::analysis(split)[[1]]),
                               rsample::analysis(split)[[2]], event_level = "second")
      }
    }


    # Creating correlation distribution for y1 and yhat1
    y_yhat1_df <- tibble::tibble(y1, yhat1)
    boots_y1 <- rsample::bootstraps(y_yhat1_df,
                                    times = bootstraps_times,
                                    apparent = FALSE)

    boot_corrss_y1 <- boots_y1 %>%
      dplyr::mutate(corr_y1 = purrr::map(splits, stats_on_bootstrap))



    boot_y1_distribution <- boot_corrss_y1 %>%
      tidyr::unnest(corr_y1) %>%
      dplyr::select(corr_y1)

    # Creating correlation distribution for y2 and yhat2
    y_yhat2_df <- tibble::tibble(y2, yhat2)
    boots_y2 <- rsample::bootstraps(y_yhat2_df,
                                    times = bootstraps_times,
                                    apparent = FALSE)

    boot_corrss_y2 <- boots_y2 %>%
      dplyr::mutate(corr_y2 = purrr::map(splits, stats_on_bootstrap))

    boot_y2_distribution <- boot_corrss_y2 %>%
      tidyr::unnest(corr_y2) %>%
      dplyr::select(corr_y2)


    ### Examining the overlap
    x_list_dist <- list(boot_y1_distribution$corr_y1, boot_y2_distribution$corr_y2)

    output <- overlapping::overlap(x_list_dist)
    output <- list(output$OV[[1]])
    names(output) <- "overlapp_p_value"
  }
  output
}





