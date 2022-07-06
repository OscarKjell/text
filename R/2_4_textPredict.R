
#' Predict scores or classification from, e.g., textTrain.
#'
#' @param model_info Model info (e.g., saved output from textTrain, textTrainRegression or textRandomForest).
#' @param new_data Word embeddings from new data to be predicted from.
#' @param  type Type of prediction; e.g., "prob", "class".
#' @param ... Setting from stats::predict can be called.
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


#' Predict from several models, selecting the correct input
#' @param models Object containing several models.
#' @param word_embeddings List of word embeddings (if using word embeddings from more than one
#' text-variable use dim_names = TRUE throughout the pipeline).
#' @param data A tibble/dataframe with additional variables used in the training of the models (optional).
#' @return A tibble with predictions.
#' @examples
#' \donttest{
#' # x <- Language_based_assessment_data_8[1:2, 1:2]
#' # word_embeddings_with_layers <- textEmbedLayersOutput(x, layers = 11:12)
#' }
#' @seealso see \code{\link{textPredict}} and \code{\link{textTrain}}
#' @importFrom dplyr bind_cols select all_of
textPredictAll <- function(models, word_embeddings, data){

  output_predictions <- list()

  # If textTrain has created many models at the same time, select them from "all_output".
  if(!is.null(models$all_output)){
    models <- models$all_output
  }

  all_embeddings <- dplyr::bind_cols(word_embeddings)
  # i = 1
  for(i in 1:length(models)){

    # Select the predictor variables needed for the prediction
    target_variables_names <- models[[i]]$final_recipe$var_info$variable[models[[i]]$final_recipe$var_info$role == "predictor"]

    # select all Dim0
    dims0 <- target_variables_names[grep("^Dim0",
                                         target_variables_names)]

    # select everything after the first _
    v_colnames <- substring(dims0, regexpr("_", dims0) + 1)

    # Select those names from the "data"
    x_variables <- data %>% dplyr::select(dplyr::all_of(v_colnames))

    # Change the name to include Dim01_
    variables_embeddings <- add_variables_to_we(all_embeddings,
                                                x_variables)

    input_x <- variables_embeddings %>%
      dplyr::select(dplyr::all_of(target_variables_names))


    preds <- textPredict(models[[i]],
                         input_x)$.pred

    pred1 <- as_tibble(preds)
    colnames(pred1) <- paste(names(word_embeddings[i]), "_pred", sep="")
    output_predictions[[i]] <- pred1
  }
  output_predictions1 <- dplyr::bind_cols(output_predictions)
  return(output_predictions1)
}







