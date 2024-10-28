
######################################                          ######################################
###################################### Implicit motives section ######################################
######################################                          ######################################

#' Returns a tibble with values relevant for calculating implicit motives
#' @param texts Texts to predict
#' @param participant_id A column with user ids.
#' @param predicted_scores2 Predictions from textPredict.
#' @return Returns a tibble with values relevant for calculating implicit motives
#' @noRd
implicit_motives <- function(texts,
                             participant_id,
                             predicted_scores2) {

  # Create a table with the number of sentences per user
  table_uniques2 <- table(participant_id[1:length(participant_id)])


  num_persons <- length(table_uniques2)

  # Define variables
  user_id_column <- c()
  current <- 0

  # Create participant_id
  for (i in 1:num_persons) {
    current <- current + table_uniques2[[i]]
    user_id_column <- c(user_id_column, participant_id[current])
  }

  # Create dataframe
  summations <- data.frame(
    OUTCOME_USER_SUM_CLASS = numeric(num_persons),
    OUTCOME_USER_SUM_PROB = numeric(num_persons),
    wc_person_per_1000 = numeric(num_persons)
  )

  # Summarize classes and probabilities (for the first row)
  summations[1, c("OUTCOME_USER_SUM_CLASS", "OUTCOME_USER_SUM_PROB")] <- c(
    OUTCOME_USER_SUM_CLASS = sum(as.numeric(as.character(predicted_scores2[[1]][1:table_uniques2[[1]]])), na.rm = TRUE),
    OUTCOME_USER_SUM_PROB = sum(as.numeric(as.character(predicted_scores2[[3]][1:table_uniques2[[1]]])), na.rm = TRUE)
  )

  # Summarize classes and probabilities (for the rest of the rows)
  for (user_ids in 2:length(table_uniques2)) {
    start_idx <- sum(table_uniques2[1:(user_ids - 1)]) + 1
    end_idx <- sum(table_uniques2[1:user_ids])

    summations[user_ids, c("OUTCOME_USER_SUM_CLASS", "OUTCOME_USER_SUM_PROB")] <- c(
      OUTCOME_USER_SUM_CLASS = sum(as.numeric(as.character(predicted_scores2[[1]][start_idx:end_idx])), na.rm = TRUE),
      OUTCOME_USER_SUM_PROB = sum(as.numeric(as.character(predicted_scores2[[3]][start_idx:end_idx])), na.rm = TRUE)
    )
  }

  # Calculate wc_person_per_1000 (for the first row)
  summations[1, "wc_person_per_1000"] <- sum(lengths(strsplit(texts[1:table_uniques2[[1]]], " ")), na.rm = TRUE) / 1000

  # Calculate wc_person_per_1000 (for the rest of the rows)
  for (user_ids in 2:length(table_uniques2)) {
    # must start on index of the next user, therefore +1
    start_idx <- sum(table_uniques2[1:(user_ids - 1)]) + 1
    end_idx <- sum(table_uniques2[1:user_ids])

    summations[user_ids, "wc_person_per_1000"] <- sum(lengths(strsplit(texts[start_idx:end_idx], " ")), na.rm = TRUE) / 1000
  }

  summations["participant_id"] <- user_id_column

  return(summations)
}

#' implicit_motives_pred returns residuals from robust linear regression.
#' @param sqrt_implicit_motives Tibble returned from function implicit_motives.
#' @param participant_id list of participant id:s.
#' @param story_id list of story id:s.
#' @return implicit_motives_pred returns residuals from robust linear regression.
#' @noRd
implicit_motives_pred <- function(sqrt_implicit_motives,
                                  participant_id,
                                  story_id) {

  # square root transform
  sqrt_implicit_motives[c("OUTCOME_USER_SUM_CLASS", "OUTCOME_USER_SUM_PROB", "wc_person_per_1000")] <- sqrt(sqrt_implicit_motives[c("OUTCOME_USER_SUM_CLASS", "OUTCOME_USER_SUM_PROB", "wc_person_per_1000")])

  # for OUTCOME_USER_SUM_PROB
  lm.OUTCOME_USER_SUM_PROB <- stats::lm(OUTCOME_USER_SUM_PROB ~ wc_person_per_1000, data = sqrt_implicit_motives)
  OUTCOME_USER_SUM_PROB.residual1 <- resid(lm.OUTCOME_USER_SUM_PROB)
  OUTCOME_USER_SUM_PROB.residual1.z <- scale(OUTCOME_USER_SUM_PROB.residual1)

  # for OUTCOME_USER_SUM_CLASS
  lm.OUTCOME_USER_SUM_CLASS <- stats::lm(OUTCOME_USER_SUM_CLASS ~ wc_person_per_1000, data = sqrt_implicit_motives)
  OUTCOME_USER_SUM_CLASS.residual1 <- resid(lm.OUTCOME_USER_SUM_CLASS)
  OUTCOME_USER_SUM_CLASS.residual1.z <- scale(OUTCOME_USER_SUM_CLASS.residual1)

  # insert residuals into a tibble
  if (identical(story_id, participant_id)){

    if (length(participant_id) < 30) {
      story_prob <- sqrt_implicit_motives$OUTCOME_USER_SUM_PROB / sqrt_implicit_motives$wc_person_per_1000
    } else {
      story_prob <- as.vector(OUTCOME_USER_SUM_PROB.residual1.z)
    }

    implicit_motives_pred <- tibble::tibble(
      story_id = sqrt_implicit_motives$participant_id,
      story_prob = story_prob,
      story_class = ifelse(is.na(as.vector(OUTCOME_USER_SUM_CLASS.residual1.z)), 0, as.vector(OUTCOME_USER_SUM_CLASS.residual1.z)),
      story_prob_no_wc_correction  = sqrt_implicit_motives$OUTCOME_USER_SUM_PROB,
      story_class_no_wc_correction  = sqrt_implicit_motives$OUTCOME_USER_SUM_CLASS)


  } else {
    # Determine the person_prob vector before creating the tibble
    if (length(sqrt_implicit_motives$participant_id) < 30) {
      person_prob <- sqrt_implicit_motives$OUTCOME_USER_SUM_PROB / sqrt_implicit_motives$wc_person_per_1000
    } else {
      person_prob <- as.vector(OUTCOME_USER_SUM_PROB.residual1.z)
    }

    implicit_motives_pred <- tibble::tibble(
      participant_id = sqrt_implicit_motives$participant_id,
      person_prob = person_prob,
      person_class = ifelse(is.na(as.vector(OUTCOME_USER_SUM_CLASS.residual1.z)),
                            0, as.vector(OUTCOME_USER_SUM_CLASS.residual1.z)),
      person_prob_no_wc_correction = sqrt_implicit_motives$OUTCOME_USER_SUM_PROB,
      person_class_no_wc_correction = sqrt_implicit_motives$OUTCOME_USER_SUM_CLASS)
  }

  if (length(participant_id) < 30) {
    warning("Warning: implicit motive scores were corrected for word count by 'score/(word count/1000)' and not residualised from a regression. This is because the number of datapoints was less than 30.")
  }

  return(implicit_motives_pred)
}

#' Separates text sentence-wise and adds additional sentences to new rows with correpsonding participant_id:s and story_id:s if provided.
#' @param df Dataframe with three columns, user_id, story_id and texts.
#' @return Returns a tibble with user_id:s and texts, where each user_id is matched to an individual sentence.
#' @noRd
update_user_and_texts <- function(df) {
  updated_user_id <- integer()
  updated_texts <- character()
  updated_story_id <- integer()
  include_story_id <- "story_id" %in% names(df)

  # check if story_id column exists
  has_story_id <- "story_id" %in% names(df)

  for (i in seq_along(df$participant_id)) {
    sentences <- stringi::stri_split_regex(df$texts[i],
                                           pattern = "(?<!\\bMr|\\bMrs|\\bMiss)[.!?]",
                                           simplify = TRUE)

    sentences <- sentences[sentences != ""]

    current_user_id <- rep(df$participant_id[i], length(sentences))
    current_texts <- sentences

    if (has_story_id) {
      current_story_id <- rep(df$story_id[i], length(sentences))
    }

    split_indices <- sapply(current_texts, function(sentence) {
      length(unlist(stringi::stri_split(sentence, regex = "\\s+"))) > 2
    })

    updated_user_id <- c(updated_user_id, rep(df$participant_id[i], sum(split_indices)))
    updated_texts <- c(updated_texts, current_texts[split_indices])

    if (has_story_id) {
      updated_story_id <- c(updated_story_id, rep(df$story_id[i], sum(split_indices)))
    }
  }
  if (include_story_id) {
    updated_df <- data.frame(participant_id = updated_user_id, story_id = updated_story_id, texts = updated_texts)
  } else {
    updated_df <- data.frame(participant_id = updated_user_id, texts = updated_texts)
  }

  # adjusted handling for missing rows
  missing_participant_rows <- setdiff(df$participant_id, updated_df$participant_id)
  if (length(missing_participant_rows) > 0) {
    if (include_story_id) {
      extra_rows <- data.frame(participant_id = missing_participant_rows, story_id = rep(NA, length(missing_participant_rows)), texts = rep("", length(missing_participant_rows)))
    } else {
      extra_rows <- data.frame(participant_id = missing_participant_rows, texts = rep("", length(missing_participant_rows)))
    }
    updated_df <- rbind(updated_df, extra_rows)
  }

  return(updated_df)
}

#' Function that binds predictions to their original dataset
#' @param data Dataset, ex csv file
#' @param predictions Predictions from textPredict as a vector
#' @return Returns the original dataset with predictions included.
#' @noRd
bind_predictions <- function(data,
                             predictions) {
  predictions <- tibble::as_tibble(predictions)

  row_diff <- nrow(data) - nrow(predictions)

  if (row_diff > 0) {
    # if data has more rows, add NA rows to predictions
    na_predictions <- tibble::as_tibble(matrix(NA, nrow = row_diff, ncol = ncol(predictions)))
    colnames(na_predictions) <- colnames(predictions)
    predictions <- rbind(predictions, na_predictions)
  } else if (row_diff < 0) {
    # if predictions have more rows, add NA rows to data
    row_diff <- abs(row_diff)
    na_data <- tibble::as_tibble(matrix(NA, nrow = row_diff, ncol = ncol(data)))
    colnames(na_data) <- colnames(data)
    data <- rbind(data, na_data)
  }

  # bind the columns
  return(dplyr::bind_cols(data, predictions))
}

#' Function that binds predictions to their original dataset
#' @param original_data Dataset, ex csv file
#' @param prediction_list Predictions from textPredict as a list
#' @return Returns the original dataset with predictions included.
#' @noRd
bind_data <- function(original_data,
                      prediction_list) {
  # copy of original_data
  result_data <- original_data

  # iterate through each "prediction"
  for (i in seq_along(prediction_list)) {
    predictions <- prediction_list[[i]]

    # rename columns in predictions to ensure they are unique
    col_names <- names(predictions)
    unique_col_names <- paste0(col_names, "_", i)
    names(predictions) <- unique_col_names

    # add a separator column with NA values before binding the new predictions
    separator_col_name <- paste0("separator_col_", i)
    result_data[[separator_col_name]] <- NA

    # bind the predictions with the result_data
    result_data <- bind_predictions(result_data, predictions)
  }

  return(result_data)
}

#' Wrapper function that prepares the data and returns a list with predictions, class residuals and probability residuals.
#' @param model_reference Reference to implicit motive model, either github URL or file-path.
#' @param participant_id A column with user ids.
#' @param story_id list of story-ids.
#' @param predicted_scores2 Predictions from textPredict() function.
#' @param texts Texts to predict from textPredict() function.
#' @param dataset your dataset.
#' @param lower_case_model character name of your model.
#' @return Returns a tibble with values relevant for calculating implicit motives
#' @noRd
implicit_motives_results <- function(model_reference,
                                     participant_id,
                                     story_id,
                                     predicted_scores2,
                                     texts,
                                     dataset,
                                     lower_case_model) {


  #### Assign correct column name ####
  if (grepl("implicit", lower_case_model) & grepl("power", lower_case_model)) {
    column_name <- "power"
  } else if (grepl("implicit", lower_case_model) & grepl("affiliation", lower_case_model)) {
    column_name <- "affiliation"
  } else if (grepl("implicit", lower_case_model) & grepl("achievement", lower_case_model)) {
    column_name <- "achievement"
  } else if (model_reference == "implicit_motives") {
    column_name <- model_reference
  }

  ##  if (length(texts) != length(participant_id)) {
  ##    stop("texts and participant_id must be of same length.")
  ##  }


  if(!is.null(participant_id)){
    # Retrieve Data
    #participant_id <- 1:80
    implicit_motives <- implicit_motives(texts, participant_id, predicted_scores2)

    # Predict
    predicted <- implicit_motives_pred(sqrt_implicit_motives = implicit_motives,
                                       participant_id = participant_id,
                                       story_id = story_id)

  }

  # set default to NULL
  predicted_story <- NULL

  # if both story_id and participant_id are defined, then also create story-level predictions.
  if (!is.null(story_id) && !is.null(participant_id)){
    # The algorithm treats participant_id and story_id the same, but was origionally created to only handle
    # participant id:s. A solution is therefore to assign the story:ids to participant_id.
    participant_id_placeholder <- story_id

    implicit_motives_story <- implicit_motives(texts, participant_id_placeholder, predicted_scores2)

    predicted_story <- implicit_motives_pred(sqrt_implicit_motives = implicit_motives_story,
                                             participant_id = participant_id_placeholder,
                                             story_id = story_id)

  }

  # Full column name
  class_col_name <- paste0(column_name, "_class")

  # Change column name in predicted_scores2
  colnames(predicted_scores2)[1] <- class_col_name

  # Change from df to tibble
  predicted_scores2 <- tibble::as_tibble(predicted_scores2)

  if (!is.null(participant_id)){

    # Two different summary lists depending on if including the dataset with integrated predictions or not
    if (is.null(dataset)) {
      if (identical(story_id, participant_id)){
        summary_list <- list(sentence_predictions = predicted_scores2,
                             story_predictions = predicted)
      } else {
        summary_list <- list(sentence_predictions = predicted_scores2,
                             person_predictions = predicted)
      }

    } else {

      if(!identical(predicted, predicted_story) && !is.null(predicted_story)){
        # include both story- and sentence-level predictions
        to_insert <- list(predicted_scores2, predicted, predicted_story)
      } else if (identical(predicted, predicted_story)){
        # include just story-level predictions
        to_insert <- list(predicted_scores2, predicted_story)
      } else {
        # predicted_scores2 = sentence predictions, predicted = person predictions
        to_insert <- list(predicted_scores2, predicted)
      }

      # old code: it was integrating ALL three datasets with the datset
      #integrated_dataset <- bind_data(dataset, to_insert)

      # new code to integrate predictions into dataset; only matching the rows
      if(nrow(dataset) == nrow(to_insert[[1]])){
        integrated_dataset <- dplyr::bind_cols(dataset, to_insert[[1]])
      }
      if(length(to_insert)>1){
        if(nrow(dataset) == nrow(to_insert[[2]])){
          integrated_dataset <- dplyr::bind_cols(dataset, to_insert[[2]])
        }
      }
      if(length(to_insert)>2){
        if(nrow(dataset) == nrow(to_insert[[3]])){
          integrated_dataset <- dplyr::bind_cols(dataset, to_insert[[3]])
        }
      }


      if (identical(story_id, participant_id)){
        # story predictions
        summary_list <- list(sentence_predictions = predicted_scores2,
                             story_predictions = predicted,
                             dataset = integrated_dataset)
      }

      if (!identical(predicted, predicted_story) && !is.null(predicted_story)){
        # story predictions
        summary_list <- list(sentence_predictions = predicted_scores2,
                             person_predictions = predicted,
                             story_predictions = predicted_story,
                             dataset = integrated_dataset)
      } else if (identical(predicted, predicted_story) && !is.null(predicted_story)){
        # just story-level predictions
        summary_list <- list(sentence_predictions = predicted_scores2,
                             story_predictions = predicted_story,
                             dataset = integrated_dataset)
      } else {
        # Summarize all predictions
        summary_list <- list(sentence_predictions = predicted_scores2,
                             person_predictions = predicted,
                             dataset = integrated_dataset)
      }
    }
  } else {

    if(!is.null(dataset)){
      predicted_scores2 <- dplyr::bind_cols(dataset, predicted_scores2)
    }

    summary_list <- predicted_scores2
  }

  # Display message to user
  cat(colourise("Predictions of implicit motives are ready!", fg = "green"))
  cat("\n")

  return(summary_list)
}

#' Function that is called in the beginning of textPredict to create the conditions for implicit motives to work.
#' @param model_info (character or r-object) model_info has three options. 1: R model object (e.g, saved output from textTrain). 2:link to github-model
#' (e.g, "https://github.com/CarlViggo/pretrained_swls_model/raw/main/trained_github_model_logistic.RDS"). 3: Path to a model stored locally (e.g, "path/to/your/model").
#' @param participant_id A column with user ids.
#' @param show_texts Show texts, TRUE / FALSE
#' @param type list of story-ids.
#' @param texts Texts to predict from textPredict() function.
#' @param story_id your dataset.
#' @param lower_case_model character name of your model.
#' @return Returns a list of conditions for implicit motive coding to work
#' @noRd
get_model_info <- function(model_info,
                           participant_id,
                           show_texts,
                           type,
                           texts,
                           story_id,
                           lower_case_model
) {
  # show_prob is by default FALSE
  show_prob <- FALSE

  if (
    (grepl("implicit", lower_case_model) & grepl("power", lower_case_model)) ||
    (grepl("implicit", lower_case_model) & grepl("affiliation", lower_case_model)) ||
    (grepl("implicit", lower_case_model) & grepl("achievement", lower_case_model)) ||
    (grepl("implicit_motives", lower_case_model) &&
     (!is.null(participant_id) || !is.null(story_id)))
  ) {

    type <- "class" # type must be class for these conditions
    # switch to the correct model URL
    if (lower_case_model == "implicit_power_roberta_large_l23_v1") {
      model_info <- "https://github.com/AugustNilsson/Implicit-motive-models/raw/main/schone_training_rob_la_l23_to_power_open.rds"
    } else if (lower_case_model == "implicit_achievement_roberta_large_l23_v1") {
      model_info <- "https://github.com/AugustNilsson/Implicit-motive-models/raw/main/schone_training_rob_la_l23_to_achievement_open.rds"
    } else if (lower_case_model == "implicit_affiliation_roberta_large_l23_v1") {
      model_info <- "https://github.com/AugustNilsson/Implicit-motive-models/raw/main/schone_training_rob_la_l23_to_affiliation_open.rds"
    } else if (lower_case_model == "implicit_ger_be_l11_to_power") {
      model_info <- "https://github.com/AugustNilsson/Implicit-motive-models/raw/main/schone_training_ger_be_l11_to_power_open.rds"
    } else if (lower_case_model == "implicit_ger_be_l11_to_achievement") {
      model_info <- "https://github.com/AugustNilsson/Implicit-motive-models/raw/main/schone_training_ger_be_l11_to_achievement_open.rds"
    } else if (lower_case_model == "implicit_ger_be_l11_to_affiliation") {
      model_info <- "https://github.com/AugustNilsson/Implicit-motive-models/raw/main/schone_training_ger_be_l11_to_affiliation_open.rds"
    }
    # specific configuration for implicit motive coding
    if (!is.null(participant_id) || !is.null(story_id)) {
      show_texts <- TRUE
      show_prob <- TRUE
      type <- "class"

      # Assign story_id to the participant_id variable (this might seem illogical, but this was a convenient
      # solution to a new problem caught along the way.
      if (is.null(participant_id)){
        participant_id <- story_id
      }

      # separate multiple sentences, and add corresponding user-id
      if (!is.null(story_id)){
        id_and_texts <- data.frame(participant_id = participant_id, texts = texts, story_id = story_id)
      } else {
        id_and_texts <- data.frame(participant_id = participant_id, texts = texts)
      }
      # correct for multiple sentences per row. # CORRECT
      update_user_and_texts <- update_user_and_texts(id_and_texts)

      # update participant_id
      participant_id <- update_user_and_texts$participant_id
      # update texts
      texts <- update_user_and_texts$texts
      # update story_id
      story_id <- update_user_and_texts$story_id
    }
  }

  # The stats package just takes "class" or "prob", therefore, allocate to "show_prob".
  if (!is.null(type) && type == "class_prob") {
    type <- "class"
    show_prob <- TRUE
  }

  return(list(model_info = model_info,
              type = type,
              show_texts = show_texts,
              show_prob = show_prob,
              type = type,
              participant_id = participant_id,
              texts = texts,
              story_id = story_id))
}



#model_info = NULL
#word_embeddings = NULL
#texts = NULL
#x_append = NULL
#type = NULL
#dim_names = TRUE
#save_model = TRUE
#threshold = NULL
#show_texts = FALSE
#device = "cpu"
#participant_id = NULL
#save_embeddings = TRUE
#save_dir = "wd"
#save_name = "textPredict"
#story_id = NULL
#dataset_to_merge_predictions = NULL
#previous_sentence = FALSE
##
#texts = PSE_stories_participant_level$stories
#model_info = "implicit_power_roberta_large_L23_v1"
##participant_id = PSE_stories_participant_level$Participant_ID
#
#model_info = "theharmonylab/implicit-motives-power-roberta-large"
##texts = Language_based_assessment_data_8[1:2,1]
#model_type = "finetuned"

#' Trained models created by e.g., textTrain() or stored on e.g., github can be used to predict
#' new scores or classes from embeddings or text using textPredict.
#' @param model_info (character or r-object) model_info has four options. 1: R model object
#' (e.g, saved output from textTrainRegression). 2: Link to a model stored in a github repo
#' (e.g, "https://github.com/CarlViggo/pretrained_swls_model/raw/main/trained_github_model_logistic.RDS").
#' 3: Link to a model stored in a osf project (e.g, https://osf.io/8fp7v).
#' 4: Path to a model stored locally (e.g, "path/to/your/model"). Information about some accessible models
#' can be found at: \href{https://r-text.org/articles/pre_trained_models.html}{r-text.org}.
#' @param word_embeddings (tibble) Embeddings from e.g., textEmbed(). If you're using a pre-trained model,
#'  then texts and embeddings cannot be submitted simultaneously (default = NULL).
#' @param texts (character) Text to predict. If this argument is specified, then arguments "word_embeddings"
#' and "pre-made embeddings" cannot be defined (default = NULL).
#' @param x_append (tibble) Variables to be appended after the word embeddings (x).
#' @param append_first If TRUE, x_appened is added before word embeddings.
# @param type (character) Defines what output to give after logistic regression prediction.
# Either probabilities, classifications or both are returned (default = "class".
# For probabilities use "prob". For both use "class_prob").
#' @param threshold (numeric) Determine threshold if you are using a logistic model (default = 0.5).
#' @param dim_names (boolean) Specifies how to handle word embedding names. If TRUE, it uses specific
#' word embedding names, and if FALSE word embeddings are changed to their generic names (Dim1, Dim2, etc).
#' If set to FALSE, the model must have been trained on word embeddings created with dim_names FALSE.
#' @param save_model (boolean) The model will by default be saved in your work-directory (default = TRUE).
#' If the model already exists in your work-directory, it will automatically be loaded from there.
#' @param save_embeddings (boolean) If set to TRUE, embeddings will be saved with a unique identifier, and
#' will be automatically opened next time textPredict is run with the same text. (default = TRUE)
#' @param save_dir (character) Directory to save embeddings. (default = "wd" (i.e, work-directory))
#' @param save_name (character) Name of the saved embeddings (will be combined with a unique identifier).
#' (default = ""). Obs: If no save_name is provided, and model_info is a character, then save_name will be set
#' to model_info.
#' @param show_texts (boolean) Show texts together with predictions (default = FALSE).
#' @param participant_id (vector; only works for implicit motives models) Vector of participant-ids. Specify this for getting person level scores
#' (i.e., summed sentence probabilities to the person level corrected for word count). (default = NULL)
#' @param story_id (vector; only works for implicit motives models) Vector of story-ids. Specify this to get story level scores (i.e., summed sentence
#' probabilities corrected for word count). When there is both story_id and participant_id indicated, the function
#' returns a list including both story level and person level prediction corrected for word count. (default = NULL)
#' @param dataset_to_merge_predictions (tibble; only works for implicit motives models) Insert your data here to integrate predictions to your dataset,
#'  (default = NULL).
#' @param previous_sentence (Boolean; only works for implicit motives models) If set to TRUE, word-embeddings will be averaged over the current and previous
#' sentence per story-id. For this, both participant-id and story-id must be specified.
#' @param device Name of device to use: 'cpu', 'gpu', 'gpu:k' or 'mps'/'mps:k' for MacOS, where k is a
#' specific device number such as 'mps:1'.
#' @param ...  Setting from stats::predict can be called.
#' @return Predictions from word-embedding or text input.
#' @examples
#' \dontrun{
#'
#' # Text data from Language_based_assessment_data_8
#' text_to_predict <- "I am not in harmony in my life as much as I would like to be."
#'
#' # Example 1: (predict using pre-made embeddings and an R model-object)
#' prediction1 <- textPredict(
#'   model_info = trained_model,
#'   word_embeddings_4$texts$satisfactiontexts
#' )
#'
#' # Example 2: (predict using a pretrained github model)
#' prediction2 <- textPredict(
#'   texts = text_to_predict,
#'   model_info = "https://github.com/CarlViggo/pretrained-models/raw/main/trained_hils_model.RDS"
#' )
#'
#' # Example 3: (predict using a pretrained logistic github model and return
#' # probabilities and classifications)
#' prediction3 <- textPredict(
#'   texts = text_to_predict,
#'   model_info = "https://github.com/CarlViggo/pretrained-models/raw/main/
#'   trained_github_model_logistic.RDS",
#'   type = "class_prob",
#'   threshold = 0.7
#' )
#'
#' # Example 4: (predict from texts using a pretrained model stored in an osf project)
#' prediction4 <- textPredict(
#'   texts = text_to_predict,
#'   model_info = "https://osf.io/8fp7v"
#' )
#' ##### Automatic implicit motive coding section ######
#'
#' # Create example dataset
#' implicit_motive_data <- dplyr::mutate(.data = Language_based_assessment_data_8,
#' participant_id = dplyr::row_number())
#'
#' # Code implicit motives.
#' implicit_motives <- textPredict(
#'   texts = implicit_motive_data$satisfactiontexts,
#'   model_info = "implicit_power_roberta_large_L23_v1",
#'   participant_id = implicit_motive_data$participant_id,
#'   dataset_to_merge_predictions = implicit_motive_data
#' )
#'
#' # Examine results
#' implicit_motives$sentence_predictions
#' implicit_motives$person_predictions
#' }
#'
#' \dontrun{
#' # Examine the correlation between the predicted values and
#' # the Satisfaction with life scale score (pre-included in text).
#'
#' psych::corr.test(
#'   predictions1$word_embeddings__ypred,
#'   Language_based_assessment_data_8$swlstotal
#' )
#' }
#' @seealso See \code{\link{textTrain}}, \code{\link{textTrainLists}} and
#' \code{\link{textTrainRandomForest}}.
#' @importFrom recipes prep bake
#' @importFrom stats predict
#' @importFrom tibble is_tibble as_tibble_col
#' @importFrom dplyr bind_cols select full_join arrange everything
#' @importFrom magrittr %>%
#' @noRd
textPredictImplicitMotives <- function(
    model_info = NULL,
    word_embeddings = NULL,
    texts = NULL,
    x_append = NULL,
    append_first = TRUE,
    threshold = NULL,
    dim_names = TRUE,
    save_model = TRUE,
    save_embeddings = TRUE,
    save_dir = "wd",
    save_name = "textPredict",
    show_texts = FALSE,
    participant_id = NULL,
    story_id = NULL,
    dataset_to_merge_predictions = NULL,
    previous_sentence = FALSE,
    device = "cpu",
    ...) {

  use_row_id_name = FALSE

  #### Special treatment for implicit motives - see private functions ####
  model_name <- model_info

  lower_case_model <- as.character(tolower(model_name))

  if (is.null(participant_id) & is.null(story_id)){
      cat(colourise("Note: The 'texts' were not at the sentence level and dataset_to_merge_predictions was provided but not participant_id and story_id. The function treated each row_id as participnat_id for merging assessments into dataset_to_merge_predictions",
                    "purple"))
  }

  # Create participant id to enable creation of sentence level predictions (not sure why this is needed)
  if (is.null(participant_id)){
      use_row_id_name <- TRUE
      participant_id <- seq_len(length(texts))
      cat(colourise("Note: participant_ID was not provided so treating rows as row_id", "purple"))
  }


  # get_model_info retrieves the particular configurations that are needed for automatic implicit motive coding
  get_model_info_results <- get_model_info(model_info = model_info,
                                           participant_id = participant_id,
                                           show_texts = show_texts,
                                           #type = type,
                                           texts = texts,
                                           story_id = story_id,
                                           lower_case_model = lower_case_model)


  model_info <- get_model_info_results$model_info

  # type <- get_model_info_results$type
  texts <- get_model_info_results$texts
  participant_id <- get_model_info_results$participant_id
  story_id = get_model_info_results$story_id


  if(model_type == "texttrained"){

      predicted_scores2 <- textPredictTextTrained(
        model_info = model_info,
        word_embeddings = word_embeddings,
        texts = texts,
        x_append = x_append,
        append_first = append_first,
        threshold = threshold,
        dim_names = dim_names,
        save_model = save_model,
        save_embeddings = save_embeddings,
        save_dir = save_dir,
        save_name = save_name,
        show_texts = show_texts,
        participant_id = participant_id,
        story_id = story_id,
        dataset_to_merge_predictions = dataset_to_merge_predictions,
        previous_sentence = previous_sentence,
        device = device
      )
  }

  if(model_type == "finetuned"){

    predicted_scores2 <- textClassifyPipe(
      x = texts,
      model = model_info,
      device = "cpu",
      tokenizer_parallelism = FALSE,
      logging_level = "error",
      force_return_results = TRUE,
      return_all_scores = FALSE,
      function_to_apply = NULL,
      set_seed = 202208
    )
    # Label, .pred_0, pred_1
    colnames(predicted_scores2) <- c("Label", ".pred_0")
    predicted_scores2$.pred_1 <- 1-predicted_scores2$.pred_0
  }

  # Include text in predictions
  if (show_texts) {
    predicted_scores2 <- predicted_scores2 %>%
      dplyr::mutate(texts = texts)
  }

  # Wrapper function that prepares data for automatic implicit motive coding and returns
  # a list with predictions, class residuals and probability residuals.
  predicted_scores2 <- implicit_motives_results(
      model_reference = model_info,
      participant_id = participant_id,
      story_id = story_id,
      predicted_scores2 = predicted_scores2,
      texts = texts,
      dataset = dataset_to_merge_predictions,
      lower_case_model = lower_case_model
    )

  # change participant_id to row_id
  if(use_row_id_name){
      colnames(predicted_scores2[[2]])[colnames(predicted_scores2[[2]]) == "participant_id"] <- "row_id"
    }

  # display message to user
  cat(colourise("Predictions are ready!", fg = "green"))
  cat("\n")
  return(predicted_scores2)
}
