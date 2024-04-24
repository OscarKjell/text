# A helper function to textPredict giving it the capabilities of textPredictEntireProcedure.
#' @param texts (character) Text to predict. If this argument is specified, then argument
#' "premade_embeddings" must be set to NULL (default = NULL).
#' @param word_embeddings (Embeddings from e.g., textEmbed) Embeddings to predict. If
#' this argument is specified, then argument "texts" must be set to NULL (default = NULL).
#' @param model_info (character or r-object) model_info has three options. 1: R model object
#' (e.g, saved output from textTrain). 2:link to github-model
#' (e.g, "https://github.com/CarlViggo/pretrained_swls_model/raw/main/trained_github_model_logistic.RDS").
#' 3: Path to a model stored locally (e.g, "path/to/your/model").
#' @param save_model (boolean) The model will by default be saved in work directory (deafult = TRUE).
#' @param type (character) Choose either 'class' or 'prob'. If your model is a logistic or multinomial
#'  model, specify whether you want to receive the
#' model's classification "class" or the underlying probabilities "prob" (default = "class").
#' @param max_token_to_sentence (numeric) This information will be automatically extracted from your
#' model, so this argument is typically not used.
#' However, if it's not defined in your model_description, you can assign a value here (default = 4).
#' @param aggregation_from_layers_to_tokens (character) This information will be automatically extracted
#' from your model, so this argument is typically not used.
#' However, if it's not defined in your model_description, you can assign a value here (default = "concatenate").
#' @param aggregation_from_tokens_to_texts (character) This information will be automatically
#' extracted from your model, so this argument is typically not used.
#' @param save_embeddings (boolean) If set to TRUE, embeddings will be saved with a unique identifier, and
#' will be automatically opened next time textPredict is run with the same text. (default = TRUE)
#' @param save_dir (character) Directory to save embeddings. (default = "wd" (i.e, work-directory))
#' @param save_name (character) Name of the saved embeddings (will be combined with a unique identifier).
#' (default = "textPredict").
#' @param previous_sentence If set to TRUE, word-embeddings will be averaged over the current and previous
#' sentence per story-id. For this, both participant-id and story-id must be specified.
#' @noRd
textReturnModelAndEmbedding <- function(
    texts = NULL,
    word_embeddings = NULL,
    model_info = NULL,
    save_model = TRUE,
    type = "class",
    device = "cpu",
    story_id = NULL,
    save_embeddings = TRUE,
    save_dir = "wd",
    save_name = "textPredict",
    previous_sentence = FALSE) {
  # extend the timeout time for larger models.
  options(timeout = 5 * 60)

  # diaplay message to user
  cat(colourise("Loading model...", fg = "red"))
  cat("\n")

  # extract model_name if its a url or filepath
  if (is.character(model_info)) {
    model_name <- basename(model_info)
    # find model in wd
    model_exists <- file.exists(model_name)

    # determine how to load model
    if (grepl("github.com/", model_info) && isFALSE(model_exists) && isTRUE(save_model)) {
      # load from github
      loaded_model <- readRDS(url(model_info))
      # save model
      saveRDS(loaded_model, model_name)

      # display message to user
      loaded_model_confirm <- paste0(c("The model:", model_name, "has been loaded and saved in:", getwd()), sep = "")
      cat(colourise(loaded_model_confirm, fg = "green"))
      cat("\n")
    } else if (grepl("github.com/", model_info) && isFALSE(model_exists) && isFALSE(save_model)) {
      # load from github, don't save
      loaded_model <- readRDS(url(model_info))

      # display message to user
      loaded_model_confirm <- paste0(c("The model:", model_name, "has been loaded from:", model_info), sep = "")
      cat(colourise(loaded_model_confirm, fg = "green"))
      cat("\n")
    } else if (grepl("github.com/", model_info) && isTRUE(model_exists)) {
      # retrive model from wd if it's already downloaded
      loaded_model <- readRDS(model_name)

      # display message to user
      loaded_model_confirm <- paste0(c("The model:", model_name, "has been loaded from:", getwd()), sep = "")
      cat(colourise(loaded_model_confirm, fg = "green"))
      cat("\n")
    } else {
      # load model from specific path (if it exists somewhere else than in the work directory)
      loaded_model <- readRDS(model_info)

      # display message to user
      loaded_model_confirm <- paste0(c("The model:", model_name, "has been loaded from:", model_info), sep = "")
      cat(colourise(loaded_model_confirm, fg = "green"))
      cat("\n")
    }
  } else {
    # model was an R object of a model
    loaded_model <- model_info
    # display message to user
    loaded_model_confirm <- paste0(c("The model has been loaded from your global environment."), sep = "")
    cat(colourise(loaded_model_confirm, fg = "green"))
    cat("\n")
  }

  # Check that both texts and word_embeddings aren't defined.
  if (!is.null(texts) && !is.null(word_embeddings)) {
    stop('Both arguments: "texts" and "word_embeddings" cannot be defined simultaneously. Choose one or the other.')
  }

  ###### Create or find embeddings based on information stored in the pre-trained model ######

  # extract the model descriptors
  line_number <- grep("impute_missing_setting", loaded_model$model_description)
  new_line_number <- line_number + 1
  input_string <- loaded_model$model_description[new_line_number]

  hash1 <- simple_hash(as.vector(input_string))
  # prepare to check if embeddings already exists
  hash2 <- simple_hash(as.vector(texts))
  file_name <- paste0(save_name,"_", hash1, hash2, ".RDS")
  save_directory <- ifelse(save_dir == "wd", "", save_dir)

  # create a full path, and check if save_dir is "" or not.
  if (nzchar(save_directory)) {
    full_path <- file.path(save_directory, file_name)
  } else {
    #In this case, save_dir is ""
    full_path <- file_name
  }

  if (!is.null(texts) &&
      is.null(word_embeddings) &&
      isTRUE(file.exists(full_path))){

      # get embeddings
      embeddings <- readRDS(full_path)

      cat(colourise(paste0("Embeddings have been loaded from: ", full_path), fg = "green"))
      cat("\n")

  } else if(!is.null(texts) &&
            is.null(word_embeddings) &&
            isFALSE(file.exists(full_path))){

    # Save default values for later use
    default_max_token_to_sentence <- 4
    default_aggregation_from_layers_to_tokens <- "concatenate"
    default_aggregation_from_tokens_to_texts <- "mean"

    # Finds the line number for the line with "impute_missing_setting" (the line above the
    # line we're looking for, which is the model description)
    line_number <- grep("impute_missing_setting", loaded_model$model_description)
    new_line_number <- line_number + 1

    # Extracts information about model type and layers.
    model_type <- extract_comment(loaded_model$model_description[new_line_number], "model")
    model_layers <- as.numeric(extract_comment(loaded_model$model_description[new_line_number], "layers"))

    # Extracts the max_token_to_sentence, aggregation_from_layers_to_tokens,
    # and aggregation_from_tokens_to_texts from the model.
    input_string <- loaded_model$model_description[new_line_number]

    max_token_to_sentence <- as.numeric(sub(".*max_token_to_sentence: (\\d+).*", "\\1", input_string))

    aggregation_from_layers_to_tokens <- sub(".*aggregation_from_layers_to_tokens = (\\S+).*", "\\1", input_string)

    aggregation_from_tokens_to_texts <- sub(".*aggregation_from_tokens_to_texts = (\\S+).*", "\\1", input_string)

    # Check if the variables match input_string and assign the default values
    if (max_token_to_sentence == input_string) {
      max_token_to_sentence <- default_max_token_to_sentence
    }

    if (aggregation_from_layers_to_tokens == input_string) {
      aggregation_from_layers_to_tokens <- default_aggregation_from_layers_to_tokens
    }

    if (aggregation_from_tokens_to_texts == input_string) {
      aggregation_from_tokens_to_texts <- default_aggregation_from_tokens_to_texts
    }
    # Create embeddings based on the extracted information from the model.
    embeddings <- textEmbed(
      texts = texts,
      model = model_type,
      layers = model_layers,
      max_token_to_sentence = max_token_to_sentence,
      aggregation_from_layers_to_tokens = aggregation_from_layers_to_tokens,
      aggregation_from_tokens_to_texts = aggregation_from_tokens_to_texts,
      device = device,
      keep_token_embeddings = FALSE
    )

    # save embeddings if save_embeddings is set to true
    if (isTRUE(save_embeddings) && save_dir == "wd"){
      saveRDS(embeddings, file_name)
      cat(colourise(paste0("Embeddings have been saved: ", full_path), fg = "green"))
      cat("\n")
    } else if (isTRUE(save_embeddings) && save_dir != "wd"){
      saveRDS(embeddings, full_path)
      cat(colourise(paste0("Embeddings have been saved: ", full_path), fg = "green"))
      cat("\n")
    }

  } else if (!is.null(word_embeddings) && is.null(texts)) {
    # If text isn't provided, but premade word-embeddings, then load them instead.
    embeddings <- word_embeddings
  }

  ####### Special treatment for implicit motives ######

  # Calculate the average of the current and the next word_embedding per story_id
  if (isTRUE(previous_sentence)) {
    T1_story_id <- Sys.time()

    embeddings$texts$texts$story_id <- as.numeric(as.factor(story_id))

    # calculate the running average
    running_avg <- function(x) {
      c(x[1], (x[-1] + x[-length(x)]) / 2)
    }

    # Apply the running average function to each embedding column by story_id
    embeddings$texts$texts <- dplyr::group_by(embeddings$texts$texts, story_id) %>%
      dplyr::mutate(
        dplyr::across(
          dplyr::starts_with("Dim"),
          running_avg)) %>%
      dplyr::ungroup()

    # Ungroup (remove the storyid column)
    embeddings$texts$texts <- dplyr::ungroup(embeddings$texts$texts)

    T2_story_id <- Sys.time()
    Time_story_id <- T2_story_id - T1_story_id
    Time_story_id <- sprintf("Completed word-embedding concatenation per story-id. Duration: %f %s", Time_story_id, units(Time_story_id))
    cat(colourise(Time_story_id, fg = "green"))
    cat("\n")
  }

  ##### End special treatment for automatic implicit motive coding #####

  # store classes
  #classes <- loaded_model$final_recipe$levels$y$values
  classes <- loaded_model$final_model$pre$actions$recipe$recipe$orig_lvls$y$values

  emb_and_model <- list(loaded_model = loaded_model, embeddings = embeddings, classes = classes)
  return(emb_and_model)
}


#' Trained models created by e.g., textTrain() or stored on e.g., github can be used to predict
#' new scores or classes from embeddings or text using textPredict.
#' @param model_info (character or r-object) model_info has three options. 1: R model object
#' (e.g, saved output from textTrain). 2:link to github-model
#' (e.g, "https://github.com/CarlViggo/pretrained_swls_model/raw/main/trained_github_model_logistic.RDS").
#' 3: Path to a model stored locally (e.g, "path/to/your/model"). Information about accessble models
#' can be found at: \href{https://r-text.org/articles/pre_trained_models.html}{r-text.org}.
#' @param word_embeddings (tibble) Embeddings from e.g., textEmbed(). If you're using a pre-trained model,
#'  then texts and embeddings cannot be submitted simultaneously (default = NULL).
#' @param x_append (tibble) Variables to be appended after the word embeddings (x).
#' @param type (character) Defines what output to give after logistic regression prediction.
#' Either probabilities, classifications or both are returned (default = "class".
#' For probabilities use "prob". For both use "class_prob").
#' @param dim_names (boolean) Account for specific dimension names from textEmbed()
#' (rather than generic names including Dim1, Dim2 etc.). If FALSE the models need to have been trained on
#' word embeddings created with dim_names FALSE, so that embeddings were only called Dim1, Dim2 etc.
#' @param texts (character) Text to predict. If this argument is specified, then arguments "word_embeddings"
#' and "premade embeddings" cannot be defined (default = NULL).
#' @param save_model (boolean) The model will by default be saved in your work-directory (default = TRUE).
#' If the model already exists in your work-directory, it will automatically be loaded from there.
#' @param threshold (numeric) Determine threshold if you are using a logistic model (default = 0.5).
#' @param show_texts (boolean) Show texts together with predictions (default = FALSE).
#' @param device Name of device to use: 'cpu', 'gpu', 'gpu:k' or 'mps'/'mps:k' for MacOS, where k is a
#' specific device number such as 'mps:1'.
#' @param participant_id (list) Vector of participant-ids. Specify this for getting person level scores
#' (i.e., summed sentence probabilities to the person level corrected for word count). (default = NULL)
#' @param story_id (vector) Vector of story-ids. Specify this to get story level scores (i.e., summed sentence
#' probabilities corrected for word count). When there is both story_id and participant_id indicated, the function
#' returns a list including both story level and person level prediction corrected for word count. (default = NULL)
#' @param dataset_to_merge_predictions (R-object, tibble) Insert your data here to integrate predictions to your dataset,
#'  (default = NULL).
#' @param save_embeddings (boolean) If set to TRUE, embeddings will be saved with a unique identifier, and
#' will be automatically opened next time textPredict is run with the same text. (default = TRUE)
#' @param save_dir (character) Directory to save embeddings. (default = "wd" (i.e, work-directory))
#' @param save_name (character) Name of the saved embeddings (will be combined with a unique identifier).
#' (default = ""). Obs: If no save_name is provided, and model_info is a character, then save_name will be set
#' to model_info.
#' @param previous_sentence If set to TRUE, word-embeddings will be averaged over the current and previous
#' sentence per story-id. For this, both participant-id and story-id must be specified.
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
#' prediction3 <- textPredict(
#'   texts = text_to_predict,
#'   model_info = "https://github.com/CarlViggo/pretrained-models/raw/main/trained_hils_model.RDS"
#' )
#'
#' # Example 3: (predict using a pretrained logistic github model and return
#' # probabilities and classifications)
#' prediction4 <- textPredict(
#'   texts = text_to_predict,
#'   model_info = "https://github.com/CarlViggo/pretrained-models/raw/main/
#'   trained_github_model_logistic.RDS",
#'   type = "class_prob",
#'   threshold = 0.7
#' )
#'
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
#' @export
textPredict <- function(model_info = NULL,
                        word_embeddings = NULL,
                        texts = NULL,
                        x_append = NULL,
                        type = NULL,
                        dim_names = TRUE,
                        save_model = TRUE,
                        threshold = NULL,
                        show_texts = FALSE,
                        device = "cpu",
                        participant_id = NULL,
                        save_embeddings = TRUE,
                        save_dir = "wd",
                        save_name = "textPredict",
                        story_id = NULL,
                        dataset_to_merge_predictions = NULL,
                        previous_sentence = FALSE,
                        ...) {
  # Stop message if user defines both word_embeddings and texts
  if (!is.null(texts) && !is.null(word_embeddings)) {
    stop('Both arguments: "texts" and "word_embeddings" cannot be defined simultaneously.
         Choose one or the other.')
  }

  #### Special treatment for implicit motives - see private functions ####
  model_name <- gsub("^\"|\"$", "", deparse(substitute(model_info)))
  lower_case_model <- as.character(tolower(model_name))

  # get_model_info retrieves the particular configurations that are needed for automatic implicit motive coding
  get_model_info <- get_model_info(model_info = model_info,
                                   participant_id = participant_id,
                                   show_texts = show_texts,
                                   type = type,
                                   texts = texts,
                                   story_id = story_id,
                                   lower_case_model = lower_case_model)

  model_info <- get_model_info$model_info
  show_texts <- get_model_info$show_texts
  show_prob <- get_model_info$show_prob
  type <- get_model_info$type
  texts <- get_model_info$texts
  participant_id <- get_model_info$participant_id
  story_id = get_model_info$story_id

  #### End Special treatment for implicit motives ####

  #### Automatically extract embeddings that are compatible with the model ####
  if (!is.null(texts)) {
    # Retrieve embeddings that are compatible with the model.
      emb_and_mod <- textReturnModelAndEmbedding(
      texts = texts,
      word_embeddings = word_embeddings,
      model_info = model_info,
      save_model = save_model,
      type = type,
      device = device,
      story_id = story_id,
      save_embeddings = save_embeddings,
      save_dir = save_dir,
      save_name = save_name,
      previous_sentence = previous_sentence
    )

    # retrieve model from emb_and_mod object
    loaded_model <- emb_and_mod$loaded_model

    # retrieve embeddings from emb_and_mod object
    word_embeddings <- emb_and_mod$embeddings$texts

    # retrieve classes in case of logistic regression
    classes <- emb_and_mod$classes
  } else {
    loaded_model <- model_info
  }

  # check if model is defined
  if (is.null(loaded_model)) {
    stop("No model was found.")
  }
  # check if embeddings are defined
  if (is.null(word_embeddings) && is.null(x_append)) {
    stop("No embeddings were found.")
  }

  # Get the right word-embeddings
  if (dim_names == TRUE) {
    # Select the predictor variables needed for the prediction
    target_variables_names <- loaded_model$final_model$pre$actions$recipe$recipe$var_info$variable[
      loaded_model$final_model$pre$actions$recipe$recipe$var_info$role == "predictor"]
    ####    #target_variables_names <- loaded_model$final_recipe$var_info$variable[loaded_model$final_recipe$var_info$role == "predictor"]

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


  # Original structure:
  # Dealing with NAs
  # new_data1$id_nr <- c(seq_len(nrow(new_data1)))
  # new_data1 <- new_data1[complete.cases(new_data1), ]
  # new_data_id_nr_col <- tibble::as_tibble_col(seq_len(nrow(new_data1)), column_name = "id_nr")
  # Dealing with NAs # Position of new_data_id_nr_col and new_data1 has now been switched.

  new_data1$id_nr <- c(seq_len(nrow(new_data1)))
  new_data_id_nr_col <- tibble::as_tibble_col(seq_len(nrow(new_data1)), column_name = "id_nr")
  new_data1 <- new_data1[complete.cases(new_data1), ]


  #### Load prepared_with_recipe
  #ok#  data_prepared_with_recipe <- recipes::bake(loaded_model$final_recipe, new_data1)

  # Get column names to be removed from output
  #  colnames_to_b_removed <- colnames(data_prepared_with_recipe)
  #  colnames_to_b_removed <- colnames_to_b_removed[!colnames_to_b_removed == "id_nr"]

  colnames_to_b_removed <- loaded_model$final_model$pre$actions$recipe$recipe$var_info$variable[
    loaded_model$final_model$pre$actions$recipe$recipe$var_info$role == "predictor"]


  # If the user has defined a threshold, then implement the threshold algorithm.
  if (!is.null(threshold)) {
    # Retrieves the two classes
    #class1 <- classes[1]
    #class2 <- classes[2]

    # Create column names
    #class1_col_name <- paste0(class1, "_prob")
    #class2_col_name <- paste0(class2, "_prob")

    # Predict
    predicted_scores2 <- new_data1 %>%
      dplyr::bind_cols(stats::predict(loaded_model$final_model, new_data = new_data1, type = "prob")) %>% # , ...
      dplyr::select(-!!colnames_to_b_removed) %>%
      dplyr::full_join(new_data_id_nr_col, by = "id_nr") %>%
      dplyr::arrange(id_nr) %>%
      dplyr::select(-id_nr)

    # Rename columns
    #predicted_scores2 <- predicted_scores2 %>%
    #  dplyr::rename(
    #     !!class1_col_name := 1,
    #    !!class2_col_name := 2
    # )

    # If the user desires to only view class, then remove the probabilty columns.
    if (type == "class" && show_prob == FALSE) {
      predicted_scores2 <- predicted_scores2 %>%
        dplyr::mutate(predicted_class = ifelse(.[[1]] >= threshold, 1, 0)) %>%
        dplyr::select(predicted_class)


      ############## CHANGE!!!!! #############
      we_names <- paste(word_embeddings_names, collapse = "_", sep = "")
      v_names <- paste(variable_names, collapse = "_", sep = "")

      y_name <- loaded_model$model_description[3]
      y_name <- gsub("[[:space:]]", "", y_name)
      y_name <- gsub("y=", "", y_name)

      colnames(predicted_scores2) <- paste(we_names, "_", v_names, "_", y_name, "pred", sep = "")

      # If the user desires to view the classes and the probability columns.
    } else if (type == "class" && show_prob == TRUE) {
      predicted_scores2 <- predicted_scores2 %>%
        dplyr::mutate(predicted_class = ifelse(.[[1]] >= threshold, 1, 0)) %>%
        dplyr::select(predicted_class, dplyr::everything())


    } else if (is.null(type)) {

    }
  }
  # If no threshold is defined, then use the predefined threshold of 50%.
  if (is.null(threshold)) {
    # Get Prediction scores help(arrange)
    #    predicted_scores2 <- data_prepared_with_recipe %>%
    #      dplyr::bind_cols(stats::predict(loaded_model$final_model, new_data = new_data1, type = type)) %>% # , ...
    #      dplyr::select(-!!colnames_to_b_removed) %>%
    #      dplyr::full_join(new_data_id_nr_col, by = "id_nr") %>%
    #      dplyr::arrange(id_nr) %>%
    #      dplyr::select(-id_nr)


    ## OK ### target_variables_names = NULL
    predicted_scores2 <- new_data1 %>%
      dplyr::bind_cols(stats::predict(loaded_model$final_model, new_data = new_data1, type = type)) %>% # , ...
      dplyr::select(-!!colnames_to_b_removed) %>% #
      dplyr::full_join(new_data_id_nr_col, by = "id_nr") %>%
      dplyr::arrange(id_nr) %>%
      dplyr::select(-id_nr)

    ## OK ###

    #################### CHANGE
    we_names <- paste(word_embeddings_names, collapse = "_", sep = "")
    v_names <- paste(variable_names, collapse = "_", sep = "")

    y_name <- loaded_model$model_description[3]
    y_name <- gsub("[[:space:]]", "", y_name)
    y_name <- gsub("y=", "", y_name)

    colnames(predicted_scores2) <- paste(we_names, "_", v_names, "_", y_name, "pred", sep = "")

    # If no threshold is defined, but both classification and prediction is to be viewed
    if (show_prob == TRUE) {
      prob_scores <- new_data1 %>%
        dplyr::bind_cols(stats::predict(loaded_model$final_model, new_data = new_data1, type = "prob")) %>%
        dplyr::select(-!!colnames_to_b_removed) %>%
        dplyr::full_join(new_data_id_nr_col, by = "id_nr") %>%
        dplyr::arrange(id_nr) %>%
        dplyr::select(-id_nr)

      # Adding probabilities to predicted_scores2
      predicted_scores2 <- cbind(predicted_scores2, prob_scores)
    }
  }

  # Include text in redictions
  if (show_texts) {
    predicted_scores2 <- predicted_scores2 %>%
      dplyr::mutate(texts = texts)
  }

  #### Implicit motives section, see private_functions #####

  # Check for implicit motives configuration

  if (
    (grepl("implicit", lower_case_model) & grepl("power", lower_case_model)) ||
    (grepl("implicit", lower_case_model) & grepl("affiliation", lower_case_model)) ||
    (grepl("implicit", lower_case_model) & grepl("achievement", lower_case_model)) ||
    (grepl("implicit_motives", lower_case_model) &&
     (!is.null(participant_id) || !is.null(story_id)))
  ){

    # Wrapper function that prepares data for
    # automatic implicit motive coding and returns
    # a list with predictions, class residuals and probability residuals.

    implicit_motives_results(
      model_reference = model_info,
      participant_id = participant_id,
      story_id = story_id,
      predicted_scores2 = predicted_scores2,
      texts = texts,
      dataset = dataset_to_merge_predictions,
      lower_case_model = lower_case_model
    )
    #### End Implicit motives section #####
  } else {
    # display message to user
    cat(colourise("Predictions are ready!", fg = "green"))
    cat("\n")
    return(predicted_scores2)
  }
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

  for (i in seq_len(length(models))) {
    preds <- textPredict(
      model_info = models[[i]],
      word_embeddings = word_embeddings,
      x_append = x_append, ...
    )

    output_predictions[[i]] <- preds
  }
  output_predictions1 <- dplyr::bind_cols(output_predictions)
  return(output_predictions1)
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
#' @param method Set "t-test" if comparing predictions from models that predict the SAME outcome.
#' Set "bootstrap" if comparing predictions from models that predict DIFFERENT outcomes or comparison from logistic
#' regression computing AUC distributions.
#' @param statistic Character ("correlation", "auc") describing statistic to be compared in bootstrapping.
#' @param event_level Character "first" or "second" for computing the auc in the bootstrap.
#' @param bootstraps_times Number of bootstraps (when providing y2).
#' @param seed Set seed.
#' @param ... Settings from stats::t.test or overlapping::overlap (e.g., plot = TRUE).
#' @return Comparison of correlations either a t-test or the overlap of a bootstrapped procedure (see $OV).
#' @examples
#' # Example random data
#' y1 <- runif(10)
#' yhat1 <- runif(10)
#' y2 <- runif(10)
#' yhat2 <- runif(10)
#'
#' boot_test <- textPredictTest(y1, y2, yhat1, yhat2)
#' @seealso see \code{\link{textTrain}} \code{\link{textPredict}}
#' @importFrom stats t.test cor
#' @importFrom tibble is_tibble as_tibble_col
#' @importFrom tidyr unnest
#' @importFrom dplyr select mutate
#' @importFrom overlapping overlap
#' @importFrom rsample analysis bootstraps
#' @importFrom yardstick roc_auc_vec
#' @export
textPredictTest <- function(y1,
                            y2,
                            yhat1,
                            yhat2,
                            method = "t-test",
                            statistic = "correlation",
                            paired = TRUE,
                            event_level = "first",
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
    if (statistic == "correlation") {
      stats_on_bootstrap <- function(split) {
        stats::cor(
          rsample::analysis(split)[[1]],
          rsample::analysis(split)[[2]]
        )
      }
    }


    # AUC function
    if (statistic == "auc") {
      stats_on_bootstrap <- function(split) {
        yardstick::roc_auc_vec(as.factor(rsample::analysis(split)[[1]]),
                               rsample::analysis(split)[[2]],
                               event_level = event_level
        )
      }
    }


    # Creating correlation distribution for y1 and yhat1
    y_yhat1_df <- tibble::tibble(y1, yhat1)
    boots_y1 <- rsample::bootstraps(y_yhat1_df,
                                    times = bootstraps_times,
                                    apparent = FALSE
    )

    boot_corrss_y1 <- boots_y1 %>%
      dplyr::mutate(corr_y1 = purrr::map(splits, stats_on_bootstrap))



    boot_y1_distribution <- boot_corrss_y1 %>%
      tidyr::unnest(corr_y1) %>%
      dplyr::select(corr_y1)

    # Creating correlation distribution for y2 and yhat2
    y_yhat2_df <- tibble::tibble(y2, yhat2)
    boots_y2 <- rsample::bootstraps(y_yhat2_df,
                                    times = bootstraps_times,
                                    apparent = FALSE
    )

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
