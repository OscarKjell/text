
# A helper function to textPredict giving it the capabilities of textPredictEntireProcedure.
#' @param model_info (character or r-object) model_info has four options. 1: R model object
#' (e.g, saved output from textTrainRegression). 2: Link to a model stored in a github repo
#' (e.g, "https://github.com/CarlViggo/pretrained_swls_model/raw/main/trained_github_model_logistic.RDS").
#' 3: Link to a model stored in a osf project (e.g, https://osf.io/8fp7v).
#' 4: Path to a model stored locally (e.g, "path/to/your/model"). Information about some accessible models
#' can be found at: \href{https://r-text.org/articles/pre_trained_models.html}{r-text.org}.
#' @param save_model (boolean) The model will by default be saved in work directory (deafult = TRUE).
#' @param save_dir (character) Directory to save embeddings. (default = "wd" (i.e, work-directory))
#' @noRd
textReturnModel<- function(
    model_info = NULL,
    save_model = TRUE,
    save_dir = "wd"
    ) {
  # extend the timeout time for larger models.
  options(timeout = 5 * 60)

  # diaplay message to user
  message(colourise("Loading model... \n", fg = "purple"))
  #message("\n")

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
      loaded_model_confirm <- paste0(c("The model: ", model_name, " has been loaded and saved in:", getwd()), sep = "")
      message(colourise(loaded_model_confirm, fg = "green"))
      #message("\n")
    } else if (grepl("github.com/", model_info) && isFALSE(model_exists) && isFALSE(save_model)) {
      # load from github, don't save
      loaded_model <- readRDS(url(model_info))

      # display message to user
      loaded_model_confirm <- paste0(c("The model: ", model_name, " has been loaded from: ", model_info), sep = "")
      message(colourise(loaded_model_confirm, fg = "green"))
      #message("\n")
    } else if (grepl("github.com/", model_info) && isTRUE(model_exists)) {
      # retrive model from wd if it's already downloaded
      loaded_model <- readRDS(model_name)

      # display message to user
      loaded_model_confirm <- paste0(c("The model: ", model_name, " has been loaded from: ", getwd()), sep = "")
      message(colourise(loaded_model_confirm, fg = "green"))
      #message("\n")
    } else if (grepl("osf.io", model_info)){

      # check if osfr is installed, otherwise, ask the user to install it.
      if (!requireNamespace("osfr", quietly = TRUE)) {
        stop("The osfr-package is required for loading files from osfr.
         Please install it using install.packages('osfr').", call. = FALSE)
      }

      # retrive osfr tibble with file
      osf_files <- osf_retrieve_file(model_info)

      # check if model is already downloaded
      if (isTRUE(file.exists(osf_files$name))){
        # load model from local memory
        loaded_model <- readRDS(osf_files$name)

        # display message to user
        loaded_model_confirm <- paste0(c("The model:", osf_files$name, "has been loaded from:", getwd()), sep = "")
        message(colourise(loaded_model_confirm, fg = "green"))
        #message("\n")

      } else{
        # download model locally
        downloaded_model <- osf_download(osf_files[1,])

        loaded_model <- readRDS(downloaded_model$local_path)

        # display message to user
        loaded_model_confirm <- paste0(c("The model:", osf_files$name, "has been loaded and saved in:", getwd()), sep = "")
        message(colourise(loaded_model_confirm, fg = "green"))
        #message("\n")
      }
    } else {
      # load model from specific path (if it exists somewhere else than in the work directory)
      loaded_model <- readRDS(model_info)

      # display message to user
      loaded_model_confirm <- paste0(c("The model:", model_name, "has been loaded from:", model_info), sep = "")
      message(colourise(loaded_model_confirm, fg = "green"))
      #message("\n")
    }
  } else {
    # model was an R object of a model
    loaded_model <- model_info
    # display message to user
    loaded_model_confirm <- paste0(c("The model has been loaded from your global environment."), sep = "")
    message(colourise(loaded_model_confirm, fg = "green"))
    #message("\n")
  }

  return(loaded_model)
}



# A helper function to textPredict giving it the capabilities of textPredictEntireProcedure.
#' @param texts (character) Text to predict. If this argument is specified, then argument
#' "premade_embeddings" must be set to NULL (default = NULL).
#' @param word_embeddings (Embeddings from e.g., textEmbed) Embeddings to predict. If
#' this argument is specified, then argument "texts" must be set to NULL (default = NULL).
#' @param model_info (character or r-object) model_info has four options. 1: R model object
#' (e.g, saved output from textTrainRegression). 2: Link to a model stored in a github repo
#' (e.g, "https://github.com/CarlViggo/pretrained_swls_model/raw/main/trained_github_model_logistic.RDS").
#' 3: Link to a model stored in a osf project (e.g, https://osf.io/8fp7v).
#' 4: Path to a model stored locally (e.g, "path/to/your/model"). Information about some accessible models
#' can be found at: \href{https://r-text.org/articles/pre_trained_models.html}{r-text.org}.
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
#' @param implementation NULL or "dlatk".
#' @param ... Parameters in textEmbed.
#' @noRd
textReturnEmbedding <- function(
    texts = NULL,
    word_embeddings = NULL,
    loaded_model = NULL,
    save_model = TRUE,
    type = "class",
    device = "cpu",
    story_id = NULL,
    save_embeddings = TRUE,
    save_dir = "wd",
    save_name = "textPredict",
    previous_sentence = FALSE,
    implementation = NULL,
    ...
    ) {
  # extend the timeout time for larger models.
  options(timeout = 5 * 60)

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

      message(colourise(paste0("Embeddings have been loaded from: ", full_path), fg = "green"))
      #message("\n")

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
    model_layers <- as.numeric(extract_comment(
      comment = loaded_model$model_description[new_line_number],
      part = "layers"))

    # Extracts the max_token_to_sentence, aggregation_from_layers_to_tokens,
    # and aggregation_from_tokens_to_texts from the model.
    input_string <- loaded_model$model_description[new_line_number]

    aggregation_from_layers_to_tokens <- sub(".*aggregation_from_layers_to_tokens = (\\S+).*", "\\1", input_string)

    aggregation_from_tokens_to_texts <- sub(".*aggregation_from_tokens_to_texts = (\\S+).*", "\\1", input_string)


    # Check if the variables match input_string (i.e., nothing was retrieved above); so we have to assign the default values
    if (aggregation_from_layers_to_tokens == input_string) {
      aggregation_from_layers_to_tokens <- default_aggregation_from_layers_to_tokens
    }

    if (aggregation_from_tokens_to_texts == input_string) {
      aggregation_from_tokens_to_texts <- default_aggregation_from_tokens_to_texts
    }

    if (is.null(implementation)){
      if(grepl("implementation:", input_string)){
        implementation_comment <- extract_comment(
          input_string,
          part = "implementation_method")
      } else {
        implementation_comment <- "original"
      }
    }

    if(!is.null(implementation)){
      implementation_comment <- implementation
    }


    if (grepl("max_token_to_sentence: \\d+", input_string)) {
      max_token_to_sentence <- as.numeric(sub(".*max_token_to_sentence: (\\d+).*", "\\1", input_string))
    } else {
      max_token_to_sentence <- default_max_token_to_sentence
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
      keep_token_embeddings = FALSE,
      implementation = implementation_comment,
      ...
    )

    # save embeddings if save_embeddings is set to true
    if (isTRUE(save_embeddings) && save_dir == "wd"){
      saveRDS(embeddings, file_name)
      message(colourise(paste0("Embeddings have been saved: ", full_path), fg = "green"))
      #message("\n")
    } else if (isTRUE(save_embeddings) && save_dir != "wd"){
      saveRDS(embeddings, full_path)
      message(colourise(paste0("Embeddings have been saved: ", full_path), fg = "green"))
      #message("\n")
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
    message(colourise(Time_story_id, fg = "green"))
    #message("\n")
  }

  ##### End special treatment for automatic implicit motive coding #####

  # store classes
  #classes <- loaded_model$final_recipe$levels$y$values
  classes <- loaded_model$final_model$pre$actions$recipe$recipe$orig_lvls$y$values

  emb_and_model <- list(loaded_model = loaded_model, embeddings = embeddings, classes = classes)
  return(emb_and_model)
}



#' Extract the string after the first underscore in required predictor names
#'
#' @param model A trained workflow
#' @return A character vector with the extracted parts after the first underscore.
#' @noRd
extract_required_suffix <- function(model) {
  if (!inherits(model, "workflow")) {
    stop("The provided object is not a workflow.")
  }

  # Extract predictor names
  predictor_names <- names(model$pre$mold$blueprint$ptypes$predictors)

  # Check if any predictor has an underscore
  if (!any(grepl("_", predictor_names))) {
    return(NULL)
  }

  # Extract the part after the first underscore
  suffixes <- sub("^[^_]+_", "", predictor_names)

  suffix <- unique(suffixes)
  return(suffix)
}


#' Extract embedding model type flexibly
#'
#' @param word_embeddings A list or a tibble containing word embeddings
#' @return The extracted model type, or NULL if no comment found
#' @noRd
extract_emb_type <- function(word_embeddings) {
  # Try extracting the comment from the first element
  emb_comment <- comment(word_embeddings[[1]])

  # If no comment found, try from the list/tibble itself
  if (is.null(emb_comment) || identical(emb_comment, "")) {
    emb_comment <- comment(word_embeddings)
  }

  return(emb_comment)
}




#' Check that word embedding descriptions match the model's embedding setup
#'
#' @param word_embeddings A word_embeddings object with comment.
#' @param loaded_model A model object with `model_description` attribute including a comment.
#' @return NULL (stops with informative message if mismatch is found)
#' @noRd
word_embedding_check <- function(word_embeddings, loaded_model) {
  embedding_comment <- extract_emb_type(word_embeddings)

  parts_to_check <- c("model", "layers", "aggregation_from_layers_to_tokens", "aggregation_from_tokens_to_texts")

  model_comment <- paste(loaded_model$model_description, collapse = " ")

  differences <- list()
  #part = 1
  for (part in parts_to_check) {
    emb_val <- extract_comment(comment = embedding_comment, part = part)
    mod_val <- extract_comment(comment = model_comment, part = part)

    if (is.numeric(emb_val) && is.numeric(mod_val)) {
      match <- all(emb_val == mod_val)
    } else {
      match <- identical(emb_val, mod_val)
    }

    if (!match) {
      differences[[part]] <- list(model = mod_val, embedding = emb_val)
    }
  }

  if (length(differences) > 0) {
    msg_lines <- c("Word embedding settings do not match the model:")
    for (part in names(differences)) {
      model_val <- paste(differences[[part]]$model, collapse = " ")
      embed_val <- paste(differences[[part]]$embedding, collapse = " ")
      msg_lines <- c(
        msg_lines,
        paste0("- ", part, ": model = ", model_val, "; embedding = ", embed_val)
      )
    }

    msg_lines <- c(msg_lines, "To ignore this, set `check_matching_word_embeddings = FALSE`.")
    stop(colourise(paste(msg_lines, collapse = "\n"), "brown"))
  }

  return(invisible(NULL))
}

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
#' @param check_matching_word_embeddings (boolean) If `TRUE`, the function will check whether the word embeddings (model type and layer) match
#' the requirement of the trained model - if a mis-match is found the function till stop. If `FALSE`, the function will not verify.
#' @param language_distribution (Character column) If you provide the raw language data used for making the embeddings used for assessment,
#' the language distribution (i.e., a word and frequency table) will be compared with saved one in the model object (if one exists).
#' This enables calculating similarity scores.
#' @param language_distribution_min_words (string or numeric) Default is to use the removal threshold used when creating the distribution in the
#' in the training set ("trained_distribution_min_words"). You can set it yourself with a numeric value.
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
#' @param dataset_to_merge_assessments (tibble; only works for implicit motives models) Insert your data here to integrate predictions to your dataset,
#'  (default = NULL).
#' @param previous_sentence (Boolean; only works for implicit motives models) If set to TRUE, word-embeddings will be averaged over the current and previous
#' sentence per story-id. For this, both participant-id and story-id must be specified.
#' @param device Name of device to use: 'cpu', 'gpu', 'gpu:k' or 'mps'/'mps:k' for MacOS, where k is a
#' specific device number such as 'mps:1'.
#' @param ...  Setting from textEmbed() (and stats::predict) can be called.
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
#'   dataset_to_merge_assessments = implicit_motive_data
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
textPredictTextTrained <- function(
    model_info = NULL,
    word_embeddings = NULL,
    texts = NULL,
    x_append = NULL,
    append_first = TRUE,
    threshold = NULL,
    dim_names = TRUE,
    check_matching_word_embeddings = TRUE,
    language_distribution = NULL,
    language_distribution_min_words = "trained_distribution_min_words",
    save_model = TRUE,
    save_embeddings = TRUE,
    save_dir = "wd",
    save_name = "textPredict",
    show_texts = FALSE,
    participant_id = NULL,
    story_id = NULL,
    dataset_to_merge_assessments = NULL,
    previous_sentence = FALSE,
    device = "cpu",
    implementation = NULL,
    ...) {

  use_row_id_name = FALSE
  # Stop message if user defines both word_embeddings and texts
  if (!is.null(texts) && !is.null(word_embeddings)) {
    stop('Both arguments: "texts" and "word_embeddings" cannot be defined simultaneously.
         Choose one or the other.')
  }

  # Get the model
  if(is.character(model_info)){

    loaded_model <- textReturnModel(
      model_info = model_info,
      save_model = save_model)

  } else {
    loaded_model <- model_info
  }


  #### Automatically extract embeddings that are compatible with the model ####
  if (!is.null(texts)) {
    # Retrieve embeddings that are compatible with the model.
      emb_and_mod <- textReturnEmbedding(
      texts = texts,
      word_embeddings = word_embeddings,
      loaded_model = loaded_model,
  #   save_model = save_model,
   #   type = type,
      device = device,
      story_id = story_id,
      save_embeddings = save_embeddings,
      save_dir = save_dir,
      save_name = save_name,
      previous_sentence = previous_sentence,
      implementation = implementation
      ,  ...
    )
    # retrieve model from emb_and_mod object
    loaded_model <- emb_and_mod$loaded_model

    # retrieve embeddings from emb_and_mod object
    word_embeddings <- emb_and_mod$embeddings$texts

    # retrieve classes in case of logistic regression
    classes <- emb_and_mod$classes
  }

  # "regression" or "classification"
  mod_type <- loaded_model$final_model$fit$actions$model$spec[[3]]

  if(mod_type == "regression"){
    type = NULL
  } else{
    type = "class"
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

    # TO DO: weight should be a more specific name <-  tailored to the text-package unlikely to be a variable name.
   # word_embeddings_names <- setdiff(word_embeddings_names, "weights_in_text")

    # Select the word embeddings
    word_embeddings <- word_embeddings[word_embeddings_names]

    if(is.null(word_embeddings[[1]])){
      message_emb <- c("Could not find the required dimensions. You may set dim_names = FALSE, but ensure that this is appropriate for your data and intended use.")
      message(colourise(message_emb, "brown"))
    }
  }

  if (dim_names == FALSE) {

    #emb_for_now_test <- word_embeddings
    # Remove specific names in the word embeddings
    word_embeddings <- textDimName(word_embeddings,
                                   dim_names = FALSE
    )
    word_embeddings_names <- "word_embeddings"

    #model_4_now <- loaded_model$final_model
    new_name <- extract_required_suffix(model = loaded_model$final_model)

    if(!is.null(new_name)){

      word_embeddings <- textDimName(word_embeddings,
                                       dim_names = TRUE,
                                       name = new_name)
    }
  }


  #### Checking word_embedding and model specifications
  if(check_matching_word_embeddings &
     length(word_embeddings) > 0){
    word_embedding_check(word_embeddings, loaded_model)
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
    x_append = x_append_target,
    append_first = append_first # i don't think this is needed here; specifying the order is only needed in training.
  )
  new_data1 <- new_data1$x1


  new_data1$id_nr <- c(seq_len(nrow(new_data1)))
  new_data_id_nr_col <- tibble::as_tibble_col(seq_len(nrow(new_data1)), column_name = "id_nr")
  new_data1 <- new_data1[complete.cases(new_data1), ]


  #### Load prepared_with_recipe  ####

  colnames_to_b_removed <- loaded_model$final_model$pre$actions$recipe$recipe$var_info$variable[
    loaded_model$final_model$pre$actions$recipe$recipe$var_info$role == "predictor"]


  # If the user has defined a threshold, then implement the threshold algorithm. threshold=0.99
  if (!is.null(threshold)) {

    # Predict
    predicted_scores2 <- new_data1 %>%
      dplyr::bind_cols(stats::predict(loaded_model$final_model,
                                      new_data = new_data1,
                                      type = "prob")) %>% # , ...
      dplyr::select(-!!colnames_to_b_removed) %>%
      dplyr::full_join(new_data_id_nr_col, by = "id_nr") %>%
      dplyr::arrange(id_nr) %>%
      dplyr::select(-id_nr)

       # Show both class and probability.
       if (mod_type == "classification") {

         class <- predicted_scores2 %>%
           dplyr::mutate(predicted_class = ifelse(.[[1]] >= threshold, 1, 0)) %>%
           dplyr::select(predicted_class)
          # dplyr::select(predicted_class, dplyr::everything())

         ################
         we_names <- paste(word_embeddings_names, collapse = "_", sep = "")
         v_names <- paste(variable_names, collapse = "_", sep = "")

         y_name <- loaded_model$model_description[3]
         y_name <- gsub("[[:space:]]", "", y_name)
         y_name <- gsub("y=", "", y_name)

         colnames(class) <- paste(we_names, "_", v_names, "_", y_name, "pred", sep = "")
         # Adding probabilities to predicted_scores2
         predicted_scores2 <- dplyr::bind_cols(class, predicted_scores2)
       }
  }

  # If no threshold is defined, then use the predefined threshold of 50%.
  if (is.null(threshold)) {
    # Get Prediction scores

    predicted_scores2 <- new_data1 %>%
      dplyr::bind_cols(stats::predict(loaded_model$final_model,
                                      new_data = new_data1, type = type)) %>% # , ... or
      dplyr::select(-!!colnames_to_b_removed) %>% #
      dplyr::full_join(new_data_id_nr_col, by = "id_nr") %>%
      dplyr::arrange(id_nr) %>%
      dplyr::select(-id_nr)

    #### Setting the column name
    we_names <- paste(word_embeddings_names, collapse = "_", sep = "")
    v_names <- paste(variable_names, collapse = "_", sep = "")
    y_name <- loaded_model$model_description[3]
    y_name <- gsub("[[:space:]]", "", y_name)
    y_name <- gsub("y=", "", y_name)

    colnames(predicted_scores2) <- paste(we_names, "_", v_names, "_", y_name, "pred", sep = "")

    # If no threshold is defined, but both classification and prediction is to be viewed
    if (mod_type == "classification") {
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

  #### Include text in predictions ####
  if (show_texts) {
    predicted_scores2 <- predicted_scores2 %>%
      dplyr::mutate(texts = texts)
  }


  #### Comparing domain similarities #### help(textTokenizeAndCount)
  if (tibble::is_tibble(loaded_model$language_distribution) &
     !is.null(texts) | !is.null(language_distribution)){ # could add option to add language distribution | !is.null(language_distribution)


    # Get language_distribution_min_words parameter from training
    if(language_distribution_min_words == "trained_distribution_min_words"){

    n_remove_threshold_comment <- comment(loaded_model$language_distribution)

    language_distribution_min_words <- extract_comment(
      comment = n_remove_threshold_comment,
      part = "n_remove_threshold")
    }

    #### Group level measures
    if (!is.null(texts) & is.null(language_distribution)){
      data_language_distribution <-  texts
    }
    if (!is.null(language_distribution)){
      data_language_distribution <- language_distribution
    }

    assess_distribution <- textTokenizeAndCount(
      data = data_language_distribution,
      n_remove_threshold = language_distribution_min_words)

    similarity_scores <- textDomainCompare(
      train_language = loaded_model$language_distribution,
      assess_language = assess_distribution
    )

    #### Instance level token measures ####
    instance_token_list <- list()
    for (i in 1:nrow(data_language_distribution)){

      # Token similarity comparisons
      instance_assess_distribution <- textTokenizeAndCount(
        data = data_language_distribution[1][i,],
        n_remove_threshold = 0)

      instance_similarity_scores <- textDomainCompare(
        train_language = loaded_model$language_distribution,
        assess_language = instance_assess_distribution
      )

      instance_tibble <- tibble::tibble(
        overlap_percentage = instance_similarity_scores$overlap_percentage,
        test_recall_percentage = instance_similarity_scores$test_recall_percentage,
        cosine_similarity = instance_similarity_scores$cosine_similarity,
        cosine_similarity_standardised = instance_similarity_scores$cosine_similarity_standardised
      )
      instance_token_list[[i]] <- instance_tibble
    }
    instance_tibble <- dplyr::bind_rows(instance_token_list)

    instance_mean_overlap_percentage <- mean(instance_tibble$overlap_percentage)
    instance_mean_test_recall_percentage <- mean(instance_tibble$test_recall_percentage)
    instance_mean_cosine_similarity <- mean(instance_tibble$cosine_similarity)
    instance_mean_cosine_similarity_standardised <- mean(instance_tibble$cosine_similarity_standardised)

    instance_mean <- list(
      instance_mean_overlap_percentage = instance_mean_overlap_percentage,
      instance_mean_test_recall_percentage = instance_mean_test_recall_percentage,
      instance_mean_cosine_similarity = instance_mean_cosine_similarity,
      instance_mean_cosine_similarity_standardised = instance_mean_cosine_similarity_standardised)


    #### Instance level embeddings measures ####
    if(length(loaded_model$aggregated_word_embeddings)>1){

      if(!tibble::is_tibble(word_embeddings)){
        word_embeddings <- word_embeddings[[1]]
      }

        ss_min <- textSimilarityNorm(
          word_embeddings,
          loaded_model$aggregated_word_embeddings$aggregated_word_embedding_min,
          method = "cosine", center = TRUE, scale = FALSE
        )

        ss_max <- textSimilarityNorm(
          word_embeddings,
          loaded_model$aggregated_word_embeddings$aggregated_word_embedding_max,
          method = "cosine", center = TRUE, scale = FALSE
        )

        ss_mean <- textSimilarityNorm(
          word_embeddings,
          loaded_model$aggregated_word_embeddings$aggregated_word_embedding_mean,
          method = "cosine", center = TRUE, scale = FALSE
        )

        instance_emb_tibble <- tibble::tibble(
          ss_min = ss_min,
          ss_max = ss_max,
          ss_mean = ss_mean
          )

      mean_instance_embedding_min <- mean(instance_emb_tibble$ss_min)
      mean_instance_embedding_max <- mean(instance_emb_tibble$ss_max)
      mean_instance_embedding_mean <- mean(instance_emb_tibble$ss_mean)

      mean_instance_emb_list <- list(
        mean_instance_embedding_min = mean_instance_embedding_min,
        mean_instance_embedding_max = mean_instance_embedding_max,
        mean_instance_embedding_mean = mean_instance_embedding_mean
      )
    } else {
      instance_emb_tibble <- c("Could not compute similarity scores because there were no aggregated word embeddings.")
      mean_instance_emb_list <- c("Could not compute similarity scores because there were no aggregated word embeddings.")
    }

    predicted_scores2 <- c(list(
      instance_emb_similarities = instance_emb_tibble,
      instance_emb_mean = mean_instance_emb_list,
      instance_token_similarities = instance_tibble,
      instance_token_similarity_mean = instance_mean,
      similarity_scores = similarity_scores),
      predictions = list(predicted_scores2))
  }

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

  if (!requireNamespace("overlapping", quietly = TRUE)) {
    msg <- c("overlapping is required for this test.\nPlease install it using install.packages('overlapping').")

    message(colourise(msg, "brown"))
  }

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
        idx <- ncol(rsample::analysis(split))
        if (idx > 2) idx <- 2:idx # multiple categories probs
        if (idx == 2) idx <- 2 # 2 categories probs
        yardstick::roc_auc_vec(as.factor(rsample::analysis(split)[[1]]),
                               as.matrix(rsample::analysis(split)[,idx]),
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

    if (requireNamespace("text", quietly = TRUE)) {
    output <- overlapping::overlap(x_list_dist)
    output <- list(output$OV[[1]])
    }

    names(output) <- "overlapp_p_value"
  }
  output
}
