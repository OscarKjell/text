# Wrapper functions for textPredictR and textClassifyPipe

#' textPredict, textAssess and textClassify
#'
#' Trained models created by e.g., textTrain() or stored on e.g., github of huggingface
#' can be used to predict  scores or classes from embeddings or text using one of these function
#' aliases.
#' @param model_info (character or r-object) model_info has four options, including:
#' 1: An R model (e.g, saved output from one of the textTrain() functions).
#' 2: The name specified in the  \href{https://r-text.org/articles/LBAM.html}{L-BAM Documentation}.
#' For the following settings, remember to also set the model_type parameter:
#' 3: Link to a text-trained model online (either in a github repo
#' (e.g, "https://github.com/CarlViggo/pretrained_swls_model/raw/main/trained_github_model_logistic.RDS"or
#' OSF https://osf.io/8fp7v)
#' 4: Name or link to a fine-tuned model from Huggingface (e.g., "distilbert-base-uncased-finetuned-sst-2-english").
#' 5: Path to a model stored locally (e.g, "path/to/your/model/model_name.rds").
#' @param texts (character) Text to predict. If this argument is specified, then arguments "word_embeddings"
#' and "premade embeddings" cannot be defined (default = NULL).
#' @param word_embeddings (tibble; only for "text-trained"-model_type) Embeddings from e.g., textEmbed(). If you're using a pre-trained model,
#'  then texts and embeddings cannot be submitted simultaneously (default = NULL).
#' @param x_append (tibble; only for "text-trained"-model_type) Variables to be appended with the word embeddings (x).
#' @param append_first (boolean; only for "text-trained" models) If TRUE, x_appened is added before word embeddings.
#' @param model_type (character) Specify how the function should handle the model argument. The default is "detect" where the fucntion ttried to detect it
#' automatically. Setting it to "fine-tuned" or "text-trained" will apply their respective default behaviors, while setting it to "implicit motives" will
#' trigger specific steps tailored to these models.
#' @param lbam_update (boolean) Updating the L-BAM file by automatically downloading it from Google Sheet.
#' @param dim_names (boolean; only for "text-trained"-models) Account for specific dimension names from textEmbed()
#' (rather than generic names including Dim1, Dim2 etc.). If FALSE the models need to have been trained on
#' word embeddings created with dim_names FALSE, so that embeddings were only called Dim1, Dim2 etc.
#' @param language_distribution (character column; only for "text-trained" models) If you provide the raw language data used for making the embeddings used for assessment,
#' the language distribution (i.e., a word and frequency table) will be compared with saved one in the model object (if one exists).
#' This enables calculating similarity scores.
#' @param language_distribution_min_words (string or numeric; only for "text-trained" models) Default is to use the removal threshold used when creating the distribution in the
#' in the training set ("trained_distribution_min_words"). You can set it yourself with a numeric value.
#' @param save_model (boolean; only for "text-trained"-models) The model will by default be saved in your work-directory (default = TRUE).
#' If the model already exists in your work-directory, it will automatically be loaded from there.
#' @param threshold (numeric; only for "text-trained"-models) Determine threshold if you are using a logistic model (default = 0.5).
#' @param show_texts (boolean; only for "implicit-motives"-models) Show texts together with predictions (default = FALSE).
#' @param device Name of device to use: 'cpu', 'gpu', 'gpu:k' or 'mps'/'mps:k' for MacOS, where k is a
#' specific device number such as 'mps:1'.
#' @param participant_id (list; only for "implicit-motives"-models) Vector of participant-ids. Specify this for getting person level scores
#' (i.e., summed sentence probabilities to the person level corrected for word count). (default = NULL)
#' @param story_id (vector; only for "implicit-motives"-models) Vector of story-ids. Specify this to get story level scores (i.e., summed sentence
#' probabilities corrected for word count). When there is both story_id and participant_id indicated, the function
#' returns a list including both story level and person level prediction corrected for word count. (default = NULL)
#' @param dataset_to_merge_assessments (R-object, tibble; only for "implicit-motives"-models) Insert your data here to integrate predictions to your dataset,
#'  (default = NULL).
#' @param save_embeddings (boolean; only for "text-trained"-models) If set to TRUE, embeddings will be saved with a unique identifier, and
#' will be automatically opened next time textPredict is run with the same text. (default = TRUE)
#' @param save_dir (character; only for "text-trained"-models) Directory to save embeddings. (default = "wd" (i.e, work-directory))
#' @param save_name (character; only for "text-trained"-models) Name of the saved embeddings (will be combined with a unique identifier).
#' (default = ""). Obs: If no save_name is provided, and model_info is a character, then save_name will be set
#' to model_info.
#' @param previous_sentence (boolean; only for "implicit-motives"-models) If set to TRUE, word-embeddings will be averaged over the current and previous
#' sentence per story-id. For this, both participant-id and story-id must be specified.
#' @param tokenizer_parallelism (boolean; only for "fine-tuned"-models)  If TRUE this will turn on tokenizer parallelism.
#' @param logging_level (string; only for "fine-tuned"-models)  Set the logging level.
#' Options (ordered from less logging to more logging): critical, error, warning, info, debug
#' @param force_return_results (boolean; only for "fine-tuned"-models)  Stop returning some incorrectly formatted/structured results.
#' This setting does CANOT evaluate the actual results (whether or not they make sense, exist, etc.).
#' All it does is to ensure the returned results are formatted correctly (e.g., does the question-answering
#' dictionary contain the key "answer", is sentiments from textClassify containing the labels "positive"
#'  and "negative").
#' @param return_all_scores (boolean; only for "fine-tuned"-models)  Whether to return all prediction scores or just the one of the predicted class.
#' @param function_to_apply (string; only for "fine-tuned"-models)  The function to apply to the model outputs to retrieve the scores.
#' @param set_seed (Integer; only for "fine-tuned" models) Set seed.
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
#' @name textPredict
#' @aliases textAssess textClassify
#' @export
textPredict <- function(
    # Common parameter
    model_info = "valence_facebook_mxbai23_eijsbroek2024",
    texts = NULL,
    model_type = "detect",
    lbam_update = TRUE,
    ## text-trained model specific parameters ##
    word_embeddings = NULL,
    x_append = NULL,
    append_first = TRUE,
    dim_names = TRUE,
    language_distribution = NULL,
    language_distribution_min_words = "trained_distribution_min_words",
    save_model = TRUE,
    threshold = NULL,
    show_texts = FALSE,
    device = "cpu",
    participant_id = NULL,
    save_embeddings = TRUE,
    save_dir = "wd",
    save_name = "textPredict",
    story_id = NULL,
    dataset_to_merge_assessments = NULL,
    previous_sentence = FALSE,
    ## fine-tuned model specific parameters ##
    tokenizer_parallelism = FALSE,
    logging_level = "error",
    force_return_results = TRUE,
    return_all_scores = FALSE,
    function_to_apply = NULL,
    set_seed = 202208,
    ...){

  # set default behaviours
  is_local_model = FALSE
  is_online_path = FALSE

  # Methods for retrieving the model and settings include:
  # 1: R model object (e.g, output from the text train functions).
  # 2: The name and model_types (currently text-trained, fine-tuned or implicit-motives) specified in the L-BAM Documentation.
  # For the following settings, remember to also set the model_type parameter:
  # 3: Link to a text-trained model online (either in a github repo or OSF)
  # 4: Name or link to a fine-tuned model from Huggingface.
  # 5: Path to a model stored locally (e.g, "path/to/your/model/model_name.rds").

  # if models is an R-object, set model_type to text-trained
  if(is.list(model_info)){

    model_type = "text-trained"

  }

  # Check if it's a valid local path model_info = NULL
  if(is.character(model_info) &
     !is.null(model_info)){

    is_local_model <- file.exists(model_info)
    is_online_path <- is_internet_path(model_info)
  }


  # Locale path
  if(is_local_model & is.null(model_type) |
    is_online_path & is.null(model_type)){

      stop(message("Please set model_type when you are calling a model with a locale or online path."))

  }

  if (model_type == "implicit-motives" |
      model_type ==  "implicit motives" |
      model_type == "implicit_motives"){

    model_type <- "implicit-motives"
  }

  # If model_type is set, check it is valid
  if(!is.null(model_type)){

    if (!(model_type %in% c("detect",
                            "text-trained", "texttrained",
                            "fine-tuned", "finetuned",
                            "implicit-motives", "implicit motives", "implicit_motives"))) {
      stop("Error: method must be either 'text-trained', 'fine-tuned' or 'implicit-motives'.")
    }
  }

  # If model_info is a path that is NOT locale
  if(is.character(model_info) & !is_local_model & !is_online_path &
     model_type != "implicit-motives"){

    model_type_and_url <- model_address_lookup(
      model_info = model_info,
      lbam_update
    )

    # Change model_info to the path specified in L-BAM
    if(!is.na(model_type_and_url$path)){
      model_info <- model_type_and_url$path
    }

    model_type <- model_type_and_url$model_type
  }

  # If path, no model_type, and no internet path and no return from L-BAM try model_type = "finetuned"


  # Applying the text-trained method #
  if(model_type == "texttrained" |
     model_type == "text-trained"){


    message(colourise("You are using a 'text-trained model' (i.e., model_type = 'texttrained'). \n",
                "green"))

      results <-  textPredictTextTrained(
        model_info = model_info,
        word_embeddings = word_embeddings,
        texts = texts,
        x_append = x_append,
        append_first = append_first,
        dim_names = dim_names,
        language_distribution = language_distribution,
        language_distribution_min_words = language_distribution_min_words,
        save_model = save_model,
        threshold = threshold,
        show_texts = show_texts,
        device = device,
        participant_id = participant_id,
        save_embeddings = save_embeddings,
        save_dir = save_dir,
        save_name = save_name,
        story_id = story_id,
        dataset_to_merge_assessments = dataset_to_merge_assessments,
        previous_sentence = previous_sentence
        , ...
        )

  }

  # Applying the finetuned method #
  if(model_type == "finetuned" |
     model_type == "fine-tuned"){

    message(colourise("You are using a fine-tuned model (i.e., model_type = 'finetuned'). \n", "green"))

      results <-  textClassifyPipe(
        x = texts,
        model = model_info,
        device = device,
        tokenizer_parallelism = tokenizer_parallelism,
        logging_level = logging_level,
        force_return_results = force_return_results,
        return_all_scores = return_all_scores,
        function_to_apply = function_to_apply,
        set_seed = set_seed)

    }

  # Applying the implicit_motives method #
  if(model_type == "implicit_motives" |
     model_type == "implicit-motives" |
     model_type == "implicit motives"){

    results <- textPredictImplicitMotives(
        model_info = model_info,
        word_embeddings = word_embeddings,
        texts = texts,
        model_type = model_type,
        x_append = x_append,
        threshold = threshold,
        dim_names = dim_names,
        language_distribution = language_distribution,
        language_distribution_min_words = language_distribution_min_words,
        save_model = save_model,
        save_embeddings = save_embeddings,
        save_dir = save_dir,
        save_name = save_name,
        show_texts = show_texts,
        participant_id = participant_id,
        story_id = story_id,
        dataset_to_merge_assessments = dataset_to_merge_assessments,
        previous_sentence = previous_sentence,
        device = device,
      )
  }

  # display message to user
  message(colourise("Assessments are ready!", fg = "green"))
  message("\n")
  return(results)

}

# Alias functions
#' @rdname textPredict
# @examples textAssess("hello")
#' @export
textAssess <- textPredict

#' @rdname textPredict
# @examples textClassify("hello")
#' @export
textClassify <- textPredict

# returns the lbam library
#' The LBAM library
#'
#' Retrieve the Language-Based Assessment Models library (LBAM).
#' @param columns (string) Select which columns to retrieve e.g., c("Name", "Path")
#' @param construct_start (string) Select which constructs concepts and/or behaviors to retrieve.
#' @param lbam_update (boolean) TRUE downloads a new copy of the LBAM file
#' @return Data frame containing information about the Language-based assessment models library (LBAM).
#' @examples
#' \dontrun{
#' library(dplyr)
#' test_lbam <- textLBAM(
#'   lbam_update = TRUE
#' )
#' subset(
#'   lbam,
#'   substr(Construct_Concept_Behaviours, 1, 3) == "Dep",
#'   select = c(Construct_Concept_Behaviours, Name)
#' )
#'
#' }
#' @importFrom  dplyr select
#' @importFrom dplyr bind_cols select full_join arrange everything
#' @importFrom utils read.csv
#' @export
textLBAM <- function(
    columns = NULL,
    construct_start = NULL,
    lbam_update = FALSE
) {

  if(lbam_update){
    # download file
    lbam_sheet_url <- "https://docs.google.com/spreadsheets/d/1K16JdK7zOmuRktqgaYs5sgaHnkUVwB9v6ERxZdd9iW8/export?format=csv&gid=0#gid=0/export?format=csv"
    destfile <- system.file("extdata",
                            "The_L-BAM_Library.csv",
                            package = "text")


    download.file(lbam_sheet_url,
                  destfile = destfile,
                  mode = "wb",
                  quiet = TRUE)

    message(colourise("Successfully updated the L-BAM library. \n ", "green"))
  }

  lbam <- utils::read.csv(
    system.file("extdata",
                "The_L-BAM_Library.csv",
                package = "text"), skip = 3)

  if (!is.null(columns)){
    lbam <- lbam[,columns]
  }

  if (!is.null(construct_start)){
    lbam <- lbam %>%
      filter(startsWith(Construct_Concept_Behaviours, construct_start))
  }

  return(lbam)
}
