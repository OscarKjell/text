# Wraper functions for textPredictR and textClassifyPipe

#' textPredict, textAssess and textClassify
#'
#' Trained models created by e.g., textTrain() or stored on e.g., github of huggingface
#' can be used to predict  scores or classes from embeddings or text using one of these function
#' aliases.
#' @param model_info (character or r-object) model_info has four options.
#' 1: R model object (e.g, saved output from one of the textTrain functions).
#' 2: Link to a model stored in a github repo (e.g, "https://github.com/CarlViggo/pretrained_swls_model/raw/main/trained_github_model_logistic.RDS"
#' or a pre-trained model from Huggingface, e.g., "distilbert-base-uncased-finetuned-sst-2-english").
#' 3: Link to a model stored in a osf project (e.g, https://osf.io/8fp7v).
#' 4: Path to a model stored locally (e.g, "path/to/your/model"). Information about some accessible models
#' can be found at: \href{https://r-text.org/articles/pre_trained_models.html}{r-text.org}.
#' @param texts (character) Text to predict. If this argument is specified, then arguments "word_embeddings"
#' and "premade embeddings" cannot be defined (default = NULL).
#' @param model_type (string) "text" or "huggingface". "text" indicates that the model is from the text-package
#' (i.e., trained using one of the textTrain() functions; see function textPredictR()); "huggingface" indicates
#' that the model comes from "huggingface" using pipe (see the textClassifyPipe function).
#' @param word_embeddings (tibble; only for "text"-model_type) Embeddings from e.g., textEmbed(). If you're using a pre-trained model,
#'  then texts and embeddings cannot be submitted simultaneously (default = NULL).
#' @param x_append (tibble; only for "text"-model_type) Variables to be appended after the word embeddings (x).
#' @param type (character; only for "text"-model_type) Defines what output to give after logistic regression prediction.
#' Either probabilities, classifications or both are returned (default = "class".
#' For probabilities use "prob". For both use "class_prob").
#' @param dim_names (boolean; only for "text"-model_type) Account for specific dimension names from textEmbed()
#' (rather than generic names including Dim1, Dim2 etc.). If FALSE the models need to have been trained on
#' word embeddings created with dim_names FALSE, so that embeddings were only called Dim1, Dim2 etc.
#' @param save_model (boolean; only for "text"-model_type) The model will by default be saved in your work-directory (default = TRUE).
#' If the model already exists in your work-directory, it will automatically be loaded from there.
#' @param threshold (numeric; only for "text"-model_type) Determine threshold if you are using a logistic model (default = 0.5).
#' @param show_texts (boolean; only for "text"-model_type) Show texts together with predictions (default = FALSE).
#' @param device Name of device to use: 'cpu', 'gpu', 'gpu:k' or 'mps'/'mps:k' for MacOS, where k is a
#' specific device number such as 'mps:1'.
#' @param participant_id (list; only for "text"-model_type) Vector of participant-ids. Specify this for getting person level scores
#' (i.e., summed sentence probabilities to the person level corrected for word count). (default = NULL)
#' @param story_id (vector; only for "text"-model_type) Vector of story-ids. Specify this to get story level scores (i.e., summed sentence
#' probabilities corrected for word count). When there is both story_id and participant_id indicated, the function
#' returns a list including both story level and person level prediction corrected for word count. (default = NULL)
#' @param dataset_to_merge_predictions (R-object, tibble; only for "text"-model_type) Insert your data here to integrate predictions to your dataset,
#'  (default = NULL).
#' @param save_embeddings (boolean; only for "text"-model_type) If set to TRUE, embeddings will be saved with a unique identifier, and
#' will be automatically opened next time textPredict is run with the same text. (default = TRUE)
#' @param save_dir (character; only for "text"-model_type) Directory to save embeddings. (default = "wd" (i.e, work-directory))
#' @param save_name (character; only for "text"-model_type) Name of the saved embeddings (will be combined with a unique identifier).
#' (default = ""). Obs: If no save_name is provided, and model_info is a character, then save_name will be set
#' to model_info.
#' @param previous_sentence (boolean; only for "text"-model_type) If set to TRUE, word-embeddings will be averaged over the current and previous
#' sentence per story-id. For this, both participant-id and story-id must be specified.
#' @param tokenizer_parallelism (boolean; only for "huggingface"-model_type)  If TRUE this will turn on tokenizer parallelism.
#' @param logging_level (string; only for "huggingface"-model_type)  Set the logging level.
#' Options (ordered from less logging to more logging): critical, error, warning, info, debug
#' @param force_return_results (boolean; only for "huggingface"-model_type)  Stop returning some incorrectly formatted/structured results.
#' This setting does CANOT evaluate the actual results (whether or not they make sense, exist, etc.).
#' All it does is to ensure the returned results are formatted correctly (e.g., does the question-answering
#' dictionary contain the key "answer", is sentiments from textClassify containing the labels "positive"
#'  and "negative").
#' @param return_all_scores (boolean; only for "huggingface"-model_type)  Whether to return all prediction scores or just the one of the predicted class.
#' @param function_to_apply (string; only for "huggingface"-model_type)  The function to apply to the model outputs to retrieve the scores.
#' @param set_seed (Integer; only for "huggingface"-model_type) Set seed.
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
#' @name textPredict
#' @aliases textAssess textClassify
#' @export
textPredict <- function(
    # Common parameter
    model_info = NULL,
    texts = NULL,
    model_type = "text",
    # text-model specific parameters
    word_embeddings = NULL,
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
    # pipe-model specific parameters
    tokenizer_parallelism = FALSE,
    logging_level = "error",
    force_return_results = FALSE,
    return_all_scores = FALSE,
    function_to_apply = NULL,
    set_seed = 202208,
    ...){

  if (!(model_type %in% c("text", "huggingface"))) {
    stop("Error: method must be either 'text' or 'huggingface'.")
  }

  if(model_type == "text"){

    print("You are using a 'text-trained model' (i.e., model_type = 'text').")

    results <-  textPredictR(
     model_info = model_info,
     word_embeddings = word_embeddings,
     texts = texts,
     x_append = x_append,
     type = type,
     dim_names = dim_names,
     save_model = save_model,
     threshold = threshold,
     show_texts = show_texts,
     device = device,
     participant_id = participant_id,
     save_embeddings = save_embeddings,
     save_dir = save_dir,
     save_name = save_name,
     story_id = story_id,
     dataset_to_merge_predictions = dataset_to_merge_predictions,
     previous_sentence = previous_sentence,
     ...)
  }

  if(model_type == "huggingface"){
    print("You are using a fine-tuned model (i.e., model_type = 'huggingface').")

    results <-  textClassifyPipe(
      x = texts,
      model = model_info,
      device = device,
      tokenizer_parallelism = tokenizer_parallelism,
      logging_level = logging_level,
      force_return_results = force_return_results,
      return_all_scores = return_all_scores,
      function_to_apply = function_to_apply,
      set_seed = set_seed,
      ...)

  }

  return(results)
}

# Alias functions
textAssess <- textPredict
textClassify <- textPredict
