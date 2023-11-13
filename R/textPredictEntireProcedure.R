#' (experimental) An end-to-end, user-friendly function that takes text as input and provides predictions based on a pre-trained model.
#' @param text_to_predict (character) Text to predict. If this argument is specified, then argument "premade_embeddings" must be set to NULL (default = NULL).
#' @param premade_embeddings (Embeddings from e.g., textEmbed) Embeddings to predict. If this argument is specified, then argument "texts" must be set to NULL (default = NULL).
#' @param model_platform (character) Either "github" or "local" (default = "github"). 
#' @param model_reference (character) Link to github-model (default = "https://github.com/CarlViggo/pretrained_swls_model/raw/main/trained_github_model_logistic.RDS",
#' a model that predicts harmony in life score). 
#' @param save_model (boolean) If set to true, the model will be saved in work-directory (default = FALSE). If TRUE, then argument "model_name" must be defined.  
#' @param model_name (character) Optional. If the 'save_model' argument is set to TRUE and you wish to assign a custom name to your model, 
#' provide a file path along with the desired filename, for example, 'C:/Users/Name/Models/linear_model.RDS' (default is NULL). If this is not defined 
#' but 'save_model' is set to TRUE, the model will be saved as 'imported_model.RDS'.
#' @param type (character) Choose either 'class' or 'prob'. If your model is a logistic or multinomial model, specify whether you want to receive the 
#' model's classification "class" or the underlying probabilities "prob" (default = "class").
#' @param max_token_to_sentence (numeric) This information will be automatically extracted from your model, so this argument is typically not used. 
#' However, if it's not defined in your model_description, you can assign a value here (default = 4). 
#' @param aggregation_from_layers_to_tokens (character) This information will be automatically extracted from your model, so this argument is typically not used. 
#' However, if it's not defined in your model_description, you can assign a value here (default = "concatenate").
#' @param aggregation_from_tokens_to_texts (character) This information will be automatically extracted from your model, so this argument is typically not used. 
#' However, if it's not defined in your model_description, you can assign a value here (default = "mean").
#' @return The textPredictEntireProcedure function takes text as input and returns predictions. The type of prediction returned depends 
#' on the specific pre-trained model. 
#' @examples
#' # (data from Language_based_assessment_data_8)
#' \dontrun{
#'   input <- "I am not in harmony in my life as much as I would like to be.  
#'     I would like to be more peaceful and more productive.  I want to have a balanced life.  
#'     I would like to spend more time with my family and be  more focused on my goals and where 
#'     I want to be in life and what I want to achieve."
#'  
#' # Example 1 (Predict valence from input): 
#'   prediction <- textPredictEntireProcedure(text_to_predict = input)
#'   
#' # Example 2 (Predict valence from premade word-embeddings) (data from Language_based_assessment_data_8): 
#'   
#'   # create embeddings
#'   embeddings <- textEmbed("I am not in harmony in my life as much as I would like to be.")
#'   
#'   # predict from embeddings
#'   prediction <- textPredictEntireProcedure(premade_embeddings = embeddings)
#' 
#' # Example 3 (Use a logistic model to predict gender, return the underlying probabilities and save model): 
#'   prediction <- textPredictEntireProcedure(text_to_predict = input, 
#'                              model_reference = "https://github.com/CarlViggo/pretrained_swls_model/raw/main/trained_github_model_logistic.RDS", 
#'                              save_model = TRUE, 
#'                              type = "prob")
#' }
#' 
#' @seealso See \code{\link{textPredict}}. 
#' @export
textPredictEntireProcedure <- function(
    text_to_predict = NULL,
    premade_embeddings = NULL, 
    model_platform = "github", 
    model_reference = "https://github.com/CarlViggo/pretrained-models/raw/main/trained_hils_model.RDS", 
    save_model = FALSE, 
    model_name = NULL, 
    type = "class",
    max_token_to_sentence = 4, 
    aggregation_from_layers_to_tokens = "concatenate",
    aggregation_from_tokens_to_texts = "mean"
    ) {
    
    
    # Load model from github. 
    if (model_platform == "github"){
      loaded_model <- readRDS(url(model_reference))
      
      # Save model into working-directory and automatically save it as "imported_model".  
      if (save_model == TRUE & is.null(model_name)){
        saveRDS(loaded_model, "imported_model.RDS")
      }
      #  Save model into working-directory and automatically save it as model_name.  
      else if (save_model == TRUE & !is.null(model_name)){
        saveRDS(loaded_model, model_name)
      }
    } 
  
    # Load model from local, "model_reference" should now be its filepath. 
    else if (model_platform == "local") {
      loaded_model <- readRDS(model_reference)
    } 
    else {
      stop('Choose either "github" or "local" as argument: model_platform')
    }
  
    # Check that both text_to_predict and premade_embeddings aren't defined. 
    if (!is.null(text_to_predict) & !is.null(premade_embeddings)) {
      stop('Both arguments: "text_to_predict" and "premade_embeddings" cannot be defined simultaneously. Choose one or the other.')
    }
    
    ###### Create embeddings based on information stored in the pre-trained model ######
  
    if (!is.null(text_to_predict) & is.null(premade_embeddings)){
      
      # Save default values for later use 
      default_max_token_to_sentence <- 4
      default_aggregation_from_layers_to_tokens <- "concatenate"
      default_aggregation_from_tokens_to_texts <- "mean"
      
      # Finds the line number for the line with "impute_missing_setting" (the line above the line we're looking for, which is the model description)
      line_number <- grep("impute_missing_setting", loaded_model$model_description)
      new_line_number <- line_number + 1
      
      # Extracts information about model type and layers. 
      model_type <- extract_comment(loaded_model$model_description[new_line_number], "model")
      model_layers <- as.numeric(extract_comment(loaded_model$model_description[new_line_number], "layers"))
      
      # Extracts the max_token_to_sentence, aggregation_from_layers_to_tokens, aggregation_from_tokens_to_texts from the model. 
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
      embeddings <- textEmbed(texts = text_to_predict,
                              model = model_type, 
                              layers = model_layers,
                              max_token_to_sentence = max_token_to_sentence, 
                              aggregation_from_layers_to_tokens = aggregation_from_layers_to_tokens,
                              aggregation_from_tokens_to_texts = aggregation_from_tokens_to_texts
      )
    } 
    
    # If text isn't provided, but premade word-embeddings, then load them instead. 
    else if (!is.null(premade_embeddings) & is.null(text_to_predict)){
      embeddings <- premade_embeddings
    }
    
    ###### Prediction based on loaded model and created word embeddings ######
  
    # Checks if the model is logistic or multinomial. If so, then introduce the argument "type" in the function call. 
    if (class(loaded_model$final_model$fit$actions$model$spec)[1] == "logistic_reg" | 
        class(loaded_model$final_model$fit$actions$model$spec)[1] == "multinomial") {
      predictions <- textPredict(model_info = loaded_model, word_embeddings = embeddings$texts, type = type)
    }
    # If the model is not multinomial or logistic, then exclude the "type" argument. 
    else{
      predictions <- textPredict(loaded_model, embeddings$texts)
    }
    
    return(predictions)
  
}


#A helper function to textPredict giving it the capabilities of textPredictEntireProcedure. 
#' @param text_to_predict (character) Text to predict. If this argument is specified, then argument "premade_embeddings" must be set to NULL (default = NULL).
#' @param premade_embeddings (Embeddings from e.g., textEmbed) Embeddings to predict. If this argument is specified, then argument "texts" must be set to NULL (default = NULL).
#' @param model_platform (character) Either "github" or "local" (default = "github"). 
#' @param model_reference (character) Link to github-model (default = "https://github.com/CarlViggo/pretrained_swls_model/raw/main/trained_github_model_logistic.RDS",
#' a model that predicts harmony in life score). 
#' @param save_model (boolean) If set to true, the model will be saved in work-directory (default = FALSE). If TRUE, then argument "model_name" must be defined.  
#' @param model_name (character) Optional. If the 'save_model' argument is set to TRUE and you wish to assign a custom name to your model, 
#' provide a file path along with the desired filename, for example, 'C:/Users/Name/Models/linear_model.RDS' (default is NULL). If this is not defined 
#' but 'save_model' is set to TRUE, the model will be saved as 'imported_model.RDS'.
#' @param type (character) Choose either 'class' or 'prob'. If your model is a logistic or multinomial model, specify whether you want to receive the 
#' model's classification "class" or the underlying probabilities "prob" (default = "class").
#' @param max_token_to_sentence (numeric) This information will be automatically extracted from your model, so this argument is typically not used. 
#' However, if it's not defined in your model_description, you can assign a value here (default = 4). 
#' @param aggregation_from_layers_to_tokens (character) This information will be automatically extracted from your model, so this argument is typically not used. 
#' However, if it's not defined in your model_description, you can assign a value here (default = "concatenate").
#' @param aggregation_from_tokens_to_texts (character) This information will be automatically extracted from your model, so this argument is typically not used. 
#' @noRd
textReturnModelAndEmbedding <- function(
    text_to_predict = NULL,
    premade_embeddings = NULL, 
    model_platform = "github", 
    model_reference = "https://github.com/CarlViggo/pretrained-models/raw/main/trained_hils_model.RDS", 
    save_model = FALSE, 
    model_name = NULL, 
    type = "class",
    max_token_to_sentence = 4, 
    aggregation_from_layers_to_tokens = "concatenate",
    aggregation_from_tokens_to_texts = "mean", 
    device = "cpu", 
    keep_token_embeddings = FALSE
) {
  
  # Load model from github. 
  if (model_platform == "github"){
    loaded_model <- readRDS(url(model_reference))
    
    # Save model into working-directory and automatically save it as "imported_model".  
    if (save_model == TRUE & is.null(model_name)){
      saveRDS(loaded_model, "imported_model.RDS")
      print("The model has been downloaded")
    }
    #  Save model into working-directory and automatically save it as model_name.  
    else if (save_model == TRUE & !is.null(model_name)){
      saveRDS(loaded_model, model_name)
      print("The model has been downloaded")
    }
  } 
  
  # Load model from local, "model_reference" should now be its filepath. 
  else if (model_platform == "local") {
    loaded_model <- readRDS(model_reference)
  } 
  else {
    stop('Choose either "github" or "local" as argument: model_platform')
  }
  
  # Check that both text_to_predict and premade_embeddings aren't defined. 
  if (!is.null(text_to_predict) & !is.null(premade_embeddings)) {
    stop('Both arguments: "text_to_predict" and "premade_embeddings" cannot be defined simultaneously. Choose one or the other.')
  }
  
  ###### Create embeddings based on information stored in the pre-trained model ######
  
  if (!is.null(text_to_predict) & is.null(premade_embeddings)){
    
    # Save default values for later use 
    default_max_token_to_sentence <- 4
    default_aggregation_from_layers_to_tokens <- "concatenate"
    default_aggregation_from_tokens_to_texts <- "mean"
    
    # Finds the line number for the line with "impute_missing_setting" (the line above the line we're looking for, which is the model description)
    line_number <- grep("impute_missing_setting", loaded_model$model_description)
    new_line_number <- line_number + 1
    
    # Extracts information about model type and layers. 
    model_type <- extract_comment(loaded_model$model_description[new_line_number], "model")
    model_layers <- as.numeric(extract_comment(loaded_model$model_description[new_line_number], "layers"))
    
    # Extracts the max_token_to_sentence, aggregation_from_layers_to_tokens, aggregation_from_tokens_to_texts from the model. 
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
    embeddings <- textEmbed(texts = text_to_predict,
                            model = model_type, 
                            layers = model_layers,
                            max_token_to_sentence = max_token_to_sentence, 
                            aggregation_from_layers_to_tokens = aggregation_from_layers_to_tokens,
                            aggregation_from_tokens_to_texts = aggregation_from_tokens_to_texts, 
                            device = device, 
                            keep_token_embeddings = keep_token_embeddings
    )
  } 
  
  # If text isn't provided, but premade word-embeddings, then load them instead. 
  else if (!is.null(premade_embeddings) & is.null(text_to_predict)){
    embeddings <- premade_embeddings
  }
  
  # store classes 
  classes <- loaded_model$final_recipe$levels$y$values
  
  emb_and_model <- list(loaded_model = loaded_model, embeddings = embeddings, classes = classes)
  return(emb_and_model)
}