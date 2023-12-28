
#' Takes all words as input and arrange them in column with an accompanying column with frequency.
#' @param words Words
#' @return Column with all words and an accompanying column with their frequency.
#' @importFrom tibble as_tibble
#' @noRd
unique_freq_words <- function(words) {
  # Make all words lower case
  words <- tolower(words)

  # separate words/tokens combined with /
  words <- gsub("/", " ", words)

  # Tokenize with nltk
  nltk <- reticulate::import("nltk")
  tokenizerNLTK <- nltk$tokenize$word_tokenize
  words_group <- unlist(lapply(words, tokenizerNLTK))

  words_groupb <- tibble::as_tibble(words_group)
  sort(words_groupb$value)
  words_groupb <- table(words_groupb)
  words_groupb_freq <- tibble::as_tibble(words_groupb, .name_repair = make.names)
  colnames(words_groupb_freq) <- c("words", "n")
  words_groupb_freq
}

#' Make x and y into same length for when we will randomly draw K-folds from them
#' Function to add rows of NA until y and x have the same number of rows.
#' @param x a variable
#' @param y a variable
#' @return x and y have equal length.
#' @noRd
addEqualNrNArows <- function(x, y) {
  success <- FALSE
  while (!success) {
    # Add row with NA
    x <- rbind(x, rep(NA, length(x)))
    # check for success
    success <- nrow(x) == nrow(y)
  }
  return(x)
}


#' Examine how the ordered data's mean of a statistics compare,
#' with the random data's null comparison distribution.
#' @param Observedresult a value representing the observed cosine.
#' @param NULLresults a tibble column with a NULL distribution of estimates (cosines).
# #' @param Npermutations number of permutation used in the test.
#' @param alternative type of test: "two_sided", "greater", "less".
#' @return p_value
#' @noRd
p_value_comparing_with_Null <- function(Observedresult,
                                        NULLresults,
                                        alternative = c("two_sided", "less", "greater")) {

  #  NULLresults= c(1:10, NA) Observedresult = 1 NA alternative = "two_sided"
  NULLresults <- NULLresults %>%
    tibble::as_tibble_col() %>%
    tidyr::drop_na()

  p_left <- sum(NULLresults <= Observedresult) / nrow(NULLresults)
  p_right <- sum(NULLresults >= Observedresult) / nrow(NULLresults)

  switch(alternative,
    "less" = {
      p_value <- p_left
    },
    "greater" = {
      p_value <- p_right
    },
    "two_sided" = {
      p_value <- min(p_left, p_right) * 2
    }
  )
  if (!is.na(p_value)) {
    if (p_value == 0) {
      p_value <- 1 / (nrow(NULLresults) + 1)
    }
  }
  return(p_value)
}


# Help functions
#' Add numeric variables to word embeddings
#' @param word_embeddings Word embeddings to add variables to.
#' @param data Variables to be added to the word embeddings before training.
#' @param append_first Option to add variables before or after all word embeddings.
#' @return Object of word embeddings with added variables referred to as Dim0X_names.
#' @examples
#' \donttest{
#' embeddings_with_variables <- add_variables_to_we(word_embeddings_4[1],
#'   Language_based_assessment_data_8[c(6, 7)],
#'   append_first = TRUE
#' )
#' }
#' @importFrom dplyr bind_cols
#' @importFrom purrr map
#' @importFrom tibble as_tibble
#' @noRd
add_variables_to_we <- function(word_embeddings,
                                data,
                                append_first = FALSE) {

  # Add Names to new Variables
  colnames(data) <- paste("Dim0", "_", colnames(data), sep = "") # 1:ncol(data),

  # Remove single_we if exist
  word_embeddings$singlewords_we <- NULL

  # If not list of word embeddings
  if (!is.data.frame(word_embeddings)) {

    # Add append_first
    if (append_first == TRUE) ratings_embeddings <- purrr::map(word_embeddings, ~ cbind(data, .x))
    # Add last
    if (append_first == FALSE) ratings_embeddings <- purrr::map(word_embeddings, ~ cbind(.x, data))

    ratings_embeddings_tibble <- lapply(ratings_embeddings, tibble::as_tibble)
  }

  # If list of word embeddings
  if (is.data.frame(word_embeddings)) {

    # Add append_first
    if (append_first == TRUE) ratings_embeddings_tibble <- dplyr::bind_cols(data, word_embeddings)
    # Add last
    if (append_first == FALSE) ratings_embeddings_tibble <- dplyr::bind_cols(word_embeddings, data)
  }

  return(ratings_embeddings_tibble)
}

#' Sorting out word_embeddings and x_append for training and predictions
#'
#' @param x word embeddings
#' @param x_append other variables than word embeddings used in training (e.g., age).
#' @param append_first Option to add variables before or after all word embeddings.
#' @return List with sorted tibble of variables, x_name, embedding_description,
#' x_append_names, and variable_name_index_pca.
#' @noRd
sorting_xs_and_x_append <- function(x, x_append, append_first, ...) {
  variable_name_index_pca <- NA

  if (!is.null(x)) {
    # In case the embedding is in list form get the tibble form
    if (!tibble::is_tibble(x) & length(x) == 1) {
      x1 <- x[[1]]
      # Get names for description
      x_name <- names(x)
      # Get embedding info to save for model description
      embedding_description <- comment(x[[1]])
      # In case there are several embeddings in list form get the x_names and
      # embedding description for model description
    } else if (!tibble::is_tibble(x) & length(x) > 1) {
      x_name <- names(x)
      x_name <- paste(x_name, sep = " ", collapse = " & ")
      x_name <- paste("input:", x_name, sep = " ", collapse = " ")

      embedding_description <- comment(x[[1]])
      # In case it is just one word embedding as tibble
    } else {
      x1 <- x
      x_name <- deparse(substitute(x))
      embedding_description <- comment(x)
    }
  }

  # Get names for the added variables to save to description
  x_append_names <- paste(names(x_append), collapse = ", ")
  # Possibility to train without word embeddings
  if (is.null(x)) {
    x1 <- x_append
    x_append <- NULL
    colnames(x1) <- paste0(
      "Dim0", "_",
      colnames(x1)
    )
    x_name <- NULL
    embedding_description <- NULL
  }

  ############ Arranging word embeddings to be concatenated from different texts ############
  ##################################################

  if (!tibble::is_tibble(x) & length(x) > 1) {

    # Select all variables that starts with Dim in each dataframe of the list.
    xlist <- lapply(x, function(X) {
      X <- dplyr::select(X, dplyr::starts_with("Dim"))
    })

    Nword_variables <- length(xlist)
    # Give each column specific names with indexes so that they can be handled separately in the PCAs
    for (i in 1:Nword_variables) {
      colnames(xlist[[i]]) <- paste("DimWs", i, ".", colnames(xlist[[i]]), sep = "")
    }

    # Make vector with each index so that we can allocate them separately for the PCAs
    variable_name_index_pca <- list()
    for (i in 1:Nword_variables) {
      variable_name_index_pca[i] <- paste("DimWs", i, sep = "")
    }

    # Make one df rather then list.
    x1 <- dplyr::bind_cols(xlist)
  }
  ############ End for multiple word embeddings ############
  ##########################################################

  #### Add other variables to word embeddings x_append=NULL
  if (!is.null(x_append)) {
    x1 <- add_variables_to_we(
      word_embeddings = x1,
      data = x_append,
      append_first = append_first,
      ...
    )
  }

  x1 <- dplyr::select(x1, dplyr::starts_with("Dim"))
  variables_names <- list(
    x1, x_name, embedding_description,
    x_append_names, variable_name_index_pca
  )
  names(variables_names) <- c(
    "x1", "x_name", "embedding_description",
    "x_append_names", "variable_name_index_pca"
  )

  return(variables_names)
}


#' Cohen's D effect size
#'
#' @param x a variable.
#' @param y a variable..
#' @return p_value
#' @importFrom stats var
#' @noRd
cohens_d <- function(x, y) {
  lx <- length(x) - 1
  ly <- length(y) - 1

  # mean difference (numerator)
  md <- abs(mean(x) - mean(y))
  # Sigma; denominator
  csd <- lx * var(x) + ly * var(y)
  csd <- csd / (lx + ly)
  csd <- sqrt(csd)

  cd <- md / csd
  # Cohen's d
  cd
}


#' Extract part of a comment
#'
#' @param comment (string) The comment
#' @param part (string) The part to be extracted ("model" or "layers").
#' @return string from the comment
#' @noRd
extract_comment <- function(comment,
                            part) {
  if (part == "model") {
    model_text <- sub(".*textEmbedRawLayers: model: ", "", comment)
    output <- sub(" ; layers.*", "", model_text)
  }

  if (part == "layers") {
    layer_text <- sub(".*layers: ", "", comment)
    output <- sub(" ; word_type_embeddings:.*", "", layer_text)
  }

  return(output)
}



#wanted_file <- "https://raw.githubusercontent.com/adithya8/ContextualEmbeddingDR/master/models/fb20/scalar.csv"
#' Name to Path
#' See if file exist in "inst/extdata/"
#' if file does not exist download it.
#' @param wanted_file (string) Name of or URL to file.
#' @return string path to file.
#' @importFrom utils download.file
#' @noRd
path_exist_download_files <- function(wanted_file) {

  destfile <- list.files(path = system.file("extdata/",
                                            "", #file_name,
                                            package = "text",
                                            mustWork = TRUE),
                         pattern = "")

  # Check if already downloaded; and if not, download
  if (startsWith(wanted_file, "http:")  |
      startsWith(wanted_file, "https:") |
      startsWith(wanted_file, "www.") ) {

    # Get file names to check if already downloaded
    file_name <- basename(wanted_file)

    # Download if not there
    if (!file_name %in% destfile){

      utils::download.file(url = wanted_file,
                           destfile = paste(system.file("extdata/",
                                                        "", #file_name,
                                                  # envir = NULL,
                                                  package = "text",
                                                  mustWork = TRUE
                           ), "/", file_name, sep = ""),
                           method = "auto")
    }

    path_to_file <-  system.file("extdata/",
                file_name,
                # envir = NULL,
                package = "text",
                mustWork = TRUE
                )

  } else if (wanted_file %in% destfile) {

    path_to_file <- system.file("extdata/",
                                wanted_file,
                                # envir = NULL,
                                package = "text",
                                mustWork = TRUE
    )
  }
  return(path_to_file)
}


######################################                          ###################################### 
###################################### Implicit motives section ###################################### 
######################################                          ###################################### 

#' Returns a tibble with values relevant for calculating implicit motives 
#' @param texts Texts to predict
#' @param user_id A column with user ids. 
#' @param predicted_scores2 Predictions from textPredict. 
#' @return Returns a tibble with values relevant for calculating implicit motives 
#' @noRd
implicit_motives <- function(texts, user_id, predicted_scores2){
  
  # Create a table with the number of sentences per user 
  table_uniques2 <- table(user_id[1:dim(predicted_scores2)[1]])
  num_persons <- length(table_uniques2)
  
  # Define variables 
  user_id_column <- c()
  current <- 0
  # Create user_id_column
  for (i in 1:num_persons) {
    current <- current + table_uniques2[[i]]
    user_id_column <- c(user_id_column, user_id[current])
  }
  
  # Create dataframe 
  summations <- data.frame(
    OUTCOME_USER_SUM_CLASS = numeric(num_persons),
    OUTCOME_USER_SUM_PROB = numeric(num_persons),
    wc_person_per_1000 = numeric(num_persons),
    user_ids = numeric(num_persons)
  )
  
  # Summarize classes and probabilities (for the first row)
  summations[1, c("OUTCOME_USER_SUM_CLASS", "OUTCOME_USER_SUM_PROB")] <- c(
    OUTCOME_USER_SUM_CLASS = sum(as.numeric(predicted_scores2[[1]][1:table_uniques2[[1]]]), na.rm = TRUE),
    OUTCOME_USER_SUM_PROB = sum(as.numeric(predicted_scores2[[3]][1:table_uniques2[[1]]]), na.rm = TRUE)
  )
  
  # Summarize classes and probabilities (for the rest of the rows)
  for (user_ids in 2:length(table_uniques2)) {
    start_idx <- sum(table_uniques2[1:(user_ids - 1)]) + 1
    end_idx <- sum(table_uniques2[1:user_ids])
    
    summations[user_ids, c("OUTCOME_USER_SUM_CLASS", "OUTCOME_USER_SUM_PROB")] <- c(
      OUTCOME_USER_SUM_CLASS = sum(as.numeric(predicted_scores2[[1]][start_idx:end_idx]), na.rm = TRUE),
      OUTCOME_USER_SUM_PROB = sum(as.numeric(predicted_scores2[[3]][start_idx:end_idx]), na.rm = TRUE)
    )
  }
  
  # Calculate wc_person_per_1000 (for the first row)
  summations[1, "wc_person_per_1000"] <- sum(lengths(strsplit(texts[1:table_uniques2[[1]]], ' ')), na.rm = TRUE) / 1000
  
  # Calculate wc_person_per_1000 (for the rest of the rows)
  for (user_ids in 2:length(table_uniques2)) {
    # must start on index of the next user, therefore +1
    start_idx <- sum(table_uniques2[1:(user_ids - 1)]) + 1
    end_idx <- sum(table_uniques2[1:user_ids])
    
    summations[user_ids, "wc_person_per_1000"] <- sum(lengths(strsplit(texts[start_idx:end_idx], ' ')), na.rm = TRUE) / 1000
  }
  
  summations["user_ids"] <- user_id_column
  return(summations)
}

#' implicit_motives_pred returns residuals from robust linear regression. 
#' @param sqrt_implicit_motives Tibble returned from function implicit_motives. 
#' @return implicit_motives_pred returns residuals from robust linear regression. 
#' @noRd
implicit_motives_pred <- function(sqrt_implicit_motives){
  #square root transform
  sqrt_implicit_motives[1:3] <- sqrt(sqrt_implicit_motives[1:3])
  # For OUTCOME_USER_SUM_PROB
  lm.OUTCOME_USER_SUM_PROB <- stats::lm(OUTCOME_USER_SUM_PROB  ~ wc_person_per_1000, data = sqrt_implicit_motives)
  OUTCOME_USER_SUM_PROB.residual1 <- resid(lm.OUTCOME_USER_SUM_PROB)
  OUTCOME_USER_SUM_PROB.residual1.z <- scale(OUTCOME_USER_SUM_PROB.residual1)
  
  # For OUTCOME_USER_SUM_CLASS
  lm.OUTCOME_USER_SUM_CLASS <- stats::lm(OUTCOME_USER_SUM_CLASS  ~ wc_person_per_1000, data = sqrt_implicit_motives)
  OUTCOME_USER_SUM_CLASS.residual1 <- resid(lm.OUTCOME_USER_SUM_CLASS)
  OUTCOME_USER_SUM_CLASS.residual1.z <- scale(OUTCOME_USER_SUM_CLASS.residual1)
  
  # Insert residuals into a tibble
  implicit_motives_pred <- tibble::tibble(
    user_id = sqrt_implicit_motives$user_id,
    person_prob = as.vector(OUTCOME_USER_SUM_PROB.residual1.z),
    person_class = as.vector(OUTCOME_USER_SUM_CLASS.residual1.z)
  )
  
  return(implicit_motives_pred)
}

#' Separates text sentence-wise and adds additional sentences to new rows with correpsonding user_id:s. 
#' @param df Dataframe with two columns, user_id and texts. 
#' @return Returns a tibble with user_id:s and texts, where each user_id is matched to an individual sentence. 
#' @noRd
update_user_and_texts <- function(df) {
  updated_user_id <- integer()
  updated_texts <- character()
  
  for (i in seq_along(df$user_id)) {
    # split sentences on ".", "!", or "?"
    sentences <- stringi::stri_split(df$texts[i], regex = "[.!?]", simplify = TRUE)
    
    # remove any empty sentences
    sentences <- sentences[sentences != ""]
    
    # if more than one sentence, repeat user_id and create a vector of updated texts
    current_user_id <- rep(df$user_id[i], length(sentences))
    current_texts <- sentences
    
    # check if the "next" sentence should be split based on its length (if it exceeds two words)
    split_indices <- sapply(current_texts, function(sentence) {
      length(unlist(stringi::stri_split(sentence, regex = "\\s+"))) > 2
    })
    
    # append the updated user_id and texts to the results
    updated_user_id <- c(updated_user_id, rep(df$user_id[i], sum(split_indices)))
    updated_texts <- c(updated_texts, current_texts[split_indices])
  }
  
  updated_df <- data.frame(user_id = updated_user_id, texts = updated_texts)
  
  # since empty rows were deleted, any extra must now be added again. 
  missing_rows <- setdiff(df$user_id, updated_df$user_id)
  if (length(missing_rows) > 0) {
    updated_df <- rbind(updated_df, data.frame(user_id = missing_rows, texts = ""))
  }
  
  return(updated_df)
}

#' Wrapper function that prepares the data and returns a list with predictions, class residuals and probability residuals. 
#' @param model_reference Reference to implicit motive model, either github URL or file-path. 
#' @param user_id A column with user ids. 
#' @param predicted_scores2 Predictions from textPredict() function. 
#' @param texts Texts to predict from textPredict() function. 
#' @return Returns a tibble with values relevant for calculating implicit motives 
#' @noRd
implicit_motives_results <- function(model_reference, 
                                     user_id, 
                                     predicted_scores2, 
                                     texts){
  
  #### Make sure there is just one sentence per user_id ####
  
  # prepare dataframe for update_user_and_texts function
  id_and_texts <- data.frame(user_id = user_id, texts = texts)
  
  # correct for multiple sentences per row. 
  update_user_and_texts <- update_user_and_texts(id_and_texts)
  
  # update user_id
  user_id = update_user_and_texts$user_id
  # update texts
  texts = update_user_and_texts$texts
  
  #### Assign correct column name #### 
  lower_case_model <- tolower(model_reference)
  
  if (grepl("power", lower_case_model)){
    column_name <- "power"
  }
  else if (grepl("affiliation", lower_case_model)){
    column_name <- "affiliation"
  }
  else if (grepl("achievement", lower_case_model)){
    column_name <- "achievement"
  }
  else if (model_reference == "achievment" | model_reference == "power" | model_reference == "affiliation" ){
    column_name <- model_reference
  }
  
  if (length(texts) != length(user_id)) {
    stop('texts and user_id must be of same length.')
  }
  
  # Retrieve Data
  implicit_motives <- implicit_motives(texts, user_id, predicted_scores2)
  
  # Predict 
  predicted <- implicit_motives_pred(implicit_motives)
  
  # Full column name
  class_col_name <- paste0(column_name, "_class")
  
  # Change column name in predicted_scores2 
  colnames(predicted_scores2)[1] <- class_col_name
  
  # Change from df to tibble 
  predicted_scores2 <- tibble::as_tibble(predicted_scores2)
  
  # Summarize all predictions
  summary_list <- list(sentence_predictions = predicted_scores2, person_predictions = predicted) 
  
  # Display message to user
  cat(colourise("Predictions of implicit motives are ready!", fg = "green"))
  cat("\n")
  
  return(summary_list)
}

# Function to expand predictions and bind them to the data
bind_predictions <- function(data, predictions) {
  na_rows <- max(0, nrow(data) - nrow(predictions))
  dplyr::bind_rows(predictions,
                   tibble::as_tibble(matrix(NA, 
                                            ncol = ncol(predictions), 
                                            nrow = na_rows)) %>%
                     setNames(names(predictions)))
}

# Function to bind original data with a list of tibbles
bind_data <- function(original_data, prediction_list) {
  for(predictions in prediction_list) {
    original_data <- dplyr::bind_cols(original_data, bind_predictions(original_data, predictions))
  }
  original_data
}

# Usage example (replace with actual tibble names)
# final_data <- bind_data(data, list(tibble1, tibble2))

