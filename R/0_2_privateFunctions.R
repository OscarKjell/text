#' Takes all words as input and arrange them in column with an accompanying column with frequency.
#' @param words Words
#' @param upper_case use tolower or not
#' @return Column with all words and an accompanying column with their frequency.
#' @importFrom tibble as_tibble
#' @noRd
unique_freq_words <- function(words,
                              upper_case = TRUE) {
  # Make all words lower case
  if (upper_case) words <- tolower(words)

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
  colnames(data) <- paste("Dim0", "_", colnames(data), sep = "")

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
sorting_xs_and_x_append <- function(x,
                                    x_append,
                                    append_first = TRUE, ...) {
  variable_name_index_pca <- NA

  if (!is.null(x)) {
    # In case the embedding is in list form get the tibble form
    if (!tibble::is_tibble(x) && length(x) == 1) {
      x1 <- x[[1]]
      # Get names for description
      x_name <- names(x)
      # Get embedding info to save for model description
      embedding_description <- comment(x[[1]])
      # In case there are several embeddings in list form get the x_names and
      # embedding description for model description
    } else if (!tibble::is_tibble(x) && length(x) > 1) {
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

  if (!tibble::is_tibble(x) && length(x) > 1) {
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
cohens_d <- function(x,
                     y) {
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

  # penalty_in_final_model
  if (part == "penalty_in_final_model") {
    selected_element <- grep("^penalty in final model =",
                             comment,
                             value = TRUE)
    pen <- sub(".*penalty in final model =  ", "",
                      selected_element)
    output <- as.numeric(pen)
  }

  # penalty_in_final_model
  if (part == "mixture_in_final_model") {
    selected_element <- grep("^mixture in final model =",
                             comment,
                             value = TRUE)
    mix <- sub(".*mixture in final model =  ", "",
               selected_element)
    output <- as.numeric(mix)
  }

  return(output)
}

#' Generates a simple hash for text imput, which is used in textPredict
#' @param text (character) text.
#' @return hash.
#' @noRd
simple_hash <- function(texts) {
  # combine all elements of texts into a single character
  combined_text <- paste0(texts, collapse = "")

  # convert text to ASCII
  ascii_vals <- as.integer(charToRaw(combined_text))

  # create a hash like value
  hash_val <- sum(ascii_vals * seq_along(ascii_vals)) %% 100000
  return(hash_val)
}

# wanted_file <- "https://raw.githubusercontent.com/adithya8/ContextualEmbeddingDR/master/models/fb20/scalar.csv"
#' Name to Path
#' See if file exist in "inst/extdata/"
#' if file does not exist download it.
#' @param wanted_file (string) Name of or URL to file.
#' @return string path to file.
#' @importFrom utils download.file
#' @noRd
path_exist_download_files <- function(wanted_file) {
  destfile <- list.files(
    path = system.file("extdata/",
      "", # file_name,
      package = "text",
      mustWork = TRUE
    ),
    pattern = ""
  )

  # Check if already downloaded; and if not, download
  if (startsWith(wanted_file, "http:") ||
        startsWith(wanted_file, "https:") ||
      startsWith(wanted_file, "www.")) {
    # Get file names to check if already downloaded
    file_name <- basename(wanted_file)

    # Download if not there
    if (!file_name %in% destfile) {
      utils::download.file(
        url = wanted_file,
        destfile = paste(system.file("extdata/",
                                     "", # file_name,
                                     # envir = NULL,
                                     package = "text",
                                     mustWork = TRUE
                                     ), "/", file_name, sep = ""),
        method = "auto"
        )
    }

    path_to_file <- system.file("extdata/",
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
                                append_first) {
  # Add Names to new Variables
  colnames(data) <- paste("Dim0", "_", colnames(data), sep = "")

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
sorting_xs_and_x_append <- function(x,
                                    x_append,
                                    append_first,
                                    ...) {
  variable_name_index_pca <- NA

  if (!is.null(x)) {
    # In case the embedding is in list form get the tibble form
    if (!tibble::is_tibble(x) && length(x) == 1) {
      x1 <- x[[1]]
      # Get names for description
      x_name <- names(x)
      # Get embedding info to save for model description
      embedding_description <- comment(x[[1]])
      # In case there are several embeddings in list form get the x_names and
      # embedding description for model description
    } else if (!tibble::is_tibble(x) && length(x) > 1) {
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

  if (!tibble::is_tibble(x) && length(x) > 1) {
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
cohens_d <- function(x,
                     y) {
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

# wanted_file <- "https://raw.githubusercontent.com/adithya8/ContextualEmbeddingDR/master/models/fb20/scalar.csv"
#' Name to Path
#' See if file exist in "inst/extdata/"
#' if file does not exist download it.
#' @param wanted_file (string) Name of or URL to file.
#' @return string path to file.
#' @importFrom utils download.file
#' @noRd
path_exist_download_files <- function(wanted_file) {
  destfile <- list.files(
    path = system.file("extdata/",
      "", # file_name,
      package = "text",
      mustWork = TRUE
    ),
    pattern = ""
  )

  # Check if already downloaded; and if not, download
  if (startsWith(wanted_file, "http:") ||
      startsWith(wanted_file, "https:") ||
      startsWith(wanted_file, "www.")) {
    # Get file names to check if already downloaded
    file_name <- basename(wanted_file)

    # Download if not there
    if (!file_name %in% destfile) {
      utils::download.file(
        url = wanted_file,
        destfile = paste(system.file("extdata/",
          "", # file_name,
          # envir = NULL,
          package = "text",
          mustWork = TRUE
        ), "/", file_name, sep = ""),
        method = "auto"
      )
    }

    path_to_file <- system.file("extdata/",
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


# Function returning TRUE or FALSE depending on model name exist in preregistered list
registered_model_name <- function(model_name_test){

  if(is.character(model_name_test)){

  if (model_name_test[[1]] %in% c(
  "implicit_power_roberta_large_l23_v1",
  "implicit_power_roberta_large_L23_v1",
  "implicit_achievement_roberta_large_l23_v1",
  "implicit_affiliation_roberta_large_l23_v1",
  "implicit_ger_be_l11_to_power",
  "implicit_ger_be_l11_to_achievement",
  "implicit_ger_be_l11_to_affiliation")) {

    true_false <- TRUE
  } else{
    true_false <- FALSE
  }
  } else{
    true_false <- FALSE
  }
  return(true_false)
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
