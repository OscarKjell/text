#' Takes all words as input and arrange them in column with an accompanying column with frequency.
#' @param words Words
#' @param upper_case use tolower or not
#' @return Column with all words and an accompanying column with their frequency.
#' @importFrom tibble as_tibble
#' @noRd
unique_freq_words <- function(
    words,
    upper_case = TRUE
    ){
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
addEqualNrNArows <- function(
    x,
    y
    ){
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
p_value_comparing_with_Null <- function(
    Observedresult,
    NULLresults,
    alternative = c("two_sided",
                    "less",
                    "greater")
    ){
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
add_variables_to_we <- function(
    word_embeddings,
    data,
    append_first
    ){
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
sorting_xs_and_x_append <- function(
    x,
    x_append,
    append_first,
    ...
    ){
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
cohens_d <- function(
    x,
    y
    ){
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
extract_comment <- function(
    comment,
    part
    ) {

  if (part == "model") {
    model_text <- sub(".*textEmbedRawLayers: model: ", "", comment)
    output <- sub(" ; layers.*", "", model_text)
  }

  if (part == "layers") {
    layer_text <- sub(".*layers: ", "", comment)
    output <- sub(" ; word_type_embeddings:.*", "", layer_text)

    # Convert to numeric vector
    output <- as.numeric(unlist(strsplit(output, " ")))
  }

  if (part == "implementation_method") {
    method_text <- sub(".*implementation: ", "", comment)
    output <- sub(" ;.*", "", method_text)
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


  # penalty_in_final_model
  if (part == "n_remove_threshold") {
    selected_element <- grep("^n_remove_threshold =",
                             comment,
                             value = TRUE)
    mix <- sub(".*n_remove_threshold =", "",
               selected_element)
    output <- as.numeric(mix)
  }

  return(output)
}

#' Generates a simple hash for text imput, which is used in textPredict
#' @param text (character) text.
#' @return hash.
#' @noRd
simple_hash <- function(
    texts
    ) {
  # combine all elements of texts into a single character
  combined_text <- paste0(texts, collapse = "")

  # convert text to ASCII
  ascii_vals <- as.integer(charToRaw(combined_text))

  # create a hash like value
  hash_val <- sum(ascii_vals * seq_along(ascii_vals)) %% 100000
  return(hash_val)
}

# model_info = "depressionselect_robertaL23_phq9_Gu2024"
#' Get URL address from (short) name
#' @param model_info The model information as specified in the L-BAM library
#' @noRd
model_address_lookup <- function(
    model_info,
    lbam_update
    ){

  lbam <- textLBAM(lbam_update = TRUE)

  target_model <- lbam %>%
    dplyr::filter(Name == model_info) %>%
    select(path = Path, model_type = Model_Type)

  # If no information is retrieve from the L-BAM library, set model_type to "fine-tuned", to see whetehr the model
  # is hosted at huggingface
  if (nrow(target_model) == 0){
    target_model <- tibble(
      model_info = model_info,
      model_type = "fine-tuned",
      path = NA
    )
  }

  return(target_model)

}


#' Name to Path
#' See if file exist in "inst/extdata/"
#' if file does not exist download it.
#' @param wanted_file (string) Name of or URL to file.
#' @return string path to file.
#' @importFrom utils download.file
#' @noRd
path_exist_download_files <- function(
    wanted_file
    ) {

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


# Check if the path is an online, internet path
is_internet_path <- function(path) {
  grepl("^(http|https|www)://", path)
}

