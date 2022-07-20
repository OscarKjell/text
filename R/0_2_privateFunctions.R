

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
#' @param first Option to add variables before or after all word embeddings.
#' @return Object of word embeddings with added variables referred to as Dim0X_names.
#' @examples
#' \donttest{
#' embeddings_with_variables <- add_variables_to_we(word_embeddings_4[1],
#' Language_based_assessment_data_8[c(6,7)],
#' first = TRUE)
#' }
#' @importFrom dplyr bind_cols
#' @importFrom purrr map
#' @importFrom tibble as_tibble
#' @noRd
add_variables_to_we <- function(word_embeddings,
                                data,
                                first=FALSE){

  # Add Names to new Variables
  colnames(data) <- paste("Dim0", "_", colnames(data), sep="") # 1:ncol(data),

  # Remove single_we if exist
  word_embeddings$singlewords_we <- NULL

  # If not list of word embeddings
  if(!is.data.frame(word_embeddings)){

    # Add first
    if(first == TRUE) ratings_embeddings <- purrr::map(word_embeddings, ~cbind(data, .x))
    # Add last
    if(first == FALSE) ratings_embeddings <- purrr::map(word_embeddings, ~cbind(.x, data))

    ratings_embeddings_tibble <- lapply(ratings_embeddings, tibble::as_tibble)
  }

  # If list of word embeddings
  if(is.data.frame(word_embeddings)){

    # Add first
    if(first == TRUE) ratings_embeddings_tibble <- dplyr::bind_cols(data, word_embeddings)
    # Add last
    if(first == FALSE) ratings_embeddings_tibble <- dplyr::bind_cols(word_embeddings, data)
  }

  return(ratings_embeddings_tibble)
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

