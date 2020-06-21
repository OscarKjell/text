
# textSimilairty
# usethis::use_package(psych==1.8.10)


########################################################################
########
########     textSimilarity [NOW I AM NORMALIZING IT; perhaps shouldn't!]
########
########################################################################

#  devtools::document()
#' Compute cosine
#'
#' @param x a word embedding
#' @param y a word embedding
#' @return cosine between x and y.
#' @noRd
# Computing the cosine between two semantic represenations
cosines <- function(x, y) {
  rowSums(x * y, na.rm = TRUE) / (sqrt(rowSums(x * x, na.rm = TRUE)) * sqrt(rowSums(y * y, na.rm = TRUE)))
}

#  devtools::document()
#' Function to normlaise the vector to one; unit vector
#'
#' @param x a word embedding
#' @return normalised (unit) vector/word embedding.
#' @noRd
normalizeV <- function(x) {
  x / sqrt(sum(x^2, na.rm = TRUE))
}

#' textSimilarity computes the semantic similiarty between texts.
#'
#' @param x Wordembeddings from textImport.
#' @param y Wordembeddings from textImport.
#' @return A vector with semantic similarity scores (based on cosine).
#' @examples
#' library(dplyr)
#' wordembeddings <- wordembeddings4_10
#' similiarty_scores <- textSimilarity(wordembeddings$harmonytext, wordembeddings$satisfactiontext)
#' @seealso see \code{\link{textSimilarityNorm}} and \code{\link{textDiff}}
#' @export
textSimilarity <- function(x, y) {
  # Select necassary columns
  x1 <- dplyr::select(x, dplyr::starts_with("V"))
  y1 <- dplyr::select(y, dplyr::starts_with("V"))
  #x1 <- subset(x, select = -c(1:5))
  #y1 <- subset(y, select = -c(1:5))

  # Apply the cosines functions
  cosines(x1, y1)
}

########################################################################
########
########     semSimilarityNorm: Compute Semantic similarity between a column and a word-norm
########
########################################################################
# devtools::document()
# Function get word embeddings and then compute COSINE
# x and y are the semantic representations from column x and y, which have been imported with textImport
#' textSimilarityNorm computes the semantic similiarty between a character variable and a word norm (i.e., a text in one cell).
#'
#' @param x Wordembeddings from textImport (with several rows of text).
#' @param y Wordembeddings from textImport (with only one text).
#' @return A vector with semantic similarity scores (based on cosine).
#' @examples
#'
# harmonynorm <- c("harmony peace cooperation balance")
# satisfactionnorm <- c("satisfaction achievement job fulfilled")
# library(tibble)
# norms <- tibble::tibble(harmonynorm, satisfactionnorm)
# library(dplyr)
# wordembeddings <- wordembeddings4_10
# wordembeddings_wordnorm <- textTransform(norms)
# similiarty_scores <- textSimilarityNorm(
#   wordembeddings$harmonytext,
#   wordembeddings_wordnorm$harmonynorm
# )
#' @seealso see \code{\link{textSimilarity}} and \code{\link{textDiff}}
#' @importFrom dplyr row_number slice select starts_with
#' @export
textSimilarityNorm <- function(x, y) {
  # Remove unnecassary columns
  x1 <- dplyr::select(x, dplyr::starts_with("V"))
  y1 <- dplyr::select(as_tibble(as.list(y)), dplyr::starts_with("V"))
  #x1 <- subset(x, select = -c(1:5))
  #y1 <- subset(y, select = -c(1:5)) help(enframe)
  is.vector(y)

  y2 <- y1 %>%
    dplyr::slice(rep(row_number(), nrow(x1)))

  # Apply the cosines functions
  cosines(x1, y2)
}





