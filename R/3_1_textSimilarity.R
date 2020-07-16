
# Is normalizeV function used somewhere?

########################################################################
########
########     textSimilarity
########
########################################################################

#  devtools::document()
#' Compute cosine
#'
#' @param x A word embedding.
#' @param y A word embedding.
#' @return Cosine between x and y.
#' @noRd
# Computing the cosine between two word embeddings.
cosines <- function(x, y) {
  rowSums(x * y, na.rm = TRUE) / (sqrt(rowSums(x * x, na.rm = TRUE)) * sqrt(rowSums(y * y, na.rm = TRUE)))
}

#  devtools::document()
#' Function to normalize the vector to one; to a unit vector.
#'
#' @param x a word embedding
#' @return normalized (unit) vector/word embedding.
#' @noRd
normalizeV <- function(x) {
  x / sqrt(sum(x^2, na.rm = TRUE))
}

#' Compute the cosine semantic similarity between two text variables.
#'
#' @param x Word embeddings from textEmbed.
#' @param y Word embeddings from textEmbed.
#' @return A vector comprising cosine semantic similarity scores.
#' @examples
#' library(dplyr)
#' wordembeddings <- wordembeddings4_10
#' similiarty_scores <- textSimilarity(wordembeddings$harmonytext, wordembeddings$satisfactiontext)
#' @seealso see \code{\link{textSimilarityNorm}} and \code{\link{textDiff}}
#' @export
textSimilarity <- function(x, y) {
  # Select necessary columns
  x1 <- dplyr::select(x, dplyr::starts_with("Dim"))
  y1 <- dplyr::select(y, dplyr::starts_with("Dim"))

  # Apply the cosines functions
  cosines(x1, y1)
}

########################################################################
########
########     semSimilarityNorm: Compute Semantic similarity between a column and a word-norm
########
########################################################################

# devtools::document()
#' Compute the semantic similarity between a text variable and a word norm
#' (i.e., a text represented by one word embedding that represent a construct).
#' @param x Word embeddings from textEmbed (with several rows of text).
#' @param y Word embedding from textEmbed (from only one text).
#' @return A vector comprising cosine semantic similarity scores.
#' @examples
#' \dontrun{
#' library(dplyr)
#' library(tibble)
#' harmonynorm <- c("harmony peace ")
#' satisfactionnorm <- c("satisfaction achievement")
#'
#' norms <- tibble::tibble(harmonynorm, satisfactionnorm)
#' wordembeddings <- wordembeddings4_10
#' wordembeddings_wordnorm <- textEmbed(norms)
#' similarity_scores <- textSimilarityNorm(
#'   wordembeddings$harmonytext,
#'   wordembeddings_wordnorm$harmonynorm
#' )
#' }
#' @seealso see \code{\link{textSimilarity}} and \code{\link{textDiff}}
#' @importFrom dplyr row_number slice select starts_with
#' @export
textSimilarityNorm <- function(x, y) {
  # Select Dimensions
  x1 <- dplyr::select(x, dplyr::starts_with("Dim"))
  y1 <- dplyr::select(as_tibble(as.list(y)), dplyr::starts_with("Dim"))

  y2 <- y1 %>%
    dplyr::slice(rep(row_number(), nrow(x1)))

  # Apply the cosines functions
  cosines(x1, y2)
}
