
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

#' Compute the cosine semantic similarity between two text variables.
#'
#' @param x Word embeddings from textEmbed.
#' @param y Word embeddings from textEmbed.
#' @return A vector comprising cosine semantic similarity scores.
#' @examples
#' library(dplyr)
#' wordembeddings <- wordembeddings4
#' similiarty_scores <- textSimilarity(wordembeddings$harmonytext, wordembeddings$satisfactiontext)
#' comment(similiarty_scores)
#' @seealso see \code{\link{textSimilarityNorm}} and \code{\link{textSimilarityTest}}
#' @export
textSimilarity <- function(x, y) {
  # Select necessary columns
  x1 <- dplyr::select(x, dplyr::starts_with("Dim"))
  y1 <- dplyr::select(y, dplyr::starts_with("Dim"))

  # Apply the cosines functions
  ss <- cosines(x1, y1)
  # Add information about the used embeddings
  embedding_descriptions_x <- comment(x)
  embedding_descriptions_y <- comment(y)
  comment(ss) <- paste("x embedding = ", embedding_descriptions_x,
                       "y embedding = ", embedding_descriptions_y, sep = ".", collapse = " ")
  ss
}

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
#' wordembeddings <- wordembeddings4
#' wordembeddings_wordnorm <- textEmbed(norms)
#' similarity_scores <- textSimilarityNorm(
#'   wordembeddings$harmonytext,
#'   wordembeddings_wordnorm$harmonynorm
#' )
#' }
#' @seealso see \code{\link{textSimilarity}} and \code{\link{textSimilarityTest}}
#' @importFrom dplyr row_number slice select starts_with
#' @export
textSimilarityNorm <- function(x, y) {
  # Select Dimensions
  x1 <- dplyr::select(x, dplyr::starts_with("Dim"))
  y1 <- dplyr::select(as_tibble(as.list(y)), dplyr::starts_with("Dim"))

  y2 <- y1 %>%
    dplyr::slice(rep(row_number(), nrow(x1)))

  # Apply the cosines functions
  ss <- cosines(x1, y2)
  # Add information about the used embeddings
  embedding_descriptions_x <- comment(x)
  embedding_descriptions_y <- comment(y)
  comment(ss) <- paste("x embedding = ", embedding_descriptions_x,
                       "y embedding = ", embedding_descriptions_y, sep = ".", collapse = " ")
  ss
}
