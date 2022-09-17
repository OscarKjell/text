
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

#' Compute the semantic similarity between two text variables.
#'
#' @param x Word embeddings from textEmbed.
#' @param y Word embeddings from textEmbed.
#' @param method Character string describing type of measure to be computed. Default is "cosine" (see also
#' "spearmen", "pearson" as well as measures from textDistance() (which here is computed as 1 - textDistance)
#' including "euclidean", "maximum", "manhattan", "canberra", "binary" and "minkowski").
#' @param center (boolean; from base::scale) If center is TRUE then centering is done by subtracting the column means
#' (omitting NAs) of x from their corresponding columns, and if center is FALSE, no centering is done.
#' @param scale (boolean; from base::scale) If scale is TRUE then scaling is done by dividing the (centered) columns of x
#' by their standard deviations if center is TRUE, and the root mean square otherwise.
#' @return A vector comprising semantic similarity scores.
#' @examples
#' library(dplyr)
#' similarity_scores <- textSimilarity(
#'   x = word_embeddings_4$texts$harmonytext,
#'   y = word_embeddings_4$texts$satisfactiontext
#' )
#' comment(similarity_scores)
#' @seealso see \code{\link{textDistance}}, \code{\link{textSimilarityNorm}} and \code{\link{textSimilarityTest}}
#' @export
textSimilarity <- function(x,
                           y,
                           method = "cosine",
                           center = TRUE,
                           scale = FALSE) {

  # Select necessary columns
  x1 <- dplyr::select(x, dplyr::starts_with("Dim"))
  y1 <- dplyr::select(y, dplyr::starts_with("Dim"))

  # Normalize embeddings
  x1 <- tibble::as_tibble(scale(x1, center = center, scale = scale))
  y1 <- tibble::as_tibble(scale(y1, center = center, scale = scale))

  # Compute cosines
  if (method == "cosine") {
    ss <- cosines(x1, y1)
  }

  # Compute correlation
  if (method == "spearman" | method == "pearson") {
    # i =1
    for (i in seq_len(nrow(x1))) {
      ss[[i]] <- cor(as_vector(x1[i, ]),
        as_vector(y1[i, ]),
        method = method
      )
    }
    ss <- unlist(ss)
    ss
  }

  if (method %in% c(
    "euclidean", "maximum", "manhattan",
    "canberra", "binary", "minkowski"
  )) {
    ss <- 1 - textDistance(x1, y1, method = method)
  }

  # Add information about the used embeddings
  embedding_descriptions_x <- comment(x)
  embedding_descriptions_y <- comment(y)
  comment(ss) <- paste(
    "x embedding = ", embedding_descriptions_x,
    "y embedding = ", embedding_descriptions_y,
    "method = ", method,
    "center = ", center,
    "scale = ", scale,
    sep = ".", collapse = " "
  )
  ss
}

# x = word_embeddings_4$texts$harmonytext
# y = word_embeddings_4$texts$satisfactiontext
#
# ss_cos <- textSimilarity(x, y,
#                         method = "cosine",
#                         center = TRUE,
#                         scale = FALSE)
#
# ss_spearman <- textSimilarity(x, y,
#                         method = "spearman",
#                         center = TRUE,
#                         scale = FALSE)
#
# ss_pearson <- textSimilarity(x, y,
#                         method = "pearson",
#                         center = TRUE,
#                         scale = FALSE)
#
# mean(ss_cos)
# mean(ss_spearman)
# mean(ss_pearson)


#' Compute the semantic distance between two text variables.
#'
#' @param x Word embeddings (from textEmbed).
#' @param y Word embeddings (from textEmbed).
#' @param method Character string describing type of measure to be computed; default is "euclidean" (see also
#' measures from stats:dist() including "maximum", "manhattan", "canberra", "binary" and "minkowski".
#' It is also possible to use "cosine", which computes the cosine distance (i.e., 1 - cosine(x, y)).
#' @param center (boolean; from base::scale) If center is TRUE then centering is done by subtracting the column means
#' (omitting NAs) of x from their corresponding columns, and if center is FALSE, no centering is done.
#' @param scale (boolean; from base::scale) If scale is TRUE then scaling is done by dividing the (centered) columns of x
#' by their standard deviations if center is TRUE, and the root mean square otherwise.
#' @return A vector comprising semantic distance scores.
#' @examples
#' library(dplyr)
#' distance_scores <- textDistance(
#'   x = word_embeddings_4$texts$harmonytext,
#'   y = word_embeddings_4$texts$satisfactiontext
#' )
#' comment(distance_scores)
#' @seealso see  \code{\link{textSimilarity}}, \code{\link{textSimilarityNorm}} and \code{\link{textSimilarityTest}}
#' @export
textDistance <- function(x,
                         y,
                         method = "euclidean",
                         center = FALSE,
                         scale = FALSE) {
  x1 <- textDimName(x, dim_names = FALSE)
  y1 <- textDimName(y, dim_names = FALSE)

  # Select necessary columns
  x1 <- dplyr::select(x1, dplyr::starts_with("Dim"))
  y1 <- dplyr::select(y1, dplyr::starts_with("Dim"))

  # normalize
  x1 <- as_tibble(scale(x1, center = center, scale = scale))
  y1 <- as_tibble(scale(y1, center = center, scale = scale))

  if (method == "cosine") {
    # Compute cosine distance
    ss <- 1 - cosines(x1, y1)
  } else {
    # Compute distance method = "euclidean"
    ss1 <- list()
    # i=1
    for (i in seq_len(nrow(x1))) {
      dist_df <- dplyr::bind_rows(x1[i, ], y1[i, ])
      ss1[i] <- stats::dist(dist_df, method = method)[1]
    }
    ss <- unlist(ss1)
  }

  # Add information about the used embeddings
  embedding_descriptions_x <- comment(x)
  embedding_descriptions_y <- comment(y)
  comment(ss) <- paste(
    "x embedding = ", embedding_descriptions_x,
    "y embedding = ", embedding_descriptions_y,
    "method = ", method,
    "center = ", center,
    "scale = ", scale,
    sep = ".", collapse = " "
  )
  ss
}


#' Compute semantic similarity scores between all combinations in a word embedding
#' @inheritParams textSimilarity
#' @return A matrix of semantic similarity scores
#' @examples
#' similarity_scores <- textSimilarityMatrix(word_embeddings_4$texts$harmonytext[1:3, ])
#' round(similarity_scores, 3)
#' @seealso see \code{\link{textSimilarityNorm}} and \code{\link{textSimilarityTest}}
#' @export
textSimilarityMatrix <- function(x,
                                 method = "cosine",
                                 center = FALSE,
                                 scale = FALSE) {
  ss_matrix <- matrix(nrow = nrow(x), ncol = nrow(x))

  for (i in seq_len(nrow(x))) {
    for (j in seq_len(nrow(x))) {
      ss_matrix[i, j] <- text::textSimilarity(
        x[i, ],
        x[j, ],
        method,
        scale = scale,
        center = center
      )
    }
  }
  ss_matrix
}

#' Compute semantic distance scores between all combinations in a word embedding
#' @inheritParams textDistance
#' @return A matrix of semantic distance scores
#' @examples
#' distance_scores <- textDistanceMatrix(word_embeddings_4$texts$harmonytext[1:3, ])
#' round(distance_scores, 3)
#' @seealso see \code{\link{textDistanceNorm}} and \code{\link{textSimilarityTest}}
#' @export
textDistanceMatrix <- function(x,
                               method = "euclidean",
                               center = FALSE,
                               scale = FALSE) {
  ss_matrix <- matrix(nrow = nrow(x), ncol = nrow(x))

  for (i in seq_len(nrow(x))) {
    for (j in seq_len(nrow(x))) {
      ss_matrix[i, j] <- text::textDistance(
        x[i, ],
        x[j, ],
        method,
        center = center,
        scale = scale
      )
    }
  }
  ss_matrix
}



#' Compute the semantic similarity between a text variable and a word norm
#' (i.e., a text represented by one word embedding that represent a construct).
#' @param y Word embedding from textEmbed (from only one text).
#' @inheritParams textSimilarity
#' @return A vector comprising semantic similarity scores.
#' @examples
#' \dontrun{
#' library(dplyr)
#' library(tibble)
#' harmonynorm <- c("harmony peace ")
#' satisfactionnorm <- c("satisfaction achievement")
#'
#' norms <- tibble::tibble(harmonynorm, satisfactionnorm)
#' word_embeddings <- word_embeddings_4$texts
#' word_embeddings_wordnorm <- textEmbed(norms)
#' similarity_scores <- textSimilarityNorm(
#'   word_embeddings$harmonytext,
#'   word_embeddings_wordnorm$harmonynorm
#' )
#' }
#' @seealso see \code{\link{textSimilarity}} and \code{\link{textSimilarityTest}}
#' @importFrom dplyr row_number slice select starts_with
#' @export
textSimilarityNorm <- function(x,
                               y,
                               method = "cosine",
                               center = FALSE,
                               scale = FALSE) {
  # Select Dimensions
  x1 <- dplyr::select(x, dplyr::starts_with("Dim"))
  y1 <- dplyr::select(as_tibble(as.list(y)), dplyr::starts_with("Dim"))

  y2 <- y1 %>%
    dplyr::slice(rep(dplyr::row_number(), nrow(x1)))

  # Compute similarity
  ss <- textSimilarity(x1, y2,
    method = method,
    center = center,
    scale = scale
  )

  # Add information about the used embeddings
  embedding_descriptions_x <- comment(x)
  embedding_descriptions_y <- comment(y)
  comment(ss) <- paste("x embedding = ", embedding_descriptions_x,
    "y embedding = ", embedding_descriptions_y,
    method,
    sep = ".", collapse = " "
  )
  ss
}



#' Compute the semantic distance between a text variable and a word norm
#' (i.e., a text represented by one word embedding that represent a construct/concept).
#' @param y Word embedding from textEmbed (from only one text).
#' @inheritParams textDistance
#' @return A vector comprising semantic distance scores.
#' @examples
#' \dontrun{
#' library(dplyr)
#' library(tibble)
#' harmonynorm <- c("harmony peace ")
#' satisfactionnorm <- c("satisfaction achievement")
#'
#' norms <- tibble::tibble(harmonynorm, satisfactionnorm)
#' word_embeddings <- word_embeddings_4$texts
#' word_embeddings_wordnorm <- textEmbed(norms)
#' similarity_scores <- textDistanceNorm(
#'   word_embeddings$harmonytext,
#'   word_embeddings_wordnorm$harmonynorm
#' )
#' }
#' @seealso see \code{\link{textDistance}} and \code{\link{textSimilarityTest}}
#' @importFrom dplyr row_number slice select starts_with
#' @export
textDistanceNorm <- function(x,
                             y,
                             method = "euclidean",
                             center = FALSE,
                             scale = FALSE) {
  # Select Dimensions
  x1 <- dplyr::select(x, dplyr::starts_with("Dim"))
  y1 <- dplyr::select(as_tibble(as.list(y)), dplyr::starts_with("Dim"))

  y2 <- y1 %>%
    dplyr::slice(rep(dplyr::row_number(), nrow(x1)))

  # Compute similarity
  ss <- textDistance(x1, y2,
    method = method,
    center = center,
    scale = scale
  )

  # Add information about the used embeddings
  embedding_descriptions_x <- comment(x)
  embedding_descriptions_y <- comment(y)
  comment(ss) <- paste("x embedding = ", embedding_descriptions_x,
    "y embedding = ", embedding_descriptions_y,
    method,
    sep = ".", collapse = " "
  )
  ss
}
