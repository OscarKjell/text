
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
#' @param method (character) Character string describing type of measure to be computed. Default is "cosine" (see also
#' "spearmen", "pearson" as well as measures from textDistance() (which here is computed as 1 - textDistance)
#' including "euclidean", "maximum", "manhattan", "canberra", "binary" and "minkowski").
#' @param center (boolean; from base::scale) If center is TRUE then centering is done by subtracting the column means
#' (omitting NAs) of x from their corresponding columns, and if center is FALSE, no centering is done.
#' @param scale (boolean; from base::scale) If scale is TRUE then scaling is done by dividing the (centered)
#' columns of x by their standard deviations if center is TRUE, and the root mean square otherwise.
#' @return A vector comprising semantic similarity scores. The closer the value is to 1 when using the default method, "cosine", the higher the semantic similarity.  
#' @examples
#' #Compute the semantic similarity between the embeddings from "harmonytext" and "satisfactiontext". 
#' \dontrun{
#' similarity_scores <- textSimilarity(
#'   x = word_embeddings_4$texts$harmonytext,
#'   y = word_embeddings_4$texts$satisfactiontext
#' )
#' 
#' #Show information about how similarity_scores were constructed.
#' comment(similarity_scores)
#' }
#' @seealso See \code{\link{textDistance}} and \code{\link{textSimilarityNorm}}. 
#' @export
textSimilarity <- function(x,
                           y,
                           method = "cosine",
                           center = TRUE,
                           scale = FALSE) {

  # Select necessary columns
  x1 <- dplyr::select(x, dplyr::starts_with("Dim"))
  y1 <- dplyr::select(y, dplyr::starts_with("Dim"))

  # Normalize embeddings help(scale)
  x1 <- tibble::as_tibble(t(scale(t(x1), center = center, scale = scale)))
  y1 <- tibble::as_tibble(t(scale(t(y1), center = center, scale = scale)))

  # Compute cosines
  if (method == "cosine") {
    ss <- cosines(x1, y1)
  }

  # Compute correlation
  if (method == "spearman" | method == "pearson") {
    ss <- list()
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


#' Compute the semantic distance between two text variables.
#'
#' @param x Word embeddings (from textEmbed).
#' @param y Word embeddings (from textEmbed).
#' @param method (character) Character string describing type of measure to be computed; default is "euclidean" (see also
#' measures from stats:dist() including "maximum", "manhattan", "canberra", "binary" and "minkowski".
#' It is also possible to use "cosine", which computes the cosine distance (i.e., 1 - cosine(x, y)).
#' @param center (boolean; from base::scale) If center is TRUE then centering is done by subtracting the embedding mean
#' (omitting NAs) of x from each of its dimension, and if center is FALSE, no centering is done.
#' @param scale (boolean; from base::scale) If scale is TRUE then scaling is done by dividing the
#' (centered) embedding dimensions by the standard deviation of the embedding if center is TRUE, and the root mean square otherwise.
#' @return A vector comprising semantic distance scores.
#' @examples
#' #Compute the semantic distance score between the embeddings from "harmonytext" and "satisfactiontext". 
#' 
#' \dontrun{
#' distance_scores <- textDistance(
#'   x = word_embeddings_4$texts$harmonytext,
#'   y = word_embeddings_4$texts$satisfactiontext
#' )
#' 
#' #Show information about how distance_scores were constructed.
#' 
#' comment(distance_scores)
#' }
#' @seealso See  \code{\link{textSimilarity}} and \code{\link{textSimilarityNorm}}. 
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
  x1 <- as_tibble(t(scale(t(x1), center = center, scale = scale)))
  y1 <- as_tibble(t(scale(t(y1), center = center, scale = scale)))

  if (method == "cosine") {
    # Compute cosine distance
    ss <- 1 - cosines(x1, y1)
  } else {
    # Compute distance method = "euclidean"
    ss1 <- list()

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
#' @seealso see \code{\link{textSimilarityNorm}}
#' @export
textSimilarityMatrix <- function(x,
                                 method = "cosine",
                                 center = TRUE,
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
#' @seealso see \code{\link{textDistanceNorm}}
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
#' @seealso see \code{\link{textSimilarity}}
#' @importFrom dplyr row_number slice select starts_with
#' @export
textSimilarityNorm <- function(x,
                               y,
                               method = "cosine",
                               center = TRUE,
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
#' @seealso see \code{\link{textDistance}}
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
