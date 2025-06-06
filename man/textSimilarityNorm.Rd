% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/3_1_textSimilarity.R
\name{textSimilarityNorm}
\alias{textSimilarityNorm}
\title{Semantic similarity between a text variable and a word norm}
\usage{
textSimilarityNorm(x, y, method = "cosine", center = TRUE, scale = FALSE)
}
\arguments{
\item{x}{Word embeddings from textEmbed().}

\item{y}{Word embedding from textEmbed (from only one text).}

\item{method}{(character) Character string describing type of measure to be computed. Default is "cosine" (see also
"spearmen", "pearson" as well as measures from textDistance() (which here is computed as 1 - textDistance)
including "euclidean", "maximum", "manhattan", "canberra", "binary" and "minkowski").}

\item{center}{(boolean; from base::scale) If center is TRUE then centering is done by subtracting the column means
(omitting NAs) of x from their corresponding columns, and if center is FALSE, no centering is done.}

\item{scale}{(boolean; from base::scale) If scale is TRUE then scaling is done by dividing the (centered)
columns of x by their standard deviations if center is TRUE, and the root mean square otherwise.}
}
\value{
A vector comprising semantic similarity scores.
}
\description{
textSimilarityNorm() computes the semantic similarity between a text variable and a word norm
(i.e., a text represented by one word embedding that represent a construct).
}
\examples{
\dontrun{
library(dplyr)
library(tibble)
harmonynorm <- c("harmony peace ")
satisfactionnorm <- c("satisfaction achievement")

norms <- tibble::tibble(harmonynorm, satisfactionnorm)
word_embeddings <- word_embeddings_4$texts
word_embeddings_wordnorm <- textEmbed(norms)
similarity_scores <- textSimilarityNorm(
  word_embeddings$harmonytext,
  word_embeddings_wordnorm$harmonynorm
)
}
}
\seealso{
see \code{\link{textSimilarity}}
}
