% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/2_4_1_textPredictTextTrained.R
\name{textPredictAll}
\alias{textPredictAll}
\title{Predict from several models, selecting the correct input}
\usage{
textPredictAll(models, word_embeddings, x_append = NULL, ...)
}
\arguments{
\item{models}{Object containing several models.}

\item{word_embeddings}{List of word embeddings (if using word embeddings from more than one
text-variable use dim_names = TRUE throughout the pipeline).}

\item{x_append}{A tibble/dataframe with additional variables used in the training of the models (optional).}

\item{...}{Settings from textPredict.}
}
\value{
A tibble with predictions.
}
\description{
Predict from several models, selecting the correct input
}
\examples{
\donttest{
# x <- Language_based_assessment_data_8[1:2, 1:2]
# word_embeddings_with_layers <- textEmbedLayersOutput(x, layers = 11:12)
}
}
\seealso{
see \code{\link{textPredict}} and \code{\link{textTrain}}
}
