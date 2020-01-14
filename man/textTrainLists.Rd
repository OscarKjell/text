% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/2_textTrain.R
\name{textTrainLists}
\alias{textTrainLists}
\title{textTrainLists trains word embeddings from several text variable to several numeric variable.}
\usage{
textTrainLists(
  x,
  y,
  nrFolds_k = 10,
  methodTrain = "ridge",
  preProcessTrain = "pca",
  preProcessThresh = 0.95,
  methodCor = "pearson",
  ...
)
}
\arguments{
\item{x}{List of several Wordembeddings with same length from textImport (i.e., remove single_word_we).}

\item{y}{A Tibble with numeric variables to predict.}

\item{nrFolds_k}{Number of folds to use.}

\item{methodTrain}{Method used to train representations.}

\item{preProcessTrain}{Preprocessing training.}

\item{preProcessThresh}{Preprocessing threshold.}

\item{methodCor}{Type of correlation used in evaluation.}

\item{...}{Arguments from caret::createFolds, caret::trainControl and caret::train.}
}
\value{
A correlation between predicted and observed values; as well as predicted values.
}
\description{
textTrainLists trains word embeddings from several text variable to several numeric variable.
}
\examples{
wordembeddings <- wordembeddings4_10[1:2]
ratings_data <- sq_data_tutorial4_10[1:2]
wordembeddings <- textTrainLists(wordembeddings, ratings_data, nrFolds_k=2)
}
\seealso{
see \code{\link{textTrain}}
}