% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/2_1_textTrain.R
\name{textTrain}
\alias{textTrain}
\title{Trains word embeddings}
\usage{
textTrain(x, y, force_train_method = "automatic", ...)
}
\arguments{
\item{x}{Word embeddings from textEmbed (or textEmbedLayerAggreation).
Can analyze several variables at the same time; but if training to several
outcomes at the same time use a tibble within the list as input rather than just a
tibble input (i.e., keep the name of the wordembedding).}

\item{y}{Numeric variable to predict. Can be several; although then make
sure to have them within a tibble (this is required
even if it is only one outcome but several word embeddings variables).}

\item{force_train_method}{Default is "automatic", so if y is a factor
random_forest is used, and if y is numeric ridge regression
is used. This can be overridden using "regression" or "random_forest".}

\item{...}{Arguments from textTrainRegression or textTrainRandomForest
the textTrain function.}
}
\value{
A correlation between predicted and observed values; as well as a
 tibble of predicted values (t-value, degree of freedom (df), p-value,
 alternative-hypothesis, confidence interval, correlation coefficient).
}
\description{
textTrain() trains word embeddings to a numeric (ridge regression) or categorical (random forest) variable.
}
\examples{
# Examines how well the embeddings from "harmonytext" can
# predict the numeric variable "hilstotal" in the pre-included
# dataset "Language_based_assessment_data_8".

\dontrun{
trained_model <- textTrain(
  x = word_embeddings_4$texts$harmonytext,
  y = Language_based_assessment_data_8$hilstotal
)

# Examine results (t-value, degree of freedom (df), p-value,
# alternative-hypothesis, confidence interval, correlation coefficient).

trained_model$results
}
}
\seealso{
See \code{\link{textTrainRegression}}, \code{\link{textTrainRandomForest}} and
\code{\link{textTrainLists}}.
}
