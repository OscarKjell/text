% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/4_1_textPlot.R
\name{textProjection}
\alias{textProjection}
\title{Compute Supervised Dimension Projection and related variables for plotting words.}
\usage{
textProjection(
  words,
  wordembeddings,
  single_wordembeddings = single_wordembeddings_df,
  x,
  y = NULL,
  pca = NULL,
  aggregation = "mean",
  split = "quartile",
  word_weight_power = 1,
  min_freq_words_test = 0,
  Npermutations = 10000,
  n_per_split = 50000,
  seed = 1003
)
}
\arguments{
\item{words}{Word or text variable to be plotted.}

\item{wordembeddings}{Word embeddings from textEmbed for the words to be plotted
(i.e., the aggregated word embeddings for the "words" parameter).}

\item{single_wordembeddings}{Word embeddings from textEmbed for individual words
(i.e., decontextualized embeddings).}

\item{x}{Numeric variable that the words should be plotted according to on the x-axes.}

\item{y}{Numeric variable that the words should be plotted according to on the y-axes (y=NULL).}

\item{pca}{Number of PCA dimensions applied to the word embeddings in the beginning of the function.
A number below 1 takes out \% of variance; An integer specify number of components to extract.
(default is NULL as this setting has not yet been evaluated).}

\item{aggregation}{Method to aggregate the word embeddings
(default = "mean"; see also "min", "max", and "[CLS]").}

\item{split}{Method to split the axes
(default = "quartile" involving selecting lower and upper quartile; see also "mean"). However, if the variable is
only containing two different values (i.e., being dichotomous) mean split is used.}

\item{word_weight_power}{Compute the power of the frequency of the words and multiply
the word embeddings with this in the computation of aggregated word embeddings for
group low (1) and group high (2). This increases the weight of more frequent words.}

\item{min_freq_words_test}{Option to select words that have occurred a specified number of
times (default = 0); when creating the Supervised Dimension Projection line
(i.e., single words receive Supervised Dimension Projection and p-value).}

\item{Npermutations}{Number of permutations in the creation of the null distribution.}

\item{n_per_split}{A setting to split Npermutations to avoid reaching computer memory limits;
the higher the faster, but too high may lead to abortion.}

\item{seed}{Set different seed.}
}
\value{
A dataframe with variables (e.g., including Supervised Dimension Projection, frequencies, p-values)
for the individual words that is used for the plotting in the textProjectionPlot function.
}
\description{
Compute Supervised Dimension Projection and related variables for plotting words.
}
\examples{
# Data
wordembeddings <- wordembeddings4
raw_data <- Language_based_assessment_data_8
# Pre-processing data for plotting
df_for_plotting <- textProjection(
  words = raw_data$harmonywords,
  wordembeddings = wordembeddings$harmonywords,
  single_wordembeddings = wordembeddings$singlewords_we,
  x = raw_data$hilstotal,
  split = "mean",
  Npermutations = 10,
  n_per_split = 1
)
df_for_plotting
#' @seealso see \code{\link{textProjectionPlot}}
}
