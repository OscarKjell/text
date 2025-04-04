% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/4_1_textPlotCentrality.R
\name{textCentrality}
\alias{textCentrality}
\title{Semantic similarity score between single words' and an aggregated word embeddings}
\usage{
textCentrality(
  words,
  word_embeddings,
  word_types_embeddings = word_types_embeddings_df,
  method = "cosine",
  aggregation = "mean",
  min_freq_words_test = 0
)
}
\arguments{
\item{words}{(character) Word or text variable to be plotted.}

\item{word_embeddings}{Word embeddings from textEmbed for the words to be plotted
(i.e., the aggregated word embeddings for the "words" variable).}

\item{word_types_embeddings}{Word embeddings from textEmbed for individual words
(i.e., the decontextualized word embeddings).}

\item{method}{(character) Character string describing type of measure to be computed. Default is "cosine" (see also
"spearmen", "pearson" as well as measures from textDistance() (which here is computed as 1 - textDistance)
including "euclidean", "maximum", "manhattan", "canberra", "binary" and "minkowski").}

\item{aggregation}{(character) Method to aggregate the word embeddings
(default = "mean"; see also "min", "max" or "[CLS]").}

\item{min_freq_words_test}{(numeric) Option to select words that have at least occurred a specified
number of times (default = 0); when creating the semantic similarity
scores.}
}
\value{
A dataframe with variables (e.g., including semantic similarity, frequencies)
for the individual words that are used as input for the plotting in the textCentralityPlot function.
}
\description{
textCentrality() computes semantic similarity score between single words' word embeddings
and the aggregated word embedding of all words.
}
\examples{
# Computes the semantic similarity between the individual word embeddings (Iwe)
# in the "harmonywords" column of the pre-installed dataset: Language_based_assessment_data_8,
# and the aggregated word embedding (Awe).
# The Awe can be interpreted the latent meaning of the text.

\dontrun{
df_for_plotting <- textCentrality(
  words = Language_based_assessment_data_8["harmonywords"],
  word_embeddings = word_embeddings_4$texts$harmonywords,
  word_types_embeddings = word_embeddings_4$word_types
)

# df_for_plotting contain variables (e.g., semantic similarity, frequencies) for
# the individual words that are used for plotting by the textCentralityPlot function.
}
}
\seealso{
See \code{\link{textCentralityPlot}} and \code{\link{textProjection}}.
}
