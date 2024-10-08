% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/5_3_textNER.R
\name{textNER}
\alias{textNER}
\title{Named Entity Recognition. (experimental)}
\usage{
textNER(
  x,
  model = "dslim/bert-base-NER",
  device = "cpu",
  tokenizer_parallelism = FALSE,
  logging_level = "error",
  force_return_results = FALSE,
  set_seed = 202208L
)
}
\arguments{
\item{x}{(string)  A  variable or a tibble/dataframe with at least one character variable.}

\item{model}{(string)  Specification of a pre-trained language model for token classification
that have been fine-tuned on a NER task (e.g., see "dslim/bert-base-NER").
Use for predicting the classes of tokens in a sequence: person, organisation, location or miscellaneous).}

\item{device}{(string)  Device to use: 'cpu', 'gpu', or 'gpu:k' where k is a specific device number}

\item{tokenizer_parallelism}{(boolean)  If TRUE this will turn on tokenizer parallelism.}

\item{logging_level}{(string)  Set the logging level.
Options (ordered from less logging to more logging): critical, error, warning, info, debug}

\item{force_return_results}{(boolean)  Stop returning some incorrectly formatted/structured results.
This setting does CANOT evaluate the actual results (whether or not they make sense, exist, etc.).
All it does is to ensure the returned results are formatted correctly (e.g., does the question-answering
dictionary contain the key "answer", is sentiments from textClassify containing the labels "positive"
and "negative").}

\item{set_seed}{(Integer) Set seed.}
}
\value{
A list with tibble(s) with NER classifications for each column.
}
\description{
Named Entity Recognition. (experimental)
}
\examples{
\donttest{
# ner_example <- textNER("Arnes plays football with Daniel")
# ner_example
}
}
\seealso{
see \code{\link{textClassify}}, \code{\link{textGeneration}}, \code{\link{textNER}},
 \code{\link{textSum}}, \code{\link{textQA}}, \code{\link{textTranslate}}
}
