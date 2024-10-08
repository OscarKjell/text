% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/5_5_textQA.R
\name{textQA}
\alias{textQA}
\title{Question Answering. (experimental)}
\usage{
textQA(
  question,
  context,
  model = "",
  device = "cpu",
  tokenizer_parallelism = FALSE,
  logging_level = "warning",
  force_return_results = FALSE,
  top_k = 1L,
  doc_stride = 128L,
  max_answer_len = 15L,
  max_seq_len = 384L,
  max_question_len = 64L,
  handle_impossible_answer = FALSE,
  set_seed = 202208L
)
}
\arguments{
\item{question}{(string)  A question}

\item{context}{(string)  The context(s) where the model will look for the answer.}

\item{model}{(string)  HuggingFace name of a pre-trained language model that have been fine-tuned
on a question answering task.}

\item{device}{(string)  Device to use: 'cpu', 'gpu', or 'gpu:k' where k is a specific device number}

\item{tokenizer_parallelism}{(boolean)  If TRUE this will turn on tokenizer parallelism.}

\item{logging_level}{(string)  Set the logging level.
Options (ordered from less logging to more logging): critical, error, warning, info, debug}

\item{force_return_results}{(boolean)  Stop returning some incorrectly formatted/structured results.
This setting does CANOT evaluate the actual results (whether or not they make sense, exist, etc.).
All it does is to ensure the returned results are formatted correctly (e.g., does the question-answering
dictionary contain the key "answer", is sentiments from textClassify containing the labels "positive"
 and "negative").}

\item{top_k}{(integer) (int)  Indicates number of possible answer span(s) to get from the model output.}

\item{doc_stride}{(integer)   If the context is too long to fit with the question for the model, it will be split
into overlapping chunks. This setting controls the overlap size.}

\item{max_answer_len}{(integer)  Max answer size to be extracted from the model’s output.}

\item{max_seq_len}{(integer)  The max total sentence length (context + question) in tokens of each chunk
passed to the model. If needed, the context is split in chunks (using doc_stride as overlap).}

\item{max_question_len}{(integer)   The max question length after tokenization. It will be truncated if needed.}

\item{handle_impossible_answer}{(boolean)  Whether or not impossible is accepted as an answer.}

\item{set_seed}{(Integer) Set seed.}
}
\value{
Answers.
}
\description{
Question Answering. (experimental)
}
\examples{
\donttest{
#   qa_examples <- textQA(question = "Which colour have trees?",
#     context = "Trees typically have leaves, are mostly green and like water.")
}
}
\seealso{
see \code{\link{textClassify}}, \code{\link{textGeneration}}, \code{\link{textNER}},
 \code{\link{textSum}}, \code{\link{textQA}}, \code{\link{textTranslate}}
}
