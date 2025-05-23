% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/1_1_textEmbed.R
\name{textTokenize}
\alias{textTokenize}
\title{Tokenize text-variables}
\usage{
textTokenize(
  texts,
  model,
  max_token_to_sentence = 4,
  device = "cpu",
  tokenizer_parallelism = FALSE,
  model_max_length = NULL,
  hg_gated = FALSE,
  hg_token = Sys.getenv("HUGGINGFACE_TOKEN", unset = ""),
  trust_remote_code = FALSE,
  logging_level = "error"
)
}
\arguments{
\item{texts}{A character variable or a tibble/dataframe with at least one character variable.}

\item{model}{Character string specifying pre-trained language model (default 'bert-base-uncased').
 For full list of options see pretrained models at
 \href{https://huggingface.co/transformers/pretrained_models.html}{HuggingFace}.
 For example use "bert-base-multilingual-cased", "openai-gpt",
"gpt2", "ctrl", "transfo-xl-wt103", "xlnet-base-cased", "xlm-mlm-enfr-1024", "distilbert-base-cased",
"roberta-base", or "xlm-roberta-base".}

\item{max_token_to_sentence}{(numeric) Maximum number of tokens in a string to handle before
switching to embedding text sentence by sentence.}

\item{device}{Name of device to use: 'cpu', 'gpu', 'gpu:k' or 'mps'/'mps:k' for MacOS, where k is a
specific device number.}

\item{tokenizer_parallelism}{If TRUE this will turn on tokenizer parallelism. Default FALSE.}

\item{model_max_length}{The maximum length (in number of tokens) for the inputs to the transformer model
(default the value stored for the associated model).}

\item{hg_gated}{Set to TRUE if the accessed model is gated.}

\item{hg_token}{The token needed to access the gated model.
Create a token from the ['Settings' page](https://huggingface.co/settings/tokens) of
the Hugging Face website. An an environment variable HUGGINGFACE_TOKEN can
be set to avoid the need to enter the token each time.}

\item{trust_remote_code}{use a model with custom code on the Huggingface Hub}

\item{logging_level}{Set the logging level. Default: "warning".
Options (ordered from less logging to more logging): critical, error, warning, info, debug}
}
\value{
Returns tokens according to specified huggingface transformer.
}
\description{
textTokenize() tokenizes according to different huggingface transformers
}
\examples{
\donttest{
# tokens <- textTokenize("hello are you?")
}
}
\seealso{
see \code{\link{textEmbed}}
}
