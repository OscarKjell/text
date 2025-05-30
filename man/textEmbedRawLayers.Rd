% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/1_1_textEmbed.R
\name{textEmbedRawLayers}
\alias{textEmbedRawLayers}
\title{Extract layers of hidden states}
\usage{
textEmbedRawLayers(
  texts,
  model = "bert-base-uncased",
  layers = -2,
  return_tokens = TRUE,
  word_type_embeddings = FALSE,
  decontextualize = FALSE,
  keep_token_embeddings = TRUE,
  device = "cpu",
  tokenizer_parallelism = FALSE,
  model_max_length = NULL,
  max_token_to_sentence = 4,
  hg_gated = FALSE,
  hg_token = Sys.getenv("HUGGINGFACE_TOKEN", unset = ""),
  trust_remote_code = FALSE,
  logging_level = "error",
  sort = TRUE
)
}
\arguments{
\item{texts}{A character variable or a tibble with at least one character variable.}

\item{model}{(character) Character string specifying pre-trained language model
(default = 'bert-base-uncased'). For full list of options see pretrained models at
 \href{https://huggingface.co/transformers/pretrained_models.html}{HuggingFace}.
 For example use "bert-base-multilingual-cased", "openai-gpt",
"gpt2", "ctrl", "transfo-xl-wt103", "xlnet-base-cased", "xlm-mlm-enfr-1024",
"distilbert-base-cased", "roberta-base", or "xlm-roberta-base". Only load models that
you trust from HuggingFace; loading a malicious model can execute arbitrary code on
your computer).}

\item{layers}{(character or numeric) Specify the layers that should be extracted
(default -2, which give the second to last layer). It is more efficient to only extract the
layers that you need (e.g., 11). You can also extract several (e.g., 11:12),
or all by setting this parameter to "all". Layer 0 is the decontextualized input layer
(i.e., not comprising hidden states) and thus should normally not be used. These layers can then
 be aggregated in the textEmbedLayerAggregation function.}

\item{return_tokens}{(boolean) If TRUE, provide the tokens used in the specified transformer
model. (default = TRUE)}

\item{word_type_embeddings}{(boolean) Wether to provide embeddings for each word/token type.
(default = FALSE)}

\item{decontextualize}{(boolean) Wether to dectonextualise embeddings (i.e., embedding one word
at a time). (default = TRUE)}

\item{keep_token_embeddings}{(boolean) Whether to keep token level embeddings in the output
(when using word_types aggregation). (default= TRUE)}

\item{device}{(character) Name of device to use: 'cpu', 'gpu', 'gpu:k' or 'mps'/'mps:k'
for MacOS, where k is a specific device number. (default = "cpu")}

\item{tokenizer_parallelism}{(boolean) If TRUE this will turn on tokenizer parallelism.
(default = FALSE).}

\item{model_max_length}{The maximum length (in number of tokens) for the inputs to the
transformer model (default the value stored for the associated model).}

\item{max_token_to_sentence}{(numeric) Maximum number of tokens in a string to handle before
switching to embedding text sentence by sentence. (default= 4)}

\item{hg_gated}{Set to TRUE if the accessed model is gated.}

\item{hg_token}{The token needed to access the gated model.
Create a token from the ['Settings' page](https://huggingface.co/settings/tokens) of
the Hugging Face website. An an environment variable HUGGINGFACE_TOKEN can
be set to avoid the need to enter the token each time.}

\item{trust_remote_code}{use a model with custom code on the Huggingface Hub}

\item{logging_level}{(character) Set the logging level. (default ="error")
Options (ordered from less logging to more logging): critical, error, warning, info, debug}

\item{sort}{(boolean) If TRUE sort the output to tidy format. (default = TRUE)}
}
\value{
The textEmbedRawLayers() takes text as input, and returns the hidden states for
each token of the text, including the [CLS] and the [SEP].
Note that layer 0 is the input embedding to the transformer, and should normally not be used.
}
\description{
textEmbedRawLayers extracts layers of hidden states (word embeddings) for all character variables
in a given dataframe.
}
\examples{
# Get hidden states of layer 11 and 12 for "I am fine".
\dontrun{
imf_embeddings_11_12 <- textEmbedRawLayers(
  "I am fine",
  layers = 11:12
)

# Show hidden states of layer 11 and 12.
imf_embeddings_11_12
}
}
\seealso{
See \code{\link{textEmbedLayerAggregation}} and \code{\link{textEmbed}}.
}
