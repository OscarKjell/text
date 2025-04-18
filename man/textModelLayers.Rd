% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/0_3_textModels.R
\name{textModelLayers}
\alias{textModelLayers}
\title{Number of layers}
\usage{
textModelLayers(
  target_model,
  hg_gated = FALSE,
  hg_token = Sys.getenv("HUGGINGFACE_TOKEN", unset = ""),
  trust_remote_code = FALSE
)
}
\arguments{
\item{target_model}{(string) The name of the model to know the number of layers of.}

\item{hg_gated}{Set to TRUE if the accessed model is gated.}

\item{hg_token}{The token needed to access the gated model.
Create a token from the ['Settings' page](https://huggingface.co/settings/tokens) of
the Hugging Face website. An an environment variable HUGGINGFACE_TOKEN can
be set to avoid the need to enter the token each time.}

\item{trust_remote_code}{use a model with custom code on the Huggingface Hub}
}
\value{
Number of layers.
}
\description{
This functions gets the number of layers in a given model.
}
\examples{
\dontrun{
textModelLayers(target_model = "bert-base-uncased")
}
}
\seealso{
see \code{\link{textModels}}
}
