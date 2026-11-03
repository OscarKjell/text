## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
evaluate = FALSE

## ----HuggingFface_tabble_long, echo=FALSE, results='asis'---------------------
library(magrittr)
Models <- c("'bert-base-uncased'",
            "'roberta-base'",
            "'distilbert-base-cased'",
            "'bert-base-multilingual-cased'",
            "'xlm-roberta-large'"
            )

References <- c("[Devlin et al. 2019]( https://aclanthology.org/N19-1423/)",
                "[Liu et al. 2019](https://arxiv.org/abs/1907.11692)",
                "[Sahn et al., 2019](https://arxiv.org/abs/1910.01108)",
                "[Devlin et al.2019]( https://aclanthology.org/N19-1423/)",
                "[Liu et al]( https://arxiv.org/pdf/1907.11692)"
                )

Layers <- c("12",
            "12", 
            "6?",
            "12",
            "24")

Language <- c("English",
              "English", 
              "English",
              "104 top languages at Wikipedia",
              "[100 language](https://huggingface.co/bert-base-multilingual-cased)")

Dimensions <- c("768", 
                "768", 
                "768?", 
                "768", 
                "1024")

Tables_short <- tibble::tibble(Models, References, Layers, Dimensions, Language)

knitr::kable(Tables_short, caption="", bootstrap_options = c("hover"), full_width = T)

## ----word_embedding_tutorial_1, eval = evaluate, warning=FALSE, message=FALSE----
# library(text)
# # Example text
# texts <- c("I feel great")
# 
# # Transform the text to BERT word embeddings
# wordembeddings <- textEmbed(texts = texts,
#                             model = 'bert-base-uncased',
#                             layers = 11:12,
#                             aggregation_from_layers_to_tokens = "concatenate",
#                             aggregation_from_tokens_to_texts = "mean"
#                             )
# 
# wordembeddings

## ----word_embedding_tutorial_2, eval = FALSE, warning=FALSE, message=FALSE----
# library(text)
# 
# # Transform the text data to BERT word embeddings
# wordembeddings <- textEmbed(texts = Language_based_assessment_data_8[1:2],
#                             aggregation_from_tokens_to_word_types = "mean",
#                             keep_token_embeddings = FALSE)
# 
# # See how word embeddings are structured
# wordembeddings

## ----word_embedding_tutorial_3, eval = evaluate, warning=FALSE, message=FALSE----
# library(text)
# 
# #Transform the text data to BERT word embeddings
# 
# # Example test
# texts <- c("I feel great")
# 
# wordembeddings_tokens_layers <- textEmbedRawLayers(
#   texts = texts,
#   layers = 10:12)
# wordembeddings_tokens_layers

## ----word_embedding_tutorial_4, eval = evaluate, warning=FALSE, message=FALSE----
# library(text)
# 
# # Aggregating layer 11 and 12 by taking the mean of each dimension.
# we_11_12_mean <- textEmbedLayerAggregation(
#   word_embeddings_layers = wordembeddings_tokens_layers$context_tokens$texts,
#   layers = 11:12,
#   aggregation_from_layers_to_tokens = "concatenate",
#   aggregation_from_tokens_to_texts = "mean")
# we_11_12_mean
# # Aggregating layer 11 and 12 by taking the minimum of each dimension accross the two layers.
# we_10_11_min <- textEmbedLayerAggregation(
#   word_embeddings_layers = wordembeddings_tokens_layers$context_tokens$texts,
#   layers = 10:11,
#   aggregation_from_layers_to_tokens = "concatenate",
#   aggregation_from_tokens_to_texts = "min")
# we_10_11_min
# # Aggregating layer 1 to 12 by taking the max value of each dimension across the 12 layers.
# we_11_max <- textEmbedLayerAggregation(
#   word_embeddings_layers = wordembeddings_tokens_layers$context_tokens$texts,
#   layers = 11,
#   aggregation_from_tokens_to_texts = "max")
# we_11_max

