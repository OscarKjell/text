###############################################################################
####  03_roberta_matching.R -- why RoBERTa word types never match          ####
####                                                                       ####
####  textProjection()/textWordPrediction() test the words NLTK finds      ####
####  (lower-cased) and look each up in word_types$words by EXACT match.  ####
####  textEmbed() stores tokenizer tokens as they are: RoBERTa keeps the   ####
####  "Ġ" space marker and the original case, so almost nothing      ####
####  matches. WordPiece (BERT) only loses words it splits into pieces.   ####
####  pp_normalise_word_types() is the proposed fix.                       ####
###############################################################################

source(file.path("dev", "_config.R"))
txt <- tibble::tibble(t = c("Happy people feel worthless sometimes. I feel happy and hopeful today.",
                            "My unhappiness is overwhelming, but happy moments help."))
v <- colnames(pp_count_matrix(txt$t))
for (m in c("roberta-base", "bert-base-uncased")) {
  e  <- text::textEmbed(txt, model = m, layers = -2, keep_token_embeddings = FALSE,
                        aggregation_from_tokens_to_word_types = "mean", device = DEVICE)
  wt <- get_word_types(e)
  wn <- pp_normalise_word_types(wt)
  cat("\n==", m,
      "\n  word types         :", paste(wt$words, collapse = " | "),
      "\n  matched as is      :", paste(v[v %in% wt$words], collapse = " | "),
      "\n  matched normalised :", paste(v[v %in% wn$words], collapse = " | "),
      "\n  still unmatched    :", paste(v[!v %in% wn$words], collapse = " | "), "\n")
}
