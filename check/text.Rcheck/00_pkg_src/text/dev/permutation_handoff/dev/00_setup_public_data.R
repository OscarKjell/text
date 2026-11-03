###############################################################################
####  00_setup_public_data.R                                               ####
####                                                                       ####
####  Builds everything the other dev scripts need from PUBLIC data only:  ####
####  topics::dep_wor_data (500 free-text descriptions of depression,      ####
####  PHQ-9 total).  Run once; results are cached in dev/cache/.           ####
####                                                                       ####
####    embeddings  bert-base-uncased, layer 11, word types = mean          ####
####    C           participants x word types counts (NLTK, as textProjection) ####
####    proj_*      textProjection() as shipped, for PHQ-9 and for noise   ####
###############################################################################

source(file.path("dev", "_config.R"))

dw <- topics::dep_wor_data
log_step("dep_wor_data: ", nrow(dw), " participants")

f_emb <- file.path(CACHE, "embeddings_bert.rds")
if (!file.exists(f_emb)) {
  log_step("Embedding with bert-base-uncased (a few minutes on CPU) ...")
  emb <- text::textEmbed(dw["Deptext"], model = "bert-base-uncased", layers = 11,
                         aggregation_from_tokens_to_word_types = "mean",
                         keep_token_embeddings = FALSE, device = DEVICE)
  saveRDS(emb, f_emb)
}
emb <- readRDS(f_emb)
wt  <- get_word_types(emb)
log_step("word types with an embedding: ", nrow(wt))

f_C <- file.path(CACHE, "counts.rds")
if (!file.exists(f_C)) saveRDS(pp_count_matrix(dw$Deptext), f_C)
C <- readRDS(f_C)
log_step("count matrix: ", nrow(C), " x ", ncol(C))

xs <- list(real = dw$PHQ9tot, noise = noise_example(nrow(dw)))
for (k in names(xs)) {
  f <- file.path(CACHE, paste0("proj_", k, ".rds"))
  if (file.exists(f)) next
  log_step("textProjection() as shipped: ", k, " ...")
  pr <- text::textProjection(words = dw$Deptext, word_embeddings = emb$texts$Deptext,
                             word_types_embeddings = wt, x = xs[[k]],
                             min_freq_words_test = MIN_FREQ)   # package defaults otherwise
  saveRDS(pr, f)
}
log_step("00_setup_public_data.R DONE")
