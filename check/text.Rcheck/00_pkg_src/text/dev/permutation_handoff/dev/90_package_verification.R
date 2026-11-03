## Verify the package implementation against the cached shipped-version run
## and the prototype expectations. Run from dev/permutation_handoff/.

HANDOFF_DIR <- Sys.getenv("HANDOFF_DIR", ".")
CACHE <- file.path(HANDOFF_DIR, "dev", "cache")
OUT <- file.path(HANDOFF_DIR, "dev", "output")
ALPHA <- 0.05
MIN_FREQ <- 3

suppressPackageStartupMessages({
  library(Matrix)
})
devtools::load_all(file.path(HANDOFF_DIR, "..", ".."), quiet = TRUE)
text::textrpp_initialize(condaenv = "textrpp_condaenv", save_profile = FALSE)

dw <- topics::dep_wor_data
emb <- readRDS(file.path(CACHE, "embeddings_bert.rds"))
wt <- emb$word_types
while (!is.data.frame(wt)) wt <- wt[[1]]

log_step <- function(...) message(format(Sys.time(), "[%H:%M:%S] "), ...)

## 1. Back-compatibility: token path must reproduce the cached (pre-change) run
log_step("textProjection, null_method = token (back-compat check) ...")
pr_new <- text::textProjection(
  words = dw$Deptext, word_embeddings = emb$texts$Deptext,
  word_types_embeddings = wt, x = dw$PHQ9tot,
  min_freq_words_test = MIN_FREQ
)
pr_old <- readRDS(file.path(CACHE, "proj_real.rds"))
j <- match(pr_old$word_data$words, pr_new$word_data$words)
ok <- !is.na(j)
cat(
  "token back-compat:",
  "words matched:", sum(ok), "/", nrow(pr_old$word_data),
  "| max |dot diff|:", signif(max(abs(pr_old$word_data$dot.x[ok] -
    pr_new$word_data$dot.x[j[ok]]), na.rm = TRUE), 3),
  "| max |p diff|:", signif(max(abs(pr_old$word_data$p_values_dot.x[ok] -
    pr_new$word_data$p_values_dot.x[j[ok]]), na.rm = TRUE), 3), "\n"
)

## 2. Participant method on PHQ-9 and on noise
count_fdr <- function(pr) {
  wd <- pr$word_data
  wd <- wd[!wd$words %in% c("Group1*", "Group2*", "projected_embedding"), ]
  wd <- wd[wd$n >= MIN_FREQ & !is.na(wd$p_values_dot.x), ]
  c(sig = sum(stats::p.adjust(wd$p_values_dot.x, "fdr") < ALPHA, na.rm = TRUE), n = nrow(wd))
}
set.seed(2026)
noise <- stats::rnorm(nrow(dw))
res <- list()
for (k in c("real", "noise")) {
  xv <- if (k == "real") dw$PHQ9tot else noise
  log_step("textProjection, null_method = participant: ", k, " ...")
  t0 <- Sys.time()
  pr_p <- text::textProjection(
    words = dw$Deptext, word_embeddings = emb$texts$Deptext,
    word_types_embeddings = wt, x = xv,
    min_freq_words_test = MIN_FREQ,
    null_method = "participant", Npermutations = 10000
  )
  secs <- round(as.numeric(difftime(Sys.time(), t0, units = "secs")))
  # observed statistic must be unchanged by the null method
  jj <- match(pr_new$word_data$words, pr_p$word_data$words)
  oo <- !is.na(jj)
  dmax <- if (k == "real") {
    signif(max(abs(pr_new$word_data$dot.x[oo] - pr_p$word_data$dot.x[jj[oo]]), na.rm = TRUE), 3)
  } else {
    NA
  }
  cc <- count_fdr(pr_p)
  res[[k]] <- c(cc, secs = secs)
  cat(
    k, ": participant FDR-significant =", cc[["sig"]], "of", cc[["n"]],
    "| seconds =", secs,
    if (k == "real") paste("| max |dot diff| vs token run:", dmax) else "", "\n"
  )
  if (k == "real") {
    cat(
      "  p_z present:", "p_z_dot.x" %in% names(pr_p$word_data),
      "| p floor respected:", min(pr_p$word_data$p_values_dot.x, na.rm = TRUE) >= 1 / 10001, "\n"
    )
  }
}

## 3. textWordPrediction on dep_wor_data
log_step("textWordPrediction (new implementation), PHQ-9 ...")
t0 <- Sys.time()
wp <- text::textWordPrediction(
  words = dw$Deptext,
  word_types_embeddings = wt,
  x = dw$PHQ9tot,
  n_permutations = 10000,
  seed = 1003
)
secs <- round(as.numeric(difftime(Sys.time(), t0, units = "secs")))
sig <- sum(stats::p.adjust(wp$word_data$p_values_x, "fdr") < ALPHA, na.rm = TRUE)
cat(
  "word prediction: tested =", nrow(wp$word_data),
  "| FDR-significant =", sig,
  "| min p =", signif(min(wp$word_data$p_values_x), 3),
  "| lambda =", signif(wp$lambda_x, 3),
  "| loo_r =", round(wp$loo_r, 3),
  "| seconds =", secs, "\n"
)

log_step("90_package_verification.R DONE")
