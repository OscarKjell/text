###############################################################################
####  02_word_prediction.R  -- corrected word-prediction test              ####
####                                                                       ####
####  Public data only (run 00_setup_public_data.R first).                 ####
####    1. LOO shortcut == explicit ridge refits                           ####
####    2. PHQ-9 and noise, centre = "token_mean" and "none", plots        ####
####    3. calibration on K noise variables                                ####
####    4. the substring problem of the current textWordPrediction()       ####
####    5. (optional, slow) the current dev textWordPrediction() for        ####
####       comparison: set RUN_ORIGINAL=1 and have text 1.9 (dev) loaded    ####
###############################################################################

source(file.path("dev", "_config.R"))
K_NOISE <- as.integer(Sys.getenv("K_NOISE", "50"))

dw <- topics::dep_wor_data
wt <- get_word_types(readRDS(file.path(CACHE, "embeddings_bert.rds")))
C  <- readRDS(file.path(CACHE, "counts.rds"))
xs <- list(real = dw$PHQ9tot, noise = noise_example(nrow(dw)))

## 1. LOO check
d <- wpf_check_loo(C, wt, xs$real)
log_step("LOO shortcut vs explicit refits, max |diff| = ", signif(d, 3))
write.csv(data.frame(max_abs_diff = d), file.path(OUT, "02_loo_check.csv"), row.names = FALSE)

## 2. cases + plots
cases <- list()
for (k in names(xs)) for (ce in c("token_mean", "none")) {
  r  <- textWordPredictionFast(C, wt, xs[[k]], MIN_FREQ, centre = ce)
  saveRDS(r, file.path(OUT, paste0("02_res_", k, "__", ce, ".rds")))
  pr <- readRDS(file.path(CACHE, paste0("proj_", k, ".rds")))
  lab <- paste(if (k == "real") "PHQ-9 total" else "A variable of pure noise", "(predicted, z)")
  draw(wpf_into_projection(pr, r), file.path(OUT, paste0("02_fig_", k, "__", ce, ".png")), lab, "fdr")
  cases[[paste(k, ce)]] <- data.frame(case = k, centre = ce, words = nrow(r$word_data),
    lambda = r$lambda, fdr = sum(r$word_data$p_fdr < ALPHA),
    pct_p05 = 100 * mean(r$word_data$p_values_x < .05), loo_r = r$loo_r, seconds = r$seconds)
}
cases <- do.call(rbind, cases); print(cases)
write.csv(cases, file.path(OUT, "02_cases.csv"), row.names = FALSE)

## 3. calibration
noise <- noise_set(nrow(dw), K_NOISE)
cal <- do.call(rbind, lapply(seq_along(noise), function(i) {
  do.call(rbind, lapply(c("token_mean", "none"), function(ce) {
    r <- textWordPredictionFast(C, wt, noise[[i]], MIN_FREQ, centre = ce, seed = 1003 + i)
    data.frame(i = i, centre = ce, fdr = sum(r$word_data$p_fdr < ALPHA),
               pct_p05 = 100 * mean(r$word_data$p_values_x < .05))
  }))
}))
write.csv(cal, file.path(OUT, "02_calibration.csv"), row.names = FALSE)
agg <- do.call(rbind, lapply(split(cal, cal$centre), function(d) data.frame(
  centre = d$centre[1], n_noise = nrow(d), share_any_fdr = mean(d$fdr > 0),
  mean_pct_p05 = mean(d$pct_p05), max_fdr = max(d$fdr))))
print(agg); write.csv(agg, file.path(OUT, "02_calibration_summary.csv"), row.names = FALSE)

## 4. substring counting in the current function (regex without word boundaries)
S  <- wpf_setup(C, wt, MIN_FREQ)
rx <- vapply(S$words, function(w) sum(stringi::stri_count_regex(dw$Deptext, w,
                                       case_insensitive = TRUE)), 0)
ratio <- rx / S$n
sub <- data.frame(words = length(ratio), pct_overcount_10 = 100 * mean(ratio >= 1.1),
                  pct_overcount_2x = 100 * mean(ratio >= 2))
ex <- intersect(c("i", "he", "a", "me", "so", "do"), names(ratio))
sub$examples <- paste(sprintf("%s %.0fx", ex, ratio[ex]), collapse = ", ")
print(sub); write.csv(sub, file.path(OUT, "02_substring.csv"), row.names = FALSE)
log_step("r(PHQ-9, words written) = ", round(stats::cor(dw$PHQ9tot, Matrix::rowSums(C)), 3))

## 5. optional: the current dev function (about 25-40 min per variable)
if (Sys.getenv("RUN_ORIGINAL") == "1" && exists("textWordPrediction", asNamespace("text"))) {
  for (k in names(xs)) {
    o <- text::textWordPrediction(words = dw$Deptext, word_types_embeddings = wt,
                                  x = as.numeric(scale(xs[[k]])),
                                  text_remove = "[^[:alnum:]'-]")   # without this it errors on "?"
    saveRDS(o, file.path(OUT, paste0("02_original_", k, ".rds")))
    w <- o$word_data[o$word_data$n >= MIN_FREQ, ]
    log_step("original ", k, ": ", sum(stats::p.adjust(w$p_values_x, "fdr") < ALPHA),
             " FDR-significant; ", round(100 * mean(w$p_values_x == 0), 1), "% with p = 0")
  }
}
log_step("02_word_prediction.R DONE")
