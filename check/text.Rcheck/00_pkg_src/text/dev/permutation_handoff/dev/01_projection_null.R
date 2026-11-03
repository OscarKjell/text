###############################################################################
####  01_projection_null.R  -- token shuffle vs participant permutation    ####
####                                                                       ####
####  Public data only (run 00_setup_public_data.R first).                 ####
####    1. the reimplemented statistic reproduces textProjection()'s dots  ####
####    2. PHQ-9 and noise: FDR-significant words under both nulls + plots ####
####    3. calibration: K noise variables under both nulls                 ####
####    4. seed dependence of the package's null (one noise variable)      ####
####  Outputs in dev/output/.                                              ####
###############################################################################

source(file.path("dev", "_config.R"))
K_NOISE <- as.integer(Sys.getenv("K_NOISE", "50"))
B_CAL   <- as.integer(Sys.getenv("B_CAL", "1000"))   # participant perms per noise variable

dw  <- topics::dep_wor_data
wt  <- get_word_types(readRDS(file.path(CACHE, "embeddings_bert.rds")))
S   <- pp_setup(readRDS(file.path(CACHE, "counts.rds")), wt, MIN_FREQ)
log_step("coverage: ", round(100 * S$coverage[["word_types"]]), "% of word types, ",
         round(100 * S$coverage[["tokens"]], 1), "% of tokens have an embedding; ",
         length(S$test), " words tested")
xs <- list(real = dw$PHQ9tot, noise = noise_example(nrow(dw)))

## 1. same statistic as the package -----------------------------------------
chk <- do.call(rbind, lapply(names(xs), function(k) {
  pr <- readRDS(file.path(CACHE, paste0("proj_", k, ".rds")))$word_data
  d  <- pp_dots(S, xs[[k]])
  i  <- match(S$words, pr$words); ok <- !is.na(i)
  data.frame(case = k, words = sum(ok), r = stats::cor(d[ok], pr$dot.x[i[ok]]),
             max_abs_diff = max(abs(d[ok] - pr$dot.x[i[ok]])))
}))
print(chk); write.csv(chk, file.path(OUT, "01_statistic_check.csv"), row.names = FALSE)

## 2. PHQ-9 and noise ---------------------------------------------------------
cases <- do.call(rbind, lapply(names(xs), function(k) {
  pr  <- readRDS(file.path(CACHE, paste0("proj_", k, ".rds")))
  pk  <- pkg_count(pr)
  tst <- pp_test(S, xs[[k]], null_method = "participant", Npermutations = 10000)
  lab <- if (k == "real") "PHQ-9 total" else "A variable of pure noise"
  draw(pr, file.path(OUT, paste0("01_fig_", k, "__token.png")), lab, "fdr")
  draw(pp_replace_pvalues(pr, tst, "p"), file.path(OUT, paste0("01_fig_", k, "__participant.png")), lab, "fdr")
  data.frame(case = k, words = pk[["n"]], token_fdr = pk[["sig"]],
             participant_fdr = sum(tst$p_fdr < ALPHA))
}))
print(cases); write.csv(cases, file.path(OUT, "01_cases.csv"), row.names = FALSE)

## 3. calibration --------------------------------------------------------------
noise <- noise_set(nrow(dw), K_NOISE)
cal <- do.call(rbind, lapply(seq_along(noise), function(i) {
  tk <- pp_test(S, noise[[i]], null_method = "token", seed = 1003)
  pp <- pp_test(S, noise[[i]], null_method = "participant", Npermutations = B_CAL, seed = 1003 + i)
  if (i %% 10 == 0) log_step("  noise ", i, "/", length(noise))
  data.frame(i = i, null = c("token", "participant"),
             pct_p05 = 100 * c(mean(tk$p < .05), mean(pp$p < .05)),
             fdr = c(sum(tk$p_fdr < ALPHA), sum(pp$p_fdr < ALPHA)))
}))
write.csv(cal, file.path(OUT, "01_calibration.csv"), row.names = FALSE)
agg <- do.call(rbind, lapply(split(cal, cal$null), function(d) data.frame(
  null = d$null[1], n_noise = nrow(d), share_any_fdr = mean(d$fdr > 0),
  mean_pct_p05 = mean(d$pct_p05), median_pct_p05 = stats::median(d$pct_p05),
  median_fdr = stats::median(d$fdr), max_fdr = max(d$fdr))))
print(agg); write.csv(agg, file.path(OUT, "01_calibration_summary.csv"), row.names = FALSE)

## 4. seed dependence of the package's null ------------------------------------
seeds <- do.call(rbind, lapply(1:20, function(s) {
  t <- pp_test(S, xs$noise, null_method = "token", seed = s)
  data.frame(seed = s, pct_p05 = 100 * mean(t$p < .05), fdr = sum(t$p_fdr < ALPHA))
}))
write.csv(seeds, file.path(OUT, "01_token_seed_dependence.csv"), row.names = FALSE)
log_step("token null, 20 seeds, same noise variable: ", min(seeds$fdr), "-", max(seeds$fdr),
         " FDR-significant words")
log_step("01_projection_null.R DONE")
