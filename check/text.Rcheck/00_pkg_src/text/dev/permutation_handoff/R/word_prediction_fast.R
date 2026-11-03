###############################################################################
####  word_prediction_fast.R                                               ####
####                                                                       ####
####  A corrected version of textWordPrediction() (text 1.9 dev):          ####
####  same idea, valid and fast word-level p-values.                       ####
####                                                                       ####
####  For each word type:                                                 ####
####    m   = frequency-weighted mean of x over the participants who used  ####
####          it                (TOKEN counts, not the regex substring      ####
####          count of the current function)                              ####
####    pos = leave-one-out ridge prediction of m from the word's          ####
####          embedding         (the plotted position)                     ####
####                                                                       ####
####  With the ridge penalty lambda fixed, both steps are linear in x:     ####
####    m   = D^-1 C' x                         (C participants x words)    ####
####    H   = 11'/W + Xc (Xc'Xc + lambda I)^-1 Xc'        (ridge smoother)  ####
####    pos = (H m - h*m) / (1 - h),  h = diag(H)      (exact LOO)          ####
####  so pos = A x with A (words x participants) computed ONCE. Each        ####
####  permutation of x is one matrix product -> 10,000 permutations in     ####
####  seconds, p = (1 + #{|null| >= |obs|}) / (B + 1), BH over tested words.####
####                                                                       ####
####  lambda is chosen once, on the observed x (minimum LOO error), and    ####
####  held fixed for the permutations.                                    ####
####                                                                       ####
####  centre = "token_mean" subtracts the token-weighted mean of x from    ####
####  every word average, so a variable related to how much people write   ####
####  does not push all frequent words one way. Because the smoother       ####
####  reproduces constants, this is pos - sum(len * x) / sum(len).         ####
###############################################################################

wpf_setup <- function(C, word_types, min_freq_words_test = 3,
                      word_pattern = "^[[:alnum:]'-]+$") {
  dims <- grep("^Dim", names(word_types), value = TRUE)
  keep <- intersect(colnames(C), word_types$words)
  if (!is.null(word_pattern)) keep <- keep[grepl(word_pattern, keep)]
  C <- C[, keep, drop = FALSE]
  nn <- Matrix::colSums(C)
  C <- C[, nn > 0, drop = FALSE]; nn <- nn[nn > 0]
  E <- as.matrix(word_types[match(colnames(C), word_types$words), dims])
  E <- E[, apply(E, 2, stats::sd) > 0, drop = FALSE]
  Xc <- scale(scale(E), center = TRUE, scale = FALSE)        # standardised, centred
  sv <- svd(Xc)
  r  <- sv$d > 1e-8 * sv$d[1]
  list(C = C, words = colnames(C), n = nn, Xs = scale(E),
       u = sv$u[, r, drop = FALSE], d = sv$d[r],
       M0 = Matrix::t(C) / nn,                                 # D^-1 C' (W x N)
       len = Matrix::rowSums(C), W = ncol(C),
       test = which(nn >= min_freq_words_test))
}

wpf_hat <- function(F, lambda) {
  f <- F$d^2 / (F$d^2 + lambda)
  list(f = f, h = 1 / F$W + rowSums(sweep(F$u^2, 2, f, "*")))
}

# exact leave-one-out ridge predictions for the columns of Y (W x k)
wpf_loo <- function(F, Y, hh) {
  Y <- as.matrix(Y)
  HY <- matrix(colMeans(Y), nrow(Y), ncol(Y), byrow = TRUE) +
    F$u %*% (hh$f * crossprod(F$u, Y))
  (HY - hh$h * Y) / (1 - hh$h)
}

wpf_lambda <- function(F, m, grid = 10^seq(-1, 7, by = 0.05)) {
  err <- vapply(grid, function(l) sum((m - wpf_loo(F, m, wpf_hat(F, l)))^2), 0)
  l <- grid[which.min(err)]
  if (l %in% range(grid)) warning("lambda at the edge of the grid: ", l)
  l
}

#' Word-level prediction scores with participant-permutation p-values
#'
#' @param C Participants x word-types count matrix (pp_count_matrix()).
#' @param word_types Word-type embeddings (words, n, Dim...).
#' @param x Numeric, one value per participant (NAs are dropped, with their rows).
#' @param centre "token_mean" (recommended) or "none" (the current behaviour).
#' @param Npermutations Number of permutations of x.
#' @return list(word_data = tibble(words, n, word_mean_value_x, x_plotted,
#'   p_values_x, p_fdr), lambda, loo_r, seconds). Columns follow what
#'   textProjectionPlot() expects for type textWordPrediction.
textWordPredictionFast <- function(C, word_types, x, min_freq_words_test = 3,
                                   centre = c("token_mean", "none"),
                                   Npermutations = 10000, seed = 1003) {
  centre <- match.arg(centre)
  t0 <- Sys.time()
  ok <- !is.na(x)
  x  <- as.numeric(scale(x[ok]))
  F  <- wpf_setup(C[ok, , drop = FALSE], word_types, min_freq_words_test)
  m  <- as.vector(F$M0 %*% x)
  lam <- wpf_lambda(F, m)
  hh  <- wpf_hat(F, lam)
  pos <- as.vector(wpf_loo(F, m, hh))
  A   <- wpf_loo(F, as.matrix(F$M0), hh)[F$test, , drop = FALSE]  # tested words x N
  shift <- function(xx) if (centre == "token_mean") sum(F$len * xx) / sum(F$len) else 0
  obs <- pos[F$test] - shift(x)
  ex  <- numeric(length(F$test))
  set.seed(seed)
  for (b0 in seq(1, Npermutations, by = 1000)) {
    nb <- min(1000, Npermutations - b0 + 1)
    XP <- vapply(seq_len(nb), function(i) sample(x), numeric(length(x)))
    NR <- as.matrix(A %*% XP)
    if (centre == "token_mean") NR <- sweep(NR, 2, colSums(F$len * XP) / sum(F$len))
    ex <- ex + rowSums(abs(NR) >= abs(obs) - 1e-12)
  }
  p <- (1 + ex) / (Npermutations + 1)
  wd <- tibble::tibble(words = F$words[F$test], n = as.integer(F$n[F$test]),
                       word_mean_value_x = m[F$test] - shift(x),
                       x_plotted = obs, p_values_x = p,
                       p_fdr = stats::p.adjust(p, "BH"))
  out <- list(word_data = wd, lambda = lam, loo_r = stats::cor(m, pos),
              seconds = as.numeric(difftime(Sys.time(), t0, units = "secs")))
  comment(out) <- paste("type = textWordPrediction ; method = linear-smoother permutation ;",
                        "centre =", centre, "; Npermutations =", Npermutations,
                        "; lambda =", signif(lam, 4))
  out
}

#' Check the LOO shortcut against explicit refits (returns max abs difference)
wpf_check_loo <- function(C, word_types, x, n_words = 20, min_freq_words_test = 3) {
  x <- as.numeric(scale(x))
  F <- wpf_setup(C, word_types, min_freq_words_test)
  m <- as.vector(F$M0 %*% x); lam <- wpf_lambda(F, m)
  short <- as.vector(wpf_loo(F, m, wpf_hat(F, lam)))
  set.seed(1); ii <- sample(F$W, n_words)
  explicit <- vapply(ii, function(i) {
    X <- F$Xs[-i, , drop = FALSE]; y <- m[-i]
    mu <- colMeans(X); Xc <- sweep(X, 2, mu)
    beta <- solve(crossprod(Xc) + lam * diag(ncol(Xc)), crossprod(Xc, y - mean(y)))
    mean(y) + sum((F$Xs[i, ] - mu) * beta)
  }, 0)
  max(abs(explicit - short[ii]))
}

#' Plot with the released textProjectionPlot(): put positions and p-values
#' into a textProjection() object of the same texts (words without a result
#' are dropped). Colour = significance after the plot's own FDR adjustment.
wpf_into_projection <- function(proj_res, res) {
  d <- proj_res$word_data
  i <- match(d$words, res$word_data$words)
  keep <- !is.na(i) | d$words %in% c("Group1*", "Group2*", "projected_embedding")
  d <- d[keep, ]; i <- i[keep]
  d$dot.x[!is.na(i)]          <- res$word_data$x_plotted[i[!is.na(i)]]
  d$p_values_dot.x[!is.na(i)] <- res$word_data$p_values_x[i[!is.na(i)]]
  proj_res$word_data <- d
  proj_res
}
