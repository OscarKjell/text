###############################################################################
####  projection_permutation.R                                             ####
####                                                                       ####
####  Participant-permutation null for textProjection(), written as        ####
####  standalone functions so they can be moved into the text package.    ####
####                                                                       ####
####  The observed statistic is the package's own (reproduced to          ####
####  numerical precision):                                               ####
####    1. split participants on x (bottom vs top quartile, or the mean)   ####
####    2. each group's word types weighted by their frequency in the      ####
####       group (the package's uncount(n)) -> group mean embedding        ####
####    3. direction d = mean_high - mean_low; midpoint = (sum) / 2         ####
####    4. dot = (word embedding - midpoint) . d                           ####
####                                                                       ####
####  Two nulls:                                                          ####
####    "token"       = the package's current null, reimplemented: one    ####
####                    coin flip per word TOKEN, one random axis, word    ####
####                    positions bootstrapped from the token pool, one    ####
####                    pooled null for every word.                        ####
####    "participant" = shuffle x across participants and recompute 1-4;   ####
####                    every word gets its own null.                      ####
####                                                                       ####
####  Everything works on a participants x word-types count matrix C, so   ####
####  one permutation is two column sums and a few matrix-vector products. ####
###############################################################################

#' Strip subword markers and case from word-type embeddings
#'
#' textEmbed() keeps tokenizer tokens as they are: RoBERTa/GPT-2 style
#' tokens carry a leading "Ġ" (space marker) and keep their case,
#' SentencePiece tokens carry "▁". The words that textProjection()
#' tests come from NLTK, lower-cased, so they never match these tokens.
#' This removes the markers, lower-cases, and merges duplicates by a
#' frequency-weighted mean. WordPiece continuation pieces ("##ing") and
#' special tokens ([CLS], <s>, ...) are dropped.
#' @param word_types Tibble with columns words, n, Dim1...
#' @return The same structure with normalised, unique words.
pp_normalise_word_types <- function(word_types) {
  wt <- as.data.frame(word_types)
  dims <- grep("^Dim", names(wt), value = TRUE)
  w <- wt$words
  w <- gsub("^[Ġ▁]", "", w)
  w <- tolower(w)
  drop <- grepl("^##", w) | grepl("^(\\[[a-z]+\\]|<[^>]+>)$", w) | w == ""
  wt <- wt[!drop, , drop = FALSE]; w <- w[!drop]
  n <- if ("n" %in% names(wt)) wt$n else rep(1, nrow(wt))
  E <- as.matrix(wt[, dims])
  g <- factor(w, levels = unique(w))
  Ew <- rowsum(E * n, g) / as.vector(rowsum(n, g))
  out <- tibble::tibble(words = levels(g), n = as.vector(rowsum(n, g)))
  dplyr::bind_cols(out, tibble::as_tibble(Ew, .name_repair = "minimal"))
}

#' Participants x word-types count matrix, tokenised the way textProjection() does
#'
#' Lower-cases, turns "/" into a space and tokenises with NLTK
#' (text:::unique_freq_words does the same), so the word types match the
#' package one for one. Requires text's Python environment.
#' @param texts Character vector, one text per participant.
#' @return Sparse dgCMatrix, participants x word types, column names = words.
pp_count_matrix <- function(texts) {
  nltk <- reticulate::import("nltk")
  toks <- lapply(gsub("/", " ", tolower(texts)), nltk$tokenize$word_tokenize)
  vocab <- sort(unique(unlist(toks)))
  i <- rep(seq_along(toks), vapply(toks, length, integer(1)))
  j <- match(unlist(toks), vocab)
  C <- Matrix::sparseMatrix(i = i, j = j, x = 1, dims = c(length(toks), length(vocab)))
  colnames(C) <- vocab
  C
}

#' Align counts with embeddings
#'
#' The vocabulary that BUILDS the axis is every word type with an embedding
#' (as in textProjection(), whose group means use all word types); the
#' frequency floor only decides what is TESTED. Keeping these separate is
#' what reproduces the package's dot products.
#' @param C Count matrix from pp_count_matrix().
#' @param word_types Word-type embeddings (words, n, Dim...).
#' @param min_freq_words_test Words with total count below this are not tested.
#' @return list(C, E, words, test, coverage)
pp_setup <- function(C, word_types, min_freq_words_test = 0) {
  dims <- grep("^Dim", names(word_types), value = TRUE)
  keep <- intersect(colnames(C), word_types$words)
  E <- as.matrix(word_types[match(keep, word_types$words), dims])
  rownames(E) <- keep
  Cc <- C[, keep, drop = FALSE]
  tot <- Matrix::colSums(C)
  coverage <- c(word_types = length(keep) / ncol(C),
                tokens = sum(tot[keep]) / sum(tot))
  list(C = Cc, E = E, words = keep, coverage = coverage,
       test = which(Matrix::colSums(Cc) >= min_freq_words_test))
}

pp_groups <- function(x, split = c("quartile", "mean")) {
  split <- match.arg(split)
  if (split == "quartile") {
    q <- stats::quantile(x, c(.25, .75), na.rm = TRUE)
    list(g1 = which(x <= q[1]), g2 = which(x >= q[2]))
  } else {
    m <- mean(x, na.rm = TRUE)
    list(g1 = which(x <= m), g2 = which(x > m))
  }
}

# frequency-weighted mean embedding of a set of participants
pp_group_mean <- function(S, rows) {
  cnt <- Matrix::colSums(S$C[rows, , drop = FALSE])
  as.vector(crossprod(S$E, cnt) / sum(cnt))
}

#' Every word's dot product for one assignment of x
pp_dots <- function(S, x, split = "quartile") {
  gr <- pp_groups(x, split)
  m1 <- pp_group_mean(S, gr$g1)
  m2 <- pp_group_mean(S, gr$g2)
  d <- m2 - m1
  as.vector(S$E %*% d) - sum((m1 + m2) / 2 * d)
}

#' The package's current null, reimplemented (pooled across words)
#' Npermutations / n_per_split rounded up = number of random axes; the
#' package default (10000 / 50000) gives ONE axis.
pp_null_token <- function(S, x, split = "quartile",
                          Npermutations = 10000, n_per_split = 50000) {
  gr <- pp_groups(x, split)
  m1 <- pp_group_mean(S, gr$g1); m2 <- pp_group_mean(S, gr$g2)
  mid <- (m1 + m2) / 2
  n_all <- Matrix::colSums(S$C[c(gr$g1, gr$g2), , drop = FALSE])
  out <- lapply(seq_len(ceiling(Npermutations / n_per_split)), function(b) {
    k  <- stats::rbinom(length(n_all), as.integer(n_all), 0.5)  # tokens to half A
    dA <- as.vector(crossprod(S$E, k) / sum(k))
    dB <- as.vector(crossprod(S$E, n_all - k) / sum(n_all - k))
    rd <- dB - dA
    idx <- sample.int(length(n_all), n_per_split, replace = TRUE, prob = n_all / sum(n_all))
    as.vector(S$E[idx, , drop = FALSE] %*% rd) - sum(mid * rd)
  })
  unlist(out)
}

#' Participant permutation: words x Npermutations matrix of null dot products
pp_null_participant <- function(S, x, split = "quartile", Npermutations = 10000) {
  n <- length(x)
  vapply(seq_len(Npermutations), function(b) pp_dots(S, x[sample.int(n)], split),
         numeric(nrow(S$E)))
}

#' One test
#' @param null_method "participant" (proposed default) or "token" (current).
#' @return tibble: words, n, dot, p, p_fdr, plus p_z (normal approximation from
#'   each word's own null; no 1/(B+1) floor; trust only where the normal fits).
pp_test <- function(S, x, split = "quartile",
                    null_method = c("participant", "token"),
                    Npermutations = 10000, seed = 1003) {
  null_method <- match.arg(null_method)
  force(x)                       # evaluate x before set.seed (see note in HANDOFF)
  set.seed(seed)
  obs <- pp_dots(S, x, split)
  if (null_method == "token") {
    nul <- pp_null_token(S, x, split, Npermutations)
    # the package: 2 * min(left, right) against the pooled null
    p <- vapply(obs, function(o) min(1, 2 * min(mean(nul <= o), mean(nul >= o))), 0)
    p[p == 0] <- 1 / (length(nul) + 1)
    mu <- mean(nul); sdv <- stats::sd(nul)
  } else {
    nul <- pp_null_participant(S, x, split, Npermutations)
    p <- (1 + rowSums(abs(nul) >= abs(obs))) / (Npermutations + 1)
    mu <- rowMeans(nul); sdv <- apply(nul, 1, stats::sd)
  }
  p_z <- 2 * stats::pnorm(-abs((obs - mu) / sdv))
  k <- S$test
  tibble::tibble(words = S$words[k], n = as.integer(Matrix::colSums(S$C))[k],
                 dot = obs[k], p = p[k], p_fdr = stats::p.adjust(p[k], "fdr"),
                 p_z = p_z[k], null_method = null_method)
}

#' Put p-values from pp_test() into a textProjection() object for plotting
#' Plot with textProjectionPlot(..., p_adjust_method = "fdr") on raw p, or
#' p_col = "p_fdr" with p_adjust_method = "none" (never correct twice).
pp_replace_pvalues <- function(proj_res, test, p_col = "p") {
  wd <- proj_res$word_data
  i <- match(wd$words, test$words)
  ok <- !is.na(i)
  wd$p_values_dot.x[ok] <- test[[p_col]][i[ok]]
  proj_res$word_data <- wd
  proj_res
}
