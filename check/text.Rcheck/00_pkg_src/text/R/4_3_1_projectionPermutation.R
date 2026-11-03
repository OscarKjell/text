# =============================================================================
# 4_3_1_projectionPermutation.R
# =============================================================================
# Internal machinery for the participant-permutation null in textProjection()
# and for the linear-smoother permutation test in textWordPrediction().
#
# Everything operates on a sparse participants x word-types count matrix, so
# one permutation of x is a couple of column sums and matrix-vector products.
# The observed statistic reproduces textProjection()'s dot products to
# numerical precision (including the min_freq_words_test floor, which the
# package applies before aggregating the group embeddings).
# =============================================================================


#' Normalise word-type tokens for lookup
#'
#' textEmbed() stores tokenizer tokens unchanged: RoBERTa/GPT-2 style tokens
#' carry a leading "Ġ" (space marker) and keep their case, and
#' SentencePiece tokens carry "▁". The words that textProjection() and
#' textWordPrediction() test come from NLTK, lower-cased, so without
#' normalisation these tokens never match. This strips the markers,
#' lower-cases, drops WordPiece continuation pieces ("##ing") and special
#' tokens ([CLS], <s>, ...), and merges duplicates by a frequency-weighted
#' mean. For lower-cased vocabularies (e.g., bert-base-uncased) it leaves
#' matching behaviour unchanged.
#'
#' @param word_types Tibble with columns words, n, Dim1, ... .
#' @return The same structure with normalised, unique words.
#' @noRd
normalise_word_types <- function(word_types) {
  wt <- as.data.frame(word_types)
  dims <- grep("^Dim", names(wt), value = TRUE)
  w <- enc2utf8(as.character(wt$words))
  # build the marker pattern from the actual characters (UTF-8-flagged), which
  # keeps the regex engine in UTF mode in any locale and for any input
  marker_pattern <- paste0("^[", intToUtf8(0x0120), intToUtf8(0x2581), "]")
  w <- gsub(marker_pattern, "", w, perl = TRUE)
  w <- tolower(w)
  drop <- grepl("^##", w) | grepl("^(\\[[a-z]+\\]|<[^>]+>)$", w, perl = TRUE) |
    w == "" | is.na(w)
  wt <- wt[!drop, , drop = FALSE]
  w <- w[!drop]
  n <- if ("n" %in% names(wt)) wt$n else rep(1, nrow(wt))
  E <- as.matrix(wt[, dims])
  g <- factor(w, levels = unique(w))
  Ew <- rowsum(E * n, g) / as.vector(rowsum(n, g))
  out <- tibble::tibble(words = levels(g), n = as.vector(rowsum(n, g)))
  out <- dplyr::bind_cols(out, tibble::as_tibble(Ew, .name_repair = "minimal"))
  comment(out) <- comment(word_types)
  out
}


#' Participants x word-types count matrix
#'
#' Tokenises the way unique_freq_words() does (lower case, "/" to space,
#' NLTK word_tokenize), so the word types match the ones textProjection()
#' tests one for one. Requires text's Python environment.
#'
#' @param texts Character vector, one text per participant.
#' @return Sparse dgCMatrix, participants x word types; column names = words.
#' @noRd
perm_count_matrix <- function(texts) {
  nltk <- reticulate::import("nltk")
  toks <- lapply(gsub("/", " ", tolower(texts)), nltk$tokenize$word_tokenize)
  vocab <- sort(unique(unlist(toks)))
  i <- rep(seq_along(toks), vapply(toks, length, integer(1)))
  j <- match(unlist(toks), vocab)
  C <- Matrix::sparseMatrix(i = i, j = j, x = 1, dims = c(length(toks), length(vocab)))
  colnames(C) <- vocab
  C
}


#' Align the count matrix with the word-type embeddings
#'
#' @param C Count matrix from perm_count_matrix().
#' @param word_types Word-type embeddings (words, n, Dim...).
#' @return list(C, E, words, coverage); coverage gives the share of word
#'   types and of tokens that have an embedding.
#' @noRd
perm_setup <- function(C, word_types) {
  dims <- grep("^Dim", names(word_types), value = TRUE)
  keep <- intersect(colnames(C), word_types$words)
  E <- as.matrix(word_types[match(keep, word_types$words), dims])
  rownames(E) <- keep
  Cc <- C[, keep, drop = FALSE]
  tot <- Matrix::colSums(C)
  coverage <- c(
    word_types = length(keep) / ncol(C),
    tokens = sum(tot[keep]) / sum(tot)
  )
  list(C = Cc, E = E, words = keep, coverage = coverage)
}


#' Split participants into a low and a high group on x
#' @noRd
perm_groups <- function(x, split = "quartile") {
  if (split == "quartile") {
    q <- stats::quantile(x, c(.25, .75), na.rm = TRUE)
    list(g1 = which(x <= q[1]), g2 = which(x >= q[2]))
  } else { # "mean"
    m <- mean(x, na.rm = TRUE)
    list(g1 = which(x <= m), g2 = which(x > m))
  }
}


#' Frequency-weighted mean embedding of a group of participants
#'
#' Words with a within-group count below floor_n are excluded before
#' aggregating, which is what textProjection() does with
#' min_freq_words_test (the floor therefore shapes the projection axis).
#' @noRd
perm_group_mean <- function(S, rows, floor_n = 0) {
  cnt <- Matrix::colSums(S$C[rows, , drop = FALSE])
  if (floor_n > 0) cnt[cnt < floor_n] <- 0
  as.vector(Matrix::crossprod(S$E, cnt) / sum(cnt))
}


#' Every word's dot product for one assignment of x
#'
#' Reproduces textProjection()'s observed statistic: group means weighted by
#' in-group word frequency, axis d = mean_high - mean_low, each word centred
#' on the groups' midpoint and projected on d.
#' @noRd
perm_dots <- function(S, x, split = "quartile", floor_n = 0) {
  gr <- perm_groups(x, split)
  m1 <- perm_group_mean(S, gr$g1, floor_n)
  m2 <- perm_group_mean(S, gr$g2, floor_n)
  d <- m2 - m1
  as.vector(S$E %*% d) - sum((m1 + m2) / 2 * d)
}


#' Participant-permutation test for the projection
#'
#' Shuffles x across participants, recomputes the whole statistic each time,
#' and compares every word with its own null distribution:
#' p = (1 + #\{|null| >= |observed|\}) / (Npermutations + 1).
#' p_z is a normal approximation from each word's own null (mean and SD of
#' the permuted values); it has no 1 / (B + 1) floor.
#'
#' @return tibble(words, p, p_z) for every word with an embedding.
#' @noRd
perm_test_participant <- function(S, x, split = "quartile", floor_n = 0,
                                  Npermutations = 10000) {
  obs <- perm_dots(S, x, split, floor_n)
  n <- length(x)
  exceed <- numeric(length(obs))
  s1 <- numeric(length(obs))
  s2 <- numeric(length(obs))
  for (b in seq_len(Npermutations)) {
    nul <- perm_dots(S, x[sample.int(n)], split, floor_n)
    exceed <- exceed + (abs(nul) >= abs(obs) - 1e-12)
    s1 <- s1 + nul
    s2 <- s2 + nul^2
  }
  p <- (1 + exceed) / (Npermutations + 1)
  mu <- s1 / Npermutations
  sdv <- sqrt(pmax(0, (s2 - Npermutations * mu^2) / (Npermutations - 1)))
  p_z <- 2 * stats::pnorm(-abs((obs - mu) / sdv))
  tibble::tibble(words = S$words, p = p, p_z = p_z)
}


# =============================================================================
# Linear-smoother machinery for textWordPrediction()
# =============================================================================
# For each word type:
#   m   = frequency-weighted mean of x over the participants who used it
#         (token counts from the count matrix)
#   pos = leave-one-out ridge prediction of m from the word's embedding
#         (the plotted position)
# With the ridge penalty lambda fixed, both steps are linear in x:
#   m   = D^-1 C' x,   pos = (H m - h m) / (1 - h) with H the ridge smoother,
# so pos = A x with A computed once; each permutation of x is one
# matrix product.
# =============================================================================

#' @noRd
wp_setup <- function(C, word_types, min_freq_words_test = 3,
                     word_pattern = NULL) {
  dims <- grep("^Dim", names(word_types), value = TRUE)
  keep <- intersect(colnames(C), word_types$words)
  if (!is.null(word_pattern)) keep <- keep[!grepl(word_pattern, keep)]
  if (length(keep) < 3) {
    stop(
      "Fewer than 3 words have a valid embedding; ",
      "check that words and word_types_embeddings match."
    )
  }
  C <- C[, keep, drop = FALSE]
  nn <- Matrix::colSums(C)
  C <- C[, nn > 0, drop = FALSE]
  nn <- nn[nn > 0]
  E <- as.matrix(word_types[match(colnames(C), word_types$words), dims])
  E <- E[, apply(E, 2, stats::sd) > 0, drop = FALSE]
  Xc <- scale(scale(E), center = TRUE, scale = FALSE) # standardised, centred
  sv <- svd(Xc)
  r <- sv$d > 1e-8 * sv$d[1]
  list(
    C = C, words = colnames(C), n = nn, Xs = scale(E),
    u = sv$u[, r, drop = FALSE], d = sv$d[r],
    M0 = Matrix::t(C) / nn, # D^-1 C'  (words x participants)
    len = Matrix::rowSums(C), W = ncol(C),
    test = which(nn >= min_freq_words_test)
  )
}

#' @noRd
wp_hat <- function(F_obj, lambda) {
  f <- F_obj$d^2 / (F_obj$d^2 + lambda)
  list(f = f, h = 1 / F_obj$W + rowSums(sweep(F_obj$u^2, 2, f, "*")))
}

#' Exact leave-one-out ridge predictions for the columns of Y (words x k)
#' @noRd
wp_loo <- function(F_obj, Y, hh) {
  Y <- as.matrix(Y)
  HY <- matrix(colMeans(Y), nrow(Y), ncol(Y), byrow = TRUE) +
    F_obj$u %*% (hh$f * crossprod(F_obj$u, Y))
  (HY - hh$h * Y) / (1 - hh$h)
}

#' Choose lambda once by minimum leave-one-out error on the observed x
#' @noRd
wp_lambda <- function(F_obj, m, grid = 10^seq(-1, 7, by = 0.05)) {
  err <- vapply(grid, function(l) sum((m - wp_loo(F_obj, m, wp_hat(F_obj, l)))^2), 0)
  l <- grid[which.min(err)]
  if (l %in% range(grid)) {
    message(colourise(paste0(
      "Note: the ridge penalty is at the edge of the grid (lambda = ", l,
      "); this is expected when the embeddings carry little signal about x."
    ), "blue"))
  }
  l
}

#' Word-level prediction scores with participant-permutation p-values
#'
#' @param C Participants x word-types count matrix.
#' @param word_types Word-type embeddings (words, n, Dim...).
#' @param x Numeric, one value per participant (no NAs; drop them upstream).
#' @param centre "none" keeps the raw word means; "token_mean" subtracts the
#'   token-weighted mean of x, separating word-specific signal from a
#'   verbosity effect (x related to how much people write).
#' @param word_pattern Words matching this regex are removed before testing.
#' @return list(word_data = tibble(words, n, word_mean_value, plotted,
#'   p_values, p_z), lambda, loo_r)
#' @noRd
wp_run <- function(C, word_types, x, min_freq_words_test = 3,
                   centre = "none", Npermutations = 10000,
                   word_pattern = NULL) {
  x <- as.numeric(scale(x))
  F_obj <- wp_setup(C, word_types, min_freq_words_test, word_pattern)
  m <- as.vector(F_obj$M0 %*% x)
  lam <- wp_lambda(F_obj, m)
  hh <- wp_hat(F_obj, lam)
  pos <- as.vector(wp_loo(F_obj, m, hh))
  A <- wp_loo(F_obj, as.matrix(F_obj$M0), hh)[F_obj$test, , drop = FALSE]
  shift <- function(xx) {
    if (centre == "token_mean") sum(F_obj$len * xx) / sum(F_obj$len) else 0
  }
  obs <- pos[F_obj$test] - shift(x)
  if (Npermutations <= 0) {
    wd <- tibble::tibble(
      words = F_obj$words[F_obj$test],
      n = as.integer(F_obj$n[F_obj$test]),
      word_mean_value = m[F_obj$test] - shift(x),
      plotted = obs,
      p_values = NA_real_,
      p_z = NA_real_
    )
    return(list(word_data = wd, lambda = lam, loo_r = stats::cor(m, pos)))
  }
  exceed <- numeric(length(F_obj$test))
  s1 <- numeric(length(F_obj$test))
  s2 <- numeric(length(F_obj$test))
  for (b0 in seq(1, Npermutations, by = 1000)) {
    nb <- min(1000, Npermutations - b0 + 1)
    XP <- vapply(seq_len(nb), function(i) sample(x), numeric(length(x)))
    NR <- as.matrix(A %*% XP)
    if (centre == "token_mean") {
      NR <- sweep(NR, 2, colSums(F_obj$len * XP) / sum(F_obj$len))
    }
    exceed <- exceed + rowSums(abs(NR) >= abs(obs) - 1e-12)
    s1 <- s1 + rowSums(NR)
    s2 <- s2 + rowSums(NR^2)
  }
  p <- (1 + exceed) / (Npermutations + 1)
  mu <- s1 / Npermutations
  sdv <- sqrt(pmax(0, (s2 - Npermutations * mu^2) / (Npermutations - 1)))
  p_z <- 2 * stats::pnorm(-abs((obs - mu) / sdv))
  wd <- tibble::tibble(
    words = F_obj$words[F_obj$test],
    n = as.integer(F_obj$n[F_obj$test]),
    word_mean_value = m[F_obj$test] - shift(x),
    plotted = obs,
    p_values = p,
    p_z = p_z
  )
  list(word_data = wd, lambda = lam, loo_r = stats::cor(m, pos))
}
