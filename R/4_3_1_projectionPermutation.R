# =============================================================================
# 4_3_1_projectionPermutation.R
# =============================================================================
# Internal functions for the participant-permutation null in textProjection()
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
  used <- 0
  for (b in seq_len(Npermutations)) {
    nul <- perm_dots(S, x[sample.int(n)], split, floor_n)
    # A shuffled group can end up with no embedded tokens (sum(cnt) = 0),
    # which leaves the axis undefined; such draws are skipped and the
    # p-values use the number of valid permutations.
    if (anyNA(nul)) next
    used <- used + 1
    exceed <- exceed + (abs(nul) >= abs(obs) - 1e-12)
    s1 <- s1 + nul
    s2 <- s2 + nul^2
  }
  if (used == 0) {
    warning("No valid permutations (a group had no embedded tokens in every draw); p-values are NA.")
    return(tibble::tibble(words = S$words, p = NA_real_, p_z = NA_real_))
  }
  p <- (1 + exceed) / (used + 1)
  mu <- s1 / used
  sdv <- sqrt(pmax(0, (s2 - used * mu^2) / (used - 1)))
  p_z <- 2 * stats::pnorm(-abs((obs - mu) / sdv))
  tibble::tibble(words = S$words, p = p, p_z = p_z)
}


#' Project entire texts with participant-permutation p-values
#'
#' Projects each participant's response embedding onto the supervised
#' dimension: the axis d = mean_high - mean_low and the anchoring midpoint
#' are rebuilt from the word machinery (identical to textProjection()'s
#' observed axis under default settings), and each text's dot product is
#' text_embedding %*% d - anchor %*% d. The null shuffles x across
#' participants and recomputes the whole statistic, so every text is
#' compared with its own null distribution (texts are participant-level
#' units, which is why the token null is not offered here):
#' p = (1 + #\{|null| >= |observed|\}) / (Npermutations + 1); p_z is the
#' normal approximation from the text's own null; cohens_d standardises the
#' observed dot by that null's mean and SD.
#'
#' @param S Setup from perm_setup().
#' @param Tm Matrix participants x embedding dimensions (response embeddings).
#' @param x Numeric, one value per participant.
#' @return tibble(dot, p, p_z, cohens_d), one row per participant.
#' @noRd
perm_test_texts <- function(S, Tm, x, split = "quartile", floor_n = 0,
                            Npermutations = 10000) {
  if (ncol(Tm) != ncol(S$E)) {
    stop(
      "project_texts: word_embeddings have ", ncol(Tm),
      " dimensions but word_types_embeddings have ", ncol(S$E),
      "; both must come from the same model/layers."
    )
  }
  text_dots <- function(xx) {
    gr <- perm_groups(xx, split)
    m1 <- perm_group_mean(S, gr$g1, floor_n)
    m2 <- perm_group_mean(S, gr$g2, floor_n)
    d <- m2 - m1
    as.vector(Tm %*% d) - sum((m1 + m2) / 2 * d)
  }
  obs <- text_dots(x)
  n <- length(x)
  exceed <- numeric(length(obs))
  s1 <- numeric(length(obs))
  s2 <- numeric(length(obs))
  used <- 0
  for (b in seq_len(Npermutations)) {
    nul <- text_dots(x[sample.int(n)])
    # A shuffled group can end up with no embedded tokens (sum(cnt) = 0),
    # which leaves the axis undefined; such draws are skipped and the
    # p-values use the number of valid permutations.
    if (anyNA(nul)) next
    used <- used + 1
    exceed <- exceed + (abs(nul) >= abs(obs) - 1e-12)
    s1 <- s1 + nul
    s2 <- s2 + nul^2
  }
  if (used == 0) {
    warning("No valid permutations (a group had no embedded tokens in every draw); p-values are NA.")
    return(tibble::tibble(
      dot = obs, p = NA_real_, p_z = NA_real_, cohens_d = NA_real_
    ))
  }
  p <- (1 + exceed) / (used + 1)
  mu <- s1 / used
  sdv <- sqrt(pmax(0, (s2 - used * mu^2) / (used - 1)))
  tibble::tibble(
    dot = obs,
    p = p,
    p_z = 2 * stats::pnorm(-abs((obs - mu) / sdv)),
    cohens_d = (obs - mu) / sdv
  )
}


# =============================================================================
# Linear-smoother functions for textWordPrediction()
# =============================================================================
# textWordPrediction() computes, for each word type w:
#
#   m_w   = the frequency-weighted mean of x over the participants who used w
#           (if "happy" occurs once for a participant with x = 2 and twice for
#           one with x = 5, then m = (1*2 + 2*5) / 3). With C the participants
#           x words token-count matrix and D = diag(colSums(C)), all word
#           means at once are the vector  m = D^-1 C' x.
#
#   pos_w = the plotted position: a ridge regression predicts m from the word
#           EMBEDDINGS (pooling information across semantically similar
#           words), and pos_w is the LEAVE-ONE-OUT (LOO) prediction -- what a
#           model trained on all OTHER words predicts for word w -- so a word
#           cannot trivially reproduce its own mean.
#
# Two classical linear-algebra facts make the permutation test fast:
#
#   (1) Ridge regression with a FIXED penalty lambda is a "linear smoother":
#       its fitted values are a fixed matrix times the outcomes, yhat = H m,
#       where H = 11'/W + Xc (Xc'Xc + lambda I)^-1 Xc' depends only on the
#       embeddings and lambda -- not on x.
#
#   (2) Linear smoothers have an EXACT closed-form LOO prediction (the same
#       identity that underlies LOO cross-validation for ridge):
#           pos_w = (yhat_w - h_w * m_w) / (1 - h_w),
#       where h_w = H[w, w] is word w's leverage. No refitting per word; the
#       unit tests verify this against brute-force refits to ~1e-15.
#
# Chaining the two steps, pos = L m = L D^-1 C' x = A x, where A (words x
# participants) depends only on counts, embeddings and lambda. A is computed
# ONCE; each permutation of x across participants (the null "x is unrelated
# to word use") is then a single matrix product instead of a full refit,
# which is what makes 10,000 permutations take seconds. Every word is
# compared with its own null distribution, so p-values are never 0 by
# construction: p = (1 + #{|null| >= |obs|}) / (Npermutations + 1).
#
# lambda is chosen once on the OBSERVED x (minimum LOO error) and held fixed
# across permutations; re-tuning it per permutation would break linearity.
# Calibration on pure-noise variables shows ~5% of words at p < .05, as a
# well-calibrated test should (see dev/permutation_handoff/).
# =============================================================================

#' Prepare the fixed ingredients: aligned counts, embeddings, and their SVD
#'
#' Everything returned here depends only on the texts and the embeddings --
#' never on x -- so it is computed once and reused for every permutation.
#' @noRd
wp_setup <- function(C, word_types, min_freq_words_test = 3,
                     word_pattern = NULL) {
  dims <- grep("^Dim", names(word_types), value = TRUE)
  # Keep the words that occur in the texts AND have an embedding; words
  # matching word_pattern (e.g., bracket artefacts) are excluded up front.
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
  # Constant embedding dimensions carry no information and would break the
  # standardisation below.
  E <- E[, apply(E, 2, stats::sd) > 0, drop = FALSE]
  # The ridge predictors: standardised, then centred (the intercept is
  # handled separately in wp_hat as the 11'/W term).
  Xc <- scale(scale(E), center = TRUE, scale = FALSE) # standardised, centred
  # Thin SVD of the predictors, Xc = U diag(d) V'. All ridge quantities below
  # (fits, leverages, LOO) can be written in terms of U and d alone, so a new
  # lambda costs almost nothing -- this is what makes the lambda grid search
  # in wp_lambda cheap. Near-zero singular values are numerical noise
  # (embedding dimensions > words); dropping them stabilises the solve.
  sv <- svd(Xc)
  r <- sv$d > 1e-8 * sv$d[1]
  list(
    C = C, words = colnames(C), n = nn, Xs = scale(E),
    u = sv$u[, r, drop = FALSE], d = sv$d[r],
    M0 = Matrix::t(C) / nn, # D^-1 C' (words x participants): m = M0 %*% x
    len = Matrix::rowSums(C), # tokens per participant (for centre = "token_mean")
    W = ncol(C),
    test = which(nn >= min_freq_words_test) # words frequent enough to test
  )
}

#' The ridge smoother in SVD form: shrinkage factors and leverages
#'
#' In the SVD basis the ridge smoother is H = 11'/W + U diag(f) U' with
#' f_j = d_j^2 / (d_j^2 + lambda): each principal direction of the embedding
#' space is shrunk by its own factor f_j (lambda = 0 gives ordinary least
#' squares, lambda -> Inf leaves only the intercept). h is the diagonal of H
#' (the "leverage" of each word: how much its own outcome influences its own
#' fitted value), which is all the LOO identity in wp_loo needs.
#' @noRd
wp_hat <- function(F_obj, lambda) {
  f <- F_obj$d^2 / (F_obj$d^2 + lambda)
  list(f = f, h = 1 / F_obj$W + rowSums(sweep(F_obj$u^2, 2, f, "*")))
}

#' Exact leave-one-out ridge predictions for the columns of Y (words x k)
#'
#' Uses the classical linear-smoother identity
#'   loo_w = (fit_w - h_w * y_w) / (1 - h_w):
#' subtract word w's own (leverage-weighted) contribution from its fitted
#' value and rescale. This equals refitting the ridge without word w --
#' exactly, not approximately (verified against brute-force refits in the
#' tests) -- at the cost of one matrix product for all words at once.
#' Works column-wise, so Y can be the vector m (giving the positions) or the
#' whole matrix M0 (giving the words x participants map A used for the
#' permutations).
#' @noRd
wp_loo <- function(F_obj, Y, hh) {
  Y <- as.matrix(Y)
  # H Y = intercept part (column means) + shrunken projection onto the
  # principal directions, U diag(f) U' Y
  HY <- matrix(colMeans(Y), nrow(Y), ncol(Y), byrow = TRUE) +
    F_obj$u %*% (hh$f * crossprod(F_obj$u, Y))
  (HY - hh$h * Y) / (1 - hh$h)
}

#' Choose lambda once by minimum leave-one-out error on the observed x
#'
#' lambda is selected on the observed word means m and then HELD FIXED for
#' all permutations: re-tuning per permutation would make pos a non-linear
#' function of x and break the pos = A x shortcut. The SVD form makes each
#' grid point cheap (no refit, just new shrinkage factors).
#' @noRd
wp_lambda <- function(F_obj, m, grid = 10^seq(-1, 7, by = 0.05)) {
  err <- vapply(grid, function(l) sum((m - wp_loo(F_obj, m, wp_hat(F_obj, l)))^2), 0)
  l <- grid[which.min(err)]
  if (l %in% range(grid)) {
    # On pure noise the best penalty is effectively infinite (the smoother
    # collapses to the intercept), so hitting the upper edge is expected
    # there and not a numerical problem.
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
  # Standardising x centres the plotted positions, so a plot split at 0
  # separates low from high scorers.
  x <- as.numeric(scale(x))
  F_obj <- wp_setup(C, word_types, min_freq_words_test, word_pattern)
  m <- as.vector(F_obj$M0 %*% x) # observed word means, m = D^-1 C' x
  lam <- wp_lambda(F_obj, m) # penalty tuned once, then held fixed
  hh <- wp_hat(F_obj, lam)
  pos <- as.vector(wp_loo(F_obj, m, hh)) # observed positions (LOO ridge)
  # The whole pipeline as one fixed linear map: pos = wp_loo(M0 x) =
  # (LOO smoother) %*% M0 %*% x = A x. Feeding the matrix M0 through wp_loo
  # column-wise yields A directly; only the tested rows are kept since only
  # those need null distributions.
  A <- wp_loo(F_obj, as.matrix(F_obj$M0), hh)[F_obj$test, , drop = FALSE]
  # centre = "token_mean": subtract the token-weighted mean of x, i.e., the
  # value every word's mean is pulled towards when x is related to how much
  # people write. Because the smoother reproduces constants, subtracting it
  # from the positions isolates word-specific signal from that verbosity
  # effect. It is linear in x, so the same shift applies inside the
  # permutation loop below.
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
  # Permutation loop, in blocks of 1,000 shuffles: each block stacks the
  # shuffled x vectors into a matrix XP, so A %*% XP evaluates 1,000
  # complete pipeline runs in one product (blocking caps memory at
  # tested-words x 1,000 instead of x Npermutations). Only running
  # summaries are kept per word: the exceedance count for p, and the sum
  # and sum of squares of the null values for p_z.
  exceed <- numeric(length(F_obj$test))
  s1 <- numeric(length(F_obj$test))
  s2 <- numeric(length(F_obj$test))
  for (b0 in seq(1, Npermutations, by = 1000)) {
    nb <- min(1000, Npermutations - b0 + 1)
    XP <- vapply(seq_len(nb), function(i) sample(x), numeric(length(x)))
    NR <- as.matrix(A %*% XP) # null positions: words x nb
    if (centre == "token_mean") {
      NR <- sweep(NR, 2, colSums(F_obj$len * XP) / sum(F_obj$len))
    }
    # the small tolerance keeps ties (|null| == |obs|) counted as exceedances
    exceed <- exceed + rowSums(abs(NR) >= abs(obs) - 1e-12)
    s1 <- s1 + rowSums(NR)
    s2 <- s2 + rowSums(NR^2)
  }
  # Exceedance p with the add-one correction: (1 + count) / (B + 1) can never
  # be 0, and its smallest value is 1 / (B + 1).
  p <- (1 + exceed) / (Npermutations + 1)
  # p_z: fit a normal to each word's own null (mean, SD from the running
  # sums) and read the two-sided p off that curve. Unlike the exceedance p
  # it has no 1 / (B + 1) floor, which helps when FDR-correcting thousands
  # of words; it assumes each null is roughly normal.
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
