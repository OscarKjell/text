library(tibble)
library(dplyr)
library(text)
library(testthat)

context("participant permutation null")

# Synthetic data only (no Python): a random count matrix and random
# "embeddings", so these tests are fast and run everywhere.

make_toy <- function(n = 120, W = 60, dims = 8, seed = 1) {
  set.seed(seed)
  C <- Matrix::Matrix(stats::rpois(n * W, 0.6), n, W, sparse = TRUE)
  colnames(C) <- paste0("w", seq_len(W))
  E <- matrix(stats::rnorm(W * dims), W, dims)
  wt <- tibble::tibble(words = colnames(C), n = as.integer(Matrix::colSums(C)))
  wt <- dplyr::bind_cols(
    wt,
    tibble::as_tibble(E, .name_repair = ~ paste0("Dim", seq_len(dims)))
  )
  list(C = C, wt = wt, n = n)
}

test_that("perm_dots equals a direct computation of the projection", {
  skip_on_cran()
  d <- make_toy()
  S <- text:::perm_setup(d$C, d$wt)
  x <- stats::rnorm(d$n)
  q <- stats::quantile(x, c(.25, .75))
  g1 <- x <= q[1]
  g2 <- x >= q[2]
  m1 <- colSums(as.matrix(d$C[g1, ]) %*% S$E) / sum(d$C[g1, ])
  m2 <- colSums(as.matrix(d$C[g2, ]) %*% S$E) / sum(d$C[g2, ])
  direct <- as.vector(sweep(S$E, 2, (m1 + m2) / 2) %*% (m2 - m1))
  expect_equal(text:::perm_dots(S, x), direct, tolerance = 1e-10)
})

test_that("the min_freq floor excludes words from the group means", {
  skip_on_cran()
  d <- make_toy()
  S <- text:::perm_setup(d$C, d$wt)
  x <- stats::rnorm(d$n)
  gr <- text:::perm_groups(x, "quartile")
  m_floor <- text:::perm_group_mean(S, gr$g1, floor_n = 3)
  cnt <- Matrix::colSums(S$C[gr$g1, , drop = FALSE])
  cnt[cnt < 3] <- 0
  direct <- as.vector(Matrix::crossprod(S$E, cnt) / sum(cnt))
  expect_equal(m_floor, direct, tolerance = 1e-10)
})

test_that("participant permutation p-values are roughly uniform under the null", {
  skip_on_cran()
  d <- make_toy(n = 200)
  S <- text:::perm_setup(d$C, d$wt)
  p <- unlist(lapply(1:20, function(i) {
    set.seed(i)
    text:::perm_test_participant(S, stats::rnorm(d$n), Npermutations = 199)$p
  }))
  expect_gt(mean(p < .05), .02)
  expect_lt(mean(p < .05), .09)
  expect_true(all(p >= 1 / 200)) # (1 + count)/(B + 1): never 0
})

test_that("LOO shortcut equals explicit ridge refits", {
  skip_on_cran()
  d <- make_toy(W = 80)
  x <- as.numeric(scale(stats::rnorm(d$n)))
  F_obj <- text:::wp_setup(d$C, d$wt, min_freq_words_test = 0)
  m <- as.vector(F_obj$M0 %*% x)
  lam <- suppressMessages(text:::wp_lambda(F_obj, m))
  short <- as.vector(text:::wp_loo(F_obj, m, text:::wp_hat(F_obj, lam)))
  set.seed(1)
  ii <- sample(F_obj$W, 10)
  explicit <- vapply(ii, function(i) {
    X <- F_obj$Xs[-i, , drop = FALSE]
    yv <- m[-i]
    mu <- colMeans(X)
    Xc <- sweep(X, 2, mu)
    beta <- solve(
      crossprod(Xc) + lam * diag(ncol(Xc)),
      crossprod(Xc, yv - mean(yv))
    )
    mean(yv) + sum((F_obj$Xs[i, ] - mu) * beta)
  }, 0)
  expect_lt(max(abs(explicit - short[ii])), 1e-8)
})

test_that("word prediction never returns p = 0 and is calibrated on noise", {
  skip_on_cran()
  d <- make_toy(n = 200, W = 80)
  p <- unlist(lapply(1:20, function(i) {
    set.seed(i)
    suppressMessages(
      text:::wp_run(d$C, d$wt, stats::rnorm(d$n),
        min_freq_words_test = 0, Npermutations = 999
      )
    )$word_data$p_values
  }))
  expect_true(all(p > 0))
  expect_gt(mean(p < .05), .02)
  expect_lt(mean(p < .05), .09)
})

test_that("RoBERTa / SentencePiece markers and case are normalised", {
  skip_on_cran()
  wt <- tibble::tibble(
    words = c("Ġhappy", "Happy", "▁feel", "##ing", "<s>", "[CLS]"),
    n = c(3, 1, 2, 4, 2, 2), Dim1 = c(1, 5, 2, 9, 9, 9), Dim2 = 0
  )
  out <- text:::normalise_word_types(wt)
  expect_setequal(out$words, c("happy", "feel"))
  expect_equal(out$Dim1[out$words == "happy"], (3 * 1 + 1 * 5) / 4)
})

test_that("word counts are tokens, not substrings", {
  skip_on_cran()
  # regex counting finds "he" inside "the"; token counts do not
  expect_equal(stringi::stri_count_regex("the cat sat on the mat", "he"), 2)
  C <- Matrix::Matrix(c(1, 0), 1, 2,
    sparse = TRUE,
    dimnames = list(NULL, c("the", "he"))
  )
  expect_equal(unname(Matrix::colSums(C)[["he"]]), 0)
})
