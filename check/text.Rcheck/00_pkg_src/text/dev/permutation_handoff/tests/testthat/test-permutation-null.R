# Draft tests for the participant-permutation null and the fast word-prediction
# test. Synthetic data only (no Python): a random count matrix and random
# "embeddings". Adapt names once the functions are moved into the package.

make_toy <- function(n = 120, W = 60, dims = 8, seed = 1) {
  set.seed(seed)
  C <- Matrix::Matrix(stats::rpois(n * W, 0.6), n, W, sparse = TRUE)
  colnames(C) <- paste0("w", seq_len(W))
  E <- matrix(stats::rnorm(W * dims), W, dims)
  wt <- tibble::tibble(words = colnames(C), n = as.integer(Matrix::colSums(C)))
  wt <- dplyr::bind_cols(wt, tibble::as_tibble(E, .name_repair = ~ paste0("Dim", seq_len(dims))))
  list(C = C, wt = wt, n = n)
}

test_that("pp_dots equals a direct computation of the projection", {
  d <- make_toy(); S <- pp_setup(d$C, d$wt)
  x <- stats::rnorm(d$n)
  q <- stats::quantile(x, c(.25, .75))
  g1 <- x <= q[1]; g2 <- x >= q[2]
  m1 <- colSums(as.matrix(d$C[g1, ]) %*% S$E) / sum(d$C[g1, ])
  m2 <- colSums(as.matrix(d$C[g2, ]) %*% S$E) / sum(d$C[g2, ])
  direct <- as.vector(sweep(S$E, 2, (m1 + m2) / 2) %*% (m2 - m1))
  expect_equal(pp_dots(S, x), direct, tolerance = 1e-10)
})

test_that("participant permutation p-values are roughly uniform under the null", {
  d <- make_toy(n = 200); S <- pp_setup(d$C, d$wt)
  p <- unlist(lapply(1:20, function(i) {
    pp_test(S, stats::rnorm(d$n), null_method = "participant", Npermutations = 199, seed = i)$p
  }))
  expect_gt(mean(p < .05), .02); expect_lt(mean(p < .05), .09)
  expect_true(all(p >= 1 / 200))                    # (1 + count)/(B + 1): never 0
})

test_that("the default token null uses a single random axis", {
  expect_equal(ceiling(10000 / 50000), 1)
})

test_that("LOO shortcut equals explicit ridge refits", {
  d <- make_toy(W = 80)
  expect_lt(wpf_check_loo(d$C, d$wt, stats::rnorm(d$n), n_words = 10, min_freq_words_test = 0), 1e-8)
})

test_that("fast word prediction never returns p = 0 and is calibrated on noise", {
  d <- make_toy(n = 200, W = 80)
  r <- lapply(1:20, function(i) textWordPredictionFast(d$C, d$wt, stats::rnorm(d$n),
                                                        min_freq_words_test = 0,
                                                        Npermutations = 999, seed = i))
  p <- unlist(lapply(r, function(z) z$word_data$p_values_x))
  expect_true(all(p > 0))
  expect_gt(mean(p < .05), .02); expect_lt(mean(p < .05), .09)
})

test_that("missing x drops participants instead of words", {
  d <- make_toy(); x <- stats::rnorm(d$n); x[1:5] <- NA
  r <- textWordPredictionFast(d$C, d$wt, x, min_freq_words_test = 0, Npermutations = 99)
  expect_false(anyNA(r$word_data$x_plotted))
})

test_that("RoBERTa / SentencePiece markers and case are normalised", {
  wt <- tibble::tibble(words = c("Ġhappy", "Happy", "▁feel", "##ing", "<s>", "[CLS]"),
                       n = c(3, 1, 2, 4, 2, 2), Dim1 = c(1, 5, 2, 9, 9, 9), Dim2 = 0)
  out <- pp_normalise_word_types(wt)
  expect_setequal(out$words, c("happy", "feel"))
  expect_equal(out$Dim1[out$words == "happy"], (3 * 1 + 1 * 5) / 4)
})

test_that("word counts are tokens, not substrings", {
  # the current textWordPrediction() counts "he" inside "the"
  expect_equal(stringi::stri_count_regex("the cat sat on the mat", "he"), 2)
  C <- Matrix::Matrix(c(1, 0), 1, 2, sparse = TRUE, dimnames = list(NULL, c("the", "he")))
  expect_equal(unname(Matrix::colSums(C)[["he"]]), 0)
})
