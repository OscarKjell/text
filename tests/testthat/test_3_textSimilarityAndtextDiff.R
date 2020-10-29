
library(testthat)
library(text)
library(tibble)

context("Semantic Similiarty Functions and textSimilarityTest")

test_that("textSimilarity produces similarity scores", {
  skip_on_cran()

  similarity_scores <- textSimilarity(
    wordembeddings4$harmonytexts,
    wordembeddings4$satisfactiontexts
  )
  expect_that(similarity_scores, is_a("numeric"))
})

test_that("textSimilarity produces similarity scores", {
  skip_on_cran()

  # Example norm
  harmonynorm <- wordembeddings4$harmonywords[1, ]

  similarity_norm_scores <- textSimilarityNorm(
    wordembeddings4$harmonytext[1:2, ],
    harmonynorm
  )

  expect_that(similarity_norm_scores, is_a("numeric"))
})

test_that("textSimilarityTest paired results in list with numeric output", {
  skip_on_cran()

  test_diff_results <- textSimilarityTest(wordembeddings4$harmonytexts, wordembeddings4$satisfactiontexts,
    method = "paired", Npermutations = 10, N_cluster_nodes = 1
  )

  expect_that(test_diff_results, is_a("list"))
  expect_is(test_diff_results[[1]][[1]], "numeric")
})

test_that("textSimilarityTest paired results in list with numeric output", {
  skip_on_cran()

  expect_error(textSimilarityTest(wordembeddings4$harmonytexts, wordembeddings4$satisfactiontexts[1:39, ],
    method = "paired", Npermutations = 10, N_cluster_nodes = 1
  ))
})

test_that("textSimilarityTest unpaired results in list with numeric output", {
  skip_on_cran()

  test_diff_results <- textSimilarityTest(wordembeddings4$harmonytexts, wordembeddings4$satisfactiontexts,
    method = "unpaired", Npermutations = 10, N_cluster_nodes = 1
  )

  expect_that(test_diff_results, is_a("list"))
  expect_is(test_diff_results[[1]][[1]], "numeric")
})
