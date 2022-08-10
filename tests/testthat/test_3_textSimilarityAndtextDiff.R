
library(testthat)
library(text)
library(tibble)

context("Semantic Similiarty Functions and textSimilarityTest")

test_that("textSimilarity produces similarity scores", {
  skip_on_cran()

  similarity_scores1 <- textSimilarity(
    word_embeddings_4$harmonytexts,
    word_embeddings_4$satisfactiontexts
  )
  expect_that(similarity_scores1, is_a("numeric"))
  expect_equal(similarity_scores1[1], 0.8438458, tolerance = 0.0001)
})

test_that("textSimilarity produces similarity scores", {
  skip_on_cran()

  # Example norm
  harmonynorm <- word_embeddings_4$harmonywords[1, ]

  similarity_norm_scores <- textSimilarityNorm(
    word_embeddings_4$harmonytext[1:2, ],
    harmonynorm
  )

  expect_that(similarity_norm_scores, is_a("numeric"))
  expect_equal(similarity_norm_scores[1], 0.4912521, tolerance = 0.001)
})

test_that("textSimilarityMatrix produces euclidean similarity scores", {
  skip_on_cran()

  similarity_scores2 <- textSimilarityMatrix(
    word_embeddings_4$harmonytexts[1:3, ],
    method = "euclidean"
  )
  expect_equal(similarity_scores2[1, 2], 0.29414779, tolerance = 0.0001)
})


test_that("textDistance produces distance scores", {
  skip_on_cran()

  distance_scores <- textDistance(
    word_embeddings_4$harmonytexts,
    word_embeddings_4$satisfactiontexts
  )
  expect_that(distance_scores, is_a("numeric"))
  expect_equal(distance_scores[1], 0.6904026, tolerance = 0.0001)
})

test_that("textSimilarityTest paired results in list with numeric output", {
  skip_on_cran()

  test_diff_results1 <- textSimilarityTest(word_embeddings_4$harmonytexts, word_embeddings_4$satisfactiontexts,
    method = "paired", Npermutations = 100, N_cluster_nodes = 1
  )

  expect_that(test_diff_results1, is_a("list"))
  expect_is(test_diff_results1[[1]][[1]], "numeric")
  expect_equal(test_diff_results1[[1]][[1]], 0.7825081, tolerance = 0.001)
})

test_that("textSimilarityTest paired results in list with numeric output", {
  skip_on_cran()

  expect_error(textSimilarityTest(word_embeddings_4$harmonytexts, word_embeddings_4$satisfactiontexts[1:39, ],
    method = "paired", Npermutations = 100, N_cluster_nodes = 1
  ))
})

test_that("textSimilarityTest unpaired results in list with numeric output", {
  skip_on_cran()

  test_diff_results2 <- textSimilarityTest(word_embeddings_4$harmonytexts, word_embeddings_4$satisfactiontexts,
    method = "unpaired", Npermutations = 100, N_cluster_nodes = 1
  )

  expect_that(test_diff_results2, is_a("list"))
  expect_equal(test_diff_results2[[1]][[1]], 0.9950558, tolerance = 0.00001)
})


test_that("textSimilarityTest unpaired results in list with numeric output", {
  skip_on_cran()

  test_diff_results1 <- textSimilarityTest(word_embeddings_4$harmonytexts, word_embeddings_4$satisfactiontexts,
    method = "unpaired", Npermutations = 100, N_cluster_nodes = 1,
    output.permutations = FALSE
  )

  expect_that(test_diff_results1, is_a("list"))
  expect_is(test_diff_results1$p.value[[1]], "numeric")
  expect_equal(test_diff_results1$p.value[[1]], 0.00990099, tolerance = 0.00001)
})

test_that("textSimilarityTest unpaired euclidean results in list with numeric output", {
  skip_on_cran()

  test_diff_results2 <- textSimilarityTest(word_embeddings_4$harmonytexts, word_embeddings_4$satisfactiontexts,
    method = "unpaired", Npermutations = 100, N_cluster_nodes = 1,
    similarity_method = "euclidean"
  )

  expect_that(test_diff_results2, is_a("list"))
  expect_is(test_diff_results2$p.value[[1]], "numeric")
  expect_equal(test_diff_results2$p.value[[1]], 0.04)
})
