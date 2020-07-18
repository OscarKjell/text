#Test your package with Ctrl/Cmd + Shift + T or devtools::test().
#usethis::use_testthat()

library(testthat)
library(text)
library(tibble)
#library(psych)
#test_check("text")

context("Semantic Similiarty Functions and textDiff")

test_that("textSimilarity produces similarity scores", {

  similarity_scores <- textSimilarity(wordembeddings4_10$harmonytexts,
                                      wordembeddings4_10$satisfactiontexts)
  expect_that(similarity_scores, is_a("numeric"))

})

test_that("textSimilarity produces similarity scores", {
  #Example norm
  harmonynorm <- wordembeddings4_10$harmonywords[1,]

  similarity_norm_scores <- textSimilarityNorm(
    wordembeddings4_10$harmonytext[1:2,],
     harmonynorm
   )

  expect_that(similarity_norm_scores, is_a("numeric"))

})


test_that("textDiff paired results in list with numeric output", {

  test_diff_results <- textDiff(wordembeddings4_10$harmonytexts, wordembeddings4_10$satisfactiontexts,
                                method = "paired", Npermutations = 10, N_cluster_nodes = 1)

  expect_that(test_diff_results, is_a("list"))
  expect_is(test_diff_results[[1]][[1]], 'numeric')
})


test_that("textDiff unpaired results in list with numeric output", {

  test_diff_results <- textDiff(wordembeddings4_10$harmonytexts, wordembeddings4_10$satisfactiontexts,
                                method = "unpaired", Npermutations = 10, N_cluster_nodes = 1)

  expect_that(test_diff_results, is_a("list"))
  expect_is(test_diff_results[[1]][[1]], 'numeric')
})






