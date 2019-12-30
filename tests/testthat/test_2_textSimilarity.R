#Test your package with Ctrl/Cmd + Shift + T or devtools::test().
#usethis::use_testthat()

library(testthat)
library(text)
library(tibble)
library(psych)
#test_check("text")

context("Semantic Similiarty Functions")

wordembeddings_text <- readRDS("/Users/oscar/Desktop/0 Studies/5 R statistical semantics package/text/tests/testthat/wordembeddings_for_testthat.RData")
wordembeddings_text


test_that("textSimilarity produces similarity scores", {

  similarity_scores <- textSimilarity(wordembeddings_text$harmonytexts,
                                      wordembeddings_text$satisfactiontexts)
  expect_that(similarity_scores, is_a("numeric"))

})


test_that("textTtest result in output", {
  library(psych)
  ttest_results <- textTtest(wordembeddings_text$harmonytexts,
                             wordembeddings_text$satisfactiontexts)

  expect_that(ttest_results, is_a("list"))

})




