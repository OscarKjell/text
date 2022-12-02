

library(testthat)
library(tibble)
library(text)

context("Big analyses flow")


test_that("Testing textDescriptives", {
  skip_on_cran()

  text::textEmbed("hello")

  word1 <- Language_based_assessment_data_8[1,1]
  desc1 <- text::textDescriptives(word1)
  expect_equal(desc1[[2]][[1]], 61)

  word2 <- Language_based_assessment_data_8[2,1]
  desc2 <- text::textDescriptives(word2)
  expect_equal(desc2[[2]][[1]], 71)

  word3 <- Language_based_assessment_data_8[3,1]
  desc3 <- text::textDescriptives(word3)
  expect_equal(desc3[[2]][[1]], 27)

  word4 <- Language_based_assessment_data_8[4,1]
  desc4 <- text::textDescriptives(word4)
  expect_equal(desc4[[2]][[1]], 46)

  word5 <- Language_based_assessment_data_8[5,1]
  desc5 <- text::textDescriptives(word5)
  expect_equal(desc5[[2]][[1]], 88)

  word6 <- Language_based_assessment_data_8[6,1]
  desc6 <- text::textDescriptives(word6)
  expect_equal(desc6[[2]][[1]], 50)

  word7 <- Language_based_assessment_data_8[7,1]
  desc7 <- text::textDescriptives(word7)
  expect_equal(desc7[[2]][[1]], 127)

  word8 <- Language_based_assessment_data_8[8,1]
  desc8 <- text::textDescriptives(word8)
  expect_equal(desc8[[2]][[1]], 62)

  word9 <- Language_based_assessment_data_8[9,1]
  desc9 <- text::textDescriptives(word9)
  expect_equal(desc9[[2]][[1]], 47)

  word10 <- Language_based_assessment_data_8[10,1]
  desc10 <- text::textDescriptives(word10)
  expect_equal(desc10[[2]][[1]], 79)
  #####

  words1 <- Language_based_assessment_data_8[1:10,1]
  dput(words1)

  text::textEmbed("hello")


  descr2 <- text::textDescriptives(words1)
  expect_equal(descr2[[2]][[1]], 658)
  expect_equal(descr2[[3]][[1]], 65.8)
  expect_equal(descr2$entropy[[1]], 7.056633, tolerance = 0.00001)


  get_encoding_change <- function(x) {
    Encoding(x) <- Encoding(enc2utf8(x))
    x
  }

  select_character_v_utf8 <- function(x) {
    # If a vector is submitted, make it a tibble column.
    if (is.vector(x) == TRUE & is.list(x) == FALSE) {
      # Select variable name to have as column name in the end
      colname_x <- deparse(substitute(x))
      # Remove everything before a "$"
      colname_x <- gsub("^.*\\$", "", colname_x)

      x <- tibble::as_tibble_col(x)
      colnames(x) <- substitute(colname_x)
    }
    # Select all character variables
    x_characters <- dplyr::select_if(x, is.character)
    # This makes sure that all variables are UTF-8 coded
    x_characters <- tibble::as_tibble(purrr::map(x_characters, get_encoding_change))
  }

  words_test <- select_character_v_utf8(words1)
  dput(words_test)

  w_total <- sum(stringi::stri_count_words(words_test$satisfactiontexts))


})
