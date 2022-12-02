

library(testthat)
library(tibble)
library(text)

context("Big analyses flow")


test_that("Testing textDescriptives", {
  skip_on_cran()

  #text::textEmbed("hello")

  # Does not work the same
  word2 <- "eat and drink.No money"
  w_total <- sum(stringi::stri_count_words(word2))
  expect_equal(w_total, 4)

#  desc2 <- text::textDescriptives(word2)
#  expect_equal(desc2[[2]][[1]], 4)




#
#  word7 <- Language_based_assessment_data_8[[7,1]]
#  desc7 <- text::textDescriptives(word7)
#  expect_equal(desc7[[2]][[1]], 127)
#
#  # Works the same
#  word1 <- "the downs and go up!I have no money,"
#  desc1 <- text::textDescriptives(word1)
#  expect_equal(desc1[[2]][[1]], 9)
#
#
#
#
#  word3 <- "Yes, overall I am satisfied with my life.  Many Ups and downs, mostly downs, but that is how you get over the the downs and go up!I have no money, but I am ok with it.  I have enough to eat and drink.No money to pay all the bills, but oh well.  Had major accident and have been housebound since July, and just starting to improve enough to sit at computer.  I am satisfied, because it could have been much worse, since I had renal failure, I could of ended up on dialysis, which I avoided.  I could have lost my leg, but instead, still working on walking.  But I can walk, slowly, but better than not. I am satisfied with what I have, no complaints."
#  desc3 <- text::textDescriptives(word3)
#  expect_equal(desc3[[2]][[1]], 127)
#  #####
#
#  words1 <- Language_based_assessment_data_8[1:10,1]
#  dput(words1)
#
#  text::textEmbed("hello")
#
#
#  descr2 <- text::textDescriptives(words1)
#  expect_equal(descr2[[2]][[1]], 658)
#  expect_equal(descr2[[3]][[1]], 65.8)
#  expect_equal(descr2$entropy[[1]], 7.056633, tolerance = 0.00001)
#
#
#  get_encoding_change <- function(x) {
#    Encoding(x) <- Encoding(enc2utf8(x))
#    x
#  }
#  wordd2_get <- get_encoding_change(word2)
#  print("wordd2_get")
#  dput(wordd2_get)


#  select_character_v_utf8 <- function(x) {
#    # If a vector is submitted, make it a tibble column.
#    if (is.vector(x) == TRUE & is.list(x) == FALSE) {
#      # Select variable name to have as column name in the end
#      colname_x <- deparse(substitute(x))
#      # Remove everything before a "$"
#      colname_x <- gsub("^.*\\$", "", colname_x)
#
#      x <- tibble::as_tibble_col(x)
#      colnames(x) <- substitute(colname_x)
#    }
#    # Select all character variables
#    x_characters <- dplyr::select_if(x, is.character)
#    # This makes sure that all variables are UTF-8 coded
#    x_characters <- tibble::as_tibble(purrr::map(x_characters, get_encoding_change))
#  }
#
  #words_test <- select_character_v_utf8(word2)
  #print("words_test")
  #dput(words_test)




})
