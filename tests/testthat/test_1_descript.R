

library(testthat)
library(tibble)
library(text)

context("Big analyses flow")


test_that("Testing textEmbed as well as train", {
  skip_on_cran()
  words1 <- Language_based_assessment_data_8[1:10,1]
  dput(words1)


  descr2 <- textDescriptives(words1)
  expect_equal(descr2[[2]][[1]], 658)
  expect_equal(descr2[[3]][[1]], 65.8)
  expect_equal(descr2$entropy[[1]], 7.056633, tolerance = 0.00001)


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
})
