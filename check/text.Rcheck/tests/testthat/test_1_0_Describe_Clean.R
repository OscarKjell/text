

library(testthat)
library(tibble)
library(text)

context("Big analyses flow")

test_that("Testing textEmbed as well as train", {
  skip_on_cran()

  textrpp_initialize()

  descr1 <- textDescriptives(Language_based_assessment_data_8[[1]][[1]],
                             entropy_unit = "log10"
  )
  testthat::expect_that(descr1[[1]], testthat::is_a("character"))
  testthat::expect_equal(descr1[[2]], 61)
  testthat::expect_equal(descr1[[10]], 1.503212, tolerance = 0.00001)

  descr2 <- textDescriptives(Language_based_assessment_data_8[1:2])
  expect_equal(descr2[[2]][[1]], 2482)
  expect_equal(descr2[[3]][[1]], 62.05)


  text = paste0("John, please get that article on www.linkedin.com to me by 5:00PM on ",
                "Jan 9th 2012. 4:00 would be ideal, actually. If you have any ",
                "questions, You can reach me at (519)-236-2723x341 or get in touch with ",
                "my associate at harold.smith@gmail.com or on Twitter at @harold_smith__ !")

  text_cleaned <- textClean(text)

  # List of placeholders to check
  placeholders <- c("<URL>", "<TIME>", "<DATE_STRING>",
                    "<PHONE_NUMBER>", "<EMAIL_ADDRESS>",
                    #"<adfasfasf>",
                    "<@_SYMBOL>")

  # Test that all placeholders are in the text
  for (placeholder in placeholders) {
    testthat::expect_equal(
      grepl(placeholder, text_cleaned),
      TRUE,
      info = paste("Placeholder", placeholder, "is missing.")
    )
  }

})


