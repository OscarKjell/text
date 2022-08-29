library(testthat)
library(text)

test_that("installing text", {
  skip_on_cran()

  textrpp_install(prompt = FALSE)

  textrpp_initialize(textEmbed_test = FALSE)

  text_test <- textEmbed("hello")

  expect_that(text_test$tokens$texts[[1]]$Dim1[[1]], equals(-0.95544952))
})
