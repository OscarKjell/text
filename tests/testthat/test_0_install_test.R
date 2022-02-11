library(testthat)
library(text)

test_that("installing text", {
  skip_on_cran()

  textrpp_install(prompt = FALSE)

  textrpp_initialize(textEmbed_test=FALSE)

  text_test <- textEmbed("hello")

  expect_that(ncol(text_test[[1]]), equals(1536))
})
