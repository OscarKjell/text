library(testthat)
library(text)

test_that("installing text", {

  #help(textrpp_install)
  textrpp_install(prompt = FALSE)

  #help(textrpp_initialize)
  #.rs.restartR()
  textrpp_initialize(textEmbed_test=FALSE)

  text_test <- textEmbed("hello")

  expect_that(ncol(text_test[[1]]), equals(1536))
})
