library(testthat)
library(text)

test_that("UNinstalling text", {
  skip_on_cran()

  if (Sys.info()["sysname"] == "Darwin" | Sys.info()["sysname"] == "Windows") {
  text::textrpp_uninstall(
    prompt = FALSE,
    envname = "test_ok")

  }

  x_ok <- 1
  expect_equal(x_ok, 1)


})
