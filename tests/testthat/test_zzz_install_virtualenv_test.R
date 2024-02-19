library(testthat)
library(text)

test_that("UNinstalling text", {
  skip_on_cran()

  textrpp_install_virtualenv()

  x_ok <- 1
  expect_equal(x_ok, 1)


})
