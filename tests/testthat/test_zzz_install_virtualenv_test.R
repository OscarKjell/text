library(testthat)
library(text)

test_that("Installing a virtual environment", {
  skip_on_cran()

  # help(textrpp_install_virtualenv)
  textrpp_install_virtualenv(
    prompt = FALSE,
    python_path = "/Users/runner/hostedtoolcache/Python/3.9.18/x64"
    )

  x_ok <- 1
  expect_equal(x_ok, 1)


})
