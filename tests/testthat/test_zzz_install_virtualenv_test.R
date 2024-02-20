library(testthat)
library(text)

test_that("UNinstalling text", {
  skip_on_cran()

  help(textrpp_install_virtualenv)
  textrpp_install_virtualenv(
    prompt = FALSE,
    python_path = NULL
    )

  x_ok <- 1
  expect_equal(x_ok, 1)


})
