library(testthat)
library(text)

test_that("installing text", {
  skip_on_cran()

  .rs.restartR()
  textrpp_install(prompt = FALSE,
                  update_conda = TRUE,
                  force_conda = TRUE)

  textrpp_initialize(textEmbed_test = FALSE)

  text_test <- textEmbed("hello")

  expect_equal(text_test$tokens$texts[[1]]$Dim1[[1]], -0.9554495, tolerance = 0.0001)
})
