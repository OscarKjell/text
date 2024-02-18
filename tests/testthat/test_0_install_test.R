library(testthat)
library(text)

test_that("installing text", {
  skip_on_cran()

  textrpp_install(prompt = FALSE)

  textrpp_initialize(textEmbed_test = TRUE,
                     save_profile = TRUE)

  text_test <- textEmbed("hello")

  expect_equal(text_test$tokens$texts[[1]]$Dim1[[1]], -0.9554495, tolerance = 0.0001)

})
