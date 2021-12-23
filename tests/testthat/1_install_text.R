
library(text)
c
ontext("Install and initilize text")

test_that("installing text", {

  #help(textrpp_install)
  textrpp_install(prompt = FALSE)

  #help(textrpp_initialize)
  #.rs.restartR()
  textrpp_initialize(textEmbed_test=TRUE)
})
