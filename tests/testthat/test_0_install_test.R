library(testthat)
library(text)

test_that("installing text", {
  skip_on_cran()

  # On Linux get error at reticulate:::pip_install(...)

  if (Sys.info()["sysname"] == "Darwin" | Sys.info()["sysname"] == "Windows") {


    text::textrpp_install(prompt = FALSE,
                          envname = "test_ok"
                          )

    text::textrpp_initialize(textEmbed_test = TRUE,
                             save_profile = TRUE,
                             prompt = FALSE,
                             condaenv = "test_ok"
                             )

    text_test <- text::textEmbed("hello")

    expect_equal(text_test$tokens$texts[[1]]$Dim1[[1]], -0.9554495, tolerance = 0.0001)


    log1 <- text::textDiagnostics()

    logTRUE <- text::textDiagnostics(
      anonymise = TRUE,
      include_other_envs = TRUE,
      search_omp = TRUE,
      full_session_info = TRUE
    )

    logFALSE <- text::textDiagnostics(
      anonymise = FALSE,
      include_other_envs = FALSE,
      search_omp = FALSE,
      full_session_info = FALSE
    )





  }

#    INSTEAD SEE HOW IT IS BEING UNINSTALLED IN TEXT_ZZ... file
#    if (Sys.info()["sysname"] == "Darwin" | Sys.info()["sysname"] == "Windows") {
#
#    # help(textrpp_uninstall)
#    text::textrpp_install(prompt = FALSE,
#                          envname = "uninstall")
#
#    textrpp_uninstall(prompt = FALSE,
#                      envname = "uninstall")
#  }


#
})
