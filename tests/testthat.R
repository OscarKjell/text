#.rs.restartR()
library(testthat)
library(text)
library(reticulate)

#sessionInfo()
test_check("text")

#py_ver <- tryCatch({
#  cfg <- reticulate::py_config()
#  base::numeric_version(cfg$version)
#}, error = function(e) {
#  base::numeric_version("1")
#})
#
#if (py_ver >= "3" && reticulate::py_module_available("torch") && reticulate::py_module_available("transformers")) {
#  local_torch <- reticulate::import("torch")
#  local_transformers <- reticulate::import("transformers")
#
#  if (base::numeric_version(local_transformers$`__version__`) >= "3") {
#    # All okay:
#    library(text)
#
#    test_check("text")
#  }
#}










# The submission to CRAN has been rejected several times because the _total_
# time to check the package is too long (>10 min). Most of this is attributable
# to the install time for dependencies. However, CRAN has been very inconsistent
# about this with one submission stating that the rule is that the check time
# should be less than 10 min while in another that the total check time,
# _aggregated over all architectures_, should be under 10 min (e.g. adding the
# windows check times for 32 and 64 bit to the linux test times and so on).
# Since they have been undependable on this, we'll avoid running all tests on
# CRAN.

#if (identical(Sys.getenv("NOT_CRAN"), "true")) { # emulates `testthat:::on_cran()`
#  if (requireNamespace("xml2")) {
#    test_check("text", reporter = MultiReporter$new(reporters = list(JunitReporter$new(file = "test-results.xml"), CheckReporter$new())))
#  } else {
#    test_check("text")
#  }
#}



