#.rs.restartR()

# text KLOK999

# devtools::document()
# devtools::check()
#devtools::install_github("OscarKjell/text", auth_token = "99a9388f1ef7a1e44b94f3650d9aeb7ea0658bea")
#1#?install_github
#?install_github

# Run once to configure your package to use pkgdown
# usethis::use_pkgdown()
# Run to build the website
# devtools::document()
# pkgdown::build_site()
# build_home(pkg = ".", override = list(), preview = NA, quiet = TRUE)
# build_reference()
# build_reference_index()

#getwd()
#reticulate::import("huggingface_Interface3")
#help(import)
#x <- "how are you"
#hgTransformerGetEmbedding(x, layers=11)
#
#getwd()
#setwd("/Users/oscarkjell/Desktop/1 Projects/0 Research/0 text r-package/text/inst/python/")
#reticulate::import("the_py_module")
#f1()
#
#reticulate::source_python("the_py_module.py")
#f1()



#Anaylse human language data with state-of-the-art Natural Language Processing and Deep Learning.

# devtools::load_all
# devtools::document()
# https://cran.r-project.org/web/packages/reticulate/vignettes/package.html

.onAttach <- function(libname, pkgname){
  if(!grepl(x = R.Version()$arch, pattern = "64")){
    warning("The text package requires running R on a 64-bit systems as it is dependent on torch from ptyhon; and you are not doing this.")
  }
  packageStartupMessage(cat(paste0("\033[0;", 32, "m", "This is text (version 0.6.0.9000).","\033[0m","\n"),
                            paste0("\033[0;", 34, "m", "Newer versions may have updated default settings to reflect current understandings of the state-of-the-art.","\033[0m")))
}



#f1 <- NULL
#the_module <- NULL
#
#.onLoad <- function(libname, pkgname) {
#  the_module <- reticulate::import_from_path(
#    "the_py_module",
#    file.path("inst", "python"))
#  f1 <<- the_module$f1
#}




#
# # global reference to scipy (will be initialized in .onLoad)
# transformers <- NULL
# torch <- NULL
# # PyTorch <- NULL
#
# .onLoad <- function(libname, pkgname) {
#    # use superassignment to update global reference to scipy
#    transformers <<- reticulate::import("transformers", delay_load = TRUE)
#    torch <<- reticulate::import("torch", delay_load = TRUE)
#    # PyTorch <<- reticulate::import("PyTorch", delay_load = TRUE)
# }
#
#






#Work in 3.6!!!
#  # Load the module and create dummy objects from it, all of which are NULL
#  the_py_module <- reticulate::import_from_path(
#    "huggingface_Interface3",
#    file.path("inst", "python")
#  )
#  for (obj in names(the_py_module)) {
#    assign(obj, NULL)
#  }
#  # Clean up
#  rm(the_py_module)
#
#  # Now all those names are in the namespace, and ready to be replaced on load
#  .onLoad <- function(libname, pkgname) {
#    the_py_module <- reticulate::import_from_path(
#      "huggingface_Interface3",
#      system.file("python", package = "text")
#    )
#    # assignInMyNamespace(...) is meant for namespace manipulation
#    for (obj in names(the_py_module)) {
#      assignInMyNamespace(obj, the_py_module[[obj]])
#    }
#  }
# #' @import reticulate
# NULL

# help(source_python)
# help(import_from_path)
# library(text)


# python 'PyTorch' and 'transformer' module I want to use in my package
#PyTorch <- NULL
#transformer <- NULL
#
#.onLoad <- function(libname, pkgname) {
#  # https://rstudio.github.io/reticulate/articles/python_dependencies.html
#  # In some cases, a user may try to load your package after Python has already been initialized.
#  # To ensure that reticulate can still configure the active Python environment, you can include the code:
#  reticulate::configure_environment(pkgname)
#
#
#  # delay load foo module (will only be loaded when accessed via $)
#  PyTorch <<- reticulate::import("torch", delay_load = TRUE)
#  transformer <<- reticulate::import("transformers", delay_load = TRUE)
#}
