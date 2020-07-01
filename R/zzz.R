
#install.packages(c("dplyr", "tokenizers", "psych", "tibble", "stringr", "tidyr", "ggplot2", "ggrepel", "cowplot", "scales", "rlang", "purrr", "stringi", "data.table", "magrittr", "parsnip", "recipes", "rsample", "reticulate", "tune", "workflows", "yardstick", "lsa", "quanteda", "broom"))
# devtools::load_all
# devtools::document()
# https://cran.r-project.org/web/packages/reticulate/vignettes/package.html

.onAttach <- function(libname, pkgname){
  if(!grepl(x = R.Version()$arch, pattern = "64")){
    warning("The text package requires running R on a 64-bit systems as it is dependent on torch from ptyhon; and you are not doing this.")
  }
  packageStartupMessage(cat(paste0("\033[0;", 32, "m", "This is text (version 0.6.0.9000).","\033[0m","\n"),
                            paste0("\033[0;", 34, "m", "Newer versions may have updated default settings to reflect current understqnding of the state-of-the-art.","\033[0m")))
}



#' @export
hgTransformerGetEmbedding <- NULL

.onLoad <- function(libname, pkgname) {
   the_module <- reticulate::import_from_path(module = "huggingface_Interface3", path = system.file("python", package = "text"))

   hgTransformerGetEmbedding <<- the_module$hgTransformerGetEmbedding
}





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

