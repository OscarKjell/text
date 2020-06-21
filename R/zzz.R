

# devtools::document()
# https://cran.r-project.org/web/packages/reticulate/vignettes/package.html

# python 'PyTorch' and 'transformer' module I want to use in my package
PyTorch <- NULL
transformer <- NULL

.onLoad <- function(libname, pkgname) {
  # https://rstudio.github.io/reticulate/articles/python_dependencies.html
  # In some cases, a user may try to load your package after Python has already been initialized.
  # To ensure that reticulate can still configure the active Python environment, you can include the code:
  reticulate::configure_environment(pkgname)


  # delay load foo module (will only be loaded when accessed via $)
  PyTorch <<- reticulate::import("torch", delay_load = TRUE)
  transformer <<- reticulate::import("transformers", delay_load = TRUE)
}

