

# devtools::document()
# https://cran.r-project.org/web/packages/reticulate/vignettes/package.html

# python 'PyTorch' and 'transformer' module I want to use in my package
PyTorch <- NULL
transformer <- NULL

.onLoad <- function(libname, pkgname) {
  # delay load foo module (will only be loaded when accessed via $)
  PyTorch <<- reticulate::import("torch", delay_load = TRUE)
  transformer <<- reticulate::import("transformers", delay_load = TRUE)
  #reticulate::source_python("/Users/oscarkjell/Desktop/1 Projects/0 Research/0 text r-package/text/R/huggingface_interface.py")
  #reticulate::source_python("~text/R/huggingface_interface.py")
}

