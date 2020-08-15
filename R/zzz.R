

.onAttach <- function(libname, pkgname) {
  if (!grepl(x = R.Version()$arch, pattern = "64")) {
    warning("The text package requires running R on a 64-bit systems as it is dependent on torch from ptyhon; and you're system is not 64-bit.")
  }
  packageStartupMessage(
    crayon::green("This is text (version 0.8.50). \n"),
    crayon::blue("Newer versions may have updated default settings to reflect current understandings of the state-of-the-art.")
  )
}

# global reference to transformer, torch and huggingface interface
# (will be initialized in .onLoad)

#transformers <- NULL
#torch <- NULL
#huggingface <- NULL
#
#.onLoad <- function(libname, pkgname) {
#  # use superassignment to update global reference to scipy
#  transformers <<- reticulate::import("transformers", delay_load = TRUE)
#  torch <<- reticulate::import("torch", delay_load = TRUE)
#  huggingface <<- reticulate::import("huggingface_Interface3", delay_load = TRUE)
#}


