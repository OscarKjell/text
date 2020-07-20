

.onAttach <- function(libname, pkgname) {
  if (!grepl(x = R.Version()$arch, pattern = "64")) {
    warning("The text package requires running R on a 64-bit systems as it is dependent on torch from ptyhon; and you're system is not 64-bit.")
  }
  packageStartupMessage(crayon::green("This is text (version 0.7.15.9000). \n"),
                        crayon::blue("Newer versions may have updated default settings to reflect current understandings of the state-of-the-art."))
}

