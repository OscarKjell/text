

.onAttach <- function(libname, pkgname) {
  if (!grepl(x = R.Version()$arch, pattern = "64")) {
    warning("The text package requires running R on a 64-bit systems as it is dependent on torch from ptyhon; and you're system is not 64-bit.")
  }

  packageStartupMessage(colourise(
    "This is text (version 0.8.91). \n",  fg = "green", bg = NULL), colourise("Newer versions may have updated default settings to reflect current understandings of the state-of-the-art."
    ,  fg = "blue", bg = NULL)
  )
}

# global reference to transformer, torch and huggingface interface
# (will be initialized in .onLoad)

#transformers <- NULL
#torch <- NULL
#huggingface <- NULL

#.onLoad <- function(libname, pkgname) {
#  # use superassignment to update global reference to scipy
#  transformers <<- reticulate::import("transformers", delay_load = TRUE)
#  torch <<- reticulate::import("torch", delay_load = TRUE)
#  huggingface <<- reticulate::import("huggingface_Interface3", delay_load = TRUE)
#}


# Below function is from testthat: https://github.com/r-lib/testthat/blob/717b02164def5c1f027d3a20b889dae35428b6d7/R/colour-text.r
#' Colourise text for display in the terminal.
#'
#' If R is not currently running in a system that supports terminal colours
#' the text will be returned unchanged.
#'
#' Allowed colours are: black, blue, brown, cyan, dark gray, green, light
#' blue, light cyan, light gray, light green, light purple, light red,
#' purple, red, white, yellow
#'
#' @param text character vector
#' @param fg foreground colour, defaults to white
#' @param bg background colour, defaults to transparent
# @examples
# print(colourise("Red", "red"))
# cat(colourise("Red", "red"), "\n")
# cat(colourise("White on red", "white", "red"), "\n")
#' @noRd
colourise <- function(text, fg = "black", bg = NULL) {
  term <- Sys.getenv()["TERM"]
  colour_terms <- c("xterm-color","xterm-256color", "screen", "screen-256color")

  if(rcmd_running() || !any(term %in% colour_terms, na.rm = TRUE)) {
    return(text)
  }

  col_escape <- function(col) {
    paste0("\033[", col, "m")
  }

  col <- .fg_colours[tolower(fg)]
  if (!is.null(bg)) {
    col <- paste0(col, .bg_colours[tolower(bg)], sep = ";")
  }

  init <- col_escape(col)
  reset <- col_escape("0")
  paste0(init, text, reset)
}

.fg_colours <- c(
  "black" = "0;30",
  "blue" = "0;34",
  "green" = "0;32"
#  "cyan" = "0;36",
#  "red" = "0;31",
#  "purple" = "0;35",
#  "brown" = "0;33",
#  "light gray" = "0;37",
#  "dark gray" = "1;30",
#  "light blue" = "1;34",
#  "light green" = "1;32",
#  "light cyan" = "1;36",
#  "light red" = "1;31",
#  "light purple" = "1;35",
#  "yellow" = "1;33",
#  "white" = "1;37"
)

.bg_colours <- c(
#  "black" = "40",
#  "red" = "41",
#  "green" = "42",
#  "brown" = "43",
#  "blue" = "44",
#  "purple" = "45",
  "cyan" = "46",
  "light gray" = "47"
)

rcmd_running <- function() {
  nchar(Sys.getenv('R_TESTS')) != 0
}



