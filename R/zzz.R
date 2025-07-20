text_check_permissions <- function() {
  if (tolower(Sys.getenv("TEXT_SKIP_PERM_CHECK", unset = "FALSE")) == "true") {
    return(invisible(NULL))
  }

  sysname <- Sys.info()[["sysname"]]
  os_type <- .Platform$OS.type

  # A) Check R package install permission
  lib_path <- .libPaths()[1]
  can_write_lib <- file.access(lib_path, 2) == 0
  if (!can_write_lib) {
    message("[text]: You do not have permission to install R packages to your default library:\n",
            "        ", lib_path, "\n",
            "        Try running R with elevated permissions or contact your system administrator.")
  }

  # B) Check system dependency installation
  if (os_type == "unix") {
    # macOS-specific
    if (sysname == "Darwin") {
      brew_exists <- Sys.which("brew") != ""
      has_sudo <- system("sudo -n true", ignore.stdout = TRUE, ignore.stderr = TRUE) == 0
      if (!brew_exists && !has_sudo) {
        message("[text]: You may not have permission to install system libraries (e.g., libomp) on macOS.\n",
                "        Consider asking your system administrator to install Homebrew and libomp.")
      }
    }

    # Linux-specific
    if (sysname == "Linux") {
      has_sudo <- system("sudo -n true", ignore.stdout = TRUE, ignore.stderr = TRUE) == 0
      if (!has_sudo) {
        message("[text]: You may not have permission to install system-level libraries (e.g., libomp, build tools).\n",
                "        Consider contacting your system administrator.")
      }
    }
  }

  # C) Windows-specific
  if (os_type == "windows") {
    conda_exists <- Sys.which("conda") != ""
    pip_exists <- Sys.which("pip") != ""
    running_as_admin <- tryCatch({
      shell("fsutil dirty query %systemdrive%", intern = TRUE)
      TRUE
    }, error = function(e) FALSE)

    if (!conda_exists || !pip_exists) {
      message("[text]: You may be missing Python tools required for language models (conda or pip).\n",
              "         Please install [Miniconda](https://docs.conda.io/en/latest/miniconda.html) and restart R.")
    }

    if (!running_as_admin) {
      message("[text]: R does not appear to be running as Administrator.\n",
              "        This might limit your ability to install system-level software.\n",
              "        You can try right-clicking R or RStudio and choosing 'Run as administrator'.")
    }
  }

  invisible(NULL)
}


#' @importFrom utils packageVersion
#' @noRd
.onAttach <- function(libname, pkgname) {

  # Check installation permissions
  text_check_permissions()

  if (!grepl(x = R.Version()$arch, pattern = "64")) {
    warning("The text package requires running R on a 64-bit systems
            as it is dependent on torch from ptyhon; and you're
            system is not 64-bit.")
  }

  text_version_nr <- tryCatch(
    {
      text_version_nr1 <- paste(" (version ", packageVersion("text"), ")", sep = "")
    },
    error = function(e) {
      text_version_nr1 <- ""
    }
  )
  OMP_msg <- ""
  if (Sys.info()[["sysname"]] == "Darwin") {
    skip_patch <- Sys.getenv("TEXT_SKIP_OMP_PATCH", unset = "FALSE")
    if (tolower(skip_patch) != "true") {
      Sys.setenv(OMP_NUM_THREADS = "1")
      Sys.setenv(OMP_MAX_ACTIVE_LEVELS = "1")
      Sys.setenv(KMP_DUPLICATE_LIB_OK = "TRUE")
      OMP_msg <- c("MacOS detected: Setting OpenMP environment variables to avoid potential crash due to libomp conflicts.")
    } else {
      OMP_msg <- c("Skipped setting OpenMP environment variables (TEXT_SKIP_OMP_PATCH is TRUE)")
    }
  }
#  if (Sys.info()["sysname"] == "Darwin") {
#    OMP_msg <- c("MacOS detected: Setting OpenMP environment variables to avoid potential crash due to libomp conflicts.")
#    Sys.setenv(OMP_NUM_THREADS = "1")
#    Sys.setenv(OMP_MAX_ACTIVE_LEVELS = "1")
#    Sys.setenv(KMP_DUPLICATE_LIB_OK = "TRUE")
#  }

  packageStartupMessage(
    colourise(
      paste(
        "This is text", text_version_nr,
        ".\n",
        sep = ""
      ),
      fg = "blue", bg = NULL
    ),
    colourise("Newer versions may have improved functions and updated defaults to reflect current understandings of the state-of-the-art.\n",
      fg = "green", bg = NULL
    ),
    colourise(OMP_msg,
              fg = "purple", bg = NULL
    ),
    colourise("\nFor more information about the package see www.r-text.org.",
      fg = "purple", bg = NULL
    )
  )

  if (isTRUE(check_textrpp_python_options()$val == "textrpp_condaenv")) {
    textrpp_initialize(check_env = FALSE)
  }
}

# Below function is from testthat:
# https://github.com/r-lib/testthat/blob/717b02164def5c1f027d3a20b889dae35428b6d7/R/colour-text.r
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
#' @noRd
colourise <- function(text, fg = "black", bg = NULL) {
  term <- Sys.getenv()["TERM"]
  colour_terms <- c("xterm-color", "xterm-256color", "screen", "screen-256color")

  if (rcmd_running() || !any(term %in% colour_terms, na.rm = TRUE)) {
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
  "green" = "0;32",
  "cyan" = "0;36",
  "red" = "0;31",
  "purple" = "0;35",
  "brown" = "0;33"
  # "light gray" = "0;37",
  # "dark gray" = "1;30",
  # "light blue" = "1;34",
  # "light green" = "1;32",
  # "light cyan" = "1;36",
  # "light red" = "1;31",
  # "light purple" = "1;35",
  # "yellow" = "1;33",
  # "white" = "1;37"
)

.bg_colours <- c(
  "black" = "40",
  "red" = "41",
  "green" = "42",
  "brown" = "43",
  "blue" = "44",
  "purple" = "45",
  "cyan" = "46",
  "light gray" = "47"
)

rcmd_running <- function() {
  nchar(Sys.getenv("R_TESTS")) != 0
}

