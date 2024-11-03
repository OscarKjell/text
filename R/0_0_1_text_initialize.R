# modified from spacyr: https://github.com/quanteda/spacyr/tree/master/R

#' Initialize text required python packages
#'
#' Initialize text required python packages to call from R.
#' @return NULL
#' @param python_executable the full path to the Python executable, for which
#'   text required python packages is installed.
#' @param ask logical; if \code{FALSE}, use the first text required python packages installation found;
#'   if \code{TRUE}, list available text required python packages installations and prompt the user for
#'   which to use. If another (e.g. \code{python_executable}) is set, then this
#'   value will always be treated as \code{FALSE}.
#' @param virtualenv set a path to the Python virtual environment with text required python packages
#'   installed Example: \code{virtualenv = "~/myenv"}
#' @param condaenv set a path to the anaconda virtual environment with text required python packages
#'   installed Example: \code{condalenv = "myenv"}
#' @param check_env logical; check whether conda/virtual environment generated
#'   by \code{textrpp_install()} exists
#' @param refresh_settings logical; if \code{TRUE}, text will ignore the saved
#'   settings in the profile and initiate a search of new settings.
#' @param save_profile logical; if \code{TRUE}, the current text required python packages setting will
#'   be saved for the future use.
#' @param textEmbed_test logical; Test whether function (textEmbed) that requires python packages works.
#' @param prompt logical; asking whether user wants to set the environment as default.
#' @export
textrpp_initialize <- function(python_executable = NULL,
                               virtualenv = NULL,
                               condaenv = "textrpp_condaenv",
                               ask = FALSE,
                               refresh_settings = FALSE,
                               save_profile = FALSE,
                               check_env = TRUE,
                               textEmbed_test = FALSE,
                               prompt = TRUE) {
  set_textrpp_python_option(
    python_executable,
    virtualenv,
    condaenv,
    check_env,
    refresh_settings,
    ask
  )

  ## check settings and start reticulate python
  settings <- check_textrpp_python_options()
  if (!is.null(settings)) {
    if (settings$key == "textrpp_python_executable") {
      reticulate::use_python(settings$val, required = TRUE)
    } else if (settings$key == "textrpp_virtualenv") {
      reticulate::use_virtualenv(settings$val, required = TRUE)
    } else if (settings$key == "textrpp_condaenv") {
      reticulate::use_condaenv(settings$val, required = TRUE)
    }
  }

  # Importing this here may start importing necessary packages
  reticulate::source_python(system.file("python",
    "huggingface_Interface3.py",
    package = "text",
    mustWork = TRUE
  ))

  message(colourise(
    "\nSuccessfully initialized text required python packages.\n",
    fg = "green", bg = NULL
  ))
  settings <- check_textrpp_python_options()

  settings_text <- paste('Python options: \n type = "', settings$key,
    '", \n name = "', settings$val, '".',
    sep = ""
  )

  message(colourise(settings_text,
    fg = "blue", bg = NULL
  ))


  options("textrpp_initialized" = TRUE)

  if (save_profile == TRUE) {
    save_textrpp_options(settings$key, settings$val, prompt = prompt)
  }

  if (textEmbed_test == TRUE) {
    textEmbed("hello")
  }
}

# Find text required python packages
#
# Locate the user's version of Python for which text required python packages are installed.
# @return textrpp_python
# @export
# @param ask logical; if \code{FALSE}, use the first text required python packages installation found;
#   if \code{TRUE}, list available text required python packages installations and prompt the user
#   for which to use. If another (e.g. \code{python_executable}) is set, then
#   this value will always be treated as \code{FALSE}.
#
# @keywords internal
#find_textrpp <- function(ask) {
#  textrpp_found <- `:=` <- NA
#  textrpp_python <- NULL
#  options(warn = -1)
#  py_execs <- if (is_windows()) {
#    system2("where", "python", stdout = TRUE)
#  } else if (is_osx() && file.exists("~/.bash_profile")) {
#    c(
#      system2("source", "~/.bash_profile; which -a python", stdout = TRUE),
#      system2("source", "~/.bash_profile; which -a python3", stdout = TRUE)
#    )
#  } else {
#    c(
#      system2("which", "-a python", stdout = TRUE),
#      system2("which", "-a python3", stdout = TRUE)
#    )
#  }
#  py_execs <- unique(py_execs)
#  options(warn = 0)
#
#  if (length(py_execs) == 0 | grepl("not find", py_execs[1])[1]) {
#    return(NA)
#  }
#
#  df_python_check <- tibble::tibble(py_execs, textrpp_found = 0)
#  for (i in seq_len(nrow(df_python_check))) {
#    py_exec <- df_python_check[i, ]
#    sys_message <- check_textrpp_model(py_exec) #
#    if (sys_message == "OK") {
#      df_python_check[i, textrpp_found := 1]
#    }
#  }
#
#  if (df_python_check[, sum(textrpp_found)] == 0) {
#    return(NULL)
#  } else if (df_python_check[, sum(textrpp_found)] == 1) {
#    textrpp_python <- df_python_check[textrpp_found == 1, py_execs]
#    message("textrpp: ", ") is installed in ", textrpp_python)
#  } else if (ask == FALSE) {
#    textrpp_python <- df_python_check[textrpp_found == 1, py_execs][1]
#    message("textrpp: is installed in more than one python")
#    message("text will use ", textrpp_python, " (because ask = FALSE)")
#  } else {
#    textrpp_pythons <- df_python_check[textrpp_found == 1, py_execs]
#    message("textrpp is installed in more than one python")
#    number <- utils::menu(textrpp_pythons, title = "Please select python:")
#    if (number == 0) {
#      stop("Initialization was canceled by user", call. = FALSE)
#    }
#    textrpp_python <- textrpp_pythons[number]
#    message("text will use: ", textrpp_python)
#  }
#  return(textrpp_python)
#}
#

#' Find text required python pacakges env
#'
#' check whether conda/virtual environment for text required python pacakges exists
#' @export
#'
#' @keywords internal
find_textrpp_env <- function() {
  if (is.null(tryCatch(reticulate::conda_binary("auto"), error = function(e) NULL))) {
    return(FALSE)
  }
  found <- if ("textrpp_condaenv" %in% reticulate::conda_list(conda = "auto")$name) {
    TRUE
  } else if (file.exists(file.path("~/.virtualenvs", "textrpp_virtualenv", "bin", "activate"))) {
    TRUE
  } else {
    FALSE
  }
  return(found)
}


check_textrpp_model <- function(py_exec) { ### , model
  options(warn = -1)
  py_exist <- if (is_windows()) {
    if (py_exec %in% system2("where", "python", stdout = TRUE)) {
      py_exec
    } else {
      NULL
    }
  } else {
    system2("which", py_exec, stdout = TRUE)
  }

  if (length(py_exist) == 0) {
    stop(py_exec, " is not a python executable")
  }
  tryCatch({
    sys_message <- "see error in text_initialize row 235"
    # system2(py_exec, c(sprintf("-c \"import texrpp; text.load('%s'); print('OK')\"", model)),
    #        stderr = TRUE, stdout = TRUE)
  })
  options(warn = 0)
  return(paste(sys_message, collapse = " "))
}


set_textrpp_python_option <- function(python_executable = NULL,
                                      virtualenv = NULL,
                                      condaenv = NULL,
                                      check_env = TRUE,
                                      refresh_settings = FALSE,
                                      ask = NULL) {
  if (refresh_settings) clear_textrpp_options()

  if (!is.null(check_textrpp_python_options())) {
    settings <- check_textrpp_python_options()

    message_text1 <- paste("textrpp python option is already set, text will use: ",
      sub("textrpp_", "", settings$key), ' = "', settings$val, '"',
      sep = ""
    )

    message(colourise(message_text1,
      fg = "blue", bg = NULL
    ))
    # a user can specify only one
  } else if (sum(!is.null(c(python_executable, virtualenv, condaenv))) > 1) {
    stop(paste(
      "Too many python environments are specified, please select only one",
      "from python_executable, virtualenv, and condaenv"
    ))
    # give warning when nothing is specified
  } else if (sum(!is.null(c(python_executable, virtualenv, condaenv))) == 1) {
    if (!is.null(python_executable)) {
      if (check_textrpp_model(python_executable) != "OK") {
        stop("Text required python packages ", " are not installed in ", python_executable)
      }
      clear_textrpp_options()
      options(textrpp_python_executable = python_executable)
    } else if (!is.null(virtualenv)) {
      clear_textrpp_options()
      options(textrpp_virtualenv = virtualenv)
    } else if (!is.null(condaenv)) {
      clear_textrpp_options()
      options(textrpp_condaenv = condaenv)
    }
  } else if (check_env &&
               !(is.null(tryCatch(reticulate::conda_binary("auto"), error = function(e) NULL))) &&
               "textrpp_condaenv" %in% reticulate::conda_list(conda = "auto")$name) {
    message(colourise(
      "Found 'textrpp_condaenv'. text will use this environment \n",
      fg = "green", bg = NULL
    ))
    clear_textrpp_options()
    options(textrpp_condaenv = "textrpp_condaenv")
  } else if (check_env && file.exists(file.path("~/.virtualenvs", virtualenv, "bin", "activate"))) {
    message(colourise(
      "Found your specified virtual environment. Text will use this environment \n",
      fg = "green", bg = NULL
    )) # OK: original: Found 'textrpp_virtualenv'. Text will use this environment"
    clear_textrpp_options()
    options(textrpp_virtualenv = file.path("~/.virtualenvs/", virtualenv))
  } else {
    message("Finding a python executable with text required python pakages installed...")
    textrpp_python <- find_textrpp(ask = ask) # model,
    if (is.null(textrpp_python)) {
      stop("Text required python packages ", " are not installed in any of python executables.") #  model,
    } else if (is.na(textrpp_python)) {
      stop("No python was found on system PATH")
    } else {
      options(textrpp_python_executable = textrpp_python)
    }
  }
  return(NULL)
}


clear_textrpp_options <- function() {
  options(textrpp_python_executable = NULL)
  options(textrpp_condaenv = NULL)
  options(textrpp_virtualenv = NULL)
}

check_textrpp_python_options <- function() {
  settings <- NULL
  for (k in c(
    "textrpp_python_executable",
    "textrpp_condaenv",
    "textrpp_virtualenv"
  )) {
    if (!is.null(getOption(k))) {
      settings$key <- k
      settings$val <- getOption(k)
    }
  }
  return(settings)
}

save_textrpp_options <- function(key, val, prompt = TRUE) {
  prof_file <- "~/.Rprofile"
  if (!is.null(getOption("textrpp_prompt"))) prompt <- getOption("textrpp_prompt")

  ans <- if (prompt) {
    utils::menu(c("No", "Yes"),
      title = sprintf('Do you want to set the option, \'%s = "%s"\' , as a default (y|[n])? ', key, val)
    )
  } else {
    2
  }
  if (ans == 2) {
    rprofile <- if (file.exists(prof_file)) readLines(prof_file) else NULL
    rprofile <- grep("options\\(\\s*textrpp_.+\\)", rprofile, value = TRUE, invert = TRUE)
    rprofile <- c(rprofile, sprintf('options(%s = "%s")', key, val))
    write(rprofile, file = prof_file)
    message(colourise(
      "The option was saved. The option will be used in textrpp_initialize() in future \n",
      fg = "green", bg = NULL
    ))
  } else {
    message("The option was not saved (user cancelled)")
  }
}
