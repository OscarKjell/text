# copied and modified from tensorflow::install.R, https://github.com/rstudio/tensorflow/blob/master/R/install.R
# and https://github.com/quanteda/spacyr/tree/master/R

conda_args <- reticulate:::conda_args


#' Install text required python packages in conda or virtualenv environment
#'
#' @description Install text required python packages (rpp) in a self-contained environment.
#' For macOS and Linux-based systems, this will also install Python itself via a "miniconda" environment, for
#'   \code{textrpp_install}.  Alternatively, an existing conda installation may be
#'   used, by specifying its path.  The default setting of \code{"auto"} will
#'   locate and use an existing installation automatically, or download and
#'   install one if none exists.
#'
#'   For Windows, automatic installation of miniconda installation is not currently
#'   available, so the user will need to
#'   \href{https://conda.io/projects/conda/en/latest/user-guide/install/index.html}{miniconda
#'    (or Anaconda) manually}.
#' @param conda character; path to conda executable. Default "auto" which
#'   automatically find the path
#' @param update_conda Boolean; update to the latest version of Miniconda after install?
#' (should be combined with force_conda = TRUE)
#' @param force_conda Boolean; force re-installation if Miniconda is already installed at the requested path?
#' @param pip \code{TRUE} to use pip for installing rpp If \code{FALSE}, conda
#' package manager with conda-forge channel will be used for installing rpp.
#' @param rpp_version character; default is "rpp_version_system_specific_defaults", because diffent systems require
#' different combinations of python version and packages. It is also possible to
#' specify your own, such as c("torch==2.0.0", "transformers==4.19.2", "numpy", "nltk", "scikit-learn", "datasets", "evaluate").
#' @param python_version character; default is "python_version_system_specific_defaults". You can specify your
#' Python version for the condaenv yourself.
#'   installation.
#' @param python_path character; path to Python in virtualenv installation
#' @param envname character; name of the conda-environment to install text required python packages.
#'   Default is "textrpp_condaenv".
#' @param prompt logical; ask whether to proceed during the installation
#' @examples
#' \dontrun{
#' # install text required python packages in a miniconda environment (macOS and Linux)
#' textrpp_install(prompt = FALSE)
#'
#' # install text required python packages to an existing conda environment
#' textrpp_install(conda = "~/anaconda/bin/")
#' }
#' @export
textrpp_install <- function(conda = "auto",
                            update_conda = FALSE,
                            force_conda = FALSE,
                            rpp_version = "rpp_version_system_specific_defaults",
                            python_version = "python_version_system_specific_defaults",
                            envname = "textrpp_condaenv",
                            pip = TRUE,
                            python_path = NULL,
                            prompt = TRUE) {

  # Set system specific default versions
  if (rpp_version[[1]] == "rpp_version_system_specific_defaults") {
    if (is_osx() | is_linux()) {
      rpp_version <- c("torch==2.0.0", "transformers==4.19.2", "numpy", "nltk", "scikit-learn", "datasets", "evaluate")
    }
    if (is_windows()) {
      rpp_version <- c("torch==2.0.0", "transformers==4.19.2", "numpy", "nltk", "scikit-learn", "datasets", "evaluate")
    }
  }

  if (python_version == "python_version_system_specific_defaults") {
    if (is_osx() | is_linux()) {
      python_version <- "3.9.0"
    }

    if (is_windows()) {
      python_version <- "3.9.0"
    }
  }

  # verify os
  if (!is_windows() && !is_osx() && !is_linux()) {
    stop("This function is available only for Windows, Mac, and Linux")
  }

  # verify 64-bit
  if (.Machine$sizeof.pointer != 8) {
    stop(
      "Unable to install the text-package on this platform.",
      "Binary installation is only available for 64-bit platforms."
    )
  }

  # resolve and look for conda help(conda_binary)
  conda <- tryCatch(reticulate::conda_binary(conda), error = function(e) NULL)
  have_conda <- !is.null(conda)

  # Mac and linux
  if (is_unix()) {
    # check for explicit conda method
    # validate that we have conda
    if (!have_conda) {
      cat("No conda was found in the system. ")
      if (prompt) {
        ans <- utils::menu(c("No", "Yes"), title = "Do you want Text to download
                           miniconda using reticulate::install_miniconda()?")
      } else {
        ans <- 2 # When no prompt is set to false, default to install miniconda.
      }
      if (ans == 2) {
        reticulate::install_miniconda(update = update_conda)
        conda <- tryCatch(reticulate::conda_binary("auto"), error = function(e) NULL)
      } else {
        stop("Conda environment installation failed (no conda binary found)\n", call. = FALSE)
      }
    }

    # Update mini_conda
    if (update_conda & force_conda | force_conda) {
      reticulate::install_miniconda(update = update_conda, force = force_conda)
    }

    # process the installation of text required python packages
    process_textrpp_installation_conda(conda,
      rpp_version,
      python_version,
      prompt,
      envname = envname,
      pip = pip
    )

    # Windows installation
  } else {

    # determine whether we have system python help(py_versions_windows)
    if (python_version == "find_python") {
      python_versions <- reticulate::py_versions_windows()
      python_versions <- python_versions[python_versions$type == "PythonCore", ]
      python_versions <- python_versions[python_versions$version %in% c("3.5", "3.6", "3.7", "3.8", "3.9"), ]
      python_versions <- python_versions[python_versions$arch == "x64", ]
      have_system <- nrow(python_versions) > 0

      if (have_system) {
        # Well this isn't used later
        python_version <- python_versions[1, ]
      }
    }

    # validate that we have conda:
    if (!have_conda) {

      # OK adds help(install_miniconda)
      reticulate::install_miniconda(update = update_conda)
      conda <- tryCatch(reticulate::conda_binary("auto"), error = function(e) NULL)
    }
    # Update mini_conda
    if (have_conda & update_conda | have_conda & force_conda) {
      reticulate::install_miniconda(update = update_conda, force = force_conda)
    }
    # process the installation of text required python packages
    process_textrpp_installation_conda(conda,
      rpp_version,
      python_version,
      prompt,
      envname = envname,
      pip = pip
    )
  }

  message(colourise(
    "\nInstallation is completed.\n",
    fg = "blue", bg = NULL
  ))
  message(
    " ",
    sprintf("Condaenv: %s ", envname), "\n"
  )

  message(colourise(
    "Great work - do not forget to initialize the environment \nwith textrpp_initialize().\n",
    fg = "green", bg = NULL
  ))
  invisible(NULL)
}

process_textrpp_installation_conda <- function(conda,
                                               rpp_version,
                                               python_version,
                                               prompt = TRUE,
                                               envname = "textrpp_condaenv",
                                               pip = FALSE) {
  conda_envs <- reticulate::conda_list(conda = conda)
  if (prompt) {
    ans <- utils::menu(c("Confirm", "Cancel"), title = "Confirm that a new conda environment will be set up.")
    if (ans == 2) stop("condaenv setup is cancelled by user", call. = FALSE)
  }
  conda_env <- subset(conda_envs, conda_envs$name == envname)
  if (nrow(conda_env) == 1) {
    cat(
      "Using existing conda environment ", envname, " for text installation\n.",
      "\ntext:",
      paste(rpp_version, collapse = ", "), "will be installed.  "
    )
  } else {
    cat(
      "A new conda environment", paste0('"', envname, '"'), "will be created and \npython required packages:",
      paste(rpp_version, collapse = ", "), "will be installed.  "
    )
    cat("Creating", envname, "conda environment for text installation...\n")
    python_packages <- ifelse(is.null(python_version), "python=3.9",
      sprintf("python=%s", python_version)
    )
    python <- reticulate::conda_create(envname, packages = python_packages, conda = conda)
  }

  cat("Installing text required python packages...\n")
  packages <- rpp_version

  reticulate::conda_install(envname, packages, pip = pip, conda = conda)
}


#' @rdname textrpp_install
#' @description If you wish to install Python in a "virtualenv", use the
#'   \code{textrpp_install_virtualenv} function. It requires that you have a python version
#'   and path to it (such as "/usr/local/bin/python3.9" for Mac and Linux.).
#' @param pip_version character;
#' @examples
#' \dontrun{
#' # install text required python packages in a virtual environment
#' textrpp_install_virtualenv()
#' }
#' @export
textrpp_install_virtualenv <- function(rpp_version = c("torch==2.0.0", "transformers==4.19.2", "numpy", "nltk"),
                                       python_path = "/usr/local/bin/python3.9",
                                       pip_version = NULL,
                                       envname = "textrpp_virtualenv",
                                       prompt = TRUE) {

  # find system python binary
  python <- if (!is.null(python_path)) python_path else python_unix_binary("python")
  if (is.null(python)) {
    stop("Unable to locate Python on this system.", call. = FALSE)
  }

  process_textrpp_installation_virtualenv(
    python = python,
    pip_version = pip_version,
    rpp_version = rpp_version,
    envname = envname,
    prompt = prompt
  )


  message(colourise(
    "\nInstallation is completed.\n",
    fg = "blue", bg = NULL
  ))
  invisible(NULL)
}


process_textrpp_installation_virtualenv <- function(python = "/usr/local/bin/python3.9",
                                                    rpp_version,
                                                    pip_version,
                                                    envname = "textrpp_virtualenv",
                                                    prompt = TRUE) {
  libraries <- paste(rpp_version, collapse = ", ")
  cat(sprintf(
    'A new virtual environment called "%s" will be created using "%s" \n and,
    the following text reuired python packages will be installed: \n "%s" \n \n',
    envname, python, libraries
  ))
  if (prompt) {
    ans <- utils::menu(c("No", "Yes"), title = "Proceed?")
    if (ans == 1) stop("Virtualenv setup is cancelled by user", call. = FALSE)
  }

  # Make python path help(virtualenv_create)
  reticulate::virtualenv_create(envname, python, pip_version = NULL, required = TRUE)

  reticulate::use_virtualenv(envname, required = TRUE)

  #
  for (i in seq_len(length(rpp_version))) {
    reticulate::py_install(rpp_version[[i]], envname = envname, pip = TRUE)
  }

  message(colourise(
    "\nSuccess!\n",
    fg = "green", bg = NULL
  ))
}


# Check whether "bin"/something exists in the bin folder
# For example, bin = "pip3" bin = "python3.9" bin = ".virtualenv"
# And for example: file.exists("/usr/local/bin/.virtualenvs") /Users/oscarkjell/.virtualenvs
python_unix_binary <- function(bin) {
  locations <- file.path(c("/usr/local/bin", "/usr/bin"), bin)
  locations <- locations[file.exists(locations)]
  if (length(locations) > 0) {
    locations[[1]]
  } else {
    NULL
  }
}


python_version_function <- function(python) {

  # check for the version
  result <- system2(python, "--version", stdout = TRUE, stderr = TRUE)

  # check for error
  error_status <- attr(result, "status")
  if (!is.null(error_status)) {
    stop("Error ", error_status, " occurred while checking for python version", call. = FALSE)
  }

  # parse out the major and minor version numbers
  matches <- regexec("^[^ ]+\\s+(\\d+)\\.(\\d+).*$", result)
  matches <- regmatches(result, matches)[[1]]
  if (length(matches) != 3) {
    stop("Unable to parse Python version '", result[[1]], "'", call. = FALSE)
  }

  # return as R numeric version
  numeric_version(paste(matches[[2]], matches[[3]], sep = "."))
}


#' Uninstall textrpp conda environment
#'
#' Removes the conda environment created by textrpp_install()
#' @param conda path to conda executable, default to "auto" which automatically
#'   finds the path
#' @param prompt logical; ask whether to proceed during the installation
#' @param envname character; name of conda environment to remove
#' @export
textrpp_uninstall <- function(conda = "auto",
                              prompt = TRUE,
                              envname = "textrpp_condaenv") {
  conda <- tryCatch(reticulate::conda_binary(conda), error = function(e) NULL)
  have_conda <- !is.null(conda)

  if (!have_conda) {
    stop("Conda installation failed (no conda binary found)\n", call. = FALSE)
  }

  conda_envs <- reticulate::conda_list(conda = conda)
  conda_env <- subset(conda_envs, conda_envs$name == envname)
  if (nrow(conda_env) != 1) {
    stop("conda environment", envname, "is not found", call. = FALSE)
  }
  cat("A conda environment", envname, "will be removed\n")
  ans <- ifelse(prompt, utils::menu(c("No", "Yes"), title = "Proceed?"), 2)
  if (ans == 1) stop("condaenv removal is cancelled by user", call. = FALSE)
  python <- reticulate::conda_remove(envname = envname)

  cat("\nUninstallation complete.\n\n")

  invisible(NULL)
}


text_install_miniconda <- function() {
  if (is_osx()) {
    message("Downloading installation script")
    system(paste(
      "curl https://repo.continuum.io/miniconda/Miniconda3-latest-MacOSX-x86_64.sh -o ~/miniconda.sh;",
      "echo \"Running installation script\";",
      "bash ~/miniconda.sh -b -p $HOME/miniconda"
    ))
    system('echo \'export PATH="$PATH:$HOME/miniconda/bin"\' >> $HOME/.bash_profile; rm ~/miniconda.sh')
    message(colourise(
      "Installation of miniconda complete",
      fg = "green", bg = NULL
    ))
  } else if (is_linux()) {
    message("Downloading installation script")
    system(paste(
      "wget -nv https://repo.continuum.io/miniconda/Miniconda3-latest-Linux-x86_64.sh -O ~/miniconda.sh;",
      "echo \"Running installation script\";",
      "bash ~/miniconda.sh -b -p $HOME/miniconda"
    ))
    system('echo \'export PATH="$PATH:$HOME/miniconda/bin"\' >> $HOME/.bashrc; rm ~/miniconda.sh')
    message(colourise(
      "Installation of miniconda complete",
      fg = "green", bg = NULL
    ))
  } else {
    stop("miniconda installation is available only for Mac or Linux")
  }
}


pip_get_version <- function(cmd, major_version) {
  regex <- "^(\\S+)\\s?(.*)$"
  cmd1 <- sub(regex, "\\1", cmd)
  cmd2 <- sub(regex, "\\2", cmd)
  oldw <- getOption("warn")
  options(warn = -1)
  result <- paste(system2(cmd1, cmd2, stdout = TRUE, stderr = TRUE),
    collapse = " "
  )
  options(warn = oldw)
  version_check_regex <- sprintf(".+(%s.\\d+\\.\\d+).+", major_version)
  return(sub(version_check_regex, "\\1", result))
}


conda_get_version <- function(major_version = NA, conda, envname) {
  condaenv_bin <- function(bin) path.expand(file.path(dirname(conda), bin))
  cmd <- sprintf(
    "%s%s %s && conda search torch -c conda-forge%s",
    ifelse(is_windows(), "", ifelse(is_osx(), "source ", "/bin/bash -c \"source ")),
    shQuote(path.expand(condaenv_bin("activate"))),
    envname,
    ifelse(is_windows(), "", ifelse(is_osx(), "", "\""))
  )
  regex <- "^(\\S+)\\s?(.*)$"
  cmd1 <- sub(regex, "\\1", cmd)
  cmd2 <- sub(regex, "\\2", cmd)

  result <- system2(cmd1, cmd2, stdout = TRUE, stderr = TRUE)
  result <- sub("\\S+\\s+(\\S+)\\s.+", "\\1", result)
  if (!is.na(major_version)) {
    result <- grep(paste0("^", major_version, "\\."), result, value = T)
  }
  #
  return(result[length(result)])
}


###### see utils.R in spacyr
# checking OS functions, thanks to r-tensorflow;

is_windows <- function() {
  identical(.Platform$OS.type, "windows")
}

is_unix <- function() {
  identical(.Platform$OS.type, "unix")
}

is_osx <- function() {
  Sys.info()["sysname"] == "Darwin"
}

is_linux <- function() {
  identical(tolower(Sys.info()[["sysname"]]), "linux")
}

is_ubuntu <- function() {
  if (is_unix() && file.exists("/etc/lsb-release")) {
    lsbrelease <- readLines("/etc/lsb-release")
    any(grepl("Ubuntu", lsbrelease))
  } else {
    FALSE
  }
}
