# copied and modified from tensorflow::install.R, https://github.com/rstudio/tensorflow/blob/master/R/install.R
# and https://github.com/quanteda/spacyr/tree/master/R

conda_args <- reticulate:::conda_args

#' Install text required python packages in conda or virtualenv environment
#'
#' @description Install text required python packages (rpp) in a self-contained environment.  For macOS and Linux-based systems, this will
#'   also install Python itself via a "miniconda" environment, for
#'   \code{textrpp_install}.  Alternatively, an existing conda installation may be
#'   used, by specifying its path.  The default setting of \code{"auto"} will
#'   locate and use an existing installation automatically, or download and
#'   install one if none exists.
#'
#'   For Windows, automatic installation of miniconda installation is not currently
#'   available, so the user will need to \href{https://conda.io/projects/conda/en/latest/user-guide/install/index.html}{miniconda (or Anaconda) manually}.
#'
#' @param conda character; path to conda executable. Default "auto" which
#'   automatically find the path
#' @param pip \code{TRUE} to use pip for installing rpp If \code{FALSE}, conda
#' package manager with conda-forge channel will be used for installing rpp.
#' @param rpp_version character; rpp:s  to install.
#' @param python_version character; determine Python version for condaenv
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
                          rpp_version = c('torch==0.4.1', 'transformers==3.3.1', 'numpy', 'nltk'),
                          python_version = "3.7",
                          envname = "textrpp_condaenv",
                          pip = TRUE,
                          python_path = NULL,
                          prompt = TRUE) {
  # verify os
  if (!is_windows() && !is_osx() && !is_linux()) {
    stop("This function is available only for Windows, Mac, and Linux")
  }
   #verify 64-bit
   if (.Machine$sizeof.pointer != 8) {
       stop("Unable to install Text on this platform.",
            "Binary installation is only available for 64-bit platforms.")
   }

  # resolve and look for conda
  conda <- tryCatch(reticulate::conda_binary(conda), error = function(e) NULL)
  have_conda <- !is.null(conda)

  # Mac and linux
  if (is_unix()) {
    # check for explicit conda method
    # validate that we have conda
    if (!have_conda) {
      cat("No conda was found in the system. ")
      ans <- utils::menu(c("No", "Yes"), title = "Do you want Text to download miniconda in ~/miniconda?")
      if (ans == 2) {
        text_install_miniconda()
        conda <- tryCatch(reticulate::conda_binary("auto"), error = function(e) NULL)
      } else stop("Conda environment installation failed (no conda binary found)\n", call. = FALSE)
    }

    # process the installation of text required python packages
    process_textrpp_installation_conda(conda,
                                     rpp_version,
                                     python_version,
                                     prompt,
                                     envname = envname,
                                     pip = pip)

    # Windows installation
  } else {

    # determine whether we have system python help(py_versions_windows)
    python_versions <- reticulate::py_versions_windows()
    python_versions <- python_versions[python_versions$type == "PythonCore", ]
    python_versions <- python_versions[python_versions$version %in% c("3.5", "3.6", "3.7"), ]
    python_versions <- python_versions[python_versions$arch == "x64", ]
    have_system <- nrow(python_versions) > 0
    if (have_system)
      # Well this isnt used later
      python_system_version <- python_versions[1, ]

    # validate that we have conda
    if (!have_conda) {

      #OK adds help(install_miniconda)
      reticulate::install_miniconda()
      conda <- tryCatch(reticulate::conda_binary("auto"), error = function(e) NULL)


#      stop("Conda installation failed (no conda binary found)\n\n",
#           "Install Anaconda 3.x for Windows (https://www.anaconda.com/download/#windows)\n",
#           "before installing text required python packages",
#           call. = FALSE)
    }

    # process the installation of text required python packages
    process_textrpp_installation_conda(conda,
                                     rpp_version,
                                     python_version,
                                     prompt,
                                     envname = envname,
                                     pip = pip)
  }

  message(colourise(
    "\nInstallation is completed.\n",
    fg = "blue", bg = NULL
  ))
  message(" ",
          sprintf("Condaenv: %s ", envname),  "\n")

  message(colourise(
    "Great work - do not forget to initialize the environment \nwith textrpp_initialize().\n",
    fg = "green", bg = NULL
  ))
  invisible(NULL)
}

#' @rdname textrpp_install
#' @description If you wish to install Python in a "virtualenv", use the
#'   \code{textrpp_install_virtualenv} function.
#' @param pip_version character;
#' @examples
#' \dontrun{
#' # install text required python packages in a virtualenv environment
#' textrpp_install_virtualenv()
#' }
#' @export
textrpp_install_virtualenv <- function(rpp_version = c('torch==0.4.1', 'transformers==3.3.1', 'numpy', 'nltk'),
                                     python_version = "3.7",
                                     pip_version = "pip",
                                     python_path = NULL,
                                     prompt = TRUE) {
  # verify os
  if (!is_osx() && !is_linux()) {
    stop("This function is available only for Mac and Linux", call. = FALSE)
  }

  # mac and linux
  # check for explicit conda method

  # find system python binary
  python <- if (!is.null(python_path)) python_path else python_unix_binary("python")
  if (is.null(python))
    stop("Unable to locate Python on this system.", call. = FALSE)

  # find other required tools
  pip <- python_unix_binary("pip")
  have_pip <- !is.null(pip)
  virtualenv <- python_unix_binary("virtualenv")
  have_virtualenv <- !is.null(virtualenv)

  # stop if either pip or virtualenv is not available
  if (!have_pip || !have_virtualenv) {
    install_commands <- NULL
    if (is_osx()) {
      if (!have_pip)
        install_commands <- c(install_commands, "$ sudo /usr/bin/easy_install pip") # original: "$ sudo /usr/bin/easy_install pip"
      if (!have_virtualenv) {
        if (is.null(pip))
          pip <- "/usr/local/bin/pip"
        install_commands <- c(install_commands, sprintf("$ sudo %s install --upgrade virtualenv", pip)) #"$ sudo %s install --upgrade virtualenv"
      }
      if (!is.null(install_commands))
        install_commands <- paste(install_commands, collapse = "\n")
    } else if (is_ubuntu()) {
      if (!have_pip)
        install_commands <- c(install_commands, "python-pip")
      if (!have_virtualenv)
        install_commands <- c(install_commands, "python-virtualenv")
      if (!is.null(install_commands)) {
        install_commands <- paste("$ sudo apt-get install", #"$ sudo apt-get install"
                                  paste(install_commands, collapse = " "))
      }
    } else {
      if (!have_pip)
        install_commands <- c(install_commands, "pip")
      if (!have_virtualenv)
        install_commands <- c(install_commands, "virtualenv")
      if (!is.null(install_commands)) {
        install_commands <- paste("Please install the following Python packages before proceeding:",
                                  paste(install_commands, collapse = ", "))
      }
    }
    if (!is.null(install_commands)) {

      # if these are terminal commands then add special preface
      if (grepl("^\\$ ", install_commands)) {
        install_commands <- paste0(
          "Execute the following at a terminal to install the prerequisites:\n",
          install_commands
        )
      }

      stop("Prerequisites for installing text required python packages in a virtual environment are not available.\n\n",
           install_commands, "\n\n", call. = FALSE)
    }
  }
  process_textrpp_installation_virtualenv(python, pip_version, virtualenv, rpp_version, prompt)

  cat("\nInstallation complete.\n\n")

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
    ans <- utils::menu(c("No", "Yes"), title = "Proceed?")
    if (ans == 1) stop("condaenv setup is cancelled by user", call. = FALSE)
  }
  conda_env <- subset(conda_envs, conda_envs$name == envname)
  if (nrow(conda_env) == 1) {
    cat("Using existing conda environment ", envname, " for text installation\n.",
        "\ntext:",
        paste(rpp_version, collapse = ", "), "will be installed.  ")
    python <- conda_env$python
  }
  else {
    cat("A new conda environment", paste0('"', envname, '"'), "will be created and \npython required packages:",
        paste(rpp_version, collapse = ", "), "will be installed.  ")
    cat("Creating", envname, "conda environment for text installation...\n")
    python_packages <- ifelse(is.null(python_version), "python=3.6",
                              sprintf("python=%s", python_version))
    python <- reticulate::conda_create(envname, packages = python_packages, conda = conda)
  }

  cat("Installing text required python packages...\n")
  packages <- rpp_version

  # OK: Install OpenSSL for conda using NOT pip
  #reticulate::conda_install(envname, packages = "opensll", pip = FALSE, conda = conda)
  reticulate::conda_install(envname, packages = "sll", pip = FALSE, conda = conda)
  #reticulate::conda_install(envname, packages = "pyopenssl", pip = FALSE, conda = conda)
  reticulate::conda_install(envname, packages, pip = pip, conda = conda)

}

process_textrpp_installation_virtualenv <- function(python,
                                                    pip_version = "pip",
                                                    virtualenv,
                                                    rpp_version,
                                                    prompt = TRUE) {

  # determine python version to use
  is_python3 <- python_version(python) >= "3.0"
  if (!is_python3) {
    stop("text does not support virtual environment installation for python 2.*", call. = FALSE)
  }
  pip_version <- ifelse(is_python3, "pip3", "pip")

  virtualenv_root <- Sys.getenv("WORKON_HOME", unset = "~/.virtualenvs")
  virtualenv_path <- file.path(virtualenv_root, "textrpp_virtualenv")

  cat(sprintf('A new virtual environment "%s" will be created and, \n text required python packages, "%s", will be installed.\n ',
              virtualenv_path,
              paste(rpp_version, collapse = ", ")))
  if (prompt) {
    ans <- utils::menu(c("No", "Yes"), title = "Proceed?")
    if (ans == 1) stop("Virtualenv setup is cancelled by user", call. = FALSE)
  }

  # create virtualenv
  if (!file.exists(virtualenv_root))
    dir.create(virtualenv_root, recursive = TRUE)

  # helper to construct paths to virtualenv binaries
  virtualenv_bin <- function(bin) path.expand(file.path(virtualenv_path, "bin", bin))

  # create virtualenv if necessary
  if (!file.exists(virtualenv_path) || !file.exists(virtualenv_bin("activate"))) {
    cat("Creating virtualenv for text required python packages at ", virtualenv_path, "\n")
    result <- system2(virtualenv, shQuote(c(
      #"--system-site-packages",
      "--python", python,
      path.expand(virtualenv_path)))
    )
    if (result != 0L)
      stop("Error ", result, " occurred creating virtualenv at ", virtualenv_path,
           call. = FALSE)
  } else {
    cat("Using existing virtualenv at ", virtualenv_path, "\n")
  }

  # function to call pip within virtual env
  pip_install <- function(pkgs, message) {
    cmd <- sprintf("%ssource %s && %s install --ignore-installed --upgrade %s%s",
                   ifelse(is_osx(), "", "/bin/bash -c \""),
                   shQuote(path.expand(virtualenv_bin("activate"))),
                   shQuote(path.expand(virtualenv_bin(pip_version))),
                   paste(shQuote(pkgs), collapse = " "),
                   ifelse(is_osx(), "", "\""))
    cat(message, "...\n")
    result <- system(cmd)
    if (result != 0L)
      stop("Error ", result, " occurred installing text required python packages", call. = FALSE)
  }

  # upgrade pip
  pip_install(pip_version, "Upgrading pip")

  # install updated version of the wheel package
  # pip_install("wheel", "Upgrading wheel")

  # upgrade setuptools so it can use wheels
  # pip_install("setuptools", "Upgrading setuptools")

  pkgs <- rpp_version
  pip_install(pkgs, "Installing text required python packages...")

}

python_unix_binary <- function(bin) {
  locations <- file.path(c( "/usr/local/bin", "/usr/bin"), bin)
  locations <- locations[file.exists(locations)]
  if (length(locations) > 0)
    locations[[1]]
  else
    NULL
}

python_version <- function(python) {

  # check for the version
  result <- system2(python, "--version", stdout = TRUE, stderr = TRUE)

  # check for error
  error_status <- attr(result, "status")
  if (!is.null(error_status))
    stop("Error ", error_status, " occurred while checking for python version", call. = FALSE)

  # parse out the major and minor version numbers
  matches <- regexec("^[^ ]+\\s+(\\d+)\\.(\\d+).*$", result)
  matches <- regmatches(result, matches)[[1]]
  if (length(matches) != 3)
    stop("Unable to parse Python version '", result[[1]], "'", call. = FALSE)

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

  if (!have_conda)
    stop("Conda installation failed (no conda binary found)\n", call. = FALSE)

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


# Removed here.
# textrpp_upgrade  <- function(conda = "auto",
#                            envname = "textrpp_condaenv",
#                            prompt = TRUE,
#                            pip = FALSE,
#                            update_conda = FALSE
#                            )

text_install_miniconda <- function() {
  if (is_osx()) {
    message("Downloading installation script")
    system(paste(
      "curl https://repo.continuum.io/miniconda/Miniconda3-latest-MacOSX-x86_64.sh -o ~/miniconda.sh;",
      "echo \"Running installation script\";",
      "bash ~/miniconda.sh -b -p $HOME/miniconda"))
    system('echo \'export PATH="$PATH:$HOME/miniconda/bin"\' >> $HOME/.bash_profile; rm ~/miniconda.sh')
    message("Installation of miniconda complete")
  } else if (is_linux()) {
    message("Downloading installation script")
    system(paste(
      "wget -nv https://repo.continuum.io/miniconda/Miniconda3-latest-Linux-x86_64.sh -O ~/miniconda.sh;",
      "echo \"Running installation script\";",
      "bash ~/miniconda.sh -b -p $HOME/miniconda"))
    system('echo \'export PATH="$PATH:$HOME/miniconda/bin"\' >> $HOME/.bashrc; rm ~/miniconda.sh')
    message("Installation of miniconda complete")
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
                  collapse = " ")
  options(warn = oldw)
  version_check_regex <- sprintf(".+(%s.\\d+\\.\\d+).+", major_version)
  return(sub(version_check_regex, "\\1", result))
}


conda_get_version <- function(major_version = NA, conda, envname) {
  condaenv_bin <- function(bin) path.expand(file.path(dirname(conda), bin))
  cmd <- sprintf("%s%s %s && conda search torch -c conda-forge%s",
                 ifelse(is_windows(), "", ifelse(is_osx(), "source ", "/bin/bash -c \"source ")),
                 shQuote(path.expand(condaenv_bin("activate"))),
                 envname,
                 ifelse(is_windows(), "", ifelse(is_osx(), "", "\"")))
  regex <- "^(\\S+)\\s?(.*)$"
  cmd1 <- sub(regex, "\\1", cmd)
  cmd2 <- sub(regex, "\\2", cmd)
  oldw <- getOption("warn")
  result <- system2(cmd1, cmd2, stdout = TRUE, stderr = TRUE)
  result <- sub("\\S+\\s+(\\S+)\\s.+", "\\1", result)
  if (!is.na(major_version)) {
    result <- grep(paste0("^", major_version, "\\."), result, value = T)
  }
  #version_check_regex <- sprintf(".+(%s.\\d+\\.\\d+).+", major_version)
  return(result[length(result)])
}



###### see utils.R in spacyr
# checking os functions, thanks to r-tensorflow;

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
