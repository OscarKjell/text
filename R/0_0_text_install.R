# copied and modified from tensorflow::install.R, https://github.com/rstudio/tensorflow/blob/master/R/install.R
# and https://github.com/quanteda/spacyr/tree/master/R

conda_args <- reticulate:::conda_args

install_rust_if_needed <- function(prompt = TRUE) {
  is_windows <- identical(.Platform$OS.type, "windows")
  is_unix <- identical(.Platform$OS.type, "unix")

  # Helper to check if an executable exists
  is_installed <- function(cmd) {
    nzchar(Sys.which(cmd))
  }

  # 1. Check if Rust is already installed
  if (is_installed("rustc")) {
    message(colourise("Rust is already installed. Skipping Rust installation.\n", fg = "blue"))
    return(invisible(NULL))
  }

  message("Rust is not installed on this system.")

  # 2. Check if 'curl' is available (needed for macOS/Linux installation)
  if (!is_windows && !is_installed("curl")) {
    warning("Rust installation aborted: 'curl' not found.\nPlease install Rust manually: https://www.rust-lang.org/")
    return(invisible(NULL))
  }

  # 3. Prompt user for permission to install
  ans <- if (prompt) {
    utils::menu(c("No", "Yes"), title = "Do you want to install Rust?")
  } else {
    2
  }

  if (ans == 1) {
    message("Rust installation cancelled by user.")
    return(invisible(NULL))
  }

  # 4. Try installing Rust
  tryCatch({
    if (is_windows) {
      message("Downloading Rust installer for Windows...")
      installer <- tempfile(fileext = ".exe")
      download.file("https://static.rust-lang.org/rustup/dist/x86_64-pc-windows-msvc/rustup-init.exe", installer, mode = "wb")
      message("Launching Rust installer...")
      shell.exec(installer)
      message("Rust installer launched. Follow on-screen instructions.\n")
    } else {
      message("Downloading and installing Rust for macOS/Linux...")
      system("curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y", intern = TRUE)
    }

    # 5. Re-check if rustc is now available
    if (is_installed("rustc")) {
      message(colourise("Rust installation completed successfully.\n", fg = "green"))
    } else {
      warning("Rust installation attempted, but 'rustc' not found in PATH.\n",
              "Try restarting your terminal or install Rust manually:\n  https://www.rust-lang.org/tools/install")
    }

  }, error = function(e) {
    warning("Rust installation failed: ", e$message, "\nPlease install Rust manually from https://www.rust-lang.org/")
  })

  invisible(NULL)
}

#' Check macos githubaction dependencies
#' @param verbose If TRUE provides verbose information
#' @return infomration regrding system dependencies
#' @noRd
check_macos_githubaction_dependencies <- function(verbose = TRUE) {
  if (!is_osx()) return(invisible(NULL))

  # Define required and optional dependencies
  required_deps <- c("homebrew", "libomp")
  optional_deps <- c("qpdf")

  status_list <- setNames(logical(length(required_deps)), required_deps)
  summary_lines <- c("== macOS Required Dependencies ==")

  # Check Homebrew
  brew_path <- Sys.which("brew")
  if (brew_path == "") {
    status_list["homebrew"] <- FALSE
    summary_lines <- c(
      summary_lines,
      "'homebrew' is NOT installed.",
      "To install it, open your Terminal and run:",
      '  /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"'
    )
  } else {
    status_list["homebrew"] <- TRUE
    summary_lines <- c(summary_lines, "'homebrew' is installed.")
  }

  # Check libomp (only if brew is available)
  if (status_list["homebrew"]) {
    status_list["libomp"] <- system("brew list libomp", ignore.stdout = TRUE, ignore.stderr = TRUE) == 0
    if (status_list["libomp"]) {
      summary_lines <- c(summary_lines, "'libomp' is installed.")
    } else {
      summary_lines <- c(
        summary_lines,
        "'libomp' is NOT installed.",
        "To install it, open your Terminal and run:",
        "  brew install libomp"
      )
    }
  } else {
    status_list["libomp"] <- FALSE
  }

  # Optional: Check qpdf
  optional_status <- list()
  if (status_list["homebrew"]) {
    optional_status$qpdf <- system("brew list qpdf", ignore.stdout = TRUE, ignore.stderr = TRUE) == 0
  } else {
    optional_status$qpdf <- NA
  }

  # Identify installed/missing
  installed <- names(status_list)[status_list]
  missing <- names(status_list)[!status_list]

  if (length(installed) > 0) {
    summary_lines <- c(summary_lines, "", "Installed:", paste0("  - ", installed))
  }

  if (length(missing) > 0) {
    summary_lines <- c(
      summary_lines,
      "",
      "Missing (required):",
      paste0("  - ", missing),
      "",
      "To install required tools, run:"
    )

    if ("homebrew" %in% missing) {
      summary_lines <- c(
        summary_lines,
        '  /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"'
      )
    } else {
      summary_lines <- c(
        summary_lines,
        paste0("  brew install ", paste(missing[missing != "homebrew"], collapse = " "))
      )
    }

    warning("Some required macOS system libraries are missing. See message for details.")
  }

  # Optional summary
  summary_lines <- c(summary_lines, "", "== Optional Tools ==")
  if (isTRUE(optional_status$qpdf)) {
    summary_lines <- c(summary_lines, "'qpdf' is installed.")
  } else {
    summary_lines <- c(
      summary_lines,
      "'qpdf' is NOT installed.",
      "You can install it using:",
      "  brew install qpdf"
    )
  }

  # Return result
  result <- list(
    os = "macOS",
    required_deps = required_deps,
    optional_deps = optional_deps,
    installed = status_list,
    missing = missing,
    optional = optional_status,
    summary_lines = summary_lines,
    required_all_ok = length(missing) == 0
  )

  if (verbose) message(paste(summary_lines, collapse = "\n"))
  invisible(result)
}


#' Check linux githubaction dependencies
#' @param verbose If TRUE provides verbose information
#' @return infomration regrding system dependencies
#' @noRd
check_linux_githubaction_dependencies <- function(verbose = TRUE) {
  if (!is_linux()) return(invisible(NULL))

  # Define required and optional dependencies
  required_deps <- c(
    "libcurl4-openssl-dev",
   # "libgit2-dev",
    "libssl-dev",
    "libharfbuzz-dev",
    "libfribidi-dev",
    "libxml2-dev",
    "libpng-dev",
    "libtiff5-dev",
    "libjpeg-dev",
    "libfontconfig1-dev",
    "libicu-dev",
   # "libcairo2-dev",
    "default-jdk"
  )

  optional_deps <- c("libfreetype6-dev")  # Add optional Linux deps here if needed

  # Check installation status for required deps
  status_list <- setNames(logical(length(required_deps)), required_deps)
  for (lib in required_deps) {
    installed <- system2("dpkg", c("-s", lib), stdout = NULL, stderr = NULL) == 0
    status_list[lib] <- installed
  }

  installed <- names(status_list)[status_list]
  missing <- names(status_list)[!status_list]

  # Format output
  summary_lines <- c("== Linux Required Dependencies ==")

  if (length(installed) > 0) {
    summary_lines <- c(summary_lines, "Installed:", paste("  -", installed))
  }

  if (length(missing) > 0) {
    summary_lines <- c(
      summary_lines,
      "",
      "Missing (required):",
      paste("  -", missing),
      "",
      "To install them on Debian/Ubuntu systems, run:",
      paste0("  sudo apt-get install -y ", paste(missing, collapse = " "))
    )
    warning("Some required system libraries are missing. See message for details.")
  }

  # Optional deps (none currently defined)
  if (length(optional_deps) > 0) {
    summary_lines <- c(summary_lines, "", "== Optional Dependencies ==")
    # Placeholder check logic for optional_deps if needed
  }

  result <- list(
    os = "Linux",
    required_deps = required_deps,
    optional_deps = optional_deps,
    installed = status_list,
    missing = missing,
    summary_lines = summary_lines,
    required_all_ok = length(missing) == 0
  )

  if (verbose) message(paste(summary_lines, collapse = "\n"))

  invisible(result)
}

#' Ensure that the conda-forge channel is used for conda installations
#'
#' This function configures the conda package manager to:
#' 1. Remove the default Anaconda channels (which may now require Terms of Service acceptance),
#' 2. Add the conda-forge channel, and
#' 3. Set strict channel priority to ensure all packages are pulled from conda-forge.
#'
#' This is especially useful in non-interactive environments (e.g., CI/CD or automated setup scripts)
#' where accepting Anaconda's Terms of Service is not feasible.
#'
#' @param conda Either "auto" or a path to the conda binary (e.g., from `reticulate::conda_binary()`).
#' @return Invisibly returns NULL. Side effect is that it modifies the conda configuration.
#' @noRd
ensure_conda_forge <- function(conda) {
  if (is.null(conda) || length(conda) == 0 || conda == "auto") {
    conda_path <- reticulate::conda_binary("auto")
  } else {
    conda_path <- conda
  }

  system2(conda_path, c("config", "--remove", "channels", "defaults"))
  system2(conda_path, c("config", "--add", "channels", "conda-forge"))
  system2(conda_path, c("config", "--set", "channel_priority", "strict"))
  invisible(NULL)
}






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
#'   available, so the user will need to install
#'   \href{https://docs.conda.io/projects/conda/en/latest/user-guide/install/index.html}{miniconda
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
#' specify your own, such as c("torch==2.0.0", "transformers==4.19.2", "numpy", "pandas", "nltk", "scikit-learn",
#' "datasets", "evaluate").
#' @param python_version character; default is "python_version_system_specific_defaults". You can specify your
#' Python version for the condaenv yourself.
#'   installation.
#' @param python_path character; path to Python only for virtualenvironment installation
#' @param bin character; e.g., "python", only for virtualenvironment installation
#' @param envname character; name of the conda-environment to install text required python packages.
#'   Default is "textrpp_condaenv".
#' @param prompt logical; ask whether to proceed during the installation
#' @param conda_forge (boolean) TRUE ensures using forge channels to avoid having to accept
#' Terms of Service from Anaconda
#' @examples
#' \dontrun{
#' # install text required python packages in a miniconda environment (macOS and Linux)
#' textrpp_install(prompt = FALSE)
#'
#' # install text required python packages to an existing conda environment
#' textrpp_install(conda = "~/anaconda/bin/")
#' }
#' @export
textrpp_install <- function(
    conda = "auto",
    update_conda = FALSE,
    force_conda = FALSE,
    rpp_version = "rpp_version_system_specific_defaults",
    python_version = "python_version_system_specific_defaults",
    envname = "textrpp_condaenv",
    pip = TRUE,
    python_path = NULL,
    prompt = TRUE,
    conda_forge = TRUE
    ) {


  # Check system-level dependencies
  macos_log <- check_macos_githubaction_dependencies(verbose = prompt)
  linux_log <- check_linux_githubaction_dependencies(verbose = prompt)

  # Collect missing dependencies
  critical_missing <- character()
  terminal_command <- NULL

  if (!is.null(macos_log$missing) && length(macos_log$missing) > 0) {
    critical_missing <- c(critical_missing, macos_log$missing)
    terminal_command <- paste0("brew install ", paste(macos_log$missing, collapse = " "))
  }

  if (!is.null(linux_log$missing) && length(linux_log$missing) > 0) {
    critical_missing <- c(critical_missing, linux_log$missing)
    terminal_command <- paste0("sudo apt-get install -y ", paste(linux_log$missing, collapse = " "))
  }

  # Stop or prompt depending on `prompt` setting
  if (length(critical_missing) > 0) {
    message("\nSystem dependency check identified the following missing libraries:")
    for (pkg in critical_missing) {
      message("  - ", pkg)
    }

    if (!is.null(terminal_command)) {
      message("\nTo install them, open your terminal and run:")
      message("  ", terminal_command)
    }

    if (!prompt) {
      message("\nInstallation stopped. Set `prompt = TRUE` if you wish to override and proceed anyway.")
      return(invisible(NULL))
    } else {
      ans <- utils::menu(c("No", "Yes"), title = "\nDo you still want to continue with installation?")
      if (ans == 1) {
        message("Installation cancelled due to missing system dependencies.")
        return(invisible(NULL))
      }
    }
  }


  # Set system specific default versions
  if (rpp_version[[1]] == "rpp_version_system_specific_defaults") {
    if (is_osx() || is_linux()) {
      rpp_version <- c(
        "torch==2.2.0",
        "transformers==4.38.0",
        "huggingface_hub==0.20.0",
        "numpy==1.26.0",
        "pandas==2.0.3",
        "nltk==3.8.1",
        "scikit-learn==1.3.0", # higher versions 1.4 and 1.5 yield errors in textTopics() and warnings in textTrain
        "datasets==2.16.1",
        "evaluate==0.4.0",
        "accelerate==0.26.0",
        "bertopic==0.16.3",
        "jsonschema==4.19.2",
        "sentence-transformers==2.2.2",
        "flair==0.13.0",
        "umap-learn==0.5.6",
        "hdbscan==0.8.33",
        "scipy==1.10.1",
        "aiohappyeyeballs==2.4.4"
      )
    }
    if (is_windows()) {
      rpp_version <- c(
        "torch==2.2.0",
        "transformers==4.38.0",
        "huggingface_hub==0.20.0",
        "numpy==1.26.0",
        "pandas==2.0.3",
        "nltk==3.8.1",
        "scikit-learn==1.3.0",
        "datasets==2.16.1",
        "evaluate==0.4.0",
        "accelerate==0.26.0",
        "bertopic==0.16.3",
        "jsonschema==4.19.2",
        "sentence-transformers==2.2.2",
        "flair==0.13.0",
        "umap-learn==0.5.6",
        "hdbscan==0.8.33",
        "scipy==1.10.1",
        "aiohappyeyeballs==2.4.4"
      )
    }
  }

  if (python_version == "python_version_system_specific_defaults") {
    if (is_osx() || is_linux()) {
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

  # install rust for singularity machine -- but it gives error in github action
  # reticulate::py_run_string("import os\nos.system(\"curl --proto '=https' --tlsv1.2 -sSf
  # https://sh.rustup.rs | sh -s -- -y\")")
  #system("curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y")
  install_rust_if_needed(prompt = prompt)

  # resolve and look for conda help(conda_binary)
  conda <- tryCatch(reticulate::conda_binary(conda), error = function(e) NULL)
  have_conda <- !is.null(conda)

  # Mac and linux
  if (is_unix()) {
    # check for explicit conda method
    # validate that we have conda
    if (!have_conda) {
      message("No conda was found in the system. ")
      if (prompt) {
        ans <- utils::menu(c("No", "Yes"), title = "Do you want Text to download
                           miniconda using reticulate::install_miniconda()?")
      } else {
        ans <- 2 # When no prompt is set to false, default to install miniconda.
      }
      if (ans == 2) {
        reticulate::install_miniconda(update = update_conda)
        conda <- tryCatch(reticulate::conda_binary(conda), error = function(e) NULL)
        # Ensure using forge channels to avoid having to accept Terms of Service from Anaconda
        if(conda_forge) ensure_conda_forge(conda)
      } else {
        stop("Conda environment installation failed (no conda binary found)\n", call. = FALSE)
      }
    }

    # Update mini_conda
    if (update_conda && force_conda || force_conda) {
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
    if (have_conda && update_conda || have_conda && force_conda) {
      reticulate::install_miniconda(update = update_conda, force = force_conda)
    }
    # Ensure using forge channels to avoid having to accept Terms of Service from Anaconda
    if(conda_forge) ensure_conda_forge(conda)

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
    message(
      "Using existing conda environment ", envname, " for text installation\n.",
      "\ntext:",
      paste(rpp_version, collapse = ", "), "will be installed.  "
    )
  } else {
    message(
      "A new conda environment", paste0('"', envname, '"'), "will be created and \npython required packages:",
      paste(rpp_version, collapse = ", "), "will be installed.  "
    )
    message("Creating", envname, "conda environment for text installation...\n")
    python_packages <- ifelse(is.null(python_version), "python=3.9",
      sprintf("python=%s", python_version)
    )
    python <- reticulate::conda_create(envname, packages = python_packages, conda = conda)
  }

  message("Installing text required python packages...\n")
  packages <- rpp_version

  reticulate::conda_install(envname, packages, pip = pip, conda = conda)
}



process_textrpp_installation_virtualenv <- function(python = "/usr/local/bin/python3.9",
                                                    rpp_version,
                                                    pip_version,
                                                    envname = "textrpp_virtualenv",
                                                    prompt = TRUE) {
  libraries <- paste(rpp_version, collapse = ", ")
  message(sprintf(
    'A new virtual environment called "%s" will be created using "%s" \n and,
    the following text reuired python packages will be installed: \n "%s" \n \n',
    envname, python, libraries
  ))
  if (prompt) {
    ans <- utils::menu(c("No", "Yes"), title = "Proceed?")
    if (ans == 1) stop("Virtualenv setup is cancelled by user", call. = FALSE)
  }

  # Make python path help(virtualenv_create)
  reticulate::virtualenv_create(envname,
                                python,
                                pip_version = NULL,
                                required = TRUE)

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
textrpp_install_virtualenv <- function(rpp_version = c("torch==2.0.0",
                                                       "transformers==4.19.2",
                                                       "numpy",
                                                       "pandas",
                                                       "nltk"),
                                       python_path = NULL, # "/usr/local/bin/python3.9",
                                       pip_version = NULL,
                                       bin = "python3",
                                       envname = "textrpp_virtualenv",
                                       prompt = TRUE) {
  # find system python binary
  if (!is.null(python_path)) {
    python <- python_path
    } else {
      python <-  python_unix_binary(bin = bin)
    }


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
  message("A conda environment", envname, "will be removed\n")
  ans <- ifelse(prompt, utils::menu(c("No", "Yes"), title = "Proceed?"), 2)
  if (ans == 1) stop("condaenv removal is cancelled by user", call. = FALSE)
  python <- reticulate::conda_remove(envname = envname)

  message("\nUninstallation complete.\n\n")

  invisible(NULL)
}

###### see utils.R in spacyr
# checking OS functions, thanks to r-tensorflow;

#' Checking whether it is windows
#' @return TRUE if is is windows otherwise FALSE
#' @noRd
is_windows <- function() {
  identical(.Platform$OS.type, "windows")
}

#' Checking whether it is unix
#' @return TRUE if is is unix otherwise FALSE
#' @noRd
is_unix <- function() {
  identical(.Platform$OS.type, "unix")
}


#' Checking whether it is macOS
#' @return TRUE if is is macOS otherwise FALSE
#' @noRd
is_osx <- function() {
  Sys.info()["sysname"] == "Darwin"
}

#' Checking whether it is linux
#' @return TRUE if is is linux otherwise FALSE
#' @noRd
is_linux <- function() {
  identical(tolower(Sys.info()[["sysname"]]), "linux")
}

#is_ubuntu <- function() {
#  if (is_unix() && file.exists("/etc/lsb-release")) {
#    lsbrelease <- readLines("/etc/lsb-release")
#    any(grepl("Ubuntu", lsbrelease))
#  } else {
#    FALSE
#  }
#}

#python_version_function <- function(python) {
#  # check for the version
#  result <- system2(python, "--version", stdout = TRUE, stderr = TRUE)
#
#  # check for error
#  error_status <- attr(result, "status")
#  if (!is.null(error_status)) {
#    stop("Error ", error_status, " occurred while checking for python version", call. = FALSE)
#  }
#
#  # parse out the major and minor version numbers
#  matches <- regexec("^[^ ]+\\s+(\\d+)\\.(\\d+).*$", result)
#  matches <- regmatches(result, matches)[[1]]
#  if (length(matches) != 3) {
#    stop("Unable to parse Python version '", result[[1]], "'", call. = FALSE)
#  }
#
#  # return as R numeric version
#  numeric_version(paste(matches[[2]], matches[[3]], sep = "."))
#}

#pip_get_version <- function(cmd, major_version) {
#  regex <- "^(\\S+)\\s?(.*)$"
#  cmd1 <- sub(regex, "\\1", cmd)
#  cmd2 <- sub(regex, "\\2", cmd)
#  oldw <- getOption("warn")
#  options(warn = -1)
#  result <- paste(system2(cmd1, cmd2, stdout = TRUE, stderr = TRUE),
#    collapse = " "
#  )
#  options(warn = oldw)
#  version_check_regex <- sprintf(".+(%s.\\d+\\.\\d+).+", major_version)
#  return(sub(version_check_regex, "\\1", result))
#}


#conda_get_version <- function(major_version = NA, conda, envname) {
#  condaenv_bin <- function(bin) path.expand(file.path(dirname(conda), bin))
#  cmd <- sprintf(
#    "%s%s %s && conda search torch -c conda-forge%s",
#    ifelse(is_windows(), "", ifelse(is_osx(), "source ", "/bin/bash -c \"source ")),
#    shQuote(path.expand(condaenv_bin("activate"))),
#    envname,
#    ifelse(is_windows(), "", ifelse(is_osx(), "", "\""))
#  )
#  regex <- "^(\\S+)\\s?(.*)$"
#  cmd1 <- sub(regex, "\\1", cmd)
#  cmd2 <- sub(regex, "\\2", cmd)
#
#  result <- system2(cmd1, cmd2, stdout = TRUE, stderr = TRUE)
#  result <- sub("\\S+\\s+(\\S+)\\s.+", "\\1", result)
#  if (!is.na(major_version)) {
#    result <- grep(paste0("^", major_version, "\\."), result, value = TRUE)
#  }
#  #
#  return(result[length(result)])
#}


