
get_active_python_info <- function() {
  if (!reticulate::py_available(initialize = FALSE)) {
    return(list(
      initialized = FALSE,
      message = "Python environment is not initialized. Run `textrpp_initialize()`.",
      python_path = NULL,
      conda_env = NULL
    ))
  }

  config <- reticulate::py_config()
  python_path <- normalizePath(config$python, winslash = "/", mustWork = FALSE)

  conda_envs <- tryCatch(reticulate::conda_list(), error = function(e) NULL)

  matched_env <- NA
  if (!is.null(conda_envs)) {
    # Normalize for comparison
    conda_envs$python <- normalizePath(conda_envs$python, winslash = "/", mustWork = FALSE)
    match_idx <- which(conda_envs$python == python_path)

    if (length(match_idx) == 1) {
      matched_env <- conda_envs$name[match_idx]
    } else {
      # Fallback: match based on parent path structure
      prefix_dirs <- normalizePath(conda_envs$prefix, winslash = "/", mustWork = FALSE)
      match_idx2 <- which(dirname(python_path) == file.path(prefix_dirs, "bin"))
      if (length(match_idx2) == 1) {
        matched_env <- conda_envs$name[match_idx2]
      }
    }
  }

  list(
    initialized = TRUE,
    message = "Python environment is initialized.",
    python_path = python_path,
    conda_env = matched_env
  )
}


# Think about the output here; they should be useful and possible to inspect in github actions :)
# Could maybe have to sections in github actions.
# 1 after just install.packages(text)
# 2 after textrpp_install()
# 3 after textrpp_initialise()
# after textEmbed()'
# You know, first installing no dependencies... failing gracefully with information provided what is needed... and then do that in the next github action section and try again... failing graecfully.

check_macos_githubaction_dependencies <- function() {
  if (!is_osx()) return(invisible(NULL))

  brew_path <- Sys.which("brew")
  if (brew_path == "") {
    warning("Homebrew is not installed. Please install it from https://brew.sh/.")
    return(invisible(NULL))
  }

  # Check if libomp is installed
  libomp_installed <- system("brew list libomp", ignore.stdout = TRUE, ignore.stderr = TRUE) == 0
  if (!libomp_installed) {
    warning(
      "The 'libomp' library is not installed on your macOS system.\n",
      "This is required for optimal performance of some text functions (e.g., transformers/tokenizers).\n",
      "Install it using:\n  brew install libomp"
    )
  }

  # Optional: Check for qpdf if relevant
  qpdf_installed <- system("brew list qpdf", ignore.stdout = TRUE, ignore.stderr = TRUE) == 0
  if (!qpdf_installed) {
    message("Optional: You may install 'qpdf' with Homebrew for PDF processing: brew install qpdf")
  }

  invisible(NULL)
}

#macOS_deps <- check_macos_githubaction_dependencies()


check_linux_githubaction_dependencies <- function() {
  if (!is_linux()) return(invisible(NULL))

  missing <- c()

  # Define the system libraries you expect
  required_libs <- c(
    "libcurl4-openssl-dev",
    "libgit2-dev",
    "libssl-dev",
    "libharfbuzz-dev",
    "libfribidi-dev",
    "libxml2-dev",
    "libfreetype6-dev",
    "libpng-dev",
    "libtiff5-dev",
    "libjpeg-dev",
    "libfontconfig1-dev",
    "libicu-dev",
    "libcairo2-dev",
    "default-jdk"
  )

  # Check each one
  for (lib in required_libs) {
    status <- system2("dpkg", c("-s", lib), stdout = NULL, stderr = NULL)
    if (!identical(status, 0L)) missing <- c(missing, lib)
  }

  if (length(missing) > 0) {
    warning(
      "The following system libraries are missing from your Linux system and may be required for full functionality:\n",
      paste("  -", missing, collapse = "\n"), "\n\n",
      "Install them using (for Debian/Ubuntu):\n",
      "  sudo apt-get install -y ", paste(missing, collapse = " "), "\n"
    )
  }

  invisible(NULL)
}


#' Run diagnostics for the text package
#'
#' This function prints system and environment diagnostics useful for debugging or user support.
#'
# @param anonymise Logical; if TRUE, user paths are anonymised.
#' @param search_omp Logical; if TRUE, scans for OMP-related shared libraries.
#' @param include_other_envs Logical; if TRUE, lists other available Python/Conda environments.
#' @param full_session_info Logical; if TRUE, includes full sessionInfo() output.
#'
#' @return A named list with all diagnostic information (also printed with message()).
#' @export
textDiagnostics <- function(
   # anonymise = FALSE,
    include_other_envs = TRUE,
    search_omp = FALSE,
    full_session_info = FALSE) {

  diagnostics <- list()

  # Session info
  if (full_session_info) {
    diagnostics$session_info <- sessionInfo()
  }

  # Basic system info
  diagnostics$OS <- Sys.info()[["sysname"]]
#  diagnostics$User <- if (anonymise) "<user>" else Sys.info()[["user"]]
  diagnostics$R_version <- R.version.string

  # R package versions
  r_packages <- c("text", "topics", "reticulate")
  r_versions <- lapply(r_packages, function(pkg) {
    tryCatch(as.character(packageVersion(pkg)), error = function(e) NA)
  })
  names(r_versions) <- r_packages
  diagnostics$R_package_versions <- r_versions

  # Python info via reticulate
  if (reticulate::py_available(initialize = FALSE)) {
    diagnostics$py_config <- reticulate::py_config()
    #diagnostics$Python <- list(
    #  python = redact_path(py_config$python),
    #  libpython = redact_path(py_config$libpython),
    #  pythonhome = redact_path(py_config$pythonhome),
    #  version = py_config$version
    #)

    py_versions <- tryCatch({
      reticulate::py_run_string(
        "import importlib.metadata as m; versions = {pkg: m.version(pkg) for pkg in [
        'torch',
        'transformers',
        'huggingface_hub',
        'numpy',
        'pandas',
        'nltk',
        'scikit-learn',
        'datasets',
        'evaluate',
        'accelerate',
        'bertopic',
        'jsonschema',
        'sentence-transformers',
        'flair',
        'umap-learn',
        'hdbscan',
        'scipy'
        ]}"
      )
      reticulate::py$versions
    }, error = function(e) {
      warning("Could not retrieve Python package versions: ", e$message)
      NULL
    })

    diagnostics$Python_package_versions <- py_versions
  } else {
    diagnostics$Python <- "Python not available"
  }

  # OMP libraries
  if (search_omp) {
    omp_libs <- system("find / -name 'libomp*' 2>/dev/null", intern = TRUE)
    diagnostics$OMP_libraries_found <- if (length(omp_libs)) omp_libs else "None found"
  } else {
    diagnostics$OMP_libraries_found <- "Search disabled"
  }

  # Other environments
  if (include_other_envs && reticulate::py_available(initialize = FALSE)) {
    diagnostics$Other_Conda_Envs <- tryCatch(
      reticulate::conda_list(),
      error = function(e) "Unable to query conda environments"
    )
    diagnostics$Other_Python_Envs <- tryCatch(
      reticulate::py_discover_config(),
      error = function(e) "Unable to discover other Python environments"
    )
  }

  python_initialized <- reticulate::py_available(initialize = FALSE)

  if (!python_initialized) {
    message("Python environment is not initialized. Run `textrpp_initialize()`.")
  }

  diagnostics$python_initialized <- get_active_python_info()


  # Print summary (not full)
  message("\n--- textDiagnostics Summary ---")
  message("OS:             ", diagnostics$OS)
#  message("User:           ", diagnostics$User)
  message("R Version:      ", diagnostics$R_version)
  message("text version:   ", diagnostics$R_package_versions$text)
  if (!is.null(diagnostics$py_config))
    message("Python version: ", diagnostics$py_config)
  if (!is.null(diagnostics$py_config)) {
    message("Python packages:")
    pkgs <- diagnostics$Python_package_versions
    for (pkg in names(pkgs)) message("  ", pkg, ": ", pkgs[[pkg]])
  }

  message("\nTo see more details, examine the returned object.")
  invisible(diagnostics)
}




#check_user_install_permissions <- function() {
#  message("Checking user installation permissions...\n")
#
#  # Check 1: Can user install R packages?
#  r_lib <- Sys.getenv("R_LIBS_USER")
#  if (r_lib == "") {
#    r_lib <- .libPaths()[1]
#  }
#
#  can_write_r_lib <- tryCatch({
#    test_file <- file.path(r_lib, paste0(".__write_test__", Sys.getpid()))
#    dir.create(r_lib, showWarnings = FALSE, recursive = TRUE)
#    file.create(test_file)
#    file.remove(test_file)
#    TRUE
#  }, error = function(e) FALSE)
#
#  if (can_write_r_lib) {
#    message("You appear to have permission to install R packages (user library: ", r_lib, ").")
#  } else {
#    warning("You do NOT have permission to install R packages to your user library.\n",
#            "You may need to run R with elevated privileges or request access.\n",
#            "R_LIBS_USER: ", r_lib)
#  }
#
#  # Check 2: Can user install system-level dependencies (e.g., python, conda)?
#  has_write_access_system_dirs <- function(paths) {
#    for (p in paths) {
#      tryCatch({
#        test_file <- file.path(p, paste0(".__write_test__", Sys.getpid()))
#        if (dir.exists(p) && file.create(test_file)) {
#          file.remove(test_file)
#          return(TRUE)
#        }
#      }, error = function(e) {})
#    }
#    return(FALSE)
#  }
#
#  # Candidate system locations (depending on OS)
#  system_paths <- if (.Platform$OS.type == "windows") {
#    c("C:/Program Files", "C:/Program Files (x86)")
#  } else {
#    c("/usr/local", "/opt", "/usr/bin", "/Library")
#  }
#
#  can_install_system <- has_write_access_system_dirs(system_paths)
#
#  if (can_install_system) {
#    message("You appear to have write access to system locations (needed for tools like Python or Conda).")
#  } else {
#    warning("You do NOT have write access to standard system locations.\n",
#            "You may not be able to install Python/Conda or system packages without admin rights.\n",
#            "If needed, install into a user-owned location or contact your system administrator.")
#  }
#
#  invisible(list(
#    can_write_r_lib = can_write_r_lib,
#    can_install_system = can_install_system,
#    r_lib_path = r_lib
#  ))
#}
