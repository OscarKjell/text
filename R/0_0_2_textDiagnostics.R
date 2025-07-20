
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

#  redact_path <- function(x) {
#    if (anonymise && is.character(x)) {
#      x <- gsub(Sys.getenv("HOME"), "~", x, fixed = TRUE)
#      x <- gsub(Sys.info()[["user"]], "<user>", x, fixed = TRUE)
#    }
#    x
#  }

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
