
#' Print diagnostic information for the text package environment
#'
#' @param anonymise logical; if TRUE, personal paths and usernames are redacted before displaying output (default is FALSE).
#' @param search_other_envs search different different python environments
#' @param search_for_omp search for OMP related packages (most relevant for MacOS, that can get conflicts).
#' @export
textDiagnostics <- function(
    anonymise = FALSE,
    search_other_envs = FALSE,
    search_omp = TRUE) {

  redact_path <- function(x) {
    if (!anonymise) return(x)
    if (is.null(x)) return(NULL)
    gsub(Sys.getenv("HOME"), "<home>", x)
  }

  get_paths <- function(pattern, dirs) {
    all_paths <- unlist(lapply(dirs, function(d) {
      if (dir.exists(d)) {
        list.files(d, pattern = pattern, recursive = TRUE, full.names = TRUE)
      } else {
        character(0)
      }
    }))
    all_paths[!dir.exists(all_paths)]
  }

  get_executables <- function(bin_name) {
    paths <- unlist(strsplit(Sys.getenv("PATH"), .Platform$path.sep))
    found <- character(0)
    for (dir in paths) {
      path <- file.path(dir, bin_name)
      if (file.exists(path) && file.access(path, 1) == 0) {
        found <- c(found, normalizePath(path))
      }
    }
    unique(found)
  }

  message_section <- function(title, content) {
    message("\n", title, ":\n", paste(content, collapse = "\n"))
  }

  # Collect system information
  os <- Sys.info()["sysname"]
  retic_env <- Sys.getenv("RETICULATE_PYTHON", unset = NA)
  py_config <- tryCatch(reticulate::py_config(), error = function(e) NULL)
  conda_envs <- tryCatch(reticulate::conda_list(), error = function(e) NULL)
  conda_bin <- tryCatch(reticulate::conda_binary(), error = function(e) NULL)
  conda_version <- tryCatch(system2(conda_bin, "--version", stdout = TRUE), error = function(e) NULL)

  omp_libs <- if (search_omp) {
    get_paths("libomp", c("/usr/local/lib", "/opt/homebrew/lib", "/usr/lib", Sys.getenv("HOME")))
  } else {
    character(0)
  }

  other_pythons <- if (search_other_envs) get_executables("python3") else character(0)
  other_condas <- if (search_other_envs) get_executables("conda") else character(0)

  diagnostics <- list(
    os = os,
    python_forced_by_env = if (!is.na(retic_env)) redact_path(retic_env) else NULL,
    python_version = if (!is.null(py_config)) py_config$version else NULL,
    python_path = if (!is.null(py_config)) redact_path(py_config$python) else NULL,
    libpython = if (!is.null(py_config)) redact_path(py_config$libpython) else NULL,
    numpy = if (!is.null(py_config)) redact_path(py_config$numpy) else NULL,
    numpy_version = if (!is.null(py_config)) py_config$numpy_version else NULL,
    conda_path = redact_path(conda_bin),
    conda_version = conda_version,
    conda_envs = if (!is.null(conda_envs)) conda_envs$name else NULL,
    omp_libraries = redact_path(omp_libs),
    other_python_executables = redact_path(other_pythons),
    other_conda_executables = redact_path(other_condas)
  )

  # Print summary
  message("==== textDiagnostics() ====")
  message("OS: ", diagnostics$os)

  if (!is.null(diagnostics$python_forced_by_env)) {
    message("NOTE: Python version was forced by RETICULATE_PYTHON")
    message("RETICULATE_PYTHON: ", diagnostics$python_forced_by_env)
  }

  if (!is.null(diagnostics$python_version)) {
    message("Python version: ", diagnostics$python_version)
    message("Python path: ", diagnostics$python_path)
    message("libpython: ", diagnostics$libpython)
    message("numpy version: ", diagnostics$numpy_version)
    message("numpy path: ", diagnostics$numpy)
  } else {
    message("Python not configured.")
  }

  message("Conda binary: ", diagnostics$conda_path)
  if (!is.null(diagnostics$conda_version)) {
    message("Conda version: ", diagnostics$conda_version)
  }

  if (!is.null(diagnostics$conda_envs)) {
    message("Conda environments:")
    for (env in diagnostics$conda_envs) {
      message("  - ", env)
    }
  }

  if (search_omp && length(diagnostics$omp_libraries) > 0) {
    message_section("OMP libraries found", diagnostics$omp_libraries)
  }

  if (search_other_envs) {
    if (length(diagnostics$other_python_executables) > 0) {
      message_section("Other Python executables found", diagnostics$other_python_executables)
    }
    if (length(diagnostics$other_conda_executables) > 0) {
      message_section("Other Conda executables found", diagnostics$other_conda_executables)
    }
  }

  invisible(diagnostics)
}
