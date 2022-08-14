#' Check downloaded, available models.
#' @return List of names of models and tokenizers
#' @examples
#' \dontrun{
#' textModels()
#' }
#' @seealso see \code{\link{textModelsRemove}}
#' @export
textModels <- function() {
  reticulate::source_python(system.file("python",
    "textModelPy.py",
    # envir = NULL,
    package = "text",
    mustWork = TRUE
  ))

  models <- textModelsPy()

  names(models) <- c("Downloaded_models", "Downloaded_tokenizers")

  return(models)
}


#' Delete a specified model and model associated files.
#' @param target_model Character string of the model name that you want to delete.
#' @return Confirmation whether the model has been deleted.
#' @examples
#' \dontrun{
#' textModelsRemove("name-of-model-to-delete")
#' }
#' @seealso see \code{\link{textModels}}
#' @export
textModelsRemove <- function(target_model) {
  reticulate::source_python(system.file("python",
    "textModelPy.py",
    # envir = NULL,
    package = "text",
    mustWork = TRUE
  ))

  # Check that target_models exist among existing models
  models <- textModels()
  if (target_model %in% models$Downloaded_models == FALSE) {
    stop(paste("Model name error. ", target_model, " was not found, and could not be deleted", sep = ""))
  }

  # Delete the model
  textModelsRMPy(target_model)

  message(colourise(
    paste(target_model, " was successfully deleted.", sep = ""),
    fg = "green", bg = NULL
  ))
}
