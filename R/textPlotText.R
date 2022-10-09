#### textProjectText ####
# for interative plots
# https://shiny.rstudio.com/articles/plot-interaction.html

#' Function to calculate the highlight color value.
#' @param texts A character variable or a tibble/dataframe with at least one character variable.
#' @param model Character string specifying pre-trained language model (default 'bert-base-uncased'). For full list of options see pretrained models at HuggingFace. For example use "bert-base-multilingual-cased", "openai-gpt", "gpt2", "ctrl", "transfo-xl-wt103", "xlnet-base-cased", "xlm-mlm-enfr-1024", "distilbert-base-cased", "roberta-base", or "xlm-roberta-base".
#' @param lang_level Set the language level in the output of predictions. The defaut value is "all".
#' @param projection_method Set the projection method. Can be either "ridge" or "dot_product." The defaulf value is "ridge". "dot_product" is not yet supported.
#' @param device param yet not supported.
#' @param tokenzier_parallelism param yet not supported.
#' @param logging_level param yet not supported.
#' @importFrom magrittr %>%
#' @importFrom reticulate import
#' @importFrom tibble as_tibble is_tibble
#' @importFrom purrr map
#' @importFrom furrr future_map
#' @return List of names of models and tibbles.
#' @examples
#' \dontrun{
#' textProjectionText()
#' }
#' @seealso see \code{\link{textProjection}} and \code{\link{textWordPrediction}}
#' @export
textProjectionText <- function(
    texts,
    model = "bert-base-uncased",
    lang_level = "all",
    projection_method = "ridge",
    device = "cpu",
    tokenizer_parallelism = FALSE,
    logging_level = "error"
){
    # Check the format of the input
    if (TRUE){
        if (texts %>% is.character()){textsIsStr <- TRUE}else{textsIsStr <- FALSE}
        if (texts %>% is.data.frame()){textsIsDF <- TRUE}else{textsIsDF <- FALSE}
        if (texts %>% is_tibble()){textsIsTb <- TRUE}else{textsIsTb <- FALSE}
    }
    
}

#### textPlotText ####

#' Function to plot the highlight color value into semantic cloud.
#' @param RObj_model The model output from the function "textPlotText".
#' @return The RObject of the plot
#' @examples
#' \dontrun{
#' textPlotText()
#' }
#' @seealso see \code{\link{textPlotText}}
#' @export
textPlotText <- function(RObj_model){
    print("Hi again!")
}
