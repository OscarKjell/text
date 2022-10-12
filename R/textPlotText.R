#### textProjectText ####
# for interative plots
# https://shiny.rstudio.com/articles/plot-interaction.html

# reticulate::py_install("nltk", envname)
# nltk <- reticulate::import("nltk")

#### private func ####

#' This is to get the tokenizer from the python package "transformers"
#' @param modelName The pre-trained model name in the transformers hub.
#' @importFrom reticulate import
#' @return The RObject of the tokenizer.
#' @noRd
getTokenizer <- function(modelName){
    transformerPack <- reticulate::import("transformers")
    autoTokenizers <- transformerPack$AutoTokenizer
    return (autoTokenizers$from_pretrained(modelName))
}

#' Transform the subword tokens back to words.
#' @param aString An input string list.
#' @param tokenizers A tokenizer model from getTokenizer.
#' @return The transformed string.
#' @noRd
decodeToken <- function(aStringList, tokenizerInUse){
    ids <- aStringList %>% tokenizerInUse$convert_tokens_to_ids()
    return (ids %>% tokenizerInUse$decode())
}

#' Split a text into sentences.
#' @param string The input string.
#' @importFrom reticulate import
#' @return A list of sentences.
#' @NoRd
# splitSent <- function(string){
#     nltk <- import("nltk")
#     return (nltk$tokenize$sent_tokenize(string) %>% reticulate::py_to_r())
# }

#' Calculate the row number of the tokens "[CLS]" and "[SEP]".
#' @param aTibble The input tibble of token embeddings.
#' @return A tibble of row number of tokens.
#' @NoRd
token2Sent_rowCLSSEP <- function(aTibble){
    rowCLSSEP <- which(aTibble[["tokens"]] == "[CLS]", arr.ind=TRUE) %>% tibble::as_tibble()
    rowCLSSEP_ <- which(aTibble[["tokens"]] == "[SEP]", arr.ind=TRUE) %>% tibble::as_tibble()
    rowCLSSEP <- cbind(rowCLSSEP, rowCLSSEP_)
    names(rowCLSSEP) <- c("CLS", "SEP")
    return (rowCLSSEP)
}
#' Create a new sentence tibble in line with the token tibble.
#' @param aTibble The input tibble of token embeddings.
#' @return A new sentence tibble.
#' @NoRd
token2Sent_newSentTb <- function(aTibble){
    newSentTb <- aTibble %>% token2Sent_rowCLSSEP()
    sentCol <- matrix(nrow=newSentTb %>% nrow(), ncol=aTibble %>% ncol()) %>%
     as.data.frame() %>% tibble::as_tibble()
    sentCol[1:nrow(newSentTb), 1] <- "new"
    sentCol[1:nrow(newSentTb), 2:ncol(aTibble)] <- 0.0
    names(sentCol)[1] <- c("sentences")
    names(sentCol)[2:ncol(aTibble)] <- names(aTibble)[2:ncol(aTibble)]
    newSentTb <- cbind(newSentTb, sentCol)
    return (newSentTb)
}
#' Transform the tokens into a sentence cell in a single cell.
#' @param newSentence The target sentence to transform and the output as well.
#' @param rowCLS The row number of the token "[CLS]".
#' @param rowSEP The row number of the token "[SEP]".
#' @param aTibble The input tibble of token embeddings.
#' @param tokenizers The tokenizer used.
#' @param include_CLS_SEP To include the embeddings of "CLS", "SEP", "both", or "none".
#' @return A sentence tibble.
#' @NoRd
token2Sent_getSentRowWise <- function(newSentence, rowCLS, 
                                      rowSEP, aTibble, 
                                      tokenizers, include_CLS_SEP){
    if (include_CLS_SEP == "CLS"){
        rowSEP <- rowSEP - 1
    }else if(include_CLS_SEP == "SEP"){
        rowCLS <- rowCLS + 1
    }else if(include_CLS_SEP == "none"){
        rowCLS <- rowCLS + 1
        rowSEP <- rowSEP - 1
    }else{include_CLS_SEP <- "both"}
    newSentence <- decodeToken(aTibble[["tokens"]][rowCLS:rowSEP], tokenizers)
    return (newSentence)
}
#' Average the embedding values across tokens.
#' @param newEmbed The input embedding value vector.
#' @param rowCLS The row number of "[CLS]".
#' @param rowSEP The row number of "[SEP]".
#' @param aTibble The input tibble of token embeddings.
#' @param include_CLS_SEP To include the embeddings of "CLS", "SEP", "both", or "none".
#' @return An integer of the number of sentences.
#' @NoRd
token2Sent_avgSentTokenRowWise <- function(newEmbed, rowCLS,
                                           rowSEP, aTibble,
                                           include_CLS_SEP){
    if (include_CLS_SEP == "CLS"){
        rowSEP <- rowSEP - 1
    }else if(include_CLS_SEP == "SEP"){
        rowCLS <- rowCLS + 1
    }else if(include_CLS_SEP == "none"){
        rowCLS <- rowCLS + 1
        rowSEP <- rowSEP - 1
    }else{include_CLS_SEP <- "both"}
    newEmbed <- aTibble[rowCLS:rowSEP, 2:ncol(aTibble)] %>% 
        as.matrix() %>% colMeans() %>%
        t() %>% tibble::as_tibble() 
    names(newEmbed) <- names(aTibble)[2:ncol(aTibble)]
    return (newEmbed)
}
# TODO: Preserve capital letters.
#' Transform the tokens into sentence cells.
#' @param aTibble The input tibble of token embeddings.
#' @param tokenizers The tokenizer used.
#' @param include_CLS_SEP To include the embeddings of "CLS", "SEP", or "both".
#' @importFrom furrr pmap
#' @importFrom furrr pmap_dfr # future for paralell
#' @importFrom future plan
#' @importFrom future cluster
#' @importFrom future future
#' @importForm future value
#' @return A sentence tibble.
#' @NoRd
token2Sent_getSent <- function(aTibble, tokenizers, include_CLS_SEP="both"){

    # future::plan(future::cluster)
    
    newSentTb <- token2Sent_newSentTb(aTibble)
    getSent_str <- future::future({
        furrr::future_pmap(
        list(
            newSentTb[["sentences"]] %>% as.vector(),
            newSentTb[["CLS"]] %>% as.vector(),
            newSentTb[["SEP"]] %>% as.vector(),
            aTibble %>% list(),
            tokenizers %>% list(),
            include_CLS_SEP %>% list()
        ),
        token2Sent_getSentRowWise
        )
    },seed=TRUE)
    getSent_embed <- future::future({
        furrr::future_pmap_dfr(
        list(
            newSentTb[,4:ncol(newSentTb)] %>% as.data.frame() %>% asplit(1),
            newSentTb[["CLS"]] %>% as.vector(),
            newSentTb[["SEP"]] %>% as.vector(),
            aTibble %>% list(),
            include_CLS_SEP %>% list()
        ),
        token2Sent_avgSentTokenRowWise
        ) 
    },seed=TRUE)
    newSentTb[["sentences"]] <- future::value(getSent_str)
    getSent_embed_ <- future::value(getSent_embed)
    newSentTb <- cbind(newSentTb[,1:3], getSent_embed_)
    return (newSentTb)
}
# TODO: continue to code here. Use func token2Sent_getSent to convert the token lists from textEmbed.
#' Get the tibble of sentence embeddings.
#' @param texts_embeddings The input tibble of token embeddings.
#' @return An integer of the number of sentences.
#' @NoRd
token2Sent <- function(texts_embeddings){

    rowNum <- length(texts_embeddings$tokens$texts)
    # for (rowNo in rowNum %>% seq_len){
        # idCLSSEP <- texts_embeddings[["tokens"]][["texts"]][rowNo] %>% 
        # texts_embeddings[["sentences"]][["texts"]][rowNo]
    #}
    # temp_[["sentences"]][["texts"]][1] <- list("a")
    # temp_[["sentences"]][2] <- list
    # texts_embeddings

    return (0)
}

#### Basic function ####

#' Function to calculate the highlight color value.
#' @param textsProj A character variable or a tibble/dataframe with at least one character variable.
#' @param texts_embeddings Embedding values from text::textEmbed()
#' @param x Numeric variable that the words should be plotted according to on the x-axes.
#' @param y Numeric variable that the words should be plotted according to on the y-axes (y=NULL).
#' @param model Character string specifying pre-trained language model (default 'bert-base-uncased'). For now it only supports the default. For full list of options see pretrained models at HuggingFace. For example use "bert-base-multilingual-cased", "openai-gpt", "gpt2", "ctrl", "transfo-xl-wt103", "xlnet-base-cased", "xlm-mlm-enfr-1024", "distilbert-base-cased", "roberta-base", or "xlm-roberta-base".
#' @param include_CLS_SEP Averaging of the sentence embedding should contain the mark of the sentence START "cls", the sentence END "sep", or "both". Current default (supported) is "both".
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
    textsProj,
    texts_embeddings = texts_embeddings_from_textEmbed,
    x,
    y = NULL,
    model = "bert-base-uncased",
    include_CLS_SEP = "both",
    lang_level = "all",
    projection_method = "ridge",
    device = "cpu",
    tokenizer_parallelism = FALSE,
    logging_level = "error"
){
    # Check the format of the input
    if (TRUE){
        textIsStr <- FALSE
        textIsDF <- FALSE
        textIsTb <- FALSE
        if (textsProj %>% is.character()){textsIsStr <- TRUE}else{textsIsStr <- FALSE}
        if (textsProj %>% is.data.frame()){textsIsDF <- TRUE}else{textsIsDF <- FALSE}
        if (textsProj %>% is_tibble()){textsIsTb <- TRUE}else{textsIsTb <- FALSE}
        if (model %>% is.character()){modelName <- model}else{modelName <- "bert_base_uncased"}
        tokenizers <- modelName %>% getTokenizer()
    }

    # if texts == str
    if (textsIsStr){
        # textsProj to textEmbed
        textEmbedding <- textsProj %>% textEmbed(model = modelName)
        # aaa <- textEmbed(Language_based_assessment_data_8$harmonytexts)
        # see aaa[["tokens"]][["texts"]][[5]]$tokens[62:120]
        # TODO: use contain and decode to transform, based on [CLS]
        # contains one row of tokens with in sentences
        temp <- textEmbedding[["tokens"]][["texts"]][[1]]$`tokens` %>%
            decodeToken(model = tokenizers)
        # After the calling of the function "decode"
    }

    # if texts == DF or Tb
    if (textsIsDF | textsIsTb){
        if (textsIsDF){texts <- texts %>% as_tibble()}
        NULL
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

