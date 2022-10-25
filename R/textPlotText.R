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
#' @param tokenizersInUse A tokenizer model from getTokenizer.
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
#' @param include_CLS_SEP To include the embeddings of "CLS", "SEP", "both", or "none".
#' @importFrom furrr pmap
#' @importFrom furrr pmap_dfr
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
    },seed=TRUE) # TRUE or NULL?
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
    },seed=TRUE) # TRUE or NULL?
    newSentTb[["sentences"]] <- future::value(getSent_str)
    getSent_embed_ <- future::value(getSent_embed)
    newSentTb <- cbind(newSentTb[,1:3], getSent_embed_)
    return (newSentTb)
}
#' Remove ID numbers of CLS and SEP
#' @param aTibble The output from token2Sent_getSent.
#' @return textEmbeds without ID number columns.
#' @NoRd 
removeColCLSSEP <- function(aTibble){
    return (aTibble[, 3:ncol(aTibble)])
}
#' Get the tibble of sentence embeddings.
#' @param textEmbeds The input tibble of token embeddings.
#' @param tokenizers The tokenizer used in function textEmbed to get token embeddings.
#' @param include_CLS_SEP To include the embeddings of "CLS", "SEP", "both", or "none".
#' @return A list containing token embeddings of the function textEmbed() along with sentence embeddings.
#' @NoRd
token2Sent <- function(textEmbeds, tokenizers, include_CLS_SEP="both"){

    tokenSent <- furrr::future_pmap(
        list(
            textEmbeds[["tokens"]][["texts"]] %>% as.vector(),
            tokenizers %>% list(),
            include_CLS_SEP %>% list()
        ),
        token2Sent_getSent
    )
    tokenSent <- furrr::future_pmap(
        list(
            tokenSent %>% as.vector()
        ),
        removeColCLSSEP
    )
    for (i in length(tokenSent) %>% seq_len()){
        if (i == 1){
            tokenSent[[1]] <- tokenSent[[1]] %>% tibble::as_tibble()
            sentenceEmbed <- tokenSent[[1]]
            next
        }
        tokenSent[[i]] <- tokenSent[[i]] %>% tibble::as_tibble()
        sentenceEmbed <- rbind(sentenceEmbed, tokenSent[[i]])
    }
    sentenceEmbed <- sentenceEmbed %>% tibble::as_tibble()
    textEmbeds <- append(textEmbeds,
                         list("sentences" = list(
                            "texts"=tokenSent, "sentence_tibble"=sentenceEmbed)
                         ), 1
    )

    return (textEmbeds)
}
#' Get the predition tibble rowwise
#' @param rowTb Each row of the original list
#' @param yValue Row value of the prediction target
#' @return A row ready for prediction
#' @NoRd
getPredictionTb_row <- function(rowTb, yValue){
    return (cbind(yValue, rowTb))
}
#' Get the tibble of sentence embeddings and their prediction targets.
#' @param PredictTb The input from token2Sent.
#' @param y Numeric variable to predict.
#' @importFrom furrr pmap_dfr
#' @return something.
#' @NoRd
getPredictTb <- function(PredictTb, y){

    PredTb <- furrr::future_pmap_dfr(
        list(PredictTb, y),
        getPredictionTb_row
    )
    return (PredTb)
}


#### Basic function ####

#' Function to calculate the highlight color value.
#' @param textsObj A character variable or a tibble/dataframe with at least one character variable.
#' @param embeds_from_textEmbed Embedding values from text::textEmbed()
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
    textsPred,
    embeds_from_textEmbed,
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

    '''
        # snowboarding would be tokenized into "snow" & "##boarding".
        textsPred <- "I wish I could have a nice dream snowboarding. Today it is quite good. Tomorrow would be even better."
    '''

    #### 1. Check the format of the input
    if (TRUE){
        textIsStr <- FALSE
        textIsDF <- FALSE
        textIsTb <- FALSE
        if (textsPred %>% is.character()){textsIsStr <- TRUE}else{textsIsStr <- FALSE}
        if (textsPred %>% is.data.frame()){textsIsDF <- TRUE}else{textsIsDF <- FALSE}
        if (textsPred %>% is_tibble()){textsIsTb <- TRUE}else{textsIsTb <- FALSE}
        if (model %>% is.character()){modelName <- model}else{modelName <- "bert-base-uncased"}
        #if (x %>% is_tibble()){return -1 %>% as.numeric()}
        tokenizers <- modelName %>% getTokenizer()
    }

    #### 2. Get the embedding of the input to predict.
    # if texts == str
    if (textsIsStr){
        # textsProj to textEmbed
        sentsPred <- textsPred %>% textEmbed(modelName)
        sentsPred <- token2Sent(sentsPred, tokenizers, include_CLS_SEP)
        # contains one row of tokens with in sentences
        # embeds_from_textEmbed <- textEmbed(Language_based_assessment_data_8$harmonytexts)
        sentsTrain <- token2Sent(embeds_from_textEmbed, tokenizers, include_CLS_SEP)
    }

    # if texts == DF or Tb
    if (textsIsDF | textsIsTb){
        if (textsIsDF){texts <- texts %>% as_tibble()}
        NULL
    }
    
    #### 3. Train the model using given training data.
    sentsTrainTb <- getPredictTb(sentsTrain[["sentences"]][["texts"]], 
                         y # Language_based_assessment_data_8$hilstotal
    ) %>% tibble::as_tibble()
    results <- textTrainRegression(
        x = sentsTrainTb[,3:ncol(sentsTrainTb)],
        y = sentsTrainTb[,1]) 
    
    '''
    TODO: continue code
    aaa <- textEmbed(Language_based_assessment_data_8$harmonytexts)
    sentsInput <- token2Sent(aaa)
    '''
    #### 4. Use trained model to predict the input
    # textPredict https://r-text.org/reference/textPredict.html
    outputValue <- textPredict(results, 
        sentsPred[["sentences"]][["sentence_tibble"]][, 2:ncol(sentsPred[["sentences"]][["sentence_tibble"]])])
    names(outputValue) <- c("y_pred")

    return (outputValue)

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

