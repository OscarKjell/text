# refactor based on the online doc
# Using inline js pack to run via htmltools: https://rstudio.github.io/htmltools/reference/htmlDependency.html

#### private func ####

#' Sentence marker rowwise
#' @param theStr (string) The string to be marked by full stop transformer model.
#' @importFrom reticulate source_python
#' @return A list of sentences
#' @noRd
sentsMarker_rowWise <- function(theStr){

    output <- fullstopCorrt(theStr)

    return (output)
}
#' Sentence marker rowwise
#' @param aTibble (tibble) The tibble contains text lists to be splitted into sentences.
#' @importFrom furrr future_pmap 
#' @return A tibble of the list of sentences
#' @noRd
sentsMarker <- function(aTibble){

    if (FALSE){
        reticulate::source_python(system.file("python",
        "fullStopCorrt.py",
        #envir = "",
        package = "text",
        convert = TRUE,
        mustWork = TRUE
        ))
    }

    if (!aTibble %>% is.null(.)){
        output <- furrr::future_pmap(
              list(
                aTibble %>% as.vector()
              ),sentsMarker_rowWise
        )
        
    }else{
        print("The input tibble is NULL!")
        output <- NULL
    }

    if (!is.null(output)){
        output <- output %>% as.data.frame(.) %>%
         t(.) %>% tibble::as_tibble(.)
        return (output)
    }else{
        return (NULL)
    }
}

#' Num to color
#' @param values (list) The list of sorted value numbers.
#' @param mode (boolean) diverging mode. The default is TRUE.
#' @param pal (R_obj) The returned obj of rainbow(), only available if mode = FALSE.
#' @param limits (boolean) The limits of x, like c(min, max), only available if mode = FALSE.
#' @importFrom colorspace diverging_hcl
#' @imporFrom grDevices rainbow
#' @return A list object containing color value codes.
#' @noRd
map2Color <- function(values, mode=TRUE, pal=grDevices::rainbow(200),limits=NULL){
    
    if(mode){
        # Prevent the neutral color to be blank.
        # colorValues <- colorspace::diverging_hcl(n=length(values), palette="Tropic", power=2.5)
        colorValues <- colorspace::divergingx_hcl(n=length(values), palette="Temps")
    }else{
        if(is.null(limits)) limits <- range(values) %>% as.integer()
        values <- values %>% as.matrix()
        colorValues <- pal[findInterval(values,seq(limits[1],limits[2],length.out=length(pal)+1), all.inside=TRUE)]
    }

    return (as.data.frame(colorValues))
    

    if (FALSE){
        # color containing info
        # https://colorspace.r-forge.r-project.org/articles/color_spaces.html
        # https://colorspace.r-forge.r-project.org/articles/hcl_palettes.html
        # https://colorspace.r-forge.r-project.org/articles/palette_visualization.html
        # https://blog.r-project.org/2019/04/01/hcl-based-color-palettes-in-grdevices/
        values <- colorspace::diverging_hcl(n=3, palette="Tropic", power=2.5)
        temp[["Pred"]] <- cbind(tibble::as_tibble(values), temp[["Pred"]])
        names(temp[["Pred"]])[1] <- "color"
    }
    if (FALSE){
        # random color gradients
        # https://astrostatistics.psu.edu/su07/R/html/grDevices/html/palettes.html
        temp <- samplePlot
        values <- map2color(as.matrix(temp[["Pred"]]["y_pred"]),grDevices::rainbow(200))
        temp[["Pred"]] <- cbind(tibble::as_tibble(values), temp[["Pred"]])
        names(temp[["Pred"]])[1] <- "color"
    }  
    if (FALSE){
        # https://stackoverflow.com/questions/15006211/how-do-i-generate-a-mapping-from-numbers-to-colors-in-r
        # check line 661 in 4_0_textPlot.R, line 1088 ; Preset, not useful
        pal[findInterval(values,seq(limits[1],limits[2],length.out=length(pal)+1), all.inside=TRUE)]
    }   
}

#' This is to get the tokenizer from the python package "transformers"
#' @param modelName (str) The pre-trained model name in the transformers hub.
#' @importFrom reticulate import
#' @return The RObject of the tokenizer.
#' @noRd
getTokenizer <- function(modelName){

    transformerPack <- reticulate::import("transformers")
    autoTokenizers <- transformerPack$AutoTokenizer
    output <- autoTokenizers$from_pretrained(modelName)
    transformerPack <- NULL
    autoTokenizers <- NULL
    
    return (output)
}


#' Transform the subword tokens back to words.
#' Cannot change the name due to "tokenizers" is a ptr.
#' @param aString An input string list.
#' @param tokenizers A tokenizer model from getTokenizer.
#' @param modelName The name of the used tokenizer.
#' @return The transformed string.
#' @noRd
decodeToken <- function(aStringList, tokenizers, modelName){
    if (reticulate::py_has_attr(tokenizers, "convert_tokens_to_ids")){
        ids <- aStringList %>% tokenizers$convert_tokens_to_ids()
    }else{
        tokenizers <- getTokenizer(modelName)
        ids <- aStringList %>% tokenizers$convert_tokens_to_ids()
    }
    if (reticulate::py_has_attr(tokenizers, "decode")){
        output <- ids %>% tokenizers$decode()
    }else{
        tokenizers <- getTokenizer(modelName)
        output <- ids %>% tokenizers$decode()
    }
    
    return (output)
}

#' Experimental:: Split a text into sentences.
#' @param string The input string.
#' @importFrom reticulate import
#' @return A list of sentences.
#' @NoRd
# splitSent <- function(string){
#     nltk <- import("nltk")
#     return (nltk$tokenize$sent_tokenize(string) %>% reticulate::py_to_r())
# }

#' Grammar correction to form a complete sentence for easier textViz rowwise.
#' @param aString (string) The input string
#' @param corrector (R_obj) The corrector from transformer
#' @return A string after grammar correction
#' @noRd
correctGram_rowwise <- function(aString = NULL, corrector){
    #  && aString %>% is.character(.)
    if (!aString %>% is.null(.)){
        output <- corrector(aString)[[1]][[1]]
    }else{
        print("The string input is not available! Pls check the data!")
        return ("NA")
    }

    return (output)
}
#' Experimental:: Grammar correction to form a complete sentence for easier textViz.
#' @param aTibble (tibble) The input tibble containing only multiple strings, the first column.
#' @importFrom reticulate import py_module_available
#' @importFrom furrr future_pmap
#' @importFrom tibble as_tibble
#' @return A tibble after string correction.
#' @noRd
correctGram <- function(aTibble=NULL){
    # 'pszemraj/grammar-synthesis-small' # correct minor symbolic issues
    # "pszemraj/t5-v1_1-base-ft-jflAUG"  # correct major semantic issues
    corModel1 <- 'pszemraj/grammar-synthesis-small' 
    corModel2 <- "pszemraj/t5-v1_1-base-ft-jflAUG"

    if (!reticulate::py_module_available("transformers")){
        reticulate::py_install("transformers")
    }
    if (!is.null(aTibble)){
        transformers_obj <- reticulate::import("transformers")
        pipeline_obj <- transformers_obj$pipeline
        if (TRUE){
            corrector1 <- pipeline_obj('text2text-generation',
              corModel1)
            transformers_obj <- NULL
            pipeline_obj <- NULL
            output <- furrr::future_pmap(
              list(
                aTibble %>% as.vector(),
                corrector1 %>% list()
              ),correctGram_rowwise
            )
        }
        if (FALSE){
            corrector2 <- pipeline_obj('text2text-generation',
              corModel2)
            transformers_obj <- NULL
            pipeline_obj <- NULL
            output <- furrr::future_pmap(
              list(
                aTibble %>% as.vector(),
                corrector2 %>% list()
              ),correctGram_rowwise
            )
        }
    }
    if (!is.null(output)){
        output <- output %>% as.data.frame(.) %>%
         t(.) %>% tibble::as_tibble(.)
        return (output)
    }else{
        return (NULL)
    }
    
}


#### token process ####
#' TBD: Delete the token level [CLS] & [SEP]
#' @param aTibble (list of tibbles) List of the token tibble of each obs.
#' @return The Tb with [CLS] & [SEP] deleted
#' @noRd
delToken_CLS_SEP <- function(aTibble){
    return (NULL)
}
#' Get the rowIDs of tokens having split sign "##".
#' @param tokensTb (tibble) The tokens tibble.
#' @param signSubWord (str) The sign used as the split among subwords. The default is "##".
#' @importFrom dplyr select
#' @return The rowIDs tibbles.
#' @noRd
getIDsSubWord <- function(tokensTb, signSubWord = "##"){
    numSubTokens <- grep("##", tokensTb$tokens) %>% tibble::as_tibble()
    temp <- matrix(nrow=nrow(numSubTokens), ncol=3) %>% as.data.frame() %>% tibble::as_tibble()
    numSubTokens <- cbind(numSubTokens[,1], temp)
    numSubTokens[,2:4] <- 0
    numSubTokens <- numSubTokens %>% tibble::as_tibble()
    colnames(numSubTokens)[1:4] <- c("index", "endID", "sRow", "eRow")
    for (index in (nrow(numSubTokens) - 1) %>% seq_len()){
        if (numSubTokens[["index"]][index + 1] - 
                numSubTokens[["index"]][index] != 1){
            numSubTokens[["endID"]][index] <- 1
        }
    }
    numSubTokens[["endID"]][nrow(numSubTokens)] <- 1
    numSubTokens[["sRow"]][1] <- numSubTokens[["index"]][1] - 1
    # && numSubTokens[["eRow"]][index - 1] == 0
    for (index in nrow(numSubTokens) %>% seq_len()){
        if (index == 1){next}
        if (numSubTokens[["endID"]][index] == 0 && numSubTokens[["sRow"]][index - 1] != 0 
        ){
            numSubTokens[["sRow"]][index] <- numSubTokens[["index"]][index] - 1
        }else if (numSubTokens[["endID"]][index] == 0 && 
        numSubTokens[["endID"]][index - 1] == 0){
            numSubTokens[["sRow"]][index] <- numSubTokens[["sRow"]][index - 1]
        }else if (numSubTokens[["endID"]][index] == 1 &&
        numSubTokens[["endID"]][index - 1] == 1){
            numSubTokens[["sRow"]][index] <- numSubTokens[["index"]][index] - 1
            numSubTokens[["eRow"]][index] <- numSubTokens[["index"]][index]
        }else{
            numSubTokens[["sRow"]][index] <- numSubTokens[["sRow"]][index - 1]
            numSubTokens[["eRow"]][index] <- numSubTokens[["index"]][index]
        }
    }
    numSubTokens <- numSubTokens %>% dplyr::filter(., endID == 1) %>% 
        dplyr::select(., sRow, eRow)
    return (numSubTokens)
}  
#' Transform the subwords row wise.
#' @param startIDRow (numeric) The start rowID of subwords.
#' @param endIDRow (numeric) The end rowID of subwords.
#' @param tokensTb (tibble) The tokens tibble.
#' @param tokenizers (R_obj) The tokenizer in use.
#' @param modelName (str) The transformer model in use.
#' @return The tranformed tokensTb.
#' @noRd
# convertSubWord_rowWise <- function(startIDRow, endIDRow, tokensTb, tokenizers, modelName){
#     theWord <- decodeToken(tokensTb[startIDRow:endIDRow], tokenizers, modelname) 
#     tokensTb[startIDRow:endIDRow] <- theWord
#     return (tokensTb)
# }
#' Transform the subwords back to words based on the input of function getIDsSubWord.
#' @param tokensTb (tibble) The tokens tibble.
#' @param numSubTokens (tibble) The output of function getIDsSubWord.
#' @param tokenizers (R_obj) The tokenizer in use.
#' @param modelName (str) The transformer model in use.
#' @importFrom furrr future_pmap
#' @return The tranformed tokens tibble without subword tokens.
#' @noRd
convertSubWord <- function(tokensTb, numSubTokens, tokenizers, modelName){
    
    for (rowNo in numSubTokens %>% nrow() %>% seq_len()){
        theWord <- decodeToken(tokensTb[["tokens"]][numSubTokens[["sRow"]][rowNo]:numSubTokens[["eRow"]][rowNo]],
         tokenizers, modelName) 
        tokensTb[["tokens"]][numSubTokens[["sRow"]][rowNo]:numSubTokens[["eRow"]][rowNo]] <- theWord
    }
    return (tokensTb)
}
#' Get the trainable Tb of tokens row wise
#' @param tokensTb_row (tibble) The tokens in each observation
#' @param target_row (float) The target value
#' @param tokenizers (RObj) The tokenizers to use;
#' @param modelName (str) The model name of transformers in use;
#' @return The Tb aligned
#' @noRd
getTokenTb_eleWise <- function(tokensTb_row, target_row){

    # tokensTb_row <- tokensTb_row %>% tibble::tibble()
    targetTb_row <- matrix(nrow=tokensTb_row %>% nrow(), ncol=1) %>% as.data.frame() %>% tibble::as_tibble()
    names(targetTb_row)[1] <- c("target")
    targetTb_row[["target"]] <- target_row
    targetTb_row <- cbind(targetTb_row, tokensTb_row)
    return (targetTb_row)
}
#' Get the trainable Tb of tokens.
#' @param aTibble (Tibble) Embeddings from text::textEmbed();
#' @param x (Tibble) The tibble of prediction target;
#' @param tokenizers (RObj) The tokenizers to use;
#' @param modelName (str) The model name of transformers in use;
#' @importFrom furrr future_pmap_dfr
#' @importFrom dplyr group_by summarise
#' @return The Tb aligned.
#' @noRd
getTokensTb <- function(aTibble,
                      x, # text::Language_based_assessment_data_8$hilstotal
                      tokenizers, 
                      modelName
                ){
    # tokensTb, 1 col target, 1 col tokens across observations
    tokensTb <- furrr::future_pmap_dfr(
        list(
            aTibble[["tokens"]][[1]],
            x[[1]]
            # tokenizers %>% list(),
            # modelName %>% list()
        ),
        getTokenTb_eleWise
    )
    tokensTb <- tokensTb %>% dplyr::filter(tokens!="[CLS]")
    tokensTb <- tokensTb %>% dplyr::filter(tokens!="[SEP]")
    # Get the function average embedding and target score across subword tokens
    numSubTokens <- getIDsSubWord(tokensTb)
    tokensTb <- convertSubWord(tokensTb, numSubTokens, tokenizers, modelName)
    tokensTb <- cbind(tokensTb[,2], tokensTb[,1], tokensTb[,3:ncol(tokensTb)])
    colnames(tokensTb)[1:2] <- c("tokens", "target")
    # average embeddings across identical tokens
    tokensTb <- tokensTb %>% dplyr::group_by(tokens) %>% 
        dplyr::summarise(across(colnames(tokensTb)[2]:colnames(tokensTb)[ncol(tokensTb)], mean))
    tokensTb <- cbind(tokensTb[,2], tokensTb[,1], tokensTb[,3:ncol(tokensTb)])
    colnames(tokensTb)[1:2] <- c("target", "tokens")
    return (tokensTb)
}

#### sentence process ####
#' Calculate the row number of the tokens "[CLS]" and "[SEP]".
#' @param aTibble The input tibble of token embeddings.
#' @param modelName The name of the used model to extract exact special tokens.
#' @return A tibble of row number of tokens.
#' @NoRd
token2Sent_rowCLSSEP <- function(aTibble, modelName){
    model_family <- list("bert" = list("bert-base-uncased", "bert-base-cased"),
                      "roberta" = list("roberta-base", "roberta-large")
    )
    model_found = FALSE

    # print(paste0("current model: ", modelName))
   
    # for (model in model_family$bert)
    if (TRUE){
        if (TRUE){ # modelName == model
            model_found = TRUE
            rowCLSSEP <- which(aTibble[["tokens"]] == "[CLS]", arr.ind=TRUE) %>% tibble::as_tibble()
            rowCLSSEP_ <- which(aTibble[["tokens"]] == "[SEP]", arr.ind=TRUE) %>% tibble::as_tibble()
        }
    }
    # for (model in model_family$roberta)
    if (FALSE){
        if (modelName == model){
            model_found = TRUE
            rowCLSSEP <- which(aTibble[["tokens"]] == "<s>", arr.ind=TRUE) %>% tibble::as_tibble()
            rowCLSSEP_ <- which(aTibble[["tokens"]] == "</s>", arr.ind=TRUE) %>% tibble::as_tibble()
        }
    }
    if (FALSE){
        print("Your input model is not supported.\n Please try bert or roberta families.")
        return (NULL)
    }

    rowCLSSEP <- cbind(rowCLSSEP, rowCLSSEP_)
    names(rowCLSSEP) <- c("CLS", "SEP")
    return (rowCLSSEP)
}
#' Create a new sentence tibble in line with the token tibble.
#' @param aTibble The input tibble of token embeddings.
#' @param modelName The name of the used model to extract exact special tokens.
#' @return A new sentence tibble.
#' @NoRd
token2Sent_newSentTb <- function(aTibble, modelName){
    newSentTb <- aTibble %>% token2Sent_rowCLSSEP(., modelName)
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
#' @param modelName The name of the used tokenizer.
#' @param include_CLS_SEP To include the embeddings of "CLS", "SEP", "both", or "none".
#' @return A sentence tibble.
#' @NoRd
token2Sent_getSentRowWise <- function(newSentence, rowCLS, 
                                      rowSEP, aTibble, 
                                      tokenizers, modelName, include_CLS_SEP){
    
    if (!reticulate::py_has_attr(tokenizers, "convert_tokens_to_ids") ||
        !reticulate::py_has_attr(tokenizers, "decode")){
            tokenizers <- getTokenizer(modelName)
        }
    
    if (include_CLS_SEP == "CLS"){
        rowSEP <- rowSEP - 1
    }else if(include_CLS_SEP == "SEP"){
        rowCLS <- rowCLS + 1
    }else if(include_CLS_SEP == "none"){
        rowCLS <- rowCLS + 1
        rowSEP <- rowSEP - 1
    }else{include_CLS_SEP <- "both"}
    newSentence <- decodeToken(aTibble[["tokens"]][rowCLS:rowSEP], tokenizers, modelName)
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
#' @param modelName The name of the used tokenizer.
#' @param include_CLS_SEP To include the embeddings of "CLS", "SEP", "both", or "none".
#' @importFrom furrr pmap
#' @importFrom furrr pmap_dfr
#' @importFrom future plan
#' @importFrom future cluster
#' @importFrom future future
#' @importForm future value
#' @return A sentence tibble.
#' @NoRd
token2Sent_getSent <- function(aTibble, tokenizers, modelName="bert-base-uncased", include_CLS_SEP="both"){

    # future::plan(future::cluster)

    if (!reticulate::py_has_attr(tokenizers, "convert_tokens_to_ids") ||
        !reticulate::py_has_attr(tokenizers, "decode")){
            tokenizers <- getTokenizer(modelName)
        }
    
    newSentTb <- token2Sent_newSentTb(aTibble, modelName)
    getSent_str <- future::future({
        furrr::future_pmap(
        list(
            newSentTb[["sentences"]] %>% as.vector(),
            newSentTb[["CLS"]] %>% as.vector(),
            newSentTb[["SEP"]] %>% as.vector(),
            aTibble %>% list(),
            tokenizers %>% list(),
            modelName %>% list(),
            include_CLS_SEP %>% list()
        ),
        token2Sent_getSentRowWise
        )
    },seed=NULL) # TRUE or NULL?
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
    },seed=NULL) # TRUE or NULL?
    newSentTb[["sentences"]] <- future::value(getSent_str)
    getSent_embed_ <- future::value(getSent_embed)
    newSentTb <- cbind(newSentTb[,1:3], getSent_embed_)
    return (newSentTb)
}
# delete this after
#' Remove ID numbers of CLS and SEP
#' @param aTibble The output from token2Sent_getSent.
#' @return textEmbeds without ID number columns.
#' @NoRd 
removeColCLSSEP <- function(aTibble){
    return (aTibble[, 3:ncol(aTibble)])
}
#' Get the tibble of sentence embeddings.
#' @param sentsList The input list of token embeddings.
#' @param tokenizers The tokenizer used in function textEmbed to get token embeddings.
#' @param modelName The name of the used tokenizer.
#' @param include_CLS_SEP To include the embeddings of "CLS", "SEP", "both", or "none".
#' @return A list containing token embeddings of the function textEmbed() along with sentence embeddings.
#' @NoRd
token2Sent <- function(sentsList, tokenizers, modelName="bert-base-uncased", include_CLS_SEP="both"){

    if (!reticulate::py_has_attr(tokenizers, "convert_tokens_to_ids") ||
        !reticulate::py_has_attr(tokenizers, "decode")){
            tokenizers <- getTokenizer(modelName)
        }
    
    tokenSent <- furrr::future_pmap(
        list(
            sentsList[["tokens"]][["texts"]] %>% as.vector(),
            tokenizers %>% list(),
            modelName %>% list(),
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
    sentsList <- append(sentsList,
                         list("sentences" = list(
                            "texts"=tokenSent, "sentence_tb"=sentenceEmbed)
                         ), 1
    )

    return (sentsList)
}
#' Get the predition tibble rowwise of sentences
#' @param rowTb Each row of the original list
#' @param yValue Row value of the prediction target
#' @return A row ready for prediction
#' @NoRd
getSentsPredTb_row <- function(rowTb, yValue){
    return (cbind(yValue, rowTb))
}
#' Get the tibble of sentence embeddings and their prediction targets.
#' @param PredictTb The input from token2Sent.
#' @param y The numeric variable to predict.
#' @importFrom furrr future_pmap_dfr
#' @return The tibble of sentence embeddings.
#' @NoRd
getSentsPredTb <- function(PredictTb, y){

    PredTb <- furrr::future_pmap_dfr(
        list(PredictTb, y %>% unlist()),
        getSentsPredTb_row
    )
    return (PredTb)
}
#### passage process ####
#' Get the predition of passage of each row
#' @param PredPasg_row (tibble) An individual row of embeddings.
#' @param y_row (numeric) The prediction target.
#' @importFrom furrr pmap
#' @NoRd
getPasgPredTb_row <- function(){
    return (NULL)
}

#' Get the prediction of sentences
#' @param textEmbeds (R obj) The output of textEmbed()
#' @param y (vector) The target to predict
#' @import furrr furrr_pmap
#' @return The trained model of passages and their prediction targets.
#' @NoRd
getPasgPredTb <- function(textEmbeds, y){

    pasgData <- cbind(y, textEmbeds[["texts"]][["texts"]])

    return (pasgData)
}

#### key training functions
#' langTrain_token
#' @param trainObj The tibble of token embeddings
#' @param x The tibble, The prediction target
#' @param tokenizers (R_obj) The tokenizers in use
#' @param modelName (str) The model name in use
#' @retrun the trained model
#' @NoRd
langTrain_tokens <- function(trainObj, x, tokenizers, modelName){

    # need a element-wise list func
    tokensTb <- getTokensTb(trainObj, x, tokenizers, modelName) %>% tibble::as_tibble()
    theModelTokens <- textTrain(#textTrainRegression(
        x = tokensTb[,3:ncol(tokensTb)],
        y = tokensTb[,1]) 

    return (theModelTokens)
}
#' langTrain_sent
#' @param trainObj The tibble of sentence embeddings
#' @param x The prediction target
#' @return the trained model
#' @NoRd
langTrain_sents <- function(trainObj, x){

    sentsTrainTb <- getSentsPredTb(trainObj[["sentences"]][["texts"]], 
                         x # Language_based_assessment_data_8$hilstotal
    ) %>% tibble::as_tibble()

    theModelSents <- textTrain(#textTrainRegression(
        x = sentsTrainTb[,3:ncol(sentsTrainTb)],
        y = sentsTrainTb[,1]) 

    return (theModelSents)
}
#' Get the models of training
#' @param trainObj (R_obj) An R obj containing the information of "token", "sentence",  and "paragraph".
#' @param x (R_obj) The training target.
#' @param lang_level (str) "token", "sentence", "passage", "all". The default is "all".
#' @param tokenizers (R_obj) The tokenizer in use.
#' @param modelName (str) The transformer model in use.
#' @importFrom future future value
#' @return The trained model
#' @NoRd
langTrain <- function(trainObj, x, lang_level="all", tokenizers, modelName){

    if(lang_level == "token"){

        theModelTokens <- langTrain_tokens(trainObj, x, tokenizers, modelName)

        return (list("modelTokens"=theModelTokens))
    }else if(lang_level == "sentence"){

        theModelSents <- langTrain_sents(trainObj, x)    
        return (list("modelSents"=theModelSents))
    
    }else if(lang_level == "paragraph"){
        
        parasTb <- cbind(x,  # Language_based_assessment_data_8$hilstotal
            trainObj[["texts"]][[1]]) %>% tibble::as_tibble()
        theModelParas <- textTrainRegression(
            x = parasTb[,2:ncol(parasTb)],
            y = parasTb[,1]) 
        return (list("modelParas"=theModelParas))
    }else{
        modelingTokens <- future::future({
            langTrain_tokens(trainObj, x, tokenizers, modelName)
        }, seed=NULL)
        modelingSents <- future::future({
            langTrain_sents(trainObj, x)
        }, seed=NULL)
        parasTb <- cbind(x,  # Language_based_assessment_data_8$hilstotal
            trainObj[["texts"]][[1]]) %>% tibble::as_tibble()
        modelingParas <- future::future({textTrain(#textTrainRegression(
            x = parasTb[,2:ncol(parasTb)],
            y = parasTb[,1])
        }, seed=NULL) 
        theModelTokens <- future::value(modelingTokens)
        theModelSents <- future::value(modelingSents)
        theModelParas <- future::value(modelingParas)

        return(list("modelTokens"=theModelTokens,
                    "modelSents"=theModelSents,
                    "modelParas"=theModelParas
        ))
    }
}
#' langPred_tokens
#' Get the tokens prediction
#' @param predObj An R obj containing the information of "token", "sentence",  and "passage".
#' @param tokensModel A trained token model.
#' @returen The prediction of tokens.
#' @NoRd
langPred_tokens <- function(predObj, tokensModel){
    
    tokensPred <- text::textPredict(tokensModel, 
      predObj[["tokens"]][["texts"]][[1]][,2:ncol(predObj[["tokens"]][["texts"]][[1]])])
    tokensPred <- tokensPred %>% tibble::as_tibble()
    colnames(tokensPred)[1] <- c("y_pred")
    tokensPred <- cbind(tokensPred, predObj[["tokens"]][["texts"]][[1]])
    return (tokensPred)
}
#' langPred_sents
#' Get the sentence prediction
#' @param predObj An R obj containing the information of "token", "sentence",  and "passage".
#' @param sentsModel A trained sentence model.
#' @return The prediction of sentences.
#' @NoRd
langPred_sents <- function(predObj, sentsModel){
    
    sentsPred <- text::textPredict(sentsModel, 
        predObj[["sentences"]][["sentence_tb"]][, 2:ncol(predObj[["sentences"]][["sentence_tb"]])])
    names(sentsPred) <- c("y_pred")
    sentsPred <- cbind(sentsPred, predObj[["sentences"]][["sentence_tb"]])

    return(sentsPred)
}
#' Get the results of paragraph prediction
#' @param predObj An R obj containing the information of "token", "sentence",  and "passage".
#' @param parasModel A trained paragraph model. 
#' @return The predictions of paragraphs.
#' @NoRd
langPred_paras <- function(predObj, parasModel){

    parasPred <- text::textPredict(parasModel,
        predObj[["texts"]][["texts"]],
        dim_names=FALSE)

    return(parasPred)    
}
#' Get the results of prediction
#' @param predObj An R obj containing the information of "token", "sentence",  and "paragraph".
#' @param theModels The models from langTrain
#' @param lang_level "token", "sentence", "parapgraph", "all". The default is "sentence".
#' @importFrom future future
#' @return The prediction R object
#' @NoRd
langPred <- function(predObj, theModels, lang_level = "sentence"){

    if(!(lang_level %>% is.character())){lang_level = "sentence"}

    if(lang_level == "token"){

        thePred <- predObj %>% langPred_tokens(., theModels[["modelTokens"]]) %>% 
            tibble::as_tibble()
        return (thePred)
    }else if(lang_level == "sentence"){

        thePred <- predObj %>% langPred_sents(., theModels[["modelSents"]]) %>% 
             tibble::as_tibble()
        return (thePred)
    
    }else if(lang_level == "paragraph"){

        thePred <- predObj %>% langPred_paras(., theModels[["modelParas"]]) %>%
           tibble::as_tibble()
        colnames(thePred)[1] <- c("y_pred")
        return (thePred)
    }else{
        
        # future::future
        predTokens <- future::future({
            predObj %>% langPred_tokens(., theModels[["modelTokens"]]) %>% tibble::as_tibble()
        }, seed=NULL)
        predSents <- future::future({
            predObj %>% langPred_sents(., theModels[["modelSents"]]) %>% tibble::as_tibble()
        }, seed=NULL)
        predParas <- future::future({
            predObj %>% langPred_paras(., theModels[["modelParas"]]) %>% tibble::as_tibble()
        }, seed=NULL)
        predOutTokens <- future::value(predTokens)
        predOutSents <- future::value(predSents)
        predOutParas <- future::value(predParas)
        colnames(predOutParas)[1] <- c("y_pred")

        return(list("predTokens"=predOutTokens,
                    "predSents"=predOutSents,
                    "predParas"=predOutParas
        ))
    }

}

# TODO: "left_join" based on "preds" might have bugs. In the future it should use id.
#' Get the color code for each language unit
#' @param embedObj The embeddings of language units
#' @importFrom dplyr arrange
#' @return The color values list contained embedObj
#' @NoRd
getLangColorTb <- function(embedObj){

    temp <- NULL
    coloredTb <- matrix(nrow=nrow(embedObj[["Pred"]][["predTokens"]]) + 
      nrow(embedObj[["Pred"]][["predSents"]]) + 1,
     ncol=4) %>% as.data.frame()
    names(coloredTb) <- c("colorCode", "preds", "texts", "unitLang")
    start <- 1
    end <- nrow(embedObj[["Pred"]][["predTokens"]])
    coloredTb[start:end, 2:3] <- 
        embedObj[["Pred"]][["predTokens"]][, 1:2]
    coloredTb[start:end, 4] <- "tokens"
    start <- nrow(embedObj[["Pred"]][["predTokens"]]) + 1
    end <- nrow(embedObj[["Pred"]][["predTokens"]]) + nrow(embedObj[["Pred"]][["predSents"]])
    coloredTb[start:end, 2:3] <- embedObj[["Pred"]][["predSents"]][, 1:2]
    coloredTb[start:end, 4] <- "sents"
    coloredTb[nrow(coloredTb), 2] <- embedObj[["Pred"]][["predParas"]][[1]][1]
    coloredTb[nrow(coloredTb), 3] <- "Lorem Ipsum"
    coloredTb[nrow(coloredTb), 4] <- "paras"
    coloredTb_sorted <- dplyr::arrange(coloredTb, preds)
    coloredTb_sorted[, 1] <- coloredTb_sorted[["preds"]] %>% map2Color(.)
    temp <- dplyr::left_join(coloredTb[,2:ncol(coloredTb)], 
                            coloredTb_sorted[,1:2], by=c("preds"))
    coloredTb[,"colorCode"] <- temp[,"colorCode"] 
    embedObj <- append(embedObj, list("coloredTb"=coloredTb), length(embedObj) + 1)
    # add colorCode to tokens embed
    temp <- NULL
    temp <- embedObj[["coloredTb"]] %>% dplyr::filter(., unitLang=="tokens")
    embedObj[["Pred"]][["predTokens"]] <- cbind(
        temp[["colorCode"]],
        embedObj[["Pred"]][["predTokens"]])
    names(embedObj[["Pred"]][["predTokens"]])[1] <- c("colorCode")
    # add colorCode to sents embed
    temp <- NULL
    temp <- embedObj[["coloredTb"]] %>% dplyr::filter(., unitLang=="sents")
    embedObj[["Pred"]][["predSents"]] <- cbind(
        temp[["colorCode"]],
        embedObj[["Pred"]][["predSents"]])
    names(embedObj[["Pred"]][["predSents"]])[1] <- c("colorCode")
    # add colorCode to paras embed, and add the "Lorem Ipsum" place holder as the replacement.
    temp <- NULL
    temp <- embedObj[["coloredTb"]] %>% dplyr::filter(., unitLang=="paras")
    embedObj[["Pred"]][["predParas"]] <- temp[, 1:ncol(temp)-1]
   
    return (embedObj)
}
# token separation based on limited symbol
#' @param outObj The R object containing the color code.
#' @importFrom tibble as_tibble
#' @return The output data frame following the data structure.
#' @NoRd
getOutDf <- function(outObj){

    # temp <- token2Sent_rowCLSSEP(toPred[["tokens"]][["texts"]][[1]])
    # use coloredTb containing period to generate a sentNo column. Then generate a new list ele in outObj.
    outTb <- matrix(nrow=nrow(outObj[["coloredTb"]]),
                    ncol=1) %>% as.data.frame()
    colnames(outTb)[1] <- c("sentNo")

    # token marking with sentence num
    # token separation, currently punctuations support ".", "?", "!"
    posPeriod <- which(outObj[["coloredTb"]][["texts"]] == "." | 
      outObj[["coloredTb"]][["texts"]] == "?" |
      outObj[["coloredTb"]][["texts"]] == "!", arr.ind=TRUE) %>%
       tibble::as_tibble()
    for (rowNo in length(posPeriod[[1]]) %>% seq_len(.)){
        if (rowNo == 1){
            start <- 1
            end <- posPeriod[[1]][rowNo]
            # Prevent the sentence ends with the "[SEP]" token.
            if (outObj[["coloredTb"]][["texts"]][end + 1] == "[SEP]"){end <- end + 1}
            outTb[["sentNo"]][start:end] <- paste0("sentNo_1")
        }else{
            start <- posPeriod[[1]][rowNo - 1] + 1
            end <- posPeriod[[1]][rowNo]
            if (outObj[["coloredTb"]][["texts"]][start] == "[SEP]"){start <- start + 1}
            if (outObj[["coloredTb"]][["texts"]][end + 1] == "[SEP]"){end <- end + 1}
            outTb[["sentNo"]][start:end] <- paste0("sentNo_", as.character(rowNo))
        }    
    }
    # sentence marking with sentence num
    start <- end + 1
    end <- nrow(outTb) - 1
    marker <- 0
    for (rowNo in start:end){
        marker <- marker + 1
        outTb[["sentNo"]][rowNo] <- paste0("sentNo_", as.character(marker))
    }
    outTb[["sentNo"]][nrow(outTb)] <- paste0("paras")
    outObj[["coloredTb"]] <- cbind(outObj[["coloredTb"]], outTb[["sentNo"]])
    colnames(outObj[["coloredTb"]])[ncol(outObj[["coloredTb"]])] <- c("sentNo")

    # outTb transform
    outTb <- matrix(nrow=nrow(outObj[["coloredTb"]] %>% dplyr::filter(., unitLang=="tokens")),
                    ncol=7) %>% as.data.frame(.)
    colnames(outTb) <- c("token", "tokenPred", "tokenColor", "sentPred", "sentColor", "paraPred", "paraColor")
    # outTb token info
    marker <- outObj[["coloredTb"]] %>% dplyr::filter(., unitLang=="tokens") %>% nrow(.)
    outTb[["token"]] <- outObj[["coloredTb"]][["texts"]][1:marker]
    outTb[["tokenPred"]] <- outObj[["coloredTb"]][["preds"]][1:marker]
    outTb[["tokenColor"]] <- outObj[["coloredTb"]][["colorCode"]][1:marker]
    # outTb sent info
    for (rowNo in start:end){
        marker <- outObj[["coloredTb"]][["sentNo"]][rowNo]
        marker <- which(outObj[["coloredTb"]][["sentNo"]] == marker)
        marker_1 <- marker[1]
        marker_2 <- marker[length(marker)-1]
        outTb[["sentPred"]][marker_1:marker_2] <- outObj[["coloredTb"]][["preds"]][rowNo]
        outTb[["sentColor"]][marker_1:marker_2] <- outObj[["coloredTb"]][["colorCode"]][rowNo]
    }
    # outTb para info
    outTb[["paraPred"]] <- outObj[["coloredTb"]][["preds"]][nrow(outObj[["coloredTb"]])]
    outTb[["paraColor"]] <- outObj[["coloredTb"]][["colorCode"]][nrow(outObj[["coloredTb"]])]
    outObj <- append(outObj, list("outDf"=outTb), length(outObj))

    return (outObj)
}



#### Basic function ####
# textsPred -> texts
# textsTrain -> word_embeddings

#' Function to calculate the highlight color value.
#' @param textsPred A character variable or a tibble/dataframe (not support now) with at least one character variable.
#' @param textsTrain Embedding values from text::textEmbed(..., dim_name=FALSE)
#' @param x Numeric variable that the words should be plotted according to on the x-axes.
#' @param model Character string specifying pre-trained language model (default 'bert-base-uncased'). For now it only supports the default. For full list of options see pretrained models at HuggingFace. For example use "bert-base-multilingual-cased", "openai-gpt", "gpt2", "ctrl", "transfo-xl-wt103", "xlnet-base-cased", "xlm-mlm-enfr-1024", "distilbert-base-cased", "roberta-base", or "xlm-roberta-base".
#' @param include_CLS_SEP Averaging of the sentence embedding should contain the mark of the sentence START "cls", the sentence END "sep", or "both". Current default (supported) is "both".
#' @param outSubwordToken To output or not the subword token in the results. Yet not supported.
#' @param lang_level Set the language level in the output of predictions. The defaut value is "all".
#' @param projection_method Set the projection method. Can be either "ridge" or "dot_product." The defaulf value is "ridge". "dot_product" is not yet supported.
#' @param device param yet not supported.
#' @param tokenzier_parallelism param yet not supported.
#' @param logging_level param yet not supported.
#' @importFrom future future value
#' @return List of names of models and tibbles.
#' @examples
#' \dontrun{
#' textProjectionText()
#' }
#' @seealso see \code{\link{textProjection}} and \code{\link{textWordPrediction}}
#' @export
textProjectionText <- function(
    textsPred,
    textsTrain,
    x,
    y = NULL,
    model = "bert-base-uncased",
    include_CLS_SEP = "both",
    outSubwordToken = FALSE,
    lang_level = "all",
    projection_method = "ridge",
    device = "cpu",
    tokenizer_parallelism = FALSE,
    logging_level = "error"
){

    require(magrittr)
    require(future)

    #### 1. Check the format of the input
    if (TRUE){
        textIsStr <- FALSE
        textIsDF <- FALSE
        textIsTb <- FALSE
        if (textsPred %>% is.character()){textsIsStr <- TRUE}else{textsIsStr <- FALSE}
        if (textsPred %>% is.data.frame()){textsIsDF <- TRUE}else{textsIsDF <- FALSE}
        if (textsPred %>% tibble::is_tibble()){textsIsTb <- TRUE}else{textsIsTb <- FALSE}
        if (lang_level %>% is.character()){lang_level <- lang_level}else(lang_level <- "all")
        if (model %>% is.character()){modelName <- model}else{modelName <- "bert-base-uncased"}
        if (!x %>% tibble::is_tibble()){x <- x %>% tibble::as_tibble}
        tokenizers <- modelName %>% getTokenizer()
    }

    #### 2. Get the embedding of the input to predict.
    # if texts == str
    if (textsIsStr){
            toPredEmbedProc <- future::future({
                textsPred %>% textEmbed(modelName, dim_name=FALSE)
                }, seed=NULL)
            toTrainProc <- future::future({textsTrain %>% token2Sent(., tokenizers, modelName, include_CLS_SEP)},
            seed=NULL)
            toPred <- future::value(toPredEmbedProc)
            toPredProc <- future::future({
                toPred %>% token2Sent(., tokenizers, modelName, include_CLS_SEP)
                }, seed=NULL)
            toPred <- future::value(toPredProc)
            toTrain <- future::value(toTrainProc)
    }

    # if texts == DF or Tb 
    if (FALSE){ # textsIsDF | textsIsTb
        if (textsIsDF){texts <- texts %>% tibble::as_tibble()}
    }
    
    #### 3. Train the model using given training data.
    # sentsTrainTb <- getPredictTb(sentsTrain[["sentences"]][["texts"]], 
    #                      x # Language_based_assessment_data_8$hilstotal
    # ) %>% tibble::as_tibble()
    # results <- textTrainRegression(
    #     x = sentsTrainTb[,3:ncol(sentsTrainTb)],
    #     y = sentsTrainTb[,1]) 
    theModels <- langTrain(toTrain, x, "all", tokenizers, modelName)

    #### 4. Use trained model to predict the input
    output <- langPred(toPred, theModels, "all")

    #### 5. Output colorCode
    output <- list("Pred" = output, "model" = theModels)
    output <- getLangColorTb(output)

    #### 6. Reorganize the data structure as the last ele in the list
    if (TRUE){
        output_out <- getOutDf(output)
    }

    return (output_out)

}

#### textPlotText ####
#' get html all, no function.
#' @param RObj_outDf The model output from the function "textProjectionText".
#' @importFrom htmltools p, span
#' @return The RObject of a plot
#' @NoRd
textPlotText_getHtmAll <- function(RObj_outDf){

    if (TRUE){
        paraColor <- RObj_outDf[["outDf"]]$`paraColor`[1]
        tokenColorDf <- RObj_outDf[["outDf"]]["tokenColor"]
        sentColorDf <- RObj_outDf[["outDf"]]["sentColor"]
        tokenSeries <- RObj_outDf[["outDf"]]["token"]
    }
    
    tokens <- htmltools::p(style=paste0("background-color:", paraColor))
    for (rowNo in nrow(RObj_outDf[["outDf"]]) %>% seq_len()){
        if (tokenSeries[["token"]][rowNo][[1]] == "[CLS]" ||
            tokenSeries[["token"]][rowNo][[1]] == "[SEP]"){next}
        tokenColor <- paste0("color:", tokenColorDf[["tokenColor"]][rowNo]
            , ";background-color:", sentColorDf[["sentColor"]][rowNo])
        token <- htmltools::span(tokenSeries[["token"]][rowNo][[1]], style=tokenColor)
        tokens[["children"]] <- append(tokens[["children"]], token %>% list())
    }
    
    return (tokens)
}
#' get html tokens, no function.
#' @param RObj_outDf The model output from the function "textProjectionText".
#' @importFrom htmltools p, span
#' @return The RObject of a plot
#' @NoRd
textPlotText_getHtmToken <- function(RObj_outDf){

    if (TRUE){
        paraColor <- RObj_outDf[["outDf"]]$`paraColor`[1]
        tokens <- htmltools::p()
        tokenColorDf <- RObj_outDf[["outDf"]]["tokenColor"]
        sentColorDf <- RObj_outDf[["outDf"]]["sentColor"]
        tokenSeries <- RObj_outDf[["outDf"]]["token"]
    }
    
    for (rowNo in nrow(RObj_outDf[["outDf"]]) %>% seq_len()){
        if (tokenSeries[["token"]][rowNo][[1]] == "[CLS]" ||
            tokenSeries[["token"]][rowNo][[1]] == "[SEP]"){next}
        tokenColor <- paste0("color:", tokenColorDf[["tokenColor"]][rowNo])
        token <- htmltools::span(tokenSeries[["token"]][rowNo][[1]], style=tokenColor)
        tokens[["children"]] <- append(tokens[["children"]], token %>% list())
    }
    return (tokens)
}
#' get html sentence, no function.
#' @param RObj_outDf The model output from the function "textProjectionText".
#' @importFrom htmltools p, span
#' @return The RObject of a plot
#' @NoRd
textPlotText_getHtmSent <- function(RObj_outDf){
    
    if (TRUE){
        paraColor <- RObj_outDf[["outDf"]]$`paraColor`[1]
        tokens <- htmltools::p()
        tokenColorDf <- RObj_outDf[["outDf"]]["tokenColor"]
        sentColorDf <- RObj_outDf[["outDf"]]["sentColor"]
        tokenSeries <- RObj_outDf[["outDf"]]["token"]
    }

    for (rowNo in nrow(RObj_outDf[["outDf"]]) %>% seq_len()){
        if (tokenSeries[["token"]][rowNo][[1]] == "[CLS]" ||
            tokenSeries[["token"]][rowNo][[1]] == "[SEP]"){next}
        tokenColor <- paste0("background-color:", sentColorDf[["sentColor"]][rowNo])
        token <- htmltools::span(tokenSeries[["token"]][rowNo][[1]], style=tokenColor)
        tokens[["children"]] <- append(tokens[["children"]], token %>% list())
    }
    return (tokens)
}
#' get html paragraph, no function.
#' @param RObj_outDf The model output from the function "textProjectionText".
#' @importFrom htmltools p, span
#' @return The RObject of a plot
#' @NoRd
textPlotText_getHtmPara <- function(RObj_outDf){
    
    if (TRUE){
        paraColor <- RObj_outDf[["outDf"]]$`paraColor`[1]
        tokens <- htmltools::p(style=paste0("background-color:", paraColor))
        tokenColorDf <- RObj_outDf[["outDf"]]["tokenColor"]
        sentColorDf <- RObj_outDf[["outDf"]]["sentColor"]
        tokenSeries <- RObj_outDf[["outDf"]]["token"]
    }

    for (rowNo in nrow(RObj_outDf[["outDf"]]) %>% seq_len()){
        if (tokenSeries[["token"]][rowNo][[1]] == "[CLS]" ||
            tokenSeries[["token"]][rowNo][[1]] == "[SEP]"){next}
        token <- htmltools::span(tokenSeries[["token"]][rowNo][[1]])
        tokens[["children"]] <- append(tokens[["children"]], token %>% list())
    }
    return (tokens)
}

#' Function to plot the legend.
#' @param RObj_outDF (R_obj) The model output from the function "textProjectionText".
#' @param sents (shiny tags) The sents tags from the previous processing within the func textPlotText().
#' @param outHTML (shiny tags) The output from the previous processing within the func textPlotText().
#' @param textColor (str) The color of the text in the legend.
#' @param plotTitle (str) The title for the plot.
#' @importFrom dplyr filter arrange
#' @importFrom tibble as_tibble
#' @return The Legend HTML string
#' @NoRd
textPlotText_getLegend <- function(RObj_outDf, sents, outHTML, textColor = "black", plotTitle = "A plot"){

    # Get the params for the legend
    if (TRUE){
        quanVals <- quantile(RObj_outDf[["coloredTb"]]["preds"] %>% as.matrix())
        toLegend <- matrix(nrow=2,ncol=5) %>% tibble::as_tibble()
        colnames(toLegend) <- names(quanVals)
        toFilt <- quanVals %>% as.matrix()
        for (colNo in ncol(toLegend) %>% seq_len()){
            temp <- RObj_outDf[["coloredTb"]] %>% dplyr::filter(preds >= toFilt[colNo]) %>%
                    dplyr::arrange(., preds)
            toLegend[[colNo]][1] <- temp[["preds"]][1] %>% round(., 1) %>% as.character()
            toLegend[[colNo]][2] <- temp[["colorCode"]][1]
        }
    }

    # Combine the plot title, the plot, and the legend into one.
    if (TRUE){
        spanStyleCtrl <- paste0("padding:2%;color:", textColor, ";background-color:")
        outHTML[["children"]] <- append(
         outHTML[["children"]],
         htmltools::withTags(htmltools::div(htmltools::h1(plotTitle))) %>% list())
        outHTML[["children"]] <- append(
         outHTML[["children"]],
         sents %>% list())
        outHTML[["children"]] <- append(
         outHTML[["children"]],
         htmltools::withTags(htmltools::div(
            htmltools::h1("Legend"),
            htmltools::tags$table(
              htmltools::tags$tr(
                htmltools::tags$td(htmltools::span(toLegend[["0%"]][1], style=paste0(spanStyleCtrl,toLegend[["0%"]][2]))),
                htmltools::tags$td(htmltools::span(toLegend[["25%"]][1], style=paste0(spanStyleCtrl,toLegend[["25%"]][2]))),
                htmltools::tags$td(htmltools::span(toLegend[["50%"]][1], style=paste0(spanStyleCtrl,toLegend[["50%"]][2]))),
                htmltools::tags$td(htmltools::span(toLegend[["75%"]][1], style=paste0(spanStyleCtrl,toLegend[["75%"]][2]))),
                htmltools::tags$td(htmltools::span(toLegend[["100%"]][1], style=paste0(spanStyleCtrl,toLegend[["100%"]][2]))),
                style="text-align:center;padding:2px"),
              htmltools::tags$tr(
                htmltools::tags$td("0% quantile"),
                htmltools::tags$td("25% quantile"),
                htmltools::tags$td("50% quantile"),
                htmltools::tags$td("75% quantile"),
                htmltools::tags$td("100% quantile"),
                style="text-align:center;padding:2px"),
              style="width:100%")
            ))%>% list())
    # sents[["children"]] <- append(sents[["children"]], div("hello") %>% list())
    }

    return (outHTML)
}

# TODO: paragraph level
#' Function to plot the highlight color value into html object.
#' @param RObj_outDf The model output from the function "textProjectionText".
#' @param lang_level (list) Set the language level in the output of the plot. The defaut value is "all". Possible options include "token", "sentence", and "paragraph". Should input a list object.
#' @param textColor (string) "black", "white".
#' @param plotTitle (string) Set the plot string.
#' @importFrom htmltools browsable, div, p, span
#' @return The RObject of the plot
#' @examples
#' \dontrun{
#' textPlotText()
#' }
#' @seealso see \code{\link{textPlotText}}
#' @export
textPlotText <- function(RObj_outDf, lang_level=list("all"), textColor="black", plotTitle="A plot"){

    require(dplyr)
    require(magrittr)
    require(htmltools)
    # require(colorspace)

    
    # https://css-tricks.com/snippets/css/a-guide-to-flexbox/
    styleControl <- "display: flex;
     flex-wrap: wrap; 
     justify-content:flex-start;
     "

    if (TRUE){
        outHTML <- htmltools::div(style = styleControl)
        paraColor <- paste0(RObj_outDf[["outDf"]]$`paraColor`[1])
        tokenColorDf <- RObj_outDf[["outDf"]]["tokenColor"]
        sentColorDf <- RObj_outDf[["outDf"]]["sentColor"]
        tokenSeries <- RObj_outDf[["outDf"]]["token"]
        if (is.character(textColor)){
            if (textColor == "black"){
                textColor_ <- "#000000"
            }else{textColor_ <- "#FFFFFF"}
        }else{
            textColor_ <- "#FFFFFF"
        }
    }

    if ("token" %in% lang_level){
        paraColor <- paste0(paraColor, "padding: 2% 2%")
        sents <- htmltools::div(style = styleControl) # level para color
        sentsUnique <- sentColorDf[["sentColor"]][!duplicated(sentColorDf[["sentColor"]])]
        counter <- list(0, 0)
        for (rowNoSent in length(sentsUnique) %>% seq_len()){
            rangeNum <- which(sentColorDf[["sentColor"]] == sentsUnique[[rowNoSent]])
            counter[[1]] <- rangeNum[[1]]
            counter[[2]] <- rangeNum[[length(rangeNum)]]
            tokens <- htmltools::p(style = paste0(
                # "background-color:", sentsUnique[[rowNoSent]], "64",
                "padding: 1% 1%;"
                )) # level sent color
            for (rowNoToken in length(rangeNum) %>% seq_len()){
                idToken <- counter[[1]] + rowNoToken - 1
                if (tokenSeries[["token"]][idToken][[1]] == "[CLS]" || tokenSeries[["token"]][idToken][[1]] == "[SEP]"){next}
                tokenColor <- paste0("color:", textColor_,";background-color:", tokenColorDf[["tokenColor"]][idToken],";")
                # level token color
                token <- htmltools::span(tokenSeries[["token"]][idToken][[1]], style=tokenColor)
                tokens[["children"]] <- append(tokens[["children"]] , token %>% list())
            }
            sents[["children"]] <- append(sents[["children"]], tokens %>% list())
        }
    }

    if ("sentence" %in% lang_level){
        paraColor <- paste0(styleControl, "padding: 2% 2%")
        sents <- htmltools::div(style = paraColor) # level para color
        sentsUnique <- sentColorDf[["sentColor"]][!duplicated(sentColorDf[["sentColor"]])]
        counter <- list(0, 0)
        for (rowNoSent in length(sentsUnique) %>% seq_len()){
            rangeNum <- which(sentColorDf[["sentColor"]] == sentsUnique[[rowNoSent]])
            counter[[1]] <- rangeNum[[1]]
            counter[[2]] <- rangeNum[[length(rangeNum)]]
            tokens <- htmltools::p(style = paste0(
                "background-color:", sentsUnique[[rowNoSent]], "64;",
                "padding: 1% 1%;"
                )) # level sent color
            for (rowNoToken in length(rangeNum) %>% seq_len()){
                idToken <- counter[[1]] + rowNoToken - 1
                if (tokenSeries[["token"]][idToken][[1]] == "[CLS]" || tokenSeries[["token"]][idToken][[1]] == "[SEP]"){next}
                tokenColor <- paste0("color:", textColor_, ";",
                # "background-color:", tokenColorDf[["tokenColor"]][idToken],
                ";")
                # level token color
                token <- htmltools::span(tokenSeries[["token"]][idToken][[1]], style=tokenColor)
                tokens[["children"]] <- append(tokens[["children"]] , token %>% list())
            }
            sents[["children"]] <- append(sents[["children"]], tokens %>% list())
        }
    }

    if ("token" %in% lang_level && "sentence" %in% lang_level){
        paraColor <- paste0(styleControl, "padding: 2% 2%")
        sents <- htmltools::div(style = paraColor) # level para color
        sentsUnique <- sentColorDf[["sentColor"]][!duplicated(sentColorDf[["sentColor"]])]
        counter <- list(0, 0)
        for (rowNoSent in length(sentsUnique) %>% seq_len()){
            rangeNum <- which(sentColorDf[["sentColor"]] == sentsUnique[[rowNoSent]])
            counter[[1]] <- rangeNum[[1]]
            counter[[2]] <- rangeNum[[length(rangeNum)]]
            tokens <- htmltools::p(style = paste0(
                "background-color:", sentsUnique[[rowNoSent]], "64;",
                "padding: 1% 1%;"
                )) # level sent color
            for (rowNoToken in length(rangeNum) %>% seq_len()){
                idToken <- counter[[1]] + rowNoToken - 1
                if (tokenSeries[["token"]][idToken][[1]] == "[CLS]" || tokenSeries[["token"]][idToken][[1]] == "[SEP]"){next}
                tokenColor <- paste0("color:", textColor_,";background-color:", tokenColorDf[["tokenColor"]][idToken],";")
                # level token color
                token <- htmltools::span(tokenSeries[["token"]][idToken][[1]], style=tokenColor)
                tokens[["children"]] <- append(tokens[["children"]] , token %>% list())
            }
            sents[["children"]] <- append(sents[["children"]], tokens %>% list())
        }
    }

    # lang_level == "all"
    # display in several <p>
    if ("all" %in% lang_level){
        paraColor <- paste0("background-color:", paraColor, "20", ";", "padding: 2% 2%")
        sents <- htmltools::div(style = paraColor) # level para color
        sentsUnique <- sentColorDf[["sentColor"]][!duplicated(sentColorDf[["sentColor"]])]
        counter <- list(0, 0)
        for (rowNoSent in length(sentsUnique) %>% seq_len()){
            rangeNum <- which(sentColorDf[["sentColor"]] == sentsUnique[[rowNoSent]])
            counter[[1]] <- rangeNum[[1]]
            counter[[2]] <- rangeNum[[length(rangeNum)]]
            tokens <- htmltools::p(style = paste0(
                "background-color:", sentsUnique[[rowNoSent]], "64;",
                "padding: 1% 1%;"
                )) # level sent color
            for (rowNoToken in length(rangeNum) %>% seq_len()){
                idToken <- counter[[1]] + rowNoToken - 1
                if (tokenSeries[["token"]][idToken][[1]] == "[CLS]" || tokenSeries[["token"]][idToken][[1]] == "[SEP]"){next}
                tokenColor <- paste0("color:", textColor_,";background-color:", tokenColorDf[["tokenColor"]][idToken],";")
                # level token color
                token <- htmltools::span(tokenSeries[["token"]][idToken][[1]], style=tokenColor)
                tokens[["children"]] <- append(tokens[["children"]] , token %>% list())
            }
            sents[["children"]] <- append(sents[["children"]], tokens %>% list())
        }

        #########################

        if (FALSE){
            for (rowNo in nrow(RObj_outDf[["outDf"]]) %>% seq_len()){
            if (tokenSeries[["token"]][rowNo][[1]] == "[CLS]" ||
                tokenSeries[["token"]][rowNo][[1]] == "[SEP]"){next}
            tokenColor <- paste0("color:", tokenColorDf[["tokenColor"]][rowNo]
                , ";background-color:", sentColorDf[["sentColor"]][rowNo])
            token <- htmltools::span(tokenSeries[["token"]][rowNo][[1]], style=tokenColor)
            tokens[["children"]] <- append(tokens[["children"]], token %>% list())
            }
            plot <- htmltools::browsable(
                style = styleControl,
                tokens
            )
        }
    }

    # TBD
    if ("paragraph" %in% lang_level && !"token" %in% lang_level && !"sentence" %in% lang_level){
        paraColor <- paste0("background-color:", paraColor, "20", ";", "padding: 2% 2%")
        sents <- htmltools::div(style = paraColor) # level para color
        plot <- htmltools::browsable(sents)
        return (plot)
    }

    # Add legend
    if ("token" %in% lang_level || "sentence" %in% lang_level || "all" %in% lang_level){
        outHTML <- textPlotText_getLegend(RObj_outDf, sents, outHTML, textColor_, plotTitle)
    }

    plot <- htmltools::browsable(outHTML)
    return (plot)
    
}

# TODO: using diverging_hcl to custom the plot!!!!!!!!
# https://www.rdocumentation.org/packages/dichromat/versions/1.1/topics/colorRampPalette
# https://r-universe.dev/manuals/grDevices.html#colorRamp
# https://r-universe.dev/manuals/grDevices.html#col2rgb
# colorRampPalette returns a function that takes an integer argument (the required number of colors) and returns a character vector of colors (see rgb) interpolating the given sequence (similar to heat.colors or terrain.colors).
# colorRampPalette(c("blue", "red"))( 4 ) ## (n)

# Manually set the color for textPlotText().
#' @param RObj_outDf (R_obj) The model output from the function "textProjectionText".
#' @param mode (character) Select the "RGB" or "HSL" space to generate color. The "HSL" is the default and the recommeded mode.
#' @param RGBstart (list) The list (Red, Green, Blue) containing RGB values where the color generation starts. Each dimension should be within 0 to 255. Only available when mode = "RGB".
#' @param RGBend (list) The list (Red, Green, Blue), same to RGBstart where the color generation ends. Only available when mode = "RGB".
#' @importFrom colorspace diverging_hcl
#' @importFrom grDevices colorRampPalette rgb
#' @importFrom magrittr
#' @importFrom tibble as_tibble
#' @return The RObject of a plot
#' @NoRd
cusColor <- function(RObj_outDf, mode="HSL", 
    HSLstart = 260, HSLend = 0,
    RGBstart=list(00, 22, 00), RGBend=list(255, 00, 00)){

    if (mode == "RGB"){
        for (id in RGBstart %>% length() %>% seq_len()){
            if (RGBstart[[id]][1] < 0){RGBstart[[id]][1] <- 0}
            if (RGBstart[[id]][1] > 255){RGBstart[[id]][1] <- 255}
            if (RGBend[[id]][1] < 0){RGBend[[id]][1] <- 0}
            if (RGBend[[id]][1] > 255){RGBend[[id]][1] <- 255}}

        RGBstart <- grDevices::rgb(RGBstart[[1]][1], RGBstart[[2]][1], RGBstart[[3]][1], maxColorValue=255)
        RGBend <- grDevices::rgb(RGBend[[1]][1], RGBend[[2]][1], RGBend[[3]][1], maxColorValue=255)
        if (TRUE){
            colorVec <- grDevices::colorRampPalette(c(RGBstart, RGBend))(
                nrow(RObj_outDf[["Pred"]][["predTokens"]]) + 
                nrow(RObj_outDf[["Pred"]][["predSents"]]) + 1
            ) %>% as.data.frame() %>% tibble::as_tibble()
            colnames(colorVec)[1] <- c("colorCode")
        }
        return (colorVec)
    }else{
        if (!HSLstart %>% is.numeric() || HSLstart > 360 || HSLstart < 0){
            HSLstart = 260
        }else if (!HSLend %>% is.numeric() || HSLend > 360 || HSLend < 0){
            HSLend = 0
        }
        colorVec <- colorspace::diverging_hcl(nrow(RObj_outDf[["Pred"]][["predTokens"]]) + 
                  nrow(RObj_outDf[["Pred"]][["predSents"]]) + 1,
                   h = c(HSLstart, HSLend), c = 80, l = c(30,90)) %>% 
                   as.data.frame() %>% tibble::as_tibble()
        colnames(colorVec)[1] <- c("colorCode")
        
        # hclplot(diverging_hcl(7, h = c(42.5, 85), c = 30, l = c(35,180)))
        # For the rainbow palette you can also select start/end color 
        # (red = 0, yellow = 1/6, green = 2/6, cyan = 3/6, blue
        # = 4/6 and magenta = 5/6) and saturation (s) and value (v): 
        # rainbow(n, s = 1, v = 1, start = 0, end = max(1, n - 1)/n, alpha = 1)
    
        return (colorVec)
    }

    return (NULL)
}

#' Get the color code for each language unit based on customed color by using cusColor()
#' @param embedObj (R_obj) The embeddings obj from textProjectionText()
#' @param cusColVec (list) The custom color vector of list(list(R,G,B), list(R,G,B)) if "mode" is "RGB". A list of two lists containing user-defined RGB values from 0-255, from starting to ending. If "mode" is "HSL", it is a list of two numbers: list(HSLstart, HSLend).
#' @param mode  (character) Select the "RGB" or "HSL" space to generate color. The "HSL" is the default and the recommeded mode.
#' @param restore (bool) If FALSE, change the input into customed color; If TRUE, change it back to default from textProjectionText().
#' @importFrom dplyr arrange
#' @return The color values list contained embedObj
#' @export
cusPlotText <- function(embedObj, cusColVec=NULL, mode="HSL", restore=FALSE){

    embedObj <- embedObj[1:2]
    temp <- NULL
    coloredTb <- matrix(nrow=nrow(embedObj[["Pred"]][["predTokens"]]) + 
      nrow(embedObj[["Pred"]][["predSents"]]) + 1,
     ncol=4) %>% as.data.frame()
    names(coloredTb) <- c("colorCode", "preds", "texts", "unitLang")
    start <- 1
    end <- nrow(embedObj[["Pred"]][["predTokens"]])
    coloredTb[start:end, 2:3] <- 
        embedObj[["Pred"]][["predTokens"]][, 2:3]
    coloredTb[start:end, 4] <- "tokens"
    start <- nrow(embedObj[["Pred"]][["predTokens"]]) + 1
    end <- nrow(embedObj[["Pred"]][["predTokens"]]) + nrow(embedObj[["Pred"]][["predSents"]])
    coloredTb[start:end, 2:3] <- embedObj[["Pred"]][["predSents"]][, 2:3]
    coloredTb[start:end, 4] <- "sents"
    coloredTb[nrow(coloredTb), 2] <- embedObj[["Pred"]][["predParas"]][[2]][1]
    coloredTb[nrow(coloredTb), 3] <- "Lorem Ipsum"
    coloredTb[nrow(coloredTb), 4] <- "paras"
    coloredTb_sorted <- dplyr::arrange(coloredTb, preds)
    if (restore){
        coloredTb_sorted[, 1] <- coloredTb_sorted[["preds"]] %>% map2Color(.)
    }else if (mode == "RGB"){
        coloredTb_sorted[, 1] <- embedObj %>% cusColor(., mode,
         RGBstart = cusColVec[[1]], RGBend = cusColVec[[2]])
    }else{
        coloredTb_sorted[, 1] <- embedObj %>% cusColor(., mode,
         HSLstart = cusColVec[[1]], HSLend = cusColVec[[2]])
    }
    temp <- dplyr::left_join(coloredTb[,2:ncol(coloredTb)], 
                            coloredTb_sorted[,1:2], by=c("preds"))
    coloredTb[,"colorCode"] <- temp[,"colorCode"] 
    embedObj <- append(embedObj, list("coloredTb"=coloredTb), length(embedObj) + 1)
    # add colorCode to tokens embed
    temp <- NULL
    temp <- embedObj[["coloredTb"]] %>% dplyr::filter(., unitLang=="tokens")
    embedObj[["Pred"]][["predTokens"]]["colorCode"] <- temp[["colorCode"]]
    # add colorCode to sents embed
    temp <- NULL
    temp <- embedObj[["coloredTb"]] %>% dplyr::filter(., unitLang=="sents")
    embedObj[["Pred"]][["predSents"]]["colorCode"] <- temp[["colorCode"]]
    # add colorCode to paras embed, and add the "Lorem Ipsum" place holder as the replacement.
    temp <- NULL
    temp <- embedObj[["coloredTb"]] %>% dplyr::filter(., unitLang=="paras")
    embedObj[["Pred"]][["predParas"]] <- temp[, 1:3]

    if (TRUE){
        output_out <- getOutDf(embedObj)
    }
   
    return (output_out)
}




# <table style="width:100%">
#   <tr style="text-align:center;padding:2px">
#     <td><span style="padding:2%;color:#000000;background-color:#001122">1</span></td>
#     <td><span style="padding:2%;color:#000000;background-color:#112200">2.5</span></td>
#     <td><span style="padding:2%;color:#000000;background-color:#221100">5</span></td>
#     <td><span style="padding:2%;color:#000000;background-color:#222222">7.5</span></td>
#     <td><span style="padding:2%;color:#000000;background-color:#333333">10</span></td>
#   </tr>
#   <tr style="text-align:center;padding:2px">
#     <td>0% quantile</td>
#     <td>25% quantile</td>
#     <td>50% quantile</td>
#     <td>75% quantile</td>
#     <td>100% quantile</td>
#   </tr>
# </table>