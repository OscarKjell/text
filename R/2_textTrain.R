# usethis::use_package("caret")
# library(caret)
# usethis::use_data()

### semTrain: Training the word embeddings to numeric output

# semTrainMulti: Adding several text-variables together with individual pca:s
# semTrainMCV:

########################################################################
########
########     textTrain: Training the word embeddings to numeric output
########
########################################################################
# x is V1 - V778
# y is numeric variable

# Split up sentences (NOW ONLY 512 tokens!!!);
# Extract pre-trained BERT-embeddings)

# ?textTrain
# help("textTrain")

# devtools::document()
#' textTrain trains word embeddings to a numeric variable.
#'
#' @param x Wordembeddings from textImport.
#' @param y A numeric variables to predict.
#' @param nrFolds_k Number of folds to use.
#' @param methodTrain Method used to train representations.
#' @param preProcessTrain Preprocessing training.
#' @param preProcessThresh Preprocessing threshold.
#' @param methodCor Type of correlation used in evaluation.
#' @param describe_model Input text to describe your model.
#' @param ... Arguments from caret::createFolds, caret::trainControl and caret::train.
#' @return A correlation between predicted and observed values; as well as predicted values.
#' @examples
#' wordembeddings <- wordembeddings4_10
#' ratings_data <- sq_data_tutorial4_10
#' wordembeddings <- textTrain(wordembeddings$harmonytext, ratings_data$hilstotal, nrFolds_k=2)
#' @seealso see \code{\link{textTtest}}
#' @importFrom stats cor.test
#' @export

textTrain <- function(x, y,  nrFolds_k=10, methodTrain = "ridge", preProcessTrain = 'pca', preProcessThresh = 0.95, methodCor = "pearson", describe_model = "Describe the model and share it with others", ...){
  # Preparing a df for training: Import data if x is character or use semreps if it has already been imported
  if (is.character(x) == TRUE) {
    df <- tibble::tibble(x, y)
    df1 <- text::textImport(df)
    df2 <- cbind(df1, df[2])
    df2 <- tibble::as_tibble(df2)
  } else {
    df2 <- cbind(x, tibble::enframe(y))
    df2 <- dplyr::rename(df2, y = "value") #perhaps value should not be within ".."?
    df2 <- tibble::as_tibble(df2)
  }
  # Remove unnecassary columns
  df3 <- subset(df2, select=-c(1:5))

  ## Training
  # Creating folds for outer cross-validation, so that the data is split into 90% for inner cv, and 10% for testing with that particular semrep dimesion solution; see solution from https://stats.stackexchange.com/questions/125843/outer-crossvalidation-cycle-in-caret-package-r
  ytrain_df3 <- c(1:nrow(df3)) # number of observation that should be split in to the number of folds.
  ntrain <- length(ytrain_df3)
  # define folds
  cv_folds <- caret::createFolds(ytrain_df3, k = nrFolds_k, list = TRUE, returnTrain = TRUE, ...)
  # define training control
  train_control <- caret::trainControl(method="cv", index = cv_folds, savePredictions = 'final', ...)
  # train the model
  model <- caret::train(y~., data=df3, trControl=train_control, method= methodTrain, preProcess = preProcessTrain, thresh = preProcessThresh, ...)

  # Saving Output
  outputST <- list(model, model$pred, summary(model$pred), describe_model, cor.test(model$pred$pred, model$pred$obs, method = methodCor))
  names(outputST) <- c("Model", "Model_pred", "Summary", "Model_Desciption", "Correlation")
  outputST
}


########################################################################
########
########  semTrainMulti: Training the word embeddings to numeric output
########
########################################################################

# semTrainMulti(x=c(wordembeddings$harmonytexts, wordembeddings$satisfactiontexts), sq_data_short$hilstotal)
# y <- sq_data_short$hilstotal
# x <- wordembeddings$harmonytexts
# x <- wordembeddings
#
# x[1]
# x[2]
# is.list(length(x))
# x
# is.list(x)
# length(x)
# lengths(x)
# nlists(x)
#
# semTrainMulti <- function(x, y, nrFolds_k = 10, methodTrain = "ridge", preProcessTrain = 'pca', methodCor = "pearson", ...){
#   #
#   df2 <- x
#   df2 <-  subset(df2, select=-c(1:5))
#   df2$y <- y
#   #Remove unnecassary columns in each list
#   df3 <- lapply(df2, FUN = function(x) subset(x, select=-c(1:5)))
#   df3
#   #preProcss each list with PCA
#   df3 <- preProcess(df2, method="pca", thresh = 0.95)
#   df4 <- predict(df3, df2)
#   df4
#   df4$y <- y
#
#   #Add the lists together and add y to df3 (here I could also add numeric variables :) )
#
#
#   ###Training
#   # Creating folds for outer cross-validation, so that the data is split into 90% for inner cv, and 10% for testing with that particular semrep dimesion solution; see solution from https://stats.stackexchange.com/questions/125843/outer-crossvalidation-cycle-in-caret-package-r
#   library(caret)
#   ytrain_df3 <- c(1:nrow(df2)) # number of observation that should be split in to the number of folds.
#   ntrain <- length(ytrain_df3)
#   # define folds
#   cv_folds <- createFolds(ytrain_df3, k = 10, list = TRUE, returnTrain = TRUE) #, ...)
#   # define training control
#   train_control <- trainControl(method="cv", index = cv_folds, savePredictions = 'final') #, ...)
#   # train the model
#   set.seed(1)
#   model <- caret::train(y~., data=df2, trControl=train_control, method= methodTrain, preProcess = preProcessTrain)
#   warnings()
#   #Saving Output
#   outputST <- list(model, model$pred, summary(model$pred), cor.test(model$pred$pred, model$pred$obs, method = methodCor))
#   names(outputST) <- c("Model", "Model_pred", "Summary", "Correlation")
#   outputST
# }
#

