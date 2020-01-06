
# textSimilairty textTtest
# usethis::use_package(psych==1.8.10)


########################################################################
########
########     textSimilarity [NOW I AM NORMALIZING IT; perhaps shouldn't!]
########
########################################################################
#First, creat cosine function
#Second creat function that computes the cosine between columns
#

# Computing the cosine between two semantic represenations
cosines <- function(x, y) {
  rowSums(x*y, na.rm=TRUE) / ( sqrt(rowSums(x * x, na.rm=TRUE)) * sqrt(rowSums(y * y, na.rm=TRUE)) )
}
#devtools::document()
#?textSimilarity
#Function get word embeddings and then compute COSINE
#x and y are the semantic representations from column x and y, which have been imported with textImport
#\code  \link{} \link{} @section

#' textSimilarity computes the semantic similiarty between texts.
#'
#' @param x Wordembeddings from textImport.
#' @param y Wordembeddings from textImport.
#' @return A vector with semantic similarity scores (based on cosine).
#' @examples
#' library(dplyr)
#' wordembeddings <- wordembeddings4_10
#' similiarty_scores <- textSimilarity(wordembeddings$harmonytext, wordembeddings$satisfactiontext)
#' @seealso see \code{\link{textSimilarityNorm}} and \code{\link{textTtest}}
#' @export

textSimilarity <- function(x,y) {
  #Remove unnecassary columns
  x1 <- subset(x, select=-c(1:5))
  y1 <- subset(y, select=-c(1:5))

  #Apply the cosines functions
  cosines(x1, y1)
}

########################################################################
########
########     semSimilarityNorm: Compute Semantic similarity between a column and a word-norm
########
########################################################################
#devtools::document()
#?textSimilarityNorm
#Function get word embeddings and then compute COSINE
#x and y are the semantic representations from column x and y, which have been imported with textImport
#' textSimilarityNorm computes the semantic similiarty between a character variable and a word norm (i.e., a text in one cell).
#'
#' @param x Wordembeddings from textImport (with several rows of text).
#' @param y Wordembeddings from textImport (with only one text).
#' @return A vector with semantic similarity scores (based on cosine).
#' @examples
#'
#' harmonynorm <- c("harmony peace cooperation balance")
#' satisfactionnorm <- c("satisfaction achievement job fulfilled")
#' library(tibble)
#' norms <- tibble::tibble(harmonynorm, satisfactionnorm)
#' library(dplyr)
#' wordembeddings <- wordembeddings4_10
#' wordembeddings_wordnorm <- textImport(norms)
#' similiarty_scores <- textSimilarityNorm(wordembeddings$harmonytext,
#' wordembeddings_wordnorm$harmonynorm)
#' @seealso see \code{\link{textSimilarity}} and \code{\link{textTtest}}
#' @importFrom dplyr row_number
#' @export

textSimilarityNorm <- function(x,y) {
  #Remove unnecassary columns
  x1 <- subset(x, select=-c(1:5))
  y1 <- subset(y, select=-c(1:5))

  y2 <- y1 %>%
    dplyr::slice(rep(row_number(), nrow(x1)))

  #Apply the cosines functions
  cosines(x1, y2)
}


########################################################################
########
########      textTtest (and first textTtestscores). SHOULD I REALLY NORMALIZE
########
########################################################################
#1. semTtest(x, y, type) shows the results for the t-test, type = Student, Repeated/Paired, Walsh
#2. semTtestscores(x, y) gives the scores that one can use for t-test, or ANOVA etc.


#Function to normlaise the vector to one; unit vector
normalizeV <- function(x) {x / sqrt(sum(x^2, na.rm = TRUE))}

#devtools::document()
#?textTtestscores
#Function get word embeddings and then compute COSINE
#x and y are the semantic representations from column x and y, which have been imported with textImport

## Make x and y into same length for when we will randomly draw K-folds from them
# Function to add rows of NA until y and x have the same amount of rows.
addEqualNrNArows <- function(x, y, Ndim) {
  success <- FALSE
  while (!success) {
    # Add row with NA
    x <- rbind(x, rep(NA, Ndim))
    # check for success
    success <- nrow(x) == nrow(y)
  }
  return(x)
}

# devtools::document()
# ?textTtestscores
# Function get word embeddings and then compute COSINE
# x and y are the semantic representations from column x and y, which have been imported with textImport
#' textTtestscores computes the semantic similiarty between a character variable and a word norm (i.e., a text in one cell).
#'
#' @param x Wordembeddings from textImport.
#' @param y Wordembeddings from textImport.
#' @param nrFolds Number of folds used.
#' @param Ndim Number of dimension used (RBERT use 768)
#' @return A vector with semantic similarity scores (based on cosine).
#' @examples
#' wordembeddings <- wordembeddings4_10
#' similiarty_scores <- textTtestscores(wordembeddings$harmonytext, wordembeddings$satisfactiontext)
#' @seealso see \code{\link{textTtest}}
#' @export
## Function that creates semnatic t-test scores
textTtestscores <- function(x, y, nrFolds=10, Ndim = 768) {

  # Apply semrpe if variables are characters (i.e., they have not already been imported)
  #  if (is.character(x)==TRUE){
  # Sum all semreps in the seperate groups that are to be compared.
  #    x <- as_tibble(t(sapply(x, semanticrepresentation)))
  #  }else{
  #    x <- x
  #  }
  #  if (is.character(y)==TRUE){
  # Sum all semreps in the seperate groups that are to be compared.
  #    y <- as_tibble(t(sapply(y, semanticrepresentation)))
  #  }else{
  #    y <- y
  #  }
  x <- subset(x, select=-c(1:5))
  y <- subset(y, select=-c(1:5))

  # see function above
  # If statement deciding which of x or y that needs row(s) of NA
  if (nrow(x) < nrow(y) ) {
    x <- addEqualNrNArows(x, y)
  }else if (nrow(x) > nrow(y)){
    y <- addEqualNrNArows(y, x)
  }else{
    x <- x
    y <- y
  }

  ### For loop that takes one minus the other to create a semantic difference representation; using k-fold procedure
  # x <- add_column(x, 1:nrow(x), .before = 1)
  nrF <- nrFolds
  folds <- c(1:nrow(x))

  # set.seed(12352)
  folds <- caret::createFolds(folds, k = nrF, list = TRUE, returnTrain = FALSE)
  folds

  semanticTtestscoreslistX <- list()
  semanticTtestscoreslistY <- list()

  for (i in 1:nrF) {
    ### Summ all semrep in one column and normlise to one
    colXsemrep <- signif(colSums(x[ -folds[[i]], ], na.rm = TRUE), 5)
    colXsemrep <- signif(normalizeV(colXsemrep), 5)

    colYsemrep <- signif(colSums(y[ -folds[[i]], ], na.rm = TRUE), 5)
    colYsemrep <- signif(normalizeV(colYsemrep), 5)

    ### The Semantic Difference Represenationa: Take colX minus colY
    semDifRep <- signif(colXsemrep - colYsemrep, 5)

    # Measure the semantic simlairty score between each responses NOT-included-in-the-semDifRep and the semDifRep (i.e., the semantic difference representation)
    # Should I normalise them before(!?)
    # normX <- normalizeV(x[ folds[[i]], ])
    # normY <- normalizeV(y[ folds[[i]], ])
    normX <- signif(x[ folds[[i]], ], 5)
    normY <- signif(y[ folds[[i]], ], 5)


    # Adds the Semantic Difference Representation into a Tibble and then duplicates it to as many rows as it will be compared to with x (nrow(normX))
    # Computing the SS between Word data and Semantic Difference Representations for X and Y
    semDifRep1 <- tibble::as_tibble(rbind(semDifRep))
    semDifRep1 <- semDifRep1[rep(seq_len(nrow(semDifRep1)), each=nrow(normX)),]
    SSsemDifRepX <- signif(cosines(normX, semDifRep1), 5)

    semDifRep1 <- tibble::as_tibble(rbind(semDifRep))
    semDifRep1 <- semDifRep1[rep(seq_len(nrow(semDifRep1)), each=nrow(normY)),]
    SSsemDifRepY <- signif(cosines(normY, semDifRep1), 5)

    # Lists to save the restuls in
    semanticTtestscoreslistX[[i]] <- list(SSsemDifRepX)
    semanticTtestscoreslistY[[i]] <- list(SSsemDifRepY)

  }
  # Sorting out a dataframe for the resuts.
  semanticTtestscoreslistXdone <- unlist(semanticTtestscoreslistX)
  semanticTtestscoreslistYdone <- unlist(semanticTtestscoreslistY)
  semanticTtestscoreslistXYdone <- data.frame(semanticTtestscoreslistXdone, semanticTtestscoreslistYdone)
}



##############################################################################################################
####### Semantic T-test function; the "..." makes it possible to use the setting from t.test function
#d evtools::document()
# ?textTtest
# Function get word embeddings and then compute COSINE
# x and y are the semantic representations from column x and y, which have been imported with textImport
#' textTtest computes the semantic similiarty between a character variable and a word norm (i.e., a text in one cell).
#'
#' @param x wordembeddings from textImport (with several rows of text).
#' @param y wordembeddings from textImport (with only one text).
#' @param nrFolds Number of folds used in the creation of semnatic Ttest scores.
#' @param ... Arguments from t.test function.
#' @return Results from a t-test including p-values, t-statistics and cohen's D.
#' @examples
#' wordembeddings <- wordembeddings4_10
#' Harmony_Satisfaction_ttest <- textTtest(wordembeddings$harmonytext, wordembeddings$satisfactiontext)
#' Harmony_Satisfaction_ttest
#' @seealso see \code{\link{textTrain}} and \code{\link{textSimilarity}}
#' @importFrom psych cohen.d
#' @importFrom stats t.test
#' @export

textTtest <- function(x, y, nrFolds=10, ...){
  # Creating the SSS between word data and semDifRep
  result <- textTtestscores(x, y, nrFolds)
  x <- result$semanticTtestscoreslistXdone
  y <- result$semanticTtestscoreslistYdone

  # Cohen's D library(tibble)
  xy <- tibble::tibble(c(x , y), c(rep(1, length(x)) , rep(2, length(y))))
  names(xy) <- c("x", "group")
  # xy$group <- as_factor(xy$group)
  CohensD <- cohen.d(xy, group="group", alpha=.05, std=TRUE) #, pooled=TRUE, paired=FALSE, na.rm=FALSE, hedges.correction=FALSE, conf.level=0.95, ...)

  # T-test
  ttest <- t.test(x, y, ...)

  # Output
  output_stt <- list(CohensD, ttest)
  output_stt
}

# The newer version of psych gave error when using cohen'd
# packageurl <- "http://cran.r-project.org/src/contrib/Archive/psych/psych_1.8.10.tar.gz"
# install.packages(packageurl, repos=NULL, type="source")
# library(psych)


