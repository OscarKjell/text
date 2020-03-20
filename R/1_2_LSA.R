

library(tidyverse)
library(quanteda)
library(lsa)

# First a function to create an LSA-based space;
# Second, functions to apply semantic representations from the LSA space to the words

# Test data
#data_raw <- read_csv("/Users/oscar/Desktop/0 Studies/5 R statistical semantics package/spaces/StudiesAll_Eng.csv", col_names = FALSE)


# Create LSA space; x = column with text; dims = number of dimensions to create, where dimcalc_share() is function from lsa that helps you select number of dimensions
textSpace <- function(x, dims = dimcalc_share()) {
  dtm <- quanteda::dfm(x, verbose = FALSE)

  # Get the "feature in document" co-occurrence matrix
  dtm_ok <- t(dtm) %*% dtm

  # Calculates a weighted document-term matrix according to the chosen local and/or global weighting scheme. help(lsa) help(lw_logtf)
  # From lsa package example: Create a vector space with Latent Semantic Analysis (LSA)
  # lw_logtf = lw(m) is one of the local weight functions lw_tf(), lw_logtf(), lw_bintf(), and
  # gw(m) is one of the global weight functions gw_normalisation(), gw_idf(), gw_gfidf(), entropy(), gw_entropy()
  # lw\_logtf() returns the logarithmised n \times m matrix. log(m_{i,j}+1) is applied on every cell.
  # gw\_idf() returns the inverse document frequency in a n \times m matrix. Every cell is 1 plus the logarithmus
  # of the number of documents divided by the number of documents where the term appears.
  dtm_weigthed = lsa::lw_logtf(as.matrix(dtm_ok)) * lsa::gw_idf(as.matrix(dtm_ok))
  # Create space
  lsa_space <- lsa::lsa(dtm_weigthed, dims = dims)

  # Splitting the two different matrices
  lsa_space_tk <- lsa_space$tk
  lsa_space_df <- lsa_space$dk

  # Add words, add them to a list and give the names
  lsa_space_tk <- tibble::rownames_to_column(as.data.frame(lsa_space_tk), "words")
  lsa_space_df <- tibble::rownames_to_column(as.data.frame(lsa_space_df), "words")
  space <- list(lsa_space_tk, lsa_space_df)
  names(space) <- c("tk", "df")
  space
}


# REMOVE

# Function to apply the semantic representation to ONE word; and return vector with NA if word is not found
applysemrep <- function(x, space = space){
  #If semrep is found get it; if not return NA vector of dimensions (which equal "Ndim"=space[["s"]][[14]] )
  if (sum(space$words==x[TRUE]) %in% 1) {
    x <- tolower(x)
    #Get the semantic representation for a word=x
    word1rep <- space[space$words==x, ]

    #Only get the semantic represenation as a vector without the actual word in the first column
    wordrep <- as_vector(word1rep[,2:length(word1rep)])

    # If the word does not have a semrep return vector with Ndim (512) dimensions of NA
  }else if (x %in% NA) {
    wordrep <- data.frame(matrix(ncol = length(space)-1, nrow = 1))
    class(wordrep)
    wordrep <- as.numeric(wordrep)
  } else {
    wordrep <- data.frame(matrix(ncol = length(space)-1, nrow = 1))
    wordrep <- as.numeric(wordrep)
  }
}


# Ad this to the other.

#  Function to take min, max, mean or the CLS (which comes from BERT models; not Static spaces) from list of vectors
textEmbeddingAggregation <- function(x, aggregation = "min"){
  if(aggregation == "min"){
    min_vector <- unlist(map(x, min, na.rm = TRUE))
  } else if (aggregation == "max") {
    max_vector <- unlist(map(x, max, na.rm = TRUE))
  } else if (aggregation == "mean") {
    mean_vector <- unlist(map(x, mean, na.rm = TRUE))
  } else if (aggregation == "CLS"){
    CLS <- x %>%
      dplyr::filter(token_index == 1, layer_index == 1)
  } else if (aggregation == "normalize1") {
#    norma_vector <- unlist(map(x, norma))
    x2 <- x[complete.cases(x), ]
    x3 <- colSums(x2)
    x4 <- normalize.vector(x3)
  }
}


# x <- c("happy", "joy") aggregate = "normalize1" space=
# if x= "" replace with NA
# Generic function to apply a common semantic representaion for ALL words in a CELL; and if there are no words return a Ndim vector with NAs
semanticrepresentation <- function(x, space = space, aggregate = "min") {
  x <- tolower(x)
  #Separates the words in a cell into a character vector with separate words.
  x <- data.frame(unlist(str_extract_all(x, "[[:alpha:]]+")))
  colnames(x) <- c("wordsAll1")
  x <- as.tibble(x)
  x <- as.character(x$wordsAll1)
  #If empty return a NA semantic representation
  if (length(x)== 0){
    x2 <- data.frame(matrix(ncol = length(space)-1, nrow = 1))
    x2 <- as.numeric(x2)
  }else{
    # Create a matrix with all the semantic representations using the function above
    #x1 <-  apply(x, 1, applysemrep)
    x1 <-  sapply(x, applysemrep, space)
    x1 <- tibble::as_tibble(t(x1))
    #IF more than one semrep; Sum all the semantic represenations; if not return it as is so that NA etc is returned/kept aggre
    x2 <- textEmbeddingAggregation(x1, aggregation = aggregate) #aggregate
    x2
    #If all values are 0 they should be NA instead; otherwise return the semantic representation.
    if (all(x2 == 0|x2 == Inf|x2 == -Inf | is.nan(x2)) == TRUE){
      x2 <- data.frame(matrix(ncol = length(space)-1, nrow = 1))
      x2 <- as.numeric(x2)
    }else{
      x2 <- x2
    }
  }
}



### Function applying semreps for all character variables and saves them in a list.
##This is done because applying semreps takes time to do over and over again.
#Apply semrep to all character variables; save them as tibbles in a list; where the tibbles are called the same as the original variable
textStaticSpace <- function(df, space=space, tk_df = "tk", aggregate = "min") {

    # Select the tk or dk matrrix derived from the lsa (svd)
  if(tk_df == "tk") {
  space <- tibble::as_tibble(space$tk)
  } else if (tk_df == "df"){
    space <- tibble:as_tibble(space$df)
  } else {
    space
    }
  # Select all character variables
  df_characters <-   select_if(df, is.character)

  # Create empty list
  list_semrep <- list()

  # For loop that apply the semrep to each character variable
  for (i in 1:length(df_characters)) {
    # Apply the semantic representation funtion to all rows; transpose the resulting matrix and making a tibble
    list_semrep[[i]] <- as_tibble(t(sapply(df_characters[[i]], semanticrepresentation, space, aggregate)))
  }

  # Gives the tibbles in the list the same name as the orginal character variables
  names(list_semrep) <- names(df_characters)
  list_semrep
}


# Testing
#x <- c("happy joy", "sad unhappy", "sadfsdfds ljhlj asdffd")
#y <- c(1, 2, 3)
#tbl <- tibble(x, y)

# Creating space
#data_comun_with_text <- data_raw
space_test <- textSpace(tbl$x)
space_test
space_test$tk

# Testing applying space
data_testing <- textStaticSpace(tbl, space=space_ok, aggregate = "max")
data_testing



