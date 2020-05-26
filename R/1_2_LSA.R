

#library(tidyverse)
#library(quanteda)
#library(lsa)

# First a function to create an LSA-based space;
# Second, functions to apply semantic representations from the LSA space to the words

# library(Matrix) help(%*%)
# #library(import)
# insert link in the future to \code{\link{textEmbed}}
# devtools::document()
#' textSpace: Create an LSA-based space with semantic representation for each word
#'
#' @param x column with text.
#' @param dims number of dimensions to create, where the default dimcalc_share() is a
#' function from lsa that assisst in selecting number of dimensions.
#' @return A list with two spaces ("tk" and "df") with words in the first column, followed
#' by N number of columns representing the words' semantic representation.
#' @examples
#' example_rawdata <- sq_data_tutorial8_10$harmonywords
#' space <- textSpace(example_rawdata)
#' @seealso see \code{\link{textStaticSpace}}
#' @importFrom quanteda dfm
#' @importFrom lsa lw_logtf gw_idf lsa dimcalc_share
#' @importFrom tibble rownames_to_column
#' @importFrom Matrix t
#' @export
# library(text)
# x <- sq_data_tutorial8_10$harmonywords
textSpace <- function(x, dims = lsa::dimcalc_share()) {
  dtm <- quanteda::dfm(x, verbose = FALSE)
  # help(Matrix)
  # detach("package:Matrix", unload=TRUE)
  # Get the "feature in document" co-occurrence matrix
  dtm_ok <- Matrix::t(dtm) %*% dtm
  # import::from("Matrix", "%*%")
  # dtm_ok <- t(dtm) %&% dtm

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
  colnames(lsa_space_tk) <- c(paste("V", 1:(ncol(lsa_space_tk)), sep=""))
  lsa_space_tk <- tibble::rownames_to_column(as.data.frame(lsa_space_tk), "words")

  lsa_space_df <- lsa_space$dk
  colnames(lsa_space_df) <- c(paste("V", 1:(ncol(lsa_space_df)), sep=""))
  lsa_space_df <- tibble::rownames_to_column(as.data.frame(lsa_space_df), "words")

  # Add them to a list and give the names
  space <- list(lsa_space_tk, lsa_space_df)
  names(space) <- c("tk", "df")
  space
}

# insert link in the future to \code{\link{textEmbed}}
# devtools::document()
#' textStaticSpace: Applies semantic representations to all character variables
#'
#' @param df dataframe with that at least contain one character column.
#' @param space semantic space from textSpace.
#' @param tk_df option to use either the "tk" of "df" space from textSpace.
#' @param aggregate method to aggregate semantic representation when their are more than a single word.
#' (default is "mean"; see also "min" and "max")
#' @return A list with tibbles for each character variable. Each tibble comprises a column with the text, followed by
#' collumns representing the semantic representations of the text. The tibbles are called the same as the original variable.
#' @seealso see \code{\link{textSpace}}
#' @importFrom tibble as_tibble
#' @importFrom dplyr select_if
#' @export
textStaticSpace <- function(df, space, tk_df = "tk", aggregate = "mean") {

    # Select the tk or dk matrrix derived from the lsa (svd)
  if(tk_df == "tk") {
    space <- tibble::as_tibble(space$tk)
  } else if (tk_df == "df"){
    space <- tibble::as_tibble(space$df)
  } else {
    space
    }
  # Select all character variables
  df_characters <-   dplyr::select_if(df, is.character)

  # Create empty list
  list_semrep <- list()
  # Send the space to Semanticrepresentation function as single_wordembeddings2
  single_wordembeddings2 <- space
  single_wordembeddings1 <- space
  # For loop that apply the semrep to each character variable
  for (i in 1:length(df_characters)) {
    # Apply the semantic representation funtion to all rows; transpose the resulting matrix and making a tibble
    list_semrep[[i]] <- tibble::as_tibble(t(sapply(df_characters[[i]], semanticrepresentation, single_wordembeddings2, aggregate, single_wordembeddings1=single_wordembeddings1)))
  }

  # Add single word embeddings used for plotting
  singlewords <- getUniqueWordsAndFreq(df_characters)
  output_vectors_sw <- map(singlewords$words, applysemrep,  single_wordembeddings1)
  names(output_vectors_sw) <- singlewords$words
  output_vectors_sw2 <- bind_cols(output_vectors_sw)
  singlewords_we <- bind_cols(singlewords, as_tibble(t(output_vectors_sw2)))

  # Gives the tibbles in the list the same name as the orginal character variables
  names(list_semrep) <- names(df_characters)
  list_semrep$singlewords_we <- singlewords_we
  list_semrep
}


# # Testing
# x <- c("happy joy", "sad unhappy"), "sadfsdfds ljhlj asdffd")
# y <- c(1, 2), 3)
# tbl <- tibble(x, y)
#
# # Creating space
# #data_comun_with_text <- data_raw
# space_test <- textSpace(tbl$x)
# space_test
# space_test$tk
# Testing applying space
#data_testing <- textStaticSpace(tbl, space=space_test, tk_df = "tk",
#                                aggregate = "max")
#data_testing






# # Glove
# library(text2vec)
# #install.packages("text2vec")
#
# word_data <- data_space_raw$X1
#
#
# # Create iterator over tokens
# tokens = space_tokenizer(word_data)
# # Create vocabulary. Terms will be unigrams (simple words).
# it = itoken(tokens, progressbar = FALSE)
# vocab = create_vocabulary(it)
#
#
# vocab = prune_vocabulary(vocab, term_count_min = 2L)
#
# # Use our filtered vocabulary
# vectorizer = vocab_vectorizer(vocab)
# # use window of 5 for context words
# tcm = create_tcm(it, vectorizer, skip_grams_window = 10L)
#
# #glove = GlobalVectors$new(word_vectors_size = 50, x_max = 10)
# glove = GlobalVectors$new(rank = 50, x_max = 10)
#
# wv_main = glove$fit_transform(tcm, n_iter = 10, convergence_tol = 0.01, n_threads = 8)
#
# dim(wv_main)
#
#
# glove = GlobalVectors$new(word_vectors_size = 150, vocabulary = vocab, x_max = 10)
#
# #glove = GlobalVectors$new(rank = 50, vocabulary = vocab, x_max = 10)
# # `glove` object will be modified by `fit_transform()` call !
# wv_main = fit_transform(tcm, glove, n_iter = 20)
#
# wv_context = glove$components
# dim(wv_context)
#
#
# word_vectors = wv_main + t(wv_context)
#
#
# space_glove <- tibble::as_tibble(tibble::rownames_to_column(as.data.frame(word_vectors), "words"))
#
# solmini <- read_csv("/Users/oscar/Desktop/0 Studies/13 OnlineMini/combinedSOL_SM.csv")
# nrow(solmini)
#
# solmini_norm_test     <- textStaticSpace(solmini, space = space_glove, tk_df = " ", aggregate  = "normalize1")
#
# train_tk_phq_min_test <- textTrain(solmini_norm_test$dep_all, solmini$phq_tot)  # 50 dims: 0.5973861; 150 dims:
# semantictrainingtest_textspace_mean <- semanticTraining(solmini_norm_test$dep_all, solmini$phq_tot, Ndim = 150) # 50: 0.6059; 150:
#
#
# solmini_norm_test2     <- textStaticSpace(solmini, space = space_glove, tk_df = " ", aggregate  = "min")
#
# train_tk_phq_min_test2 <- textTrain(solmini_norm_test2$dep_all, solmini$phq_tot)  # 150:
# semantictrainingtest_textspace_mean2 <- semanticTraining(solmini_norm_test2$dep_all, solmini$phq_tot, Ndim = 150) # 150:
#



