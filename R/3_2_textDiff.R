# https://rdrr.io/bioc/PharmacoGx/src/R/cosinePerm.R

#
#
#
#library(text)


# Examine how the ordered data's mean of the cosine compare with the random data's, null comparison distribution help(switch)
#  devtools::document()
#' p_value_comparing_with_Null
#'
#' @param NULLresults a vector with NULL distribution of estimate (cosines)
#' @param Observedresults a value representing the observed cosine
#' @param Npermutations Number of permutation used in the test
#' @param alternative "two_sided", "greater", "less"
#' @return p_value
#' @noRd
p_value_comparing_with_Null <- function(Observedresults, NULLresults,  Npermutations, alternative = c("two_sided", "less", "greater")){
  switch(alternative,
         "two_sided" = {
           p_value <- 2 * (min(sum(NULLresults < Observedresults), sum(NULLresults > Observedresults)) / sum(!is.na(NULLresults)))
           },
         "less" = {
           p_value <- sum(NULLresults < Observedresults) / sum(!is.na(NULLresults))
           },
         "greater" = {
           p_value <- sum(NULLresults > Observedresults) / sum(!is.na(NULLresults))
           }
  )
  if (!is.na(p_value)) {
    if (p_value == 0) { p_value <- 1 / (Npermutations + 1) }
  }
  return(p_value)
}


# For Observed data we have = Mean and SD (perhaps I could bootstrap and Mean of means? )

# For Permutated data we have = Mean of means and the SD of the Mean of means.; which then becomes the standard error?


# x <- wordembeddings4_10$harmonywords
# y <- wordembeddings4_10$satisfactionwords
# devtools::document()
#' textDiff: Test whether there is a significant difference between two sets of texts
#' (i.e., between their word embeddings). This is achieved using permutation, in the following steps:
#'
#' @param x set of word embeddings from textEmbedd.
#' @param y set of word embeddings from textEmbedd.
#' @param Npermutations number of permatation (default 1000).
#' @param method compute a "paired" or "unpaired" test.
#' @param alternative wheather to use a two or one-sided test.
#' @param output.permutations If TRUE returns permuted values in output.
#' @param N_cluster_nodes Number of cluster nodes to use (more makes computation faster; see parallel package.
#' @return A list with a p-value, estimate and perumted values if output.permutations=TRUE.
#' @examples
#' x <- wordembeddings4_10$harmonywords
#' y <- wordembeddings4_10$satisfactionwords
#' textDiff(x, y, method = "paired", Npermutations = 10, N_cluster_nodes = 1)
#' @importFrom dplyr select starts_with
#' @importFrom lsa cosine
#' @importFrom parallel splitIndices mclapply
#' @export

textDiff <- function(x, y, Npermutations = 1000, method = "paired", alternative = c("two_sided", "less", "greater"), output.permutations = TRUE, N_cluster_nodes = 1) {
  set.seed(2020)
  if ((nrow(x) != nrow(y))) { stop("x and y must have the same number of rows for a paired textDiff test.") }
  alternative <- match.arg(alternative)
  results <- c("estimate" = NA, "p.value" = NA)

  # Select variables beginning with V
  x1 <- dplyr::select(x, dplyr::starts_with("V"))
  y1 <- dplyr::select(y, dplyr::starts_with("V"))

  if(method == "paired") {
    # Compute cosine between all pairs
    cosine_observed <- cosines(x1, y1)
    # Compute the data's mean of the cosine
    results["estimate"] <- mean(abs(cosine_observed))
    }

  if(method == "unpaired"){
    X_all <- textEmbeddingAggregation(x1, aggregation = "mean")
    Y_all <- textEmbeddingAggregation(y1, aggregation = "mean")
    # Compute cosine between the simmed word embedding
    cosine_observed <- lsa::cosine(X_all, Y_all)
    # Compute the data's mean of the cosine
    results["estimate"] <- mean(abs(cosine_observed))
    }

  ### Compute comparison distrubution of cosine based on randomly drawn word embeddings from both groups
  # adding groups together
  x1y1 <- rbind(x1, y1)

  # Setting up parallel processing; Npermutations=100 Nthread = 2 help(splitIndices)
  splitix <- parallel::splitIndices(nx = Npermutations, ncl = N_cluster_nodes)
  splitix <- splitix[sapply(splitix, length) > 0]

  distribution_mean_cosine_permutated <- parallel::mclapply(splitix, function(x, xx, yy) {
    mean_cosine_permutated <- sapply(x, function(x, xx, yy) {
      # Get indices for how to randomly split the word embeddings
      indices <- sample(c((rep(TRUE, nrow(x1y1)/2)), (rep(FALSE, nrow(x1y1)/2))), nrow(x1y1), replace=FALSE)
      # Randomly select word embeddings into two different data frames
      rdata1 <- x1y1[indices, ]
      rdata2 <- x1y1[!indices, ]
      # Compute the cosine between randomly drawn word embeddings and compute the mean
      if(method == "paired") {
        rcosines <- cosines(rdata1, rdata2)
        return(mean(abs(rcosines)))
      }
      if(method == "unpaired") {
        R1_all <- textEmbeddingAggregation(rdata1, aggregation = "mean")
        R2_all <- textEmbeddingAggregation(rdata2, aggregation = "mean")
        # Compute cosine between the summed word embedding
        rcosines <- lsa::cosine(R1_all, R2_all)
        # Compute the data's mean of the cosine
        return(abs(rcosines))
      }
    }, xx=xx, yy=yy)
    return (mean_cosine_permutated)
  }, xx=x, yy=y)
  NULLresults <- unlist(distribution_mean_cosine_permutated)

  # Examine how the ordered data's mean of the cosine compare with the random data's, null comparison distribution help(switch)
  p_value <- p_value_comparing_with_Null(NULLresults, Observedresults=results["estimate"], Npermutations = Npermutations, alternative=alternative)
  results["p.value"] <- p_value
  results <- as.list(results)

  if (output.permutations) {
  results <- c(list("random.estimates.4.null" = NULLresults, results))
  results
  }


}

######
# library(text)
# textDiff(wordembeddings4_10$harmonywords, wordembeddings4_10$satisfactionwords, Npermutation = 10, alternative="two_sided", N_cluster_nodes = 2)

# T1_2core <- Sys.time()
# test_pair <- textDiff(x, y, Npermutation = 1000, method = "paired", alternative="greater", N_cluster_nodes=2, output.permutations=TRUE)
# test2_unpair <- textDiff(x, y, Npermutation = 10, method = "unpaired", alternative="greater", N_cluster_nodes=2, output.permutations=TRUE)
# T2_2core <- Sys.time()
# T2_2core-T1_2core
# test2
#
#
# textttest_test <- textTtest(x, y)
# textttest_test

