

#' EXPERIMENTAL: Test whether there is a significant difference in meaning between two sets of texts
#' (i.e., between their word embeddings).
#' @param x Set of word embeddings from textEmbed.
#' @param y Set of word embeddings from textEmbed.
#' @param similarity_method Character string describing type of measure to be computed;
#' default is "cosine" (see also measures from textDistance (here computed as 1 - textDistance()) including
#' "euclidean", "maximum", "manhattan", "canberra", "binary" and "minkowski").
#' @param Npermutations Number of permutations (default 10000).
#' @param method Compute a "paired" or an "unpaired" test.
#' @param alternative Use a two or one-sided test (select one of: "two_sided", "less", "greater").
#' @param output.permutations If TRUE, returns permuted values in output.
#' @param N_cluster_nodes Number of cluster nodes to use (more makes computation faster; see parallel package).
#' @param seed Set different seed.
#' @return A list with a p-value, similarity score estimate and permuted values if output.permutations=TRUE.
#' @examples
#' x <- word_embeddings_4$texts$harmonywords
#' y <- word_embeddings_4$texts$satisfactionwords
#' textSimilarityTest(x,
#'   y,
#'   method = "paired",
#'   Npermutations = 100,
#'   N_cluster_nodes = 1,
#'   alternative = "two_sided"
#' )
#' @importFrom dplyr select starts_with
#' @importFrom parallel splitIndices mclapply
#' @export
textSimilarityTest <- function(x,
                               y,
                               similarity_method = "cosine",
                               Npermutations = 10000,
                               method = "paired",
                               alternative = "greater", # less = dissimalar; greater = similar; two-sided = a correlation; c("two_sided", "less", "greater"),
                               output.permutations = TRUE,
                               N_cluster_nodes = 1,
                               seed = 1001) {
  T1_textSimilarityTest <- Sys.time()

  x1 <- textDimName(x, dim_names = FALSE)
  y1 <- textDimName(y, dim_names = FALSE)

  set.seed(seed)
  if (method == "paired" & (nrow(x) != nrow(y))) {
    stop("x and y must have the same number of rows for a paired textSimilarityTest test.")
  }
  alternative <- match.arg(alternative)
  results_title <- paste(similarity_method, "_estimate", sep = "")
  results <- tibble::tibble("title1" = NA, "title2" = NA)
  colnames(results) <- c(results_title, "p.value")

  # Select variables beginning with Dim
  x1 <- dplyr::select(x1, dplyr::starts_with("Dim"))
  y1 <- dplyr::select(y1, dplyr::starts_with("Dim"))

  if (method == "paired") {
    # Compute similarity between all pairs
    ss_observed <- textSimilarity(x1, y1, method = similarity_method)
    # Compute the data's mean of the similarity scores
    results[1] <- mean(ss_observed) # removed abs(
  }

  if (method == "unpaired") {
    X_all <- tibble::as_tibble_row(textEmbeddingAggregation(x1, aggregation = "mean"))
    Y_all <- tibble::as_tibble_row(textEmbeddingAggregation(y1, aggregation = "mean"))

    # Compute similarity between the aggregated word embedding
    ss_observed <- textSimilarity(X_all, Y_all, method = similarity_method)

    # Compute the data's mean of the similarity scores
    results[1] <- mean(ss_observed) #removed abso
  }

  ### Compute comparison distribution of similarity scores based on randomly drawn word embeddings from both groups
  # adding groups together.
  x1y1 <- rbind(x1, y1)

  # Setting up parallel processing (Npermutations=100 Nthread = 2),
  splitix <- parallel::splitIndices(nx = Npermutations, ncl = N_cluster_nodes)
  splitix <- splitix[sapply(splitix, length) > 0]

  distribution_mean_ss_permutated <- parallel::mclapply(splitix, function(x, xx, yy) {
    mean_ss_permutated <- sapply(x, function(x, xx, yy) {
      # Get indixes for how to randomly split the word embeddings.
      indices <- sample(c(
        (rep(TRUE, ceiling(nrow(x1y1) / 2))),
        (rep(FALSE, floor(nrow(x1y1) / 2)))
      ),
      nrow(x1y1),
      replace = FALSE
      )
      # Randomly select word embeddings into two different data frames.
      rdata1 <- x1y1[indices, ]
      rdata2 <- x1y1[!indices, ]
      # Compute the semantic similarity between randomly drawn word embeddings and compute the mean.
      if (method == "paired") {
        rand_ss <- textSimilarity(rdata1, rdata2, method = similarity_method)
        return(mean(rand_ss)) # removed abs(
      }
      if (method == "unpaired") {
        R1_all <- tibble::as_tibble_row(textEmbeddingAggregation(rdata1, aggregation = "mean"))
        R2_all <- tibble::as_tibble_row(textEmbeddingAggregation(rdata2, aggregation = "mean"))

        # Compute similarity between the summed word embedding
        rand_ss <- textSimilarity(R1_all, R2_all, method = similarity_method)

        # Compute the data's mean of the similarity scores
        return(rand_ss) # removed abs()
      }
    }, xx = xx, yy = yy)
    return(mean_ss_permutated)
  }, xx = x, yy = y)
  NULLresults <- unlist(distribution_mean_ss_permutated)
  # Examine how the ordered data's mean of the similarity scores compare with the random data's,
  # null comparison distribution
  p_value <- p_value_comparing_with_Null(NULLresults,
    Observedresult = results[[1]],
    alternative = alternative
  )
  results["p.value"] <- p_value
  results <- as.list(results)

  if (output.permutations) {
    random.estimates.4.null <- list(NULLresults)
    names(random.estimates.4.null) <- "random.estimates.4.null"
  } else {
    random.estimates.4.null <- NULL
  }

  x_name <- deparse(substitute(x))
  y_name <- deparse(substitute(y))
  embedding_descriptions_x <- paste(x_name, ":", comment(x), col = "")
  embedding_descriptions_y <- paste(y_name, ":", comment(y), col = "")

  # Time
  T2_textSimilarityTest <- Sys.time()
  Time_textSimilarityTest <- T2_textSimilarityTest - T1_textSimilarityTest
  Time_textSimilarityTest <- sprintf(
    "Duration to run the test: %f %s", Time_textSimilarityTest,
    units(Time_textSimilarityTest)
  )
  Date_textSimilarityTest <- Sys.time()
  time_date <- paste(Time_textSimilarityTest,
    "; Date created: ", Date_textSimilarityTest,
    sep = "",
    collapse = " "
  )

  test_description <- paste("permutations = ", Npermutations,
    "similarity_method = ", similarity_method,
    "method = ", method,
    "alternative = ", alternative,
    collapse = " "
  )

  descriptions <- c(
    embedding_descriptions_x,
    embedding_descriptions_y,
    test_description,
    time_date
  )
  names(descriptions) <- c("embedding_x", "embedding_y", "test_description", "time_date")

  results <- c(
    random.estimates.4.null,
    descriptions,
    results
  )
  results
}
