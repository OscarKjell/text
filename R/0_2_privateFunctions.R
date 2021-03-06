


#  devtools::document()
#' Make x and y into same length for when we will randomly draw K-folds from them
#' Function to add rows of NA until y and x have the same number of rows.
#' @param x a variable
#' @param y a variable
#' @return x and y have equal length.
#' @noRd
addEqualNrNArows <- function(x, y) {
  success <- FALSE
  while (!success) {
    # Add row with NA
    x <- rbind(x, rep(NA, length(x)))
    # check for success
    success <- nrow(x) == nrow(y)
  }
  return(x)
}


#  devtools::document()
#' Examine how the ordered data's mean of a statistics compare,
#' with the random data's null comparison distribution.
#' @param Observedresults a value representing the observed cosine.
#' @param NULLresults a tibble column with a NULL distribution of estimates (cosines).
# #' @param Npermutations number of permutation used in the test.
#' @param alternative type of test: "two_sided", "greater", "less".
#' @return p_value
#' @noRd
p_value_comparing_with_Null <- function(Observedresults,
                                        NULLresults,
                                        # Npermutations,
                                        alternative = c("two_sided", "less", "greater")) {

  NULLresults <- tibble::as_tibble_col(NULLresults)
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
    if (p_value == 0) {
      p_value <- 1 / (nrow(NULLresults) + 1)
    }
  }
  return(p_value)
}



#  devtools::document()
#' Cohen's D effect size
#'
#' @param x a variable.
#' @param y a variable..
#' @return p_value
#' @importFrom stats var
#' @noRd
cohens_d <- function(x, y) {
  lx <- length(x)- 1
  ly <- length(y)- 1

  # mean difference (numerator)
  md  <- abs(mean(x) - mean(y))
  # Sigma; denominator
  csd <- lx * var(x) + ly * var(y)
  csd <- csd/(lx + ly)
  csd <- sqrt(csd)

  cd  <- md/csd
  # Cohen's d
  cd
}





