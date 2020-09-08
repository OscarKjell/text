


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
#' Examine how the ordered data's mean of the cosine compare,
#' with the random data's null comparison distribution.
#' p_value_comparing_with_Null
#' @param NULLresults a vector with a NULL distribution of estimates (cosines).
#' @param Observedresults a value representing the observed cosine.
#' @param Npermutations number of permutation used in the test.
#' @param alternative type of test: "two_sided", "greater", "less".
#' @return p_value
#' @noRd
p_value_comparing_with_Null <- function(Observedresults,
                                        NULLresults,
                                        Npermutations,
                                        alternative = c("two_sided", "less", "greater")) {
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
      p_value <- 1 / (Npermutations + 1)
    }
  }
  return(p_value)
}

