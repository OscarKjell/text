# =============================================================================
# textWordPrediction.R
# =============================================================================
# A complement to textProjection() that works at the INDIVIDUAL WORD level.
#
# Key distinction from textProjection():
#   - textProjection()    : uses full *contextualized* sentence embeddings,
#                           projected onto an axis defined by a numeric variable.
#   - textWordPrediction(): uses *decontextualized* single-word embeddings.
#                           Each unique word gets a score based on the mean
#                           value of `x` (and optionally `y`) over the
#                           responses in which that word appeared. A ridge
#                           regression predicts that mean from the word's
#                           embedding, so the approach generalises to words
#                           NOT seen in training.
#
# For each word type:
#   m   = frequency-weighted mean of x over the participants who used the word
#         (token counts from an NLTK count matrix)
#   pos = leave-one-out ridge prediction of m from the word's embedding
#         (the plotted position)
#
# With the ridge penalty fixed (chosen once on the observed x by minimum
# leave-one-out error), both steps are linear in x, so the whole word-score
# vector is pos = A x with A computed once. Each permutation of x is then a
# single matrix product, which makes 10,000 participant permutations take
# seconds; every word is compared with its own null distribution:
# p = (1 + #{|null| >= |observed|}) / (Npermutations + 1).
#
# Output is designed to be passed directly to textProjectionPlot().
# =============================================================================


#' Compute word-level prediction scores and permutation-based p-values (EXPERIMENTAL)
#'
#' \code{textWordPrediction()} scores each unique word by the mean value of
#' \code{x} over the responses in which the word appears (weighted by how
#' often it appears), predicts that mean from the word's decontextualized
#' embedding with ridge regression (leave-one-out prediction = the plotted
#' position), and computes p-values by permuting \code{x} across participants.
#' Each word is compared with its own null distribution.
#'
#' Words are counted as tokens (NLTK tokenization, lower-cased), and word
#' types from the embeddings are normalised for lookup (RoBERTa/SentencePiece
#' markers and case are removed), so that words and embeddings match.
#' Participants with a missing \code{x} (or \code{y}) are dropped with a
#' message. \code{x} is standardised internally, so the plotted positions are
#' centred and a plot split at 0 separates low from high scores.
#'
#' @details
#' This function is EXPERIMENTAL and its interface or defaults may still
#' change.
#'
#' Relation to \code{\link{textTrainRegression}}: earlier development
#' versions fitted the word-level ridge model with
#' \code{textTrainRegression()} (nested cross-validation, penalty re-tuned
#' per fold). The current implementation fits the same kind of model -- a
#' ridge regression predicting each word's mean \code{x} from its embedding
#' -- but with the penalty chosen by exact leave-one-out error and the
#' leave-one-out predictions computed in closed form. This matters for the
#' p-values: with the penalty fixed, the whole pipeline is a fixed linear
#' map of \code{x}, so each of the \code{n_permutations} participant
#' permutations costs one matrix product instead of a full nested-CV refit.
#' \code{textTrainRegression()} predictions are not a linear function of
#' \code{x} (random fold splits, per-fold tuning), so using it here would
#' make a full permutation test computationally prohibitive. Note also that
#' the regression in this function runs across words (one observation per
#' word type), whereas \code{textTrainRegression()} is designed for
#' participant-level assessment models (one observation per person); the
#' plotted positions from the two approaches agree closely in rank
#' (Spearman correlation about .95 in the validation runs, see
#' dev/permutation_handoff/).
#'
#' @param words               Character vector (or single-column data frame)
#'                            of text responses - one per participant.
#' @param word_types_embeddings Word-type embeddings from
#'                            \code{\link{textEmbed}} (with
#'                            \code{aggregation_from_tokens_to_word_types}
#'                            set, e.g., to "mean").
#' @param x                   Numeric vector - the variable to relate the
#'                            words to.
#' @param y                   Optional second numeric variable; tested with
#'                            its own, separate permutations.
#' @param centre              "none" (default) keeps the raw word means.
#'                            "token_mean" subtracts the token-weighted mean
#'                            of \code{x} from every word score, which
#'                            separates word-specific signal from a verbosity
#'                            effect (when \code{x} is related to how much
#'                            people write, all frequent words are otherwise
#'                            pushed in the direction of that association).
#' @param min_freq_words_test (numeric) Only words occurring at least this
#'                            many times are tested (default 3).
#' @param n_models            Deprecated; ignored with a warning. P-values
#'                            are now based on full participant permutations.
#' @param n_permutations      Number of permutations of \code{x} (and
#'                            \code{y}) across participants. Default 10000.
#' @param seed                Integer seed for reproducibility. Default 1003.
#' @param case_insensitive    Deprecated; matching is always lower-cased
#'                            (ignored with a warning when set to FALSE).
#' @param text_remove         Regex pattern; words matching it are removed
#'                            before testing (default \code{"[()]"}).
#' @param ...                 Ignored (kept for backward compatibility).
#'
#' @return A named list:
#' \describe{
#'   \item{word_data}{A tibble with one row per tested word:
#'     \code{words}, \code{n} (token frequency),
#'     \code{word_mean_value_x} (mean of standardised x for the word),
#'     \code{x_plotted} (leave-one-out ridge prediction - the position),
#'     \code{p_values_x} (permutation p-value against the word's own null),
#'     \code{p_z_x} (normal approximation from the word's own null; no
#'     1/(n_permutations + 1) floor); plus the y-equivalents when \code{y}
#'     is provided.}
#'   \item{lambda_x}{The ridge penalty chosen by leave-one-out error
#'     (\code{lambda_y} when \code{y} is provided).}
#'   \item{loo_r}{Correlation between the word means and their leave-one-out
#'     predictions (\code{loo_r_y} when \code{y} is provided).}
#' }
#' The comment attribute on the output stores a human-readable description of
#' the call parameters for reproducibility.
#'
#' @examples
#' \dontrun{
#' library(text)
#'
#' # --- Step 1: embed the text column (word-type embeddings are needed)
#' embeddings <- textEmbed(
#'   Language_based_assessment_data_8["harmonywords"],
#'   aggregation_from_tokens_to_word_types = "mean"
#' )
#'
#' # --- Step 2: run textWordPrediction
#' result <- textWordPrediction(
#'   words                 = Language_based_assessment_data_8$harmonywords,
#'   word_types_embeddings = embeddings$word_types,
#'   x                     = Language_based_assessment_data_8$hilstotal,
#'   n_permutations        = 10000
#' )
#'
#' # --- Step 3: inspect word-level scores
#' result$word_data
#' }
#'
#' @seealso \code{\link{textProjection}}, \code{\link{textProjectionPlot}}
#'
#' @importFrom tibble as_tibble
#' @importFrom dplyr bind_cols rename
#' @export
textWordPrediction <- function(words,
                               word_types_embeddings = word_types_embeddings_df,
                               x,
                               y                = NULL,
                               centre           = "none",
                               min_freq_words_test = 3,
                               n_models         = NULL,
                               n_permutations   = 10000,
                               seed             = 1003,
                               case_insensitive = TRUE,
                               text_remove      = "[()]",
                               ...) {
  # ---------------------------------------------------------------------------
  # 0. Guard the interface: deprecated arguments and valid options
  # ---------------------------------------------------------------------------
  if (!is.null(n_models)) {
    warning(
      "n_models is deprecated and ignored: p-values are now based on full ",
      "participant permutations of x (see n_permutations)."
    )
  }
  if (!isTRUE(case_insensitive)) {
    warning("case_insensitive is deprecated and ignored: matching is lower-cased.")
  }
  if (!centre %in% c("none", "token_mean")) {
    stop("centre must be \"none\" or \"token_mean\".")
  }

  # ---------------------------------------------------------------------------
  # 1. Coerce inputs: accept single-column data frames (e.g., data["col"]) and
  #    a word_types list of length one (as textEmbed() returns it)
  # ---------------------------------------------------------------------------
  if (is.data.frame(words)) words <- words[[1]]
  if (is.data.frame(x)) x <- x[[1]]
  if (!is.null(y) && is.data.frame(y)) y <- y[[1]]
  if (length(word_types_embeddings) == 1 && !is.data.frame(word_types_embeddings)) {
    word_types_embeddings <- word_types_embeddings[[1]]
  }

  # ---------------------------------------------------------------------------
  # 2. Record the call settings; attached as a comment() on the output so the
  #    analysis is reproducible from the saved object alone
  # ---------------------------------------------------------------------------
  call_description <- paste(
    "type = textWordPrediction",
    "words =", substitute(words),
    "word_types_embeddings =", comment(word_types_embeddings),
    "x =", substitute(x),
    "y =", substitute(y),
    "centre =", centre,
    "min_freq_words_test =", min_freq_words_test,
    "n_permutations =", n_permutations,
    "seed =", seed,
    sep = " ", collapse = " "
  )

  # ---------------------------------------------------------------------------
  # 3. Drop participants with a missing x (or y): a participant without an
  #    outcome cannot contribute to any word mean. (Dropping the PARTICIPANT,
  #    not every word they used, is what keeps words from disappearing.)
  # ---------------------------------------------------------------------------
  ok <- !is.na(x) & !is.na(words)
  if (!is.null(y)) ok <- ok & !is.na(y)
  if (any(!ok)) {
    message(colourise(paste0(
      sum(!ok), " participant(s) with missing values were dropped.\n"
    ), "blue"))
  }
  words <- words[ok]
  x <- x[ok]
  if (!is.null(y)) y <- y[ok]

  # ---------------------------------------------------------------------------
  # 4. Build the word-matching ingredients:
  #    - normalise_word_types(): strips RoBERTa/SentencePiece markers and case
  #      from the embedding vocabulary so it can match the NLTK words
  #    - perm_count_matrix(): sparse participants x word-types TOKEN counts
  #      (the basis for the word means; replaces regex substring counting)
  #    - coverage message: the share of the language that has an embedding,
  #      so silent mismatches (e.g., a cased model) are visible to the user
  # ---------------------------------------------------------------------------
  word_types_embeddings <- normalise_word_types(word_types_embeddings)
  C <- perm_count_matrix(words)
  n_tokens <- Matrix::colSums(C)
  matched <- colnames(C) %in% word_types_embeddings$words
  message(colourise(paste0(
    "Coverage: ", round(100 * mean(matched), 1), "% of word types and ",
    round(100 * sum(n_tokens[matched]) / sum(n_tokens), 1),
    "% of tokens have an embedding.\n"
  ), "blue"))

  # ---------------------------------------------------------------------------
  # 5. The x-dimension analysis. wp_run() (see 4_3_1_projectionPermutation.R)
  #    does all the statistics: word means, ridge penalty by exact
  #    leave-one-out, plotted positions, and the participant-permutation
  #    p-values (plus p_z). Columns are then renamed to the x-suffixed names
  #    that textProjectionPlot() expects.
  # ---------------------------------------------------------------------------
  set.seed(seed)
  res_x <- wp_run(C, word_types_embeddings, x,
    min_freq_words_test = min_freq_words_test,
    centre = centre, Npermutations = n_permutations,
    word_pattern = text_remove
  )
  word_data <- dplyr::rename(res_x$word_data,
    word_mean_value_x = word_mean_value,
    x_plotted = plotted,
    p_values_x = p_values,
    p_z_x = p_z
  )

  out <- list(word_data = word_data, lambda_x = res_x$lambda, loo_r = res_x$loo_r)

  # ---------------------------------------------------------------------------
  # 6. Optional y-dimension: the identical analysis for y, with its own
  #    permutations (seed + 1 ensures the y shuffles differ from the x
  #    shuffles). Results are joined word-by-word; the shared n column is
  #    kept once from the x side.
  # ---------------------------------------------------------------------------
  if (!is.null(y)) {
    set.seed(seed + 1)
    res_y <- wp_run(C, word_types_embeddings, y,
      min_freq_words_test = min_freq_words_test,
      centre = centre, Npermutations = n_permutations,
      word_pattern = text_remove
    )
    wd_y <- dplyr::rename(res_y$word_data,
      word_mean_value_y = word_mean_value,
      y_plotted = plotted,
      p_values_y = p_values,
      p_z_y = p_z
    )
    wd_y$n <- NULL
    out$word_data <- dplyr::full_join(out$word_data, wd_y, by = "words")
    out$lambda_y <- res_y$lambda
    out$loo_r_y <- res_y$loo_r
  }

  # ---------------------------------------------------------------------------
  # 7. Attach the call description (step 2) for reproducibility and return
  # ---------------------------------------------------------------------------
  comment(out) <- call_description
  out
}
