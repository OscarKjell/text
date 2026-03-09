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
#                           value of `x` (and optionally `y`) for all
#                           responses in which that word appeared. A ridge
#                           regression model is then trained so the approach
#                           generalises to words NOT seen in training.
#
# Output is designed to be passed directly to textProjectionPlot().
# =============================================================================


# -----------------------------------------------------------------------------
# Helper: wordsMeanValue
# -----------------------------------------------------------------------------
#' Compute the mean value of a numeric variable for a single target word.
#'
#' For every row in `words` the function counts how many times `target_word`
#' appears (case-optionally). It then computes a frequency-weighted mean of
#' `x_value` — i.e., the average numeric score for rows that contain the word.
#'
#' @param words        Character vector of text responses (one per participant).
#' @param target_word  The specific word whose mean x-value we want.
#' @param x_value      Numeric vector (same length as `words`) — the outcome
#'                     variable (e.g., a well-being scale score).
#' @param case_insensitive Logical. If TRUE, matching ignores capitalisation.
#' @return A single numeric value: the frequency-weighted mean of `x_value`
#'         for rows containing `target_word`. Returns NaN if the word never
#'         appears.
#' @importFrom stringi stri_count_regex
#' @noRd
wordsMeanValue <- function(words, target_word, x_value, case_insensitive) {

  # Count how many times target_word appears in each response (can be >1)
  word_freq_row <- stringi::stri_count_regex(
    str              = words,
    pattern          = target_word,
    case_insensitive = case_insensitive
  )

  # Frequency-weighted mean: sum(count_i * score_i) / sum(count_i)
  # If the word never appears, sum(word_freq_row) == 0 → returns NaN (handled
  # downstream with complete.cases()).
  mean_value <- sum(word_freq_row * x_value) / sum(word_freq_row)
  return(mean_value)
}


# -----------------------------------------------------------------------------
# Helper: permutationPValue
# -----------------------------------------------------------------------------
#' Compute permutation-based p-values for word-level prediction scores.
#'
#' ## Strategy: "null model fits + bootstrap"
#'
#' **Level 1 — `n_models` NULL model fits (expensive):**
#' Each model is trained on *shuffled* x labels (H₀: x is unrelated to words).
#' Each null model produces cross-validated out-of-sample prediction scores —
#' one per word. These are genuine null scores because the model learned from
#' a world where x had no relationship to word use.
#'
#' Each model uses a different label permutation AND a different CV seed, so
#' the `n_models` null fits sample independently from the null distribution.
#'
#' **Level 2 — bootstrap to smooth the null distribution (cheap):**
#' With `n_models` null fits we have `n_models` null scores per word. We
#' bootstrap-resample from these to obtain `n_permutations` null samples per
#' word. This smooths the null distribution without any additional model fits.
#' The minimum achievable p-value is 1 / n_permutations (after bootstrapping),
#' but p-value *resolution* is ultimately limited by n_models:
#' unique p-value steps ≈ 1 / n_models (e.g., 0.04 steps with n_models = 25).
#'
#' **Recommended defaults:** n_models = 25, n_permutations = 10 000.
#' With 25 null fits the smallest non-trivial p-value is 1/25 = 0.04, which
#' falls just below the conventional α = 0.05 threshold.
#'
#' @param words_sorted        Tibble with columns `words` and `n`.
#' @param words_raw           Raw character vector of all text responses.
#' @param x_value             Numeric outcome vector (one value per participant).
#' @param word_embeddings_mat Tibble/matrix of decontextualised embeddings (one
#'                            row per unique word, aligned with words_sorted).
#' @param observed_scores     Numeric vector of observed out-of-sample prediction
#'                            scores from the real model (one per unique word).
#' @param n_models            Number of null ridge models to fit (each on a
#'                            different permuted x vector). Determines p-value
#'                            resolution: min step ≈ 1/n_models. Default 25.
#' @param n_permutations      Bootstrap samples drawn from the n_models null
#'                            scores to smooth the null distribution. Default
#'                            10000. Set to 0 to skip p-values entirely.
#' @param case_insensitive    Passed through to \code{wordsMeanValue()}.
#' @param seed                Base random seed. Null model i uses seed + i for
#'                            both the label shuffle and the CV split.
#' @return A numeric vector of two-tailed p-values (one per unique word;
#'         \code{NA} for words without a valid embedding).
#' @noRd
permutationPValue <- function(words_sorted,
                              words_raw,
                              x_value,
                              word_embeddings_mat,
                              observed_scores,
                              n_models         = 25,
                              n_permutations   = 10000,
                              case_insensitive = TRUE,
                              seed             = 1003) {

  n_words <- nrow(words_sorted)

  # ---------------------------------------------------------------------------
  # 1. Identify trainable words: must have BOTH a valid embedding AND a valid
  #    observed score. Words without a vocabulary embedding get NA throughout.
  # ---------------------------------------------------------------------------
  has_valid_embedding <- apply(word_embeddings_mat, 1, function(row) !any(is.na(row)))
  has_valid_observed  <- !is.na(observed_scores)
  trainable_idx       <- has_valid_embedding & has_valid_observed

  if (sum(trainable_idx) < 3) {
    warning("Fewer than 3 words have valid embeddings; returning NA p-values.")
    return(rep(NA_real_, n_words))
  }

  n_trainable        <- sum(trainable_idx)
  trainable_pos      <- which(trainable_idx)
  emb_trainable      <- as.matrix(word_embeddings_mat[trainable_idx, ])
  observed_trainable <- observed_scores[trainable_idx]

  # ---------------------------------------------------------------------------
  # 2. Fit n_models NULL ridge regressions (Level 1 — expensive).
  #
  #    Each model is trained on SHUFFLED x labels so it learns under H₀.
  #    Cross-validated out-of-sample predictions from each null model give one
  #    genuine null score per word — no approximation needed.
  #
  #    Null model i uses set.seed(seed + i) for BOTH the label shuffle and the
  #    CV split, ensuring full reproducibility.
  # ---------------------------------------------------------------------------
  message(sprintf("  Fitting %d null model(s) (permuted x labels)...", n_models))

  # null_scores_raw: rows = trainable words, cols = null models
  # Each column is the vector of CV predictions from one null fit.
  null_scores_raw <- matrix(NA_real_, nrow = n_trainable, ncol = n_models)

  n_fitted <- 0L

  for (model_i in seq_len(n_models)) {

    set.seed(seed + model_i)   # governs BOTH the shuffle and the CV split

    # Shuffle x labels → break any real word–outcome association
    x_shuffled <- sample(x_value)

    # Recompute per-word means under the null labels
    null_means_all <- unlist(lapply(
      words_sorted$words,
      wordsMeanValue,
      words            = words_raw,
      x_value          = x_shuffled,
      case_insensitive = case_insensitive
    ))
    null_means_trainable <- null_means_all[trainable_idx]

    # Skip if any mean is undefined (word appears in every response → no
    # variance in which participants "have" it → mean is always global mean)
    if (any(is.na(null_means_trainable))) {
      warning(sprintf("Null model %d: NA means after shuffle, skipping.", model_i))
      next
    }

    # Fit ridge regression on the shuffled means
    null_model <- tryCatch(
      textTrainRegression(
        x = tibble::as_tibble(emb_trainable),
        y = null_means_trainable
      ),
      error = function(e) {
        warning(sprintf("Null model %d failed to fit: %s", model_i, conditionMessage(e)))
        NULL
      }
    )

    if (is.null(null_model)) next

    # Extract cross-validated out-of-sample predictions (one per trainable word).
    # textTrainRegression returns a list where $predictions is a tibble with a
    # $predictions column — so the actual numeric vector is two levels deep.
    cv_preds <- tryCatch(
      as.numeric(null_model$predictions$predictions),
      error = function(e) NULL
    )

    if (is.null(cv_preds) || length(cv_preds) != n_trainable) {
      warning(sprintf(
        "Null model %d: predictions have unexpected length (%s), skipping.",
        model_i, if (is.null(cv_preds)) "NULL" else length(cv_preds)
      ))
      next
    }

    n_fitted <- n_fitted + 1L
    null_scores_raw[, model_i] <- cv_preds
  }

  # Drop columns from failed fits
  valid_cols      <- which(apply(null_scores_raw, 2, function(col) !all(is.na(col))))
  null_scores_raw <- null_scores_raw[, valid_cols, drop = FALSE]
  n_fitted        <- ncol(null_scores_raw)

  message(sprintf("  %d/%d null models fitted successfully.", n_fitted, n_models))

  if (n_fitted == 0L) {
    warning("All null model fits failed; returning NA p-values.")
    return(rep(NA_real_, n_words))
  }

  # ---------------------------------------------------------------------------
  # 3. Bootstrap the null distribution (Level 2 — cheap).
  #
  #    For each word we now have n_fitted null scores (one per null model).
  #    We resample these with replacement n_permutations times to smooth the
  #    null distribution. This does not add information beyond what the n_fitted
  #    null models provide, but it gives stable p-value estimates and allows
  #    fine-grained thresholds (e.g., p < .01) without fitting more models.
  # ---------------------------------------------------------------------------
  message(sprintf(
    "  Bootstrapping null distribution: %d samples from %d null scores per word...",
    n_permutations, n_fitted
  ))

  set.seed(seed)   # reproducible bootstrap

  # For each word, resample its n_fitted null scores n_permutations times.
  # Result: rows = trainable words, cols = bootstrap samples
  boot_idx        <- matrix(
    sample.int(n_fitted, size = n_trainable * n_permutations, replace = TRUE),
    nrow = n_trainable,
    ncol = n_permutations
  )
  # boot_scores[j, k] = null_scores_raw[j, boot_idx[j, k]]
  null_scores_boot <- matrix(
    null_scores_raw[cbind(
      rep(seq_len(n_trainable), times = n_permutations),
      as.vector(boot_idx)
    )],
    nrow = n_trainable,
    ncol = n_permutations
  )

  # ---------------------------------------------------------------------------
  # 4. Compute two-tailed p-values.
  #    p_i = proportion of bootstrapped |null scores| >= |observed score|.
  #    Words without a valid embedding keep p = NA.
  # ---------------------------------------------------------------------------
  p_values <- rep(NA_real_, n_words)

  for (j in seq_len(n_trainable)) {
    i         <- trainable_pos[j]
    null_dist <- null_scores_boot[j, ]
    null_dist <- null_dist[!is.na(null_dist)]

    if (length(null_dist) == 0L) next

    p_values[i] <- mean(abs(null_dist) >= abs(observed_trainable[j]))
  }

  return(p_values)
}


# -----------------------------------------------------------------------------
# Main function: textWordPrediction (experimental)
# -----------------------------------------------------------------------------
#' Compute word-level prediction scores for plotting with textProjectionPlot().
#'
#' @description
#' For each unique word in `words` the function:
#' \enumerate{
#'   \item Computes the **mean value of `x`** (and optionally `y`) across all
#'         participants whose response contained that word.
#'   \item Looks up the **decontextualised embedding** for that word from
#'         `word_types_embeddings`.
#'   \item Trains a **ridge regression** model: embedding → mean x score. The
#'         out-of-sample predictions become the `x_plotted` plotting coordinate,
#'         allowing generalisation to words unseen in training.
#'   \item Optionally computes **permutation-based p-values** (see
#'         `n_permutations`) by shuffling `x` labels and building a null
#'         distribution of prediction scores.
#' }
#'
#' The returned `word_data` tibble has column names that match the expectations
#' of \code{\link{textProjectionPlot}}: `x_plotted` (and `y_plotted`) for coordinates
#' and `p_values_x` (and `p_values_y`) for significance.
#'
#' @param words               Character vector **or** single-column tibble of
#'                            free-text responses (one per participant).
#' @param word_types_embeddings
#'                            Word-type embeddings from \code{\link{textEmbed}}
#'                            — specifically the `$word_types` component. These
#'                            are *decontextualised*: one fixed vector per
#'                            unique word type.
#' @param x                   Numeric vector (or single-column tibble) of the
#'                            outcome variable to project words onto the x-axis
#'                            (e.g., a well-being scale score).
#' @param y                   Optional numeric vector for a second outcome to
#'                            project onto the y-axis. Default \code{NULL}.
#' @param n_models            Number of null ridge regression models to fit,
#'                            each trained on a *different* permuted x vector.
#'                            Each null fit produces genuine cross-validated
#'                            out-of-sample null scores — one per word.
#'                            Determines p-value resolution: the minimum
#'                            non-trivial p-value step is approximately
#'                            \code{1/n_models} (e.g., 0.04 with 25 models,
#'                            which is just below alpha = 0.05). Default \code{25}.
#' @param n_permutations      Number of bootstrap samples drawn from the
#'                            \code{n_models} null scores to smooth the null
#'                            distribution. Does not require additional model
#'                            fits. Set to \code{0} to skip p-values entirely.
#'                            Default \code{10000}.
#' @param seed                Integer seed for reproducibility. Default 1003.
#' @param case_insensitive    Logical. If \code{TRUE} (default), word matching
#'                            ignores capitalisation.
#' @param text_remove         Regex pattern for characters to strip before
#'                            processing (e.g., brackets). Default
#'                            \code{"[()]"}.
#' @param ...                 Additional arguments forwarded to
#'                            \code{\link{textTrainRegression}}.
#'
#' @return A named list:
#' \describe{
#'   \item{model_x}{The fitted \code{textTrainRegression} model for the x-axis.}
#'   \item{model_y}{(Only if `y` is supplied) Fitted model for the y-axis.}
#'   \item{word_data}{A tibble with one row per unique word containing:
#'     \code{words}, \code{n} (frequency), \code{word_mean_value_x},
#'     \code{x_plotted} (embedding-based prediction), \code{p_values_x}; plus
#'     the y-equivalents when `y` is provided.}
#' }
#' The comment attribute on the output stores a human-readable description of
#' all call parameters for reproducibility.
#'
#' @examples
#' \dontrun{
#' library(text)
#'
#' # --- Step 1: embed the text column (produces text-level + word-type embeddings)
#' embeddings <- textEmbed(Language_based_assessment_data_8["harmonywords"])
#'
#' # --- Step 2: run textWordPrediction
#' result <- textWordPrediction(
#'   words                 = Language_based_assessment_data_8$harmonywords,
#'   word_types_embeddings = embeddings$word_types,
#'   x                     = Language_based_assessment_data_8$hilstotal,
#'   n_models              = 5,      # 5 real fits with different CV seeds
#'   n_permutations        = 10000,  # 5 × 10 000 = 50 000 total null samples
#'   seed                  = 1003
#' )
#'
#' # --- Step 3: inspect word-level scores
#' result$word_data
#'
#' # --- Step 4: pass directly to textProjectionPlot
#' textProjectionPlot(result)
#' }
#'
#' @seealso \code{\link{textProjection}}, \code{\link{textProjectionPlot}},
#'          \code{\link{textTrainRegression}}
#'
#' @importFrom tibble as_tibble_col as_tibble
#' @importFrom dplyr bind_cols
#' @export
textWordPrediction <- function(words,
                               word_types_embeddings = word_types_embeddings_df,
                               x,
                               y                = NULL,
                               n_models         = 25,
                               n_permutations   = 10000,
                               seed             = 1003,
                               case_insensitive = TRUE,
                               text_remove      = "[()]",
                               ...) {

  set.seed(seed)

  # ---------------------------------------------------------------------------
  # 0. Build a human-readable description for provenance tracking
  # ---------------------------------------------------------------------------
  call_description <- paste(
    "type = textWordPrediction",
    "words =",               substitute(words),
    "word_types_embeddings =", comment(word_types_embeddings),
    "x =",                   substitute(x),
    "y =",                   substitute(y),
    "n_models =",            n_models,
    "n_permutations =",      n_permutations,
    "case_insensitive =",    case_insensitive,
    sep = " ", collapse = " "
  )

  # ---------------------------------------------------------------------------
  # 1. Coerce inputs to plain vectors (accept tibble columns or bare vectors)
  # ---------------------------------------------------------------------------
  # Allow `words` to be passed as a single-column tibble (e.g. data[,"col"])
  if (is.data.frame(words))  words <- words[[1]]
  # Same for x
  if (is.data.frame(x))      x     <- x[[1]]
  # Same for y
  if (!is.null(y) && is.data.frame(y)) y <- y[[1]]

  # ---------------------------------------------------------------------------
  # 2. Find all unique words and their frequencies across responses
  # ---------------------------------------------------------------------------
  # unique_freq_words() is an internal text-package helper that returns a
  # tibble with columns:  words (character) | n (integer frequency)
  words_sorted <- unique_freq_words(words)

  # Remove tokens that contain bracket characters (artefacts of transcription)
  words_sorted <- words_sorted[
    !grepl(pattern = text_remove, x = words_sorted$words),
  ]

  # ---------------------------------------------------------------------------
  # 3a. Compute per-word mean of x
  # ---------------------------------------------------------------------------
  # For every unique word, calculate the average x-score of participants whose
  # response contained that word (frequency-weighted via wordsMeanValue).
  mean_x_per_word <- unlist(lapply(
    words_sorted$words,
    wordsMeanValue,
    words            = words,
    x_value          = x,
    case_insensitive = case_insensitive
  ))

  mean_x_col  <- tibble::as_tibble_col(mean_x_per_word, column_name = "word_mean_value_x")
  words_mean_x <- dplyr::bind_cols(words_sorted, mean_x_col)

  # Drop words for which no valid mean could be computed (e.g., never appeared
  # in responses that also had a valid x score)
  words_mean_x <- words_mean_x[complete.cases(words_mean_x$word_mean_value_x), ]

  # ---------------------------------------------------------------------------
  # 3b. Compute per-word mean of y (if supplied)
  # ---------------------------------------------------------------------------
  if (!is.null(y)) {
    mean_y_per_word <- unlist(lapply(
      words_sorted$words,
      wordsMeanValue,
      words            = words,
      x_value          = y,   # re-use helper; "x_value" is the generic slot
      case_insensitive = case_insensitive
    ))

    mean_y_col   <- tibble::as_tibble_col(mean_y_per_word, column_name = "word_mean_value_y")
    words_mean_y <- dplyr::bind_cols(words_sorted, mean_y_col)
    words_mean_y <- words_mean_y[complete.cases(words_mean_y$word_mean_value_y), ]
  }

  # ---------------------------------------------------------------------------
  # 4. Retrieve decontextualised embeddings for each unique word
  # ---------------------------------------------------------------------------
  # applysemrep() is an internal text helper: given a word string it looks it
  # up in the word_types_embeddings object and returns its embedding vector.
  # sapply over all unique words → matrix (embedding_dim × n_words); we
  # transpose so rows = words, columns = embedding dimensions.
  word_emb_matrix <- tibble::as_tibble(
    t(sapply(words_mean_x$words, applysemrep, word_types_embeddings))
  )

  # ---------------------------------------------------------------------------
  # 5a. Train ridge regression: embedding → mean x value  (x-axis model)
  # ---------------------------------------------------------------------------
  # The out-of-sample predictions give each word a score on the x-axis that
  # is grounded in its *semantic embedding*, not just its observed mean.
  # This allows the model to score words not seen in training (generalisation).
  model_x <- textTrainRegression(
    x = word_emb_matrix,
    y = words_mean_x$word_mean_value_x,
    ...
  )

  # Rename prediction column to the name expected by textProjectionPlot
  pred_x_col <- tibble::as_tibble_col(
    model_x$predictions$predictions,
    column_name = "x_plotted"
  )

  # ---------------------------------------------------------------------------
  # 5b. Train ridge regression for y-axis (if y is supplied)
  # ---------------------------------------------------------------------------
  if (!is.null(y)) {
    model_y <- textTrainRegression(
      x = word_emb_matrix,
      y = words_mean_y$word_mean_value_y,
      ...
    )

    pred_y_col <- tibble::as_tibble_col(
      model_y$predictions$predictions,
      column_name = "y_plotted"
    )
  }

  # ---------------------------------------------------------------------------
  # 6. Compute permutation-based p-values
  # ---------------------------------------------------------------------------
  # We build a null distribution by repeatedly shuffling x (breaking the
  # word–score link) and re-running the whole pipeline. The p-value for each
  # word is the fraction of null predictions at least as extreme as observed.
  #
  # Set n_permutations = 0 to skip (returns NA — useful during development).
  if (n_permutations > 0) {

    message(sprintf(
      "Running %d models × %d permutations = %d null samples for x-axis p-values...",
      n_models, n_permutations, n_models * n_permutations
    ))

    p_values_x <- permutationPValue(
      words_sorted        = words_mean_x[, c("words", "n")],
      words_raw           = words,
      x_value             = x,
      word_embeddings_mat = word_emb_matrix,
      observed_scores     = model_x$predictions$predictions,
      n_models            = n_models,
      n_permutations      = n_permutations,
      case_insensitive    = case_insensitive,
      seed                = seed
    )

    if (!is.null(y)) {
      message(sprintf(
        "Running %d models × %d permutations for y-axis p-values...",
        n_models, n_permutations
      ))

      p_values_y <- permutationPValue(
        words_sorted        = words_mean_y[, c("words", "n")],
        words_raw           = words,
        x_value             = y,
        word_embeddings_mat = word_emb_matrix,
        observed_scores     = model_y$predictions$predictions,
        n_models            = n_models,
        n_permutations      = n_permutations,
        case_insensitive    = case_insensitive,
        seed                = seed
      )
    }

  } else {
    # Permutations skipped — fill with NA so downstream code still works
    p_values_x <- rep(NA_real_, nrow(words_mean_x))
    if (!is.null(y)) p_values_y <- rep(NA_real_, nrow(words_mean_y))
  }

  p_val_x_col <- tibble::as_tibble_col(p_values_x, column_name = "p_values_x")
  if (!is.null(y)) {
    p_val_y_col <- tibble::as_tibble_col(p_values_y, column_name = "p_values_y")
  }

  # ---------------------------------------------------------------------------
  # 7. Assemble final word_data tibble
  # ---------------------------------------------------------------------------
  # Column order is chosen to match what textProjectionPlot() expects:
  #   words | n | word_mean_value_x | x_plotted | p_values_x
  #   (+ y equivalents when y is provided)
  if (is.null(y)) {

    word_data <- dplyr::bind_cols(
      words_mean_x,   # words, n, word_mean_value_x
      pred_x_col,     # x_plotted
      p_val_x_col     # p_values_x
    )

    output <- list(
      model_x   = model_x,
      word_data = word_data
    )

  } else {

    # Merge x and y mean-value columns (both share words/n from words_sorted)
    word_data <- dplyr::bind_cols(
      words_mean_x,                                     # words, n, word_mean_value_x
      mean_y_col[complete.cases(mean_y_col), , drop = FALSE], # word_mean_value_y
      pred_x_col,                                       # x_plotted
      p_val_x_col,                                      # p_values_x
      pred_y_col,                                       # y_plotted
      p_val_y_col                                       # p_values_y
    )

    output <- list(
      model_x   = model_x,
      model_y   = model_y,
      word_data = word_data
    )
  }

  # Attach provenance description as a comment attribute
  comment(output) <- call_description

  return(output)
}


# =============================================================================
# EXAMPLE DATA & USAGE DEMO
# =============================================================================
# Run this section interactively to verify the function end-to-end.
# It uses the built-in Language_based_assessment_data_8 dataset that ships
# with the text package (100 participants, harmony/satisfaction text + scales).
# =============================================================================

if (FALSE) {  # wrapped in FALSE so it doesn't execute on source(); run manually

  library(text)
  library(dplyr)

  # ---------------------------------------------------------------------------
  # A. Inspect the built-in example data
  # ---------------------------------------------------------------------------
  # Language_based_assessment_data_8 ships with the text package.
  # It contains 40 participants with open-ended text responses and numeric
  # scale scores:
  #   - harmonywords     : "describe your harmony in life"
  #   - satisfactionwords: "describe your satisfaction with life"
  #   - hilstotal        : Harmony In Life Scale total score (numeric)
  #   - swlstotal        : Satisfaction With Life Scale total score (numeric)

  head(Language_based_assessment_data_8[, c("harmonywords", "satisfactionwords",
                                            "hilstotal", "swlstotal")])

  # ---------------------------------------------------------------------------
  # B. Embed the text variable
  # ---------------------------------------------------------------------------
  # textEmbed() returns a named list where each text column gets its own entry.
  # Each entry contains:
  #   $texts      — one aggregated vector per response (contextualized)
  #   $word_types — one vector per unique word type (decontextualized)
  #     └── $texts — the actual tibble of embeddings (words × dims)
  #
  # We need $word_types$texts for textWordPrediction.

  embeddings <- textEmbed(
    texts           = Language_based_assessment_data_8["harmonywords"],
    model           = "bert-base-uncased",   # swap for any HuggingFace model
    layers          = -2,                    # second-to-last layer (common default)
    aggregation_from_layers_to_tokens         = "mean",
    aggregation_from_tokens_to_texts          = "mean",
    aggregation_from_tokens_to_word_types     = "mean"
  )

  # Check what we got — should be a tibble: words × 768
  str(embeddings$word_types$texts, max.level = 1)

  # ---------------------------------------------------------------------------
  # C. Run textWordPrediction — x-axis only (1D projection)
  # ---------------------------------------------------------------------------
  result_1d <- textWordPrediction(
    words                 = Language_based_assessment_data_8$harmonywords,
    word_types_embeddings = embeddings$word_types$texts,
    y                     = NULL,                                         # 1D only
    n_models              = 2,        # set low for quick testing; increase for real use
    n_permutations        = 10000,   # bootstrap samples from the 25 null scores; use 0 to skip
    seed                  = 1003,
    case_insensitive      = TRUE
  )

  # Inspect the word-level output table
  result_1d$word_data

  # Top words with highest (most positive) x_plotted score
  result_1d$word_data |>
    arrange(desc(x_plotted)) |>
    head(10)

  # Words with significant p-values (< .05)
  result_1d$word_data |>
    filter(p_values_x < .05) |>
    arrange(p_values_x)

  # ---------------------------------------------------------------------------
  # D. Run textWordPrediction — with y-axis (2D projection)
  # ---------------------------------------------------------------------------
  result_2d <- textWordPrediction(
    words                 = Language_based_assessment_data_8$harmonywords,
    word_types_embeddings = embeddings$word_types$texts,
    x                     = Language_based_assessment_data_8$hilstotal,  # Harmony → x
    y                     = Language_based_assessment_data_8$swlstotal,  # Satisfaction → y
    n_models              = 2,
    n_permutations        = 10000,
    seed                  = 1003,
    case_insensitive      = TRUE
  )

  result_2d$word_data

  # ---------------------------------------------------------------------------
  # E. Pass directly to textProjectionPlot()
  # ---------------------------------------------------------------------------
  # textProjectionPlot expects the list with a $word_data component whose
  # columns include: words, n, x_plotted, p_values_x (+ y_plotted, p_values_y)

  plot_out <- textProjectionPlot(
    word_data       = result_2d,
    y_axes          = TRUE,
    title_top       = "textWordPrediction: Harmony Words",
    x_axes_label    = "Harmony In Life (HILS)",
    y_axes_label    = "Satisfaction With Life (SWLS)",
    p_alpha         = 0.05
  )

  plot_out$final_plot

}
