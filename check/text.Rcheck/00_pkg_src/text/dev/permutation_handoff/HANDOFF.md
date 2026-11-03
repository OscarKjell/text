# Hand-off: permutation tests in `textProjection()` and `textWordPrediction()`

**For:** a Claude Code session running locally inside a clone of the `text` package (github.com/OscarKjell/text), working with Oscar.
**Goal:** implement the changes below in `text` on a new branch, and let Oscar look at them on the open example data `topics::dep_wor_data` before anything is pushed.

Everything here uses public data only. No embeddings or results are included. The dev scripts create them locally.

---

## 1. Start here (instructions for the local Claude session)

1. **Read this file to the end before changing code.**
2. **Create a branch** in the `text` repo, e.g. `git checkout -b participant-permutation`. Do not push, and do not open a PR, until Oscar says so.
3. **Copy this folder into the repo as `dev/permutation_handoff/`.**
   - Add `^dev$` to `.Rbuildignore` if it isn't already there.
   - The files in `R/` here are **prototypes**, not package code. Port them into the package files listed in section 4. Don't source them from the package.
4. **Show Oscar the current behaviour first.** Run from inside `dev/permutation_handoff/`, with the released or current `text` loaded:
   ```r
   # set TEXT_CONDAENV if text needs a named conda env; TEXT_DEVICE = "cpu"/"gpu"/"mps"
   source("dev/00_setup_public_data.R")   # embeds dep_wor_data with bert-base-uncased (slow on CPU), caches in dev/cache/
   source("dev/01_projection_null.R")     # token vs participant: counts, plots, calibration, seed dependence
   source("dev/02_word_prediction.R")     # corrected word prediction: counts, plots, calibration, substring check
   source("dev/03_roberta_matching.R")    # why RoBERTa word types never match
   ```
   - Outputs (CSVs and PNGs) go to `dev/output/`. Show Oscar the plots and the `*_summary.csv` files.
   - Set `K_NOISE=10` in the environment for a quicker first pass; the default is 50 noise variables.
5. **Implement the changes in section 4, in the order given.** After each step:
   - run `devtools::load_all()` and `devtools::test(filter = "permutation")`;
   - re-run the relevant dev script, switched to the package functions;
   - compare against the expected results in section 3.
6. **Ask Oscar** about the open decisions in section 6 when you reach them. Don't decide them silently.

---

## 2. The problem in one page

### 2.1 `textProjection()`

The observed statistic is fine and stays unchanged:
- Split participants on x (bottom vs. top quartile).
- Average each group's word embeddings, weighted by word frequency.
- The axis is d = mean_high − mean_low.
- Each word's score is its dot product with d, after centring on the groups' midpoint.

The **null distribution** is the issue. Currently, in `R/4_3_textPlotProjection.R`:
- The words of the low and high groups are pooled as one row per **token** (`uncount(n)`).
- Each token is given to group A or B by a coin flip, which gives one random axis.
- 50,000 token positions are drawn along that axis.
- **Every word is compared with this one pooled distribution:** `p = 2 * min(left, right)` in `p_value_comparing_with_Null()`, `R/0_2_privateFunctions.R`.

**Proposed: participant permutation.** Shuffle x across participants, recompute the whole statistic, and repeat B times. Each word is compared with its *own* null:
p = (1 + #{|null| ≥ |obs|}) / (B + 1).

**The two tests ask different questions.** Both are legitimate. Present them that way in the docs; don't call the token test "wrong".
- **Token shuffle:** within the words the low- and high-x groups wrote, does this word lean further towards one group than words do when the labels are assigned at random to individual tokens? Each token counts as an independent observation, so this *describes these texts*.
- **Participant permutation:** does this word lean further towards one group than the *same* word does when x is randomly reassigned among participants? Each participant counts as one observation, so this tests whether the word is related to x *in a way expected to hold for new people*.
- In both, x decides which participants form the groups. They differ in what counts as an independent observation.

**Why participant permutation should be the default:** `textProjection()` is mostly used to make claims about x. For such claims the token shuffle is liberal, for three reasons:
1. **One reference for all words.** Words far from the vocabulary centre look extreme on any axis.
2. **Tokens from the same person are treated as independent.** The person-to-person variation that two random groups of people show even when x is noise is not in the null.
3. **The default null comes from a single random split.** `ceiling(Npermutations / n_per_split)` = `ceiling(10000 / 50000)` = 1, so results depend on the seed.

### 2.2 `textWordPrediction()` (dev version 1.9, `R/4_4_textWordPrediction.R`)

What it does, per word:
1. Mean x of the participants who used the word.
2. Ridge regression (textTrainRegression) across words, predicting that mean from the word's embedding.
3. The cross-validated prediction is the word's plotted position.
4. p-values from 25 shuffles of x (`n_models = 25`), each a full refit, bootstrapped 10,000 times; p = mean(|null| ≥ |obs|).

Problems:
- **p = 0 whenever the observed value beats all 25 nulls,** and BH cannot remove p = 0. Bootstrapping the 25 values adds no information.
- **Substring counting.** `wordsMeanValue()` uses `stringi::stri_count_regex(text, word)` with no word boundaries: "he" is counted inside "the", "i" inside "it".
- **It errors on tokens that are regex metacharacters.** "?" gives `U_REGEX_RULE_SYNTAX`. `dep_wor_data` fails unless you pass `text_remove = "[^[:alnum:]'-]"`.
- **One NA in x makes every word that participant used NA,** and those words are dropped.
- **Colour split.** `textProjectionPlot()` colours by `x_plotted < 0` vs `> 0`. With raw x (PHQ-9 is 0–27), every word lands on the "high" side.
- **Frequent words lean one way when x relates to text length.** Each word's mean is pulled towards the token-weighted mean of x, not 0. In `dep_wor_data`, r(PHQ-9, words written) ≈ .14, which gives a token-weighted mean of z(PHQ-9) ≈ +0.08. Function words ("the", "to", "my") then come out significant for PHQ-9. That is a real association with verbosity, not a word-specific one.
- **It is slow:** roughly 25–40 minutes per variable on 500 short texts.

**The fix (prototype in `R/word_prediction_fast.R`).** With the ridge penalty fixed, everything is linear in x:
- m = D⁻¹C′x, where C is the participants × words *token* count matrix and D = diag(colSums C).
- Leave-one-out ridge predictions are pos = P m, where P depends only on the embeddings and λ.
- So pos = A x, with A computed once. Each permutation is one matrix product, so 10,000 permutations take seconds.
- λ is chosen once on the observed x (minimum leave-one-out error) and held fixed for the permutations.
- An option `centre = "token_mean"` subtracts the token-weighted mean of x, which separates word-specific signal from the verbosity effect.

### 2.3 Word ↔ embedding matching (affects both functions)

- **Which words are tested.** The words come from NLTK (`unique_freq_words()`, lower-cased). Each is looked up in `word_types$words` by **exact match** (`applysemrep()`, `R/1_2_textEmbedStatic.R`).
- **What `textEmbed()` stores.** `word_types` holds the tokenizer's tokens unchanged (`convert_ids_to_tokens`).
- **BERT/WordPiece:** words the model splits ("unhappiness" → un/##ha/##pp/##iness) silently get no embedding and are dropped.
- **RoBERTa:** tokens keep the "Ġ" marker and their case ("Ġhappy", "Happy", "ĠI"), so **almost no word matches**. In the toy test only "," and "." matched.
- Fix: `pp_normalise_word_types()` strips Ġ/▁, lower-cases and merges duplicates. Also report coverage, i.e. the share of word types and tokens that got an embedding.

---

## 3. Expected results on `topics::dep_wor_data`

These come from an earlier run with bert-base-uncased, layer 11, words with n ≥ 3. Re-embedding can shift counts somewhat; the pattern should hold.

| Check | Expected |
|---|---|
| Reimplemented dot products vs. `textProjection()`'s own | r ≈ 1 (identical to numerical precision) |
| PHQ-9, FDR-significant words: token / participant | ~527 / ~285 of ~869 |
| One noise variable: token / participant | ~38 / 0 |
| 50 noise variables, share with ≥ 1 FDR-significant word: token / participant | ~78% / ~2% (participant target ≤ ~5%) |
| 50 noise variables, mean % of words at p < .05: token / participant | ~9% / ~5.1% (target 5%) |
| Token null rebuilt with 20 seeds, same noise variable | from ~8 to ~300 FDR-significant words |
| Current `textWordPrediction()`, 10 noise variables | all 10 have FDR-significant words (1–50 each), exactly the p = 0 words |
| Current `textWordPrediction()`: PHQ-9 / noise | ~250 / ~18 FDR-significant words |
| Substring counting | ~30% of word types over-counted ≥ 10%, ~19% at least doubled; "he" ~147× its true count |
| Fast version: LOO shortcut vs. explicit refits | max difference ~1e-15 |
| Fast version, 50 noise variables, share with any FDR word: centre none / token_mean | 0% / ~2% |
| Fast version, 50 noise variables, mean % p < .05 | ~5.1% for both |
| Fast version, PHQ-9 FDR-significant words: none / token_mean | ~51 / ~44 |
| Fast version, rank agreement of positions with the current function | Spearman ~.95 |
| Fast version, run time | ~10–15 s per variable (10,000 permutations) |

Note on the participant test's 2%: it is not too conservative.
- The per-word rate averages 5.06%, on target. The median is lower (~2.9%) because words share one axis and rise and fall together, which skews the distribution.
- 5% is a ceiling for FDR under a complete null, not a target.
- With 50 variables, 1/50 has a 95% CI of 0.1–10.6%.

---

## 4. Changes to implement (in order)

### A. `textProjection()`, in `R/4_3_textPlotProjection.R`
1. Add `null_method = c("participant", "token")`, defaulting to `"participant"`.
   - Implement it on a sparse participants × word-types count matrix: see `pp_count_matrix()`, `pp_setup()`, `pp_dots()` and `pp_null_participant()` in `R/projection_permutation.R`.
   - Build the count matrix with the same NLTK tokenisation as `unique_freq_words()`, so the word types match one for one.
   - **The vocabulary that builds the axis is every word type with an embedding.** `min_freq_words_test` only decides what is *tested*. Applying the frequency floor before aggregating changes the axis, and the dot products then no longer match the package's.
2. Make `Npermutations` mean the number of permutations. Default 10,000.
   - Keep `"token"` for reproducing published results and for describing texts, with a doc note.
   - Fix the single-split problem there too: loop until `Npermutations` random axes, or deprecate `n_per_split`.
3. p = (1 + count) / (B + 1). Optionally also return `p_z`, a normal approximation from each word's own null, which has no 1/(B+1) floor. With ~2,000 tested words, BH needs the top p below about .05/2000 = 2.5e-5, so B ≥ 10,000 matters.
4. 2-D (`y`): permute x and y separately (as the prototype does), unless Oscar decides otherwise (section 6).
5. Output columns are unchanged (`p_values_dot.x`, …), so `textProjectionPlot()` needs no change. Don't adjust p twice: the plot applies `p_adjust_method` itself.
6. Docs: describe both tests as in section 2.1.

### B. Word ↔ embedding matching
7. Apply `pp_normalise_word_types()` when building or using word types. The best place is probably at the end of word-type aggregation in `textEmbed()` (`R/1_1_textEmbed.R`), or in `applysemrep()`. Ask Oscar which.
8. In `textProjection()` and `textWordPrediction()`, print coverage: the % of word types and % of tokens that have an embedding.

### C. `textWordPrediction()`, in `R/4_4_textWordPrediction.R`
9. Replace `wordsMeanValue()` regex counting with token counts from the count matrix. This fixes the substring bug, the regex crash and the speed.
10. Replace the `n_models` + bootstrap p-values with the linear-smoother permutation (`textWordPredictionFast()`).
    - The ridge penalty is chosen once by LOO on the observed x, then held fixed.
    - The plotted position is the LOO prediction rather than textTrainRegression's nested CV. Explain this in the docs.
    - Deprecate `n_models`.
11. Add `centre = c("token_mean", "none")`. The default is open (section 6).
12. Drop participants with missing x inside the function, with a message.
13. Colour split for word prediction: z-score x internally, or split at the centre of x rather than 0.

### D. Tests and docs
14. Move `tests/testthat/test-permutation-null.R` into the package tests and rename the functions to match.
    - Fast tests only, on synthetic data. They pass against the prototypes.
    - Expect "lambda at the edge of the grid" warnings on pure synthetic noise: the best penalty is effectively infinite. Silence them or widen the grid.
15. Optional slow test (skip on CRAN): 20 noise variables on `dep_wor_data`; the share with any FDR-significant word should be ≤ ~10%.
16. Add a NEWS.md entry and update the vignette text for both functions.

---

## 5. Files in this folder

| File | What it is |
|---|---|
| `R/projection_permutation.R` | Prototype functions: `pp_count_matrix`, `pp_setup`, `pp_dots`, `pp_null_token` (the current null, reimplemented), `pp_null_participant`, `pp_test`, `pp_replace_pvalues`, `pp_normalise_word_types` |
| `R/word_prediction_fast.R` | Prototype `textWordPredictionFast()`, plus the helpers `wpf_*`, `wpf_check_loo()` and `wpf_into_projection()` (plot through the released `textProjectionPlot()`) |
| `dev/_config.R` | Shared settings (env vars `TEXT_CONDAENV`, `TEXT_DEVICE`, `HANDOFF_DIR`); the `draw()` plot helper |
| `dev/00_setup_public_data.R` | Embeds `dep_wor_data`, builds the count matrix, runs `textProjection()` as shipped for PHQ-9 and for one noise variable |
| `dev/01_projection_null.R` | Statistic check, token vs. participant counts and plots, calibration on noise, seed dependence |
| `dev/02_word_prediction.R` | LOO check, fast word prediction (both centre options), plots, calibration, substring demo; `RUN_ORIGINAL=1` also runs the current dev `textWordPrediction()` (slow) |
| `dev/03_roberta_matching.R` | Toy demonstration of the RoBERTa and WordPiece matching problem, and the fix |
| `tests/testthat/test-permutation-null.R` | Draft unit tests (synthetic data, no Python) |

All scripts run from the folder root, e.g. `Rscript dev/01_projection_null.R`.

---

## 6. Open decisions for Oscar

1. **Default `null_method`.** Proposed: `"participant"`, keeping `"token"` as an option. To be discussed with Andrew.
2. **2-D plots.** Permute x and y separately (current prototype) or jointly (same shuffle for both)?
3. **`textWordPrediction()` default `centre`.** `"token_mean"` asks what is specific to the word; `"none"` is the current behaviour, and its results include verbosity effects.
4. **Where to normalise RoBERTa/SentencePiece word types.** In `textEmbed()` (changes stored objects) or at lookup time (`applysemrep()`)?
5. **Whether to return the normal-approximation `p_z`** alongside exceedance p.
6. **Back-compatibility.** Should published analyses that used the token default get a note in NEWS or the docs?

---

## 7. Gotchas met while prototyping

- **Word types need the right argument.** `textEmbed()` only returns word types when `aggregation_from_tokens_to_word_types` is set (e.g. `"mean"`); otherwise `word_types` is an empty list.
- **Evaluate x before calling `set.seed()`.** R arguments are lazy: `f(rnorm(n))` with `set.seed()` inside `f` would redraw the same "random" x on every call. Hence `force(x)` in `pp_test()`.
- **Don't correct p twice.** `textProjectionPlot()` applies `p.adjust()` itself. Pass raw p with `p_adjust_method = "fdr"`, or BH-adjusted p with `"none"`.
- **Small samples.** With 50 participants, PHQ-9 gives 0 significant words under both nulls; the token null still flags dozens of words for noise. Don't read 0 as a failure of the participant test.
- **Long runs need care.** The current dev `textWordPrediction()` takes a long time; run it in a separate R process if needed.
