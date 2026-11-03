## Shared settings for the dev scripts. Run them from the text_handoff/
## folder (or set HANDOFF_DIR), e.g.  Rscript dev/00_setup_public_data.R

HANDOFF_DIR <- Sys.getenv("HANDOFF_DIR", ".")
CONDAENV    <- Sys.getenv("TEXT_CONDAENV", "")      # e.g. "textrpp_condaenv"
DEVICE      <- Sys.getenv("TEXT_DEVICE", "cpu")     # "gpu", "mps" ...
MIN_FREQ    <- 3          # words tested / plotted (dep_wor_data is small)
ALPHA       <- 0.05

suppressPackageStartupMessages({
  library(text); library(Matrix); library(ggplot2)
})
if (nzchar(CONDAENV)) text::textrpp_initialize(condaenv = CONDAENV, save_profile = FALSE) else
  text::textrpp_initialize(save_profile = FALSE)

source(file.path(HANDOFF_DIR, "R", "projection_permutation.R"))
source(file.path(HANDOFF_DIR, "R", "word_prediction_fast.R"))

CACHE <- file.path(HANDOFF_DIR, "dev", "cache");  dir.create(CACHE,  FALSE, TRUE)
OUT   <- file.path(HANDOFF_DIR, "dev", "output"); dir.create(OUT,    FALSE, TRUE)

log_step <- function(...) message(format(Sys.time(), "[%H:%M:%S] "), ...)

get_word_types <- function(emb) {
  wt <- emb$word_types
  while (!is.data.frame(wt)) wt <- wt[[1]]
  wt
}

# the noise variable used for the example figures, and the calibration set
noise_example <- function(n) { set.seed(2026); stats::rnorm(n) }
noise_set     <- function(n, k = 50) { set.seed(58); lapply(seq_len(k), function(i) stats::rnorm(n)) }

# textProjectionPlot() with fixed settings so all panels are comparable
draw <- function(proj, file, xlab, adjust = "fdr") {
  p <- text::textProjectionPlot(
    word_data = proj, min_freq_words_test = MIN_FREQ, min_freq_words_plot = MIN_FREQ,
    plot_n_word_extreme = 10, plot_n_words_square = 8, plot_n_words_p = 8,
    plot_n_word_frequency = 5, plot_n_words_middle = 5, plot_n_word_random = 0,
    y_axes = FALSE, title_top = "", x_axes_label = xlab, y_axes_label = " ",
    p_alpha = ALPHA, p_adjust_method = adjust)
  ggplot2::ggsave(file, p$final_plot, width = 8, height = 6, dpi = 150)
  invisible(file)
}

# FDR-significant words among tested words in a textProjection() object
pkg_count <- function(proj) {
  wd <- proj$word_data
  wd <- wd[!wd$words %in% c("Group1*", "Group2*", "projected_embedding"), ]
  wd <- wd[wd$n >= MIN_FREQ & !is.na(wd$p_values_dot.x), ]
  c(sig = sum(stats::p.adjust(wd$p_values_dot.x, "fdr") < ALPHA), n = nrow(wd))
}
