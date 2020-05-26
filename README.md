
<!-- README.md is generated from README.Rmd. Please edit that file -->

# text

<!-- badges: start -->

<!-- badges: end -->

The `text`-package uses natural language processing and machine learning
methods to assist in examining text variables, as well as their relation
to numerical variables.

## Installation

You can install the released version of text from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("text")
```

The default method to map words to numbers in `text` uses BERT. BERT is
a general-purpose language model that creates state-of-the-art word
embeddings based on deep learning. To achieve this, `text` relies on
`RBERT`, which in turn is based on TensorFlow; so please make sure that
these packages are properly installed and working\! For up-to-date
instructions see [RBERT](https://github.com/johnERgordon/RBERT).

When above packages are installed and working, you can install the
development version `text` from [GitHub](https://github.com/), by first
installing devtools and then the command bellow.

``` r
# install.packages("devtools")
devtools::install_github("oscarkjell/text")
```

## Map your text to numbers

You can easily transform text variables to BERT word embeddings:

``` r
library(text)
# Get example data including both text and numerical variables
sq_data <- sq_data_tutorial8_10

# Transform the text data to BERT word embeddings
# wordembeddings <- textEmbedd(sq_data)
```

``` r
library(text)
# Load already imported word embeddings, and their corresponding numeric variables
wordembeddings <- wordembeddings1_100
numeric_data <- sq_data_tutorial4_100


# Examine the relationship between harmony-text and Harmony in life scale (HILS) scores
model_htext_hils <- textTrain(wordembeddings$harmonytexts, numeric_data$hilstotal, nrFolds_k = 2)
#> Warning: The `maximize` argument is no longer needed. This value was ignored.

# Show the correlation between predicted and observed Harmony in life scale scores
model_htext_hils$correlation
#> 
#>  Pearson's product-moment correlation
#> 
#> data:  df3_predictions$y and df3_predictions$.pred
#> t = 10.62, df = 98, p-value < 2.2e-16
#> alternative hypothesis: true correlation is not equal to 0
#> 95 percent confidence interval:
#>  0.6248292 0.8113259
#> sample estimates:
#>      cor 
#> 0.731475
```

``` r
library(text)
# Example data that have been pre-processed with textPlotData function
# The test-data included in the package is called: sq_data_tutorial_plotting_hilswl
load("data/sq_data_tutorial_plotting_hilswl.rda")
plot_projection <- textPlotViz(
word_data = sq_data_tutorial_plotting_hilswl,
plot_n_words_p = 50,
x_axes = "dot.x",
y_axes = NULL,
x_axes_label = "Dot product",
y_axes_label = NULL,
p_alpha = .05,
p_values_x = "p_values_dot.x",
p_values_y = NULL,
p_adjust_method = "BY",
title_top = "Dot-Project",
scale_y_axes_lim = NULL
)
plot_projection
```

<img src="man/figures/README-DPP_plot-1.png" width="100%" />
