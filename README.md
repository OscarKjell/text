
<!-- README.md is generated from README.Rmd. Please edit that file -->

# text

<!-- badges: start -->

<!-- badges: end -->

The `text`-package uses natural language processing and machine learning
methods to help you examine text variables, and their relation to
numerical variables.

## Installation

<!--You can install the released version of text from [CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("text")
```-->

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("oscarkjell/text")
```

The default method in `text` to map words to numbers uses BERT. BERT is
a general-purpose language model that creates state-of-the-art word
embeddings based on deep learning. To achieve this, `text` relies on
`RBERT`, which in turn is based on TensorFlow; so make sure that this
packages are properly installed and working (see
[RBERT](https://github.com/johnERgordon/RBERT).

## Map your text to numbers

You can easily transform text variables to BERT word embeddings:

``` r
library(text)
# Get example data including both text and numerical variables
sq_data <- sq_data_tutorial8_10

# Transform the text data to BERT word embeddings
wordembeddings <- textImport(sq_data)
```

## Examine how your text relate to numerical variables

``` r
library(text)

# Load already imported word embeddings, and their corresponding numeric variables
wordembeddings <- wordembeddings4_100
numeric_data <- sq_data_tutorial4_100

# Examine the relationship between harmony-text and Harmony in life scale (HILS) scores
model_htext_hils <- textTrain(wordembeddings$harmonytexts, numeric_data$hilstotal)
#> Loading required package: lattice
#> Loading required package: ggplot2

# Show the correlation between predicted and observed Harmony in life scale scores
model_htext_hils$Correlation
#> 
#>  Pearson's product-moment correlation
#> 
#> data:  model$pred$pred and model$pred$obs
#> t = 12.073, df = 98, p-value < 2.2e-16
#> alternative hypothesis: true correlation is not equal to 0
#> 95 percent confidence interval:
#>  0.6801794 0.8418399
#> sample estimates:
#>       cor 
#> 0.7732868
```

<!--
<img src="man/figures/README-pressure-1.png" width="100%" />

In that case, don't forget to commit and push the resulting figure files, so they display on GitHub!-->
