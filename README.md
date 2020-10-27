
<!-- README.md is generated from README.Rmd. Please edit that file -->

# text <img src="man/figures/text_logo_animation.gif" align="right" alt="" width="330" />

<!-- badges: start -->

[![Lifecycle:
maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
<!--    [![CRAN Status](https://www.r-pkg.org/badges/version/text)](https://cran.r-project.org/package=text) 
[![Github build status](https://github.com/oscarkjell/text/workflows/R-CMD-check/badge.svg)](https://github.com/oscarkjell/text/actions)-->
[![codecov](https://codecov.io/gh/oscarkjell/text/branch/master/graph/badge.svg?token=IZI3IKCFYA)](https://codecov.io/gh/oscarkjell/text)

[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)

<!-- badges: end -->

The language that individuals use contains a wealth of psychological
information interesting for research. The *text*-package has two main
objectives:

  - First, to serve R-users as a *point solution* for transforming text
    to state-of-the-art word embeddings that are ready to be used for
    downstream tasks.

  - Second, to serve as an *end-to-end solution* that provides
    state-of-the-art AI techniques tailored for social and behavioral
    scientists.

![Modular and End-to-End Solution](man/figures/modular_end_solution.png)

*Text* is created through a collaboration between psychology and
computer science to address research needs and ensure state-of-the-art
techniques. It provides powerful functions tailored to test research
hypotheses in social and behavior sciences for both relatively small and
large datasets. *Text* is continuously tested on Ubuntu, Mac OS and
Windows using the latest stable R version.

### Short installation guide

Most users simply need to run below installation code. For those
experiencing problems, please see the [Extended Installation
Guide](https://www.r-text.org/articles/Extended%20Installation%20Guide.html).

<!--[CRAN](https://CRAN.R-project.org) version:

``` r
install.packages("text")
```-->

[GitHub](https://github.com/) development version:

``` r
# install.packages("devtools")
devtools::install_github("oscarkjell/text")
```

### Point solution for transforming text to embeddings

Recent significant advances in NLP research have resulted in improved
representations of human language (i.e., language models). These
language models have produced big performance gains in tasks related to
understanding human language. Text are making these SOTA models easily
accessible through an interface to
[HuggingFace](https://huggingface.co/transformers/) in Python.

``` r
library(text)
# Transform the text data to BERT word embeddings
wordembeddings <- textEmbed(Language_based_assessment_data_8, 
                            model = 'bert-base-uncased')
```

*Text* provides many of the contemporary state-of-the-art language
models that are based on deep learning to model word order and context.
Multilingual language models can also represent several languages;
multilingual BERT comprises *104 different languages*.

*Table 1. Some of the available language models*

| Models                         | References                                                       | Layers | Dimensions | Language                                                                             |
| :----------------------------- | :--------------------------------------------------------------- | :----- | :--------- | :----------------------------------------------------------------------------------- |
| ‘bert-base-uncased’            | [Devlin et al. 2019](https://www.aclweb.org/anthology/N19-1423/) | 12     | 768        | English                                                                              |
| ‘roberta-base’                 | [Liu et al. 2019](https://arxiv.org/abs/1907.11692)              | 12     | 768        | English                                                                              |
| ‘distilbert-base-cased’        | [Sahn et al., 2019](https://arxiv.org/abs/1910.01108)            | 6      | 768        | English                                                                              |
| ‘bert-base-multilingual-cased’ | [Devlin et al. 2019](https://www.aclweb.org/anthology/N19-1423/) | 12     | 768        | [104 top languages at Wikipedia](https://meta.wikimedia.org/wiki/List_of_Wikipedias) |
| ‘xlm-roberta-large’            | [Liu et al](https://arxiv.org/pdf/1907.11692.pdf)                | 24     | 1024       | [100 language](https://huggingface.co/transformers/multilingual.html)                |

See
[HuggingFace](https://huggingface.co/transformers/pretrained_models.html)
for a more comprehensive list of models.

### An end-to-end package

*Text* also provides functions to analyse the word embeddings with
well-tested machine learning algorithms and statistics. The focus is to
analyze and visualize text, and their relation to other text or
numerical variables. An example is functions plotting statistically
significant words in the word embedding space.

``` r
library(text) 
# Use data (DP_projections_HILS_SWLS_100) that have been pre-processed with the textProjectionData function; the preprocessed test-data included in the package is called: DP_projections_HILS_SWLS_100
plot_projection <- textProjectionPlot(
  word_data = DP_projections_HILS_SWLS_100,
  y_axes = TRUE,
  title_top = " Supervised Bicentroid Projection of Harmony in life words",
  x_axes_label = "Low vs. High HILS score",
  y_axes_label = "Low vs. High SWLS score",
  position_jitter_hight = 0.5,
  position_jitter_width = 0.8
)
plot_projection
#> $final_plot
```

<img src="man/figures/README-DPP_plot-1.png" width="100%" />

    #> 
    #> $description
    #> [1] "INFORMATION ABOUT THE PROJECTION  INFORMATION ABOUT THE PLOT word_data = DP_projections_HILS_SWLS_100 k_n_words_to_test = FALSE min_freq_words_test = 1 min_freq_words_plot = 1 plot_n_words_square = 3 plot_n_words_p = 5 plot_n_word_extreme = 5 plot_n_word_frequency = 5 plot_n_words_middle = 5 y_axes = TRUE p_alpha = 0.05 p_adjust_method = none bivariate_color_codes = #398CF9 #60A1F7 #5dc688 #e07f6a #EAEAEA #40DD52 #FF0000 #EA7467 #85DB8E word_size_range = 3 - 8 position_jitter_hight = 0.5 position_jitter_width = 0.8 point_size = 0.5 arrow_transparency = 0.5 points_without_words_size = 0.2 points_without_words_alpha = 0.2 legend_x_position = 0.02 legend_y_position = 0.02 legend_h_size = 0.2 legend_w_size = 0.2 legend_title_size = 7 legend_number_size = 2"
    #> 
    #> $processed_word_data
    #> # A tibble: 583 x 32
    #>    words    dot.x p_values_dot.x n_g1.x n_g2.x  dot.y p_values_dot.y n_g1.y
    #>    <chr>    <dbl>          <dbl>  <dbl>  <dbl>  <dbl>          <dbl>  <dbl>
    #>  1 able   6.86e-1     0.194          NA      1  2.31      0.0123         NA
    #>  2 acce…  1.52e+0     0.0272         -1      2  1.15      0.0620         -1
    #>  3 acco…  2.14e+0     0.00856        NA      1  3.51      0.00273        NA
    #>  4 acti…  1.23e+0     0.0503         NA      1  1.56      0.0361         NA
    #>  5 adap… -3.87e-4     0.969          -1     NA  0.331     0.476          -1
    #>  6 admi…  5.14e-1     0.315          NA      1  1.52      0.0398         NA
    #>  7 adri… -3.79e+0     0.00000100     -1     NA -3.60      0.00000100     -1
    #>  8 affi…  7.49e-1     0.150          NA      1  2.04      0.0184         NA
    #>  9 agre…  2.23e+0     0.00626        NA      1  1.69      0.0312         NA
    #> 10 alco… -5.51e-1     0.318          -1     NA -1.07      0.0605         -1
    #> # … with 573 more rows, and 24 more variables: n_g2.y <dbl>, n <int>,
    #> #   n.percent <dbl>, N_participant_responses <int>, adjusted_p_values.x <dbl>,
    #> #   adjusted_p_values.y <dbl>, square_categories <dbl>, check_p_square <dbl>,
    #> #   check_p_x_neg <dbl>, check_p_x_pos <dbl>, check_extreme_max_x <dbl>,
    #> #   check_extreme_min_x <dbl>, check_extreme_frequency_x <dbl>,
    #> #   check_middle_x <dbl>, extremes_all_x <dbl>, check_p_y_pos <dbl>,
    #> #   check_p_y_neg <dbl>, check_extreme_max_y <dbl>, check_extreme_min_y <dbl>,
    #> #   check_extreme_frequency_y <dbl>, check_middle_y <dbl>,
    #> #   extremes_all_y <dbl>, extremes_all <dbl>, colour_categories <chr>
