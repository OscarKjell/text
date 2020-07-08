
<!-- README.md is generated from README.Rmd. Please edit that file -->

# text

<!-- badges: start -->

<!-- badges: end -->

The language that individuals use contains a wealth of information
interesting for research. *Text* analyzes and visualizes text, and their
relation to other text or numerical variables. The r-package is based on
state-of-the-art techniques from statistics and artificial intelligence,
including natural language processing, deep learning and machine
learning.

*Text* is created through a collaboration between psychology and
computer science to address research needs and ensure state-of-the-art
techiniques. It provides powerful functions tailored to test research
hypotheses in social and behaviour sciences for both relatively small
and large datasets.

### Short installation guide

Most users simply need to run below installation code. For those
experiencing problems, please see the [Extended Installation
Guide](https://www.r-text.org/articles/Extended%20Installation%20Guide.html).

[CRAN](https://CRAN.R-project.org) version:

``` r
install.packages("text")
```

[GitHub](https://github.com/) development version:

``` r
# install.packages("devtools")
devtools::install_github("oscarkjell/text")
```

### Map your text to numbers

Recent significant advances in NLP research have resulted in improved
representations of human language (i.e., language models). These
language models have produced big performance gains in tasks related to
understanding human language. Using deep learning to model word order
and context now gives great results. Multilingual language models can
also represent several languages; multilingual BERT comprises *104
different languages*. Text are making these SOTA models easily
accessible through an interface to
[HuggingFace](https://huggingface.co/transformers/) in Python.

Model | Reference | Layers | States/dimensions | Language
‘bert-base-uncased’ | Devlan et al., 2019 | 12 | 768 | English *Table
1. Some of the available language models*

| model\_short    | tokenizer\_short    | pretrained\_weights\_shortcut\_short |
| :-------------- | :------------------ | :----------------------------------- |
| BertModel       | BertTokenizer       | ‘bert-base-uncased’                  |
| BertModel       | BertTokenizer       | **‘bert-multilingual-uncased’**      |
| XLNetModel      | XLNetTokenizer      | ‘xlnet-base-cased’                   |
| DistilBertModel | DistilBertTokenizer | ‘distilbert-base-cased’              |
| RobertaModel    | RobertaTokenizer    | ‘roberta-base’                       |

See [HuggingFace’s Github](https://github.com/huggingface/transformers)
for a more comprehensive list of models.

### An end-to-end package

Text also provides functions to analyse the word embeddings, with
well-tested machine learning algorithms and statistics. An example is
how it is possible to plot statistically significant words in the word
embedding space.

``` r
library(text)
#> [0;32mThis is text (version 0.6.0.9000).[0m
#>  [0;34mNewer versions may have updated default settings to reflect current understandings of the state-of-the-art.[0m
# Use data (DP_projections_HILS_SWLS_100) that have been pre-processed with the textProjectionData function

# The test-data included in the package is called: sq_data_tutorial_plotting_hilswl
plot_projection <- textProjectionPlot(
  word_data = DP_projections_HILS_SWLS_100,
  k_n_words_two_test = TRUE, 
  plot_n_words_square = 3,
  plot_n_words_p = 3,
  plot_n_word_extreme = 1,
  plot_n_word_frequency = 1,
  plot_n_words_middle = 1,
  x_axes = "dot.x",
  y_axes = "dot.y",
  p_values_x = "p_values_dot.x",
  p_values_y = "p_values_dot.y",
  p_alpha = 0.05,
  title_top = " Dot Product Projection (DPP) of Harmony in life words",
  x_axes_label = "Low vs. High HILS score",
  y_axes_label = "Low vs. High SWLS score",
  p_adjust_method = "bonferroni",
  scale_x_axes_lim = c(-15, 15),
  scale_y_axes_lim = c(-15, 15),
  y_axes_values_hide = FALSE
)
plot_projection
```

<img src="man/figures/README-DPP_plot-1.png" width="100%" />
