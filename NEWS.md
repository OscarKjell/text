# text (development version)


<!-- README.md is generated from README.Rmd. Please edit that file -->

# text 1.1
## Functions
- textEmbed() is now fully embedding one column at the time; and reducing word_types for each column. This can break some code; and produc different results in plots where word_types and based on several embedded columns.
- textTrainN() and textTrainNPlot evaluates prediction accuracy across cases. 



# text 1.0
## Function
- multinomial regression in `textTrainRegression()`
- `textPredictTest()` can handle `auc`
- `textEmbed()` is faster (thanks to faster handling of aggregating layers)
- Added `sort` parameter in `textEmbedRawLayers()`. 

## Bug/unexpected behaviour
- Tests using training with random forest was updated since outcomes changed when updating from R 4.2 to R 4.3.1. (see test_2_textTrain.R in tests/testthat folder) 

# text 0.9.99.9
## Function
Possibility to use GPU for MacOS M1 and M2 chip using device = "mps" in `textEmbed()`

# text 0.9.99.8
## Function
`textFineTune()` as an experimental function is implemented
`max_length` implemented in `textTranslate()`

# text 0.9.99.7
## Function
- `textEmbedReduce()` implemented


# text 0.9.99.6
## Bug Fix
- Fixing textEmbed error for many non-BERT models. 


# text 0.9.99.5
## Bug Fix
- fixed `textEmbed(decontextualize=TRUE)`, which gave error.

# text 0.9.99.3

## Functions changes
- Removing `textSimialirtyTest()` for version 1.0 because it needs more evaluations. 

## Bug Fix
- changed hard coded "bert-base-uncased" to `model`, so that `layers` = -2 works in `textEmbed()`. 
- Update logging level critical using integer 50 with `set_verbosity`.
- changed in `sorting_xs_and_x_append` from Dim to Dim0 when renaming x_appended variables.
- changed `first` to `append_first` and  made it an option in `textTrainRegression()` and `textTrainRandomForest()`. 

# text 0.9.99.2

## DEFAULT CHANGES
- The default setting of textEmbed() is now providing token-level embeddings and text-level embeddings. Word_type embeddings are optional. 
- In `textEmbed()` `layers = 11:12` is now `second_to_last`.
- In `textEmbedRawLayers` default is now `second_to_last`.
- In `textEmbedLayerAggregation()`  `layers = 11:12` is now `layers = "all"`.
- In `textEmbed()` and `textEmbedRawLayers()` `x` is now called `texts`.
- `textEmbedLayerAggregation()` now uses `layers = "all"`, `aggregation_from_layers_to_tokens`, `aggregation_from_tokens_to_texts`. 


## New Function
- `textZeroShot()` is implemented.
- `textDistanceNorm()` and `textDistanceMatrix()`
- `textDistance()` can compute cosine `distance`.
- `textModelLayers()` provides N layers for a given model

## New Setting
`max_token_to_sentence` in `textEmbed()`

## Setting name changes
- `aggregate_layers` is now called `aggregation_from_layers_to_tokens`.
- `aggregate_tokens` is now called `aggregation_from_tokens_to_texts`.
`single_word_embeddings` is now called `word_types_embeddings`

## Function name changes
- `textEmbedLayersOutput()` is now called `textEmbedRawLayers()`


# text 0.9.98
 - adding `textDimName()`
 - DEFAULT CHANGE in `textEmbed()`: `dim_name` = `TRUE`
 - DEFAULT CHANGE in `textEmbed()`: `single_context_embeddings` = `TRUE`
 - DEFAULT CHANGE in `textEmbed()`: device = "gpu"
 - Adding specific layer aggregations for `explore_words` in `textPlot()`
 - Adding `x_append_target` in `textPredict()` function

# text 0.9.97
 - updating `textClassify()`, `textGeneration()`, `textNER()`, `textSum()`, `textQA()`, and `textTranslate()`.


# text 0.9.96
### New features
  - harmonizing `x_add` to `x_append` across functions
  - adding `set_seed` to language analysis tasks

### Code changes
- abstracting function for sorting out `x'` in training and prediction
- `textPredict` does not take `word_embeddings` and `x_append` (not `new_data`)


# text 0.9.95
### New features
  - `textClassify()` (under development)
  - `textGeneration()` (under development)
  - `textNER()` (under development)
  - `textSum()` (under development)
  - `textQA()` (under development)
  - `textTranslate()` (under development)

# text 0.9.93
### New features
- New function: `textSentiment()`, from huggingface transformers models.
- add progression for time consuming functions including `textEmbed()`, `textTrainRegression()`, `textTrainRandomForest()` and `textProjection()`. 

# text 0.9.92
### New features
- Option `dim_names` to set unique dimension names in `textEmbed()` and `textEmbedStatic()`.
- `textPreictAll()` function that can take several models, word embeddings, and variables as input to provide multiple outputs.
- option to add variables to the embeddings in `textTrain()` functions with `x_append`.

# text 0.9.91

### New features
- text version is printed from DESCRIPTION-file (rather than manual update)
- `textPredict` related functions are located in its own file
- textEmbed comment includes `text_version` number
- `textEmbedLayersOutput` and `textEmbed` can provide `single_context_embeddings`

### Fixes
- Removed `return_tokens` option from textEmbed (since it is only relevant for textEmbedLayersOutput)
- removed the empty list `$single_we` when `decontexts` is `FALSE`.


# text 0.9.90
### New features
- Visualization of the download process of language models
- Can set error level from python
- `Logistic` regression is default for classification in textTrain.
- Megatron language model functionality 

### Bug Fixes
- When GPU is not found, CPU is used.


# text 0.9.80
### New features
- Option to set `model_max_length` in `textEmbed()`.
- `textModels()` show downloaded models.
- `textModelsRemove()` deletes specified models.

### Bug Fixes
- Fixed error for unpaired `textSimilarityTest()` when uneven number of cases are tested. 

# text 0.9.70
### New Features
- Inclusion of `textDistance()` function with distance measures.
- Adding more measures to `textSimilarity()`.
- Adding functionality from `textSimilarity()` in `textSimilarityTest()`, `textProjection()` and `textCentrality()` for plotting.
- Adding information about how `textTrainRegression()` concatenates word embeddings when provided with a list of several word embeddings.
- Adding two word embedding dimensions to example data of single word embeddings to match the 10 of the contextualized embeddings in `word_embeddings_4$singlewords_we`. 

### Bug Fixes
- In `textCentrality()`, words to be plotted are selected  with `word_data1_all$extremes_all_x >= 1` (rather than `==1`).

# text 0.9.60

- `textSimilarityMatrix()` computes semantic similarity among all combinations in a given word embedding. 

# text 0.9.54

- `textDescriptives()` gets options to remove NA and compute total scores.

# text 0.9.53

- inclusion of `textDescriptives()`


# text 0.9.52

- prompt option added to `textrpp_initiate()`


# text 0.9.51

- `tokenization` is made with `NLTK` from python.



# text 0.9.50

- Code has been cleaned up and prepared for CRAN

# text 0.9.20
### New Features
- New functions being tested: `textWordPredictions()` (which has a trial period/not fully developed and might be removed in future versions); p-values are not yet implemented.
- Possibility to use `textPlot()` for objects from both `textProjection()` and `textWordPredictions()`


### Minor changes
- Changed wordembeddigs to word_embeddings through out the code/package.

### Bug Fixes
- Warnings about seed when using multi-cores on Mac is addressed. 

# text 0.9.17
### New Features
- `textrpp_initiate()` runs automatically in `library(text)` when default environment exits
- Python warnings a captured in embedding comments
- Option to print python options to console
- Updated the permutation test for plotting and `textSimilarityTest()`.  

### Minor changes
- Changed from `stringr` to `stringi` (and removed tokenizer) as imported package

# text 0.9.16
### New Features
- `textrpp_install()` installs a `conda` environment with text required python packages.
- `textrpp_install_virtualenv()` install a virtual environment with text required python packages.
- `textrpp_initialize()` initializes installed environment. 
- `textrpp_uninstall()` uninstalls `conda` environment.

# text 0.9.13
### New Features
- `textEmbed()` and `textEmbedLayersOutput()` support the use of GPU using the `device` setting.  
- `remove_words` makes it possible to remove specific words from `textProjectionPlot()`

# text 0.9.12

### New Features
- In `textProjetion()` and `textProjetionPlot()` it now possible to add points of the aggregated word embeddings in the plot
- In `textProjetion()` it now possible to manually add words to the plot in order to explore them in the word embedding space.
- In `textProjetion()` it is possible to add color or remove words that are more frequent on the opposite "side" of its dot product projection. 
- In `textProjection()` with `split == quartile`, the comparison distribution is now based on the quartile data (rather than the data for mean)

### Bug Fixes
- If any of the tokens to remove is "[CLS]", subtract 1 on token_id so that it works with layer_aggregation_helper. 0.9.11
- Can now submit one word to `textEmbed()` with `decontexts=TRUE`. 

# text 0.9.11 
- `textSimilarityTest()` is not giving error when using method = unpaired, with unequal number of participants in each group. 

### New Features
- `textPredictTest()` function to significance test correlations of different models. 0.9.11 

### Bug Fixes
- If any of the tokens to remove is "[CLS]", subtract 1 on token_id so that it works with layer_aggregation_helper. 0.9.11

# text 0.9.10 
This version is now on CRAN.
### New Features
- Adding option to deselect the `step_centre` and `step_scale` in training.
- Cross-validation method in `textTrainRegression()` and `textTrainRandomForrest()` have two options `cv_folds` and `validation_split`. (0.9.02)
- Better handling of `NA` in `step_naomit` in training. 
- `DistilBert` model works (0.9.03)

### Major changes


### Bug Fixes
- `textProjectionPlot()` plots words extreme in more than just one feature (i.e., words are now plotted that satisfy, for example, *both* `plot_n_word_extreme` and `plot_n_word_frequency`). (0.9.01)
- `textTrainRegression()` and `textTrainRandomForest()` also have function that select the max evaluation measure results (before only minimum was selected all the time, which, e.g., was correct for rmse but not for r) (0.9.02)
- removed `id_nr` in training and predict by using workflows (0.9.02).

### Minor changes




