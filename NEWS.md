# text (development versions)

<!-- README.md is generated from README.Rmd. Please edit that file -->

# text 1.4.7
- Added checks for matching `word_embeddings` and `model` requirements in the `textPredict()` function. 
This is controlled via the new `check_matching_word_embeddings` parameter, which validates compatibility of model type, layers, and aggregation settings.
-	Added a name parameter to the `textDimName()` function, allowing users to specify or change the name suffix for word embedding dimensions.
- Improved the `dim_names` = `FALSE` behavior in the `textDimName()` function to also ignore model-required dimension suffixes. 
Now includes clearer and more informative warnings when dimension mismatches occur.

# text 1.4.6
- updating from the depracated `rsample::function validation_split()` to `initial_validation_split()`. However, 
this changes some results in `textTrainRegression()` and `textTrainRandomForrest()`.
- updating `textLBAM()` to take `construct_start` parameter.

# text 1.4.5
- removing objects in the environment of `textTrainRegression()` to reduce saved model sizes.

# text 1.4.2
- fixing bug in layer selection in `textEmbedRawLayers()` (when using default -2, layer 11 was selected even for large models). This was never a problem in `textEmbed()`.

# text 1.4.1
- adding the `dlatk_method` to the `textEmbed()` function.  

# text 1.4
- adding `cv_method` = "group_cv" in the `textTrainRegression()` function. 

# text 1.3.6
- fixing python dependency (aiohappyeyeballs)
- adding parameter `plot_n_word_random` and `legend_number_colour` in textPlot. 
- removed `nltk` warning when running the functions requiring pyhon.
- anchouring group word embeddings in the `textProjection()` function.
- adding cohen's d to the output of the `textProjection()` function

# text 1.3.4
- harmonizing wordclouds with topics-package
- implementing `textTrainExamples()`
- updating legend plots.

## Result change
- # Result change: Updating the workflows package from version "1.1.4"  to "1.2.0" result in changes in the random forest function tests.

# text 1.3.1
- included parameter `highest_parameter` and `lowest_parameter` when parameters are tied. 

# text 1.3.0
- Alias function: `textPredict()`, `textAssess()` and `textClassify()`.
- LBAM integration with `textLBAM()`.
- Full support of implicit motives models.
- Text cleaning functionality with `textClean()` (removing common personal information).
- Compatability with the topics-package, see [www.r-topics.org](https://r-topics.org/).

# text 1.2.17
- `textLBAM()` returns the library as a datafram

# text 1.2.16
- `textPredict()` detects `model_type`.
- Instead of having to specify the URL, one can now specify the model name from 
the Language-Based Assessmet Model (L-BAM) library. 
- Including default option to download an updated version of the L-BAM file

# text 1.2.8 - 1.2.13
- fixing bugs related to text prediction functions
- adding method_typ = "texttrained" and "finetuned"
- streamlining code for implicit motives output
- adding `textFindNonASCII()` function and feature in `textEmbed()` to 
warn and clean non-ASCII characters. This may change results slightly. 
- removed `type` parameter in textPredict() and instead giving both probability and class.

# text 1.2.7
- `textClassify()` is now called `textClassifyPipe() `
- `textPredict()` is now called `textPredictR()`
- Making `textAssess()`, `textPredict()` and `textClassify()` works the same, now taking the parameter `method` with the string "text" to using textPredict(), and 
"huggingface" to using textClassifyPipe(). 


# text 1.2.6
- updating python code, including adding parameters `hg_gated`, `hg_token`, and `trust_remote_code`. 
- changed parameter name from `return_incorrect_results` to `force_return_results`
- changed default of `function_to_apply` = NULL instead of "none"; this 
is to mimic huggingface default.
- `textWordPrediction` since it is under development and note tested.

# text 1.2.5
- updating security issues with python packages. 
- updating the default range of penalties in textTrain() functions. 
- updating textPredict() functionality

# text 1.2.2
- Improving `textTrainN()` including `subsets` sampling (new: default change from `random` to `subsets`), `use_same_penalty_mixture` (new:default change from `FALSE` to `TRUE`) and `std_err` (new output).
- Improving `textTrainPlot()`

# text 1.2.1
- Improving `textPredict()` functionality. 
- Implementing experimental features related to `textTopics()`

# text 1.2
## Functions
- `textTopics()` trains a BERTopic model with different modules and returns the model, data, and topic_document distributions based on c-td-idf
- `textTopicsTest()` can perform multiple tests (correlation, t-test, regression) between a BERTopic model from `textTopics()` and data
- `textTopicsWordcloud()` can plot word clouds of topics tested with `textTopicsTest()`
- `textTopicsTree()` prints out a tree structure of the hierarchical topic structure

# text 1.1
## Functions
- `textEmbed()` is now fully embedding one column at the time; and reducing word_types for each column. This can break some code; and produce different results in plots where word_types are based on several embedded columns.
- `textTrainN()` and `textTrainNPlot()` evaluates prediction accuracy across number of cases. 
- `textTrainRegression()` and `textTrainRandomForest` now takes tibble as input in strata. 


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




