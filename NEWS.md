
<!-- README.md is generated from README.Rmd. Please edit that file -->

# text 0.9.93
- New function: `textSentiment()`, from huggingface transformers models.
- add progression for time consuming functions including `textEmbed()`, `textTrainRegression()`, `textTrainRandomForest()` and `textProjection()`. 

# text 0.9.92
- Option `dim_names` to set unique dimension names in `textEmbed()` and `textEmbedStatic()`.
- `textPreictAll()` function that can take several models, word embeddings, and variables as input to provide multiple outputs.
- option to add variables to the embeddings in `textTrain()` functions with `x_append`.

# text 0.9.91

### New feature
- text version is printed from DESCRIPTION-file (rather than manual update)
- `textPredict` related functions are located in its own file
- textEmbed comment includes `text_version` number
- `textEmbedLayersOutput` and `textEmbed` can provide `single_context_embeddings`

### Fixes
- Removed `return_tokens` option from textEmbed (since it is only relevant for textEmbedLayersOutput)
- removed the empty list `$single_we` when `decontexts` is `FALSE`.


# text 0.9.90
### New feature
- Visualization of the download process of language models
- Can set error level from python
- `Logistic` regression is default for classification in textTrain.
- Megatron language model functionality 

### Bug Fixes
- When GPU is not found, CPU is used.


# text 0.9.80
### New feature
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




