
<!-- README.md is generated from README.Rmd. Please edit that file -->

# text 0.9.12


### New Features
- In `textProjetion` and `textProjetionPlot` it now possible to add points of the aggregated word embeddings in the plot
- In `textProjetion` it now possible to manually add words to the plot in order to explore them in the word embedding space.
- In `textProjetion` it is possible to add color or remove words that are more frequent on the opposite "side" of its dot product projection. 
- In `textProjection` with `split == quartile`, the comparison distribution is now based on the quartile data (rather than the data for mean)

### Bug Fixes
- If any of the tokens to remove is "[CLS]", subtract 1 on token_id so that it works with layer_aggregation_helper. 0.9.11

# text 0.9.11 
- `textSimilarityTest` is not giving error when using method = unpaired, with unequal number of participants in each group. 

### New Features
- `textPredictTest` function to significance test correlations of different models. 0.9.11 

### Bug Fixes
- If any of the tokens to remove is "[CLS]", subtract 1 on token_id so that it works with layer_aggregation_helper. 0.9.11

# text 0.9.10 
This version is now on CRAN.
### New Features
- Adding option to deselect the `step_centre` and `step_scale` in training.
- Cross-validation method in `textTrainRegression()` and `textTrainRandomForrest()` have two options `cv_folds` and `validation_split`. (0.9.02)
- Better handling of `NA` in `step_naomit` in training. 
- DistilBert model works (0.9.03)

### Major changes


### Bug Fixes
- `textProjectionPlot()` plots words extreme in more than just one feature (i.e., words are now plotted that satisfy, for example, *both* `plot_n_word_extreme` and `plot_n_word_frequency`). (0.9.01)
- `textTrainRegression()` and `textTrainRandomForest()` also have function that select the max evaluation measure results (before only minimum was selected all the time, which, e.g., was correct for rmse but not for r) (0.9.02)
- removed `id_nr` in training and predict by using workflows (0.9.02).

### Minor changes




