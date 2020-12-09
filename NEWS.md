
<!-- README.md is generated from README.Rmd. Please edit that file -->


# text 0.9.02 

### Major changes
- Cross-validation method in `textTrainRegression()` and `textTrainRandomForrest()` have two options `cv_folds` and `validation_split`. (0.9.02)

### Bug Fixes
- `textProjectionPlot` plots words extreme in more than just one feature (i.e., words are now plotted that satisfy, for example, *both* `plot_n_word_extreme` and `plot_n_word_frequency`). (0.9.01)
- `textTrainRegression` and `textTrainRandomForest` also have function that select the max evalutation measure results (before only minimum was selected all the time, which, e.g., was correct for rmse but not r) (0.9.02)
- removed `id_nr` in training and predict by using workflows (0.9.02). 

### Minor changes
Adding option to deselect the `step_centre` and `step_scale` in training.



