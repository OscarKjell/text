% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/2_2_textTrainRegression.R
\name{textTrainRegression}
\alias{textTrainRegression}
\title{Train word embeddings to a numeric variable.}
\usage{
textTrainRegression(
  x,
  y,
  x_append = NULL,
  append_first = FALSE,
  cv_method = "validation_split",
  id_variable = NULL,
  outside_folds = 10,
  inside_folds = 3/4,
  strata = "y",
  outside_strata = TRUE,
  outside_breaks = 4,
  inside_strata = TRUE,
  inside_breaks = 4,
  model = "regression",
  eval_measure = "default",
  save_aggregated_word_embedding = FALSE,
  language_distribution = NULL,
  language_distribution_min_words = 3,
  preprocess_step_center = TRUE,
  preprocess_step_scale = TRUE,
  preprocess_PCA = NA,
  penalty = 10^seq(-6, 6),
  parameter_selection_method = "lowest_penalty",
  mixture = c(0),
  first_n_predictors = NA,
  impute_missing = FALSE,
  method_cor = "pearson",
  model_description = "Consider writing a description of your model here.",
  multi_cores = "multi_cores_sys_default",
  save_output = "all",
  simulate.p.value = FALSE,
  seed = 2020,
  weights = NULL,
  ...
)
}
\arguments{
\item{x}{Word embeddings from textEmbed (or textEmbedLayerAggregation). If several word embedding are
provided in a list they will be concatenated.}

\item{y}{Numeric variable to predict.}

\item{x_append}{(optional) Variables to be appended after the word embeddings (x);
if wanting to preappend them before the word embeddings use the option first = TRUE.
If not wanting to train with word embeddings, set x = NULL (default = NULL).}

\item{append_first}{(boolean) Option to add variables before or after all word embeddings (default = False).}

\item{cv_method}{(character) Cross-validation method to use within a pipeline of nested outer and inner loops
of folds (see nested_cv in rsample). Default is using "cv_folds" in the outside folds and "validation_split"
using rsample::validation_split in the inner loop to achieve a development and assessment set (note that
for "validation_split" the inside_folds should be a proportion, e.g., inside_folds = 3/4); whereas "cv_folds"
uses rsample::vfold_cv to achieve n-folds in both the outer and inner loops. Use "group_cv" to ensure that all cases
with the same ID remain in the same fold. (it uses rsample::group_vfold_cv uses  to ensure that all cases with the same
ID remain in the same fold. group_vfold_cv cannot handle stratification, so if that is requested,
it tries to approximate stratification while preserving group integrity.}

\item{id_variable}{(variable) If specifying cv_method = "group_cv", you need to submit an id variable here.}

\item{outside_folds}{(numeric) Number of folds for the outer folds (default = 10).}

\item{inside_folds}{(numeric) The proportion of data to be used for modeling/analysis; (default proportion = 3/4).
For more information see validation_split in rsample.}

\item{strata}{(string or tibble; default "y") Variable to stratify according;
if a string the variable needs to be in the training set - if you want to stratify
according to another variable you can include it as a tibble (please note you
can only add 1 variable to stratify according). Can set it to NULL.}

\item{outside_strata}{(boolean) Whether to stratify the outside folds.}

\item{outside_breaks}{(numeric) The number of bins wanted to stratify a numeric stratification variable in the
outer cross-validation loop (default = 4).}

\item{inside_strata}{Whether to stratify the outside folds.}

\item{inside_breaks}{The number of bins wanted to stratify a numeric stratification variable in the inner
cross-validation loop (default = 4).}

\item{model}{Type of model. Default is "regression"; see also "logistic" and "multinomial" for classification.}

\item{eval_measure}{(character) Type of evaluative measure to select models from. Default = "rmse" for regression, "weighted_correlations for
weighted regression and "bal_accuracy" for logistic. For regression use "rsq" or "rmse"; and for classification use "accuracy",
 "bal_accuracy", "sens", "spec", "precision", "kappa", "f_measure", or "roc_auc", (for more details see
 the yardstick package). See also the method_cor setting below.}

\item{save_aggregated_word_embedding}{(boolean) If TRUE, the aggregated word embeddings (mean, min, and max) are saved
for comparison with other language input when the model is applied to other types of data.}

\item{language_distribution}{(Character column) If you provide the raw language data used for making the embeddings,
the language distribution (i.e., a word and frequency table) will be saved to the model object. This enables
calculating similarity scores when the model is being applied to new language domains.
Note that this saves the individual words, which, if you are analyzing sensitive data, can be problematic from a
privacy perspective; to some extent this can be mitigated by increasing the number of words needed to be saved.}

\item{language_distribution_min_words}{(numeric) Minimum number a words need to occur in the data set to be saved to the
language distribution.}

\item{preprocess_step_center}{(boolean) Normalizes dimensions to have a mean of zero; default is set to TRUE.
For more info see (step_center in recipes).}

\item{preprocess_step_scale}{(boolean) Normalize dimensions to have a standard deviation of one;
default is set to TRUE. For more info see (step_scale in recipes).}

\item{preprocess_PCA}{Pre-processing threshold for PCA (to skip this step set it to NA).
Can select amount of variance to retain (e.g., .90 or as a grid c(0.80, 0.90)); or
number of components to select (e.g., 10). Default is "min_halving", which is a function
that selects the number of PCA components based on number  of participants and feature (word embedding dimensions)
in the data. The formula is:
preprocess_PCA = round(max(min(number_features/2), number_participants/2), min(50, number_features))).}

\item{penalty}{(numeric) Hyper parameter that is tuned (default = 10^seq(-16,16)).}

\item{parameter_selection_method}{If several results are tied for different parameters (i.e., penalty or mixture),
then select the "lowest_penalty", "highest_penalty", "median_penalty", or "lowest_mixture", the "highest_mixture" or
the "median_mixture" order of all the tied penalties/mixtures.}

\item{mixture}{A number between 0 and 1 (inclusive) that reflects the proportion of L1 regularization
(i.e. lasso) in the model (for more information see the linear_reg-function in the parsnip-package).
When mixture = 1, it is a pure lasso model while mixture = 0 indicates that ridge regression is being
used (specific engines only).}

\item{first_n_predictors}{By default this setting is turned off (i.e., NA). To use this method,
set it to the highest number of predictors you want to test. Then the X first dimensions are used in training,
using a sequence from Kjell et al., 2019 paper in Psychological Methods. Adding 1,
then multiplying by 1.3 and finally rounding to the nearest integer (e.g., 1, 3, 5, 8).
This option is currently only possible for one embedding at the time.}

\item{impute_missing}{Default FALSE (can be set to TRUE if something else than word_embeddings are trained).}

\item{method_cor}{Type of correlation used in final model estimation evaluation (default "pearson";
can set to "spearman" or "kendall").}

\item{model_description}{(character) Text to describe your model (optional; good when sharing the model with others).}

\item{multi_cores}{If TRUE it enables the use of multiple cores if the computer system allows for it
(i.e., only on unix, not windows). Hence it makes the analyses considerably faster to run. Default is
"multi_cores_sys_default", where it automatically uses TRUE for Mac and Linux and FALSE for Windows.}

\item{save_output}{(character) Option not to save all output; default = "all". see also "no_plot", "only_results",
and "only_results_predictions". Note that "no_plot" is good when wanting to save a logistic or multnomial regression,
since the lot makes the saved object bloated when being saved.}

\item{simulate.p.value}{(Boolean or string) From fisher.test: a logical indicating whether to compute p-values by
Monte Carlo simulation, in larger than 2 * 2 tables. The test can be turned off if set to "turn_off".}

\item{seed}{(numeric) Set different seed (default = 2020).}

\item{weights}{Optional vector containing weights (default = NULL); for details see importance_weights hardhat. For now only working for
model = "regression".}

\item{...}{For example settings in yardstick::accuracy to set event_level (e.g., event_level = "second").}
}
\value{
A (one-sided) correlation test between predicted and observed values; tibble
of predicted values (t-value, degree of freedom (df), p-value,
 alternative-hypothesis, confidence interval, correlation coefficient), as well as information about
 the model (preprossing_recipe, final_model and model_description).
}
\description{
textTrainRegression() trains word embeddings to a numeric or a factor variable.
}
\details{
By default, NAs are treated as follows:
   1. rows with NAs in word embeddings are removed.
   2. rows with NAs in y are removed
   3. rows with NAs in  x_append are removed; if impute_missing is set to
      TRUE, missing values will be imputed using k-nearest neighbours.
   When rows are omitted, the user will get a warning.
   The CV predictions will include NAs with the same length as the input.
}
\examples{
# Examines how well the embeddings from the column "harmonytext" can
# predict the numerical values in the column "hilstotal".

\dontrun{
trained_model <- textTrainRegression(
  x = word_embeddings_4$texts$harmonytext,
  y = Language_based_assessment_data_8$hilstotal,
  multi_cores = FALSE # This is FALSE due to CRAN testing and Windows machines.
)

# Examine results (t-value, degree of freedom (df), p-value, alternative-hypothesis,
# confidence interval, correlation coefficient).

trained_model$results
}
}
\seealso{
See \code{\link{textEmbedLayerAggregation}}, \code{\link{textTrainLists}} and
\code{\link{textTrainRandomForest}}.
}
