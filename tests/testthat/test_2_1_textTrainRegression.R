library(testthat)
library(text)
library(tibble)
library(dplyr)

context("textTrainRegression")

test_that("textTrainRegression, textTrainList and textPredcit", {
  skip_on_cran()

  # Regression
  model_reg <- textTrainRegression(
    x = word_embeddings_4$texts$harmonytext,
    y = Language_based_assessment_data_8$hilstotal,
    outside_folds = 2,
    inside_folds = 2 / 3,
    outside_strata = FALSE,
    inside_strata = FALSE,
    penalty = c(1),
    mixture = c(0),
    preprocess_PCA = NA,
    multi_cores = "multi_cores_sys_default",
    save_output_size = "small_model" #"small_model" # NULL #"all" # "small_model"
  )

  breakItDown = function(mod) {
     sapply(mod, FUN=function(x){length(serialize(x, NULL))}, simplify=T)
   }
  #breakItDown(model_reg1)
  breakItDown(model_reg)

  testthat::expect_that(model_reg, is_a("list"))
  testthat::expect_is(model_reg$predictions$predictions[1], "numeric")
  testthat::expect_equal(model_reg$predictions$predictions[1], 28.5811, tolerance = 0.001)

  model_reg_preds <- text::textPredict(
    model_info = model_reg,
    word_embeddings = word_embeddings_4$texts["harmonytexts"]
  )

#  testthat::expect_equal(model_reg_preds$harmonytexts__ypred[1], 16.05805, tolerance = 0.001)
#  testthat::expect_equal(model_reg_preds$harmonytexts__ypred[2], 24.3845, tolerance = 0.001)

  testthat::expect_equal(model_reg_preds[[1]][1], 16.05805, tolerance = 0.001)
  testthat::expect_equal(model_reg_preds[[1]][2], 24.3845, tolerance = 0.001)

  # testing with one component; and thus a standard logistic.
  model_logistic_PCA1 <- text::textTrainRegression(
     x = word_embeddings_4$texts[1],
     y = as.factor(Language_based_assessment_data_8$gender),
     outside_folds = 2,
     # inside_folds = 2,
     outside_strata = FALSE,
     inside_strata = FALSE,
     model = "logistic",
     eval_measure = "precision",
     penalty = c(1),
     mixture = c(0),
     preprocess_PCA = 1,
     multi_cores = "multi_cores_sys_default",
     # force_train_method = "automatic",
     save_output = "all"
   )
   testthat::expect_that(model_logistic_PCA1, is_a("list"))
   testthat::expect_is(model_logistic_PCA1$results_metrics$.estimate[[1]], "numeric")
   testthat::expect_equal(model_logistic_PCA1$results_metrics$.estimate[[1]], 0.525)

   model_logistic_PCA1_preds <- text::textPredict(
     model_info = model_logistic_PCA1,
     word_embeddings = word_embeddings_4$texts[1],
     dim_names = TRUE
   )
   testthat::expect_equal(as.character(model_logistic_PCA1_preds[[1]][[1]]), "male")
   testthat::expect_equal(as.character(model_logistic_PCA1_preds[[1]][[3]]), "female")


   # test Multinomial logistic regression with four outcomes. Note that the data has
   # few observations so there will be many warnings.
   model_multinomial4 <- text::textTrainRegression(
     x = word_embeddings_4$texts["harmonywords"],
     y = as.factor(ntile(Language_based_assessment_data_8$hilstotal, 4)),
     cv_method = "validation_split",
     outside_folds = 10,
     inside_folds = 3 / 4,
     model = "multinomial",
     eval_measure = "bal_accuracy",
     penalty = c(1),
     mixture = c(0),
     preprocess_PCA = "min_halving",
     multi_cores = "multi_cores_sys_default"
   )

   testthat::expect_that(model_multinomial4, testthat::is_a("list"))
   testthat::expect_is(model_multinomial4$results_metrics$.estimate[[1]], "numeric")
   testthat::expect_equal(model_multinomial4$results_metrics$.estimate[[1]], 0.275)

   model_logistic2 <- text::textTrainRegression(
     x = word_embeddings_4$texts["harmonywords"],
     y = as.factor(Language_based_assessment_data_8$gender),
     cv_method = "validation_split",
     outside_folds = 10,
     inside_folds = 3 / 4,
     model = "logistic",
     eval_measure = "bal_accuracy",
     penalty = c(1),
     mixture = c(0),
     preprocess_PCA = "min_halving",
     multi_cores = "multi_cores_sys_default"
   )

   testthat::expect_that(model_logistic2, testthat::is_a("list"))
   testthat::expect_is(model_logistic2$results_metrics$.estimate[[1]], "numeric")
   testthat::expect_equal(model_logistic2$results_metrics$.estimate[[1]], 0.6)

   model_multinomial4_preds <- text::textPredict(
     model_info = model_multinomial4,
     word_embeddings = word_embeddings_4$texts["harmonywords"],
   )

   testthat::expect_equal(as.character(model_multinomial4_preds[[1]][1]), "2")
   testthat::expect_equal(as.character(model_multinomial4_preds[[1]][2]), "3")
   testthat::expect_equal(as.character(model_multinomial4_preds[[1]][5]), "4")

   # test logistic model help(textPredict)
   model_logistic2_preds <- text::textPredict(
     model_info = model_logistic2,
     word_embeddings = word_embeddings_4$texts["harmonywords"],
     type = "class_prob",
     show_texts = TRUE
   )

   testthat::expect_equal(as.character(model_logistic2_preds[[1]][3]), "female")
   testthat::expect_equal(model_logistic2_preds[[2]][5], 0.5404381, tolerance = 0.0001)
   testthat::expect_equal(model_logistic2_preds[[3]][5], 0.4595619, tolerance = 0.0001)

   model_lists <- text::textTrainLists(
     x = word_embeddings_4$texts[1:2],
     y = Language_based_assessment_data_8[5:6],
     outside_folds = 2,
     inside_folds = 3 / 4,
     penalty = c(1),
     mixture = c(0),
   )

   model_multinomial4_preds <- text::textPredictAll(
     models = model_lists,
     word_embeddings = word_embeddings_4$texts[1:2],
     y = Language_based_assessment_data_8[5:6]
   )

   testthat::expect_equal(model_multinomial4_preds[[1]][1], 23.90620, tolerance = 0.0001)
   testthat::expect_equal(model_multinomial4_preds[[2]][1], 20.57408, tolerance = 0.0001)
   testthat::expect_equal(model_multinomial4_preds[[3]][1], 20.16958, tolerance = 0.0001)
   testthat::expect_equal(model_multinomial4_preds[[4]][1], 15.67582, tolerance = 0.0001)

})

test_that("textTrain Regression without saving models", {
  skip_on_cran()

  # Do not save the model
  trained_min_halving <- text::textTrainRegression(
    x = word_embeddings_4$texts["harmonywords"],
    y = Language_based_assessment_data_8[6],
    cv_method = "cv_folds",
    outside_folds = 2,
    inside_folds = 2,
    strata = NULL,
    outside_strata = FALSE,
    inside_strata = FALSE,
    model = "regression",
    eval_measure = "rmse",
    penalty = c(1),
    mixture = c(0),
    preprocess_PCA = 1,
    multi_cores = FALSE,
    # force_train_method = "automatic",
    save_output = "only_results"
  )

  testthat::expect_that(trained_min_halving, is_a("list"))
  testthat::expect_is(trained_min_halving$results$statistic[[1]], "numeric")
  testthat::expect_equal(trained_min_halving$results$statistic[[1]], 0.2979104, tolerance = 0.00001)


  trained_logistic <- text::textTrainRegression(
    x = word_embeddings_4$texts["harmonywords"],
    y = as.factor(Language_based_assessment_data_8$gender),
    cv_method = "validation_split",
    outside_folds = 2,
    inside_folds = 3 / 4,
    outside_strata = FALSE,
    inside_strata = FALSE,
    model = "logistic",
    eval_measure = "bal_accuracy",
    penalty = c(1),
    mixture = c(0),
    preprocess_PCA = "min_halving",
    multi_cores = "multi_cores_sys_default",
    save_output = "only_results"
  )
  testthat::expect_that(trained_logistic, is_a("list"))
  testthat::expect_is(trained_logistic$results_metrics$.estimate[[1]], "numeric")
  testthat::expect_equal(trained_logistic$results_metrics$.estimate[[1]], 0.475)

  trained_logistic2 <- text::textTrainRegression(
    x = word_embeddings_4$texts[1],
    y = as.factor(Language_based_assessment_data_8$gender),
    cv_method = "cv_folds",
    outside_folds = 2,
    inside_folds = 2,
    outside_strata = FALSE,
    inside_strata = FALSE,
    model = "logistic",
    eval_measure = "accuracy",
    penalty = c(1),
    mixture = c(0),
    preprocess_PCA = 1,
    multi_cores = "multi_cores_sys_default",
    save_output = "only_results_predictions"
  )
  testthat::expect_that(trained_logistic2, is_a("list"))
  testthat::expect_is(trained_logistic2$results_metrics$.estimate[[1]], "numeric")
  testthat::expect_equal(trained_logistic2$results_metrics$.estimate[[1]], 0.525)

  trained_1 <- text::textTrain(
    x = word_embeddings_4$texts$harmonytext,
    y = Language_based_assessment_data_8$hilstotal,
    outside_folds = 2,
    inside_folds = 2 / 3,
    outside_strata = FALSE,
    inside_strata = FALSE,
    penalty = c(1),
    mixture = c(0),
    preprocess_PCA = NA,
    multi_cores = "multi_cores_sys_default",
    force_train_method = "regression",
    save_output = "only_results_predictions"
  )

  testthat::expect_that(trained_1, is_a("list"))
  testthat::expect_is(trained_1$prediction$predictions[1], "numeric")
  testthat::expect_equal(trained_1$prediction$predictions[1], 28.5811, tolerance = 0.001)


  # test multinomial logistic regression with 3 outcomes
  trained_multinomial <- text::textTrainRegression(
    x = word_embeddings_4$texts["harmonywords"],
    y = as.factor(ntile(Language_based_assessment_data_8$hilstotal, 3)),
    cv_method = "validation_split",
    outside_folds = 10,
    inside_folds = 3 / 4,
    model = "multinomial",
    eval_measure = "bal_accuracy",
    penalty = c(1),
    mixture = c(0),
    preprocess_PCA = "min_halving",
    multi_cores = "multi_cores_sys_default",
    save_output = "only_results"
  )

  testthat::expect_that(trained_multinomial, testthat::is_a("list"))
  testthat::expect_is(trained_multinomial$results_metrics$.estimate[[1]], "numeric")
  testthat::expect_equal(trained_multinomial$results_metrics$.estimate[[1]], 0.675)


  factors1 <- as.factor(Language_based_assessment_data_8$gender)
  factors2 <- as.factor(Language_based_assessment_data_8$gender)
  rating1 <- Language_based_assessment_data_8$hilstotal

  ratings_data_factors <- tibble::tibble(factors1, factors2, rating1)

  results_list_logistic <- text::textTrain(
    x = word_embeddings_4$texts$harmonywords,
    y = ratings_data_factors,
    preprocess_PCA = c(0.90),
    outside_folds = 2,
    inside_folds = 2 / 3,
    outside_strata = FALSE,
    inside_strata = FALSE,
    penalty = c(2),
    mixture = c(0),
    force_train_method = "regression",
    save_output = "only_results_predictions",
    multi_cores = FALSE
  )

  testthat::expect_that(results_list_logistic, testthat::is_a("list"))
  testthat::expect_is(results_list_logistic$results[[2]][[1]], "integer")
  testthat::expect_equal(results_list_logistic$results[[3]][[1]], 0.008647702, tolerance = 0.0001)

})



test_that("textTrainLists Regression produces a list of results with prediction being numeric", {
  skip_on_cran()

  # One word embedding and two rating scales help(textTrainRegression)
  model_list <- text::textTrainLists(
    x = word_embeddings_4$texts$harmonywords,
    y = Language_based_assessment_data_8[5:6],
    outside_folds = 2,
    inside_folds = 2 / 3,
    outside_strata = FALSE,
    inside_strata = FALSE,
    preprocess_PCA = c(0.90),
    # outside_strata_y = NULL,
    # inside_strata_y = NULL,
    penalty = c(2),
    mixture = c(0),
    force_train_method = "regression",
    save_output = "only_results",
    method_cor = "kendall",
    multi_cores = FALSE
  )

  testthat::expect_that(model_list, testthat::is_a("list"))
  testthat::expect_is(model_list$results$tau_correlation[1], "character")
  testthat::expect_equal(model_list$results$tau_correlation[1], "0.21297093352316")

  ratings_data1 <- Language_based_assessment_data_8[5]
  ratings_data2 <- Language_based_assessment_data_8[6]
  factors1 <- tibble::as_tibble_col(as.factor(Language_based_assessment_data_8$gender))
  ratings_data <- cbind(ratings_data1, ratings_data2, factors1)

  model_list_2rs <- text::textTrainLists(
    x = word_embeddings_4$texts[1],
    y = ratings_data,
    preprocess_PCA = c(0.90),
    outside_folds = 2,
    inside_folds = 2 / 3,
    outside_strata = FALSE,
    inside_strata = FALSE,
    penalty = c(2),
    mixture = c(0),
    force_train_method = "automatic",
    save_output = "only_results_predictions",
    multi_cores = FALSE
  )

  testthat::expect_that(model_list_2rs, testthat::is_a("list"))
  testthat::expect_is(model_list_2rs$results$correlation[1], "character")
  testthat::expect_equal(as.numeric(model_list_2rs$results$correlation[1]), .3744373834122,
                         tolerence = 0.0000000001) # "0.374437383412246" "0.374436371225743"


  factors1 <- as.factor(Language_based_assessment_data_8$gender)
  factors2 <- as.factor(Language_based_assessment_data_8$gender)
  rating1 <- Language_based_assessment_data_8$hilstotal

  ratings_data_factors <- tibble::tibble(factors1, factors2, rating1)

  # Logistic
  results_list_logistic1 <- text::textTrainLists(
    x = word_embeddings_4$texts[1],
    y = ratings_data_factors,
    preprocess_PCA = c(0.90),
    outside_folds = 2,
    inside_folds = 2 / 3,
    outside_strata = FALSE,
    inside_strata = FALSE,
    penalty = c(2),
    mixture = c(0),
    force_train_method = "automatic",
    # model = "logistic",
    eval_measure = "default",
    save_output = "only_results_predictions",
    multi_cores = FALSE
  )

  testthat::expect_that(results_list_logistic1, testthat::is_a("list"))
  testthat::expect_equal(results_list_logistic1$results[[2]][1], "0.538720538720539")

 })



test_that("textTrainRegression adding word_embedding together", {
  skip_on_cran()

  multi_we_PCA_09 <- text::textTrainRegression(
    x = word_embeddings_4$texts[1:2],
    y = Language_based_assessment_data_8$hilstotal,
    outside_folds = 2,
    inside_folds = 2 / 3,
    outside_strata = FALSE,
    inside_strata = FALSE,
    preprocess_PCA = c(0.9),
    penalty = 1,
    multi_cores = FALSE
  )

  testthat::expect_that(multi_we_PCA_09, testthat::is_a("list"))
  testthat::expect_is(multi_we_PCA_09$results[[1]][[1]], "numeric")
  testthat::expect_equal(multi_we_PCA_09$results[[1]][[1]], 1.159983, tolerance = 0.0001)


  # Prediction based on multiple we
  predictions_multi <- text::textPredict(
    model_info = multi_we_PCA_09,
    word_embeddings = word_embeddings_4$texts[1:2],
    dim_names = TRUE)
  testthat::expect_is(predictions_multi[[1]][[1]], "numeric")
  testthat::expect_equal(predictions_multi[[1]][[1]], 19.70077, tolerance = 0.0001)


  multi_we_PCA_3 <- text::textTrainRegression(
    x = word_embeddings_4$texts[1:2],
    y = Language_based_assessment_data_8$hilstotal,
    outside_folds = 2,
    inside_folds = 2 / 3,
    outside_strata = FALSE,
    inside_strata = FALSE,
    preprocess_PCA = 3,
    penalty = 1,
    multi_cores = FALSE
  )

  testthat::expect_that(multi_we_PCA_3, testthat::is_a("list"))
  testthat::expect_is(multi_we_PCA_3$results[[1]][[1]], "numeric")
  testthat::expect_equal(multi_we_PCA_3$results[[1]][[1]], 1.456567, tolerance = 0.0001)


  multi_we_PCA_NA <- text::textTrainRegression(
    x = word_embeddings_4$texts[1:2],
    y = Language_based_assessment_data_8$hilstotal,
    outside_folds = 2,
    inside_folds = 2 / 3,
    outside_strata = FALSE,
    inside_strata = FALSE,
    preprocess_PCA = NA,
    penalty = 1,
    multi_cores = FALSE
  )

  testthat::expect_that(multi_we_PCA_NA, testthat::is_a("list"))
  testthat::expect_is(multi_we_PCA_NA$results[[1]][[1]], "numeric")
  testthat::expect_equal(multi_we_PCA_NA$results[[1]][[1]], 1.58414, tolerance = 0.001)
})


test_that("training with only x_append (without word embeddings)", {
  skip_on_cran()

  # help("textTrainRegression")
  # x_append and word embeddings
  emb_append <- text::textTrainRegression(
    x = word_embeddings_4$texts$harmonywords,
    x_append = Language_based_assessment_data_8[6:7],
    y = Language_based_assessment_data_8[5],
    outside_folds = 2,
    append_first = TRUE,
    multi_cores = FALSE
  )
  testthat::expect_that(emb_append, testthat::is_a("list"))
  testthat::expect_equal(emb_append$results[[1]][[1]], 8.184946)

  model_emb_append <- textPredict(
    model_info = emb_append,
    word_embeddings = word_embeddings_4$texts["harmonywords"],
    x_append = Language_based_assessment_data_8[6:7],
    dim_names = TRUE,
    append_first = TRUE
  )

  testthat::expect_equal(model_emb_append$harmonywords_swlstotal_age_hilstotalpred[[1]], 13.92264, tolerance = 0.00001)
  testthat::expect_equal(model_emb_append$harmonywords_swlstotal_age_hilstotalpred[[2]], 22.78044, tolerance = 0.00001)


  # No x_append
  model_no_append <- text::textTrainRegression(
    x = NULL,
    x_append = Language_based_assessment_data_8[6:7],
    y = Language_based_assessment_data_8[6],
    outside_folds = 2
  )

  testthat::expect_that(model_no_append, testthat::is_a("list"))
  testthat::expect_equal(model_no_append$results[[1]][[1]], 339.6473, tolerance = 0.1)


  model_no_append_preds <- textPredict(
    model_info = model_no_append,
    word_embeddings = NULL,
    x_append = Language_based_assessment_data_8[6:7]
    )

  testthat::expect_equal(model_no_append_preds$`_swlstotal_age_swlstotalpred`[[1]], 6.426066, tolerance = 0.00001)
  testthat::expect_equal(model_no_append_preds$`_swlstotal_age_swlstotalpred`[[2]], 15.50044, tolerance = 0.00001)

})
