library(tibble)
library(dplyr)
library(text)
library(testthat)

test_that("Bertopic", {
  skip_on_cran()

  data1 <- Language_based_assessment_data_8[c("satisfactiontexts", "swlstotal")]
  colnames(data1) <- c("text", "score")

  data2 <- Language_based_assessment_data_8[c("harmonytexts", "hilstotal")]
  colnames(data2) <- c("text", "score")

  data <- bind_rows(data1, data2)


  bertopic <- text::get_bertopic_model(data=data,
                                 data_var=data["text"],
                                 embedding_model="distilroberta",
                                 umap_model="default",
                                 hdbscan_model="default",
                                 vectorizer_model="default",
                                 representation_model="default",
                                 num_top_words=10,
                                 n_gram_range=c(1,3),
                                 min_df=10,
                                 bm25_weighting=FALSE,
                                 reduce_frequent_words=TRUE,
                                 stop_words="english",
                                 save_dir="./results")
  testthat::expect_is(bertopic$model$summary, is.data.frame())
  testthat::expect_is(bertopic$preds, is.data.frame())
  testthat::expect_is(bertopic$train_data, is.data.frame())

})


test_that("Test ridge regression with bertopic", {
  skip_on_cran()


  test <- textTopicTest(model=bertopic$model,
                         preds=bertopic$preds,
                         data=bertopic$train_data,
                         group_var = "score",
                         control_vars = c("score"),
                         test_method = "ridge_regression",
                         seed=8,
                         save_dir="./results")

})

test_that("Test linear regression with bertopic", {
  skip_on_cran()


  test <- textTopicTest(model=bertopic$model,
                         preds=bertopic$preds,
                         data=bertopic$train_data,
                         group_var = "score",
                         control_vars = c("score"),
                         test_method = "linear_regression",
                         seed=8,
                         save_dir="./results")

})

test_that("Test ridge regression with bertopic", {
  skip_on_cran()


  test <- textTopicTest(model=bertopic$model,
                         preds=bertopic$preds,
                         data=bertopic$train_data,
                         group_var = "score",
                         control_vars = c("score"),
                         test_method = "ridge_regression",
                         seed=8,
                         save_dir="./results")

})

test_that("Test ridge regression with bertopic", {
  skip_on_cran()


  textTopicsWordcloud(model = bertopic$model,
                      model_type = "bert_topic",
                      test = test,
                      test_type = "linear_regression",
                      cor_var = "score",
                      plot_topics_idx = NULL,
                      p_threshold = 0.05,
                      scale_size=FALSE,
                      color_negative_cor = scale_color_gradient(low = "darkgreen", high = "green"),
                      color_positive_cor = scale_color_gradient(low = "darkred", high = "red"),
                      save_dir="./results",
                      seed=8)

})



