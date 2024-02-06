#library(tibble)
#library(dplyr)
#library(text)
#library(testthat)
#
#
#test_that("Bertopic", {
#  skip_on_cran()
#
#  data1 <- Language_based_assessment_data_8[c("satisfactiontexts", "swlstotal")]
#  colnames(data1) <- c("text", "score")
#
#  data2 <- Language_based_assessment_data_8[c("harmonytexts", "hilstotal")]
#  colnames(data2) <- c("text", "score")
#
#  data3 <- Language_based_assessment_data_3_100[1:2]
#  colnames(data3) <- c("text", "score")
#
#  data <- dplyr::bind_rows(data1, data2, data3)
#
#  bert_model <- textTopics(
#      data = data,
#      variable_name = "text",
#      embedding_model = "distilroberta",
#      representation_model= "mmr",
#      min_df=2,
#      stopwords="english",
#      bm25_weighting=FALSE,
#      reduce_frequent_words=TRUE,
#      n_gram_range=c(1,3),
#      seed = 8,
#      save_dir="./results"
#      )
#
#  testthat::expect_equal(bert_model$preds$t_1[2],
#                           .1115696,
#                           tolerance = 0.0001)
#
#  # Testing if we can predict "score" from all dimentsions
#  test <- text::textTopicTest(
#      model = bert_model$model,
#      preds = bert_model$preds,
#      data = bert_model$train_data,
#      group_var = "score",
#      control_vars = c("score"),
#      test_method = "ridge_regression",
#      seed = 8,
#      save_dir = "./results" # could this be found in a comment/and describd in description of function that it needs this
#      )
#
#  testthat::expect_equal(test[3]$p.value,
#                           .7673133,
#                           tolerance = 0.0001)
#
#  test2 <- text::textTopicTest(
#    model = bert_model$model,
#    preds = bert_model$preds,
#    data = bert_model$train_data,
#    group_var = "score",
#    control_vars = c("score"),
#    test_method = "linear_regression",
#    seed = 8,
#    save_dir = "./results"
#  )
#
#  testthat::expect_equal(test2$score.estimate[1],
#                         .1056764,
#                         tolerance = 0.0001)
#
#  plots <- textTopicsWordcloud(
#    model = bert_model,
#    model_type = "bert_topic", # Good if this is in a comment() (i..e., now needed)
#    test = test2,
#    test_type = "linear_regression", # Good if this is in a comment() (i..e., now needed)
#    cor_var = "score", # can this be automatically found
#    seed = 8,
#    plot_topics_idx = NULL,
#    p_threshold = .2,
#    scale_size = FALSE,
#    color_negative_cor = scale_color_gradient(low = "darkgreen", high = "green"),
#    color_positive_cor = scale_color_gradient(low = "darkred", high = "red"),
#    save_dir = "./results"
#    )
#
#
#  unlink("./results", recursive = TRUE)
#})
#
#
#

