library(tibble)
library(dplyr)
library(text)
library(testthat)
library(ggplot2)

#textrpp_initialize()

test_that("Bertopic", {
  skip_on_cran()

  #Langauge_based_assessment_data_8 <- load(Language_based_assessment_data_8.rda")

  # Load and prepare data
  data1 <- Language_based_assessment_data_8[c("satisfactiontexts", "swlstotal")]
  colnames(data1) <- c("text", "score")

  data2 <- Language_based_assessment_data_8[c("harmonytexts", "hilstotal")]
  colnames(data2) <- c("text", "score")

  data3 <- Language_based_assessment_data_3_100[1:2]
  colnames(data3) <- c("text", "score")

  data <- dplyr::bind_rows(data1, data2, data3)

  # Create BERTopic model trained on data["text"]
  bert_model <- textTopics(data = data,
                           variable_name = "text",
                           embedding_model = "distilroberta",
                           min_df = 2,
                           set_seed = 8,
                           save_dir="./results")

  testthat::expect_equal(bert_model$preds$t_1[2],
                         .1115696,
                         tolerance = 0.0001)


#  textTopicsReduce(
#    data = data,
#    data_var = "text",
#    n_topics = 10L,
#    load_path = "./results/seed_8/my_model/", # From textTopics saved output
#    save_dir = "./results_reduced",
#    embedding_model = "distilroberta"
#  )
#


  # Testing if we can predict "score" from from topic-document distribution
  test <- text::textTopicsTest(model = bert_model,
                              pred_var = "score",
                              test_method = "ridge_regression")

  testthat::expect_equal(test$test[3]$p.value,
                         .7673133,
                         tolerance = 0.0001)

  # Testing which how individual topics are associated with "score"
  test2 <- text::textTopicsTest(model = bert_model,
                               pred_var = "score",
                               test_method = "linear_regression")

  testthat::expect_equal(test2$test$score.estimate[1],
                         .1056764,
                         tolerance = 0.0001)

  # Plot wordclouds for each significant topic
  plots <- textTopicsWordcloud(
    model = bert_model,
    test = test2,
    )


  unlink("./results", recursive = TRUE)
})




