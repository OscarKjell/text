library(tibble)
library(dplyr)
library(text)
library(testthat)
library(ggplot2)
#install.packages("topics")
#
# .rs.restartR()
#help(textrpp_initialize)
#textrpp_install()
#text::textrpp_initialize()
#text::textrpp_initialize(
#  save_profile = TRUE,
#  condaenv = "textrpp_condaenv",
#  refresh_settings = TRUE)
#textrpp_initialize(
#  condaenv = "berttopic2",
#  refresh_settings = TRUE
#)
#textEmbed("hello")

test_that("Bertopic", {
  skip_on_cran()

  save_dir_temp <- tempdir()
  #Langauge_based_assessment_data_8 <- load(Language_based_assessment_data_8.rda")

  # Load and prepare data
  data1 <- Language_based_assessment_data_8[c("satisfactiontexts", "swlstotal")]
  colnames(data1) <- c("text", "score")

  data2 <- Language_based_assessment_data_8[c("harmonytexts", "hilstotal")]
  colnames(data2) <- c("text", "score")

  data3 <- Language_based_assessment_data_3_100[1:2]
  colnames(data3) <- c("text", "score")

  data <- dplyr::bind_rows(data1, data2, data3)

  if (Sys.info()["sysname"] == "Darwin" | Sys.info()["sysname"] == "Windows") {

  # Create BERTopic model trained on data["text"] help(textTopics)


    embedding_model = "distilroberta"
    umap_model = "default"
    hdbscan_model = "default"
    vectorizer_model = "default"
    representation_model = "mmr"
    num_top_words = 10
    n_gram_range = c(1, 3)
    stopwords = "english"
    min_df = 2
    bm25_weighting = FALSE
    reduce_frequent_words = TRUE
    set_seed = 8
    save_dir = save_dir_temp

  bert_model <- text::textTopics(
    data = data,
    variable_name = "text",
    embedding_model = "distilroberta",
    min_df = 2,
    set_seed = 42,
    save_dir = save_dir_temp)

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

#  Testing  how individual topics are associated with "score"
  test2 <- text::textTopicsTest(
    model = bert_model,
   # data = data,
    x_variable = "score",
    test_method = "linear_regression"
    )

  testthat::expect_equal(test2$test$x.z_score.estimate_beta[1],
                         .1056764,
                         tolerance = 0.0001)


  plots <- text::textTopicsWordcloud(
    model = bert_model,
  #  save_dir = save_dir_temp,
    figure_format = "png",
    seed = 42,
  )

  plots1 <- text::textTopicsWordcloud(
    model = bert_model,
    test = test2,
    p_alpha = 1,
    figure_format = "png",
   seed = 42
  )
  plots1$square1
  plots1$square2
  plots1$square3
  plots1$legend
  plots1$distribution

  }
})




