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

  bert_model <- text::textTopics(
    data = data,
    variable_name = "text",
    embedding_model = "distilroberta",
    min_df = 2,
    set_seed = 42,
    save_dir = save_dir_temp)

  testthat::expect_equal(bert_model$model_type, "bert_topic")
  testthat::expect_true(is.numeric(bert_model$seed) || is.integer(bert_model$seed))
  testthat::expect_true(is.character(bert_model$save_dir) && nchar(bert_model$save_dir) > 0)

  testthat::expect_s3_class(bert_model$preds, "tbl_df")

  # Must have at least 1 doc and >=1 topic
  testthat::expect_gt(nrow(bert_model$preds), 0)
  testthat::expect_gt(ncol(bert_model$preds), 0)

  # Column names look like topics: t_1, t_2, ...
  cn <- colnames(bert_model$preds)
  testthat::expect_true(all(grepl("^t_\\d+$", cn)))

  m <- as.matrix(bert_model$preds)

  # Finite values only
  testthat::expect_true(all(is.finite(m)))

  # Should not contain negative probabilities (allow tiny numerical jitter)
  testthat::expect_gte(min(m), -1e-12)

  # Row sums: either 1 or 0 (for edge cases where rs == 0)
  rs <- rowSums(m)
  testthat::expect_true(all(abs(rs - 1) < 1e-6 | abs(rs - 0) < 1e-12))

  # Each row max should be <= 1 (+ tiny jitter)
  testthat::expect_lte(max(m), 1 + 1e-12)

  testthat::expect_s3_class(bert_model$train_data, "tbl_df")
  testthat::expect_equal(nrow(bert_model$preds), nrow(bert_model$train_data))


  testthat::expect_s3_class(bert_model$doc_info, "tbl_df")
  testthat::expect_equal(nrow(bert_model$doc_info), nrow(bert_model$preds))

  # Check there is some topic label column (common names)
  possible <- c("Topic", "topic", "topic_id", "topicid")
  has_topic_col <- any(possible %in% names(bert_model$doc_info))
  testthat::expect_true(has_topic_col)

  # If Topic column exists: must be integer-ish and allow -1 outliers
  if ("Topic" %in% names(bert_model$doc_info)) {
    x <- bert_model$doc_info$Topic
    testthat::expect_true(is.numeric(x) || is.integer(x))
    testthat::expect_true(any(x == -1) || all(x >= 0)) # allow either presence/absence of outliers
  }


  # All columns numeric
  testthat::expect_true(all(vapply(bert_model$preds, is.numeric, logical(1))))

  testthat::expect_equal(names(bert_model),
                         c("train_data", "preds", "doc_info", "topic_info",
                           "model", "model_type", "seed","save_dir"))


#  testthat::expect_equal(bert_model$preds$t_1[2],
#                         8.452748e-310,
#                         tolerance = 0.0001)
#
#  testthat::expect_equal(bert_model$preds$t_2[1],
#                         .9424498,
#                         tolerance = 0.0001)
#

  testthat::expect_s3_class(bert_model$topic_info, "tbl_df")
  testthat::expect_gt(nrow(bert_model$topic_info), 0)

  # Often BERTopic has a Topic column. If so, must contain integers.

  testthat::expect_true(is.numeric(bert_model$topic_info$Topic) ||
                            is.integer(bert_model$topic_info$Topic))


#  Testing  how individual topics are associated with "score"
  test2 <- text::textTopicsTest(
    model = bert_model,
    x_variable = "score",
    test_method = "linear_regression"
    )

  testthat::expect_equal(test2$test$x.z_score.estimate_beta[1],
                         0.1372805,
                         tolerance = 0.0001)


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
  plots1$overview_plot

  plots <- text::textTopicsWordcloud(
    model = bert_model,
    #  save_dir = save_dir_temp,
    figure_format = "png",
    seed = 42,
  )

  }
})




