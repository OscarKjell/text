library(tibble)
library(dplyr)
library(text)
library(testthat)

test_that("textTopic.", {
  skip_on_cran()

#
# data1 <- Language_based_assessment_data_8[c("satisfactiontexts", "swlstotal")]
# colnames(data1) <- c("text", "score")

# data2 <- Language_based_assessment_data_8[c("harmonytexts", "hilstotal")]
# colnames(data2) <- c("text", "score")

# data3 <- Language_based_assessment_data_3_100[1:2]
# colnames(data3) <- c("text", "score")

# data <- dplyr::bind_rows(data1, data2, data3)
#
#  test <- textTopics(data = data,
#                    variable_name = "text",
#                    embedding_model = "distilroberta",
#                    representation_model= "mmr",
#                    min_df=2,
#                    stopwords="english",
#                    bm25_weighting=FALSE,
#                    reduce_frequent_words=TRUE,
#                    n_gram_range=c(1,3),
#                    save_dir="./results")
#
#  testthat::expect_equal(test$preds$t_1[2], .1115696, tolerance = 0.0001)
#
#  unlink("./results", recursive = TRUE)

})
