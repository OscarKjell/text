library(tibble)
library(dplyr)
library(text)
library(testthat)

test_that("textProjection MEAN and PCA produces a tibble with character variable and numeric variable.", {
  skip_on_cran()

  bertopic <- text::get_bertopic_model(data=data,
                                 data_var=data_cols[j],
                                 embedding_model=embedding_models[i],
                                 umap_model=umap_model,
                                 hdbscan_model=hdbscan_model,
                                 vectorizer_model=vectorizer_model,
                                 representation_model=representation_model,
                                 num_top_words=num_top_words,
                                 n_gram_range=n_gram_window,
                                 min_df=min_df,
                                 bm25_weighting=bm25_weighting,
                                 reduce_frequent_words=reduce_frequent_words,
                                 stop_words=stop_words,
                                 save_dir="./results")
  testthat::expect_is(bertopic$model$summary, is.data.frame())
  testthat::expect_is(bertopic$preds, is.data.frame())
  testthat::expect_is(bertopic$train_data, is.data.frame())

})
