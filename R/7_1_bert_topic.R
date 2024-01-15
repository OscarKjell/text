#' BERTopic implementation with prediction extraction
#' @param data (data.frame)
#' @param data_var (string)  data variable to perform topic modeling on
#' @param embedding_model (string) embedding model to use for embedding data, choose from miniLM, mpnet, multi-mpnet, distilroberta
#' @param umap_model (string) dimension reduction algorithm, currently only "default" supported
#' @param hdbscan (string) clustering algorihtm, currently only "default" supported
#' @param vectorizer_model (string) vectorizer model, currently only "default" supported
#' @param representation_model (string) representation models used for topics, "keybert" or "mmr"
#' @param n_gram_range (list) ngram range used for vectorizer model
#' @param stopwords (string)
#' @param min_df (int) minimum document frequency of terms
#' @param bm25_weighting (bool) determine whether bm25_weighting is used for ClassTfidfTransformer
#' @param reduce_frequent_words (bool) determine whether frequent words are reduced by ClassTfidfTransformer
#' @param num_top_words (int) determine the number of top words for each topic
#' @param seed (int) set random seed for intialization of umap model
#' @param save_dir (string) set directory for saving results, defaults to "./results"
#' @return A folder containing the model, data, another folder with terms and values for each topic, document-topic matrix
#' @export
get_bertopic_model <- function(data,
                               data_var,
                               embedding_model="default",
                               umap_model="default",
                               hdbscan_model="default",
                               vectorizer_model="default",
                               representation_model="default",
                               num_top_words=10,
                               n_gram_range=c(1,3),
                               stop_words="english",
                               min_df=min_df,
                               bm25_weighting=bm25_weighting,
                               reduce_frequent_words=reduce_frequent_words,
                               seed=1234,
                               save_dir="./results"){

  # Run python file with HunggingFace interface to state-of-the-art transformers
  reticulate::source_python(system.file("python",
                                        "bert_topic.py",
                                        # envir = NULL,
                                        package = "text",
                                        mustWork = TRUE
  ))

  model <- create_bertopic_model(data=data,
                                 data_var=data_var,
                                 embedding_model=embedding_model,  # provide a value for embedding_model
                                 umap_model=umap_model,       # provide a value for umap_model
                                 hdbscan_model=hdbscan_model,    # provide a value for hdbscan_model
                                 vectorizer_model=vectorizer_model,  # provide a value for vectorizer_model
                                 representation_model=representation_model,  # provide a value for representation_model
                                 top_n_words=num_top_words,  # provide a value for top_n_words
                                 n_gram_range=n_gram_range,
                                 min_df=min_df,
                                 bm25_weighting=bm25_weighting,
                                 reduce_frequent_words=reduce_frequent_words,
                                 stop_words = stop_words,  # provide a value for n_gram_range
                                 seed=seed,
                                 save_dir=save_dir  # provide a value for save_dir
  )
  model$summary <- data.frame(model[2])
  preds <- data.frame(read.csv(paste0(save_dir,"/seed_",seed,"/topic_distr.csv")))
  train_data <- data.frame(read.csv(paste0(save_dir,"/seed_",seed,"/data.csv")))
  return(list(model=model, preds=preds, train_data=train_data))
}
