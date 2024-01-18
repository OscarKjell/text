#' BERTopic implementation with prediction extraction
#' @param data (tibble/data.frame) A tibble with a text-variable to be analysed, and optional numeric/categorical variables that you
#' might want to use for later analyses testing the significance of topics in relation to these variables.
#' @param variable_name (string)  Name of the text-variable in the data tibble that you want to perform topic modeling on.
#' @param embedding_model (string) Name of the embedding model to use such as "miniLM", "mpnet", "multi-mpnet", "distilroberta".
#' @param umap_model (string) The dimension reduction algorithm, currently only "default" is supported.
#' @param hdbscan_model (string) The clustering algorithm to use, currently only "default" is supported.
#' @param vectorizer_model (string) Name of the vectorizer model, currently only "default" is supported.
#' @param representation_model (string) Name of the representation model used for topics, including "keybert" or "mmr".
#' @param n_gram_range (vector) Two-dimensional vector indicating the ngram range used for the vectorizer model.
#' @param stopwords (string) Name of the stopword dictionary to use.
#' @param min_df (integer) The minimum document frequency of terms.
#' @param bm25_weighting (boolean) Determine whether bm25_weighting is used for ClassTfidfTransformer.
#' @param reduce_frequent_words (boolean) Determine whether frequent words are reduced by ClassTfidfTransformer.
#' @param num_top_words (integer) Determine the number of top words presented for each topic.
#' @param seed (integer) The random seed for initialization of the umap model.
#' @param save_dir (string) The directory for saving results.
#' @return A folder containing the model, data, folder with terms and values for each topic, and the document-topic matrix.
#' @export
textTopics <- function(data,
                       variable_name,
                       embedding_model = "distilroberta",
                       umap_model = "default",
                       hdbscan_model = "default",
                       vectorizer_model = "default",
                       representation_model = "mmr",
                       num_top_words = 10,
                       n_gram_range = c(1,3),
                       stopwords = "english",
                       min_df = 5,
                       bm25_weighting = FALSE,
                       reduce_frequent_words = TRUE,
                       seed = 1234,
                       save_dir = "./results"){

  # Run python file with HunggingFace interface to state-of-the-art transformers
  reticulate::source_python(system.file("python",
                                        "bert_topic.py",
                                        # envir = NULL,
                                        package = "text",
                                        mustWork = TRUE
  ))

  model <- create_bertopic_model(data = data,
                                 data_var = variable_name,
                                 embedding_model = embedding_model,  # provide a value for embedding_model
                                 umap_model = umap_model,       # provide a value for umap_model
                                 hdbscan_model = hdbscan_model,    # provide a value for hdbscan_model
                                 vectorizer_model = vectorizer_model,  # provide a value for vectorizer_model
                                 representation_model = representation_model,  # provide a value for representation_model
                                 top_n_words = num_top_words,  # provide a value for top_n_words
                                 n_gram_range = n_gram_range,
                                 min_df = min_df,
                                 bm25_weighting = bm25_weighting,
                                 reduce_frequent_words = reduce_frequent_words,
                                 stop_words = stopwords,  # provide a value for n_gram_range
                                 seed = seed,
                                 save_dir = save_dir  # provide a value for save_dir
  )

  model$summary <- data.frame(model[2])
  preds <- data.frame(read.csv(
    paste0(save_dir,"/seed_",seed,"/topic_distr.csv")))

  train_data <- data.frame(read.csv(
    paste0(save_dir,"/seed_",seed,"/data.csv")))

  return(list(
    model = model,
    preds = preds,
    train_data = train_data))

}