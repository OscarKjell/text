#' BERTopics
#'
#' textTopics creates and trains a BERTopic model (based on bertopic python packaged) on a
#' text-variable in a tibble/data.frame. (EXPERIMENTAL)
#' @param data (tibble/data.frame) A tibble with a text-variable to be analysed, and optional
#' numeric/categorical variables that you might want to use for later analyses testing the
#' significance of topics in relation to these variables.
#' @param variable_name (string)  Name of the text-variable in the data tibble that you want
#' to perform topic modeling on.
#' @param embedding_model (string) Name of the embedding model to use such as "miniLM", "mpnet",
#' "multi-mpnet", "distilroberta".
#' @param umap_model (string) The dimension reduction algorithm, currently only "default"
#' is supported.
#' @param hdbscan_model (string) The clustering algorithm to use, currently only "default"
#'  is supported.
#' @param vectorizer_model (string) Name of the vectorizer model, currently only "default"
#' is supported.
#' @param representation_model (string) Name of the representation model used for topics,
#' including "keybert" or "mmr".
#' @param n_gram_range (vector) Two-dimensional vector indicating the ngram range used for
#' the vectorizer model.
#' @param stopwords (string) Name of the stopword dictionary to use.
#' @param min_df (integer) The minimum document frequency of terms.
#' @param bm25_weighting (boolean) Determine whether bm25_weighting is used for ClassTfidfTransformer.
#' @param reduce_frequent_words (boolean) Determine whether frequent words are reduced by ClassTfidfTransformer.
#' @param num_top_words (integer) Determine the number of top words presented for each topic.
#' @param set_seed (integer) The random seed for initialization of the umap model.
#' @param save_dir (string) The directory for saving results.
#' @return A folder containing the model, data, folder with terms and values for each topic,
#' and the document-topic matrix. Moreover the model itself is returned formatted as a data.frame
#' together with metdata.
# @examples
# #
# \dontrun{
#
# }
#' See \code{\link{textTopicsReduce}} \code{\link{textTopicsTest}} and \code{\link{textTopicsWordcloud}}.
#' @export
textTopics <- function(
    data,
    variable_name,
    embedding_model = "distilroberta",
    umap_model = "default",
    hdbscan_model = "default",
    vectorizer_model = "default",
    representation_model = "mmr",
    num_top_words = 10,
    n_gram_range = c(1, 3),
    stopwords = "english",
    min_df = 5,
    bm25_weighting = FALSE,
    reduce_frequent_words = TRUE,
    set_seed = 8,
    save_dir
    ) {
  # Run python file with HunggingFace interface to state-of-the-art transformers
  reticulate::source_python(system.file("python",
    "bert_topic.py",
    package = "text",
    mustWork = TRUE
  ))

  model <- create_bertopic_model(
    data = data,
    data_var = variable_name,
    embedding_model = embedding_model, # provide a value for embedding_model
    umap_model = umap_model, # provide a value for umap_model
    hdbscan_model = hdbscan_model, # provide a value for hdbscan_model
    vectorizer_model = vectorizer_model, # provide a value for vectorizer_model
    representation_model = representation_model, # provide a value for representation_model
    top_n_words = num_top_words, # provide a value for top_n_words
    n_gram_range = n_gram_range,
    min_df = min_df,
    bm25_weighting = bm25_weighting,
    reduce_frequent_words = reduce_frequent_words,
    stop_words = stopwords, # provide a value for n_gram_range
    seed = set_seed,
    save_dir = save_dir # provide a value for save_dir
  )

  model$summary <- data.frame(model[2])
  preds <- data.frame(read.csv(
    paste0(save_dir, "/seed_", set_seed, "/topic_distr.csv")
  ))

  train_data <- data.frame(read.csv(
    paste0(save_dir, "/seed_", set_seed, "/data.csv")
  ))

  # save a dataframe with model_type, seed and save_dir inside the comment

  return(list(
    model = model,
    preds = preds,
    train_data = train_data,
    model_type = "bert_topic",
    seed = set_seed,
    save_dir = save_dir
  ))
}

#' textTopicsReduce (EXPERIMENTAL)
#' @param data (tibble/data.frame) A tibble with a text-variable to be analysed, and optional
#' numeric/categorical variables that you might want to use for later analyses testing the
#' significance of topics in relation to these variables.
#' @param data_var (string)  Name of the text-variable in the data tibble that you want
#' to perform topic modeling on.
#' @param embedding_model (string) Name of the embedding model to use such as "miniLM", "mpnet",
#' "multi-mpnet", "distilroberta".
#' @param n_topics (string) The dimension reduction algorithm, currently only "default"
#' is supported.
#' @param load_path (string) The clustering algorithm to use, currently only "default"
#'  is supported.
#' @param save_dir (string) Name of the vectorizer model, currently only "default"
#' is supported.
#' @param save_dir (string) The directory for saving results.
#' @return A folder containing the model, data, folder with terms and values for each topic,
#' and the document-topic matrix. Moreover the model itself is returned formatted as a data.frame
#' together with metdata.
# @examples
# #
# \dontrun{
#
# }
#' @seealso See \code{\link{textTopics}} \code{\link{textTopicsTest}} and \code{\link{textTopicsWordcloud}}.
#' @export
textTopicsReduce <- function(
    data,
    data_var,
    n_topics = 10,
    load_path = "./results", # From textTopics saved output
    save_dir,
    embedding_model = "default"
    ){

  # Run python file with HunggingFace interface to state-of-the-art transformers
  reticulate::source_python(system.file("python",
                                        "bert_topic.py",
                                        package = "text",
                                        mustWork = TRUE
  ))

  reduce_topics(
    data = data,
    data_var = data_var,
    n_topics = n_topics,
    load_path = load_path,
    save_path = save_dir,
    embedding_model = embedding_model
  )

  message("Complete")
}


#' textTopicsTest (EXPERIMENTAL) to get the hierarchical topic tree
#' @param topic_model (list) The output from textTopics.
#' @param data (tibble/data.frame) A tibble with the data
#' @param data_var (string) The name of the text variable that the topic model was trained on
#' @return prints a hierarchical topic tree on the console
#' @export
textTopicsTree <- function(
    topic_model,
    data,
    data_var
    ){

  topic_model <- topic_model$topic_model
  get_topic_tree(topic_model, data, data_var)

}



