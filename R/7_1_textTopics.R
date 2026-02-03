#' BERTopic
#'
#' \code{textTopics()} trains a BERTopic model (via the \pkg{bertopic} Python package) on a
#' text variable in a \code{tibble}/\code{data.frame}. The function embeds documents, reduces
#' dimensionality (UMAP), clusters documents (HDBSCAN), and extracts topic representations
#' using c-TF-IDF with optional KeyBERT/MMR-based representation. (EXPERIMENTAL)
#'
#' @param data A \code{tibble}/\code{data.frame} containing a text variable to analyze and,
#'   optionally, additional numeric/categorical variables that can be used in later analyses
#'   (e.g., testing topic prevalence differences across groups).
#' @param variable_name A character string giving the name of the text variable in \code{data}
#'   to perform topic modeling on.
#'
#' @param embedding_model A character string specifying which embedding model to use.
#'   Common options include \code{"miniLM"}, \code{"mpnet"}, \code{"multi-mpnet"},
#'   and \code{"distilroberta"}. The choice affects topic quality, speed, and memory usage.
#'
#' @param representation_model A character string specifying the topic representation method.
#'   Must be one of \code{"mmr"} or \code{"keybert"}.
#'   \itemize{
#'     \item \code{"keybert"} uses embedding similarity to select representative words/phrases.
#'     \item \code{"mmr"} (Maximal Marginal Relevance) promotes diversity among selected terms.
#'   }
#'
#' @param umap_n_neighbors Integer. Number of neighbors used by UMAP to balance local versus
#'   global structure. Smaller values emphasize local clusters; larger values emphasize global structure.
#' @param umap_n_components Integer. Number of dimensions to reduce to with UMAP (the embedding
#'   space used for clustering).
#' @param umap_min_dist Numeric. Minimum distance between embedded points in UMAP. Smaller values
#'   typically yield tighter clusters.
#' @param umap_metric Character string specifying the distance metric used by UMAP, e.g.
#'   \code{"cosine"}.
#'
#' @param hdbscan_min_cluster_size Integer. The minimum cluster size for HDBSCAN. Larger values
#'   yield fewer, broader topics; smaller values yield more, finer-grained topics.
#' @param hdbscan_min_samples Integer or \code{NULL}. Controls how conservative clustering is.
#'   If \code{NULL}, HDBSCAN chooses a default.
#' @param hdbscan_metric Character string specifying the metric used by HDBSCAN, typically
#'   \code{"euclidean"} when clustering in reduced UMAP space.
#' @param hdbscan_cluster_selection_method Character string specifying cluster selection strategy.
#'   Either \code{"eom"} (excess of mass; often yields more stable clusters) or \code{"leaf"}
#'   (can yield more fine-grained clusters).
#' @param hdbscan_prediction_data Logical. If \code{TRUE}, stores additional information enabling
#'   approximate topic prediction for new documents (when supported by the underlying pipeline).
#'
#' @param num_top_words Integer. Number of top terms to return per topic.
#' @param n_gram_range Integer vector of length 2 giving the min and max n-gram length used by
#'   the vectorizer (e.g., \code{c(1L, 3L)}).
#' @param stopwords Character string naming the stopword dictionary to use (e.g. \code{"english"}).
#' @param min_df Integer. Minimum document frequency for terms included in the vectorizer.
#'
#' @param bm25_weighting Logical. If \code{TRUE}, uses BM25 weighting in the class-based TF-IDF
#'   transformer (can improve term weighting in some corpora).
#' @param reduce_frequent_words Logical. If \code{TRUE}, down-weights very frequent words using
#'   the class-based TF-IDF transformer.
#'
#' @param set_seed Integer. Random seed used to initialize UMAP (and other stochastic components)
#'   for reproducibility.
#' @param save_dir Character string specifying the directory where outputs should be saved.
#'   A folder will be created (or reused) to store the fitted model and derived outputs.
#'
#' @return A named list containing:
#' \describe{
#'   \item{train_data}{The training data used to fit the model (or loaded from disk if available).}
#' \item{preds}{A document-by-topic matrix of normalized topic mixtures (LDA-like).
#'   Rows typically sum to 1; rows of zeros can occur if no topic mass was assigned.}
#'   \item{doc_info}{Document-level outputs including hard topic labels (\code{-1} indicates outliers).}
#'   \item{topic_info}{Topic-level outputs including topic sizes and top terms.}
#'   \item{model}{The fitted BERTopic model object (Python-backed).}
#'   \item{model_type}{Model identifier (currently \code{"bert_topic"}).}
#'   \item{seed}{Random seed used.}
#'   \item{save_dir}{Directory where artifacts were saved.}
#' }
#' @details
#' Typical tuning levers:
#' \itemize{
#'   \item \strong{More topics / finer clusters}: decrease \code{hdbscan_min_cluster_size},
#'     decrease \code{umap_n_neighbors}, and/or increase \code{umap_n_components}.
#'   \item \strong{Fewer topics / broader clusters}: increase \code{hdbscan_min_cluster_size}
#'     and/or increase \code{umap_n_neighbors}.
#'   \item \strong{More phrase-like terms}: increase \code{n_gram_range} max (e.g., up to 3).
#'   \item \strong{Cleaner vocabulary}: increase \code{min_df}, and use \code{reduce_frequent_words = TRUE}.
#' }
#'
#' @examples
#' \dontrun{
#' res <- textTopics(
#'   data = Language_based_assessment_data_8,
#'   variable_name = "harmonytexts",
#'   embedding_model = "distilroberta",
#'   representation_model = "mmr",
#'   min_df = 3,
#'   save_dir = "bertopic_results"
#' )
#'
#' }
#'
#' @seealso \code{\link{textTopicsReduce}}, \code{\link{textTopicsTest}},
#'   \code{\link{textTopicsWordcloud}}
#'
#' @export
textTopics <- function(
    data,
    variable_name,

    embedding_model = "distilroberta",
    representation_model = c("mmr", "keybert"),

    # UMAP (controls manifold + separation)
    umap_n_neighbors = 15L,
    umap_n_components = 5L,
    umap_min_dist = 0.0,
    umap_metric = "cosine",

    # HDBSCAN (controls clustering aggressiveness)
    hdbscan_min_cluster_size = 5L,
    hdbscan_min_samples = NULL,
    hdbscan_metric = "euclidean",
    hdbscan_cluster_selection_method = "eom",
    hdbscan_prediction_data = TRUE,

    # Vectorizer / c-TF-IDF side (controls word features)
    num_top_words = 10L,
    n_gram_range = c(1L, 3L),
    stopwords = "english",
    min_df = 5L,

    bm25_weighting = FALSE,
    reduce_frequent_words = TRUE,

    set_seed = 8L,
    save_dir
    ) {

  representation_model <- match.arg(representation_model)
  embedding_model <- as.character(embedding_model)[1]

  # Run python file with HunggingFace interface to state-of-the-art transformers
  reticulate::source_python(system.file("python",
    "bert_topic.py",
    package = "text",
    mustWork = TRUE
  ))

  model <- create_bertopic_model(
    data = data,
    data_var = variable_name,
    embedding_model = embedding_model,
    representation_model = representation_model,

    umap_n_neighbors = as.integer(umap_n_neighbors),
    umap_n_components = as.integer(umap_n_components),
    umap_min_dist = as.numeric(umap_min_dist),
    umap_metric = umap_metric,

    hdbscan_min_cluster_size = as.integer(hdbscan_min_cluster_size),
    hdbscan_min_samples = if (is.null(hdbscan_min_samples)) NULL else as.integer(hdbscan_min_samples),
    hdbscan_metric = hdbscan_metric,
    hdbscan_cluster_selection_method = hdbscan_cluster_selection_method,
    hdbscan_prediction_data = isTRUE(hdbscan_prediction_data),

    top_n_words = as.integer(num_top_words),
    n_gram_range = as.integer(n_gram_range),
    min_df = as.integer(min_df),
    stop_words = stopwords,

    bm25_weighting = isTRUE(bm25_weighting),
    reduce_frequent_words = isTRUE(reduce_frequent_words),

    seed = as.integer(set_seed),
    save_dir = save_dir
  )

  names(model) <- c("model", "summary")

  seed_dir <- file.path(save_dir, paste0("seed_", set_seed))

  # 1) Soft topic mixture (BEST default: LDA-like)
  preds_probs <- as.matrix(read.csv(file.path(seed_dir, "topic_probs.csv"), check.names = FALSE))
  storage.mode(preds_probs) <- "double"

  rs <- rowSums(preds_probs)
  preds_mat <- preds_probs
  ok <- rs > 0
  preds_mat[ok, ] <- preds_mat[ok, , drop = FALSE] / rs[ok]
  preds_mat[!ok, ] <- 0

  preds <- tibble::as_tibble(preds_mat)

#  preds_distr_path <- file.path(seed_dir, "topic_distr_approx.csv")
#  preds_distr <- if (file.exists(preds_distr_path)) {
#    m <- as.matrix(read.csv(preds_distr_path, check.names = FALSE))
#    storage.mode(m) <- "double"
#    tibble::as_tibble(m)
#  } else NULL

  # Hard labels + metadata
  doc_info_path <- file.path(seed_dir, "doc_info.csv")
  doc_info <- if (file.exists(doc_info_path)) read.csv(doc_info_path, check.names = FALSE) %>% tibble::as_tibble() else NULL

  topic_info_path <- file.path(seed_dir, "topic_info.csv")
  topic_info <- if (file.exists(topic_info_path)) read.csv(topic_info_path, check.names = FALSE)  %>% tibble::as_tibble()  else NULL

  # Training data (optional)
  train_data_path <- file.path(seed_dir, "data.csv")
  train_data <- if (file.exists(train_data_path)) {
    read.csv(train_data_path, check.names = FALSE) %>% tibble::as_tibble()
  } else {
    tibble::as_tibble(data)
  }

  return(list(
    train_data = train_data,
    preds = preds,                 # <-- DEFAULT: LDA-like topic mixture (normalized)
  #  preds_probs = preds_probs,     # <-- raw soft probs from BERTopic
  #  preds_distr = preds_distr,     # <-- optional approximate/token distribution
    doc_info = doc_info,           # <-- hard labels (-1, 0, 1, ...)
    topic_info = topic_info,
    model = model,
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



