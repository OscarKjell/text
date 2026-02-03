from bertopic import BERTopic
import json
import pandas as pd
import csv
import pandas as pd
from bertopic.representation import KeyBERTInspired
from sentence_transformers import SentenceTransformer
from flair.embeddings import TransformerDocumentEmbeddings
from umap import UMAP
from hdbscan import HDBSCAN
from sklearn.feature_extraction.text import CountVectorizer
from bertopic.vectorizers import ClassTfidfTransformer
from bertopic.representation import KeyBERTInspired, MaximalMarginalRelevance, PartOfSpeech
import os

# turning off parallelism for transformers
os.environ["TOKENIZERS_PARALLELISM"] = "false"
#from sentence_transformers import SentenceTransformer


embedding_models = {"miniLM":SentenceTransformer("all-MiniLM-L6-v2"),
                    #"roberta": TransformerDocumentEmbeddings('roberta-base'),
                    "mpnet": SentenceTransformer("all-mpnet-base-v2"),
                    "multi-mpnet": SentenceTransformer("multi-qa-mpnet-base-dot-v1"),
                    "distilroberta": SentenceTransformer("all-distilroberta-v1")}
def get_umap_models(umap_model, seed):
    umap_models = {"default": UMAP(n_neighbors=15, n_components=5, min_dist=0.0, metric='cosine', random_state=seed)}
    return umap_models[umap_model]
hdbscan_models = {"default": HDBSCAN(min_cluster_size=5, metric='euclidean', cluster_selection_method='eom', prediction_data=True)}

#def get_vectorizer_models(vectorizer_model, n_gram_range, stop_words, min_df):
#    vectorizer_models = {"default": CountVectorizer(stop_words=stop_words, min_df=min_df, ngram_range=(1,3))}
#    return vectorizer_models[vectorizer_model]

def get_vectorizer_models(vectorizer_model, n_gram_range, stop_words, min_df):
    # n_gram_range may come from R as floats (e.g., [1.0, 3.0])
    if isinstance(n_gram_range, (list, tuple)):
        if len(n_gram_range) != 2:
            raise ValueError(f"n_gram_range must be length 2, got: {n_gram_range}")
        n_gram_range = (int(n_gram_range[0]), int(n_gram_range[1]))
    else:
        # fallback: if passed as a scalar or weird type
        n_gram_range = (1, int(n_gram_range))

    vectorizer_models = {
        "default": CountVectorizer(
            stop_words=stop_words,
            min_df=int(min_df),
            ngram_range=n_gram_range
        )
    }
    return vectorizer_models[vectorizer_model]


representation_models = {"keybert": KeyBERTInspired(),
                         "mmr": MaximalMarginalRelevance(diversity=0.3)}

def create_bertopic_model(
    data,
    data_var,
    embedding_model="distilroberta",
    representation_model="mmr",
    top_n_words=10,
    n_gram_range=(1, 3),
    min_df=5,
    stop_words="english",
    bm25_weighting=False,
    reduce_frequent_words=True,
    seed=1234,
    save_dir="./results",

    # UMAP params
    umap_n_neighbors=15,
    umap_n_components=5,
    umap_min_dist=0.0,
    umap_metric="cosine",

    # HDBSCAN params
    hdbscan_min_cluster_size=5,
    hdbscan_min_samples=None,
    hdbscan_metric="euclidean",
    hdbscan_cluster_selection_method="eom",
    hdbscan_prediction_data=True
):

    os.environ["TOKENIZERS_PARALLELISM"] = "false"

    # ---- cast types defensively (R -> python sometimes floats)
    top_n_words = int(top_n_words)
    min_df = int(min_df)
    seed = int(seed)

    umap_n_neighbors = int(umap_n_neighbors)
    umap_n_components = int(umap_n_components)
    umap_min_dist = float(umap_min_dist)

    hdbscan_min_cluster_size = int(hdbscan_min_cluster_size)
    if hdbscan_min_samples is not None:
        hdbscan_min_samples = int(hdbscan_min_samples)

    # n_gram_range can arrive as list/tuple of floats
    if isinstance(n_gram_range, (list, tuple)):
        if len(n_gram_range) != 2:
            raise ValueError(f"n_gram_range must be length 2, got: {n_gram_range}")
        n_gram_range = (int(n_gram_range[0]), int(n_gram_range[1]))
    else:
        n_gram_range = (1, int(n_gram_range))

    # ---- Create output dir early
    save_path = f"{save_dir}/seed_{seed}"
    if not os.path.exists(save_path):
        os.makedirs(save_path)

    # ---- Clean + prep data
    data = data.copy()
    data[data_var] = data[data_var].apply(lambda x: "".join([c for c in str(x) if not c.isdigit()]))
    data = data.dropna().reset_index(drop=True)

    # Save cleaned data (optional but useful for reproducibility/debugging)
    data.to_csv(f"{save_path}/data.csv", index=False)

    # ---- Embeddings
    embedding_model_used = embedding_models[embedding_model]
    embeddings = embedding_model_used.encode(data[data_var], show_progress_bar=False)

    # ---- Build pipeline objects from explicit params
    umap_model_obj = UMAP(
        n_neighbors=umap_n_neighbors,
        n_components=umap_n_components,
        min_dist=umap_min_dist,
        metric=umap_metric,
        random_state=seed
    )

    hdbscan_kwargs = dict(
        min_cluster_size=hdbscan_min_cluster_size,
        metric=hdbscan_metric,
        cluster_selection_method=hdbscan_cluster_selection_method,
        prediction_data=bool(hdbscan_prediction_data),
    )
    if hdbscan_min_samples is not None:
        hdbscan_kwargs["min_samples"] = hdbscan_min_samples

    hdbscan_model_obj = HDBSCAN(**hdbscan_kwargs)

    vectorizer_model_obj = CountVectorizer(
        stop_words=stop_words,
        min_df=min_df,
        ngram_range=n_gram_range
    )

    repr_model_obj = representation_models[representation_model]

    # ---- Build BERTopic model
    topic_model = BERTopic(
        embedding_model=embedding_model_used,
        umap_model=umap_model_obj,
        hdbscan_model=hdbscan_model_obj,
        vectorizer_model=vectorizer_model_obj,
        ctfidf_model=ClassTfidfTransformer(
            bm25_weighting=bool(bm25_weighting),
            reduce_frequent_words=bool(reduce_frequent_words),
        ),
        representation_model=repr_model_obj,
        top_n_words=top_n_words,
        calculate_probabilities=True,
        verbose=True
    )

    # ---- Fit
    topics, probs = topic_model.fit_transform(data[data_var], embeddings=embeddings)

    # quick diagnostics (optional)
    print("topics[:10] =", topics[:10])
    print("probs type:", type(probs))
    
    # --- Save doc_info (hard assignments etc.) ---
    doc_info = topic_model.get_document_info(data[data_var].astype(str).tolist())
    doc_info.to_csv(f"{save_path}/doc_info.csv", index=False)

    # --- Save topic_info (topic metadata) ---
    topic_info = topic_model.get_topic_info()
    topic_info.to_csv(f"{save_path}/topic_info.csv", index=False)
    
    # --- Save topic probabilities (best default for R: dense doc-topic mixture) ---
    if probs is None:
        _, probs = topic_model.transform(data[data_var], embeddings=embeddings)

    probs_cols = [f"t_{i}" for i in range(1, probs.shape[1] + 1)]
    with open(f"{save_path}/topic_probs.csv", "w", newline="") as csv_file:
        csv_writer = csv.writer(csv_file)
        csv_writer.writerow(probs_cols)
        csv_writer.writerows(probs)

    # --- Optional: approximate_distribution (often sparse; keep for diagnostics) ---
    topic_distr, topic_token_distr = topic_model.approximate_distribution(
        data[data_var], calculate_tokens=True
    )
    distr_cols = [f"t_{i}" for i in range(1, topic_distr.shape[1] + 1)]
    with open(f"{save_path}/topic_distr_approx.csv", "w", newline="") as csv_file:
        csv_writer = csv.writer(csv_file)
        csv_writer.writerow(distr_cols)
        csv_writer.writerows(topic_distr)

    # ---- Save model
    topic_model.save(
        f"{save_path}/my_model",
        serialization="safetensors",
        save_ctfidf=True,
        save_embedding_model=embedding_model_used
    )

    # ---- Save top words per topic
    top_terms = topic_model.topic_representations_
    top_terms_filtered = {label: terms for label, terms in top_terms.items() if label != -1}
    model_summary = pd.DataFrame(
        [(f"t_{i}", ", ".join([term for term, _ in terms]))
         for i, (_, terms) in enumerate(top_terms_filtered.items(), start=1)],
        columns=["topic", "top_terms"]
    )

    csv_dir = f"{save_path}/df_list_term_phi/"
    if not os.path.exists(csv_dir):
        os.makedirs(csv_dir)

    for topic, topic_data in top_terms.items():
        if int(topic) == -1:
            continue
    
        topic_out = int(topic) + 1
        csv_file_path = f"{csv_dir}{topic_out}_top_words.csv"
    
        with open(csv_file_path, "w", newline="") as csv_file:
            writer = csv.writer(csv_file)
            writer.writerow(["Word", "phi"])
    
            for word_row in topic_data:
                joined_words = "_".join(word_row[0].split())
                writer.writerow([joined_words, word_row[1]])

    return [topic_model, model_summary]

#reduce topics to n
def reduce_topics(data, 
                  data_var, 
                  n_topics, 
                  load_path, 
                  save_path, 
                  embedding_model="default"):

    data = data
    data[data_var] = data[data_var].apply(lambda x: ''.join([c for c in str(x) if not c.isdigit()]))
    # dropping the rows having NaN values
    data = data.dropna()
 
    # To reset the indices
    data = data.reset_index(drop=True)
    embedding_model_used = embedding_models[embedding_model]

    # Load the model
    topic_model = BERTopic.load(load_path, embedding_model_used)

    # Reduce the number of topics
    topic_model.reduce_topics(data[data_var], nr_topics=n_topics)
    topics, probs = topic_model.transform(data[data_var])
    topic_distr, topic_token_distr = topic_model.approximate_distribution(data[data_var], calculate_tokens=True)
    columns = ["t_" + str(i) for i in range(1,topic_distr.shape[1]+1)]
    
    # Create the directory if it doesn't exist
    save_path = f"{save_path}/reduced_{n_topics}"
    if not os.path.exists(save_path):
        os.makedirs(save_path)

    data.to_csv(f"{save_path}/data.csv", index=False)

    with open(f"{save_path}/topic_distr.csv", 'w', newline='') as csv_file:
        # Create a CSV writer object
        csv_writer = csv.writer(csv_file)
        csv_writer.writerow(columns)
        # Write the array to the CSV file
        csv_writer.writerows(topic_distr)

    topic_model.save(f"{save_path}/model", serialization="safetensors", save_ctfidf=True, save_embedding_model=embedding_model_used)
    
    top_terms = topic_model.topic_representations_
    top_terms_filtered = {label: terms for label, terms in top_terms.items() if label != -1}
    model_summary = pd.DataFrame([(f't_{i}', ', '.join([term for term, _ in terms])) for i, (_, terms) in enumerate(top_terms_filtered.items(), start=1)], columns=['topic', 'top_terms'])

def get_topic_tree(topic_model, data, data_var):
    topic_model = topic_model[0]
    hierarchical_topics = topic_model.hierarchical_topics(data[data_var])
    print(topic_model.get_topic_tree(hierarchical_topics))
