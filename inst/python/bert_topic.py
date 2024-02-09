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

def get_vectorizer_models(vectorizer_model, n_gram_range, stop_words, min_df):
    vectorizer_models = {"default": CountVectorizer(stop_words=stop_words, min_df=min_df, ngram_range=(1,3))}
    return vectorizer_models[vectorizer_model]

representation_models = {"keybert": KeyBERTInspired(),
                         "mmr": MaximalMarginalRelevance(diversity=0.3)}

def create_bertopic_model(data,
                       data_var,
                       embedding_model="default",
                       umap_model="default",
                       hdbscan_model="default",
                       vectorizer_model="default",
                       representation_model="default",
                       top_n_words=10,
                       n_gram_range=(1,3),
                       min_df=10,
                       stop_words="english",
                       bm25_weighting=False, # bool
                       reduce_frequent_words=True, # bool
                       seed=1234,
                       save_dir="./results"):

    # turning off parallelism for transformers
    os.environ["TOKENIZERS_PARALLELISM"] = "false"
    #data = pd.read_csv('../data/depression_anxiety_cleaned.csv')
    data = data
    print(data)
    print(type(top_n_words))
    top_n_words = int(top_n_words)
    min_df = int(min_df)
    seed = int(seed)
    data[data_var] = data[data_var].apply(lambda x: ''.join([c for c in str(x) if not c.isdigit()]))
    # dropping the rows having NaN values
    data = data.dropna()
 
    # To reset the indices
    data = data.reset_index(drop=True)
    # Specify the file path where you want to save the CSV file

    # Save the DataFrame to a CSV file
 
    print(embedding_model)
    embedding_model_used = embedding_models[embedding_model]
    embeddings = embedding_model_used.encode(data[data_var], show_progress_bar=False)

    topic_model = BERTopic(     
        # Pipeline models
        embedding_model=embedding_models[embedding_model],
        umap_model=get_umap_models(umap_model=umap_model, seed=seed),
        hdbscan_model=hdbscan_models[umap_model],
        vectorizer_model=get_vectorizer_models(umap_model, n_gram_range, stop_words, min_df),
        ctfidf_model = ClassTfidfTransformer(bm25_weighting=bm25_weighting, reduce_frequent_words=reduce_frequent_words),
        representation_model=representation_models[representation_model],

        # Hyperparameters
        top_n_words=top_n_words,
        verbose=True
    )

    # Train model
    topics, probs = topic_model.fit_transform(data[data_var], embeddings=embeddings)
    topic_distr, topic_token_distr = topic_model.approximate_distribution(data[data_var], calculate_tokens=True)
    columns = ["t_" + str(i) for i in range(1,topic_distr.shape[1]+1)]
    
    # Create the directory if it doesn't exist
    save_path = f"{save_dir}/seed_{seed}"
    if not os.path.exists(save_path):
        os.makedirs(save_path)

    data.to_csv(f"{save_path}/data.csv", index=False)

    with open(f"{save_path}/topic_distr.csv", 'w', newline='') as csv_file:
        # Create a CSV writer object
        csv_writer = csv.writer(csv_file)
        csv_writer.writerow(columns)
        # Write the array to the CSV file
        csv_writer.writerows(topic_distr)

    topic_model.save(f"{save_path}/my_model", serialization="safetensors", save_ctfidf=True, save_embedding_model=embedding_model_used)
    
    top_terms = topic_model.topic_representations_
    top_terms_filtered = {label: terms for label, terms in top_terms.items() if label != -1}
    model_summary = pd.DataFrame([(f't_{i}', ', '.join([term for term, _ in terms])) for i, (_, terms) in enumerate(top_terms_filtered.items(), start=1)], columns=['topic', 'top_terms'])
    
    # Specify the directory to save CSV files
    csv_dir = f'{save_path}/df_list_term_phi/'

    if not os.path.exists(csv_dir):
        os.makedirs(csv_dir)

    # Iterate over topics and create CSV files
    for topic, topic_data in top_terms.items():
        # Create a CSV file for each topic
        topic = int(topic)+1
        #print(topic)
        csv_file_path = f'{csv_dir}{topic}_top_words.csv'

        # Write the topic data to the CSV file
        with open(csv_file_path, 'w', newline='') as csv_file:
            writer = csv.writer(csv_file)
            writer.writerow(['Word', 'phi'])

            # Write each row individually
            writer.writerows(topic_data)
    
    return [topic_model, model_summary]

