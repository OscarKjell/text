#note: I think layer 0 is the input embedding. 
import warnings
warnings.simplefilter(action='ignore', category=FutureWarning)

import torch
from transformers import AutoConfig, AutoModel, AutoTokenizer
import numpy as np

import nltk
try:
    nltk.data.find('tokenizers/punkt/PY3/english.pickle')
except:
    nltk.download("punkt")

from nltk.tokenize import sent_tokenize

#TODO:#dictionary of pretrained weights to models
def hgTransformerGetEmbedding(text_strings,
                              model = 'bert-base-uncased',
                              layers = 'all',  
                              return_tokens = True,
                              max_token_to_sentence = 4):
    """
    Simple Python method for embedding text with pretained Hugging Face models

    Parameters
    ----------
    text_strings : list
        list of strings, each is embedded separately
    model : str
        shortcut name for Hugging Face pretained model
        Full list https://huggingface.co/transformers/pretrained_models.html
    layers : str or list
        'all' or an integer list of layers to keep
    return_tokens : boolean
        return tokenized version of text_strings
    max_token_to_sentence : int
        maximum number of tokens in a string to handle before switching to embedding text
        sentence by sentence


    Returns
    -------
    all_embs : list
        embeddings for each item in text_strings
    all_toks : list, optional
        tokenized version of text_strings

    """

    config = AutoConfig.from_pretrained(model, output_hidden_states=True)
    tokenizer = AutoTokenizer.from_pretrained(model)
    transformer_model = AutoModel.from_pretrained(model, config=config)

    max_tokens = tokenizer.max_len_sentences_pair

    ##Check and Adjust Input Typs
    if not isinstance(text_strings, list):
        text_strings = [text_strings]
    if layers != 'all':
        if not isinstance(layers, list):
            layers = [layers]
        layers = [int(i) for i in layers]

    all_embs = []
    all_toks = []

    for text_string in text_strings:
        
        # if length of text_string is > max_token_to_sentence*4
        # embedd each sentence separately
        if len(text_string) > max_token_to_sentence*4:
            
            sentence_batch = [s for s in sent_tokenize(text_string)]
            batch = tokenizer(sentence_batch, padding=True, truncation=True, add_special_tokens=True)
            input_ids = batch["input_ids"]
            attention_mask = batch['attention_mask']
            
            if return_tokens:
                tokens = []
                for ids in input_ids:
                    tokens.extend([token for token in tokenizer.convert_ids_to_tokens(ids) if token != '[PAD]'])
                all_toks.append(tokens)
            
            with torch.no_grad():
                hidden_states = transformer_model(torch.tensor(input_ids),attention_mask=torch.tensor(attention_mask))[-1]
                if layers != 'all': 
                    hidden_states = [hidden_states[l] for l in layers]
                hidden_states = [h.tolist() for h in hidden_states]
            
            sent_embedding = []
            for l in range(len(hidden_states)): # iterate over layers
                layer_embedding = []
                for m in range(len(hidden_states[l])): # iterate over sentences
                    layer_embedding.extend([tok for ii, tok in enumerate(hidden_states[l][m]) if attention_mask[m][ii]>0])
                sent_embedding.append(layer_embedding)
            all_embs.append([[l] for l in sent_embedding])

        else:
            input_ids = tokenizer.encode(text_string, add_special_tokens=True)
            if return_tokens:
                tokens = tokenizer.convert_ids_to_tokens(input_ids)
            with torch.no_grad():
                hidden_states = transformer_model(torch.tensor([input_ids]))[-1]
                if layers != 'all': 
                    hidden_states = [hidden_states[l] for l in layers]
                hidden_states = [h.tolist() for h in hidden_states]
                all_embs.append(hidden_states)
                if return_tokens:
                    all_toks.append(tokens)

    if return_tokens:
        return all_embs, all_toks
    else:
        return all_embs

    
#EXAMPLE TEST CODE:
#if __name__   == '__main__':
#    embeddings, tokens = hgTransformerGetEmbedding("Here is one sentence.", layers=[0,10])
#    print(np.array(embeddings).shape)
#    print(tokens)
#
#    embeddings, tokens = hgTransformerGetEmbedding("Here is more sentences. But why is not . and , and ? indicated with SEP?", layers=[0,10])
#    print(np.array(embeddings).shape)
#    print(tokens)

