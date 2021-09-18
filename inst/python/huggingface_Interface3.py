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

def hgTransformerGetEmbedding(text_strings,
                              model = 'bert-base-uncased',
                              layers = 'all',  
                              return_tokens = True,
                              max_token_to_sentence = 4,
                              device = 'cpu'):
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
    device : str
        name of device: 'cpu', 'gpu', or 'gpu:k' where k is a specific device number

    Returns
    -------
    all_embs : list
        embeddings for each item in text_strings
    all_toks : list, optional
        tokenized version of text_strings

    """
    device = device.lower()
    if not device.startswith('cpu') and not device.startswith('gpu'):
        print("device must be 'cpu', 'gpu', or of the form 'gpu:k'")
        print("\twhere k is an integer value for the device")
        print("Trying GPUs")
        device = 'gpu'
    
    config = AutoConfig.from_pretrained(model, output_hidden_states=True)
    tokenizer = AutoTokenizer.from_pretrained(model)
    transformer_model = AutoModel.from_pretrained(model, config=config)
    
    if device != 'cpu':
        if torch.cuda.is_available():
            attached = False
            if device == 'gpu':
                for device_num in range(0,torch.cuda.device_count()):
                    try:
                        transformer_model.to(device=device_num)
                        attached = True
                        break
                    except:
                        continue
            else: # assign to specific gpu device number
                try:
                    device_num = int(device[-1])
                    transformer_model.to(device=device_num)
                    attached = True
                except:
                    pass
            if not attached:
                print("Unable to use CUDA (GPU), using CPU")
        else:
            print("Unable to use CUDA (GPU), using CPU")
    
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
            input_ids = torch.tensor(batch["input_ids"])
            attention_mask = torch.tensor(batch['attention_mask'])
            
            if device != 'cpu':
                input_ids = input_ids.to(device=device_num)
                attention_mask = attention_mask.to(device=device_num)
            
            if return_tokens:
                tokens = []
                for ids in input_ids:
                    tokens.extend([token for token in tokenizer.convert_ids_to_tokens(ids) if token != '[PAD]'])
                all_toks.append(tokens)
           
            with torch.no_grad():
                hidden_states = transformer_model(input_ids,attention_mask=attention_mask)[-1]
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
            input_ids = torch.tensor([input_ids])
            
            if device != 'cpu':
                input_ids = input_ids.to(device=device_num)
            
            if return_tokens:
                tokens = tokenizer.convert_ids_to_tokens(input_ids)
            
            with torch.no_grad():
                hidden_states = transformer_model(input_ids)[-1]
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

