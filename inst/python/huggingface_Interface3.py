#note: I think layer 0 is the input embedding. 
import warnings
warnings.simplefilter(action='ignore', category=FutureWarning)

import torch
from transformers import *
import numpy as np



#TODO:#dictionary of pretrained weights to models
def hgTransformerGetEmbedding(text_strings, #list of strings
                              pretrained_weights = 'bert-base-uncased',
                              tokenizer_class = BertTokenizer,
                              model_class = BertModel,
                              layers = 'all',  #all or a list of layers to keep
                              return_tokens=True):

    tokenizer = tokenizer_class.from_pretrained(pretrained_weights)
    transformer_model = model_class.from_pretrained(pretrained_weights, output_hidden_states=True)

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

        input_ids = tokenizer.encode(text_string, add_special_tokens=True)
        if return_tokens:
            tokens = tokenizer.convert_ids_to_tokens(input_ids)

        with torch.no_grad():
            hidden_states = transformer_model(torch.tensor([input_ids]))[2]
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

