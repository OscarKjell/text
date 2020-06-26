import torch
from transformers import *
import numpy as np
def hgTransformerGetEmbedding(text_strings = "Here is more text.",
                              pretrained_weights = 'bert-base-uncased',
                              tokenizer_class = BertTokenizer,
                              model_class = BertModel,
                              num_hidden_layers = 11,
                              return_tokens= True):
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
            hidden_states = transformer_model(torch.tensor([input_ids]))[0]
            #if layers != 'all':
            #    hidden_states = num_hidden_layers[int(layers)]
            all_embs.append(hidden_states.tolist())
            if return_tokens:
                all_toks.append(tokens)
    if return_tokens:
        return all_embs, all_toks
    else:
        return all_embs
if __name__   == '__main__':
    return_tokens=True
    if return_tokens:
        embeddings, tokens = hgTransformerGetEmbedding("Here is an example.", return_tokens=return_tokens)
    else:
        embeddings = hgTransformerGetEmbedding("Here is an example.", return_tokens=return_tokens)
    print(np.array(embeddings).shape)
    print(tokens)