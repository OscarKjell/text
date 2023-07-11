import numpy as np
import pandas as pd
#import huggingface_Interface3

def sortingLayersPy(x, layers, return_tokens):
    # If selecting "all" layers, find out number of layers to help indicate layer index later in code
    if isinstance(layers, str):
        layers = list(range(len(x[0][0])))
    if isinstance(layers, float):
        if layers > 0: layers -= 1 
        layers = list(layers)

    # Find number of dimensions (where the place differ depending on return_token is True or False)
    if return_tokens:
        dimensions = len(x[0][0][0][0][0])
        participants = len(x[0])
    else:
        dimensions = len(x[0][0][0][0])
        participants = len(x)

    # Tidy-structure tokens and embeddings
    # Loop over the cases in the variable; i_in_variable = 0
    variable_x = []
    for i_in_variable in range(participants):
        if return_tokens:
            tokens = x[1][i_in_variable]
            token_id = list(range(1, len(tokens)+1))
            all_layers = x[0][i_in_variable]
        else:
            tokens = None
            all_layers = x[i_in_variable]
            # Count number of embeddings within one layer
            token_id = list(range(1, len(all_layers[0][0])+1))

        # Loop of the number of layers; i_layers=1
        layers_list = []
        for i_layers in range(len(all_layers)):
            i_layers_for_tokens = all_layers[i_layers]

            # Transpose layers and give each column a DimX names
            i_layers_for_tokens = np.array(i_layers_for_tokens)
            i_layers_for_tokens = np.reshape(i_layers_for_tokens, (i_layers_for_tokens.shape[1], -1))
            
            layers_4_token = pd.DataFrame(i_layers_for_tokens) # 
            layers_4_token.columns = ["Dim" + str(i) for i in range(1, dimensions+1)]

            if return_tokens:
                tokens_layer_number = pd.DataFrame({'tokens': tokens,
                                                     'token_id': token_id,
                                                     'layer_number': [layers[i_layers]]*len(tokens)})
                # Bind tokens with word embeddings (not selecting <pad>s)
                tokens_lnumber_layers = pd.concat([tokens_layer_number,
                                                    layers_4_token.iloc[0:len(tokens), :]], axis=1)
            else:
                layer_number = pd.DataFrame({'token_id': token_id,
                                              'layer_number': [layers[i_layers]]*len(all_layers[0][0])})
                # Bind tokens with word embeddings (not selecting <pad>s)
                tokens_lnumber_layers = pd.concat([layer_number,
                                                    layers_4_token.iloc[0:len(all_layers[0][0]), :]], axis=1)

            layers_list.append(tokens_lnumber_layers)
        layers_tibble = pd.concat(layers_list)

        variable_x.append(layers_tibble)
    return variable_x

# if __name__ == '__main__':

#     model = "bert-base-cased"
#     layers = [-2] # [-2,-1]
#     return_tokens = True
#     device = "cpu"
#     tokenizer_parallelism = False
#     max_token_to_sentence = 4
#     logging_level = "error"

#     hg_embeddings = huggingface_Interface3.hgTransformerGetEmbedding(
#         text_strings = ["Hello word", "Mary, hello! How are you?", "Hi!"],
#         model = model,
#         layers = layers,
#         return_tokens = return_tokens,
#         device = device,
#         tokenizer_parallelism = tokenizer_parallelism,
#         #model_max_length = model_max_length,
#         max_token_to_sentence = max_token_to_sentence,
#         logging_level = logging_level
#     )
#     print(f"The type: {type(hg_embeddings)}")

#     temp = sortingLayersPy(hg_embeddings, layers, True)
#     print(type(temp))

