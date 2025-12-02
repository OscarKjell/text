from huggingface_Interface3 import hgDLATKTransformerGetEmbedding
import numpy as np
import datasets
from datasets import Dataset

##########################################################################
### Test 1: Get Embeddings ###############################################
##########################################################################

def basic_DLATKGetEmbedding_test(**kwargs):
    """
    Basic test for checking if the Get Embeddings feature works
    """
    device = kwargs.pop("device", "cpu")
    batch_size = kwargs.pop("batch_size", 2)
    text_ids = kwargs.pop("text_ids", [])
    group_ids = kwargs.pop("group_ids", []) 
    
    if "text_strings" not in kwargs:
        data = datasets.load_dataset("rotten_tomatoes")
        text_strings = data['test']['text']
    else:
        text_strings = kwargs.pop("text_strings")
    return hgDLATKTransformerGetEmbedding(text_strings=text_strings, text_ids=text_ids, group_ids=group_ids, 
                                          device=device, batch_size=batch_size, **kwargs) 


# kwargs: output_dir="/data/avirinchipur/text_dummy_run/models/", seed=2023, do_train=True, do_eval=True, evaluation_strategy="epoch"    

if __name__ == "__main__":
    # data = datasets.load_dataset("rotten_tomatoes")
    # data = datasets.load_from_disk("/cronus_data/transformers_cache/datasets--rotten_tomatoes/")
    data = Dataset.from_parquet("/cronus_data/transformers_cache/datasets--rotten_tomatoes/snapshots/aa13bc287fa6fcab6daf52f0dfb9994269ffea28/test.parquet")
    text_strings = data['text']
    
    # msg_embs, cf_embs = basic_DLATKGetEmbedding_test(text_strings=text_strings, device="cuda", batch_size=16)
    # print (np.array(msg_embs).shape, np.array(cf_embs).shape)
    
    group_ids = [i//2 for i in range(len(text_strings))]
    cf_embs, token_embs, tokens = basic_DLATKGetEmbedding_test(text_strings=text_strings, device="cuda", batch_size=16, 
                                                    group_ids=group_ids, return_tokens=True)
    
    print (np.array(cf_embs).shape)

    text_strings = ['a '*768]*10
    group_ids = [i//2 for i in range(len(text_strings))]
    cf_embs, token_embs, tokens = basic_DLATKGetEmbedding_test(text_strings=text_strings, device="cuda", batch_size=16, 
                                                    group_ids=group_ids, return_tokens=True)
    
    print (np.array(cf_embs).shape)
    import pdb; pdb.set_trace()