from huggingface_Interface4 import hgTransformerMLM
import datasets
import pandas as pd

##########################################################################
### Test 1: MLM Task #####################################################
##########################################################################

def basic_mlm_test():
    """
    Basic test for checking if the MLM training feature works
    """
    data = datasets.load_dataset("rotten_tomatoes")
    train_df = pd.DataFrame(data['train'])
    val_df = pd.DataFrame(data['validation'])
    test_df = pd.DataFrame(data['test'])
    json_path = './mlm_args.json'
    hgTransformerMLM(json_path, train_df, val_df, test_df)
    return 
    