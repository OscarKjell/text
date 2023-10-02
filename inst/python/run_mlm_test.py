from huggingface_Interface4 import hgTransformerMLM
import datasets

##########################################################################
### Test 1: MLM Task #####################################################
##########################################################################

def basic_mlm_test(**kwargs):
    """
    Basic test for checking if the MLM training feature works
    """
    data = datasets.load_dataset("rotten_tomatoes")
    train_df = data['train'].to_pandas()
    val_df = data['validation'].to_pandas()
    test_df = data['test'].to_pandas()
    json_path = './mlm_args2.json'
    hgTransformerMLM(json_path, train_df, val_df, test_df, **kwargs)
    return 


# kwargs: output_dir="/data/aviricnhipur/text_dummy_run/models/", seed=2023, do_train=True, do_eval=True, evaluation_strategy="epoch"    