import os, sys
sys.path.append('inst/python/')
from task_finetune import main
import numpy as np
import json

def hgTransformerFineTune(json_path, text_outcome_df, text_outcome_df_val, text_outcome_df_test, is_regression = True):

    """
    Simple Python method for fine tuning pretrained Hugging Face models

    Parameters
    ----------
    json_path : str
        Path to the json file containing the arguments for fine tuning model
    text_outcome_df : pandas dataframe
        Dataframe containing the text and outcome variables for training
    text_outcome_df_val : pandas dataframe
        Dataframe containing the text and outcome variables for validation
    text_outcome_df_test : pandas dataframe
        Dataframe containing the text and outcome variables for testing
    is_regression : bool
        True if the outcome variable is continuous, False if the outcome variable is categorical
    
    Returns
    -------
    None
    """
    # if os.path.isdir("/Users/oscarkjell/Desktop/1 Projects/0 Research/0 text r-package/text/"):
    #   print("ADI Hej")
    args = json.load(open(json_path))
    return main(args, text_outcome_df, text_outcome_df_val, text_outcome_df_test, is_regression)
     
    

