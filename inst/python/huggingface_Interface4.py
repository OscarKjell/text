import os, sys

# Paths in python to task_finetune is set in R function
from task_finetune import main
import numpy as np
import json

def hgTransformerFineTune(json_path, text_outcome_df, text_outcome_df_val, text_outcome_df_test, 
                        is_regression = True, label_names = None, **kwargs):

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
    label_names : list
        List of strings containing the class names for classification task
    
    Returns
    -------
    None
    """

    args = json.load(open(json_path))
    return main(args, text_outcome_df, text_outcome_df_val, text_outcome_df_test, is_regression, label_names, **kwargs)
     
    

