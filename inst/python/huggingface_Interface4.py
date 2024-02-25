# Paths in python to task_finetune is set in R function
from task_finetune import main as task_finetuner
from run_mlm import main as mlm_finetuner
import json
import os

from platform import system


def set_tokenizer_parallelism(tokenizer_parallelism):
    if tokenizer_parallelism:
        os.environ["TOKENIZERS_PARALLELISM"] = "true"
    else:
        os.environ["TOKENIZERS_PARALLELISM"] = "false"


def hgTransformerMLM(json_path, text_df_train, text_df_val, text_df_test, **kwargs):
    """
    Simple Python method for MLM fine tuning pretrained Hugging Face models
    
    Parameters
    ----------
    json_path : str
        Path to the json file containing the arguments for fine tuning model
    text_df_train : pandas dataframe
        Dataframe containing the text for training
    text_df_val : pandas dataframe
        Dataframe containing the text for validation
    text_df_test : pandas dataframe
        Dataframe containing the text for testing
        
    Returns
    -------
    None
    """
    args = json.load(open(json_path))
    return mlm_finetuner(args, text_df_train, text_df_val, text_df_test, **kwargs)


def hgTransformerFineTune(json_path, 
                            text_outcome_df_train, 
                            text_outcome_df_val, 
                            text_outcome_df_test,
                            is_regression = True,
                            pytorch_mps_high_watermark_ratio,
                            tokenizer_parallelism = False,
                            label_names = None, 
                            **kwargs):

    """
    Simple Python method for fine tuning pretrained Hugging Face models

    Parameters
    ----------
    json_path : str
        Path to the json file containing the arguments for fine tuning model
    text_outcome_df_train : pandas dataframe
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

    # Check if running on macOS and PYTORCH_MPS_HIGH_WATERMARK_RATIO is set to 'TRUE'
    if system() == 'Darwin' and pytorch_mps_high_watermark_ratio:
      os.environ['PYTORCH_MPS_HIGH_WATERMARK_RATIO'] = '0.0'
      print("Setting PYTORCH_MPS_HIGH_WATERMARK_RATIO to '0.0' on macOS with enabled flag.")


    args = json.load(open(json_path))
    return task_finetuner(args, text_outcome_df_train, text_outcome_df_val, text_outcome_df_test, is_regression, label_names, **kwargs)
     
    

