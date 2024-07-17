#note: I think layer 0 is the input embedding.
import warnings
warnings.simplefilter(action='ignore', category=FutureWarning)

import torch
import huggingface_hub
from transformers import AutoConfig, AutoModel, AutoTokenizer
try:
    from transformers.utils import logging
except ImportError:
    print("Warning: Unable to importing transformers.utils logging")
from transformers import pipeline
import numpy as np

import nltk
try:
    nltk.data.find('tokenizers/punkt/PY3/english.pickle')
except:
    nltk.download("punkt")

from nltk.tokenize import sent_tokenize

import os, sys

ACCEPTED_TASKS = ["text-classification", "sentiment-analysis", "question-answering", "translation", 
    "summarization", "token-classification", "ner", "text-generation", "zero-shot-classification"]

PIPELINE_RESULTS_BY_TASK = {
    "text-classification": ["POSITIVE", "NEGATIVE"], 
    "sentiment-analysis": ["POSITIVE", "NEGATIVE"], 
    "question-answering": ["answer"], 
    "translation": ["translation_text"], 
    "summarization": ["summary_text"], 
    "token-classification": ["entity"], 
    "ner": ["entity"], 
    "text-generation": ["generated_text", "generated_token_ids"], 
    "zero-shot-classification": ["scores"], 
}

def set_hg_gated_access(access_token):
    """
    Local save of the access token for gated models on hg.
    
    Parameters
    ----------
    access_token : str
        Steps to get the access_token:
        1. Log in to your Hugging Face account.
        2. Click on your profile picture in the top right corner.
        3. Select ‘Settings’ from the dropdown menu.
        4. In the settings, you’ll find an option to generate a new token.
        Or, visit URL: https://huggingface.co/settings/tokens
    """
    huggingface_hub.login(access_token)
    print("Successfully login to Huggingface!")
    
def del_hg_gated_access():
    """
    Remove the access_token saved locally.

    """
    huggingface_hub.logout()
    print("Successfully logout to Huggingface!")

def set_logging_level(logging_level):
    """
    Set the logging level

    Parameters
    ----------
    logging_level : str
        set logging level, options: critical, error, warning, info, debug
    """
    logging_level = logging_level.lower()
    # default level is warning, which is in between "error" and "info"
    if logging_level in ['warn', 'warning']:
        logging.set_verbosity_warning()
    elif logging_level == "critical":
        logging.set_verbosity(50)
    elif logging_level == "error":
        logging.set_verbosity_error()
    elif logging_level == "info":
        logging.set_verbosity_info()
    elif logging_level == "debug":
        logging.set_verbosity_debug()
    else:
        print("Warning: Logging level {l} is not an option.".format(l=logging_level))
        print("\tUse one of: critical, error, warning, info, debug")

def set_tokenizer_parallelism(tokenizer_parallelism):
    if tokenizer_parallelism:
        os.environ["TOKENIZERS_PARALLELISM"] = "true"
    else:
        os.environ["TOKENIZERS_PARALLELISM"] = "false"

def get_device(device):
    """
    Get device and device number

    Parameters
    ----------
    device : str
        name of device: 'cpu', 'gpu', 'cuda', 'mps', or of the form 'gpu:k', 'cuda:k', or 'mps:0'
        where k is a specific device number

    Returns
    -------
    device : str
        final selected device name
    device_num : int
        device number, -1 for CPU
    """
    device = device.lower()
    if not device.startswith('cpu') and not device.startswith('gpu') and not device.startswith('cuda') and not device.startswith('mps'):
        print("device must be 'cpu', 'gpu', 'cuda', 'mps', or of the form 'gpu:k', 'cuda:k', or 'mps:0'")
        print("\twhere k is an integer value for the device")
        print("Trying CPUs")
        device = 'cpu'
    
    device_num = -1
    if device != 'cpu':
        attached = False
        
        if hasattr(torch.backends, "mps"):
            mps_available = torch.backends.mps.is_available()
        else:
            mps_available = False
        print(f"MPS for Mac available: {mps_available}")
        if torch.cuda.is_available():
            if device == 'gpu' or device == 'cuda': 
                # assign to first gpu device number
                device = 'cuda'
                device_num = list(range(torch.cuda.device_count()))[0]
                attached = True
            elif 'gpu:' in device or 'cuda:' in device:
                try:
                    device_num = int(device.split(":")[-1])
                    device = 'cuda:' + str(device_num)
                    attached = True
                except:
                    attached = False
                    print(f"Device number {str(device_num)} does not exist! Use 'device = gpus' to see available gpu numbers.")
            elif 'gpus' in device:
                device = 'cuda'
                device_num = list(range(torch.cuda.device_count()))
                device = [device + ':' + str(num1) for num1 in device_num]
                attached = True
                print(f"Running on {str(len(device))} GPUs!")
                print(f"Available gpus to set: \n {device}")
        elif "mps" in device:
            if not torch.backends.mps.is_available():
                if not torch.backends.mps.is_built():
                    print("MPS not available because the current PyTorch install was not built with MPS enabled.")
                else:
                    print("MPS not available because the current MacOS version is not 12.3+ and/or you do not have an MPS-enabled device on this machine.")
            else:
                device_num = 0 # list(range(torch.cuda.device_count()))[0]
                device = 'mps:' + str(device_num)
                attached = True
                print("Using Metal Performance Shaders (MPS) backend for GPU training acceleration!")
        else:
            attached = False
        if not attached:
            print("Unable to use MPS (Mac M1+), CUDA (GPU), using CPU")
            device = "cpu"
            device_num = -1

    return device, device_num

def get_model(model, tokenizer_only=False, config_only=False, hg_gated=False, hg_token=""):
    """
    Get model and tokenizer from model string

    Parameters
    ----------
    model : str
        shortcut name for Hugging Face pretained model
        Full list https://huggingface.co/transformers/pretrained_models.html
    hg_gated : bool
        Set to True if the model is gated
    hg_token: str
        The token to access the gated model got in huggingface website
    
    Returns
    -------
    config
    tokenizer
    model
    """
    if "megatron-bert" in model:
        try:
            from transformers import BertTokenizer, MegatronBertForMaskedLM
        except:
            print("WARNING: You must install transformers>4.10 to use MegatronBertForMaskedLM")
            print("\tPlease try another model.")
            sys.exit()

        config = AutoConfig.from_pretrained(model, output_hidden_states=True)
        if not config_only:
            if "megatron-bert-cased" in model:
                tokenizer = BertTokenizer.from_pretrained('nvidia/megatron-bert-cased-345m')
            else:
                tokenizer = BertTokenizer.from_pretrained('nvidia/megatron-bert-uncased-345m')
            transformer_model = MegatronBertForMaskedLM.from_pretrained(model, config=config)
    elif "bigscience/bloom" in model:
        try:
            from transformers import BloomTokenizerFast, BloomModel, BloomConfig
        except:
            print("WARNING: You must install transformers>4.21.0 to use BloomModel")
            print("\tPlease try another model.")
            sys.exit()
        
        config = BloomConfig()
        if not config_only:
            tokenizer = BloomTokenizerFast.from_pretrained(model)
            transformer_model = BloomModel.from_pretrained(model, config=config)
    elif "hart" in model:
        try:
            from transformers import AutoConfig, AutoTokenizer
            from hart import HaRTModel
        except:
            print("WARNING: HART")
            print("\tPlease try another model.")
            sys.exit()
        model_path = "hlab/hart-gpt2sml-twt-v1"
        config = AutoConfig.from_pretrained(model_path)
        
        if not config_only:
            tokenizer = AutoTokenizer.from_pretrained(model_path)
            transformer_model = HaRTModel(model_path, config=config)
            print("HART 1")
            print(transformer_model)
    else:
        #print("I am in get_model function now!!!!")
        #print(f"!!!!hg_gated: {hg_gated} !!!")
        #print(f"!!!!hg_token: {hg_token} !!!")
        if hg_gated:
            set_hg_gated_access(access_token=hg_token)
        config = AutoConfig.from_pretrained(model, output_hidden_states=True)
        if not config_only:
            tokenizer = AutoTokenizer.from_pretrained(model)
            transformer_model = AutoModel.from_pretrained(model, config=config)
            
    if config_only:
        return config
    elif tokenizer_only:
        return tokenizer
    else:
        return config, tokenizer, transformer_model

def get_number_of_hidden_layers(model, logging_level = "error", hg_gated=False, hg_token=""):
    """
    Return the number of hidden layers for a given model.
    Returns -1 if the model's config doesn't have the num_hidden_layers parameter
    """
    set_logging_level(logging_level)
    config = get_model(model, config_only=True, hg_gated=hg_gated, hg_token=hg_token)
    number_of_hidden_layers = -1
    try:
        number_of_hidden_layers = config.num_hidden_layers
    except:
        print("Warning: Unable to get number of hidden layers")
        print("         num_hidden_layers is not a parameter of transformer_model.config")
        pass
    return number_of_hidden_layers

def hgTransformerGetPipeline(text_strings,
                            task = '',
                            model = '',
                            device = 'cpu',
                            tokenizer_parallelism = False,
                            logging_level = 'warning',
                            return_incorrect_results = False,
                            hg_gated = False,
                            hg_token = "",
                            set_seed = None,
                            **kwargs):
    """
    Simple interface getting Huggingface Pipeline
    https://huggingface.co/docs/transformers/main_classes/pipelines
    
    Parameters
    ----------
    text_strings : list
        list of strings, each is embedded separately
    task : str
        String representing task
        Task descriptions https://huggingface.co/docs/transformers/v4.20.1/en/task_summary
    model : str
        shortcut name for Hugging Face pretained model
        Full list https://huggingface.co/transformers/pretrained_models.html
    device : str
        name of device: 'cpu', 'gpu', or 'gpu:k' where k is a specific device number
    tokenizer_parallelism :  bool
        something
    logging_level : str
        set logging level, options: critical, error, warning, info, debug
    return_incorrect_results : bool
        return results if they are not properly formatted for the task
    hg_gated : bool
        Set to True if the accessed model is gated
    hg_token: str
        The token needed to access the gated model, gen in huggingface website 
    set_seed : int
        integer value for manually setting seed
    kwargs : dict
        pipeline task specific arguments
    
    Returns
    -------
    sentiment_scores : list
        list of dictionaries with sentiment scores and labels
    """

    if not (task in ACCEPTED_TASKS or task.startswith("translation")):
        print("Task {t} is not recognized".format(t=task))
        return []
    if isinstance(set_seed, int):
        torch.manual_seed(set_seed)
    set_logging_level(logging_level)
    set_tokenizer_parallelism(tokenizer_parallelism)
    device, device_num = get_device(device)
    
    # check and adjust input types
    if not isinstance(text_strings, list):
        text_strings = [text_strings]
    if model:
        config, tokenizer, transformer_model = get_model(model,hg_gated=hg_gated, hg_token=hg_token)
        if device_num >= 0:
            task_pipeline = pipeline(task, model=model, tokenizer=tokenizer, device=device_num)
        else:
            task_pipeline = pipeline(task, model=model, tokenizer=tokenizer)
    else:
        if device_num >= 0:
            task_pipeline = pipeline(task, device=device_num)
        else:
            task_pipeline = pipeline(task)
    
    task_scores = []
    if task in ['question-answering', 'zero-shot-classification']:
        task_scores = task_pipeline(**kwargs)
    else:
        task_scores = task_pipeline(text_strings, **kwargs)

    if len(task_scores) == 0 or (isinstance(task_scores, list) and len(task_scores[0]) == 0):
        return task_scores
    
    results_check = {}
    if isinstance(task_scores, dict):
        results_check = task_scores
        task_scores = [task_scores]
    elif isinstance(task_scores[0], dict):
        results_check = task_scores[0]
    elif isinstance(task_scores[0][0], dict):
        results_check = task_scores[0][0]
        task_scores = task_scores[0]
   
    if task.startswith("translation"):
        default_result_keys = PIPELINE_RESULTS_BY_TASK["translation"]
    else:
        default_result_keys = PIPELINE_RESULTS_BY_TASK[task]
    
    print_warning = False
    if task in ["text-classification", "sentiment-analysis"]:
        if results_check['label'] not in default_result_keys:
            print_warning = True
    elif len(task_scores) > 0 and not any(k in default_result_keys for k in list(results_check.keys())):
        print_warning = True
    if print_warning:
        print("WARNING: Results do not match the defaults for the task")
        print("\tBy default, one of the following should be in the results for this task: {t}".format(t=", ".join(PIPELINE_RESULTS_BY_TASK[task])))
        print("\tYou may want to try a different model or the default model for the task")
        # todo add list of defaults and print the task default in warning
        if not return_incorrect_results:
            task_scores = []
    return task_scores


def get_hart_embeddings(
    data,
    text_column=None,
    user_id_column=None,
    text_id_column=None,
    block_size=1024,
    max_blocks=None,
    order_by_column=None,
    retain_original_order=True,
    return_document_embeddings=True,
    return_user_representations=True,
    use_last_pred_token_as_user_representation=False,
    last_pred_token_as_last_insep=False,
    return_all_user_states=False,
    return_output_with_data=False,
    return_word_embeddings=False,
    return_word_embeds_with_insep=False,
    representative_layer='last'
 ):
    model_path= "hlab/hart-gpt2sml-twt-v1"
    
    from hart import HaRTModel, load_tokenized_dataset as load_dataset, hart_default_data_collator
    from transformers import AutoConfig, AutoTokenizer
    from torch.utils.data.dataloader import DataLoader
    from torch.utils.data.sampler import SequentialSampler

    
    from tqdm import tqdm

    
    if text_column is None:
        raise ValueError("text_column is required")
    config = AutoConfig.from_pretrained(model_path)
    tokenizer = AutoTokenizer.from_pretrained(model_path)
    config.pad_token_id = tokenizer.eos_token_id
    config.sep_token_id = tokenizer.sep_token_id

    model = HaRTModel(model_path, config=config)

    device = torch.device('cuda' if torch.cuda.is_available() else 'cpu')
    print(device)
    model.to(device)
    model.eval()

    x, original_data_order = load_dataset(
        tokenizer=tokenizer,
        data=data,
        text_column=text_column,
        user_id_column=user_id_column,
        text_id_column=text_id_column,
        block_size=block_size,
        max_blocks=max_blocks,
        order_by_column=order_by_column,
        retain_original_order=retain_original_order
    )

    # only for debugging
    print("***************************")
    print(len(x))
    # x = x[:35]

    user_id_column = user_id_column if user_id_column else 'user_id'
    text_id_column = text_id_column if text_id_column else 'text_id'

    dataloader = DataLoader(
        x,
        sampler=SequentialSampler(x),
        batch_size=5,
        collate_fn=hart_default_data_collator(tokenizer),
    )

    outputs = []

    with torch.no_grad():
        for steps, inputs in enumerate(tqdm(dataloader)):
            inputs = {k: v.to(device) if k in (
                "input_ids", "attention_mask", "labels", "history") else v for k, v in inputs.items()}
            model_outputs = model(**inputs, 
                                    return_all_user_states=return_all_user_states,
                                    return_user_representation_as_mean_user_states=return_user_representations,
                                    use_last_pred_token_as_user_representation=use_last_pred_token_as_user_representation,
                                    return_all_doc_embeds=return_document_embeddings,
                                    return_token_embeds=return_word_embeddings,
                                    representative_layer=representative_layer,
                                    return_token_embeds_with_insep=return_word_embeds_with_insep,
                                    last_pred_token_as_last_insep=last_pred_token_as_last_insep,
                                    )
            outputs.append(model_outputs)

    return_dict = {}

    if return_document_embeddings:
        # combine doc_embeds_with_text_ids into a single dictionary
        doc_embeds_with_text_ids = {}
        for d in outputs:
            for k, v in d['doc_embeds_with_text_ids'].items():
                if k not in doc_embeds_with_text_ids:
                    doc_embeds_with_text_ids[k] = []
                doc_embeds_with_text_ids[k].append(v)

        doc_embeds_with_text_ids_df = pd.DataFrame.from_dict(
            doc_embeds_with_text_ids, orient='index').rename(columns={0: 'doc_embeds'})

        if retain_original_order:
            doc_embeds_in_original_order = original_data_order.join(
                doc_embeds_with_text_ids_df, on=text_id_column)['doc_embeds']
            
            return_dict['document embeddings'] = torch.stack(doc_embeds_in_original_order.tolist())
        else:
            return_dict['document embeddings'] = doc_embeds_with_text_ids_df

    if return_user_representations:
        # combine user representations into a single dictionary
        user_reps_with_user_ids = {}
        for d in outputs:
            for k, v in d['user representations'].items():
                if k not in user_reps_with_user_ids:
                    user_reps_with_user_ids[k] = []
                user_reps_with_user_ids[k].append(v)

        user_reps_with_user_ids_df = pd.DataFrame.from_dict(
            user_reps_with_user_ids, orient='index').rename(columns={0: 'user_reps'})

        if retain_original_order:
            user_reps_in_original_order = original_data_order.join(
                user_reps_with_user_ids_df, on=user_id_column)['user_reps']
            
            return_dict['user representations'] = torch.stack(user_reps_in_original_order.tolist())
        else:
            return_dict['user representations'] = user_reps_with_user_ids_df

    if return_all_user_states:
        # combine all_user_states into a single dictionary
        all_user_states_with_user_ids = {}
        for d in outputs:
            for k, v in d['all_user_states'].items():
                if k not in all_user_states_with_user_ids:
                    all_user_states_with_user_ids[k] = []
                all_user_states_with_user_ids[k].append(v)

        all_user_states_with_user_ids_df = pd.DataFrame.from_dict(
            all_user_states_with_user_ids, orient='index').rename(columns={0: 'all_user_states'})

        if retain_original_order:
            all_user_states_in_original_order = original_data_order.join(
                all_user_states_with_user_ids_df, on=user_id_column)['all_user_states']
            
            return_dict['all user states'] = all_user_states_in_original_order
        else:
            return_dict['all user states'] = all_user_states_with_user_ids_df

    if return_word_embeddings:
        # combine token_embeds_with_text_ids into a single dictionary
        token_embeds_with_text_ids = {}
        for d in outputs:
            for k, v in d['word_embeds_with_text_ids'].items():
                if k not in token_embeds_with_text_ids:
                    token_embeds_with_text_ids[k] = []
                token_embeds_with_text_ids[k].append(v)

        token_embeds_with_text_ids_df = pd.DataFrame.from_dict(
            token_embeds_with_text_ids, orient='index').rename(columns={0: 'token_ids_and_embeds'})

        # split the token_ids_and_embeds into token_ids and token embeds
        token_embeds_with_text_ids_df[['token_ids', 'token_embeds']] = pd.DataFrame(token_embeds_with_text_ids_df['token_ids_and_embeds'].tolist(), index=token_embeds_with_text_ids_df.index)
        
        if retain_original_order:
            print(original_data_order)
            token_ids_embeds_in_original_order = original_data_order.join(
                token_embeds_with_text_ids_df, on=text_id_column)
            token_embeds_in_original_order = token_ids_embeds_in_original_order['token_embeds']
            token_ids_in_original_order = token_ids_embeds_in_original_order['token_ids']
            
            return_dict['word embeddings'] = token_embeds_in_original_order.tolist()
            return_dict['token ids'] = token_ids_in_original_order.tolist()
        else:
            token_embeds_with_text_ids_df['text_id'] = token_embeds_with_text_ids_df.index
            token_embeds_with_text_ids_df.reset_index(drop=True, inplace=True)
            return_dict['word embeddings'] = token_embeds_with_text_ids_df[['text_id', 'token_ids', 'token_embeds']]
                    
    if return_output_with_data:
        data_with_outputs = original_data_order
        
        if return_document_embeddings:
            data_with_outputs = data_with_outputs.join(
                doc_embeds_with_text_ids_df, on=text_id_column)
        if return_user_representations:
            data_with_outputs = data_with_outputs.join(
                user_reps_with_user_ids_df, on=user_id_column)
        if return_all_user_states:
            data_with_outputs = data_with_outputs.join(
                all_user_states_with_user_ids_df, on=user_id_column)
        
        return_dict['data with outputs'] = data_with_outputs
        
    return return_dict
        

    
### EXAMPLE TEST CODE:
#if __name__   == '__main__':
#   embeddings, tokens = hgTransformerGetEmbedding("Here is one sentence.", layers=[0,10], device="gpu", logging_level="warn")
#   print(np.array(embeddings).shape)
#   print(tokens)
#
#   embeddings, tokens = hgTransformerGetEmbedding("Here is more sentences. But why is not . and , and ? indicated with SEP?", layers=[0,10], device="gpu")
#   print(np.array(embeddings).shape)
#   print(tokens)
