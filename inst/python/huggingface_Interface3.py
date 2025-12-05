#note: I think layer 0 is the input embedding.
import warnings
warnings.simplefilter(action='ignore', category=FutureWarning)

import torch
import huggingface_hub
import transformers
from transformers import AutoConfig, AutoModel, AutoTokenizer
from transformers import pipeline
try:
    from transformers.utils import logging
except ImportError:
    print("Warning: Unable to importing transformers.utils logging")

import numpy as np
import nltk
try:
    nltk.data.find('tokenizers/punkt/PY3/english.pickle')
except:
    nltk.download('punkt')
#try:
#    nltk.data.find('tokenizers/punkt_tab/english/')
#except:
#    nltk.download('punkt_tab')

from nltk.tokenize import sent_tokenize

import os, sys

from collections import Counter
from tqdm import tqdm
from transformer_embs import transformer_embeddings

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

def get_model(model, tokenizer_only=False, config_only=False, hg_gated=False, hg_token="", trust_remote_code=False):
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
    else:
        if hg_gated:
            set_hg_gated_access(access_token=hg_token)
        else: 
            pass
        config = AutoConfig.from_pretrained(model, output_hidden_states=True)
        if not config_only:
            tokenizer = AutoTokenizer.from_pretrained(model)
            transformer_model = AutoModel.from_pretrained(model, config=config, trust_remote_code=trust_remote_code)
            
    if config_only:
        return config
    elif tokenizer_only:
        # Do not know how to fix this. Some decoder-only files do not have pad_token.
        if tokenizer.pad_token is None:
            print("The language model entered might have issues since the model does not provide the padding_token.")
            print("Consider use BERT-like models instead if meeting errors.")
        #    tokenizer.pad_token = tokenizer.eos_token
        #    tokenizer.pad_token_id = tokenizer.eos_token_id
        return tokenizer
    else:
        if tokenizer.pad_token is None:
            print("The language model entered might have issues since the model does not provide the padding_token.")
            print("Consider use BERT-like models instead if meeting errors.")    
        #    tokenizer.pad_token = tokenizer.eos_token
        #    tokenizer.pad_token_id = tokenizer.eos_token_id        
        return config, tokenizer, transformer_model

def get_number_of_hidden_layers(model, logging_level = "error", hg_gated = False, hg_token = "", trust_remote_code = False):
    """
    Return the number of hidden layers for a given model.
    Returns -1 if the model's config doesn't have the num_hidden_layers parameter
    """
    set_logging_level(logging_level)
    config = get_model(model, config_only=True, hg_gated=hg_gated, hg_token=hg_token, trust_remote_code=trust_remote_code)
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
                            force_return_results = False,
                            hg_gated = False,
                            hg_token = "",
                            trust_remote_code = False,
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
    force_return_results : bool
        return results if they are not properly formatted for the task
    hg_gated : bool
        Set to True if the accessed model is gated
    hg_token: str
        The token needed to access the gated model, gen in huggingface website 
    trust_remote_code : bool
        use a model with custom code on the Huggingface Hub
    set_seed : int
        integer value for manually setting seed
    kwargs : dict
        pipeline task specific arguments
    
    Returns
    -------
    sentiment_scores : list
        list of dictionaries with sentiment scores and labels
    """

    if isinstance(set_seed, int):
        torch.manual_seed(set_seed)
    set_logging_level(logging_level)
    set_tokenizer_parallelism(tokenizer_parallelism)
    device, device_num = get_device(device)
    
    # check and adjust input types
    if not isinstance(text_strings, list):
        text_strings = [text_strings]
    
    if model:
        config, tokenizer, transformer_model = get_model(model,hg_gated=hg_gated, hg_token=hg_token, trust_remote_code=trust_remote_code)
        if device_num >= 0:
            task_pipeline = pipeline(task, model=model, tokenizer=tokenizer, device=device_num)
        else:
            task_pipeline = pipeline(task, model=model, tokenizer=tokenizer)
    else:
        if device_num >= 0:
            task_pipeline = pipeline(task, device=device_num)
        else:
            task_pipeline = pipeline(task)

    if transformers.__version__ >= "4.20" and "return_all_scores" in kwargs:
        return_all_scores = kwargs["return_all_scores"]
        if return_all_scores == True:
            kwargs["top_k"] = None
        else:
            kwargs["top_k"] = 1
        del kwargs["return_all_scores"]
    
    task_scores = []
    if task in ['question-answering', 'zero-shot-classification']:
        task_scores = task_pipeline(**kwargs)
    else:
        task_scores = task_pipeline(text_strings, **kwargs)
    
    return task_scores


# Convert floats to integers or propagate None
def _as_integer(x):
    if isinstance(x, int) or isinstance(x, np.integer):
        return x
    elif isinstance(x, float) or isinstance(x, np.floating):
        return int(x)
    else:
        return None

def hgTransformerGetTextGeneration(text_strings,
                            model = '',
                            device = 'cpu',
                            tokenizer_parallelism = False,
                            max_length = None,
                            max_new_tokens = 20,
                            min_length = 0,
                            min_new_tokens = None,
                            logging_level = 'warning',
                            force_return_results = False,
                            set_seed = None,
                            return_tensors = False,
                            #return_text = True,
                            return_full_text = True,
                            clean_up_tokenization_spaces = False,
                            prefix = '', 
                            handle_long_generation = None):
    # Prepare kwargs
    if max_new_tokens is not None and max_new_tokens <= 0:
        print(f"Warning: `max_new_tokens` must be greater than 0, but is {max_new_tokens}")
        print( "         Using default value…")
        max_new_tokens = None
    generation_kwargs = {
        'max_length': _as_integer(max_length),
        'min_length': _as_integer(min_length),
        'min_new_tokens': _as_integer(min_new_tokens)
    }
    # `max_new_tokens` should not be explicitly None
    max_new_tokens = _as_integer(max_new_tokens)
    if max_new_tokens is not None:
        generation_kwargs['max_new_tokens'] = max_new_tokens
    
    if return_tensors:
        if return_full_text:
            print("Warning: you set return_tensors and return_text (or return_full_text)")
            print("         Returning tensors only, as you cannot return both tensors and text.")
            print("         Please set return_tensors = FALSE if you need the generated text.")
        generated_texts = hgTransformerGetPipeline(text_strings = text_strings,
                            task = 'text-generation',
                            model = model,
                            device = device,
                            tokenizer_parallelism = tokenizer_parallelism,
                            logging_level = logging_level,
                            force_return_results = force_return_results,
                            set_seed = set_seed,
                            return_tensors = return_tensors, 
                            clean_up_tokenization_spaces = clean_up_tokenization_spaces, 
                            prefix = prefix,
                            handle_long_generation = handle_long_generation,
                            **generation_kwargs)
    else:
        generated_texts = hgTransformerGetPipeline(text_strings = text_strings,
                            task = 'text-generation',
                            model = model,
                            device = device,
                            tokenizer_parallelism = tokenizer_parallelism,
                            logging_level = logging_level,
                            force_return_results = force_return_results,
                            set_seed = set_seed,
                            #return_tensors = return_tensors, 
                            #return_text = return_text, 
                            return_full_text = return_full_text, 
                            clean_up_tokenization_spaces = clean_up_tokenization_spaces, 
                            prefix = prefix,
                            handle_long_generation = handle_long_generation,
                            **generation_kwargs)
    return generated_texts

def hgTransformerGetNER(text_strings,
                            model = '',
                            device = 'cpu',
                            tokenizer_parallelism = False,
                            logging_level = 'warning',
                            force_return_results = False,
                            set_seed = None):
    ner_scores = hgTransformerGetPipeline(text_strings = text_strings,
                            task = 'ner',
                            model = model,
                            device = device,
                            tokenizer_parallelism = tokenizer_parallelism,
                            logging_level = logging_level,
                            force_return_results = force_return_results,
                            set_seed = set_seed)
    return ner_scores

def hgTransformerGetZeroShot(sequences,
                            candidate_labels,
                            model = '',
                            device = 'cpu',
                            tokenizer_parallelism = False,
                            logging_level = 'warning',
                            force_return_results = False,
                            set_seed = None,
                            hypothesis_template = "This example is {}.",
                            multi_label = False):
    classifier_output = hgTransformerGetPipeline(text_strings = [],
                            task = 'zero-shot-classification',
                            model = model,
                            device = device,
                            tokenizer_parallelism = tokenizer_parallelism,
                            logging_level = logging_level,
                            force_return_results = force_return_results,
                            set_seed = set_seed,
                            sequences = sequences,
                            candidate_labels = candidate_labels,
                            hypothesis_template = hypothesis_template,
                            multi_label = multi_label)
    return classifier_output

def hgTransformerGetSentiment(text_strings,
                            model = '',
                            device = 'cpu',
                            tokenizer_parallelism = False,
                            logging_level = 'warning',
                            force_return_results = False,
                            set_seed = None,
                            return_all_scores = False,
                            function_to_apply = None):
    sentiment_scores = hgTransformerGetPipeline(text_strings = text_strings,
                            task = 'sentiment-analysis',
                            model = model,
                            device = device,
                            tokenizer_parallelism = tokenizer_parallelism,
                            logging_level = logging_level,
                            force_return_results = force_return_results,
                            set_seed = set_seed,
                            return_all_scores = return_all_scores,
                            function_to_apply = function_to_apply)
    return sentiment_scores

def hgTransformerGetSummarization(text_strings,
                            model = '',
                            device = 'cpu',
                            tokenizer_parallelism = False,
                            logging_level = 'warning',
                            force_return_results = False,
                            set_seed = None,
                            return_text = True,
                            return_tensors = False,
                            clean_up_tokenization_spaces = False, 
                            min_length = 10,
                            max_length = 200):
    summarizations = hgTransformerGetPipeline(text_strings = text_strings,
                            task = 'summarization',
                            model = model,
                            device = device,
                            tokenizer_parallelism = tokenizer_parallelism,
                            logging_level = logging_level,
                            force_return_results = force_return_results,
                            set_seed = set_seed,
                            return_text = return_text, 
                            return_tensors = return_tensors, 
                            clean_up_tokenization_spaces = clean_up_tokenization_spaces, 
                            min_length = min_length,
                            max_length = max_length)
    return summarizations

def hgTransformerGetQA(question,
                        context,
                        model = '',
                        device = 'cpu',
                        tokenizer_parallelism = False,
                        logging_level = 'warning',
                        force_return_results = False,
                        set_seed = None,
                        top_k = 1,
                        doc_stride = 128,
                        max_answer_len = 15,
                        max_seq_len = 384,
                        max_question_len = 64,
                        handle_impossible_answer = False):
    qas = hgTransformerGetPipeline(text_strings = [],
                            task = 'question-answering',
                            model = model,
                            device = device,
                            tokenizer_parallelism = tokenizer_parallelism,
                            logging_level = logging_level,
                            force_return_results = force_return_results,
                            set_seed = set_seed,
                            question = question, 
                            context = context, 
                            top_k = top_k, 
                            doc_stride = doc_stride, 
                            max_answer_len = max_answer_len, 
                            max_seq_len = max_seq_len, 
                            max_question_len = max_question_len, 
                            handle_impossible_answer = handle_impossible_answer)
    return qas

def hgTransformerGetTranslation(text_strings,
                            model = '',
                            device = 'cpu',
                            tokenizer_parallelism = False,
                            logging_level = 'warning',
                            force_return_results = False,
                            set_seed = None,
                            source_lang = '',
                            target_lang = '',
                            return_tensors = False,
                            return_text = True,
                            clean_up_tokenization_spaces = False, 
                            max_length = ''):
    task = 'translation'
    if source_lang and target_lang:
        task = "translation_{s}_to_{t}".format(s=source_lang, t=target_lang)
    translations = hgTransformerGetPipeline(text_strings = text_strings,
                            task = task,
                            model = model,
                            device = device,
                            tokenizer_parallelism = tokenizer_parallelism,
                            logging_level = logging_level,
                            force_return_results = force_return_results,
                            set_seed = set_seed,
                            src_lang = source_lang,
                            tgt_lang = target_lang,
                            return_tensors = return_tensors,
                            return_text = return_text,
                            clean_up_tokenization_spaces = clean_up_tokenization_spaces, 
                            max_length = max_length)
    return translations

def hgTransformerGetEmbedding(text_strings,
                              model = 'bert-large-uncased',
                              layers = 'all',
                              return_tokens = True,
                              max_token_to_sentence = 4,
                              device = 'cpu',
                              tokenizer_parallelism = False,
                              model_max_length = None,
                              hg_gated = False,
                              hg_token = "",
                              trust_remote_code = False,
                              logging_level = 'warning',
                              sentence_tokenize = True):
    """
    Simple Python method for embedding text with pretained Hugging Face models

    Parameters
    ----------
    text_strings : list
        list of strings, each is embedded separately
    model : str
        shortcut name for Hugging Face pretained model
        Full list https://huggingface.co/transformers/pretrained_models.html
    layers : str or list
        'all' or an integer list of layers to keep
    return_tokens : boolean
        return tokenized version of text_strings
    max_token_to_sentence : int
        maximum number of tokens in a string to handle before switching to embedding text
        sentence by sentence
    device : str
        name of device: 'cpu', 'gpu', or 'gpu:k' where k is a specific device number
    tokenizer_parallelism :  bool
        something
    model_max_length : int
        maximum length of the tokenized text
    hg_gated : bool
        Whether the accessed model is gated
    hg_token: str
        The token generated in huggingface website
    trust_remote_code : bool
        use a model with custom code on the Huggingface Hub
    logging_level : str
        set logging level, options: critical, error, warning, info, debug
    sentence_tokenize : bool
        tokenize long documents into sentences before embedding

    Returns
    -------
    all_embs : list
        embeddings for each item in text_strings
    all_toks : list, optional
        tokenized version of text_strings
    """
    #print("I am in hgTransformerGetEmbedding function now!!!!")
    #print(f"!!!!hg_gated: {hg_gated} !!!")
    #print(f"!!!!hg_token: {hg_token} !!!")
                                  
    set_logging_level(logging_level)
    set_tokenizer_parallelism(tokenizer_parallelism)
    device, device_num = get_device(device)

    config, tokenizer, transformer_model = get_model(model, hg_gated=hg_gated, hg_token=hg_token, trust_remote_code=trust_remote_code)

    if device != 'cpu':
        transformer_model.to(device)

    max_tokens = tokenizer.max_len_sentences_pair

    # check and adjust input types
    if not isinstance(text_strings, list):
        text_strings = [text_strings]

    if layers != 'all':
        if not isinstance(layers, list):
            layers = [layers]
        layers = [int(i) for i in layers]

    all_embs = []
    all_toks = []

    for text_string in text_strings:
        # if length of text_string is > max_token_to_sentence*4
        # embedd each sentence separately
        if len(text_string) > max_token_to_sentence*4 and sentence_tokenize:
            sentence_batch = [s for s in sent_tokenize(text_string)]
            if model_max_length is None:
                batch = tokenizer(sentence_batch, padding=True, truncation=True, add_special_tokens=True)
            else:
                batch = tokenizer(sentence_batch, padding=True, truncation=True, add_special_tokens=True, max_length=model_max_length)
            input_ids = torch.tensor(batch["input_ids"])
            attention_mask = torch.tensor(batch['attention_mask'])
            if device != 'cpu':
                input_ids = input_ids.to(device)
                attention_mask = attention_mask.to(device)

            if return_tokens:
                tokens = []
                for ids in input_ids:
                    tokens.extend([token for token in tokenizer.convert_ids_to_tokens(ids) if token != '[PAD]' and token != '<pad>'])
                all_toks.append(tokens)

            with torch.no_grad():
                hidden_states = transformer_model(input_ids,attention_mask=attention_mask)[-1]
                if layers != 'all':
                    hidden_states = [hidden_states[l] for l in layers]
                hidden_states = [h.tolist() for h in hidden_states]

            sent_embedding = []

            for l in range(len(hidden_states)): # iterate over layers
                layer_embedding = []
                for m in range(len(hidden_states[l])): # iterate over sentences
                    layer_embedding.extend([tok for ii, tok in enumerate(hidden_states[l][m]) if attention_mask[m][ii]>0])
                sent_embedding.append(layer_embedding)

            all_embs.append([[l] for l in sent_embedding])
        else:
            input_ids = tokenizer.encode(text_string, add_special_tokens=True)
            if return_tokens:
                tokens = tokenizer.convert_ids_to_tokens(input_ids)

            if device != 'cpu':
                input_ids = torch.tensor([input_ids]).to(device)
            else:
                input_ids = torch.tensor([input_ids])

            with torch.no_grad():
                hidden_states = transformer_model(input_ids)[-1]
                if layers != 'all':
                    hidden_states = [hidden_states[l] for l in layers]
                hidden_states = [h.tolist() for h in hidden_states]
                all_embs.append(hidden_states)
                if return_tokens:
                    all_toks.append(tokens)

    if hg_gated:
        del_hg_gated_access()                              
    if return_tokens:
        return all_embs, all_toks
    else:
        return all_embs

def hgDLATKTransformerGetEmbedding(text_strings = ["hello everyone"],
                                   text_ids = [],
                                   group_ids = [],
                                   model = 'bert-large-uncased',
                                   layers = 'all',
                                   device = 'cpu',
                                   tokenizer_parallelism = False,
                                   model_max_length = None,
                                   hg_gated = False,
                                   hg_token = "",
                                   trust_remote_code = False,
                                   logging_level = 'warning',
                                    batch_size = 1,
                                    aggregations= ['mean'],
                                    return_tokens = False
                                   ):
    """
    Simple Python method for embedding text with pretained Hugging Face models using DLATK's hypercontextualized embeddings.

    Parameters
    ----------
    text_strings : list
        list of strings, each is embedded separately
    text_ids : list
        list of unique identifiers for each text_string
    group_ids : list
        list of unique identifiers for each group of text_strings
    model : str
        shortcut name for Hugging Face pretained model
        Full list https://huggingface.co/transformers/pretrained_models.html
    layers : str or list
        'all' or an integer list of layers to keep
    device : str
        name of device: 'cpu', 'gpu', or 'gpu:k' where k is a specific device number
    tokenizer_parallelism :  bool
        enable parallelism for tokenizer
    model_max_length : int
        maximum length of the tokenized text
    hg_gated : bool
        Whether the accessed model is gated
    hg_token: str
        The token generated in huggingface website
    trust_remote_code : bool
        use a model with custom code on the Huggingface Hub
    logging_level : str
        set logging level, options: critical, error, warning, info, debug
    batch_size : int
        batch size for generating embeddings
    aggregations : list
        list of aggregation methods to use for aggregating embeddings from message to group level
    return_tokens : bool
        return token embeddings and tokens along with cf embeddings. This could take a bit longer. 

    Returns
    -------
    cf_embeddings : list
        embeddings for each group of text_strings
    msg_ids_all : list
        list of unique identifiers for each message
    token_embeddings_all_grouped : list, optional
        embeddings for each token in text_strings
    tokens_all_grouped : list, optional
        tokenized version of text_strings
    """ 
    def getMessagesForCorrelFieldGroups(cfGrp, text_strings, group_ids, text_ids):
        """ Return a list of list containing [cfId, msgId, msg] for the given cfGrp """
        filtered_rows = [(group_id, text_ids[i], text_strings[i]) for i, group_id in enumerate(group_ids) if group_id in cfGrp]
        return filtered_rows

    set_logging_level(logging_level)
    set_tokenizer_parallelism(tokenizer_parallelism)
    device, device_num = get_device(device)

    config, tokenizer, transformer_model = get_model(model, hg_gated=hg_gated, hg_token=hg_token, trust_remote_code=trust_remote_code)

    if device != 'cpu': transformer_model.to(device)
        
    # check and adjust input types
    if isinstance(text_strings, str):
        text_strings = [text_strings]
    
    if layers == 'all':
        layers = list(range(transformer_model.config.num_hidden_layers))
    elif isinstance(layers, int):
        layers = [layers]
    layers = [int(i) for i in layers]
    
    embedding_generator = transformer_embeddings(modelObj=transformer_model, tokenizerObj=tokenizer, layersToKeep=layers, 
                                                aggregations=aggregations, layerAggregations=['concatenate'], 
                                                wordAggregations=['mean'], maxTokensPerSeg=255, batchSize=batch_size, 
                                                noContext=False)
    
    msgs = 0 #keeps track of the number of messages read
    if text_ids == []: text_ids = range(len(text_strings))
    assert len(text_ids) == len(text_strings), "Length of text_ids must be equal to length of text_strings"
    assert len(set(text_ids)) == len(text_ids), "text_ids must be unique"
        
    if group_ids == []:  group_ids = range(len(text_strings))
    assert len(group_ids) == len(text_strings), "Length of group_ids must be equal to length of text_strings"
    
    num_rows_per_group = Counter(group_ids)
    
    msgsInBatch = 0
    cfGroups = [[]] #holds the list of cfIds that needs to batched together 1
    #Handle None here to maximise performance. 
    for row in num_rows_per_group.items():
        cfId = row[0] #correl field id
        cfNumMsgs = row[1] #Number of messages
        if msgsInBatch + cfNumMsgs > batch_size and len(cfGroups[-1])>=1:
            cfGroups.append([])
            msgsInBatch = 0
        cfGroups[-1].append(cfId)
        msgsInBatch += cfNumMsgs

    num_cfs = 0
    cfGroups = [cfGrp for cfGrp in cfGroups if cfGrp]    
    
    msg_embeddings = []
    cf_embeddings = []
    token_embeddings_all = []
    tokens_all = []
    for cfGrp in tqdm(cfGroups):
        # mIdSeen = set() #currently seen message ids
        # mIdList = [] #only for keepMsgFeats

        # msgEmb, cfEmb  = {}, {}
        cfId_msgId_map = {}
        groupedMessageRows = {} # To be turned into List[CF IDs, List[messages]]
        messageRows = getMessagesForCorrelFieldGroups(cfGrp=cfGrp, text_strings=text_strings, group_ids=group_ids, text_ids=text_ids)
        
        for cfId, msgId, msg in messageRows:
            if cfId not in cfId_msgId_map: 
                cfId_msgId_map[cfId] = set()
                groupedMessageRows[cfId] = []
            if msgId not in cfId_msgId_map[cfId]:
                cfId_msgId_map[cfId].add(msgId)
                groupedMessageRows[cfId].append([msgId, msg])
        groupedMessageRows = [[cfId, groupedMessageRows[cfId]] for cfId in cfId_msgId_map]
            
        tokenIdsDict, (cfId_seq, msgId_seq) = embedding_generator.prepare_messages(groupedMessageRows, sent_tok_onthefly=True, noContext=False)
        if len(tokenIdsDict["input_ids"]) == 0:
            continue
        
        encSelectedLayers = embedding_generator.generate_transformer_embeddings(tokenIdsDict)

        if encSelectedLayers is None:
            continue

        msg_reps, msgIds_new, cfIds_new = embedding_generator.message_aggregate(encSelectedLayers, msgId_seq, cfId_seq)

        if all([len(cfId_msgId_map[cfId]) == 1 for cfId in cfId_msgId_map]):
            cf_reps = msg_reps
        else:
            msg_reps_dict = dict(zip([i for i in msgIds_new], [msg_reps[i] for i in range(len(msgIds_new))]))
            cf_reps, cfIds_new = embedding_generator.correl_field_aggregate(msg_reps_dict, cfId_msgId_map)
            cf_reps = [[cf_reps[i][j].rep for j in range(len(aggregations))] for i in range(len(cf_reps))]    

        msg_embeddings.extend(msg_reps)
        cf_embeddings.extend(cf_reps)
        
        if return_tokens:
            decoded_tokens = [tokenizer.convert_ids_to_tokens(input_ids) for input_ids in tokenIdsDict["input_ids"]]
            tokens_all.extend(decoded_tokens)
            token_embeddings = [encSelectedLayers[0][i, :len(decoded_tokens[i])].tolist() for i in range(len(encSelectedLayers[0]))]
            token_embeddings_all.extend(token_embeddings)
    
    msg_embeddings = [msg_embeddings[i].tolist() for i in range(len(msg_embeddings))]
    cf_embeddings = [cf_embeddings[i].tolist() if isinstance(cf_embeddings[i], np.ndarray) else cf_embeddings[i] for i in range(len(cf_embeddings))] 

    if return_tokens: 
        msg_ids_all = list(map(lambda x: x[0], msgId_seq))
        assert len(msg_ids_all) == len(token_embeddings_all) == len(tokens_all), "Length of msg_ids_all, token_embeddings_all, tokens_all must be equal"
        # Return cf_embeddings, msg ids, token embeddings and tokens.
        # But msg_ids should be a unique list (currently msg_ids_all has repeated msg_ids whenever a message is split into multiple submessages).
        # Then group token_embeddings for indices with same message_id, same for tokens

        # Group token_embeddings for indices with same message_id, same for tokens
        token_embeddings_all_grouped = {}
        tokens_all_grouped = {}
        for i in range(len(msg_ids_all)):
            if msg_ids_all[i] not in token_embeddings_all_grouped:
                token_embeddings_all_grouped[msg_ids_all[i]] = [token_embeddings_all[i]]
                tokens_all_grouped[msg_ids_all[i]] = [tokens_all[i]]
            else:
                token_embeddings_all_grouped[msg_ids_all[i]].append(token_embeddings_all[i])
                tokens_all_grouped[msg_ids_all[i]].append(tokens_all[i])
        
        msg_ids_all_grouped = sorted(token_embeddings_all_grouped.keys())
        token_embeddings_all_grouped = [token_embeddings_all_grouped[msg_id] for msg_id in msg_ids_all_grouped]
        tokens_all_grouped = [tokens_all_grouped[msg_id] for msg_id in msg_ids_all_grouped]
        return cf_embeddings, msg_ids_all_grouped, token_embeddings_all_grouped, tokens_all_grouped
    return cf_embeddings

def hgTokenizerGetTokens(text_strings,
                              model = 'bert-large-uncased',
                              max_token_to_sentence = 4,
                              device = 'cpu',
                              tokenizer_parallelism = False,
                              model_max_length = None,
                              hg_gated = False,
                              hg_token = "",
                              trust_remote_code = False,
                              logging_level = 'warning'):
    """
    Simple Python method for embedding text with pretained Hugging Face models

    Parameters
    ----------
    text_strings : list
        list of strings, each is embedded separately
    model : str
        shortcut name for Hugging Face pretained model
        Full list https://huggingface.co/transformers/pretrained_models.html
    max_token_to_sentence : int
        maximum number of tokens in a string to handle before switching to embedding text
        sentence by sentence
    device : str
        name of device: 'cpu', 'gpu', or 'gpu:k' where k is a specific device number
    tokenizer_parallelism :  bool
        something
    model_max_length : int
        maximum length of the tokenized text
    hg_gated : bool
        Set to True if the accessed model is gated
    hg_token: str
        The token to access the gated model gen in hg website
    trust_remote_code : bool
        use a model with custom code on the Huggingface Hub
    logging_level : str
        set logging level, options: critical, error, warning, info, debug

    Returns
    -------
    all_embs : list
        embeddings for each item in text_strings
    all_toks : list, optional
        tokenized version of text_strings
    """
    try:
        set_logging_level(logging_level)
    except NameError:
        pass
    set_tokenizer_parallelism(tokenizer_parallelism)
    device, device_num = get_device(device)

    tokenizer = get_model(model, tokenizer_only=True, hg_gated=hg_gated, hg_token=hg_token, trust_remote_code=trust_remote_code)

    if device != 'cpu':
        tokenizer.to(device)

    max_tokens = tokenizer.max_len_sentences_pair

    # check and adjust input types
    if not isinstance(text_strings, list):
        text_strings = [text_strings]

    all_toks = []

    for text_string in text_strings:
        # if length of text_string is > max_token_to_sentence*4
        # embedd each sentence separately
        if len(text_string) > max_token_to_sentence*4:
            sentence_batch = [s for s in sent_tokenize(text_string)]
            if model_max_length is None:
                batch = tokenizer(sentence_batch, padding=True, truncation=True, add_special_tokens=True)
            else:
                batch = tokenizer(sentence_batch, padding=True, truncation=True, add_special_tokens=True, max_length=model_max_length)
            input_ids = torch.tensor(batch["input_ids"])

            tokens = []
            for ids in input_ids:
                tokens.extend([token for token in tokenizer.convert_ids_to_tokens(ids) if token != '[PAD]' and token != '<pad>'])
            all_toks.append(tokens)

        else:
            input_ids = tokenizer.encode(text_string, add_special_tokens=True)
            tokens = tokenizer.convert_ids_to_tokens(input_ids)

            all_toks.append(tokens)

    return all_toks


    
### EXAMPLE TEST CODE:
#if __name__   == '__main__':
#   embeddings, tokens = hgTransformerGetEmbedding("Here is one sentence.", layers=[0,10], device="gpu", logging_level="warn")
#   print(np.array(embeddings).shape)
#   print(tokens)
#
#   embeddings, tokens = hgTransformerGetEmbedding("Here is more sentences. But why is not . and , and ? indicated with SEP?", layers=[0,10], device="gpu")
#   print(np.array(embeddings).shape)
#   print(tokens)
