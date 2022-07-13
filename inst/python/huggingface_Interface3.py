#note: I think layer 0 is the input embedding.
import warnings
warnings.simplefilter(action='ignore', category=FutureWarning)

import torch
from transformers import AutoConfig, AutoModel, AutoTokenizer
from transformers.utils import logging
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
    if logging_level not in ['warn', 'warning']:
        if logging_level == "critical":
            logging.set_verbosity_critical()
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
        name of device: 'cpu', 'gpu', 'cuda', or of the form 'gpu:k' or 'cuda:k' 
        where k is a specific device number

    Returns
    -------
    device : str
        final selected device name
    device_num : int
        device number, -1 for CPU
    """
    device = device.lower()
    if not device.startswith('cpu') and not device.startswith('gpu') and not device.startswith('cuda'):
        print("device must be 'cpu', 'gpu', 'cuda', or of the form 'gpu:k' or 'cuda:k'")
        print("\twhere k is an integer value for the device")
        print("Trying CPUs")
        device = 'cpu'
    
    device_num = -1
    if device != 'cpu':
        attached = False
        if torch.cuda.is_available():
            if device == 'gpu' or device == 'cuda': 
                # assign to first gpu device number
                device = 'cuda'
                device_num = list(range(torch.cuda.device_count()))[0]
                attached = True
            else: # assign to specific gpu device number
                try:
                    device_num = int(device.split(":")[-1])
                    device = 'cuda:' + str(device_num)
                    attached = True
                except:
                    pass
        if not attached:
            print("Unable to use CUDA (GPU), using CPU")
            device = "cpu"
            device_num = -1

    return device, device_num

def get_model(model):
    """
    Get model and tokenizer from model string

    Parameters
    ----------
    model : str
        shortcut name for Hugging Face pretained model
        Full list https://huggingface.co/transformers/pretrained_models.html
    
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
        if "megatron-bert-cased" in model:
            tokenizer = BertTokenizer.from_pretrained('nvidia/megatron-bert-cased-345m')
        else:
            tokenizer = BertTokenizer.from_pretrained('nvidia/megatron-bert-uncased-345m')
        transformer_model = MegatronBertForMaskedLM.from_pretrained(model, config=config)
    else:
        config = AutoConfig.from_pretrained(model, output_hidden_states=True)
        tokenizer = AutoTokenizer.from_pretrained(model)
        transformer_model = AutoModel.from_pretrained(model, config=config)
    return config, tokenizer, transformer_model

def hgTransformerGetPipeline(text_strings,
                            task = '',
                            model = '',
                            device = 'cpu',
                            tokenizer_parallelism = False,
                            logging_level = 'warning',
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

    Returns
    -------
    sentiment_scores : list
        list of dictionaries with sentiment scores and labels
    """

    set_logging_level(logging_level)
    set_tokenizer_parallelism(tokenizer_parallelism)
    device, device_num = get_device(device)

    # check and adjust input types
    if not isinstance(text_strings, list):
        text_strings = [text_strings]

    if model:
        config, tokenizer, transformer_model = get_model(model)
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
    if task in ACCEPTED_TASKS or task.startswith("translation"):
        if task == 'question-answering':
            task_scores = task_pipeline(**kwargs)
        else:
            task_scores = task_pipeline(text_strings, **kwargs)
    else:
        print("Task {t} is not recognized".format(t=task))
    return task_scores

def hgTransformerGetTextGeneration(text_strings,
                            model = '',
                            device = 'cpu',
                            tokenizer_parallelism = False,
                            logging_level = 'warning',
                            return_tensors = False,
                            return_text = True,
                            return_full_text = True,
                            clean_up_tokenization_spaces = False,
                            prefix = '', 
                            handle_long_generation = None):
    
    generated_texts = hgTransformerGetPipeline(text_strings = text_strings,
                            task = 'text-generation',
                            model = model,
                            device = device,
                            tokenizer_parallelism = tokenizer_parallelism,
                            logging_level = logging_level,
                            return_tensors = return_tensors, 
                            return_text = return_text, 
                            return_full_text = return_full_text, 
                            clean_up_tokenization_spaces = clean_up_tokenization_spaces, 
                            prefix = prefix,
                            handle_long_generation = handle_long_generation)
    return generated_texts

def hgTransformerGetNER(text_strings,
                            model = '',
                            device = 'cpu',
                            tokenizer_parallelism = False,
                            logging_level = 'warning'):
    ner_scores = hgTransformerGetPipeline(text_strings = text_strings,
                            task = 'ner',
                            model = model,
                            device = device,
                            tokenizer_parallelism = tokenizer_parallelism,
                            logging_level = logging_level)
    return ner_scores

def hgTransformerGetSentiment(text_strings,
                            model = '',
                            device = 'cpu',
                            tokenizer_parallelism = False,
                            logging_level = 'warning',
                            top_k = 1,
                            function_to_apply = "default"):
    sentiment_scores = hgTransformerGetPipeline(text_strings = text_strings,
                            task = 'sentiment-analysis',
                            model = model,
                            device = device,
                            tokenizer_parallelism = tokenizer_parallelism,
                            logging_level = logging_level,
                            top_k = top_k,
                            function_to_apply = function_to_apply)
    return sentiment_scores

def hgTransformerGetSummarization(text_strings,
                            model = '',
                            device = 'cpu',
                            tokenizer_parallelism = False,
                            logging_level = 'warning',
                            return_text = True,
                            return_tensors = False,
                            clean_up_tokenization_spaces = False):
    summarizations = hgTransformerGetPipeline(text_strings = text_strings,
                            task = 'summarization',
                            model = model,
                            device = device,
                            tokenizer_parallelism = tokenizer_parallelism,
                            logging_level = logging_level,
                            return_text = return_text, 
                            return_tensors = return_tensors, 
                            clean_up_tokenization_spaces = clean_up_tokenization_spaces)
    return summarizations

def hgTransformerGetQA(question,
                        context,
                        model = '',
                        device = 'cpu',
                        tokenizer_parallelism = False,
                        logging_level = 'warning',
                        topk = 1,
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
                            question = question, 
                            context = context, 
                            topk = topk, 
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
                            source_lang = '',
                            target_lang = '',
                            return_tensors = False,
                            return_text = True,
                            clean_up_tokenization_spaces = False):
    task = 'translation'
    if source_lang and target_lang:
        task = "translation_{s}_to_{t}".format(s=source_lang, t=target_lang)
    translations = hgTransformerGetPipeline(text_strings = text_strings,
                            task = task,
                            model = model,
                            device = device,
                            tokenizer_parallelism = tokenizer_parallelism,
                            logging_level = logging_level,
                            src_lang = source_lang,
                            tgt_lang = target_lang,
                            return_tensors = return_tensors,
                            return_text = return_text,
                            clean_up_tokenization_spaces = clean_up_tokenization_spaces)
    return translations

def hgTransformerGetEmbedding(text_strings,
                              model = 'bert-large-uncased',
                              layers = 'all',
                              return_tokens = True,
                              max_token_to_sentence = 4,
                              device = 'cpu',
                              tokenizer_parallelism = False,
                              model_max_length = None,
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
    logging_level : str
        set logging level, options: critical, error, warning, info, debug

    Returns
    -------
    all_embs : list
        embeddings for each item in text_strings
    all_toks : list, optional
        tokenized version of text_strings
    """

    set_logging_level(logging_level)
    set_tokenizer_parallelism(tokenizer_parallelism)
    device, device_num = get_device(device)

    config, tokenizer, transformer_model = get_model(model)

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
        if len(text_string) > max_token_to_sentence*4:
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
                    tokens.extend([token for token in tokenizer.convert_ids_to_tokens(ids) if token != '[PAD]'])
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

    if return_tokens:
        return all_embs, all_toks
    else:
        return all_embs

### EXAMPLE TEST CODE:
#if __name__   == '__main__':
#   embeddings, tokens = hgTransformerGetEmbedding("Here is one sentence.", layers=[0,10], device="gpu", logging_level="warn")
#   print(np.array(embeddings).shape)
#   print(tokens)
#
#   embeddings, tokens = hgTransformerGetEmbedding("Here is more sentences. But why is not . and , and ? indicated with SEP?", layers=[0,10], device="gpu")
#   print(np.array(embeddings).shape)
#   print(tokens)
