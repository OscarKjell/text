# Adapted from oliverguhr/deepmultilingualpunctuation
# -*- coding: utf-8 -*-

from transformers import pipeline
import re
import torch

# V0.2

# helper functions
def overlap_chunks(lst, n, stride=0):
    """Yield successive n-sized chunks from lst with stride length of overlap."""
    for i in range(0, len(lst), n-stride):
            yield lst[i:i + n]

def pipeDef(model = "oliverguhr/fullstop-punctuation-multilang-large"):
    if torch.cuda.is_available():
        return pipeline("ner",model, device=0) # aggregation_strategy="AggregationStrategy.NONE, grouped_entities=False
    else:
        return pipeline("ner",model)

# work flow
def preprocess(text):
    #remove markers except for markers in numbers 
    text = re.sub(r"(?<!\d)[.,;:!?](?!\d)","",text) 
    #todo: match acronyms https://stackoverflow.com/questions/35076016/regex-to-match-acronyms
    text = text.split()
    return text

def predict(words, pipe):
    overlap = 5
    chunk_size = int(1e5)
    if len(words) <= chunk_size:
        overlap = 0

    batches = list(overlap_chunks(words,chunk_size,overlap))

    # if the last batch is smaller than the overlap, 
    # we can just remove it
    if len(batches[-1]) <= overlap:
        batches.pop()

    tagged_words = []     
    for batch in batches:
        # use last batch completely
        if batch == batches[-1]: 
            overlap = 0
        text = " ".join(batch)
        result = pipe(text)      
        assert len(text) == result[-1]["end"], "chunk size too large, text got clipped"
                
        char_index = 0
        result_index = 0
        for word in batch[:len(batch)-overlap]:                
            char_index += len(word) + 1
            # if any subtoken of an word is labled as sentence end
            # we label the whole word as sentence end        
            label = "0"
            while result_index < len(result) and char_index > result[result_index]["end"] :
                label = result[result_index]['entity']
                score = result[result_index]['score']
                result_index += 1                        
            tagged_words.append([word,label, score])
        
    assert len(tagged_words) == len(words)
    return tagged_words

def prediction_to_text(prediction):
    result = ""
    for word, label, _ in prediction:
        result += word
        if label == "0":
            result += " "
        if label in ".,?-:":
            result += label+" "
    return result.strip()

# API
def fullstopCorrt(text):

    pipe = pipeDef()
    text = prediction_to_text(predict(preprocess(text), pipe))

    return text

"""
# V0.1
class fullstopCorrt():
    def __init__(self, model = "oliverguhr/fullstop-punctuation-multilang-large") -> None:        
        if torch.cuda.is_available():
            self.pipe = pipeline("ner",model, grouped_entities=False, device=0)
        else:
            self.pipe = pipeline("ner",model, grouped_entities=False)        

    def preprocess(self,text):
        #remove markers except for markers in numbers 
        text = re.sub(r"(?<!\d)[.,;:!?](?!\d)","",text) 
        #todo: match acronyms https://stackoverflow.com/questions/35076016/regex-to-match-acronyms
        text = text.split()
        return text

    def restore_punctuation(self,text):        
        result = self.predict(self.preprocess(text))
        return self.prediction_to_text(result)
        
    def overlap_chunks(self,lst, n, stride=0):
        #Yield successive n-sized chunks from lst with stride length of overlap.#
        for i in range(0, len(lst), n-stride):
                yield lst[i:i + n]

    def predict(self,words):
        overlap = 5
        chunk_size = int(1e5)
        if len(words) <= chunk_size:
            overlap = 0

        batches = list(self.overlap_chunks(words,chunk_size,overlap))

        # if the last batch is smaller than the overlap, 
        # we can just remove it
        if len(batches[-1]) <= overlap:
            batches.pop()

        tagged_words = []     
        for batch in batches:
            # use last batch completely
            if batch == batches[-1]: 
                overlap = 0
            text = " ".join(batch)
            result = self.pipe(text)      
            assert len(text) == result[-1]["end"], "chunk size too large, text got clipped"
                
            char_index = 0
            result_index = 0
            for word in batch[:len(batch)-overlap]:                
                char_index += len(word) + 1
                # if any subtoken of an word is labled as sentence end
                # we label the whole word as sentence end        
                label = "0"
                while result_index < len(result) and char_index > result[result_index]["end"] :
                    label = result[result_index]['entity']
                    score = result[result_index]['score']
                    result_index += 1                        
                tagged_words.append([word,label, score])
        
        assert len(tagged_words) == len(words)
        return tagged_words

    def prediction_to_text(self,prediction):
        result = ""
        for word, label, _ in prediction:
            result += word
            if label == "0":
                result += " "
            if label in ".,?-:":
                result += label+" "
        return result.strip()

# model = fullstopCorrt()
# def restore2R(aStr):
#     return model.restore_punctuation(aStr)
"""

# if __name__ == "__main__":

    # # v0.2
    # text = "das , ist fies "
    # result = fullstopCorrt(text)
    # print(result)


    #v0.1
    # model = fullstopCorrt()

    # text = "das , ist fies "
    # restore add missing punctuation
    # result = model.restore_punctuation(text)
    # print(result)

    # clean_text = model.preprocess(text)
    # labled_words = model.predict(clean_text)
    # print(labled_words)


# Runtime Warnings in v0.2
# /Users/zh0832gu/Library/r-miniconda-arm64/envs/textrpp_condaenv/lib/python3.9/site-packages/transformers/pipelines/token_classification.py:135: UserWarning: `grouped_entities` is deprecated and will be removed in version v5.0.0, defaulted to `aggregation_strategy="AggregationStrategy.NONE"` instead.
#   warnings.warn(
# huggingface/tokenizers: The current process just got forked, after parallelism has already been used. Disabling parallelism to avoid deadlocks...
# To disable this warning, you can either:
# 	- Avoid using `tokenizers` before the fork if possible
# 	- Explicitly set the environment variable TOKENIZERS_PARALLELISM=(true | false)
# Warning: The `x` argument of `as_tibble.matrix()` must have unique column names if `.name_repair` is omitted as of tibble 2.0.0.
# Using compatibility `.name_repair`.