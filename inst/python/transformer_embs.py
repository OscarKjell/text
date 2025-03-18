######################
######################
#author-gh: @adithya8
#transformer embs for DLATK v2
#Previous versions:
#    v1 (core) - Andy Schwartz
#    v1.1      - Adi
#    v2 (curr) - Adi
######################
######################

import sys
from typing import Optional, List
import json
from json import loads
#from simplejson import loads
import numpy as np
from collections import namedtuple
try:
    import torch
    from torch.nn.utils.rnn import pad_sequence
    from transformers import AutoConfig, AutoTokenizer, AutoModel
    from transformers.utils import check_min_version
    check_min_version("4.18.0")
except ImportError:
    print ("transformers library not present. Install transformers (github.com/huggingface/transformers) to run transformer embeddings command")
    sys.exit()

######################
######################

"""
#Future TODO: Prune unecessary layers based on layersToKeep;
#Blocker: The model definitions are different. So the torch variable inside the nn Module that represents the transformer layers are different.  
def prune_transformer_layers(model):
    return
"""

######################
######################

class sentenceTokenizer:
    """
        NLTK sentence tokenizer
    """
    def __init__(self):
        try:
            import nltk.data
            import sys
        except ImportError:
             print("warning: unable to import nltk.tree or nltk.corpus or nltk.data")

        self.sentDetector = nltk.data.load('tokenizers/punkt/english.pickle')

    def __call__(self, messageRows):
        #Input: List of (msg Id, msg) pairs
        
        messages = list(map(lambda x: x[1], messageRows))
        parses = []
        for m_id, message in messageRows:
            if message is not None and message.strip() is not None: parses.append([m_id, self.sentDetector.tokenize(message.strip())])
        return parses
        
######################
######################
    
class textTransformerInterface:

    def __init__(self, transformerTokenizer):
        self.sentTokenizer = sentenceTokenizer()
        self.msgIdSeen = set()
        self.transformerTokenizer = transformerTokenizer
        self.maxSeqLen = self.transformerTokenizer.max_len_sentences_pair
        if self.maxSeqLen > 1e6: #Some models like Bertweet-large have been accidentally set to a very large value. This fixes that issue
            print ("MaxSeqLen is set to %d in the tokenizer object. Resetting to 500"%(self.maxSeqLen))
            self.maxSeqLen = 500 
        elif self.maxSeqLen > 1024:
            print ("MaxSeqLen is %d"%(self.maxSeqLen))
        
        self.maxSeqLen = self.maxSeqLen//2 # Divinding for "the pair of sentences DLATK embedding extraction method"  
        self.tokenizationRule = self.findRule()

    def findRule(self):
        """Finds the numner of tokens to be removed from the beginning end, [and middle] of the tokenized sequence for single [and pair inputs] to model
        """

        tokenizationRule = {"single":{"first":None, "last": None}, "pair":{"first":None, "middle":None, "last": None}}

        sample_data = ["1", "5"]
        special_tokens_map = self.transformerTokenizer.special_tokens_map
        special_tokens_id = {self.transformerTokenizer.convert_tokens_to_ids(i) for i in special_tokens_map.values()}
        
        sample_data_ids_single = self.transformerTokenizer(sample_data[0])["input_ids"]
        sample_data_tokens_single = self.transformerTokenizer.convert_tokens_to_ids(self.transformerTokenizer.tokenize(sample_data[0]))
        tokenizationRule["single"]["first"] = sample_data_ids_single.index(sample_data_tokens_single[0])
        tokenizationRule["single"]["last"] = len(sample_data_ids_single) - 1 - sample_data_ids_single.index(sample_data_tokens_single[-1])
        
        sample_data_ids_pair = self.transformerTokenizer(*sample_data)["input_ids"]
        sample_data_tokens_pair = []
        for i in sample_data:
            sample_data_tokens_pair.extend(self.transformerTokenizer.convert_tokens_to_ids(self.transformerTokenizer.tokenize(i)))
        tokenizationRule["pair"]["first"] = sample_data_ids_pair.index(sample_data_tokens_pair[0])
        tokenizationRule["pair"]["last"] = len(sample_data_ids_pair) - 1 - sample_data_ids_pair.index(sample_data_tokens_pair[-1])
        tokenizationRule["pair"]["middle"] = len(sample_data_ids_pair) - tokenizationRule["pair"]["first"] - tokenizationRule["pair"]["last"] - len(sample_data_tokens_pair)
        
        return tokenizationRule

    def context_preparation(self, groupedMessageRows, sent_tok_onthefly):
        #groupedMessageRows: List[correl field id, List[List[Message Id, message]]]
        
        if sent_tok_onthefly:
            for cfRows in groupedMessageRows:
                #cfId = cfRows[0]
                #sentTokenizer takes in lists of messageId, message pairs 
                cfRows[1] = self.sentTokenizer(cfRows[1])

        input_ids, token_type_ids, attention_mask = [], [], []
        msgId_seq, cfId_seq = [], []

        #stores the sequence of message_id corresponding to the message embeddings for applying aggregation later 
        #along with the sentence 1 and sentence 2 lengths

        for cfRows in groupedMessageRows:
            
            cfId = cfRows[0]
            #all messages from one cfId is inside cfRows[1]
            #print ("context prep for cf: %s with %s messages"%(cfId, len(cfRows[1])))
            
            for messageRow in cfRows[1]:
                message_id = messageRow[0]
                try:
                    messageSents = messageRow[1] if sent_tok_onthefly else loads(messageRow[1])
                except NameError: 
                    print ("Error: Cannot import jsonrpclib or simplejson in order to get sentences for Bert")
                    sys.exit(1)
                except json.JSONDecodeError:
                    print ("WARNING: JSONDecodeError on %s. Skipping Message"%str(messageRow))
                    continue
                except Exception as e:
                    print ("Warning: cannot load message, skipping. Exception: %s"%str(e))
                    continue

                #TODO: replace all mids with self.msgIdSeen
                if ((message_id not in self.msgIdSeen) and (len(messageSents) > 0)):
                    #msgs+=1
                    subMessages = []
                    #keep context; one submessage; subMessages: [[Msg1, Msg2...]]
                    messageSents=[self.transformerTokenizer.tokenize(sents) for sents in messageSents]
                    # messageSents: [[Msg1_tok1, Msg1_tok2...], [Msg2_tok1, Msg2_tok2...]]
                    i = 0
                    while (i < len(messageSents)):
                        if len(messageSents[i]) > self.maxSeqLen:
                            temp = [messageSents[i][j:j+self.maxSeqLen] for j in range(0, len(messageSents[i]), self.maxSeqLen)]
                            messageSents = messageSents[:i] + temp + messageSents[i+1:]
                            i += len(temp) - 1
                        i += 1
                    messageSents_ids = [self.transformerTokenizer.convert_tokens_to_ids(sents) for sents in messageSents]
                    
                    for idx in range(len(messageSents_ids)):
                        thisPair = messageSents_ids[idx:idx+2]
                        encoded = self.transformerTokenizer.prepare_for_model(*thisPair, is_split_into_words=True)
                        indexedToks = encoded['input_ids']
                        segIds = encoded['token_type_ids'] if 'token_type_ids' in encoded else None
                        
                        input_ids.append(torch.tensor(indexedToks, dtype=torch.long))
                        if 'token_type_ids' in encoded: token_type_ids.append(torch.tensor(segIds, dtype=torch.long))
                        attention_mask.append(torch.tensor([1]*len(indexedToks), dtype=torch.long))
                        if len(thisPair) > 1:
                            msgId_seq.append([message_id, len(thisPair[0]), len(thisPair[1])])
                        else:
                            msgId_seq.append([message_id, len(thisPair[0]), 0])
                        
                        cfId_seq.append(cfId)
                        
                self.msgIdSeen.add(message_id)
                
        assert len(input_ids) == len(msgId_seq) == len(cfId_seq) == len(attention_mask), "lengths of input_ids, msgId_seq, cfId_seq, attention_mask are not equal"
        return {"input_ids": input_ids, "attention_mask": attention_mask, "token_type_ids": token_type_ids}, (cfId_seq, msgId_seq) 

    def no_context_preparation(self, messageRows, sent_tok_onthefly):

        return 

######################
######################

class transformer_embeddings:
    """
    Class to retrieve transformer embeddings. Supply table name, model name, layer numbers, aggregation methods, [device numbers], [output table name]
    """
    #modelName, tokenizerName, modelClass=None, batchSize=dlac.GPU_BATCH_SIZE, aggregations = ['mean'], layersToKeep = [8,9,10,11], maxTokensPerSeg=255, noContext=True, layerAggregations = ['concatenate'], wordAggregations = ['mean'], keepMsgFeats = False, customTableName = None, valueFunc = lambda d: d
    
    def __init__(self, modelName:str, tokenizerName:str=None, layersToKeep:List=[-1, -2, -3, -4], aggregations:List=['mean'], layerAggregations:List=['mean'], wordAggregations:List=['mean'], maxTokensPerSeg=None, batchSize:int=None, noContext=True, customTableName:str=None, savePath:str=None):
        
        self.groups_processed = 0
        self.groups = [] #run get_groups()
        self.batchSize = batchSize
        self.config = AutoConfig.from_pretrained(modelName, output_hidden_states=True)
        if tokenizerName is not None:
            self.transformerTokenizer = AutoTokenizer.from_pretrained(tokenizerName)
        else:
            self.transformerTokenizer = AutoTokenizer.from_pretrained(modelName)
        #TODO: Add default cache file dir
        self.transformerModel = AutoModel.from_pretrained(modelName, config=self.config)

        #Fix for gpt2
        self.pad_token_id = self.transformerTokenizer.pad_token_id if self.transformerTokenizer.pad_token_id else 0
        self.transformerModel.eval()

        self.cuda = True
        #TODO: Turn this into dataparallel to leverage multiple GPUs
        try:
            self.transformerModel.to('cuda')
        except:
            print (" unable to use CUDA (GPU) for BERT")
            self.cuda = False
        
        layersToKeep = self.parse_layers(layersToKeep)
        self.layersToKeep = np.array(layersToKeep, dtype='int')

        self.aggTypeTuple = namedtuple('AggTypeTuple', ['aggType', 'rep'])
        self.aggregations = aggregations
        self.layerAggregations = layerAggregations
        self.wordAggregations = wordAggregations #not being used right now
        
        self.textToTokensInterface = textTransformerInterface(self.transformerTokenizer)

    def parse_layers(self, layers):
        """
            Checks the validity of the input layer number
            Turns negative layer idx into equivalent positive and sorts layer numbers in ascending order
        """
        for lyr in layers:
            if (lyr > self.transformerModel.config.num_hidden_layers):
                print ("You have supplied a layer number %s greater than the total number of layers (%s) in the model. Retry with valid layer number(s)."%(lyr, self.transformerModel.config.num_hidden_layers))
                sys.exit()

        layers = [(lyr%self.transformerModel.config.num_hidden_layers)+1 if lyr<0 else lyr for lyr in layers]
        #removing duplicate layer inputs
        layers = sorted(list(set(layers)))

        return layers

        
    def load_text(self, ):
        """
        Function to load text from the database
        """
        groups_to_process = self.groups[self.groups_processed:]

    def prepare_messages(self, groupedMessageRows, sent_tok_onthefly=False, noContext=False):

        if noContext:
            tokenIdstokensDict, (cfId_seq, msgId_seq) = self.textToTokensInterface.no_context_preparation(groupedMessageRows, sent_tok_onthefly)
        else:
            tokenIdsDict, (cfId_seq, msgId_seq) = self.textToTokensInterface.context_preparation(groupedMessageRows, sent_tok_onthefly)
            
        #print ("Len cfs/msgs: ", len(cfId_seq), len(msgId_seq))
        #print ("Num unique cfs/message Ids: ", np.unique(cfId_seq), len(set(map(lambda x: x[0], msgId_seq))))
        return tokenIdsDict, (cfId_seq, msgId_seq)

    def generate_transformer_embeddings(self, tokenIdsDict):

        #Number of Batches
        num_batches = int(np.ceil(len(tokenIdsDict["input_ids"])/float(self.batchSize)))
        encSelectLayers = []
        #print ('len(input_ids): ',len(tokens["input_ids"]))
        # print ('Num Batches:', num_batches)

        if len(tokenIdsDict["input_ids"]) == 0:
            print ("No messages in this batch!!! Message table might be consisting NULLs")
            return encSelectLayers
        
        #TODO: Check if len(messageSents) = 0, skip this and print warning
        for i in range(num_batches):
            #Padding for batch input
            input_ids_padded = pad_sequence(tokenIdsDict["input_ids"][i*self.batchSize:(i+1)*self.batchSize], batch_first = True, padding_value=self.pad_token_id)
            if len(tokenIdsDict["token_type_ids"])>0:
                token_type_ids_padded = pad_sequence(tokenIdsDict["token_type_ids"][i*self.batchSize:(i+1)*self.batchSize], batch_first = True, padding_value=0)
            attention_mask_padded = pad_sequence(tokenIdsDict["attention_mask"][i*self.batchSize:(i+1)*self.batchSize], batch_first = True, padding_value=0)

            if self.cuda:
                input_ids_padded = input_ids_padded.to('cuda') 
                if len(tokenIdsDict["token_type_ids"])>0:
                    token_type_ids_padded = token_type_ids_padded.to('cuda')
                attention_mask_padded = attention_mask_padded.to('cuda')

            input_ids_padded = input_ids_padded.long()
            if len(tokenIdsDict["token_type_ids"])>0:
                token_type_ids_padded = token_type_ids_padded.long()
            attention_mask_padded = attention_mask_padded.long()

            #print (input_ids_padded.shape, token_type_ids_padded.shape, attention_mask_padded.shape)
            encSelectLayers_temp = []
            with torch.no_grad():
                if len(tokenIdsDict["token_type_ids"])>0:
                    encAllLayers = self.transformerModel(input_ids = input_ids_padded, attention_mask = attention_mask_padded,  token_type_ids = token_type_ids_padded)
                else:
                    encAllLayers = self.transformerModel(input_ids = input_ids_padded, attention_mask = attention_mask_padded)    

                #Getting all layers output
                encAllLayers = encAllLayers[-1]
                for lyr in self.layersToKeep: #Shape: (batch_size, max Seq len, hidden dim, #layers)
                    encSelectLayers_temp.append(encAllLayers[int(lyr)].detach().cpu().numpy())

                #print(encSelectLayers[-1].shape)
                del encAllLayers, input_ids_padded, attention_mask_padded
                if len(tokenIdsDict["token_type_ids"])>0: del token_type_ids_padded

                #TODO: at regular intervals clear cache from cuda device: torch.cuda.empty_cache()
                #May be add nvsmi to fetch mem usage 

            encSelectLayers.append(np.transpose(np.array(encSelectLayers_temp),(1,2,3,0)))

        #print (f"Len of embeddings: {len(encSelectLayers)}")
        #for i in range(len(encSelectLayers)):
            #print (f"Shape {i}: {encSelectLayers[i].shape}")
            
        return encSelectLayers
    
    def submsg_aggregate(self, msg_rep, msgId_seq=None):

        #Layer aggregation followed by word aggregation
        message_rep = [] 
        for i in range(len(msg_rep)):#Iterating through messages
            sent_rep = []
            for j in range(len(msg_rep[i])): #Iterate through the submessages to apply layer aggregation. 
                sub_msg = msg_rep[i][j]
                sub_msg_lagg = []
                for lagg in self.layerAggregations:
                    if lagg == 'concatenate':
                        sub_msg_lagg.append(sub_msg) #(seq len, hidden dim, num layers)
                    else:
                        sub_msg_lagg.append(eval("np."+lagg+"(sub_msg, axis=-1)").reshape(sub_msg.shape[0], sub_msg.shape[1], 1) )#(seq len, hidden dim, 1)
                # Shape: (seq len, hidden dim, (num_layers*(concatenate==True)+(sum(other layer aggregations))))
                # Example: lagg = [mean, min, concatenate], layers = [8,9]; Shape: (seq len, hidden dim, 2 + 1 + 1)
                sub_msg_lagg_ = np.concatenate(sub_msg_lagg, axis=-1) 
                #Getting the mean of all tokens representation
                #TODO: add word agg list and do eval
                sub_msg_lagg_wagg = np.mean(sub_msg_lagg_, axis=0) #Shape: (hidden dim, lagg)
                #ReShaping: (1, hidden dim, lagg)
                sub_msg_lagg_wagg = sub_msg_lagg_wagg.reshape(1, sub_msg_lagg_wagg.shape[0], sub_msg_lagg_wagg.shape[1]) 
                #Sentence representations
                sent_rep.append(sub_msg_lagg_wagg)
            #Accumulate all the sentence representation of a message
            message_rep.append(np.mean(np.concatenate(sent_rep, axis=0), axis=0)) 

        message_rep = np.array(message_rep)
        return message_rep
    
    def correl_field_aggregate(self, msg_reps_dict, cfId_msgId_map):
        
        cf_reps, cfIds_seq = [], []
        for cfId, msgIds_set in cfId_msgId_map.items():
            temp = [msg_reps_dict[msgId] for msgId in msgIds_set if msgId in msg_reps_dict.keys()]
            if not len(temp):
                # empty list - message might be an empty space that wouldn't be embedded by LMs
                continue
            temp = np.concatenate(temp, axis=0)
            cf_rep = []
            for agg in self.aggregations:
                temp_agg = eval("np."+agg+"(temp, axis=0)")
                temp_agg_tuple = self.aggTypeTuple(agg, temp_agg)
                cf_rep.append(temp_agg_tuple)

            cf_reps.append(cf_rep)
            cfIds_seq.append(cfId)
            
        return cf_reps, cfIds_seq

    def message_aggregate(self, encSelectLayers, msgId_seq, cfId_seq):
        """Function that aggregates sub message representation into message representation

        Args:
            encSelectLayers (List): List of sub message representations returned by generate_embeddings method
            msgId_seq (List): List of message ids
            cfId_seq (List): List of correl field IDs

        Returns:
            Tuple(cf_reps:List, cfIds:List): Tuple of list of message representations and list of correl field ids
        """

        msg_rep, msgId_seq_new, cfId_seq_new = self.overlap_agg(encSelectLayers, msgId_seq, cfId_seq)

        current_firstIdx = 0   # first index of the current msg Id
        current_msgId = msgId_seq_new[current_firstIdx] # current msg Id
        msg_reps, msgIds = [], []
        for idx, msgId in enumerate(msgId_seq_new):
            if current_msgId != msgId:
                msg_reps.append(self.submsg_aggregate(msg_rep[current_firstIdx:idx]))
                msgIds.append(current_msgId)
                current_msgId = msgId_seq_new[idx]
                current_firstIdx = idx
                if idx == len(msgId_seq_new)-1:
                    msg_reps.append(self.submsg_aggregate(msg_rep[current_firstIdx:]))
                    msgIds.append(current_msgId)

        return (msg_reps, msgIds, cfId_seq_new)

    def overlap_agg(self, encSelectLayers, msgId_seq, cfId_seq):
        """Function that aggregates the representation of the same sub message from left and right context 

        Args:
            encSelectLayers (List): List of sub message representations returned by generate_embeddings method
            msgId_seq (List): List of message ids
            cfId_seq (List): List of correl field IDs

        Returns:
            Tuple(msg_rep:List, msgId_seq:List, cfId_seq:List): Returns the aggregated sub message representation along with the corresponding message ids and correl field ids
        """

        singleSent_start_idx = self.textToTokensInterface.tokenizationRule['single']['first']
        pairSents_start_idx = self.textToTokensInterface.tokenizationRule['pair']['first']
        pairSents_mid_idx = self.textToTokensInterface.tokenizationRule['pair']['middle']
        i = 0
        msg_rep = [] #Shape: (num layers, seq Len, hidden_dim)
        msgId_seq_new = []
        cfId_seq_new = []
        while i < len(msgId_seq):
            # Does next embedding also pertain to the same message
            if i == len(msgId_seq)-1:
                next_msg_same = False
            else:
                next_msg_same = True if msgId_seq[i][0] == msgId_seq[i+1][0] else False
            if next_msg_same:
                # Process all sub messages pertaining to a single message at once
                msg_rep_temp = []
                # Store the first message's first sentence embedding followed by averaging the second sentence of that message and the first sentence of next message's embedding   
                msg_rep_temp.append(encSelectLayers[i//self.batchSize][i%self.batchSize, pairSents_start_idx+1:pairSents_start_idx+msgId_seq[i][1]+1])
                while next_msg_same:
                    enc_batch_number, next_enc_batch_number = i//self.batchSize, (i+1)//self.batchSize 
                    seq_len1, seq_len2 = msgId_seq[i][1], msgId_seq[i][2]
                    #Shape: (seq2 len, hidden dim, num layers)
                    #Apply mean for the embedding of the sentence that appeared second in the current part and first in the next part
                    #print (msgId_seq[i][0], seq_len1, seq_len2, encSelectLayers[enc_batch_number].shape, encSelectLayers[next_enc_batch_number].shape)
                    sent2enc = (encSelectLayers[enc_batch_number][i%self.batchSize, pairSents_start_idx+seq_len1+pairSents_mid_idx+1:seq_len1+pairSents_start_idx+pairSents_mid_idx+1+seq_len2] + encSelectLayers[next_enc_batch_number][(i+1)%self.batchSize, pairSents_start_idx+1:pairSents_start_idx+1+seq_len2])/2
                    #Store the representation
                    msg_rep_temp.append(sent2enc)
                    #print (message_id_seq[i], sent2enc.shape) #debug
                    #Check if the next part of the message has single sentence, if yes, break
                    i+=1
                    if msgId_seq[i][2] == 0:
                        i+=1
                        break
                    next_msg_same = True if (msgId_seq[i][0] == msgId_seq[i+1][0]) else False
            else:
                # Single message representations.
                enc_batch_number = i//self.batchSize
                seq_len = msgId_seq[i][1] # msgId_seq[i][1]
                #Store the message representation
                #print (msgId_seq[i][0], enc_batch_number, i, j, seq_len, len(encSelectLayers), len(msg_rep))
                try:
                    sent2enc = encSelectLayers[enc_batch_number][i%self.batchSize, singleSent_start_idx+1:singleSent_start_idx+seq_len+1] #Shape: (seq len, hidden dim, #layers)
                except:
                    print (encSelectLayers[enc_batch_number].shape, i%self.batchSize, seq_len, msgId_seq[i][0])
                    exit()
                i+=1
                msg_rep_temp = [sent2enc]

            #Store all the representation as a list                
            msg_rep.append(msg_rep_temp)
            msgId_seq_new.append(msgId_seq[i-1][0])
            cfId_seq_new.append(cfId_seq[i-1])

        if not (len(msg_rep) == len(msgId_seq_new) == len(cfId_seq_new)):
            print ("----------------------------------------------------------------")
            print ("not equal: ", len(msg_rep), len(msgId_seq_new), len(cfId_seq_new))
            print ("----------------------------------------------------------------")
                
        return msg_rep, msgId_seq_new, cfId_seq_new

    def layer_level_agg(self,):
        return 

    def group_level_agg(self,):
        return

    def save_embeddings(self):
        return

######################
######################
