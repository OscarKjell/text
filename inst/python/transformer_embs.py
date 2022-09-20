######################
######################
#author-gh: @adithya8
#transformer embs for DLATK v2
######################
######################

import sys
from typing import Optional, List
import json
from json import loads
#from simplejson import loads
import numpy as np
try:
    import torch
    from torch.nn.utils.rnn import pad_sequence
    from transformers import AutoConfig, AutoTokenizer, AutoModel
    #TODO: assert transformer minimum version
except ImportError:
    print ("transformers library not present. Install transformers (github.com/huggingface/transformers) to run transformer embeddings command")
    sys.exit()

#from . import textCleaner as tc

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
            if message is not None:
              parses.append([m_id, json.dumps(self.sentDetector.tokenize(message[1].strip()))])
        return parses
        
######################
######################
    
class textTransformerInterface:

    def __init__(self, transformerTokenizer):
        self.sentTokenizer = sentenceTokenizer()
        self.msgIdSeen = set()
        self.transformerTokenizer = transformerTokenizer
        self.maxTokensPerSeg = self.transformerTokenizer.max_len_sentences_pair//2

        self.tokenizationRule = self.findRule()

    def findRule(self):

        tokenizationRule = {"single":{}, "pair":{}}

        sample_data = ["1 2 3 4", "5 6 7 8 9"]
        special_tokens_map = self.transformerTokenizer.special_tokens_map
        special_tokens_id = {self.transformerTokenizer.convert_tokens_to_ids(i) for i in special_tokens_map.values()}
        sample_data_ids = self.transformerTokenizer(*sample_data)
        
        return

    def context_preparation(self, groupedMessageRows, sent_tok_onthefly):
        #groupedMessageRows: List[correl field id, List[List[Message Id, message]]]
        
        if sent_tok_onthefly:
            for cfRows in groupedMessageRows:
                #cfId = cfRows[0]
                #sentTokenizer takes in lists of messageId, message pairs 
                cfRows[1] = self.sentTokenizer(cfRows[1])

        input_ids = []
        token_type_ids = []
        attention_mask = []
        msgId_seq = []
        cfId_seq = []

        #stores the sequence of message_id corresponding to the message embeddings for applying aggregation later 
        #along with the sentence 1 and sentence 2 lengths

        for cfRows in groupedMessageRows:
            
            cfId = cfRows[0]
            #all messages from one cfId is inside cfRows[1]
            #print ("context prep for cf: %s with %s messages"%(cfId, len(cfRows[1])))
            
            for messageRow in cfRows[1]:
                message_id = messageRow[0]
                try:
                    messageSents = loads(messageRow[1])
                except NameError: 
                    print ("Error: Cannot import jsonrpclib or simplejson in order to get sentences for Bert")
                    sys.exit(1)
                except json.JSONDecodeError:
                    print ("WARNING: JSONDecodeError on %s. Skipping Message"%str(messageRow))
                    continue
                except:
                    print ("Warning: cannot load message, skipping")
                    continue

                #TODO: replace all mids with self.msgIdSeen
                if ((message_id not in self.msgIdSeen) and (len(messageSents) > 0)):
                    #msgs+=1
                    subMessages = []
                    #keep context; one submessage; subMessages: [[Msg1, Msg2...]]
                    subMessages=[messageSents]

                    for sents in subMessages: #only matters for noContext
                        #TODO: preprocess to remove newlines
                        sentsTok = [self.transformerTokenizer.tokenize(s) for s in sents]
                        #print(sentsTok)#debug
                        #check for over the max length. Below code (loop) segments the messages based on the number of tokens
                        i = 0
                        while (i < len(sentsTok)):#while instead of for since array may change size
                            if len(sentsTok[i]) > self.maxTokensPerSeg: #If the number of tokens is greater than maxTokenPerSeg in a Sentence, split it
                                newSegs = [sentsTok[i][j:j+self.maxTokensPerSeg] for j in range(0, len(sentsTok[i]), self.maxTokensPerSeg)]
                                #TODO: Change this to 1 update per user
                                #if not lengthWarned:
                                    #print ("AddEmb: Some segments are too long; splitting up; first example: %s" % str(newSegs))
                                    #lengthWarned = True
                                sentsTok = sentsTok[:i] + newSegs + sentsTok[i+1:]
                                i+=(len(newSegs) - 1)#skip ahead new segments
                            i+=1

                        for i in range(len(sentsTok)):
                            thisPair = sentsTok[i:i+2] #Give two sequences as input
                            try:
                                #thisPair = [self.transformerTokenizer.convert_tokens_to_string(i) for i in thisPair]
                                encoded = self.transformerTokenizer.prepare_for_model(*[self.transformerTokenizer.convert_tokens_to_ids(i) for i in thisPair])
                                #encoded = self.transformerTokenizer.encode_plus(thisPair[0], thisPair[1]) if len(thisPair)>1 else self.transformerTokenizer.encode_plus(thisPair[0])
                            except:
                                #print(thisPair, message_id)
                                print ("Message pair/ message unreadable. Skipping this....")
                                continue
                                #sys.exit(0)
                            
                            indexedToks = encoded['input_ids']
                            segIds = encoded['token_type_ids'] if 'token_type_ids' in encoded else None

                            input_ids.append(torch.tensor(indexedToks, dtype=torch.long))
                            if 'token_type_ids' in encoded:
                                token_type_ids.append(torch.tensor(segIds, dtype=torch.long))
                            attention_mask.append(torch.tensor([1]*len(indexedToks), dtype=torch.long))

                            if len(thisPair)>1: #Collecting the sentence length of the pair along with their message IDs
                                # If multiple sentences in a message, it will store the message_ids multiple times for aggregating emb later.
                                msgId_seq.append([message_id, len(sentsTok[i]), len(sentsTok[i+1])])
                            else:
                                msgId_seq.append([message_id, len(sentsTok[i]), 0])

                            cfId_seq.append(cfId)

            #TODO move this to outer call ofor handling progress from DLATK
                #if msgs % int(dlac.PROGRESS_AFTER_ROWS/5) == 0: #progress update
                #    dlac.warn("Messages Read: %.2f k" % (msgs/1000.0))
                self.msgIdSeen.add(message_id)
            #midList.append(message_id)

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
        self.batchSize=batchSize
        
        layersToKeep = self.parse_layers(layersToKeep)
        self.layersToKeep = np.array(layersToKeep, dtype='int')

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
                print (f"You have supplied a layer number ({lyr}) greater than the total number of layers ({self.transformerModel.config.num_hidden_layers}) in the model. Retry with valid layer number(s).")
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
        print ('Num Batches:', num_batches)

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
    
    def _aggregate(self, msg_rep, msgId_seq=None):

        #msg_rep = self.msg_level_agg(encSelectLayers, msgId_seq)

        #Layer aggregation followed by word aggregation
        user_rep = [] #(num msgs, hidden_dim, lagg)
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
                    #Shape: (seq len, hidden dim, (num_layers*(concatenate==True)+(sum(other layer aggregations))))
                    #Example: lagg = [mean, min, concatenate], layers = [8,9]; Shape: (seq len, hidden dim, 2 + 1 + 1)
                    sub_msg_lagg_ = np.concatenate(sub_msg_lagg, axis=-1) 
                #Getting the mean of all tokens representation
                #TODO: add word agg list and do eval
                sub_msg_lagg_wagg = np.mean(sub_msg_lagg_, axis=0) #Shape: (hidden dim, lagg)
                #ReShaping: (1, hidden dim, lagg)
                sub_msg_lagg_wagg = sub_msg_lagg_wagg.reshape(1, sub_msg_lagg_wagg.shape[0], sub_msg_lagg_wagg.shape[1]) 
                #Sentence representations
                sent_rep.append(sub_msg_lagg_wagg)
            #Accumulate all the sentence representation of a user
            user_rep.append(np.mean(np.concatenate(sent_rep, axis=0), axis=0)) 

        user_rep = np.array(user_rep)
        
        return user_rep

    def aggregate(self, encSelectLayers, msgId_seq, cfId_seq):

        msg_rep, msgId_seq, cfId_seq = self.msg_level_agg(encSelectLayers, msgId_seq, cfId_seq)

        current_cfId = cfId_seq[0]
        current_firstIdx = 0
        cf_reps = []
        cfIds = []
        for idx, cfId in enumerate(cfId_seq):
            if idx == len(cfId_seq)-1:
                cf_reps.append(self._aggregate(msg_rep[current_firstIdx:idx+1]))
                cfIds.append(current_cfId)
            elif current_cfId != cfId:
                cf_reps.append(self._aggregate(msg_rep[current_firstIdx:idx]))
                cfIds.append(current_cfId)
                current_cfId = cfId_seq[idx]
                current_firstIdx = idx

        return (cf_reps, cfIds)

    def msg_level_agg(self, encSelectLayers, msgId_seq, cfId_seq):
        #Change function name to group_submsg_rep

        i = 0
        j = 0
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
                msg_rep_temp.append(encSelectLayers[i//self.batchSize][j%self.batchSize, :msgId_seq[i][1]])
                while next_msg_same:
                    enc_batch_number = i//self.batchSize
                    next_enc_batch_number = (i+1)//self.batchSize
                    seq_len1 = msgId_seq[i][1]
                    seq_len2 = msgId_seq[i][2]
                    #Shape: (seq2 len, hidden dim, num layers)
                    #Apply mean for the embedding of the sentence that appeared second in the current part and first in the next part
                    #print (msgId_seq[i][0], seq_len1, seq_len2, encSelectLayers[enc_batch_number].shape, encSelectLayers[next_enc_batch_number].shape)
                    sent2enc = (encSelectLayers[enc_batch_number][j%self.batchSize, seq_len1:seq_len1+seq_len2] + encSelectLayers[next_enc_batch_number][(j+1)%self.batchSize, :seq_len2])/2
                    #Store the representation
                    msg_rep_temp.append(sent2enc)
                    #print (message_id_seq[i], sent2enc.shape) #debug
                    #Check if the next part of the message has single sentence, if yes, break
                    i+=1
                    j+=1
                    if msgId_seq[i][2] == 0:
                        i+=1
                        j+=1
                        break
                    next_msg_same = True if (msgId_seq[i][0] == msgId_seq[i+1][0]) else False
            else:
                # Single message representations.
                enc_batch_number = i//self.batchSize
                seq_len = msgId_seq[i][1]
                #Store the message representation
                #print (msgId_seq[i][0], enc_batch_number, i, j, seq_len, len(encSelectLayers), len(msg_rep))
                sent2enc = encSelectLayers[enc_batch_number][j%self.batchSize, :seq_len] #Shape: (seq len, hidden dim, #layers)
                i+=1
                j+=1
                msg_rep_temp = [sent2enc]

            #Store all the representation as a list                
            msg_rep.append(msg_rep_temp)
            msgId_seq_new.append(msgId_seq[i-1][0])
            cfId_seq_new.append(cfId_seq[i-1])

        if not (len(msg_rep) == len(msgId_seq_new) == len(cfId_seq_new)):
            print ("not equal: ", len(msg_rep), len(msgId_seq_new), len(cfId_seq_new))
                
        return msg_rep, msgId_seq_new, cfId_seq_new

    def layer_level_agg(self,):
        return 

    def group_level_agg(self,):
        return

    def save_embeddings(self):
        return

######################
######################
