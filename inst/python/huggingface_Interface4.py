import os, sys
sys.path.append('inst/python/')
from transformer_embs import transformer_embeddings
from task_finetune import main
import numpy as np
import json

BATCH_SIZE=32

def hgTransformerGetEmbedding(text_strings,
                              model = 'bert-large-uncased',
                              layers = 'all',
                              aggregations=["mean"],
                              return_tokens = True,
                              max_token_to_sentence = 4,
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
    cf_embedding_generator = transformer_embeddings(modelName=model, layersToKeep=layers, batchSize=BATCH_SIZE)
    layersToKeep = cf_embedding_generator.layersToKeep.tolist()

    totalMessages = len(text_strings)
    print (f"Total number of messages: {totalMessages}")

    lengthWarned = False
    num_cfs = 0

    text_strings = [[idx, str(x)] for idx, x in enumerate(text_strings)]
    cfRows = list(map(lambda row: row[0], text_strings))

    msgsInBatch = 0
    cfGroups = [[]] #holds the list of cfIds that needs to batched together 1
    #Handle None here to maximise performance. 
    for cfRow in cfRows:
        cfId = cfRow #correl field id
        cfNumMsgs = 1 #Number of messages
        if msgsInBatch + cfNumMsgs > BATCH_SIZE and len(cfGroups[-1])>=1:
            cfGroups.append([])
            msgsInBatch = 0
        cfGroups[-1].append(cfId)
        msgsInBatch += cfNumMsgs

    cfGroups = [cfGrp for cfGrp in cfGroups if cfGrp]
    embs = []
            
    for cfGrp in cfGroups:
        mIdSeen = set() #currently seen message ids
        mIdList = [] #only for keepMsgFeats

        groupedMessageRows = [] #List[CF IDs, List[messages]]
        for cfId in cfGrp:  
            #grab sents by messages for that correl field:
            #Some messages might be None. Handled during tokenization
            cfMsgRows = [[cfId, text_strings[cfId]]]
            groupedMessageRows.append([cfId, cfMsgRows])
        
        #TODO: write method for prepare messages
        #prepare_messages() should take into account context, no context and other possible modes.
        #prepare_messages() can parallelize the message tokenization.
        #TODO: add num_parallel_workers for above
        tokenIdsDict, (cfId_seq, msgId_seq) = cf_embedding_generator.prepare_messages(groupedMessageRows, sent_tok_onthefly=True)
        if len(tokenIdsDict["input_ids"]) == 0:
            continue
        
        #TODO: Function call to embedding generation
        encSelectedLayers = cf_embedding_generator.generate_transformer_embeddings(tokenIdsDict)

        if encSelectedLayers is None:
            continue
        
        #TODO: Function call to embedding aggregation
        cf_reps, cfIds = cf_embedding_generator.aggregate(encSelectedLayers, msgId_seq, cfId_seq)
        print ([i.shape for i in cf_reps], len(cfIds))
        print (cfIds)
        
        #sys.exit()
        
        for idx, cfId in enumerate(cfIds):
            embFeats = dict()
            for ag in aggregations:
                #Flatten the features [layer aggregations] to a single dimension.
                thisAg = eval("np."+ag+"(cf_reps[idx].reshape(cf_reps[idx].shape[0], -1), axis=0)")
                embFeats.update([(str(k)+ag[:2],  v) for (k, v) in enumerate(thisAg)])
                # query = self.qb.create_insert_query(embTableName).set_values([("group_id",str(cfId)),("feat",""),("value",""),("group_norm","")])
                embRows = [(k, float(v)) for k, v in embFeats.items()] #adds group_norm and applies freq filter
            embs.append(embRows)

        num_cfs += len(set(cfIds))
        print (num_cfs," cfs finished processing...")

    return embs

def hgTransformerFineTune(json_path, text_outcome_df, text_outcome_df_val, text_outcome_df_test, is_regression = True):
    """
    Simple Python method for embedding text with pretained Hugging Face models

    Parameters
    ----------
    text_strings : list
        list of strings, each is embedded separately
    model : str
        shortcut name for Hugging Face pretained model
        Full list https://huggingface.co/transformers/pretrained_models.html
    model_max_length : int
        maximum length of the tokenized text
    

    Returns
    -------
    
    """
    if os.path.isdir("/Users/oscarkjell/Desktop/1 Projects/0 Research/0 text r-package/text/"):
      print("ADI Hej")
    args = json.load(open(json_path))
    main(args, text_outcome_df, text_outcome_df_val, text_outcome_df_test, is_regression)
    return 
    

