from huggingface_Interface3 import hgDLATKTransformerGetEmbedding
import numpy as np
import datasets
from datasets import Dataset

##########################################################################
### Test 1: Get Embeddings ###############################################
##########################################################################

def basic_DLATKGetEmbedding_test(**kwargs):
    """
    Basic test for checking if the Get Embeddings feature works
    """
    device = kwargs.pop("device", "cpu")
    batch_size = kwargs.pop("batch_size", 2)
    text_ids = kwargs.pop("text_ids", [])
    group_ids = kwargs.pop("group_ids", []) 
    
    if "text_strings" not in kwargs:
        data = datasets.load_dataset("rotten_tomatoes")
        text_strings = data['test']['text']
    else:
        text_strings = kwargs.pop("text_strings")
    return hgDLATKTransformerGetEmbedding(text_strings=text_strings, text_ids=text_ids, group_ids=group_ids, 
                                          device=device, batch_size=batch_size, **kwargs) 


# kwargs: output_dir="/data/avirinchipur/text_dummy_run/models/", seed=2023, do_train=True, do_eval=True, evaluation_strategy="epoch"    

if __name__ == "__main__":
    # data = datasets.load_dataset("rotten_tomatoes")
    # data = datasets.load_from_disk("/cronus_data/transformers_cache/datasets--rotten_tomatoes/")
    # data = Dataset.from_parquet("/cronus_data/transformers_cache/datasets--rotten_tomatoes/snapshots/aa13bc287fa6fcab6daf52f0dfb9994269ffea28/test.parquet")
    # text_strings = data['text']
    
    # group_ids = [i//2 for i in range(len(text_strings))]
    # cf_embs, msg_ids, token_embs, tokens = basic_DLATKGetEmbedding_test(text_strings=text_strings, device="cuda", batch_size=16, 
    #                                                 group_ids=group_ids, return_tokens=True)
    
    # print (np.array(cf_embs).shape)

    text_strings = ['a '*768, """Humour (British English) or humor (American English; see spelling differences) is the tendency to experiences to provoke laughter and provide amusement. The term derives from the humoral medicine of the ancient Greeks, which taught that the balance of fluids in the human body, known as humours (Latin: humor, body fluid), controlled human health and emotion.
 People of all ages and cultures respond to humour. Most people are able to experience humour—be amused, smile or laugh at something funny (such as a pun or joke)—and thus are considered to have a sense of humour. The hypothetical person lacking a sense of humour would likely find the behaviour inducing it to be inexplicable, strange, or even irrational. Though ultimately decided by personal taste, the extent to which a person finds something humorous depends on a host of variables, including geographical location, culture, maturity, level of education, intelligence and context. For example, young children may favour slapstick such as Punch and Judy puppet shows or the Tom and Jerry cartoons, whose physical nature makes it accessible to them. By contrast, more sophisticated forms of humour such as satire require an understanding of its social meaning and context, and thus tend to appeal to a more mature audience.
 Humour (British English) or humor (American English; see spelling differences) is the tendency of experiences to provoke laughter and provide amusement. The term derives from the humoral medicine of the ancient Greeks, which taught that the balance of fluids in the human body, known as humours (Latin: humor, body fluid), controlled human health and emotion.
 People of all ages and cultures respond to humour. Most people are able to experience humour—be amused, smile or laugh at something funny (such as a pun or joke)—and thus are considered to have a sense of humour. The hypothetical person lacking a sense of humour would likely find the behaviour inducing it to be inexplicable, strange, or even irrational. Though ultimately decided by personal taste, the extent to which a person finds something humorous depends on a host of variables, including geographical location, culture, maturity, level of education, intelligence and context. For example, young children may favour slapstick such as Punch and Judy puppet shows or the Tom and Jerry cartoons, whose physical nature makes it accessible to them. By contrast, more sophisticated forms of humour such as satire require an understanding of its social meaning and context, and thus tend to appeal to a more mature audience.
 Humour (British English) or humor (American English; see spelling differences) is the tendency of experiences to provoke laughter and provide amusement. The term derives from the humoral medicine of the ancient Greeks, which taught that the balance of fluids in the human body, known as humours (Latin: humor, body fluid), controlled human health and emotion.
 People of all ages and cultures respond to humour. Most people are able to experience humour—be amused, smile or laugh at something funny (such as a pun or joke)—and thus are considered to have a sense of humour. The hypothetical person lacking a sense of humour would likely find the behaviour inducing it to be inexplicable, strange, or even irrational. Though ultimately decided by personal taste, the extent to which a person finds something humorous depends on a host of variables, including geographical location, culture, maturity, level of education, intelligence and context. For example, young children may favour slapstick such as Punch and Judy puppet shows or the Tom and Jerry cartoons, whose physical nature makes it accessible to them. By contrast, more sophisticated forms of humour such as satire require an understanding of its social meaning and context, and thus tend to appeal to a more mature audience.
 """, "", " ", """Humour (British English) or humor (American English; see spelling differences) is the tendency to experiences to provoke laughter and provide amusement. The term derives from the humoral medicine of the ancient Greeks, which taught that the balance of fluids in the human body, known as humours (Latin: humor, body fluid), controlled human health and emotion.
 People of all ages and cultures respond to humour. Most people are able to experience humour—be amused, smile or laugh at something funny (such as a pun or joke)—and thus are considered to have a sense of humour. The hypothetical person lacking a sense of humour would likely find the behaviour inducing it to be inexplicable, strange, or even irrational. Though ultimately decided by personal taste, the extent to which a person finds something humorous depends on a host of variables, including geographical location, culture, maturity, level of education, intelligence and context. For example, young children may favour slapstick such as Punch and Judy puppet shows or the Tom and Jerry cartoons, whose physical nature makes it accessible to them. By contrast, more sophisticated forms of humour such as satire require an understanding of its social meaning and context, and thus tend to appeal to a more mature audience.
 Humour (British English) or humor (American English; see spelling differences) is the tendency of experiences to provoke laughter and provide amusement. The term derives from the humoral medicine of the ancient Greeks, which taught that the balance of fluids in the human body, known as humours (Latin: humor, body fluid), controlled human health and emotion.
 People of all ages and cultures respond to humour. Most people are able to experience humour—be amused, smile or laugh at something funny (such as a pun or joke)—and thus are considered to have a sense of humour. The hypothetical person lacking a sense of humour would likely find the behaviour inducing it to be inexplicable, strange, or even irrational. Though ultimately decided by personal taste, the extent to which a person finds something humorous depends on a host of variables, including geographical location, culture, maturity, level of education, intelligence and context. For example, young children may favour slapstick such as Punch and Judy puppet shows or the Tom and Jerry cartoons, whose physical nature makes it accessible to them. By contrast, more sophisticated forms of humour such as satire require an understanding of its social meaning and context, and thus tend to appeal to a more mature audience.
 Humour (British English) or humor (American English; see spelling differences) is the tendency of experiences to provoke laughter and provide amusement. The term derives from the humoral medicine of the ancient Greeks, which taught that the balance of fluids in the human body, known as humours (Latin: humor, body fluid), controlled human health and emotion.
 People of all ages and cultures respond to humour. Most people are able to experience humour—be amused, smile or laugh at something funny (such as a pun or joke)—and thus are considered to have a sense of humour. The hypothetical person lacking a sense of humour would likely find the behaviour inducing it to be inexplicable, strange, or even irrational. Though ultimately decided by personal taste, the extent to which a person finds something humorous depends on a host of variables, including geographical location, culture, maturity, level of education, intelligence and context. For example, young children may favour slapstick such as Punch and Judy puppet shows or the Tom and Jerry cartoons, whose physical nature makes it accessible to them. By contrast, more sophisticated forms of humour such as satire require an understanding of its social meaning and context, and thus tend to appeal to a more mature audience.
 """]
    group_ids = [i//2 for i in range(len(text_strings))]
    text_ids = [i for i in range(len(text_strings))]
    cf_embs, msg_ids, token_embs, tokens = basic_DLATKGetEmbedding_test(text_strings=text_strings, device="cuda", batch_size=16, 
                                                    group_ids=group_ids, return_tokens=True, text_ids=text_ids)
    print (np.array(cf_embs).shape)
    print (len(msg_ids))
    print (len(token_embs))
    print (len(tokens))
    import pdb; pdb.set_trace()