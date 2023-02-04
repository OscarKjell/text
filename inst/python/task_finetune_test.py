from huggingface_Interface4 import hgTransformerFineTune
import pandas as pd
import numpy as np

##########################################################################
### Test 1: Regression task ##############################################
##########################################################################

def basic_reg_test():
    """
    Basic test for checking if the regression training feature works
    """
    # Create a dataframe with text and outcome variables
    # Populate with 10 entries with label as floating point numbers
    text_outcome_df = pd.DataFrame(columns = ['idx', 'text', 'label'])
    text_outcome_df['idx'] = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
    text_outcome_df['text'] = ['This is a text', 'This is another text', 'This is a third text', 'This is a fourth text', 'This is a fifth text', 'This is a sixth text', 'This is a seventh text', 'This is an eighth text', 'This is a ninth text', 'This is a tenth text']
    text_outcome_df['label'] = [1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0]

    # Split this dataframe into 4, 3 and 3 for training, validation and testing
    text_outcome_df_train = text_outcome_df.iloc[0:4]
    text_outcome_df_val = text_outcome_df.iloc[4:7]
    text_outcome_df_test = text_outcome_df.iloc[7:10]

    json_path = './args2.json'
    is_regression = True
    hgTransformerFineTune(json_path, text_outcome_df_train, text_outcome_df_val, text_outcome_df_test, is_regression)

##########################################################################
### Test 2: Classification task ##########################################
##########################################################################

def basic_cls_test():
    """
    Basic test for checking if the classification training feature works
    """
    # Create a dataframe with text and outcome variables
    # Populate with 10 entries with class labels between 0-2
#    text_outcome_df = pd.DataFrame(columns = ['idx', 'text', 'label'])
#    text_outcome_df['idx'] = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
#    text_outcome_df['text'] = ['This is a text', 'This is another text', 'This is a third text', 'This is a fourth text', 'This is a fifth text', 'This is a sixth text', 'This is a seventh text', 'This is an eighth text', 'This is a ninth text', 'This is a tenth text'] 
#    text_outcome_df['label'] = ["y", "n", "i", "y", "n", "i", "y", "n", "i", "y"]
#    label_names = ["y", "n", "i"]
    
    text_outcome_df = pd.read_csv("/Users/oscarkjell/Desktop/1 Projects/0 Research/0 text r-package/text/dep_all_text_50_gender.csv")
    # Split this dataframe into 4, 3 and 3 for training, validation and testing
    text_outcome_df_train = text_outcome_df.iloc[0:31]
    text_outcome_df_val = text_outcome_df.iloc[31:41]
    text_outcome_df_test = text_outcome_df.iloc[41:50]
    label_names = ["male", "female"]
    
    # Print the shape of the data
#    print ("-------------------------------------*1*")
#    print ("Train data shape: ", text_outcome_df_train.shape)
#    print ("Validation data shape: ", text_outcome_df_val.shape)
#    print ("Test data shape: ", text_outcome_df_test.shape)
#    print ("Test data 1: ", text_outcome_df_test)
#    print ("-------------------------------------**")
#    text_outcome_df['text'] = text_outcome_df['text'].astype(str).apply(lambda x: x.encode('UTF-8'))
#    text_outcome_df_val['text'] = text_outcome_df_val['text'].astype(str).apply(lambda x: x.encode('UTF-8'))
#    text_outcome_df_test['text'] = text_outcome_df_test['text'].astype(str).apply(lambda x: x.encode('UTF-8'))
#    
#    text_outcome_df['label'] = text_outcome_df['label'].astype(str).apply(lambda x: x.encode('UTF-8'))
#    text_outcome_df_val['label'] = text_outcome_df_val['label'].astype(str).apply(lambda x: x.encode('UTF-8'))
#    text_outcome_df_test['label'] = text_outcome_df_test['label'].astype(str).apply(lambda x: x.encode('UTF-8'))

    #print(text_outcome_df["text"].iloc[1].encoding)
    #print(text_outcome_df["label"].encoding)
#    print("***************")
#    print (type(label_names))
#    print (label_names)
#    print("***************")
#    print (text_outcome_df.dtypes)
#    #print (text_outcome_df_val.dtypes)
#    #print (text_outcome_df_test.dtypes)
#    print("***************")
#    print (text_outcome_df)
#    #print (text_outcome_df_val)
#    #print (text_outcome_df_test)
#    print("***************")
#    print (all([isinstance(l, str) for l in label_names]))
    #print (text_outcome_df.isnull().sum(0))
    #print (text_outcome_df_val.isnull().sum(0))
    #print (text_outcome_df_test.isnull().sum(0))
    #print ((text_outcome_df.text.str.len()>0).sum())
    #print ((text_outcome_df_val.text.str.len()>0).sum())
    #print ((text_outcome_df_test.text.str.len()>0).sum())
    #exit()

#    json_path = './args2.json'
    json_path = '/Users/oscarkjell/Desktop/1 Projects/0 Research/0 text r-package/text/inst/python/args2.json'
    is_regression = False
    model_name_or_path = 'roberta-large'
    num_epochs = 2
    hgTransformerFineTune(json_path, text_outcome_df_train, text_outcome_df_val, text_outcome_df_test, is_regression, label_names, 
                            model_name_or_path=model_name_or_path, num_epochs=num_epochs)

##########################################################################
### Test 3: Regression Task on real dataset ##############################
##########################################################################

def real_reg_test(path_to_csv="/data1/avirinchipur/r-text-data/resp_form_PHQtot.csv"):
    
    # Read task data from the path and split it into train, validation and test
    #data = pd.read_csv(path_to_csv)
    
    # Random split of data into train, validation and test with 60%, 20% and 20% split
    #train_data, val_data, test_data = np.split(data.sample(frac=1), [int(.6*len(data)), int(.8*len(data))])
    
    #train_data = pd.read_csv("/Users/oscarkjell/Desktop/1 Projects/0 Research/0 text r-package/text/text_outcome_df.csv")
    #val_data = pd.read_csv("/Users/oscarkjell/Desktop/1 Projects/0 Research/0 text r-package/text/text_outcome_df_val.csv")
    #test_data = pd.read_csv("/Users/oscarkjell/Desktop/1 Projects/0 Research/0 text r-package/text/text_outcome_df_test.csv")
  
      
    # Print the shape of the data
    print ("-------------------------------------*5*")
    print ("Train data shape: ", train_data.shape)
    print ("Validation data shape: ", val_data.shape)
    print ("Test data shape: ", test_data.shape)
    print ("Test data: ", test_data)
    print ("-------------------------------------**")
    
    #json_path = './args2.json'
    json_path = '/Users/oscarkjell/Desktop/1 Projects/0 Research/0 text r-package/text/inst/python/args2.json'
    is_regression = True
    model_name_or_path = 'roberta-base'
    num_epochs = 2
    hgTransformerFineTune(json_path, train_data, val_data, test_data, is_regression, #label_names=None,
                            model_name_or_path=model_name_or_path, num_train_epochs=num_epochs)

##########################################################################

def real_cls_test(path_to_csv="/data1/avirinchipur/r-text-data/dep_all_text_956_gender_processed.csv"):
    
    # Read task data from the path and split it into train, validation and test
    data = pd.read_csv(path_to_csv)
    
    # Random split of data into train, validation and test with 60%, 20% and 20% split
    train_data, val_data, test_data = np.split(data.sample(frac=1), [int(.6*len(data)), int(.8*len(data))])
    label_names = ["male", "female"]
     
    # Print the shape of the data
    print ("-------------------------------------")
    print ("Train data shape: ", train_data.shape)
    print ("Validation data shape: ", val_data.shape)
    print ("Test data shape: ", test_data.shape)
    print ("-------------------------------------")
     
    
    json_path = './args2.json'
    is_regression = False
    model_name_or_path = 'roberta-base'
    num_train_epochs = 10
    output_dir = "./runs/trial3/"
    logging_strategy = "epoch"
    evaluation_strategy = "epoch"
    hgTransformerFineTune(json_path, train_data, val_data, test_data, is_regression, label_names=label_names,
                            model_name_or_path=model_name_or_path, num_train_epochs=num_train_epochs, output_dir=output_dir, logging_strategy=logging_strategy, evaluation_strategy=evaluation_strategy)
    
##########################################################################

if __name__ == "__main__":
    real_cls_test()
#    real_reg_test()
