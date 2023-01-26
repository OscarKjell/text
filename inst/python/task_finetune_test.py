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
    text_outcome_df = pd.DataFrame(columns = ['idx', 'text', 'label'])
    text_outcome_df['idx'] = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
    text_outcome_df['text'] = ['This is a text', 'This is another text', 'This is a third text', 'This is a fourth text', 'This is a fifth text', 'This is a sixth text', 'This is a seventh text', 'This is an eighth text', 'This is a ninth text', 'This is a tenth text'] 
    text_outcome_df['label'] = ["y", "n", "i", "y", "n", "i", "y", "n", "i", "y"]
    label_names = ["y", "n", "i"]

    # Split this dataframe into 4, 3 and 3 for training, validation and testing
    text_outcome_df_train = text_outcome_df.iloc[0:4]
    text_outcome_df_val = text_outcome_df.iloc[4:7]
    text_outcome_df_test = text_outcome_df.iloc[7:10]

    json_path = './args2.json'
    is_regression = False
    model_name_or_path = 'roberta-large'
    num_epochs = 5
    hgTransformerFineTune(json_path, text_outcome_df_train, text_outcome_df_val, text_outcome_df_test, is_regression, label_names, 
                            model_name_or_path=model_name_or_path, num_epochs=num_epochs)

##########################################################################
### Test 3: Regression Task on real dataset ##############################
##########################################################################

def real_reg_test(path_to_csv="/data1/avirinchipur/resp_form_PHQtot.csv"):
    
    # Read task data from the path and split it into train, validation and test
    data = pd.read_csv(path_to_csv)
    
    # Random split of data into train, validation and test with 60%, 20% and 20% split
    train_data, val_data, test_data = np.split(data.sample(frac=1), [int(.6*len(data)), int(.8*len(data))])
    
    # Print the shape of the data
    print ("-------------------------------------")
    print ("Train data shape: ", train_data.shape)
    print ("Validation data shape: ", val_data.shape)
    print ("Test data shape: ", test_data.shape)
    print ("-------------------------------------")
     
    
    json_path = './args2.json'
    is_regression = True
    model_name_or_path = 'roberta-base'
    num_epochs = 10
    hgTransformerFineTune(json_path, train_data, val_data, test_data, is_regression, label_names=None,
                            model_name_or_path=model_name_or_path, num_epochs=num_epochs)

##########################################################################

if __name__ == "__main__":
    real_reg_test()