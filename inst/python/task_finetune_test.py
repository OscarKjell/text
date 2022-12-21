from huggingface_Interface4 import hgTransformerFineTune
import pandas as pd

##########################################################################
### Test 1: Regression task ##############################################
##########################################################################

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

from huggingface_Interface4 import hgTransformerFineTune
import pandas as pd

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
hgTransformerFineTune(json_path, text_outcome_df_train, text_outcome_df_val, text_outcome_df_test, is_regression, label_names)
