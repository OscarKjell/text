#hugging face and reticulate

# First installed pytorh with this command in the terminal for Mac
# pip install torch torchvision

#install.packages(reticulate)
#.rs.restartR()
library(reticulate)

#By default, reticulate uses the version of Python found on your PATH (i.e. Sys.which("python")).
Sys.which("python")

use_condaenv("anaconda3")
# Set the path to the Python executable file
#use_python("/Users/oscar/PycharmProjects/VeryFirstTest", required = T)
use_python("/opt/anaconda3/bin/python3", required = T)

# Check the version of Python.
py_config()

setwd("/Users/oscar/Desktop/0_Studies/5 R statistical semantics package/")
source_python("huggingface_basic.py")
source_python("huggingface_interface.py")

# Create parameter to pass to Python function
x  <-  "Where are you sweet embeddings, I'm waiting for you?"
x  <-  "Ön hör vår sång"

# Call created function
y  <-  hgTransformerGetEmbedding(text_strings = x,
                              pretrained_weights = 'bert-base-uncased',
                              tokenizer_class = BertTokenizer,
                              model_class = BertModel)
y
y[[1]][[1]]

swe  <-  hgTransformerGetEmbedding(text_strings = x,
                              pretrained_weights = 'bert-base-multilingual-uncased',
                              tokenizer_class = BertTokenizer,
                              model_class = BertModel)
swe

typeof(y)
str(y)

# 1. Receive tokens!
# 2. Layers?

df <- list("I want", "The summer.")
df
df1  <-  hgTransformerGetEmbedding(text_strings = df,
                                 pretrained_weights = 'bert-base-uncased',
                                 tokenizer_class = BertTokenizer,
                                 model_class = BertModel)
df1

