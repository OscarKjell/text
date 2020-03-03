#hugging face and reticulate

# First installed pytorh with this command in the terminal for Mac
# pip install torch torchvision

########################################################################
########
########      textEmbed
########
########################################################################



# Select all character variables and make then UTF-8 coded, since BERT wants it that way
select_character_v_utf8 <- function(x){
  # Select all character variables
  x_characters <- dplyr::select_if(x, is.character)
  # This makes sure that all variables are UTF-8 coded, since BERT wants it that way
  x_characters <- tibble::as_tibble(purrr::map(x_characters, stringi::stri_encode, "", "UTF-8"))
}

# Set option to aggreate layers; or select "CLS"
textEmbeddingAggregation <- function(x, aggregation = "mean"){
  if(aggregation == "min"){
    min_vector <- unlist(map(x, min))
  } else if (aggregation == "max") {
    max_vector <- unlist(map(x, max))
  } else if (aggregation == "mean") {
    mean_vector <- unlist(map(x, mean))
  } else if (aggregation == "CLS"){
    CLS <- x %>%
      dplyr::filter(token_index == 1, layer_index == 1)
  }
}



#install.packages(reticulate)
#.rs.restartR()
library(reticulate)

#By default, reticulate uses the version of Python found on your PATH (i.e. Sys.which("python")).
Sys.which("python")
use_condaenv("anaconda3")

# Set the path to the Python executable file
use_python("/opt/anaconda3/bin/python3", required = T)

# Check the version of Python.
py_config()

# Run python file with hungface interface
source_python("/Users/oscar/Desktop/0 Studies/5 R statistical semantics package/text/R/huggingface_interface.py")
x  <-  "I want!"
y  <-  hgTransformerGetEmbedding(text_strings = x,
                                 pretrained_weights = 'bert-base-uncased',
                                 tokenizer_class = BertTokenizer,
                                 model_class = BertModel,
                                 layers = 'all',  #all or a list of layers to keep
                                 return_tokens = 'False') #setting does not work
y



# Create parameter to pass to Python function
x  <-  "Where are you sweet embeddings, I'm waiting for you?"
x  <-  "Ön hör vår sång"

df <- list("I want", "The summer.")
df
x  <-  "Where are you sweet embeddings, I'm waiting for you?"

x  <-  "I want!"

textEmbed <- function(x, return_tokens = 'False'){
# Select all character variables and make them UTF-8 coded, since BERT wants it that way
#x <- select_character_v_utf8(x)

# Call python created function
y  <-  hgTransformerGetEmbedding(text_strings = x,
                              pretrained_weights = 'bert-base-uncased',
                              tokenizer_class = BertTokenizer,
                              model_class = BertModel,
                              layers = 'all',  #all or a list of layers to keep
                              return_tokens = 'False') #setting does not work
y
w_embeddings  <-  as_tibble(matrix(unlist(y[1]), nrow=length(unlist(y[2]))))
token <- unlist(y[2])
output_wd <- as_tibble(cbind(token, w_embeddings))
output_wd
}
x  <-  "I want nothing"
y <- textEmbed(x, return_tokens='False')
y

y[[1]][[1]]
typeof(y)
str(y)
x  <-  "I want it now"
y <- textEmbed(x)
y



swe  <-  hgTransformerGetEmbedding(text_strings = x,
                              pretrained_weights = 'bert-base-multilingual-uncased',
                              tokenizer_class = BertTokenizer,
                              model_class = BertModel)
swe

# 1. Receive tokens!
# 2. Layers?

df <- list("I want", "The summer.")
df
df1  <-  hgTransformerGetEmbedding(text_strings = df,
                                 pretrained_weights = 'bert-base-uncased',
                                 tokenizer_class = BertTokenizer,
                                 model_class = BertModel)
df1

