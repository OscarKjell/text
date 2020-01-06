# Global settings; this is to avoid the
# note (warning) "no visible binding for global variable ‘token_index’
# https://community.rstudio.com/t/how-to-solve-no-visible-binding-for-global-variable-note/28887
utils::globalVariables(c("token_index", "layer_index",
                         "SS", "p_values.x", "p_values.y", "words", "n.x",
                         "addEqualNrNArows", "applysemrep_plot", "single_word_embeddings_df",
                         "guide_legend"))
