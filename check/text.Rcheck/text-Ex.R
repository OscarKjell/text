pkgname <- "text"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
base::assign(".ExTimings", "text-Ex.timings", pos = 'CheckExEnv')
base::cat("name\tuser\tsystem\telapsed\n", file=base::get(".ExTimings", pos = 'CheckExEnv'))
base::assign(".format_ptime",
function(x) {
  if(!is.na(x[4L])) x[1L] <- x[1L] + x[4L]
  if(!is.na(x[5L])) x[2L] <- x[2L] + x[5L]
  options(OutDec = '.')
  format(x[1L:3L], digits = 7L)
},
pos = 'CheckExEnv')

### * </HEADER>
library('text')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("textCentrality")
### * textCentrality

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: textCentrality
### Title: Semantic similarity score between single words' and an
###   aggregated word embeddings
### Aliases: textCentrality

### ** Examples

# Computes the semantic similarity between the individual word embeddings (Iwe)
# in the "harmonywords" column of the pre-installed dataset: Language_based_assessment_data_8,
# and the aggregated word embedding (Awe).
# The Awe can be interpreted the latent meaning of the text.

## Not run: 
##D df_for_plotting <- textCentrality(
##D   words = Language_based_assessment_data_8["harmonywords"],
##D   word_embeddings = word_embeddings_4$texts$harmonywords,
##D   word_types_embeddings = word_embeddings_4$word_types
##D )
##D 
##D # df_for_plotting contain variables (e.g., semantic similarity, frequencies) for
##D # the individual words that are used for plotting by the textCentralityPlot function.
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("textCentrality", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("textCentralityPlot")
### * textCentralityPlot

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: textCentralityPlot
### Title: Plots words from textCentrality()
### Aliases: textCentralityPlot

### ** Examples

# Plot a centrality plot from the dataframe df_for_plotting
# that is returned by the textCentrality function.
## Not run: 
##D textCentralityPlot(
##D   df_for_plotting,
##D   min_freq_words_test = 1,
##D   plot_n_word_extreme = 10,
##D   plot_n_word_frequency = 10,
##D   plot_n_words_middle = 10,
##D   titles_color = "#61605e",
##D   x_axes = "central_semantic_similarity",
##D   title_top = "Semantic Centrality Plot",
##D   x_axes_label = "Semantic Centrality",
##D   scale_x_axes_lim = NULL,
##D   scale_y_axes_lim = NULL,
##D   word_font = NULL,
##D   centrality_color_codes = c("#EAEAEA", "#85DB8E", "#398CF9", "#9e9d9d"),
##D   word_size_range = c(3, 8),
##D   position_jitter_hight = 0,
##D   position_jitter_width = 0.03,
##D   point_size = 0.5,
##D   arrow_transparency = 0.1,
##D   points_without_words_size = 0.5,
##D   points_without_words_alpha = 0.5,
##D   legend_title = "SC",
##D   legend_x_axes_label = "x",
##D   legend_x_position = 0.02,
##D   legend_y_position = 0.02,
##D   legend_h_size = 0.2,
##D   legend_w_size = 0.2,
##D   legend_title_size = 7,
##D   legend_number_size = 2,
##D   seed = 1007
##D )
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("textCentralityPlot", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("textDescriptives")
### * textDescriptives

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: textDescriptives
### Title: Compute descriptive statistics of character variables.
### Aliases: textDescriptives

### ** Examples

## Not run: 
##D textDescriptives(Language_based_assessment_data_8[1:2])
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("textDescriptives", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("textDimName")
### * textDimName

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: textDimName
### Title: Change dimension names
### Aliases: textDimName

### ** Examples

## No test: 
# Note that dimensions are called Dim1_harmonytexts etc.
word_embeddings_4$texts$harmonytexts

# Change to just Dim
w_e_T <- textDimName(word_embeddings_4$texts["harmonytexts"],
  dim_names = FALSE
)

# Change back to include the original name
w_e_F <- textDimName(w_e_T, dim_names = TRUE)

# Change and add a custom name
w_e_custom <- textDimName(w_e_T, dim_names = TRUE, name = "CustomName")
## End(No test)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("textDimName", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("textDistance")
### * textDistance

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: textDistance
### Title: Semantic distance
### Aliases: textDistance

### ** Examples

# Compute the semantic distance score between the embeddings
# from "harmonytext" and "satisfactiontext".

## Not run: 
##D distance_scores <- textDistance(
##D   x = word_embeddings_4$texts$harmonytext,
##D   y = word_embeddings_4$texts$satisfactiontext
##D )
##D 
##D # Show information about how distance_scores were constructed.
##D 
##D comment(distance_scores)
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("textDistance", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("textDistanceMatrix")
### * textDistanceMatrix

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: textDistanceMatrix
### Title: Semantic distance across multiple word embeddings
### Aliases: textDistanceMatrix

### ** Examples

distance_scores <- textDistanceMatrix(word_embeddings_4$texts$harmonytext[1:3, ])
round(distance_scores, 3)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("textDistanceMatrix", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("textDistanceNorm")
### * textDistanceNorm

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: textDistanceNorm
### Title: Semantic distance between a text variable and a word norm
### Aliases: textDistanceNorm

### ** Examples

## Not run: 
##D library(dplyr)
##D library(tibble)
##D harmonynorm <- c("harmony peace ")
##D satisfactionnorm <- c("satisfaction achievement")
##D 
##D norms <- tibble::tibble(harmonynorm, satisfactionnorm)
##D word_embeddings <- word_embeddings_4$texts
##D word_embeddings_wordnorm <- textEmbed(norms)
##D similarity_scores <- textDistanceNorm(
##D   word_embeddings$harmonytext,
##D   word_embeddings_wordnorm$harmonynorm
##D )
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("textDistanceNorm", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("textDomainCompare")
### * textDomainCompare

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: textDomainCompare
### Title: Compare two language domains
### Aliases: textDomainCompare

### ** Examples

## Not run: 
##D train_language <- textTokenizeAndCount(Language_based_assessment_data_8["harmonytexts"])
##D assess_language <- textTokenizeAndCount(Language_based_assessment_data_8["satisfactiontexts"])
##D textDomainCompare(train_language, assess_language)
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("textDomainCompare", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("textEmbed")
### * textEmbed

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: textEmbed
### Title: textEmbed() extracts layers and aggregate them to word
###   embeddings, for all character variables in a given dataframe.
### Aliases: textEmbed

### ** Examples

# Automatically transforms the characters in the example dataset:
# Language_based_assessment_data_8 (included in text-package), to embeddings.
## Not run: 
##D word_embeddings <- textEmbed(Language_based_assessment_data_8[1:2, 1:2],
##D   layers = 10:11,
##D   aggregation_from_layers_to_tokens = "concatenate",
##D   aggregation_from_tokens_to_texts = "mean",
##D   aggregation_from_tokens_to_word_types = "mean"
##D )
##D 
##D # Show information about how the embeddings were constructed.
##D comment(word_embeddings$texts$satisfactiontexts)
##D comment(word_embeddings$word_types)
##D comment(word_embeddings$tokens$satisfactiontexts)
##D 
##D # See how the word embeddings are structured.
##D word_embeddings
##D 
##D # Save the word embeddings to avoid having to embed the text again.
##D saveRDS(word_embeddings, "word_embeddings.rds")
##D 
##D # Retrieve the saved word embeddings.
##D word_embeddings <- readRDS("word_embeddings.rds")
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("textEmbed", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("textEmbedLayerAggregation")
### * textEmbedLayerAggregation

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: textEmbedLayerAggregation
### Title: Aggregate layers
### Aliases: textEmbedLayerAggregation

### ** Examples

# Aggregate the hidden states from textEmbedRawLayers
# to create a word embedding representing the entire text.
# This is achieved by concatenating layer 11 and 12.
## Not run: 
##D word_embedding <- textEmbedLayerAggregation(
##D   imf_embeddings_11_12$context_tokens,
##D   layers = 11:12,
##D   aggregation_from_layers_to_tokens = "concatenate",
##D   aggregation_from_tokens_to_texts = "mean"
##D )
##D 
##D # Examine word_embedding
##D word_embedding
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("textEmbedLayerAggregation", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("textEmbedRawLayers")
### * textEmbedRawLayers

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: textEmbedRawLayers
### Title: Extract layers of hidden states
### Aliases: textEmbedRawLayers

### ** Examples

# Get hidden states of layer 11 and 12 for "I am fine".
## Not run: 
##D imf_embeddings_11_12 <- textEmbedRawLayers(
##D   "I am fine",
##D   layers = 11:12
##D )
##D 
##D # Show hidden states of layer 11 and 12.
##D imf_embeddings_11_12
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("textEmbedRawLayers", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("textEmbedReduce")
### * textEmbedReduce

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: textEmbedReduce
### Title: Pre-trained dimension reduction (experimental)
### Aliases: textEmbedReduce

### ** Examples

## Not run: 
##D embeddings <- textEmbedReduce(word_embeddings_4$texts)
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("textEmbedReduce", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("textFineTuneDomain")
### * textFineTuneDomain

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: textFineTuneDomain
### Title: Domain Adapted Pre-Training (EXPERIMENTAL - under development)
### Aliases: textFineTuneDomain

### ** Examples

## Not run: 
##D textFineTuneDomain(text_data)
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("textFineTuneDomain", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("textFineTuneTask")
### * textFineTuneTask

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: textFineTuneTask
### Title: Task Adapted Pre-Training (EXPERIMENTAL - under development)
### Aliases: textFineTuneTask

### ** Examples

## Not run: 
##D textFineTuneTask(text_outcome_data)
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("textFineTuneTask", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("textGeneration")
### * textGeneration

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: textGeneration
### Title: Text generation
### Aliases: textGeneration

### ** Examples

## No test: 
# generated_text <- textGeneration("The meaning of life is")
# generated_text
## End(No test)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("textGeneration", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("textLBAM")
### * textLBAM

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: textLBAM
### Title: The LBAM library
### Aliases: textLBAM

### ** Examples

## Not run: 
##D library(dplyr)
##D test_lbam <- textLBAM(
##D   lbam_update = TRUE
##D )
##D subset(
##D   lbam,
##D   substr(Construct_Concept_Behaviours, 1, 3) == "Dep",
##D   select = c(Construct_Concept_Behaviours, Name)
##D )
##D 
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("textLBAM", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("textModelLayers")
### * textModelLayers

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: textModelLayers
### Title: Number of layers
### Aliases: textModelLayers

### ** Examples

## Not run: 
##D textModelLayers(target_model = "bert-base-uncased")
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("textModelLayers", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("textModels")
### * textModels

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: textModels
### Title: Check downloaded, available models.
### Aliases: textModels

### ** Examples

## Not run: 
##D textModels()
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("textModels", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("textModelsRemove")
### * textModelsRemove

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: textModelsRemove
### Title: Delete a specified model
### Aliases: textModelsRemove

### ** Examples

## Not run: 
##D textModelsRemove("name-of-model-to-delete")
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("textModelsRemove", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("textNER")
### * textNER

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: textNER
### Title: Named Entity Recognition. (experimental)
### Aliases: textNER

### ** Examples

## No test: 
# ner_example <- textNER("Arnes plays football with Daniel")
# ner_example
## End(No test)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("textNER", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("textPCA")
### * textPCA

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: textPCA
### Title: textPCA()
### Aliases: textPCA

### ** Examples

## Not run: 
##D # Data
##D df_for_plotting2d <- textPCA(
##D   words = Language_based_assessment_data_8$harmonywords,
##D   word_types_embeddings = word_embeddings_4$word_types
##D )
##D df_for_plotting2d
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("textPCA", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("textPCAPlot")
### * textPCAPlot

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: textPCAPlot
### Title: textPCAPlot
### Aliases: textPCAPlot

### ** Examples

# The test-data included in the package is called: DP_projections_HILS_SWLS_100

# Supervised Dimension Projection Plot
principle_component_plot_projection <- textPCAPlot(PC_projections_satisfactionwords_40)
principle_component_plot_projection

names(DP_projections_HILS_SWLS_100)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("textPCAPlot", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("textPlot")
### * textPlot

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: textPlot
### Title: Plot words
### Aliases: textPlot

### ** Examples

# The test-data included in the package is called: DP_projections_HILS_SWLS_100

# Supervised Dimension Projection Plot
plot_projection <- textPlot(
  word_data = DP_projections_HILS_SWLS_100,
  k_n_words_to_test = FALSE,
  min_freq_words_test = 1,
  plot_n_words_square = 3,
  plot_n_words_p = 3,
  plot_n_word_extreme = 1,
  plot_n_word_frequency = 1,
  plot_n_words_middle = 1,
  y_axes = FALSE,
  p_alpha = 0.05,
  title_top = "Supervised Dimension Projection (SDP)",
  x_axes_label = "Low vs. High HILS score",
  y_axes_label = "Low vs. High SWLS score",
  p_adjust_method = "bonferroni",
  scale_y_axes_lim = NULL
)
plot_projection

names(DP_projections_HILS_SWLS_100)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("textPlot", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("textPredict")
### * textPredict

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: textPredict
### Title: textPredict, textAssess and textClassify
### Aliases: textPredict textAssess textClassify

### ** Examples

## Not run: 
##D 
##D # Text data from Language_based_assessment_data_8
##D text_to_predict <- "I am not in harmony in my life as much as I would like to be."
##D 
##D # Example 1: (predict using pre-made embeddings and an R model-object)
##D prediction1 <- textPredict(
##D   model_info = trained_model,
##D   word_embeddings_4$texts$satisfactiontexts
##D )
##D 
##D # Example 2: (predict using a pretrained github model)
##D prediction2 <- textPredict(
##D   texts = text_to_predict,
##D   model_info = "https://github.com/CarlViggo/pretrained-models/raw/main/trained_hils_model.RDS"
##D )
##D 
##D # Example 3: (predict using a pretrained logistic github model and return
##D # probabilities and classifications)
##D prediction3 <- textPredict(
##D   texts = text_to_predict,
##D   model_info = "https://github.com/CarlViggo/pretrained-models/raw/main/
##D   trained_github_model_logistic.RDS",
##D   type = "class_prob",
##D   threshold = 0.7
##D )
##D 
##D # Example 4: (predict from texts using a pretrained model stored in an osf project)
##D prediction4 <- textPredict(
##D   texts = text_to_predict,
##D   model_info = "https://osf.io/8fp7v"
##D )
##D ##### Automatic implicit motive coding section ######
##D 
##D # Create example dataset
##D implicit_motive_data <- dplyr::mutate(.data = Language_based_assessment_data_8,
##D participant_id = dplyr::row_number())
##D 
##D # Code implicit motives.
##D implicit_motives <- textPredict(
##D   texts = implicit_motive_data$satisfactiontexts,
##D   model_info = "implicit_power_roberta_large_L23_v1",
##D   participant_id = implicit_motive_data$participant_id,
##D   dataset_to_merge_assessments = implicit_motive_data
##D )
##D 
##D # Examine results
##D implicit_motives$sentence_predictions
##D implicit_motives$person_predictions
## End(Not run)

## Not run: 
##D # Examine the correlation between the predicted values and
##D # the Satisfaction with life scale score (pre-included in text).
##D 
##D psych::corr.test(
##D   predictions1$word_embeddings__ypred,
##D   Language_based_assessment_data_8$swlstotal
##D )
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("textPredict", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("textPredictAll")
### * textPredictAll

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: textPredictAll
### Title: Predict from several models, selecting the correct input
### Aliases: textPredictAll

### ** Examples

## No test: 
# x <- Language_based_assessment_data_8[1:2, 1:2]
# word_embeddings_with_layers <- textEmbedLayersOutput(x, layers = 11:12)
## End(No test)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("textPredictAll", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("textPredictTest")
### * textPredictTest

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: textPredictTest
### Title: Significance testing for model prediction performance
### Aliases: textPredictTest

### ** Examples

# Example random data
y1 <- runif(10)
yhat1 <- runif(10)
y2 <- runif(10)
yhat2 <- runif(10)

boot_test <- textPredictTest(y1, y2, yhat1, yhat2)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("textPredictTest", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("textProjection")
### * textProjection

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: textProjection
### Title: Supervised Dimension Projection
### Aliases: textProjection

### ** Examples

# Pre-processing data for plotting.
## Not run: 
##D df_for_plotting <- textProjection(
##D   words = Language_based_assessment_data_8$harmonywords,
##D   word_embeddings = word_embeddings_4$texts$harmonywords,
##D   word_types_embeddings = word_embeddings_4$word_types,
##D   x = Language_based_assessment_data_8$hilstotal,
##D   split = "mean",
##D   Npermutations = 10,
##D   n_per_split = 1
##D )
##D # Run df_for_plotting to examine result.
##D df_for_plotting
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("textProjection", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("textProjectionPlot")
### * textProjectionPlot

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: textProjectionPlot
### Title: Plot Supervised Dimension Projection
### Aliases: textProjectionPlot

### ** Examples

# The test-data included in the package is called: DP_projections_HILS_SWLS_100.
# The dataframe created by textProjection can also be used as input-data.

# Supervised Dimension Projection Plot
plot_projection <- textProjectionPlot(
  word_data = DP_projections_HILS_SWLS_100,
  k_n_words_to_test = FALSE,
  min_freq_words_test = 1,
  plot_n_words_square = 3,
  plot_n_words_p = 3,
  plot_n_word_extreme = 1,
  plot_n_word_frequency = 1,
  plot_n_words_middle = 1,
  y_axes = FALSE,
  p_alpha = 0.05,
  title_top = "Supervised Dimension Projection (SDP)",
  x_axes_label = "Low vs. High HILS score",
  y_axes_label = "Low vs. High SWLS score",
  p_adjust_method = "bonferroni",
  scale_y_axes_lim = NULL
)

plot_projection

# Investigate elements in DP_projections_HILS_SWLS_100.
names(DP_projections_HILS_SWLS_100)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("textProjectionPlot", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("textQA")
### * textQA

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: textQA
### Title: Question Answering. (experimental)
### Aliases: textQA

### ** Examples

## No test: 
#   qa_examples <- textQA(question = "Which colour have trees?",
#     context = "Trees typically have leaves, are mostly green and like water.")
## End(No test)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("textQA", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("textSimilarity")
### * textSimilarity

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: textSimilarity
### Title: Semantic Similarity
### Aliases: textSimilarity

### ** Examples

# Compute the semantic similarity between the embeddings from "harmonytext" and "satisfactiontext".
## Not run: 
##D similarity_scores <- textSimilarity(
##D   x = word_embeddings_4$texts$harmonytext,
##D   y = word_embeddings_4$texts$satisfactiontext
##D )
##D 
##D # Show information about how similarity_scores were constructed.
##D comment(similarity_scores)
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("textSimilarity", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("textSimilarityMatrix")
### * textSimilarityMatrix

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: textSimilarityMatrix
### Title: Semantic similarity across multiple word embeddings
### Aliases: textSimilarityMatrix

### ** Examples

similarity_scores <- textSimilarityMatrix(word_embeddings_4$texts$harmonytext[1:3, ])
round(similarity_scores, 3)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("textSimilarityMatrix", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("textSimilarityNorm")
### * textSimilarityNorm

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: textSimilarityNorm
### Title: Semantic similarity between a text variable and a word norm
### Aliases: textSimilarityNorm

### ** Examples

## Not run: 
##D library(dplyr)
##D library(tibble)
##D harmonynorm <- c("harmony peace ")
##D satisfactionnorm <- c("satisfaction achievement")
##D 
##D norms <- tibble::tibble(harmonynorm, satisfactionnorm)
##D word_embeddings <- word_embeddings_4$texts
##D word_embeddings_wordnorm <- textEmbed(norms)
##D similarity_scores <- textSimilarityNorm(
##D   word_embeddings$harmonytext,
##D   word_embeddings_wordnorm$harmonynorm
##D )
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("textSimilarityNorm", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("textSum")
### * textSum

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: textSum
### Title: Summarize texts. (experimental)
### Aliases: textSum

### ** Examples

## No test: 
# sum_examples <- textSum(Language_based_assessment_data_8[1:2,1:2],
# min_length = 5L,
# max_length = 10L)
## End(No test)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("textSum", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("textTokenize")
### * textTokenize

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: textTokenize
### Title: Tokenize text-variables
### Aliases: textTokenize

### ** Examples

## No test: 
# tokens <- textTokenize("hello are you?")
## End(No test)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("textTokenize", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("textTokenizeAndCount")
### * textTokenizeAndCount

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: textTokenizeAndCount
### Title: Tokenize and count
### Aliases: textTokenizeAndCount

### ** Examples

## Not run: 
##D textTokenizeAndCount(Language_based_assessment_data_8["harmonytexts"])
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("textTokenizeAndCount", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("textTopics")
### * textTopics

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: textTopics
### Title: BERTopic
### Aliases: textTopics textBERTopics

### ** Examples

## Not run: 
##D res <- textTopics(
##D   data = Language_based_assessment_data_8,
##D   variable_name = "harmonytexts",
##D   embedding_model = "distilroberta",
##D   representation_model = "mmr",
##D   min_df = 3,
##D   save_dir = "bertopic_results"
##D )
##D 
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("textTopics", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("textTrain")
### * textTrain

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: textTrain
### Title: Trains word embeddings
### Aliases: textTrain

### ** Examples

# Examines how well the embeddings from "harmonytext" can
# predict the numeric variable "hilstotal" in the pre-included
# dataset "Language_based_assessment_data_8".

## Not run: 
##D trained_model <- textTrain(
##D   x = word_embeddings_4$texts$harmonytext,
##D   y = Language_based_assessment_data_8$hilstotal
##D )
##D 
##D # Examine results (t-value, degree of freedom (df), p-value,
##D # alternative-hypothesis, confidence interval, correlation coefficient).
##D 
##D trained_model$results
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("textTrain", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("textTrainLists")
### * textTrainLists

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: textTrainLists
### Title: Train lists of word embeddings
### Aliases: textTrainLists

### ** Examples

# Examines how well the embeddings from Language_based_assessment_data_8 can
# predict the numerical numerical variables in Language_based_assessment_data_8.
# The training is done combination wise, i.e., correlations are tested pair wise,
# column: 1-5,1-6,2-5,2-6, resulting in a dataframe with four rows.

## Not run: 
##D word_embeddings <- word_embeddings_4$texts[1:2]
##D ratings_data <- Language_based_assessment_data_8[5:6]
##D 
##D trained_model <- textTrainLists(
##D   x = word_embeddings,
##D   y = ratings_data
##D )
##D 
##D # Examine results (t-value, degree of freedom (df), p-value,
##D # alternative-hypothesis, confidence interval, correlation coefficient).
##D 
##D trained_model$results
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("textTrainLists", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("textTrainN")
### * textTrainN

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: textTrainN
### Title: Cross-validated accuracies across sample-sizes
### Aliases: textTrainN

### ** Examples

# Compute correlations for 25%, 50%, 75% and 100% of the data in word_embeddings and perform
# cross-validation thrice.

## Not run: 
##D tibble_to_plot <- textTrainN(
##D   x = word_embeddings_4$texts$harmonytext,
##D   y = Language_based_assessment_data_8$hilstotal,
##D   sample_percents = c(25, 50, 75, 100),
##D   n_cross_val = 3
##D )
##D 
##D # tibble_to_plot contains correlation-coefficients for each cross_validation and
##D # standard deviation and mean value for each sample. The tibble can be plotted
##D # using the testTrainNPlot function.
##D 
##D # Examine tibble
##D tibble_to_plot
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("textTrainN", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("textTrainNPlot")
### * textTrainNPlot

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: textTrainNPlot
### Title: Plot cross-validated accuracies across sample sizes
### Aliases: textTrainNPlot

### ** Examples

# Plot cross-validated correlation coefficients across different sample-sizes from the object
# returned by the textTrainN function.

## Not run: 
##D # Plot the performance of a single model across different sample sizes
##D plot_object1 <- textTrainNPlot(
##D   train_data = tibble_to_plot,
##D   n_cross_val = 3,
##D   x_unit = "quantity"
##D )
##D 
##D # Visualize plot
##D plot_object1
##D 
##D # Plot the performance of several models across different sample sizes.
##D plot_object2 <- textTrainNPlot(train_data = list(object1, object2, object3),
##D                                n_cross_val = c(2,1,1),
##D                                line_color = c("","","#0000FF")) # "" gives the default settings.
##D # Visualize plot
##D plot_object2
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("textTrainNPlot", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("textTrainRandomForest")
### * textTrainRandomForest

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: textTrainRandomForest
### Title: Trains word embeddings usig random forest
### Aliases: textTrainRandomForest

### ** Examples

# Examines how well the embeddings from column "harmonywords" in
# Language_based_assessment_data_8 can binarily classify gender.

## Not run: 
##D trained_model <- textTrainRandomForest(
##D   x = word_embeddings_4$texts$harmonywords,
##D   y = as.factor(Language_based_assessment_data_8$gender),
##D   trees = c(1000, 1500),
##D   mtry = c(1), # this is short because of testing
##D   min_n = c(1), # this is short because of testing
##D   multi_cores = FALSE # This is FALSE due to CRAN testing and Windows machines.
##D )
##D 
##D 
##D # Examine results (t-value, degree of freedom (df), p-value,
##D # alternative-hypothesis, confidence interval, correlation coefficient).
##D 
##D trained_model$results
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("textTrainRandomForest", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("textTrainRegression")
### * textTrainRegression

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: textTrainRegression
### Title: Train word embeddings to a numeric variable.
### Aliases: textTrainRegression

### ** Examples

# Examines how well the embeddings from the column "harmonytext" can
# predict the numerical values in the column "hilstotal".

## Not run: 
##D trained_model <- textTrainRegression(
##D   x = word_embeddings_4$texts$harmonytext,
##D   y = Language_based_assessment_data_8$hilstotal,
##D   multi_cores = FALSE # This is FALSE due to CRAN testing and Windows machines.
##D )
##D 
##D # Examine results (t-value, degree of freedom (df), p-value, alternative-hypothesis,
##D # confidence interval, correlation coefficient).
##D 
##D trained_model$results
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("textTrainRegression", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("textTranslate")
### * textTranslate

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: textTranslate
### Title: Translation. (experimental)
### Aliases: textTranslate

### ** Examples

## No test: 
# translation_example <- text::textTranslate(
#  Language_based_assessment_data_8[1,1:2],
#  source_lang = "en",
#  target_lang = "fr",
#  model = "t5-base")
## End(No test)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("textTranslate", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("textWordPrediction")
### * textWordPrediction

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: textWordPrediction
### Title: Compute word-level prediction scores and permutation-based
###   p-values
### Aliases: textWordPrediction

### ** Examples

## Not run: 
##D library(text)
##D 
##D # --- Step 1: embed the text column (word-type embeddings are needed)
##D embeddings <- textEmbed(
##D   Language_based_assessment_data_8["harmonywords"],
##D   aggregation_from_tokens_to_word_types = "mean"
##D )
##D 
##D # --- Step 2: run textWordPrediction
##D result <- textWordPrediction(
##D   words                 = Language_based_assessment_data_8$harmonywords,
##D   word_types_embeddings = embeddings$word_types,
##D   x                     = Language_based_assessment_data_8$hilstotal,
##D   n_permutations        = 10000
##D )
##D 
##D # --- Step 3: inspect word-level scores
##D result$word_data
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("textWordPrediction", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("textZeroShot")
### * textZeroShot

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: textZeroShot
### Title: Zero Shot Classification (Experimental)
### Aliases: textZeroShot

### ** Examples

## No test: 
# ZeroShot_example <- text::textZeroShot(sequences = c("I play football",
# "The forest is wonderful"),
# candidate_labels = c("sport", "nature", "research"),
# model = "facebook/bart-large-mnli")
## End(No test)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("textZeroShot", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("textrpp_install")
### * textrpp_install

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: textrpp_install
### Title: Install text required python packages in conda or virtualenv
###   environment
### Aliases: textrpp_install textrpp_install_virtualenv

### ** Examples

## Not run: 
##D # install text required python packages in a miniconda environment (macOS and Linux)
##D textrpp_install(prompt = FALSE)
##D 
##D # install text required python packages to an existing conda environment
##D textrpp_install(conda = "~/anaconda/bin/")
## End(Not run)
## Not run: 
##D # install text required python packages in a virtual environment
##D textrpp_install_virtualenv()
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("textrpp_install", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
