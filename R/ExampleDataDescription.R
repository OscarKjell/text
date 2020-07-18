# devtools::document()
# usethis::use_data(/Users/oscarkjell/Desktop/1 Projects/0 Research/0 text r-package/text/data/sq_data_tutorial_plotting_hilswl.rda)
#' Text and numeric data for 10 participants.
#'
#' The dataset is a shortened version of the data sets of Study 3-5
#' from Kjell et al., (2018; https://psyarxiv.com/er6t7/).
#'
#' @format A data frame with 10 participants and 8 variables:
#' \describe{
#'   \item{harmonywords}{descriptive words where respondents describe their harmony in life}
#'   \item{satisfactionwords}{descriptive words where respondents describe their satisfaction with life}
#'   \item{harmonytexts}{text where respondents describe their harmony in life}
#'   \item{satisfactiontexts}{text where respondents describe their satisfaction with life}
#'   \item{hilstotal}{total score of the Harmony In Life Scale}
#'   \item{swlstotal}{total score of the Satisfaction With Life Scale}
#'   \item{age}{respondents age in years}
#'   \item{gender}{respondents gender 1=male, 2=female}
#' }
#' @source \url{https://psyarxiv.com/er6t7/}
"Language_based_assessment_data_8_10"


#' Example text and numeric data.
#'
#' The dataset is a shortened version of the data sets of Study 3-5
#' from Kjell, Kjell, Garcia and Sikström 2018.
#'
#' @format A data frame with 100 rows and 4 variables:
#' \describe{
#'    \item{harmonywords}{Word responses from the harmony in life word question}
#'   \item{hilstotal}{total score of the Harmony In Life Scale}
#'   \item{swlstotal}{total score of the Satisfaction With Life Scale}
#' }
#' @source \url{https://psyarxiv.com/er6t7/}
"Language_based_assessment_data_3_100"


#' Word embeddings from textHuggingFace function
#'
#' The dataset is a shortened version of the data sets of Study 3-5
#' from Kjell, Kjell, Garcia and Sikström 2018.
#'
#' @format A list with word embeddings for harmony words for only contexts. BERT-base embeddings based on
#' mean aggregation of layer 1 and 2.
#' \describe{
#'   \item{tokens}{words}
#'   \item{layer_number }{layer of the transformer model}
#'   \item{Dim1:Dim768}{Word embeddings dimensions}
#' }
#' @source \url{https://psyarxiv.com/er6t7/}
"embeddings_from_huggingface2"



#' Wordembeddings for 4 text variables for 10 participants
#'
#' The dataset is a shortened version of the data sets of Study 3-5
#' from Kjell, Kjell, Garcia and Sikström 2018.
#'
#' @format A list with word embeddings for harmony words, satisfaction words, harmony text,
#' satisfaction text and decontextualized word embeddings. BERT-base embeddings based on
#' mean aggregation of layer 11 and 12.
#' \describe{
#'   \item{words}{words}
#'   \item{n}{word frequency}
#'   \item{Dim1:Dim768}{Word embeddings dimensions}
#' }
#' @source \url{https://psyarxiv.com/er6t7/}
"wordembeddings4_10"


#' Data for plotting a Dot Product Projection Plot.
#'
#' Tibble is the output from textProjectionData.
#' The dataset is a shortened version of the data sets of Study 3-5
#' from Kjell, Kjell, Garcia and Sikström 2018.
#'
#' @format A data frame with 583 rows and 12 variables:
#' \describe{
#'   \item{words}{unique words}
#'   \item{dot.x}{dot product projection on the x-axes}
#'   \item{p_values_dot.x}{p-value for the word in relation to the x-axes}
#'   \item{n_g1.x}{frequency of the word in group 1 on the x-axes variable}
#'   \item{n_g2.x}{frequency of the word in group 2 on the x-axes variable}
#'   \item{dot.y}{dot product projection on the y-axes}
#'   \item{p_values_dot.y}{p-value for the word in relation to the y-axes}
#'   \item{n_g1.y}{frequency of the word in group 1 on the y-axes variable}
#'   \item{n_g2.y}{frequency of the word in group 2 on the x-axes variable}
#'   \item{n}{overall word frequency}
#'   \item{n.percent}{frequency in percent}
#'   \item{N_participant_responses}{number of participants (as this is needed
#'    in the analyses)}
#' }
#' @source \url{https://psyarxiv.com/er6t7/}
"DP_projections_HILS_SWLS_100"


#' Example data for plotting a Semantic Centrality Plot.
#'
#' Oscar The dataset is a shortened version of the data sets of Study 1
#' from Kjell, et al., 2016.
#'
#' @format A data frame with 2,146 and 4 variables:
#' \describe{
#'   \item{words}{unique words}
#'   \item{n}{overall word frequency}
#'   \item{central_cosine}{cosine semantic similarity to the aggregated word embedding}
#'   \item{n_percent}{frequency in percent}
#' }
#' @source \url{https://link.springer.com/article/10.1007/s11205-015-0903-z}
"centrality_data_harmony"
