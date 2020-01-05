
#.rs.restartR()
# textPlotData
# textPlotViz

#Need this when this is not part of the package
cosines <- function(x, y) {
  rowSums(x*y, na.rm=TRUE) / ( sqrt(rowSums(x * x, na.rm=TRUE)) * sqrt(rowSums(y * y, na.rm=TRUE)) )
}


addEqualNrNArows <- function(x, y, Ndim) {
  success <- FALSE
  while (!success) {
    # Add row with NA
    x <- rbind(x, rep(NA, Ndim))
    # check for success
    success <- nrow(x) == nrow(y)
  }
  return(x)
}

library(text)
library(tibble)
library(purrr)
library(BSDA)

# Load example data
text_data <- readRDS("/Users/oscar/Desktop/0 Studies/5 R statistical semantics package/text_plot_test/sq_data_tutorial8_100.rda")

#text_data1 <- text_data[1:10, ]
T1_w <- Sys.time()
#testing_data <- textImport(text_data)
T2_w <- Sys.time()
#T2_w-T1_w
#testing_data

#saveRDS(testing_data, "/Users/oscar/Desktop/0 Studies/5 R statistical semantics package/text_plot_test/testing_plot_data.rda")
testing_data <- readRDS("/Users/oscar/Desktop/0 Studies/5 R statistical semantics package/text_plot_test/testing_plot_data.rda")

######################################################################
words = text_data$harmonywords 
wordembeddings = testing_data$harmonywords
single_word_embeddings_df = testing_data$singlewords_we

x = text_data$gender #text_data$swlstotal # text_data$hilstotal
y = text_data$hilstotal # text_data$swlstotal

# One problem is that there needs to be more than on word. However, could: 
# 1. add word-frequency so that there cannot be just one word
# 2. And then assume SD for those words only occuring once. 
# -> But not we are only plotting those occurring more than once.


# devtools::document()
#' textVizData trains word embeddings to a numeric variable.
#'
#' @param words Words to be plotted.
#' @param wordembeddings Wordembeddings from textImport for the words to be plotted .
#' @param single_wordembeddings Wordembeddings from textImport for individual words " 
#' @param x Numeric variable of a dimension the words should be plotted according to (y=NULL) 
#' @param y Numeric variable of a dimension the words should be plotted according to (y=NULL)
#' @param Bonferroni Whether to apply Bonferroni correction in relation to the number of t-test carried out for each individual word (default = TRUE) 
#' @param nrFolds_k Number of folds used in creation of semantic similiarty scores for individual words.
#' @param Ndim Number of dimensions used in the word embedding (default = 768).
#' 
# #' @param ... Arguments from caret::createFolds, caret::trainControl and caret::train.
#' @return A dataframe with values for the individual words that is used wor the plotting (e.g., including p-values, frequency, Cohen's D).
#' @examples
#' wordembeddings <- wordembeddings4_100
#' ratings_data <- sq_data_tutorial4_100
#' wordembeddings <- textVizData(words=wordembeddings$harmonywords, wordembeddings=wordembeddings$harmonywords, 
#' x=ratings_data$hilstotal, x=ratings_data$swlstotal)
#' @seealso see \code{\link{textVizPlot}}
# #' @importFrom purrr
#' @importFrom tibble as_tibble
#' @importFrom BSDA tsum.test
#' @importFrom dplyr row_number slice mutate_if bind_rows group_by summarize full_join
#' @importFrom caret createFolds
#' @export

# Function that creates semnatic t-test scores for single words for plotting purposes

textPlotData <- function(words, wordembeddings, single_wordembeddings, x, y=NULL, Bonferroni = TRUE, nrFolds=10, Ndim=768) {
  
  if (is.null(y) == TRUE) {
    x <- data.frame(x)
  } else {
  # Combing the dimensions for for-loop
  x <- data.frame(x, y)
  }
  
  # Creating list for the x and y dimension 
  word_data_list <- list()
  
  # For loop for x and y input; i.e., the two dimensions of the plot i_dim=1 i_dim=2
  for (i_dim in 1:ncol(x)) {
    
  #Get the word embeddings and Scale/Category
  x1 <- data.frame(words, x[i_dim])
  x1 <- tibble::as_tibble(cbind(x1, wordembeddings))

  #Splitting datasets up to low versus high according to median split
  group1 <- x1[ x1[2] > median(x1[[2]]), ]
  group2 <- x1[ x1[2] <= median(x1[[2]]), ]

  # Use function addEqualNrNArows from textTtestscores
  # Function adds rows of NA until group2 and group1 have the same amount of rows.
  # If statement deciding which of group1 or group2 that needs row(s) of NA
  if (nrow(group1) < nrow(group2) ) {
    group1 <- addEqualNrNArows(group1, group2)
  }else if (nrow(group1) > nrow(group2)){
    group2 <- addEqualNrNArows(group2, group1)
  }else{
    group1 <- group1
    group2 <- group2
  }
  
  # For loop taking one minus the other to create a 
  # semantic difference representation; using k-fold procedure
  
  #Creating folds with acret package
  nrF <- nrFolds
  folds <- c(1:nrow(group1))
  folds <- caret::createFolds(folds, k = nrF, list = TRUE, returnTrain = FALSE)
  folds
  
  semanticTtestscoreslistG1 <- list()
  semanticTtestscoreslistG2 <- list()
  
  for (i in 1:nrF) {
    ### Summ all semrep in one column and normlise to one SHOULD AVOID HARDCODING 1:7!!!  i=1
    colXsemrep <- colSums(group1[ , -c(1:7)][ -folds[[i]], ], na.rm = TRUE)
    # SHOULD WE NORMALISE HERE
    #colXsemrep <- normalizeV(colXsemrep)
    
    colYsemrep <- colSums(group2[ , -c(1:7)][ -folds[[i]], ], na.rm = TRUE)
    #colYsemrep <- normalizeV(colYsemrep)
    
    # The Semantic Difference Represenation: Taking colX minus colY
    semDifRep <- colXsemrep - colYsemrep
    
    # Measure the semantic simlarity score between the WORDS of the responses 
    #NOT-included-in-the-semDifRep and the semDifRep (i.e., the semantic difference representation)
    # Should I normalise them before(!?)
    # normX <- normalizeV(x[ folds[[i]], ])
    # normY <- normalizeV(y[ folds[[i]], ])
    
    # Get the words in a column (i=1)
    normG1 <- group1[ folds[[i]], ]
    normG1_words <- data.frame(unlist(strsplit(tolower(normG1$words), " ")))
    normG1_words1 <- tibble::as_tibble(as.character(normG1_words$unlist.strsplit.tolower.normG1.words........))
    
    normG2 <- group2[ folds[[i]], ]
    normG2_words <- data.frame(unlist(strsplit(tolower(normG2$words), " ")))
    normG2_words1 <- tibble::as_tibble(as.character(normG2_words$unlist.strsplit.tolower.normG2.words........))
    
    # Get the words's word embeddings
    #Function to aply the semantic representation to ONE word; and return vector with NA if word is not found
    applysemrep_plot <- function(x){
      #If semrep is found get it; if not return NA vector of dimensions (which equal "Ndim"=space[["s"]][[14]] )
      if (sum(single_word_embeddings_df$words == x[TRUE]) %in% 1) {
        x <- tolower(x)  
        #Get the semantic representation for a word=x
        word1rep <- single_word_embeddings_df[single_word_embeddings_df$words ==x, ]
        #Only get the semantic represenation as a vector without the actual word in the first column
        wordrep <- as_vector(word1rep[,8:length(word1rep)])
        
        #If the word does not have a semrep return vector with Ndim (512) dimensions of NA; Ndim=768
      }else if (x %in% NA) {
        wordrep <- data.frame(matrix(ncol = Ndim, nrow = 1))
        class(wordrep)
        wordrep <- as.numeric(wordrep)
      } else {
        wordrep <- data.frame(matrix(ncol = Ndim, nrow = 1))
        wordrep <- as.numeric(wordrep)
      } 
    }
    
    #Get word embeddings for each word
    group1_single1 <- sapply(normG1_words1$value, applysemrep_plot)
    group1_single2 <- tibble::as_tibble(t(group1_single1))
    
    group2_single1 <-  sapply(normG2_words1$value, applysemrep_plot)
    group2_single2 <- tibble::as_tibble(t(group2_single1))
    
    # Adds the Semantic Difference Representation into a Tibble and then duplicates it 
    # to as many rows as it will be compared to with x (nrow(normX))
    
    # For x: Only get the word embeddings and make them numeric
    group1_single3 <- group1_single2 %>%
     dplyr::mutate_if(is.character,as.numeric)
    # Get as many SemDifRep as group_single3 so that it can be compred 
    semDifRep_x <- tibble::as_tibble(t(semDifRep)) %>%
      dplyr::slice(rep(dplyr::row_number(), nrow(group1_single2)))
    # Get Semantic Similairty score between words and SemDifRep
    group1_single4_ss <- cosines(group1_single3, semDifRep_x)
    group1_single4_ss_1 <- tibble::tibble(normG1_words1$value, group1_single4_ss)
    
    # For y: Only get the word embeddings and make them numeric
    group2_single3 <- group2_single2 %>%
      dplyr::mutate_if(is.character,as.numeric)
    # Get as many SemDifRep as y3 so that it can be compred 
    semDifRep_y <- as_tibble(t(semDifRep)) %>%
      dplyr::slice(rep(dplyr::row_number(), nrow(group2_single2)))
    
    # Get Semantic Similairty score between words and SemDifRep
    group2_single4_ss <- cosines(group2_single3, semDifRep_y)
    group2_single4_ss_1 <- tibble(normG2_words1$value, group2_single4_ss)
    
    # Lists to save the restuls in
    semanticTtestscoreslistG1[[i]] <- group1_single4_ss_1
    semanticTtestscoreslistG2[[i]] <- group2_single4_ss_1
  }
  
  # Sorting out a dataframe for the resuts; bind the list of tibbles together to one
  semanticTtestscoreslistG1done <- dplyr::bind_rows(semanticTtestscoreslistG1)
  colnames(semanticTtestscoreslistG1done) <- c("words", "SS")
  semanticTtestscoreslistG1done$g1_1_g2_2 <- rep(1, nrow(semanticTtestscoreslistG1done))
  
  semanticTtestscoreslistG2done <- dplyr::bind_rows(semanticTtestscoreslistG2)
  colnames(semanticTtestscoreslistG2done) <- c("words", "SS")
  semanticTtestscoreslistG2done$g1_1_g2_2 <- rep(2, nrow(semanticTtestscoreslistG2done))
  
  # Bind all words together
  semanticTtestscoreslistG1G2done <- dplyr::bind_rows(semanticTtestscoreslistG1done, semanticTtestscoreslistG2done)

  # Getting Descriptives of the words
  # All words SS mean to the semantic comparison representation
  words_mean <- mean(semanticTtestscoreslistG1G2done$SS, na.rm=TRUE)
  # All words SD to the semantic comparison representation
  words_sd <- sd(semanticTtestscoreslistG1G2done$SS, na.rm=TRUE)
  
  # Each word's mean and sd (i.e., they may have different SS score due to being in different K-folds)
  Words_info <- dplyr::group_by(semanticTtestscoreslistG1G2done, words) %>% 
    dplyr::summarize(mean = mean(SS))
  Words_info_sd <- dplyr::group_by(semanticTtestscoreslistG1G2done, words) %>% 
    dplyr::summarize(sd = sd(SS))
  
  semanticTtestscoreslistG1G2done$words
  table(semanticTtestscoreslistG1G2done$words, useNA = "always" )
  # The n/frequency of each word
  Words_info$n <- as.numeric(table(semanticTtestscoreslistG1G2done$words, useNA = "ifany"))
  Words_info$sd <-  Words_info_sd$sd
  #Add the max SD to the words that do not have a SD
  Words_info$sd[is.na(Words_info$sd)] <- max(Words_info$sd, na.rm = T)
  
  # To get the p-values even for words only occuring once; I make n = 1 to n = 2. 
  # Words_info$n_2 <- Words_info$n
  # Words_info$n_2[Words_info$n_2 == 1] <- 2

  # Computing t-tests: library(BSDA) tsum.test(mean.x=.1,   s.x=.01, n.x=2, mean.y=.136, s.y=.02, n.y=7)
  # Words_info$n_2 adds so it is at least 2 words; and more t-tests can be made.
  n_total_words <- sum(Words_info$n)
  Words_ttest <- data.frame(mapply(BSDA::tsum.test, 
                                   mean.x = Words_info$mean, s.x = Words_info$sd, n.x = Words_info$n,
                                               mean.y = words_mean, s.y = words_sd, n.y = n_total_words-Words_info$n, 
                                  var.equal = FALSE))
  # Pulling out the t-statistics and p-values from the t-test done above
  p_values <- t(Words_ttest[1:nrow(Words_info)][3,])
  Words_info$p_values <- unlist(p_values)
  t_statistic <- t(Words_ttest[1:nrow(Words_info)][1,])
  Words_info$t_statistic <- unlist(t_statistic)
  
  # Function for Computing Cohen's D (https://www.socscistatistics.com/effectsize/default3.aspx) 
  cohens_d <- function(mean_x, sd_x, n_x, mean_y, sd_y, n_y) {
    (mean_x - mean_y)/ (sqrt((sd_x^2 + sd_y^2)/2))
  }
  # Applying Cohens D individually for each word
  cohen_D_df <- data.frame(mapply(cohens_d, 
                                  mean_x = Words_info$mean, sd_x = Words_info$sd, n_x = Words_info$n,    #Words_info$n_2 adds so it is at least 2 words; and more t-tests can be made.
                                  mean_y = words_mean, sd_y = words_sd, n_y = n_total_words-Words_info$n))
  colnames(cohen_D_df) <- "cohensD"
  Words_info$cohensD <- cohen_D_df$cohensD
  
  word_data_list[i_dim] <- list(Words_info)
  }
  # Arranging it to one tibble; accounting for x versus x and y input
  if (is.null(y)==TRUE) {
    word_data_tibble <- word_data_list[[1]]
    colnames(word_data_tibble) <- c("words", "mean.x", "n.x", "sd.x", 
                                    "p_values.x", "t_statistic.x", "cohensD.x")
  } else {
    word_data_tibble <- dplyr::full_join(word_data_list[[1]], word_data_list[[2]], by = "words")  
  }
  
  # Bonferroni correction or not
  if (isTRUE(Bonferroni) == TRUE) {
    # If there are no y axes or not. 
    if ( is.null(y) == TRUE) {
      bonferroni_x <- as.numeric(table(!is.na(word_data_tibble$p_values.x))["TRUE"])
      word_data_tibble_bonf <- word_data_tibble[word_data_tibble$p_values.x < .05/bonferroni_x, ]
    } else {
    # Counting number of t-test made (i.e, on words more than 1)
    bonferroni_x <- as.numeric(table(!is.na(word_data_tibble$p_values.x))["TRUE"])
    bonferroni_y <- as.numeric(table(!is.na(word_data_tibble$p_values.y))["TRUE"])
    # Select significant words when Bonferroni correcting for multiple comparison 
    word_data_tibble_bonf <- word_data_tibble[word_data_tibble$p_values.x < .05/bonferroni_x | word_data_tibble$p_values.y < .05/bonferroni_y , ]
    }
    # Remove NAs
    word_data_tibble <- word_data_tibble_bonf[!is.na(word_data_tibble_bonf$words),]
    word_data_tibble
  } else {
    word_data_tibble
  }
  word_data_tibble
}
# End of function 

#Test Data
words = text_data$harmonywords 
wordembeddings = testing_data$harmonywords
single_word_embeddings_df = testing_data$singlewords_we
x = text_data$hilstotal #text_data$gender # # text_data$hilstotal
y = text_data$swlstotal

#Testing function
df_for_plotting <- textPlotData(words, wordembeddings, single_wordembeddings, x, y, Bonferroni = TRUE, nrFolds=10, Ndim=768)
df_for_plotting


# Sorting table for plotting function

# Remove stopwords
#view(df_for_plotting, 25)
df_for_plotting

# One scale (i.e, cohen's d and p-value. )
#library(scales)
library(dplyr)
library(ggplot2)
library(ggrepel)

textPlotViz <- function(data = df_for_plotting, top_p_words = -25,  
                        x_axes = "cohensD.x",
                        y_axes = "cohensD.y", #1_dim
                        x_axes_label = "Cohen's D", 
                        y_axes_label = "p-value",
                        scale_x_axes_lim = NULL,
                        scale_y_axes_lim = NULL,
                        y_axes_values = NULL,
                        word_font = "Arial",
                        colors_words = c("#ff0000", "#ff8080", "white", "#1aff1a", "#00cc00"),
                        colors_words_scale = c(-0.1, -0.01, 0, 0.01, 0.1), 
                        word_size_range = c(3, 8),
                        position_jitter_hight = .0,
                        position_jitter_width = .03,
                        point_size = 0.5,
                        arrow_transparency = 0) {
  
  #Make limits for color gradient so that 0 becomes in the middle; 
  #should NOT hardcode data$cohensD.x
  color_limit <- max(abs(data$cohensD.x)) * c(-1, 1)
  
  # This solution is because it is not possible to send "0" as a parameter
  if(is.null(y_axes) == TRUE){
    one_dime <- 0
    y_axes <- "one_dime"
  }else{
    y_axes
  }
  
  # Select lowest p-values for x or both x and y if needed. 
  if(is.null(y_axes)){
    data <- dplyr::top_n(data, top_p_words, wt = p_values.x)
  }else{
    data1 <- dplyr::top_n(data, top_p_words, wt = p_values.x)
    data2 <- dplyr::top_n(data, top_p_words, wt = p_values.y)
    data3 <- rbind(data1, data2)
    data <- unique(data3)
  }
  
  # Plot
  plot <- data %>% 
    # Select X bottom words by p_values
   # top_n(top_p_words, wt = p_values.x) %>%
    
    # construct ggplot; the !!sym( ) is to  turn the strings into symbols.
    ggplot2::ggplot(aes(!!sym(x_axes), !!sym(y_axes), label = words)) +
    
    # Help creat possibility to remove y-axes numbers help(scale_y_continuous)
    scale_x_continuous(limits = scale_x_axes_lim) +
    scale_y_continuous(limits = scale_y_axes_lim) +
    
    # ggrepel geom, make arrows transparent, color by rank, size by n help(geom_text_repel)
    ggrepel::geom_text_repel(segment.alpha = arrow_transparency,
                             position=position_jitter(h = position_jitter_hight, w = position_jitter_width),
                             aes(color = !!sym(x_axes), size = n.x, family = word_font)) + 
    
    # Decide size and color of the points
    geom_point(size = point_size,
              aes(color = !!sym(x_axes))) +
    
    # set color gradient, & customize legend help(guide_colorbar)
    scale_colour_gradientn(colours = colors_words, 
                           limit = color_limit,
                           values = rescale(colors_words_scale), 
                           space = "Lab",
                           aesthetics = "colour",
                           guide = guide_colorbar(direction = "horizontal",
                                                  title.position ="top",
                                                  title = "Cohen's D",
                                                  element_text(color = "#61605e"))) +
  
    # set word size range 
    scale_size_continuous(range = word_size_range,
                          guide = guide_legend(title= "Frequency", 
                                               title.position = "top",
                                               direction = "horizontal",
                                               label.position = "bottom", 
                                               element_text(color = "#61605e"))) +
  
    labs(y = y_axes_label, x = x_axes_label) +
    
    # minimal theme, and turning off legends
    theme_minimal() +
    theme(legend.position= c("bottom"),
          legend.justification = c("right","top"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.y = y_axes_values,
          axis.title.x = element_text(color = "#61605e"),
          axis.title.y = element_text(color = "#61605e"))
  plot
}


plot <- textPlotViz(data = df_for_plotting, top_p_words = -25,  
                    x_axes = "cohensD.x",
                    y_axes = "cohensD.y",
                    x_axes_label = "Cohen's D", 
                    y_axes_label =  " ",
                    word_font = "Helvetica",
                    scale_x_axes_lim = c(-4,4),
                    #scale_y_axes_lim = c(0,0),
                    #y_axes_values = element_blank(),
                    colors_words = c("#ff0000", "#ff8080", "white",  "#1aff1a", "#00cc00"),
                    colors_words_scale = c(-4, -0.1, 0, 0.1, 4), 
                    word_size_range = c(3, 8),
                    position_jitter_hight = 0.0,
                    position_jitter_width = 0.03,
                    point_size = 0.3,
                    arrow_transparency = 0.1)
plot

view(data)

#
plot1















#######
textPlotViz2 <- function(data = df_for_plotting, top_p_words = -25,  
                         x_axes = data$cohensD.x, y_axes = data$cohensD.y,
                        x_axes_label = "Cohen's D", 
                        y_axes_label = "p-value", 
                        colors_words = c("#ff0000", "#ff8080", "white", "#1aff1a", "#00cc00"),
                        colors_words_scale = c(-0.1, -0.001, 0.001, 0.1), 
                        word_size_range = c(3, 8),
                        position_jitter_hight = 0.0,
                        position_jitter_width = 0.03,
                        point_size = 0.5,
                        arrow_transparency = 0) {
  plot <-   data %>% 
    # Select X bottom words by p_values
    top_n(top_p_words, wt = p_values.x) %>%
    
    # construct ggplot
    ggplot(aes(cohensD.x, p_values.x, label = words)) +
    #Reversing the p-value scalre so that 0 is on top
    scale_y_reverse() +
    
    # ggrepel geom, make arrows transparent, color by rank, size by n
    ggrepel::geom_text_repel(segment.alpha = arrow_transparency,
                             position=position_jitter(h = position_jitter_hight, w = position_jitter_width),
                             aes(color = cohensD.x, size = n.x)) + 
    # Decide sice and color of the points
    geom_point( size = point_size,
                aes(color = cohensD.x)) +
    
    # set color gradient, & customize legend
    scale_colour_gradientn(colours = colors_words, 
                           values = rescale(colors_words_scale), 
                           space = "Lab",
                           aesthetics = "colour",
                           guide = guide_colorbar(direction = "horizontal",
                                                  title.position ="top",
                                                  title = "Cohen's D")) +
    
    # set word size range 
    scale_size_continuous(range = word_size_range,
                          guide = guide_legend(title= "Frequency", 
                                               title.position = "top",
                                               direction = "horizontal",
                                               label.position = "bottom")) +
    
    labs(y = y_axes_label, x = x_axes_label) +
    
    # minimal theme, and turning off legends
    theme_minimal() +
    theme(legend.position= c("bottom"),
          legend.justification = c("right","top"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
  plot
}







  
  
##### 2-D: x and y plot
df_for_plotting %>% 
  
  # Select X bottom words by p_values
  top_n(-25, wt = p_values.x) %>%
#  top_n(-25, wt = p_values.y) %>%
  
  # construct ggplot
  ggplot(aes(cohensD.x, y=c(0), label = words)) +
  
  # Avoid overlapping text
  ggrepel::geom_text_repel(segment.alpha = 0,
                           position=position_jitter(h=0.0,w=0.3),
                           aes(color = cohensD.x, size = n.x)) + 
  
  # Get points for each word
  geom_point( size = 0.5,
              aes(color = cohensD.x)) +
  
  # Set color gradient in 5 steps; white in the middle avoiding mixing  colors in the middle
  scale_colour_gradientn(colours = c("#ff0000", "#ff8080", "white", "#1aff1a", "#00cc00"), 
                         values=rescale(c(-0.1, -0.001, 0.001, 0.1)), 
                         space = "Lab",
                         aesthetics = "colour",
                         guide = guide_colorbar(direction = "horizontal",
                                                title.position ="top",
                                                title = "Cohen's D")) +
  # set word size range and its guide
  scale_size_continuous(range = c(3, 8),
                        guide = guide_legend(title= "Frequency", 
                                             title.position = "top",
                                             direction = "horizontal",
                                             label.position = "bottom")) +
  labs(y = "SWLS Cohen's D", x = "HILS Cohen's D") +
  
  # minimal theme & customizations & turn off legend
  theme_minimal() +
  theme(legend.position= c("bottom"),
        legend.justification = c("right","top"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

#













###### Fungerande script
 ##RESERV 
#2- dim x and y
##### 2-D: x and y plot
df_for_plotting %>% 
  
  # Select X bottom words by p_values
  top_n(-25, wt = p_values.x) %>%
  top_n(-25, wt = p_values.y) %>%
  
  # construct ggplot
  ggplot(aes(cohensD.x, cohensD.y, label = words)) +
  
  # Avoid overlapping text
  ggrepel::geom_text_repel(segment.alpha = 0,
                           position=position_jitter(h=0.0,w=0.3),
                           aes(color = cohensD.x, size = n.x)) + 
  
  # Get points for each word
  geom_point( size = 0.5,
              aes(color = cohensD.x)) +
  
  # Set color gradient in 5 steps; white in the middle avoiding mixing  colors in the middle
  scale_colour_gradientn(colours = c("#ff0000", "#ff8080", "white", "#1aff1a", "#00cc00"), 
                         values=rescale(c(-0.1, -0.001, 0.001, 0.1)), 
                         space = "Lab",
                         aesthetics = "colour",
                         guide = guide_colorbar(direction = "horizontal",
                                                title.position ="top",
                                                title = "Cohen's D")) +
  # set word size range and its guide
  scale_size_continuous(range = c(3, 8),
                        guide = guide_legend(title= "Frequency", 
                                             title.position = "top",
                                             direction = "horizontal",
                                             label.position = "bottom")) +
  labs(y = "SWLS Cohen's D", x = "HILS Cohen's D") +
  
  # minimal theme & customizations & turn off legend
  theme_minimal() +
  theme(legend.position= c("bottom"),
        legend.justification = c("right","top"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

#

##### Reserv

# Only x-dimension (x=Cohen's D; y = p-value)
df_for_plotting %>% 
  # Select X bottom words by p_values
  top_n(-25, wt = p_values.x) %>%
  
  # construct ggplot
  ggplot(aes(cohensD.x, p_values.x, label = words)) +
  #Reversing the p-value scalre so that 0 is on top
  scale_y_reverse() +
  
  # ggrepel geom, make arrows transparent, color by rank, size by n
  ggrepel::geom_text_repel(segment.alpha = 0,
                           position=position_jitter(h=0.0,w=0.3),
                           aes(color = cohensD.x, size = n.x)) + 
  # Decide sice and color of the points
  geom_point( size = 0.5,
              aes(color = cohensD.x)) +
  
  # set color gradient, & customize legend
  scale_colour_gradientn(colours = c("#ff0000", "#ff8080", "white", "#1aff1a", "#00cc00"), 
                         values=rescale(c(-0.1, -0.001, 0.001, 0.1)), 
                         space = "Lab",
                         aesthetics = "colour",
                         guide = guide_colorbar(direction = "horizontal",
                                                title.position ="top",
                                                title = "Cohen's D")) +
  
  # set word size range 
  scale_size_continuous(range = c(3, 8),
                        guide = guide_legend(title= "Frequency", 
                                             title.position = "top",
                                             direction = "horizontal",
                                             label.position = "bottom")) +
  
  labs(y = "p-value", x = "Cohen's D related to Low vs High HILS") +
  
  # minimal theme, and turning off legends
  theme_minimal() +
  theme(legend.position= c("bottom"), #c(.99, .9999)
        legend.justification = c("right","top"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())








#####


# tokenize text at the single word (aka unigram) level
hn_word_tokens <- text_data %>% unnest_tokens(word, input = harmonywords)
head(hn_word_tokens, 20)
# remove stop words (e.g. 'a', 'the', 'and')
hn_word_tokens_no_stop <- hn_word_tokens %>% anti_join(get_stopwords())

# create word counts
hn_word_counts <- hn_word_tokens_no_stop %>% count(word, sort = T)

# print top 10 most frequent words
hn_word_counts %>% head(10)

# compute avg comment ranking of each word
hn_word_avg_rank <- hn_word_tokens_no_stop %>%
  group_by(word) %>%
  summarize(avg_rank = mean(text_data$hilstotal))# filter word counts & join to average ranks
hn_counts_and_ranks <- hn_word_counts %>% filter(n < 2000 ) %>% 
  left_join(hn_word_avg_rank, by = "word")


library(ggplot2)
library(ggrepel)

# select the top 100 words by n (aka word count)
hn_counts_and_ranks %>% top_n(100, wt = n) %>%
  
  # construct ggplot
  ggplot(aes(avg_rank, n, label = word)) +
  
  # ggrepel geom, make arrows transparent, color by rank, size by n
  geom_text_repel(segment.alpha = 0, 
                  aes(colour=avg_rank, size=n)) + 
  
  # set color gradient,log transform & customize legend
  scale_color_gradient(low="green3", high="violetred", 
                       trans = "log10",
                       guide = guide_colourbar(direction = "horizontal",
                                               title.position ="top")) +
  # set word size range & turn off legend
  scale_size_continuous(range = c(3, 10),
                        guide = FALSE) +
  scale_x_log10() +     # use log-scale for x-axis
  ggtitle(paste0("Top 100 words from ",
                 nrow(hn_counts_and_ranks),   # dynamically include row count
                 " harmoyn text answers"),
          subtitle = "word frequency (size) ~ avg comment ranking (color)") + 
  labs(y = "Word frequency", x = "Avg rank (log scale)") +
  
  # minimal theme & customizations
  theme_minimal() +
  theme(legend.position=c(.99, .99),
        legend.justification = c("right","top"),
        panel.grid.major = element_line(colour = "whitesmoke"))



#
