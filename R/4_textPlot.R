
#.rs.restartR()
# textPlotData
# textPlotViz
# devtools::document()
#' textPlotData computes variables about individual words for plotting.
#'
#' @param words Words to be plotted.
#' @param wordembeddings Wordembeddings from textImport for the words to be plotted .
#' @param single_wordembeddings_df Wordembeddings from textImport for individual words "
#' @param x Numeric variable of a dimension the words should be plotted according to (y=NULL)
#' @param y Numeric variable of a dimension the words should be plotted according to (y=NULL)
#' @param Bonferroni Whether to apply Bonferroni correction in relation to the number of t-test carried out for each individual word (default = TRUE)
#' @param nrFolds Number of folds used in creation of semantic similiarty scores for individual words.
#' @param Ndim Number of dimensions used in the word embedding (default = 768).
#'
# #' @param ... Arguments from caret::createFolds, caret::trainControl and caret::train.
#' @return A dataframe with variables (e.g., including p-values, frequency, Cohen's D) for the individual words
#' that is used for the plotting in the textPlotViz function.
#' @examples
#' wordembeddings <- wordembeddings4_10
#' data <- sq_data_tutorial8_10
#' df_for_plotting <- textPlotData(data$harmonywords, wordembeddings$harmonywords,
#' wordembeddings$singlewords_we,
#' data$hilstotal, data$swlstotal,
#' Bonferroni = TRUE, nrFolds=2, Ndim=768)
#' df_for_plotting
#' @seealso see \code{\link{textPlotViz}}
#' @importFrom tibble as_tibble
#' @importFrom purrr as_vector
#' @importFrom BSDA tsum.test
#' @importFrom dplyr row_number slice mutate_if bind_rows group_by summarize full_join
#' @importFrom caret createFolds
#' @importFrom stats median sd
#' @export

# Function that creates semnatic t-test scores for single words for plotting purposes
textPlotData <- function(words, wordembeddings, single_wordembeddings_df, x, y=NULL, Bonferroni = TRUE, nrFolds=10, Ndim=768) {

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
    group1 <- x1[ x1[2] > stats::median(x1[[2]]), ]
    group2 <- x1[ x1[2] <= stats::median(x1[[2]]), ]

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

      #Get word embeddings for each word; applysemrep is created in 1_textImport
      group1_single1 <- sapply(normG1_words1$value, applysemrep, single_wordembeddings_df, Ndim)
      group1_single2 <- tibble::as_tibble(t(group1_single1))

      group2_single1 <-  sapply(normG2_words1$value, applysemrep, single_wordembeddings_df, Ndim)
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
    words_sd <- stats::sd(semanticTtestscoreslistG1G2done$SS, na.rm=TRUE)

    # Each word's mean and sd (i.e., they may have different SS score due to being in different K-folds)
    Words_info <- dplyr::group_by(semanticTtestscoreslistG1G2done, words) %>%
      dplyr::summarize(mean = mean(SS))
    Words_info_sd <- dplyr::group_by(semanticTtestscoreslistG1G2done, words) %>%
      dplyr::summarize(sd = stats::sd(SS))

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


# devtools::document()
#' textPlotViz trains word embeddings to a numeric variable.
#'
#' @param word_data Dataframe from textPlotData.
#' @param plot_n_words Number of significant words to plot; selects fist according to lowest p-value and then to highest absolut Cohen's D (default 25 for each scale).
#' @param title_top string for title (default "  ")
#' @param titles_color Color for all the titles (default: "#61605e")
#' @param x_axes Variable to be plotted on the x axes (default is "cohensD.x").
#' @param y_axes Variable to be plotted on the y axes (default is "cohensD.y"). To only print 1-dimension insert NULL.
#' @param x_axes_label Label on the x-axes (default: "Cohen's D").
#' @param y_axes_label Label on the y-axes (default: "Cohen's D").
#' @param scale_x_axes_lim NULL,
#' @param scale_y_axes_lim NULL,
#' @param y_axes_values NULL,
#' @param word_font Type of font (default: "Helvetica").
#' @param colors_words The diffent colors of the words (default: c("#ff0000", "#ff8080", "white", "#1aff1a", "#00cc00")).
#' @param colors_words_scale Limits indicating where the colors should change (default: c(-0.1, -0.01, 0, 0.01, 0.1).
#' @param word_size_range a vector with minimum and maximum (default: c(3, 8)).
#' @param position_jitter_hight degree of jitter hight (default: .0).
#' @param position_jitter_width degree of jitter hight (default: .03).
#' @param point_size the size of the points indicating the words' position (default: 0.5).
#' @param arrow_transparency The transparency of the lines between each word and point (default: 0.1)
#'
#' @return A 1- or 2-dimensional word plot.
#' @examples
#' # The test-data included in the package is called: sq_data_plottingHw_HILSSSWLS_100
#' plot <- textPlotViz(word_data = sq_data_plottingHw_HILSSSWLS_100,
#'      plot_n_words = 25,
#'      x_axes = "cohensD.x",
#'      y_axes = "cohensD.y",
#'      x_axes_label = "HILS: Cohen's D",
#'      y_axes_label = "SWLS: Cohen's D",
#'      scale_x_axes_lim = NULL,
#'      scale_y_axes_lim = NULL,
#'      y_axes_values = NULL,
#'      word_font = "Helvetica",
#'      colors_words = c("#ff0000", "#ff8080", "white", "#1aff1a", "#00cc00"),
#'      colors_words_scale = c(-0.1, -0.01, 0, 0.01, 0.1),
#'      word_size_range = c(3, 8),
#'      position_jitter_hight = .0,
#'      position_jitter_width = .03,
#'      point_size = 0.5,
#'      arrow_transparency = 0.1)
#' plot
#' @seealso see \code{\link{textPlotData}}
#' @importFrom tibble as_tibble
#' @importFrom BSDA tsum.test
#' @importFrom dplyr row_number slice mutate_if bind_rows group_by summarize full_join %>%
#' @importFrom caret createFolds
#' @importFrom scales rescale
#' @importFrom ggplot2 position_jitter element_text
#' @importFrom rlang sym
#' @export

#word_data <-sq_data_plottingHw_HILSSSWLS_100
textPlotViz <- function(word_data,
                        plot_n_words = 25,
                        title_top = " ",
                        titles_color = "#61605e",
                        x_axes = "cohensD.x",
                        y_axes = "cohensD.y",
                        x_axes_label = "Cohen's D",
                        y_axes_label = "Cohen's D",
                        scale_x_axes_lim = NULL,
                        scale_y_axes_lim = NULL,
                        y_axes_values = NULL,
                        word_font = "Helvetica",
                        colors_words = c("#ff0000", "#ff8080", "white", "#99e699", "#33cc33"),
                        colors_words_scale = c(-0.1, -0.01, 0, 0.01, 0.1),
                        word_size_range = c(3, 8),
                        position_jitter_hight = .0,
                        position_jitter_width = .03,
                        point_size = 0.5,
                        arrow_transparency = 0.1) {

  #Make limits for color gradient so that 0 becomes in the middle;
  #should NOT hardcode data$cohensD.x
  color_limit <- max(abs(word_data$cohensD.x)) * c(-1, 1)

  # This solution is because it is not possible to send "0" as a parameter
  if(is.null(y_axes) == TRUE){
    one_dime <- 0
    y_axes <- "one_dime"
  }else{
    y_axes
  }

  # Select lowest p-values for x or both x and y if needed. help(which.min)
  if(is.null(y_axes)){
    # Order data fram first according to lowest p-value and then to highest absolut Cohen's D
    data1 <- word_data[with(word_data, order(p_values.x, -abs(cohensD.x))), ]
    # Selecting the plot_n_words
    data <- data1[1:plot_n_words, ]
  }else{
   # Selecting as above but for both x and y
   data1x <- word_data[with(word_data, order(p_values.x, -abs(cohensD.x))), ]
   # Selecting the plot_n_words
   data2x <- data1x[1:plot_n_words, ]
   # Selecting as above but for both x and y
   data1y <- word_data[with(word_data, order(p_values.y, -abs(cohensD.y))), ]
   # Selecting the plot_n_words
   data2y <- data1y[1:plot_n_words, ]
   # Combing the words and selecting remove duplicates
   data3 <- rbind(data2x, data2y)
    data <- unique(data3)
  }

  # Plot
  plot <- data %>%

    # construct ggplot; the !!sym( ) is to  turn the strings into symbols.
    ggplot2::ggplot(ggplot2::aes(!!rlang::sym(x_axes), !!rlang::sym(y_axes), label = words)) +

    # Title
    ggplot2::ggtitle(paste0(title_top)) +

    # Help creat possibility to remove y-axes numbers help(scale_y_continuous)
    ggplot2::scale_x_continuous(limits = scale_x_axes_lim) +
    ggplot2::scale_y_continuous(limits = scale_y_axes_lim) +

    # ggrepel geom, make arrows transparent, color by rank, size by n help(geom_text_repel)
    ggrepel::geom_text_repel(segment.alpha = arrow_transparency,
                             position = ggplot2::position_jitter(h = position_jitter_hight, w = position_jitter_width),
                             ggplot2::aes(color = !!rlang::sym(x_axes), size = n.x, family = word_font)) +

    # Decide size and color of the points
    ggplot2::geom_point(size = point_size,
                        ggplot2::aes(color = !!rlang::sym(x_axes))) +

    # set color gradient, & customize legend help(guide_colorbar)
    ggplot2::scale_colour_gradientn(colours = colors_words,
                           limit = color_limit,
                           values = scales::rescale(colors_words_scale),
                           space = "Lab",
                           aesthetics = "colour",
                           guide = ggplot2::guide_colorbar(direction = "horizontal",
                                                  title.position ="top",
                                                  title = "Cohen's D",
                                                  ggplot2::element_text(color = titles_color))) +

    # set word size range
    ggplot2::scale_size_continuous(range = word_size_range,
                          guide = ggplot2::guide_legend(title= "Frequency",
                                               title.position = "top",
                                               direction = "horizontal",
                                               label.position = "bottom",
                                               ggplot2::element_text(color = titles_color))) +

    ggplot2::labs(y = y_axes_label, x = x_axes_label) +

    # minimal theme, and turning off legends
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position= c("bottom"),
          plot.title = element_text(hjust = 0.5),
          legend.justification = c("right","top"),
          panel.grid.major = ggplot2::element_blank(),
          panel.grid.minor = ggplot2::element_blank(),
          axis.text.y = y_axes_values,
          title = ggplot2::element_text(color = titles_color),
          axis.title.x = ggplot2::element_text(color = titles_color),
          axis.title.y = ggplot2::element_text(color = titles_color))
  plot
}


