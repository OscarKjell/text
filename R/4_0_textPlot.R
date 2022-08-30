
#### Supervised Dimension Projection #####

#' Creates the plot object (except for the legend).
#' @return A plot object.
#' @noRd
textPlotting <- function(word_data_all = word_data_all,
                         word_data_all_yadjusted = word_data_all_yadjusted,
                         only_x_dimension = only_x_dimension,
                         x_axes_1 = x_axes_1,
                         y_axes_1 = y_axes_1,
                         group_embeddings1 = group_embeddings1,
                         group_embeddings2 = group_embeddings2,
                         projection_embedding = projection_embedding,
                         label = words,
                         points_without_words_size = points_without_words_size,
                         points_without_words_alpha = points_without_words_alpha,
                         colour_categories = colour_categories,
                         arrow_transparency = arrow_transparency,
                         scale_x_axes_lim = scale_x_axes_lim,
                         scale_y_axes_lim = scale_y_axes_lim,
                         position_jitter_hight = position_jitter_hight,
                         position_jitter_width = position_jitter_width,
                         word_font = word_font,
                         point_size = point_size,
                         aggregated_embeddings_data = aggregated_embeddings_data,
                         aggregated_point_size = aggregated_point_size,
                         aggregated_shape = aggregated_shape,
                         aggregated_color_G1 = aggregated_color_G1,
                         aggregated_color_G2 = aggregated_color_G2,
                         projection_color = projection_color,
                         word_size_range = word_size_range,
                         # titles
                         title_top = title_top,
                         titles_color = titles_color,
                         x_axes_label = x_axes_label,
                         y_axes_label = y_axes_label,
                         y_axes_values = y_axes_values) {
  plot <-
    # construct ggplot; the !!sym( ) is to  turn the strings into symbols.
    ggplot2::ggplot(data = word_data_all, ggplot2::aes(!!rlang::sym(x_axes_1), !!rlang::sym(y_axes_1), label = words)) +
    ggplot2::geom_point(
      data = word_data_all,
      size = points_without_words_size,
      alpha = points_without_words_alpha,
      ggplot2::aes(color = colour_categories)
    ) +

    # ggrepel geom, make arrows transparent, color by rank, size by n
    ggrepel::geom_text_repel(
      data = word_data_all_yadjusted,
      segment.alpha  = arrow_transparency,
      position = ggplot2::position_jitter(h = position_jitter_hight, w = position_jitter_width),
      ggplot2::aes(color = colour_categories, size = n, family = word_font),
    ) +
    ggplot2::scale_color_identity() +

    # Decide size and color of the points
    ggplot2::geom_point(
      data = word_data_all_yadjusted,
      size = point_size,
      ggplot2::aes(color = colour_categories)
    ) +
    {
      if (group_embeddings1 == TRUE) {
        # Aggregated point help(geom_point)
        ggplot2::geom_point(
          data = aggregated_embeddings_data[1, ],
          size = aggregated_point_size,
          shape = aggregated_shape,
          ggplot2::aes(color = aggregated_color_G1)
        )
      }
    } +

    # Aggregated point 2
    {
      if (group_embeddings2 == TRUE) {
        ggplot2::geom_point(
          data = aggregated_embeddings_data[2, ],
          size = aggregated_point_size,
          shape = aggregated_shape,
          ggplot2::aes(color = aggregated_color_G2)
        )
      }
    } +

    # Projection embedding
    {
      if (projection_embedding == TRUE) {
        ggplot2::geom_point(
          data = aggregated_embeddings_data[3, ],
          size = aggregated_point_size,
          shape = aggregated_shape,
          ggplot2::aes(color = projection_color)
        )
      }
    } +

    # set word size range and the guide
    ggplot2::scale_size_continuous(
      range = word_size_range,
      guide = ggplot2::guide_legend(
        title = "Frequency",
        title.position = "top",
        direction = "horizontal",
        label.position = "bottom",
        ggplot2::element_text(color = titles_color)
      )
    ) +

    # Title
    ggplot2::ggtitle(paste0(title_top)) +
    ggplot2::labs(y = y_axes_label, x = x_axes_label) +

    # Help create possibility to remove y-axes numbers
    ggplot2::scale_x_continuous(limits = scale_x_axes_lim) +
    ggplot2::scale_y_continuous(limits = scale_y_axes_lim) +

    # Minimal theme, and turning off legends
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = c("bottom"),
      plot.title = ggplot2::element_text(hjust = 0.5),
      legend.justification = c("right", "top"),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      axis.text.y = y_axes_values,
      title = ggplot2::element_text(color = titles_color),
      axis.title.x = ggplot2::element_text(color = titles_color),
      axis.title.y = ggplot2::element_text(color = titles_color)
    )
  return(plot)
}


#' Creates the legend for the plot.
#' @return A legend object that can be combined with the plot object.
#' @noRd
textLegend <- function(bivariate_color_codes = bivariate_color_codes,
                       y_axes_1 = y_axes_1,
                       fill = fill,
                       legend_title = legend_title,
                       legend_title_size = legend_title_size,
                       legend_x_axes_label = legend_x_axes_label,
                       legend_y_axes_label = legend_y_axes_label,
                       word_data_all = word_data_all,
                       legend_number_size = legend_number_size,
                       # only_x_dimension = only_x_dimension,
                       titles_color = titles_color) {
  bivariate_color_data <- tibble::tibble(
    "1 - 3" = "#XXXXXX", "2 - 3" = "#XXXXXX", "3 - 3" = "#XXXXXX",
    "1 - 2" = "#XXXXXX", "2 - 2" = "#XXXXXX", "3 - 2" = "#XXXXXX",
    "1 - 1" = "#XXXXXX", "2 - 1" = "#XXXXXX", "3 - 1" = "#XXXXXX"
  )
  bivariate_color_data <- rbind(bivariate_color_data, bivariate_color_codes)
  bivariate_color_data <- bivariate_color_data[-1, ]

  if (y_axes_1 == "only_x_dimension") {
    # Only select 3 colors
    bivariate_color_data <- bivariate_color_data[, c(4, 5, 6)]
    colnames(bivariate_color_data) <- c("1 - 2", "2 - 2", "3 - 2")
    bivariate_color_data
    # Remove the y axes title on the legend
    legend_y_axes_label <- " "
  }

  legend <- bivariate_color_data %>%
    tidyr::gather("group", "fill") %>%
    tidyr::separate(group, into = c("x", "y"), sep = " - ") %>%
    dplyr::mutate(
      x = as.integer(x),
      y = as.integer(y)
    ) %>%
    ggplot2::ggplot(ggplot2::aes(x, y)) +
    ggplot2::geom_tile(ggplot2::aes(fill = fill)) +
    ggplot2::ggtitle(paste0(legend_title)) +
    ggplot2::scale_fill_identity() +
    ggplot2::labs(
      x = legend_x_axes_label,
      y = legend_y_axes_label
    ) +
    ggplot2::theme_void() +
    #    ggplot2::annotate(geom="text", x=2, y=2, label="ns",
    #               color = titles_color, size=legend_number_size)+
    {
      if (y_axes_1 != "only_x_dimension") {
        ggplot2::annotate(
          geom = "text", x = 1, y = 3, label = sum(word_data_all$colour_categories == bivariate_color_codes[1],
            na.rm = T
          ),
          color = titles_color, size = legend_number_size
        )
      }
    } +
    {
      if (y_axes_1 != "only_x_dimension") {
        ggplot2::annotate(
          geom = "text", x = 2, y = 3, label = sum(word_data_all$colour_categories == bivariate_color_codes[2],
            na.rm = T
          ),
          color = titles_color, size = legend_number_size
        )
      }
    } +
    {
      if (y_axes_1 != "only_x_dimension") {
        ggplot2::annotate(
          geom = "text", x = 3, y = 3, label = sum(word_data_all$colour_categories == bivariate_color_codes[3],
            na.rm = T
          ),
          color = titles_color, size = legend_number_size
        )
      }
    } +
    ggplot2::annotate(
      geom = "text", x = 1, y = 2, label = sum(word_data_all$colour_categories == bivariate_color_codes[4],
        na.rm = T
      ),
      color = titles_color, size = legend_number_size
    ) +
    ggplot2::annotate(
      geom = "text", x = 2, y = 2, label = sum(word_data_all$colour_categories == bivariate_color_codes[5],
        na.rm = T
      ),
      color = titles_color, size = legend_number_size
    ) +
    ggplot2::annotate(
      geom = "text", x = 3, y = 2, label = sum(word_data_all$colour_categories == bivariate_color_codes[6],
        na.rm = T
      ),
      color = titles_color, size = legend_number_size
    ) +
    {
      if (y_axes_1 != "only_x_dimension") {
        ggplot2::annotate(
          geom = "text", x = 1, y = 1, label = sum(word_data_all$colour_categories == bivariate_color_codes[7],
            na.rm = T
          ),
          color = titles_color, size = legend_number_size
        )
      }
    } +
    {
      if (y_axes_1 != "only_x_dimension") {
        ggplot2::annotate(
          geom = "text", x = 2, y = 1, label = sum(word_data_all$colour_categories == bivariate_color_codes[8],
            na.rm = T
          ),
          color = titles_color, size = legend_number_size
        )
      }
    } +
    {
      if (y_axes_1 != "only_x_dimension") {
        ggplot2::annotate(
          geom = "text", x = 3, y = 1, label = sum(word_data_all$colour_categories == bivariate_color_codes[9],
            na.rm = T
          ),
          color = titles_color, size = legend_number_size
        )
      }
    } +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, size = legend_title_size + 1),
      title = ggplot2::element_text(color = titles_color),
      axis.title.x = ggplot2::element_text(color = titles_color),
      axis.title = ggplot2::element_text(size = legend_title_size),
      axis.title.y = ggplot2::element_text(angle = 90, color = titles_color)
    ) +
    ggplot2::coord_fixed()
  legend
}





#' Computes the dot product projection for added data.
#' @return Word_data_all_yadjusted with added information for the added words.
#' @noRd
textOwnWordsProjection <- function(word_data = word_data,
                                   word_data_all = word_data_all,
                                   word_data_all_yadjusted = word_data_all_yadjusted,
                                   y_axes = y_axes,
                                   explore_words = explore_words,
                                   explore_words_color = explore_words_color,
                                   explore_words_point = explore_words_point,
                                   explore_words_aggregation = explore_words_aggregation,
                                   space = space,
                                   text_plot_comment = text_plot_comment,
                                   scaling = scaling) {

  # For loop for different batches of added words; i_add_w=1 explore_words = "happy harmony love"
  forloops_add_w <- length(explore_words)
  added_words_information <- list()

  # i_add_w = 1
  for (i_add_w in 1:forloops_add_w) {

    # If using a contextualized language model
    if (is.null(space) == TRUE) {

      # Creating word embeddings for the words.
      model_text <- sub(".*model: ", "", text_plot_comment)
      model_name <- sub(" ;.*", "", model_text)
      layers_text <- sub(".*layers: ", "", text_plot_comment)
      layers_number <- sub(" ;.*", "", layers_text)
      layers_number_split <- stringi::stri_split_boundaries(layers_number,
        type = "word",
        skip_word_none = TRUE,
        skip_word_number = FALSE
      )

      #
      aggregate_layers_text <- sub(".*aggregation_from_layers_to_tokens =  ", "", text_plot_comment)
      aggregate_layers_type <- sub(" aggregation_from_tokens_to_texts.*", "", aggregate_layers_text)

      aggregation_tokens_text <- sub(".*aggregation_from_tokens_to_texts =  ", "", text_plot_comment)
      aggregation_tokens_type <- sub(" tokens_select.*", "", aggregation_tokens_text)

      aggregation_word_text <- sub(".*aggregation_from_tokens_to_word_types =  ", "", text_plot_comment)
      aggregation_word_type <- sub("  ; decontextualize.*", "", aggregation_word_text)

      explore_words_embeddings <- textEmbed(explore_words[i_add_w],
        model = model_name,
        layers = as.numeric(layers_number_split[[1]]),
        aggregation_from_layers_to_tokens = aggregate_layers_type,
        aggregation_from_tokens_to_texts = aggregation_tokens_type,
        aggregation_from_tokens_to_word_types = aggregation_word_type# NULL? # add aggregation_from_tokens_to_word_types and keep_token_embeddings = FALSE
      )
    }
    # If using a static/decontextualized language model
    if (!is.null(space) == TRUE) {
      explore_words_embeddings <- textEmbedStatic(data.frame(explore_words[i_add_w]),
        space = space,
        aggregation_from_tokens_to_texts = explore_words_aggregation
      )
    }

    words <- tibble::as_tibble_col(explore_words_point[i_add_w])
    colnames(words) <- "words"
    n_words <- tibble::as_tibble_col(1)
    colnames(n_words) <- "n"

    # Scaling embeddings before aggregation
    if (scaling == TRUE) {
      singlewords_we_x <- dplyr::select(explore_words_embeddings$word_types, dplyr::starts_with("Dim"))

      # Applying scaling parameters to all the unique word's embeddings
      scale_center_weights <- word_data$background[[1]]$scale_centre.x %>%
        dplyr::slice(rep(1:dplyr::n(), each = nrow(singlewords_we_x)))

      scale_scale_weights <- word_data$background[[1]]$scale_scale.x %>%
        dplyr::slice(rep(1:dplyr::n(), each = nrow(singlewords_we_x)))

      singlewords_we_x_scaled <- tibble::as_tibble((singlewords_we_x - scale_center_weights) / scale_scale_weights)

      singlewords_we_x_scaled_w_n <- bind_cols(explore_words_embeddings$word_types[1:2], singlewords_we_x_scaled)

      #### Create token and aggregated word embeddings ####
      # Aggregate the words
      Aggregated_embedding_added_words <- tibble::as_tibble_row(textEmbeddingAggregation(singlewords_we_x_scaled,
        aggregation = explore_words_aggregation
      ))

      Mean1 <- dplyr::bind_cols(words, n_words, Aggregated_embedding_added_words)
      manual_words_mean1 <- bind_rows(singlewords_we_x_scaled_w_n, Mean1)
    } else {
      # Aggregate the words
      Aggregated_embedding_added_words <- tibble::as_tibble_row(textEmbeddingAggregation(dplyr::select(
        explore_words_embeddings$word_types,
        dplyr::starts_with("Dim")
      ),
      aggregation = explore_words_aggregation
      ))
      Mean1 <- dplyr::bind_cols(words, n_words, Aggregated_embedding_added_words)
      manual_words_mean1 <- bind_rows(explore_words_embeddings$word_types, Mean1)
    }

    #### Project embedding on the x axes ######
    projected_embedding.x <- as.vector(word_data$background[[1]]$Aggregated_word_embedding_group2.x -
      word_data$background[[1]]$Aggregated_word_embedding_group1.x)

    # Position words in relation to Aggregated word embedding
    # Position the embedding; i.e., taking the word embedding subtracted with aggregated word embedding
    embedding_to_anchour_with.x <- tibble::as_tibble((word_data$background[[1]]$Aggregated_word_embedding_group2.x +
      word_data$background[[1]]$Aggregated_word_embedding_group1.x) / 2)
    manual_words_mean1_1.x <- dplyr::select(manual_words_mean1, dplyr::starts_with("Dim"))

    embedding_to_anchour_with.x_df <- embedding_to_anchour_with.x %>%
      dplyr::slice(rep(1:dplyr::n(), each = nrow(manual_words_mean1_1.x)))

    words_positioned_embeddings <- tibble::as_tibble(manual_words_mean1_1.x - embedding_to_anchour_with.x_df)

    projected_embedding.x_df <- tibble::as_tibble(projected_embedding.x) %>%
      slice(rep(1:dplyr::n(), each = nrow(manual_words_mean1)))

    # Project the embeddings using dot product.
    dot_products_observed.x <- rowSums(words_positioned_embeddings * projected_embedding.x_df)

    ### Compare observed dot-product with null
    p_values_dot_prod.x <- purrr::map(as.list(purrr::as_vector(dot_products_observed.x)), p_value_comparing_with_Null,
      word_data$background[[1]]$dot_null_distribution[[1]],
      alternative = "two_sided"
    )

    p_values_dot_prod.x <- unlist(p_values_dot_prod.x)

    #### Project embedding on the Y axes ####

    if (y_axes == TRUE) {
      projected_embedding.y <- as.vector(word_data$background[[2]]$Aggregated_word_embedding_group2.y -
        word_data$background[[2]]$Aggregated_word_embedding_group1.y)
      # Position words in relation to Aggregated word embedding
      # Position the embedding; i.e., taking the word embedding subtracted with aggregated word embedding
      embedding_to_anchour_with.y <- tibble::as_tibble((word_data$background[[2]]$Aggregated_word_embedding_group2.y +
        word_data$background[[2]]$Aggregated_word_embedding_group1.y) / 2)
      manual_words_mean1_1.y <- dplyr::select(manual_words_mean1, dplyr::starts_with("Dim"))

      embedding_to_anchour_with.y_df <- embedding_to_anchour_with.y %>%
        dplyr::slice(rep(1:dplyr::n(), each = nrow(manual_words_mean1_1.y)))

      words_positioned_embeddings <- tibble::as_tibble(manual_words_mean1_1.y - embedding_to_anchour_with.y_df)

      projected_embedding.y_df <- tibble::as_tibble(projected_embedding.y) %>%
        slice(rep(1:dplyr::n(), each = nrow(manual_words_mean1)))

      # Project the embeddings using dot product.
      dot_products_observed.y <- rowSums(words_positioned_embeddings * projected_embedding.y_df)

      ### Compare observed dot-product with null
      p_values_dot_prod.y <- purrr::map(as.list(purrr::as_vector(dot_products_observed.y)), p_value_comparing_with_Null,
        word_data$background[[2]]$dot_null_distribution[[1]],
        alternative = "two_sided"
      )

      p_values_dot_prod.y <- unlist(p_values_dot_prod.y)
    }

    #### Sort out dataframe for textOwnWordsProjection ####
    explore_words_results <- manual_words_mean1[1:2]
    explore_words_results$x_plotted <- dot_products_observed.x
    explore_words_results$p_values_x <- p_values_dot_prod.x
    explore_words_results$adjusted_p_values.x <- p_values_dot_prod.x

    if (y_axes == TRUE) {
      explore_words_results$y_plotted <- dot_products_observed.y
      explore_words_results$p_values_y <- p_values_dot_prod.y
      explore_words_results$adjusted_p_values.y <- p_values_dot_prod.y
    }

    explore_words_results$colour_categories <- explore_words_color[i_add_w]
    # TODO; should not have to print extreme?
    explore_words_results$extremes_all_x <- rep(NA, nrow(explore_words_results))
    explore_words_results$n <- rep(mean(word_data_all$n), nrow(explore_words_results))
    explore_words_results$n.percent <- rep(0.5, nrow(explore_words_results))
    explore_words_results$n_g2.x <- rep(5, nrow(explore_words_results))
    explore_words_results$N_participant_responses <- rep(
      max(word_data_all$N_participant_responses),
      nrow(explore_words_results)
    )

    added_words_information[[i_add_w]] <- explore_words_results
  }
  added_words_information_unlist <- dplyr::bind_rows(added_words_information)
  word_data_all_yadjusted <- dplyr::bind_rows(word_data_all_yadjusted, added_words_information_unlist)

  return(word_data_all_yadjusted)
}


#' Computes the dot product projection for added data.
#' @return Word_data_all_yadjusted with added infomration for the added words.
#' @noRd
textOwnWordPrediction <- function(word_data = word_data,
                                  word_data_all = word_data_all,
                                  word_data_all_yadjusted = word_data_all_yadjusted,
                                  y_axes = y_axes,
                                  explore_words = explore_words,
                                  explore_words_color = explore_words_color,
                                  explore_words_point = explore_words_point,
                                  explore_words_aggregation = explore_words_aggregation,
                                  space = space,
                                  text_plot_comment = text_plot_comment,
                                  scaling = scaling) {

  # For loop for different batches of added words; i_add_w=1 explore_words = "happy harmony love"
  forloops_add_w <- length(explore_words)
  added_words_information <- list()


  for (i_add_w in 1:forloops_add_w) {

    # If using a contextualized language model
    if (is.null(space) == TRUE) {

      # Creating word embeddings for the words.
      model_text <- sub(".*model: ", "", text_plot_comment)
      model_name <- sub(" ; layer.*", "", model_text)
      layers_text <- sub(".*layers: ", "", text_plot_comment)
      layers_number <- sub(" ; word_type_embeddings.*", "", layers_text)
      layers_number_split <- stringi::stri_split_boundaries(layers_number,
        type = "word",
        skip_word_none = TRUE,
        skip_word_number = FALSE
      )

      #
      aggregate_layers_text <- sub(".*aggregation_from_layers_to_tokens =  ", "", text_plot_comment)
      aggregate_layers_type <- sub(" aggregation_from_tokens_to_texts.*", "", aggregate_layers_text)

      aggregation_tokens_text <- sub(".*aggregation_from_tokens_to_texts =  ", "", text_plot_comment)
      aggregation_tokens_type <- sub(" tokens_select.*", "", aggregation_tokens_text)

      aggregation_word_text <- sub(".*aggregation_from_tokens_to_word_types =  ", "", text_plot_comment)
      aggregation_word_type <- sub("  ; decontextualize.*", "", aggregation_word_text)

      explore_words_embeddings <- textEmbed(explore_words[i_add_w],
                                            model = model_name,
                                            layers = as.numeric(layers_number_split[[1]]),
                                            aggregation_from_layers_to_tokens = aggregate_layers_type,
                                            aggregation_from_tokens_to_texts = aggregation_tokens_type,
                                            aggregation_from_tokens_to_word_types = aggregation_word_type# NULL? # add aggregation_from_tokens_to_word_types and keep_token_embeddings = FALSE
      )

    }
    # If using a static/decontextualized language model
    if (!is.null(space) == TRUE) {
      explore_words_embeddings <- textEmbedStatic(data.frame(explore_words[i_add_w]),
        space = space,
        aggregation_from_tokens_to_texts = explore_words_aggregation
      )
    }

    #### Create token and aggregated word embeddings ####
    words <- tibble::as_tibble_col(explore_words_point[i_add_w])
    colnames(words) <- "words"
    n_words <- tibble::as_tibble_col(1)
    colnames(n_words) <- "n"

    # Aggregate the words
    Aggregated_embedding_added_words <- tibble::as_tibble_row(textEmbeddingAggregation(dplyr::select(
      explore_words_embeddings$word_types,
      dplyr::starts_with("Dim")
    ),
    aggregation = explore_words_aggregation
    ))
    Mean1 <- dplyr::bind_cols(words, n_words, Aggregated_embedding_added_words)
    manual_words_mean1 <- bind_rows(explore_words_embeddings$word_types, Mean1)


    prediction_x <- textPredict(word_data$model_x, manual_words_mean1)$.pred # , ...

    # Creating p-value column  TO DO: implement a real p-value
    p_value_x <- rep(1, length(prediction_x))

    if (y_axes == TRUE) {
      prediction_y <- textPredict(word_data$model_y, manual_words_mean1)$.pred # , ...

      # Creating p-value column  TO DO: implement a real p-value
      p_value_y <- rep(1, length(prediction_y))
    }

    #### Sort out df for textOwnWordPrediction ####
    explore_words_results <- manual_words_mean1[1:2]
    explore_words_results$x_plotted <- prediction_x
    explore_words_results$p_values_x <- p_value_x
    explore_words_results$adjusted_p_values.x <- p_value_x

    if (y_axes == TRUE) {
      explore_words_results$y_plotted <- prediction_y
      explore_words_results$p_values_y <- p_value_y
      explore_words_results$adjusted_p_values.y <- p_value_y
    }

    explore_words_results$colour_categories <- explore_words_color[i_add_w]
    explore_words_results$extremes_all_x <- rep(NA, nrow(explore_words_results))
    explore_words_results$n <- rep(mean(word_data_all$n), nrow(explore_words_results))
    explore_words_results$n.percent <- rep(0.5, nrow(explore_words_results))
    explore_words_results$n_g2.x <- rep(5, nrow(explore_words_results))

    added_words_information[[i_add_w]] <- explore_words_results
  }
  added_words_information_unlist <- dplyr::bind_rows(added_words_information)
  word_data_all_yadjusted <- dplyr::bind_rows(word_data_all_yadjusted, added_words_information_unlist)

  return(word_data_all_yadjusted)
}


#' Find out plot type to be plotted and adjust word_data columns accordingly.
#' @return word_data with generically called columns that can run in textPlot.
#' @noRd
adjust_for_plot_type <- function(word_data, y_axes) {
  type_text <- sub(".*type = ", "", comment(word_data))
  type_name <- sub(" .*", "", type_text)

  # Making column names generic across different input

  if (type_name == "textWordPrediction") {
    colnames(word_data$word_data)[which(names(word_data$word_data) == "p_value_w_pred_x")] <- "p_values_x"
    colnames(word_data$word_data)[which(names(word_data$word_data) == "embedding_based_prediction_x")] <- "x_plotted"
    if (y_axes) {
      colnames(word_data$word_data)[which(names(word_data$word_data) == "p_value_w_pred_y")] <- "p_values_y"
      colnames(word_data$word_data)[which(names(word_data$word_data) == "embedding_based_prediction_y")] <- "y_plotted"
    }
  }

  if (type_name == "textProjection") {
    colnames(word_data$word_data)[which(names(word_data$word_data) == "p_values_dot.x")] <- "p_values_x"
    colnames(word_data$word_data)[which(names(word_data$word_data) == "dot.x")] <- "x_plotted"
    if (y_axes) {
      colnames(word_data$word_data)[which(names(word_data$word_data) == "p_values_dot.y")] <- "p_values_y"
      colnames(word_data$word_data)[which(names(word_data$word_data) == "dot.y")] <- "y_plotted"
    }
  }
  return(word_data)
}


#' Plot words from textProjection() or textWordPrediction().
#' @param word_data Dataframe from textProjection
#' @param k_n_words_to_test Select the k most frequent words to significance
#' test (k = sqrt(100*N); N = number of participant responses). Default = TRUE.
#' @param min_freq_words_test Select words to significance test that have occurred
#' at least min_freq_words_test (default = 1).
#' @param min_freq_words_plot Select words to plot that has occurred at
#' least min_freq_words_plot times.
#' @param plot_n_words_square Select number of significant words in each square
#' of the figure to plot. The significant words, in each square is selected
#' according to most frequent words.
#' @param plot_n_words_p Number of significant words to plot on each(positive
#' and negative) side of the x-axes and y-axes, (where duplicates are removed);
#' selects first according to lowest p-value and then according to frequency. Hence, on a two
#' dimensional plot it is possible that plot_n_words_p = 1 yield 4 words.
#' @param plot_n_word_extreme Number of words that are extreme on Supervised Dimension
#' Projection per dimension. (i.e., even if not significant; per dimensions,
#' where duplicates are removed).
#' @param plot_n_word_frequency Number of words based on being most frequent.
#' (i.e., even if not significant).
#' @param plot_n_words_middle Number of words plotted that are in the middle in Supervised
#' Dimension Projection score (i.e., even if not significant;  per dimensions, where duplicates are removed).
#' @param title_top Title (default "  ")
#' @param titles_color Color for all the titles (default: "#61605e")
# @param x_axes If TRUE, plotting on the x_axes.
#' @param y_axes If TRUE, also plotting on the y-axes (default is FALSE). Also plotting on
#' y-axes produces a two dimension 2-dimensional plot, but the textProjection function has to
#' have had a variable on the y-axes.
#' @param p_alpha Alpha (default = .05).
#' @param p_adjust_method Method to adjust/correct p-values for multiple comparisons
#' (default = "holm"; see also "none", "hochberg", "hommel", "bonferroni", "BH", "BY",  "fdr").
#' @param x_axes_label Label on the x-axes.
#' @param y_axes_label Label on the y-axes.
#' @param scale_x_axes_lim Manually set the length of the x-axes (default = NULL, which uses
#' ggplot2::scale_x_continuous(limits = scale_x_axes_lim); change e.g., by trying c(-5, 5)).
#' @param scale_y_axes_lim Manually set the length of the y-axes (default = NULL; which uses
#' ggplot2::scale_y_continuous(limits = scale_y_axes_lim); change e.g., by trying c(-5, 5)).
#' @param word_font Font type (default: NULL).
#' @param bivariate_color_codes The different colors of the words. Note that, at the moment, two
#' squares should not have the exact same colour-code because the numbers within the squares of the
#' legend will then be aggregated (and show the same, incorrect  value).
#' (default: c("#398CF9", "#60A1F7", "#5dc688",
#' "#e07f6a", "#EAEAEA", "#40DD52",
#' "#FF0000", "#EA7467", "#85DB8E")).
#' @param word_size_range Vector with minimum and maximum font size (default: c(3, 8)).
#' @param position_jitter_hight Jitter height (default: .0).
#' @param position_jitter_width Jitter width (default: .03).
#' @param point_size Size of the points indicating the words' position (default: 0.5).
#' @param arrow_transparency Transparency of the lines between each word and point (default: 0.1).
#' @param points_without_words_size Size of the points not linked with a words
#' (default is to not show it, i.e., 0).
#' @param points_without_words_alpha Transparency of the points not linked with a words
#' (default is to not show it, i.e., 0).
#' @param legend_title Title on the color legend (default: "(SDP)".
#' @param legend_x_axes_label Label on the color legend (default: "(x)".
#' @param legend_y_axes_label Label on the color legend (default: "(y)".
#' @param legend_x_position Position on the x coordinates of the color legend (default: 0.02).
#' @param legend_y_position Position on the y coordinates of the color legend (default: 0.05).
#' @param legend_h_size Height of the color legend (default 0.15).
#' @param legend_w_size Width of the color legend (default 0.15).
#' @param legend_title_size Font size (default: 7).
#' @param legend_number_size Font size of the values in the legend (default: 2).
#' @param group_embeddings1 Shows a point representing the aggregated word embedding
#' for group 1 (default = FALSE).
#' @param group_embeddings2 Shows a point representing the aggregated word embedding
#' for group 2 (default = FALSE).
#' @param projection_embedding Shows a point representing the aggregated direction
#' embedding (default = FALSE).
#' @param aggregated_point_size Size of the points representing the group_embeddings1,
#' group_embeddings2 and projection_embedding.
#' @param aggregated_shape Shape type of the points representing the group_embeddings1,
#' group_embeddings2 and projection_embedding.
#' @param aggregated_color_G1 Color
#' @param aggregated_color_G2 Color
#' @param projection_color Color
#' @param seed Set different seed.
#' @param explore_words Explore where specific words are positioned in the embedding space.
#' For example, c("happy content", "sad down").
#' @param explore_words_color Specify the color(s) of the words being explored.
#' For example c("#ad42f5", "green")
#' @param explore_words_point Specify the names of the point for the aggregated word embeddings
#' of all the explored words.
#' @param explore_words_aggregation Specify how to aggregate the word embeddings of
#' the explored words.
#' @param remove_words manually remove words from the plot (which is done just before the
#' words are plotted so that the remove_words are part of previous counts/analyses).
#' @param space Provide a semantic space if using static embeddings and wanting to explore words.
#' @param n_contrast_group_color Set color to words that have higher frequency (N)
#' on the other opposite side of its dot product projection (default = NULL).
#' @param n_contrast_group_remove Remove words that have higher frequency (N) on the other
#' opposite side of its dot product projection (default = FALSE).
#' @param scaling Scaling word embeddings before aggregation.
#' @return A 1- or 2-dimensional word plot, as well as tibble with processed data used
#' to plot.
#' @examples
#' # The test-data included in the package is called: DP_projections_HILS_SWLS_100
#'
#' # Supervised Dimension Projection Plot
#' plot_projection <- textPlot(
#'   word_data = DP_projections_HILS_SWLS_100,
#'   k_n_words_to_test = FALSE,
#'   min_freq_words_test = 1,
#'   plot_n_words_square = 3,
#'   plot_n_words_p = 3,
#'   plot_n_word_extreme = 1,
#'   plot_n_word_frequency = 1,
#'   plot_n_words_middle = 1,
#'   y_axes = FALSE,
#'   p_alpha = 0.05,
#'   title_top = "Supervised Dimension Projection (SDP)",
#'   x_axes_label = "Low vs. High HILS score",
#'   y_axes_label = "Low vs. High SWLS score",
#'   p_adjust_method = "bonferroni",
#'   scale_y_axes_lim = NULL
#' )
#' plot_projection
#'
#' names(DP_projections_HILS_SWLS_100)
#' @seealso see \code{\link{textProjection}}
#' @importFrom tibble as_tibble tibble
#' @importFrom dplyr row_number slice mutate mutate_if bind_rows group_by summarize left_join %>% n
#' @importFrom tidyr gather separate
#' @importFrom ggplot2 position_jitter element_text element_blank coord_fixed theme
#' theme_void theme_minimal aes labs scale_color_identity
#' @importFrom rlang sym
#' @importFrom cowplot ggdraw draw_plot
#' @importFrom purrr as_vector
#' @importFrom stringi stri_split_boundaries
#' @export
textPlot <- function(word_data,
                     k_n_words_to_test = FALSE,
                     min_freq_words_test = 1,
                     min_freq_words_plot = 1,
                     plot_n_words_square = 3,
                     plot_n_words_p = 5,
                     plot_n_word_extreme = 5,
                     plot_n_word_frequency = 5,
                     plot_n_words_middle = 5,
                     titles_color = "#61605e",
                     # x_axes = TRUE,
                     y_axes = FALSE,
                     p_alpha = 0.05,
                     p_adjust_method = "none",
                     title_top = "Supervised Dimension Projection",
                     x_axes_label = "Supervised Dimension Projection (SDP)",
                     y_axes_label = "Supervised Dimension Projection (SDP)",
                     scale_x_axes_lim = NULL,
                     scale_y_axes_lim = NULL,
                     word_font = NULL,
                     bivariate_color_codes = c(
                       "#398CF9", "#60A1F7", "#5dc688",
                       "#e07f6a", "#EAEAEA", "#40DD52",
                       "#FF0000", "#EA7467", "#85DB8E"
                     ),
                     word_size_range = c(3, 8),
                     position_jitter_hight = .0,
                     position_jitter_width = .03,
                     point_size = 0.5,
                     arrow_transparency = 0.1,
                     points_without_words_size = 0.2,
                     points_without_words_alpha = 0.2,
                     legend_title = "SDP",
                     legend_x_axes_label = "x",
                     legend_y_axes_label = "y",
                     legend_x_position = 0.02,
                     legend_y_position = 0.02,
                     legend_h_size = 0.2,
                     legend_w_size = 0.2,
                     legend_title_size = 7,
                     legend_number_size = 2,
                     group_embeddings1 = FALSE,
                     group_embeddings2 = FALSE,
                     projection_embedding = FALSE,
                     aggregated_point_size = 0.8,
                     aggregated_shape = 8,
                     aggregated_color_G1 = "black",
                     aggregated_color_G2 = "black",
                     projection_color = "blue",
                     seed = 1005,
                     explore_words = NULL,
                     explore_words_color = "#ad42f5",
                     explore_words_point = "ALL_1",
                     explore_words_aggregation = "mean",
                     remove_words = NULL,
                     n_contrast_group_color = NULL,
                     n_contrast_group_remove = FALSE,
                     space = NULL,
                     scaling = FALSE) {


  ##### Comment to be saved ####
  text_plot_comment <- paste(
    "INFORMATION ABOUT THE PROJECTION",
    comment(word_data),
    "INFORMATION ABOUT THE PLOT",
    "word_data =", substitute(word_data),
    "k_n_words_to_test =", k_n_words_to_test,
    "min_freq_words_test =", min_freq_words_test,
    "min_freq_words_plot =", min_freq_words_plot,
    "plot_n_words_square =", plot_n_words_square,
    "plot_n_words_p =", plot_n_words_p,
    "plot_n_word_extreme =", plot_n_word_extreme,
    "plot_n_word_frequency =", plot_n_word_frequency,
    "plot_n_words_middle =", plot_n_words_middle,
    "y_axes =", y_axes,
    "p_alpha =", p_alpha,
    "p_adjust_method =", p_adjust_method,
    "bivariate_color_codes =", paste(bivariate_color_codes, collapse = " "),
    "word_size_range =", paste(word_size_range, sep = "-", collapse = " - "),
    "position_jitter_hight =", position_jitter_hight,
    "position_jitter_width =", position_jitter_width,
    "point_size =", point_size,
    "arrow_transparency =", point_size,
    "points_without_words_size =", points_without_words_size,
    "points_without_words_alpha =", points_without_words_alpha,
    "legend_x_position =", legend_x_position,
    "legend_y_position =", legend_y_position,
    "legend_h_size =", legend_h_size,
    "legend_w_size =", legend_w_size,
    "legend_title_size =", legend_title_size,
    "legend_number_size =", legend_number_size
  )

  set.seed(seed)
  plot_type_text <- sub(".*type = ", "", comment(word_data))
  plot_type_name <- sub(" .*", "", plot_type_text)

  #### Sorting out axes ####
  # Renaming column names from different types of word_data
  word_data <- adjust_for_plot_type(
    word_data = word_data,
    y_axes = y_axes
  )
  x_axes_1 <- "x_plotted"
  p_values_x <- "p_values_x"

  if (y_axes == TRUE) {
    y_axes_1 <- "y_plotted"
    p_values_y <- "p_values_y"
    y_axes_values_hide <- FALSE
  } else if (y_axes == FALSE) {
    y_axes_1 <- NULL
    p_values_y <- NULL
    y_axes_values_hide <- TRUE
  }

  #### Removing words MANUALY #######

  if (!is.null(remove_words)) {
    word_data$word_data <- word_data$word_data %>% dplyr::filter(!words %in% remove_words)
  }

  #### Selecting words to plot ####
  # Computing adjusted p-values with those words selected by min_freq_words_test
  word_data_padjusted <- word_data$word_data[word_data$word_data$n >= min_freq_words_test, ]

  # Selected Aggregated points
  aggregated_embeddings_data <- word_data$word_data[word_data$word_data$n == 0, ]

  # View(word_data_padjusted) Computing adjusted p-values with those words selected by: k = sqrt(100*N)
  if (k_n_words_to_test == TRUE) {
    words_k <- sqrt(100 * word_data$word_data$N_participant_responses[1])
    word_data_padjusted <- word_data_padjusted %>%
      dplyr::arrange(-n) %>%
      dplyr::slice(0:words_k)
  }
  # word_data_padjusted$p_values_x
  word_data_padjusted$adjusted_p_values.x <- stats::p.adjust(purrr::as_vector(word_data_padjusted[, "p_values_x"]),
    method = p_adjust_method
  )
  word_data1 <- dplyr::left_join(word_data$word_data, word_data_padjusted[, c("words", "adjusted_p_values.x")],
    by = "words"
  )
  # word_data$adjusted_p_values.x

  if (is.null(y_axes_1) == FALSE) {
    # Computing adjusted p-values
    word_data1_padjusted_y <- word_data1[word_data1$n >= min_freq_words_test, ]
    word_data1_padjusted_y$adjusted_p_values.y <- stats::p.adjust(
      purrr::as_vector(word_data1_padjusted_y[, "p_values_y"]),
      method = p_adjust_method
    )
    word_data1 <- dplyr::left_join(word_data1, word_data1_padjusted_y[, c("words", "adjusted_p_values.y")], by = "words")
  }

  # Select only min_freq_words_plot to plot (i.e., after correction of multiple comparison for sig. test)
  word_data1 <- word_data1[word_data1$n >= min_freq_words_plot, ]

  #  Select only words based on square-position; and then top frequency in each "square"
  # (see legend) plot_n_words_square
  if (is.null(y_axes_1) == TRUE) {
    word_data1 <- word_data1 %>%
      dplyr::mutate(square_categories = dplyr::case_when(
        x_plotted < 0 & adjusted_p_values.x < p_alpha ~ 1,
        x_plotted < 0 & adjusted_p_values.x > p_alpha ~ 2,
        x_plotted > 0 & adjusted_p_values.x < p_alpha ~ 3
      ))

    data_p_sq1 <- word_data1[word_data1$square_categories == 1, ] %>%
      dplyr::arrange(-n) %>%
      dplyr::slice(0:plot_n_words_square)

    data_p_sq3 <- word_data1[word_data1$square_categories == 3, ] %>%
      dplyr::arrange(-n) %>%
      dplyr::slice(0:plot_n_words_square)

    data_p_sq_all <- rbind(data_p_sq1, data_p_sq3) # data_p_sq2,
  }

  if (is.null(y_axes_1) == FALSE) {
    # Categorize words to apply specific color plot_n_words_square=1
    word_data1 <- word_data1 %>%
      dplyr::mutate(square_categories = dplyr::case_when(
        x_plotted < 0 & adjusted_p_values.x < p_alpha & y_plotted > 0 & adjusted_p_values.y < p_alpha ~ 1,
        adjusted_p_values.x > p_alpha & y_plotted > 0 & adjusted_p_values.y < p_alpha ~ 2,
        x_plotted > 0 & adjusted_p_values.x < p_alpha & y_plotted > 0 & adjusted_p_values.y < p_alpha ~ 3,
        x_plotted < 0 & adjusted_p_values.x < p_alpha & adjusted_p_values.y > p_alpha ~ 4,
        adjusted_p_values.x > p_alpha & adjusted_p_values.y > p_alpha ~ 5,
        x_plotted > 0 & adjusted_p_values.x < p_alpha & adjusted_p_values.y > p_alpha ~ 6,
        x_plotted < 0 & adjusted_p_values.x < p_alpha & y_plotted < 0 & adjusted_p_values.y < p_alpha ~ 7,
        adjusted_p_values.x > p_alpha & y_plotted < 0 & adjusted_p_values.y < p_alpha ~ 8,
        x_plotted > 0 & adjusted_p_values.x < p_alpha & y_plotted < 0 & adjusted_p_values.y < p_alpha ~ 9
      ))

    data_p_sq1 <- word_data1[word_data1$square_categories == 1, ] %>%
      dplyr::arrange(-n) %>%
      dplyr::slice(0:plot_n_words_square)
    data_p_sq2 <- word_data1[word_data1$square_categories == 2, ] %>%
      dplyr::arrange(-n) %>%
      dplyr::slice(0:plot_n_words_square)
    data_p_sq3 <- word_data1[word_data1$square_categories == 3, ] %>%
      dplyr::arrange(-n) %>%
      dplyr::slice(0:plot_n_words_square)
    data_p_sq4 <- word_data1[word_data1$square_categories == 4, ] %>%
      dplyr::arrange(-n) %>%
      dplyr::slice(0:plot_n_words_square)
    #  data_p_sq5
    data_p_sq6 <- word_data1[word_data1$square_categories == 6, ] %>%
      dplyr::arrange(-n) %>%
      dplyr::slice(0:plot_n_words_square)
    data_p_sq7 <- word_data1[word_data1$square_categories == 7, ] %>%
      dplyr::arrange(-n) %>%
      dplyr::slice(0:plot_n_words_square)
    data_p_sq8 <- word_data1[word_data1$square_categories == 8, ] %>%
      dplyr::arrange(-n) %>%
      dplyr::slice(0:plot_n_words_square)
    data_p_sq9 <- word_data1[word_data1$square_categories == 9, ] %>%
      dplyr::arrange(-n) %>%
      dplyr::slice(0:plot_n_words_square)

    data_p_sq_all <- rbind(
      data_p_sq1, data_p_sq2, data_p_sq3,
      data_p_sq4, data_p_sq6, # data_p_sq5,
      data_p_sq7, data_p_sq8, data_p_sq9
    )
  }


  # Select only words below alpha; and then top x_plotted
  data_p_x_neg <- word_data1 %>%
    dplyr::filter(adjusted_p_values.x < p_alpha) %>%
    dplyr::arrange(x_plotted) %>%
    dplyr::slice(0:plot_n_words_p)

  data_p_x_pos <- word_data1 %>%
    dplyr::filter(adjusted_p_values.x < p_alpha) %>%
    dplyr::arrange(-x_plotted) %>%
    dplyr::slice(0:plot_n_words_p)

  # Select plot_n_word_extreme and Select plot_n_word_frequency
  word_data1_extrem_max_x <- word_data1 %>%
    dplyr::arrange(-x_plotted) %>%
    dplyr::slice(0:plot_n_word_extreme)

  word_data1_extrem_min_x <- word_data1 %>%
    dplyr::arrange(x_plotted) %>%
    dplyr::slice(0:plot_n_word_extreme)

  word_data1_frequency_x <- word_data1 %>%
    dplyr::arrange(-n) %>%
    dplyr::slice(0:plot_n_word_frequency)

  # Select the middle range, order according to frequency and then select the plot_n_words_middle = 5
  mean_m_sd_x <- mean(word_data1$x_plotted, na.rm = TRUE) - (sd(word_data1$x_plotted, na.rm = TRUE) / 10)
  mean_p_sd_x <- mean(word_data1$x_plotted, na.rm = TRUE) + (sd(word_data1$x_plotted, na.rm = TRUE) / 10)
  word_data1_middle_x <- word_data1 %>%
    dplyr::filter(dplyr::between(word_data1$x_plotted, mean_m_sd_x, mean_p_sd_x)) %>%
    dplyr::arrange(-n) %>%
    dplyr::slice(0:plot_n_words_middle)

  word_data1_x <- word_data1 %>%
    dplyr::left_join(data_p_sq_all %>%
      dplyr::transmute(words, check_p_square = 1), by = "words") %>%
    dplyr::left_join(data_p_x_neg %>%
      dplyr::transmute(words, check_p_x_neg = 1), by = "words") %>%
    dplyr::left_join(data_p_x_pos %>%
      dplyr::transmute(words, check_p_x_pos = 1), by = "words") %>%
    dplyr::left_join(word_data1_extrem_max_x %>%
      dplyr::transmute(words, check_extreme_max_x = 1), by = "words") %>%
    dplyr::left_join(word_data1_extrem_min_x %>%
      dplyr::transmute(words, check_extreme_min_x = 1), by = "words") %>%
    dplyr::left_join(word_data1_frequency_x %>%
      dplyr::transmute(words, check_extreme_frequency_x = 1), by = "words") %>%
    dplyr::left_join(word_data1_middle_x %>%
      dplyr::transmute(words, check_middle_x = 1), by = "words") %>%
    dplyr::mutate(extremes_all_x = rowSums(cbind(
      check_p_square, check_p_x_neg, check_p_x_pos, check_extreme_max_x, check_extreme_min_x,
      check_extreme_frequency_x, check_middle_x
    ), na.rm = T))


  ###### Sort words for y-axes.
  if (is.null(y_axes_1) == FALSE) {
    # Computing adjusted p-values
    # Select only words below alpha; and then top x_plotted
    data_p_y_neg <- word_data1 %>%
      dplyr::filter(adjusted_p_values.y < p_alpha) %>%
      dplyr::arrange(y_plotted) %>%
      dplyr::slice(0:plot_n_words_p)

    data_p_y_pos <- word_data1 %>%
      dplyr::filter(adjusted_p_values.y < p_alpha) %>%
      dplyr::arrange(-y_plotted) %>%
      dplyr::slice(0:plot_n_words_p)

    # Select plot_n_word_extreme and Select plot_n_word_frequency
    word_data1_extrem_max_y <- word_data1 %>%
      dplyr::arrange(-y_plotted) %>%
      dplyr::slice(0:plot_n_word_extreme)

    word_data1_extrem_min_y <- word_data1 %>%
      dplyr::arrange(y_plotted) %>%
      dplyr::slice(0:plot_n_word_extreme)

    word_data1_frequency_y <- word_data1 %>%
      dplyr::arrange(-n) %>%
      dplyr::slice(0:plot_n_word_frequency)

    # Select the middle range, order according to frequency and then select the plot_n_words_middle =5
    mean_m_sd_y <- mean(word_data1$y_plotted, na.rm = TRUE) - (sd(word_data1$y_plotted, na.rm = TRUE) / 10)
    mean_p_sd_y <- mean(word_data1$y_plotted, na.rm = TRUE) + (sd(word_data1$y_plotted, na.rm = TRUE) / 10)
    word_data1_middle_y <- word_data1 %>%
      dplyr::filter(dplyr::between(word_data1$y_plotted, mean_m_sd_y, mean_p_sd_y)) %>%
      dplyr::arrange(-n) %>%
      dplyr::slice(0:plot_n_words_middle) # TODO selecting on frequency again. perhaps point to have exact middle?

    word_data_all <- word_data1_x %>%
      dplyr::left_join(data_p_y_pos %>%
        dplyr::transmute(words, check_p_y_pos = 1), by = "words") %>%
      dplyr::left_join(data_p_y_neg %>%
        dplyr::transmute(words, check_p_y_neg = 1), by = "words") %>%
      dplyr::left_join(word_data1_extrem_max_y %>%
        dplyr::transmute(words, check_extreme_max_y = 1), by = "words") %>%
      dplyr::left_join(word_data1_extrem_min_y %>%
        dplyr::transmute(words, check_extreme_min_y = 1), by = "words") %>%
      dplyr::left_join(word_data1_frequency_y %>%
        dplyr::transmute(words, check_extreme_frequency_y = 1), by = "words") %>%
      dplyr::left_join(word_data1_middle_y %>%
        dplyr::transmute(words, check_middle_y = 1), by = "words") %>%
      dplyr::mutate(extremes_all_y = rowSums(cbind(
        check_p_y_neg, check_p_y_pos, check_extreme_max_y, check_extreme_min_y,
        check_extreme_frequency_y, check_middle_y
      ), na.rm = T)) %>%
      dplyr::mutate(extremes_all = rowSums(cbind(extremes_all_x, extremes_all_y), na.rm = T))


    # Categorize words to apply specific color
    word_data_all <- word_data_all %>%
      dplyr::mutate(colour_categories = dplyr::case_when(
        x_plotted < 0 & adjusted_p_values.x < p_alpha & y_plotted > 0 & adjusted_p_values.y < p_alpha ~ bivariate_color_codes[1],
        adjusted_p_values.x > p_alpha & y_plotted > 0 & adjusted_p_values.y < p_alpha ~ bivariate_color_codes[2],
        x_plotted > 0 & adjusted_p_values.x < p_alpha & y_plotted > 0 & adjusted_p_values.y < p_alpha ~ bivariate_color_codes[3],
        x_plotted < 0 & adjusted_p_values.x < p_alpha & adjusted_p_values.y > p_alpha ~ bivariate_color_codes[4],
        adjusted_p_values.x > p_alpha & adjusted_p_values.y > p_alpha ~ bivariate_color_codes[5],
        x_plotted > 0 & adjusted_p_values.x < p_alpha & adjusted_p_values.y > p_alpha ~ bivariate_color_codes[6],
        x_plotted < 0 & adjusted_p_values.x < p_alpha & y_plotted < 0 & adjusted_p_values.y < p_alpha ~ bivariate_color_codes[7],
        adjusted_p_values.x > p_alpha & y_plotted < 0 & adjusted_p_values.y < p_alpha ~ bivariate_color_codes[8],
        x_plotted > 0 & adjusted_p_values.x < p_alpha & y_plotted < 0 & adjusted_p_values.y < p_alpha ~ bivariate_color_codes[9]
      ))
  }


  if (is.null(y_axes_1) == TRUE) {
    word_data_all <- word_data1_x %>%
      dplyr::mutate(colour_categories = dplyr::case_when(
        x_plotted < 0 & adjusted_p_values.x < p_alpha ~ bivariate_color_codes[4],
        # x_plotted < 0 & adjusted_p_values.x > p_alpha ~ bivariate_color_codes[5],
        # Some adjusted_p_values.x has NA becasue they where not tested as multiple input
        # (this is because min_frequency selects out before)
        adjusted_p_values.x > p_alpha | is.na(adjusted_p_values.x) ~ bivariate_color_codes[5],
        x_plotted > 0 & adjusted_p_values.x < p_alpha ~ bivariate_color_codes[6]
      ))
  }

  #### Colorize words that are more frequent on the opposite side of the dot product projection ####
  if (is.character(n_contrast_group_color) == TRUE) {

    # Select words with MORE words in G1 and POSITIVE dot product (i.e., remove words that are
    # more represented in the opposite group of its dot product projection)
    word_data_all$colour_categories[(abs(word_data_all$n_g1.x) > abs(word_data_all$n_g2.x) &
      word_data_all$x_plotted > 0)] <- n_contrast_group_color

    # Select words with MORE words in G2 and POSITIVE dot product (i.e., remove words that are more
    # represented in the opposite group of its dot product projection)
    word_data_all$colour_categories[(abs(word_data_all$n_g1.x) < abs(word_data_all$n_g2.x) &
      word_data_all$x_plotted < 0)] <- n_contrast_group_color
  }

  #### Remove more frequent words on the opposite side of the dot product projection ####
  if (n_contrast_group_remove == TRUE) {
    word_data_all1 <- word_data_all %>%
      # Select words with MORE words in G1 and NEGATIVE dot product (i.e., do not select words
      # that are more represented in the opposite group of its dot product projection)
      filter((abs(n_g1.x) > abs(n_g2.x) &
        x_plotted < 0))

    word_data_all2 <- word_data_all %>%
      # Select words with MORE words in G2 and POSITIVE dot product (i.e., do not select words that
      # are more represented in the opposite group of its dot product projection)
      filter((abs(n_g1.x) < abs(n_g2.x) &
        x_plotted > 0))

    word_data_all <- bind_rows(word_data_all1, word_data_all2)
  }

  #### Preparing for the plot function ####

  # This solution is because it is not possible to send "0" as a parameter
  only_x_dimension <- NULL
  if (is.null(y_axes_1) == TRUE) {
    only_x_dimension <- 0
    y_axes_1 <- "only_x_dimension"
  }

  # Add or Remove values on y-axes
  if (y_axes_values_hide) {
    y_axes_values <- ggplot2::element_blank()
  } else {
    y_axes_values <- ggplot2::element_text()
  }

  # Word data adjusted for if y_axes exists
  if (y_axes == TRUE) {
    word_data_all_yadjusted <- word_data_all[word_data_all$extremes_all_x >= 1 | word_data_all$extremes_all_y >= 1, ]
  } else if (y_axes == FALSE) {
    word_data_all_yadjusted <- word_data_all[word_data_all$extremes_all_x >= 1, ]
  }


  ##### Adding/exploring words MANUALY ###### explore_words = "happy"

  if (!is.null(explore_words) == TRUE) {
    if (plot_type_name == "textProjection") {
      word_data_all_yadjusted <- textOwnWordsProjection(
        word_data = word_data,
        word_data_all = word_data_all,
        word_data_all_yadjusted = word_data_all_yadjusted,
        y_axes = y_axes,
        explore_words = explore_words,
        explore_words_color = explore_words_color,
        explore_words_point = explore_words_point,
        explore_words_aggregation = explore_words_aggregation,
        space = space,
        text_plot_comment = text_plot_comment,
        scaling = scaling
      )
    }

    if (plot_type_name == "textWordPrediction") {
      word_data_all_yadjusted <- textOwnWordPrediction(
        word_data = word_data,
        word_data_all = word_data_all,
        word_data_all_yadjusted = word_data_all_yadjusted,
        y_axes = y_axes,
        explore_words = explore_words,
        explore_words_color = explore_words_color,
        explore_words_point = explore_words_point,
        explore_words_aggregation = explore_words_aggregation,
        space = space,
        text_plot_comment = text_plot_comment,
        scaling = scaling
      )
    }
  }

  #### Plotting  ####
  plot <- textPlotting(
    word_data_all = word_data_all,
    word_data_all_yadjusted = word_data_all_yadjusted,
    only_x_dimension = only_x_dimension,
    x_axes_1 = x_axes_1,
    y_axes_1 = y_axes_1,
    group_embeddings1 = group_embeddings1,
    group_embeddings2 = group_embeddings2,
    projection_embedding = projection_embedding,
    label = words,
    points_without_words_size = points_without_words_size,
    points_without_words_alpha = points_without_words_alpha,
    colour_categories = colour_categories,
    arrow_transparency = arrow_transparency,
    scale_x_axes_lim = scale_x_axes_lim,
    scale_y_axes_lim = scale_y_axes_lim,
    position_jitter_hight = position_jitter_hight,
    position_jitter_width = position_jitter_width,
    word_font = word_font,
    point_size = point_size,
    aggregated_embeddings_data = aggregated_embeddings_data,
    aggregated_point_size = aggregated_point_size,
    aggregated_shape = aggregated_shape,
    aggregated_color_G1 = aggregated_color_G1,
    aggregated_color_G2 = aggregated_color_G2,
    projection_color = projection_color,
    word_size_range = word_size_range,
    # titles
    title_top = title_top,
    titles_color = titles_color,
    x_axes_label = x_axes_label,
    y_axes_label = y_axes_label,
    y_axes_values = y_axes_values
  )
  # plot

  #### Creating the legend ####

  legend <- textLegend(
    bivariate_color_codes = bivariate_color_codes,
    y_axes_1 = y_axes_1,
    fill = fill,
    legend_title = legend_title,
    legend_title_size = legend_title_size,
    legend_x_axes_label = legend_x_axes_label,
    legend_y_axes_label = legend_y_axes_label,
    word_data_all = word_data_all,
    legend_number_size = legend_number_size,
    # only_x_dimension = only_x_dimension,
    titles_color = titles_color
  )
  # legend

  #### Plot both figure and legend help(null_dev_env) ####
  final_plot <- suppressWarnings(cowplot::ggdraw() +
    cowplot::draw_plot(plot, 0, 0, 1, 1) +
    cowplot::draw_plot(legend, legend_x_position, legend_y_position, legend_h_size, legend_w_size))

  output_plot_data <- list(final_plot, text_plot_comment, word_data_all)
  names(output_plot_data) <- c("final_plot", "description", "processed_word_data")
  output_plot_data
}
