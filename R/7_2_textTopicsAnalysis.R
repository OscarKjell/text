#' This function tests the relationship between a single topic or all topics and a
#' variable of interest. Available tests include correlation, t-test, linear regression,
#' binary regression, and ridge regression. (EXPERIMENTAL - under development)
#' @param model (data.frame) The model returned from textTopics().
#' @param group_var (string) Grouping variable for t-test
#' @param pred_var (string) Variable of interest for linear or binary regression
#' @param control_vars (list) Control variables for linear or binary regression
#' @param test_method (string) Choose between "correlation", "t-test", "binary_regression",
#'  "linear_regression" or "ridge_regression"
#' @param multiple_comparison Method for correction of multiple tests
#' (e.g., "fdr", "bonferroni").
#' @param load_dir (string) if specified, the function returns the precomputed analysis
#' from the directory, otherwise leave blank
#' @importFrom dplyr bind_cols
#' @importFrom tibble tibble
#' @importFrom stats as.formula glm p.adjust
#' @return Metadata and results of the test such as estimate, t-value, p-value,
#' and variable name.
#' @export
textTopicsTest <- function(model,
                           pred_var,
                           group_var = NULL, # only one in the case of t-test
                           control_vars = c(),
                           test_method = "linear_regression",
                           multiple_comparison = "fdr",
                           load_dir = NULL) {
  model_info <- model
  model <- model_info$model
  preds <- model_info$preds
  data <- model_info$train_data
  seed <- model_info$seed
  save_dir <- model_info$save_dir
  control_vars <- c(pred_var, control_vars)
  if (is.null(group_var)) {
    group_var <- pred_var
  }
  if (!is.null(load_dir)) {
    test <- readRDS(paste0(load_dir, "/seed_", seed, "/test.rds"))
  } else {
    if (!(group_var %in% names(preds))) {
      preds <- dplyr::bind_cols(data[group_var], preds)
    }
    for (control_var in control_vars) {
      if (!(control_var %in% names(preds))) {
        preds <- dplyr::bind_cols(data[control_var], preds)
      }
    }
    preds <- preds %>% tibble::tibble()
    test <- topic_test(
      topic_terms = model$summary,
      topics_loadings = preds,
      grouping_variable = preds[group_var],
      control_vars = control_vars,
      test_method = test_method,
      split = "median",
      n_min_max = 20,
      multiple_comparison = multiple_comparison
    )
  }

  if (!is.null(save_dir)) {
    if (!dir.exists(save_dir)) {
      # Create the directory
      dir.create(save_dir)
      cat("Directory created successfully.\n")
    } else {
      cat("Directory already exists.\n")
    }
    if (!dir.exists(paste0(save_dir, "/seed_", seed))) {
      dir.create(paste0(save_dir, "/seed_", seed))
    }
    if (test_method == "ridge_regression") {
      df <- list(
        variable = group_var,
        estimate = test$estimate,
        t_value = test$statistic,
        p_value = test$p.value
      )
      utils::write.csv(data.frame(df), paste0(save_dir, "/seed_", seed, "/textTrain_regression.csv"))
    }
    saveRDS(test, paste0(save_dir, "/seed_", seed, "/test_", test_method, ".rds"))
    print(paste0("The test was saved in: ", save_dir, "/seed_", seed, "/test_", test_method, ".rds"))
  }

  return_test <- list(
    test = test,
    test_method = test_method,
    pred_var = pred_var
  )
  return(return_test)
}




#' Wrapper for topicsTest function from the topics package
#'
#' This function tests the relationship between a single topic or all topics and a
#' variable of interest using the `topicsTest` function from the topics package.
#' @param model (data.frame) The model returned from textTopics().
#' @param group_var (string) Grouping variable for t-test
#' @param pred_var (string) Variable of interest for linear or binary regression
#' @param control_vars (list) Control variables for linear or binary regression
#' @param test_method (string) Choose between "correlation", "t-test", "binary_regression",
#'  "linear_regression" or "ridge_regression"
#' @param multiple_comparison Method for correction of multiple tests
#' (e.g., "fdr", "bonferroni").
#' @param load_dir (string) if specified, the function returns the precomputed analysis
#' from the directory, otherwise leave blank
#' @return Metadata and results of the test such as estimate, t-value, p-value,
#' and variable name.
#' @export
textTopicsTest2 <- function(
    model,
    pred_var_x = NULL,
    pred_var_y = NULL,
    group_var = NULL, # only one in the case of t-test
    control_vars = c(),
    test_method = "linear_regression",
    p_alpha = 0.05,
    p_adjust_method = "fdr",
    load_dir = NULL) {

  # Extract components from the model
  data <- model$train_data %>% tibble::as_tibble()
  preds <- model$preds %>% tibble::as_tibble()
  seed <- model$seed
  save_dir <- model$save_dir
  ngrams <- model$ngrams
  model_summary <- model$model

  # Call topicsTest from the topics package
  help(topicsTest)
  results <- topics::topicsTest(
    data = data,
    model = model_summary,
    preds = preds,
    ngrams = ngrams,
    pred_var_x = pred_var_x,
    pred_var_y = pred_var_y,
    group_var = group_var,
    control_vars = control_vars,
    test_method = test_method,
    p_alpha = p_alpha,
    p_adjust_method = p_adjust_method,
    seed = seed,
   # load_dir = NULL,
    save_dir = save_dir
  )

  return(results)
}


#' The function for topic testing
#' @param topic_loadings (tibble) The predicted loadings of topics including the grouping variable.
#' @param grouping_variable (tibble) The variable for grouping
#' @param topic_terms (R_obj) The object from model$summary in textmineR package vignette topic_modeling
#' @param split (string) How to split the CONTINUOUS test_values for testing
#' @param n_min_max (integer) If split = "min_max", the number of records to test per group.
#' @param multiple_comparison (string) The p-correction method
#' @importFrom dplyr select everything right_join contains
#' @importFrom purrr map
#' @seealso See \code{\link{textTrainRegression}}
#' @return the test as a data.frame
#' @noRd
topic_test <- function(topic_terms,
                       topics_loadings,
                       grouping_variable,
                       control_vars,
                       test_method = "correlation",
                       split = "median",
                       n_min_max = 20,
                       multiple_comparison = "bonferroni") {
  colnames(grouping_variable) <- "value"
  topics_groupings <- bind_cols(
    topics_loadings,
    grouping_variable
  )

  topics_loadings <- topics_loadings[complete.cases(topics_groupings), ]
  grouping_variable <- grouping_variable[complete.cases(topics_groupings), ]


  if (TRUE) {
    # format checker
    if (!tibble::is_tibble(topics_loadings)) {
      stop("Parameter `topics_loadings` must be a tibble.")
    }
    if (!tibble::is_tibble(grouping_variable)) {
      stop("Parameter `grouping_variable` must be a tibble.")
    }
    if (nrow(topics_loadings) != nrow(grouping_variable)) {
      stop("Parameters `topics_loadings` & `grouping_variable`
           should have the same length.")
    }
    if (!is.character(split)) {
      stop("Parameter `split` must be a string.")
    }
    if (!split %in% c("median", "quartile", "min_max")) {
      stop("Parameter `split` must be one of 'median', 'quartile', or 'min_max'.")
    }
    if (!is.numeric(n_min_max)) {
      stop("Parameter `n_min_max` must be numeric.")
    }
    if (!is.character(multiple_comparison)) {
      stop("Parameter `multiple_comparison` must be a string.")
    }
    if (!multiple_comparison %in% c(
      "holm", "hochberg", "hommel", "bonferroni",
      "BH", "BY", "fdr", "none"
    )) {
      stop("Variable `multiple_comparison` must be one of `holm`, `hochberg`,
      `hommel`, `bonferroni`, `BH`, `BY`,`fdr`, or `none`.")
    }
  }


  if (test_method == "correlation") {
    if (TRUE) {
      temp <- cbind(grouping_variable, topics_loadings)
      colnames(temp)[1] <- colnames(grouping_variable)[1]
      colnames(temp)[2:ncol(temp)] <- colnames(topics_loadings)
      topics_loadings <- temp
      temp <- NULL
      result <- topics_corr_grouping(
        topics_loadings,
        grouping1 = colnames(topics_loadings)[1],
        colnames1 = colnames(topics_loadings)[2:ncol(topics_loadings)],
        method1 = multiple_comparison
      )
      # Change the output of a list to a tibble. For corr only now.
      output <- extract_topic_stats_corr(result)
      names(output)[1] <- c("topic_name")
      output <- dplyr::left_join(output, topic_terms,
        by = dplyr::join_by(topic_name == topic)
      )

      output <- output %>%
        dplyr::select(
          topic_name,
          p.value,
          adjust.p_value,
          top_terms,
          prevalence,
          coherence,
          dplyr::everything() # this will include the rest of the columns in their original order
        )
    }

    return(output %>% sort_stats_tibble())
  }
  if (test_method == "t-test") {
    temp <- cbind(grouping_variable, topics_loadings)
    colnames(temp)[1] <- colnames(grouping_variable)[1]
    colnames(temp)[2:ncol(temp)] <- colnames(topics_loadings)
    topics_loadings <- temp
    temp <- NULL
    result <- topics_t_test_grouping(topics_loadings,
      method1 = multiple_comparison
    )

    # Produce the topic list through the pairs of categories
    output_list <- purrr::map(names(result), function(name) {
      output <- extract_topic_stats_cate(result[[name]])
      names(output)[1] <- c("topic_name")
      output <- dplyr::left_join(output, topic_terms, by = dplyr::join_by(topic_name == topic))

      output <- output %>%
        dplyr::select(
          topic_name,
          p.value,
          adjusted_p.value,
          cohen_d,
          top_terms,
          label_1,
          # label.label_2,
          prevalence,
          coherence,
          mean_group_1,
          mean_group_2,
          dplyr::everything() # this will include the rest of the columns in their original order
        )
      output <- sort_stats_tibble(output)
      return(output)
    })
    names(output_list) <- names(result)

    return(output_list)
  }
  if (test_method == "linear_regression" || test_method == "logistic_regression") {
    # still get number of topics automatically
    num_topics <- sum(grepl("t_", names(topics_loadings)))

    lda_topics <- character(num_topics)
    # Create the list of LDA topics
    for (i in 1:num_topics) {
      lda_topics[i] <- paste("t_", i, sep = "")
    }


    preds <- topics_loadings # load topics_loading into different variable to reduce naming errors
    for (topic in lda_topics) {
      mean_value <- mean(preds[[topic]])
      std_dev <- sd(preds[[topic]])
      preds[[paste0("z_", topic)]] <- (preds[[topic]] - mean_value) / std_dev
    }

    control_variables <- control_vars
    for (variable in control_variables) {
      preds[[paste0("z_", variable)]] <- scale(preds[[variable]])
    }

    # Initialize an empty list to store the topic names
    z_lda_topics <- character(num_topics)
    for (i in 1:num_topics) {
      z_lda_topics[i] <- paste0("z_t_", i)
    }
    # Loop through each LDA topic and create a linear model
    multi_models <- list()
    preds[is.na(preds)] <- 0


    if (test_method == "linear_regression") {
      formula_tail <- "~"
      for (variable in control_variables) {
        formula_tail <- paste0(formula_tail, " + z_", variable)
      }


      for (topic in z_lda_topics) {
        formula <- stats::as.formula(paste0(topic, formula_tail))
        multi_models[[paste0("t_", topic)]] <- lm(formula, data = preds)
      }
    }

    if (test_method == "logistic_regression") {
      for (topic in z_lda_topics) {
        multi_models[[paste0("t_", topic)]] <- stats::glm(paste0("z_", control_variables[1], " ~ ", topic),
                                                          data = preds)
      }
    }

    control_variable_summary <- list()
    topics <- c()
    if (test_method == "linear_regression") {
      for (variable in control_variables) {
        control_variable_summary[[variable]] <- list()
        control_variable_summary[[variable]][["estimate"]] <- c()
        control_variable_summary[[variable]][["t"]] <- c()
        control_variable_summary[[variable]][["p"]] <- c()
        control_variable_summary[[variable]][["p_adjusted"]] <- c()
      }
    }
    if (test_method == "logistic_regression") {
      control_variable_summary[["estimate"]] <- c()
      control_variable_summary[["t"]] <- c()
      control_variable_summary[["p"]] <- c()
      control_variable_summary[["p_adjusted"]] <- c()
    }

    for (i in 1:length(multi_models)) {
      temp <- multi_models[[i]]
      p_values <- summary(temp)$coefficients[, "Pr(>|t|)"]
      t_values <- summary(temp)$coefficients[, "t value"]
      estimate_values <- summary(temp)$coefficients[, "Estimate"]
      topics <- c(topics, paste0("t", i))
      if (test_method == "linear_regression") {
        for (variable in control_variables) {
          control_variable_summary[[variable]][["estimate"]] <- c(
            control_variable_summary[[variable]][["estimate"]],
            estimate_values[[paste0("z_", variable)]]
          )
          control_variable_summary[[variable]][["t"]] <- c(
            control_variable_summary[[variable]][["t"]],
            t_values[[paste0("z_", variable)]]
          )
          control_variable_summary[[variable]][["p"]] <- c(
            control_variable_summary[[variable]][["p"]],
            p_values[[paste0("z_", variable)]]
          )
        }
      }
      if (test_method == "logistic_regression") {
        control_variable_summary[["estimate"]] <- c(
          control_variable_summary[["estimate"]],
          estimate_values[[paste0("z_t_", i)]]
        )
        control_variable_summary[["t"]] <- c(
          control_variable_summary[["t"]],
          t_values[[paste0("z_t_", i)]]
        )
        control_variable_summary[["p"]] <- c(
          control_variable_summary[["p"]],
          p_values[[paste0("z_t_", i)]]
        )
      }
    }

    if (test_method == "linear_regression") {
      for (variable in control_variables) {
        p_adjusted <- stats::p.adjust(
          control_variable_summary[[variable]][["p"]],
          multiple_comparison,
          length(multi_models)
        )
        control_variable_summary[[variable]][[paste0("p_adjusted")]] <- c(
          control_variable_summary[[variable]][["p_adjusted"]],
          p_adjusted
        )
      }
    }
    if (test_method == "logistic_regression") {
      p_adjusted <- stats::p.adjust(
        control_variable_summary[["p"]],
        multiple_comparison,
        length(multi_models)
      )
      control_variable_summary[[paste0("p_adjusted")]] <- c(
        control_variable_summary[["p_adjusted"]],
        p_adjusted
      )
    }

    # return (control_variable_summary)
    control_variable_summary$topic <- lda_topics

    output <- dplyr::right_join(topic_terms[c("topic", "top_terms")],
                                data.frame(control_variable_summary),
                                by = dplyr::join_by(topic))
    # add the adjustment for bonferroni
    return(output)
  }

  if (test_method == "ridge_regression") {
    num_topics <- nrow(topic_terms)
    preds <- topics_loadings

    # rename topic columns
    for (i in 1:num_topics) {
      old_column_name <- paste0("t_", i)
      new_column_name <- paste0("Dim", i, "_texts")

      if (old_column_name %in% colnames(preds)) {
        colnames(preds)[colnames(preds) == old_column_name] <- new_column_name
      }
    }

    dims <- as.data.frame(preds) %>% dplyr::select(dplyr::contains("Dim"))
    dims <- tibble::as_tibble(dims)
    preds <- tibble::as_tibble(preds)
    for (col in colnames(dims)) {
      dims[[col]] <- as.numeric(dims[[col]])
    }
    trained_model <- textTrainRegression(
      x = dims,
      y = grouping_variable,
      multi_cores = FALSE # This is FALSE due to CRAN testing and Windows machines.
    )

    return(trained_model$results)
  }
}

#' This is a private function and used internally by textTopicsWordcloud
#' @param df_list (list) a list of data frames with each topic
#' @param phi (data.frame) data frame with topic word scores
#' @param model_type (string) "mallet", or "bert_topic" ("textmineR" not supported)
#' @return list of data.frames with assigned phi
#' @noRd
assign_phi_to_words <- function(df_list, phi, model_type) {
  for (i in 1:length(df_list)) {
    df <- data.frame(df_list[[i]])
    colnames(df)[1] <- "Word"
    phi_vector <- c()
    for (j in 1:nrow(df)) {
      word <- df[j, ]
      if (model_type == "mallet") {
        phi_vector <- c(phi_vector, phi[i, ][word])
      } else {
        phi_vector <- c(phi_vector, phi[paste0("t_", i), ][word])
      }
    }
    df$phi <- phi_vector
    df_list[[i]] <- df
  }
  return(df_list)
}

#' This is a private function and used internally by textTopicsWordcloud
#' @param summary (data.frame) the models summary
#' @return a list of dataframes for each topic filled with top terms
#' @noRd
create_topic_words_dfs <- function(summary) {
  n <- nrow(summary)
  df_list <- vector("list", n)
  # Create and name the dataframes in a loop
  for (i in 1:n) {
    word_vector <- unlist(strsplit(summary[paste0("t_", i), ]$top_terms, ", "))
    df <- data.frame(Word = word_vector) # Create an empty dataframe
    df <- df_cleaned <- df[complete.cases(df), ]
    df_list[[i]] <- df # Add the dataframe to the list

    name <- paste("t", i, sep = "_") # Create the name for the dataframe
    assign(name, df_list[[i]]) # Assign the dataframe to a variable with the specified name
  }

  return(df_list)
}


#' This is a private function and used internally by textTopicsWordcloud
#' @param df_list (list) list of data.frames with topics most frequent words and assigned topic term scores
#' @param test (data.frame) the test returned from textTopicTest()
#' @param test_type (string) "linear_regression", or "binary_regression"
#' @param cor_var (string) Variable for t-test, linear, binary or ridge regression
#' @param color_negative_cor (scale_color_gradient) color of topic cloud with negative correlation
#' @param color_positive_cor (scale_color_gradient) color of topic cloud with positive correlation
#' @param scale_size (bool) if True, then the size of the topic cloud is scaled by the prevalence of the topic
#' @param plot_topics_idx (list) if specified, then only the specified topics are plotted
#' @param p_threshold (float) set threshold which determines which topics are plotted
#' @param save_dir (string) save plots in specified directory, if left blank, plots is not saved,
#' thus save_dir is necessary
#' @param seed (int) seed is needed for saving the plots in the correct directory
#' @importFrom ggplot2 ggplot ggsave
#' @importFrom dplyr select everything
# @importFrom ggwordcloud geom_text_wordcloud (removing this since it is required through requireNamespace)
#' @noRd
create_plots <- function(df_list,
                         summary,
                         test,
                         test_type,
                         cor_var,
                         seed,
                         color_negative_cor,
                         color_positive_cor,
                         scale_size = TRUE,
                         plot_topics_idx = NULL,
                         p_threshold = NULL,
                         save_dir = ".") {

  if (!requireNamespace("ggwordcloud", quietly = TRUE)) {
    stop("ggwordcloud is required for this feature.
         Please install it using install.packages('ggwordcloud').", call. = FALSE)
  }


  if (is.null(plot_topics_idx)) {
    plot_topics_idx <- seq(1, length(df_list))
  }
  for (i in plot_topics_idx) {
    if (test_type == "linear_regression") {
      estimate_col <- paste0(cor_var, ".estimate")
      p_adjusted_col <- paste0(cor_var, ".p_adjusted")
    } else if (test_type == "t-test") {
      estimate_col <- "cohens d" # probably doesnt work yet
    } else if (test_type == "logistic_regression") {
      estimate_col <- "estimate"
      estimate_col <- "p_adjustedfdr"
    }
    estimate <- test[i, ][[estimate_col]] # $PHQtot.estimate
    p_adjusted <- test[i, ][[p_adjusted_col]] # $PHQtot.p_adjustedfdr
    if (scale_size == TRUE) {
      prevalence <- summary[paste0("t_", i), ]$prevalence
    }


    # this will ensure that all topics are plotted
    if (is.null(p_threshold)) {
      p_threshold <- p_adjusted + 1
    }

    if (!is.nan(p_adjusted) && p_adjusted < p_threshold) {

      if (estimate < 0) {
        color_scheme <- color_negative_cor
      } else {
        color_scheme <- color_positive_cor
      }
      if (scale_size == TRUE) {
        max_size <- 10 * log(prevalence)
        y <- paste0("P = ", prevalence)
      } else {
        max_size <- 10
        y <- ""
      }
      plot <- ggplot2::ggplot(df_list[[i]], aes(label = Word, size = phi, color = phi)) + # ,x=estimate)) +
        ggwordcloud::geom_text_wordcloud() +
        scale_size_area(max_size = max_size) +
        theme_minimal() +
        color_scheme +
        labs(
          x = paste0("r = ", estimate),
          y = y
        )

      if (!dir.exists(save_dir)) {
        dir.create(save_dir)
        cat("Directory created successfully.\n")
      }
      if (!dir.exists(paste0(save_dir, "/seed_", seed, "/wordclouds"))) {
        dir.create(paste0(save_dir, "/seed_", seed, "/wordclouds"))
      }
      p_adjusted <- sprintf("%.2e", p_adjusted)
      ggplot2::ggsave(paste0(save_dir,
                             "/seed_",
                             seed,
                             "/wordclouds/t_",
                             i, "_r_", estimate,
                             "_p_", p_adjusted, ".png"),
                      plot = plot, width = 10, height = 8, units = "in")
    }
  }
}

#' Create list of data.frames with topics most frequent words and topic term scores
#' @param save_dir (string) directory where to get data from
#' @param num_topics (int) the number of topics
#' @return list of data.frames
#' @noRd
create_df_list_bert_topics <- function(save_dir,
                                       seed,
                                       num_topics) {
  df_list <- list()
  for (i in 1:num_topics) {
    df_list[[i]] <- read.csv(paste0(save_dir, "/seed_", seed, "/df_list_term_phi/", i, "_top_words.csv"))
  }
  return(df_list)
}


#' Plots wordcloud (experimental)
#'
#' textTopicsWordcloud() plots wordclouds of topics from a Topic Model based on their significance
#' determined by a linear or binary regression
#' @param model (data.frame) The model returned from textTopics().
#' @param test (data.frame) the test returned from textTopicTest()
#' @param color_negative_cor (ggplot2::scale_color_gradient()) color gradient of topic cloud
#' with negative correlation
#' @param color_positive_cor (ggplot2::scale_color_gradient) color gradient of topic cloud
#' with positive correlation
#' @param scale_size (bool) if True, then the size of the topic cloud is scaled by the
#'  prevalence of the topic
#' @param plot_topics_idx (list) if specified, then only the specified topics are plotted
#' @param p_threshold (float) set significance threshold which determines which topics are plotted
#' @importFrom ggplot2 scale_color_gradient
#' @export
textTopicsWordcloud <- function(model,
                                test,
                                color_negative_cor = ggplot2::scale_color_gradient(low = "darkred", high = "red"),
                                color_positive_cor = ggplot2::scale_color_gradient(low = "darkgreen", high = "green"),
                                scale_size = FALSE,
                                plot_topics_idx = NULL,
                                p_threshold = 0.05) {
  model_info <- model
  model <- model_info$model
  cor_var <- test$pred_var
  test_method <- test$test_method
  test <- test$test
  save_dir <- model_info$save_dir
  seed <- model_info$seed
  model_type <- model_info$model_type
  if (model_type == "bert_topic") {
    num_topics <- nrow(test)
    df_list <- create_df_list_bert_topics(save_dir, seed, num_topics)
  } else if (model_type == "mallet") {
    model <- name_cols_with_vocab(model, "phi", model$vocabulary)
    df_list <- create_topic_words_dfs(model$summary)
    df_list <- assign_phi_to_words(df_list, model$phi, model_type)
  } else if (model_type == "neural_topic_model") {
    df_list <- create_topic_words_dfs(model$summary)
    df_list <- assign_phi_to_words(df_list, model$phi, "mallet")
  }

  create_plots(
    df_list = df_list,
    summary = model$summary,
    test = test,
    test_type = test_method,
    cor_var = cor_var,
    seed = seed,
    color_negative_cor = color_negative_cor,
    color_positive_cor = color_positive_cor,
    scale_size = scale_size,
    plot_topics_idx = plot_topics_idx,
    p_threshold = p_threshold,
    save_dir = save_dir
  )
  print(paste0("The plots (p<", p_threshold, ") are saved in ", save_dir, "/seed_", seed, "/wordclouds"))
}



#' Plot word clouds
#'
#' This function create word clouds and topic fugures
#' @param model (list) A trained topics model. For examples from topicsModel(). Should be NULL if plotting ngrams.
#' @param ngrams (list) The output from the the topicsGram() function . Should be NULL if plotting topics.
#' @param test (list) The test results; if plotting according to dimension(s) include the object from topicsTest() function.
#' @param p_threshold (integer) The p-value threshold to use for significance testing.
#' @param color_scheme (string 'default' or vector) The color scheme.
#'
#' For plots not including a test, the color_scheme should in clude 2 colours (1 gradient pair), such as:
#'
#' c("lightgray", "darkblue)
#'
#' For 1 dimensional plots of n-grams it should contain 4 colours (2 gradient pairs), such as:
#'
#' c(
#' "#EAEAEA", "darkred", # negative ngrams colors
#'
#' "#EAEAEA", "darkgreen" # positve ngrams colors)
#'
#'
#'
#' For 1-dimension plots of topics, it should contain 6 colours (3 gradient pairs), such as
#'
#'  c(
#' "#EAEAEA", "darkred",     # negative topics colors
#'
#' "#EAEAEA", "darkgray",     # colours of topics not significantly associated
#'
#' "#EAEAEA", "darkgreen"     # positve topics colors)
#'
#'
#'
#'  For 2-dimensional plots of topics, the color scheme should contain 18 colours (9 gradient pairs), such as:
#'
#'  c(
#'   "lightgray", "#398CF9",     # quadrant 1 (upper left corner)
#'
#'   "lightgray", "#60A1F7",     # quadrant 2
#'
#'   "lightgray", "#5dc688",     # quadrant 3 (upper right corner)
#'
#'   "lightgray", "#e07f6a",     # quadrant 4
#'
#'   "lightgray", "darkgray",     # quadrant 5 (middle square)
#'
#'   "lightgray", "#40DD52",     # quadrant 6
#'
#'   "lightgray", "#FF0000",     # quadrant 7 (bottom left corner)
#'
#'   "lightgray", "#EA7467",     # quadrant 8
#'
#'   "lightgray", "#85DB8E")     # quadrant 9 (bottom right corner)
#'
#'
#' @param scale_size (logical) Whether to scale the size of the words.
#' @param plot_topics_idx (vector)  The index or indeces of the topics to plot
#' (e.g., look in the model-object for the indices; can for example, be c(1, 3:5) to plot topic t_1, t_3, t_4 and t_5) (optional).
#' @param save_dir (string) The directory to save the plots.
#' @param figure_format (string) Set the figure format, e.g., ".svg", or ".png".
#' @param width (integer) The width of the topic (units = "in").
#' @param height (integer) The width of the topic (units = "in").
#' @param max_size (integer) The max size of the words.
#' @param seed (integer) The seed to set for reproducibility
#' @param scatter_legend_dot_size (integer) The size of dots in the scatter legend.
#' @param scatter_legend_bg_dot_size (integer) The size of background dots in the scatter legend.
#' @param scatter_legend_n (numeric or vector) A vector determining the number of dots to emphasis in each quadrant of the scatter legend.
#' For example: c(1,1,1,1,0,1,1,1,1) result in one dot in each quadrant except for the middle quadrant.
#' @param scatter_legend_method (string) The method to filter topics to be emphasised in the scatter legend.
#' Can be either "mean", "max_x", or "max_y"
#' @param scatter_legend_specified_topics (vector) Specify which topic(s) to be emphasised in the scatter legend.
#' For example c("t_1", "t_2"). If set, scatter_legend_method will have no effect.
#' @param scatter_legend_topic_n (boolean) Allow showing the topic number or not in the scatter legend
#' @param grid_legend_title The title of grid topic plot.
#' @param grid_legend_title_size The size of the title of the plot.
#' @param grid_legend_title_color The color of the legend title.
#' @param grid_legend_x_axes_label The label of the x axes.
#' @param grid_legend_y_axes_label The label of the y axes.
#' @param grid_legend_number_color The color in the text in the legend.
#' @param grid_legend_number_size The color in the text in the legend.
#' @return The function saves figures in the save_dir.
#' @export
textTopicsWordcloud2 <- function(
    model = NULL,
    ngrams = NULL,
    test = NULL,
    p_threshold = 0.05, # Why is this set here since the test[[3]]$test$color_categories is determnied in in testTopics test?
    color_scheme = "default",
    scale_size = FALSE,
    plot_topics_idx = NULL,
    save_dir = "./results",
    figure_format = "svg",
    width = 10,
    height = 8,
    max_size = 10,
    seed = 42,
    scatter_legend_dot_size = 15,
    scatter_legend_bg_dot_size = 9,
    scatter_legend_n = c(1,1,1,1,0,1,1,1,1),
    scatter_legend_method = c("mean"),
    scatter_legend_specified_topics = NULL,
    scatter_legend_topic_n = FALSE,
    grid_legend_title = "legend_title",
    grid_legend_title_size = 5,
    grid_legend_title_color = 'black',
    grid_legend_x_axes_label = "legend_x_axes_label",
    grid_legend_y_axes_label = "legend_y_axes_label",
    grid_legend_number_color = 'black',
    grid_legend_number_size = 5) {

  model_info <- model
  model <- model_info$model
  cor_var <- test$pred_var
  test_method <- test$test_method
  test <- test$test
  save_dir <- model_info$save_dir
  seed <- model_info$seed
  model_type <- model_info$model_type

 # if (model_type == "bert_topic") {
 #   num_topics <- nrow(test)
 #   df_list <- create_df_list_bert_topics(save_dir, seed, num_topics)
 # } else if (model_type == "mallet") {
 #   model <- name_cols_with_vocab(model, "phi", model$vocabulary)
 #   df_list <- create_topic_words_dfs(model$summary)
 #   df_list <- assign_phi_to_words(df_list, model$phi, model_type)
 # } else if (model_type == "neural_topic_model") {
 #   df_list <- create_topic_words_dfs(model$summary)
 #   df_list <- assign_phi_to_words(df_list, model$phi, "mallet")
 # }

  topics::topicsPlot(
    model = model,
    ngrams = NULL,
    test = test,
    p_threshold = p_threshold,
    color_scheme = color_scheme,
    scale_size = scale_size,
    plot_topics_idx = plot_topics_idx,
    save_dir = save_dir,
    figure_format = figure_format,
    width = width,
    height = height,
    max_size = max_size,
    seed = seed,
    scatter_legend_dot_size = scatter_legend_dot_size,
    scatter_legend_bg_dot_size = scatter_legend_bg_dot_size,
    scatter_legend_n = scatter_legend_n,
    scatter_legend_method = scatter_legend_method,
    scatter_legend_specified_topics = scatter_legend_specified_topics,
    scatter_legend_topic_n = scatter_legend_topic_n,
    grid_legend_title = grid_legend_title,
    grid_legend_title_size = grid_legend_title_size,
    grid_legend_title_color = grid_legend_title_color,
    grid_legend_x_axes_label = grid_legend_x_axes_label,
    grid_legend_y_axes_label = grid_legend_y_axes_label,
    grid_legend_number_color = grid_legend_number_color,
    grid_legend_number_size = grid_legend_number_size
  )

  completion_text <- paste0("The plots (p<", p_threshold, ") are saved in ", save_dir, "/seed_", seed, "/wordclouds")
  cat(colorise(completion_text, "green"))

}




