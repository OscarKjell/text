

library(tidyselect)
library(tidymodels)
library(tidyverse)
text_we_1 <- solmini_text_11$dep_all
text_we_2 <- solmini_text_11$wor_all

xlist <- solmini_text_11[1:3]
y <- solmini_num_6$phq_tot

####### textTrainTidyMultiText
# Description: Function takes several text variables; applies separet PCAs to each text's word embeddings separetaly;
# concenate the PCA components to predict one outcome with ridge regression!
# x is a list of word embeddings
textTrainMultiTexts <- function(xlist, y){

  set.seed(2020)
  Nword_variables <- length(xlist)
  # Give each column specific names with indeces so that they can be handled separately in the PCAs
  for (i in 1:Nword_variables){
    colnames(xlist[[i]]) <- paste("V_text", i, ".", names(xlist[i]), colnames(xlist[[i]]), sep="")
  }

  # Make vector with each index so that we can allocate them separately for the PCAs
  for (i in 1:Nword_variables){
    variable_index_vec[i] <- paste("V_text", i, sep="")
  }

  # Make one df rather then list.
  df1 <- bind_cols(xlist)
  V1 <- colnames(df1)[1]

  #x1 <- dplyr::select(x, dplyr::starts_with("V"))
  df2 <- cbind(df1, y)
  df3 <- df2[complete.cases(df2),]
  df3 <- as_tibble(df3)

  df3_recipe <-
    recipes::recipe(y ~ .,
                    data = df3) %>%
    #    recipes::step_BoxCox(all_predictors()) %>%
    recipes::step_naomit(V1, skip = TRUE) %>%
    recipes::step_center(all_predictors()) %>%
    recipes::step_scale(all_predictors())

  # Adding a PCA in each loop; first selecting all variables starting with V_text1; and then V_text2 etc
  for (i in variable_index_vec) {
    df3_recipe <-
      df3_recipe %>%
      # !! splices the current name into the `matches()` function.
      # We use a custom prefix so there are no name collisions for the
      # results of each PCA step.
      step_pca(matches(!!i), threshold = .95, prefix = paste("PCA_", i, "_"))
  }

  # Cross-validation
  df3_cv_splits <- rsample::vfold_cv(df3, v = 10, repeats = 1, strata = y) # , ... ,  breaks = 4

  # Model
  df3_model <-
    parsnip::linear_reg(penalty = tune(), mixture = tune()) %>% # tune() uses the grid
    parsnip::set_engine("glmnet")

  # Tuning; parameters; grid for ridge regression. https://rstudio-conf-2020.github.io/applied-ml/Part_5.html#26
  df3_glmn_grid <- base::expand.grid(
    penalty = 10 ^ seq(-3, -1, length = 20),
    mixture = (0:5) / 5
  )

  ctrl <- control_grid(save_pred = TRUE, verbose = TRUE)

  #cl <- makeCluster(10)
  #registerDoParallel(cl)
  # tune_grid() df3_glmn_tune$.notes
  df3_glmn_tune <- tune_grid(
    df3_recipe,
    model = df3_model,
    resamples = df3_cv_splits,
    grid = df3_glmn_grid,
    control = ctrl
  )

  # Select the best penelty and mixture based on rmsea
  best_glmn <- select_best(df3_glmn_tune, metric = "rmse", maximize = FALSE)

  # Get predictions and observed (https://rstudio-conf-2020.github.io/applied-ml/Part_5.html#32)
  df3_predictions <- tune::collect_predictions(df3_glmn_tune) %>%
    filter(penalty == best_glmn$penalty, mixture == best_glmn$mixture)

  # Evaluating the predictions using correlation
  correlation <- cor.test(df3_predictions$y, df3_predictions$.pred)
  output <- list(df3_predictions, correlation)
  names(output) <- c("predictions", "correlation")
  output
}
### End of textTrainTidy function


