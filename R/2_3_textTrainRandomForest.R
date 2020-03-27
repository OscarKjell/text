


################
#### Random Forest: Training Cross-validated data
################

x = solmini_sd300_tk_mean$movement[1:40,1:5]
y = solmini$minidep_diagnose[1:40]

#df3_data <- as_tibble(df3)
#colnames(df3_data) <- c(paste("V", 1:(ncol(df3_data)-1), sep=""), "y")


textTrainRandomForest <- function(x, y, trees=500){

  set.seed(2020)

  y <- as.factor(y)

  x1 <- dplyr::select(x, dplyr::starts_with("V"))
  df2 <- cbind(x1, y)
  nrow(df2)
  df2$id1 <- c(1:nrow(df2))
  df3_data <- as_tibble(df2[complete.cases(df2),])
  nrow(df3_data)

  colnames(df3_data)
  is.numeric(df3_data$y)

  # Recipe: Preprocessing by removing na and normalising variables help(step_naomit)
  df3_recipe <-
    recipes::recipe(y ~ .,
                  data = df3_data) %>%
    recipes::update_role(id1, new_role = "id variable") %>%
    recipes::update_role(-id1, new_role = "predictor") %>%
    recipes::update_role(y, new_role = "outcome") %>%
    recipes::step_naomit(V1, skip = FALSE) %>% #Does this not work here?
    recipes::step_center(all_predictors()) %>%
    recipes::step_scale(all_predictors()) %>%
    recipes::step_BoxCox(all_predictors())


  # Cross-validation
  df3_cv_splits <- rsample::vfold_cv(df3_data, v = 10, repeats = 1, strata = y) # , ... ,  breaks = 4

  # Model: random forrest
  df3_model <- rand_forest(trees = trees, mode = "classification") %>%
    #set_engine("ranger")
    set_engine("randomForest")

  # help("workflow")
  df3_workflow <-
    workflows::workflow() %>%
    workflows::add_model(df3_model) %>%
    workflows::add_recipe(df3_recipe)

  # Applying the resampling; help(fit_resamples)
  df3_easy_eval <- tune::fit_resamples(df3_workflow, resamples = df3_cv_splits,  control = control_resamples(save_pred = TRUE))
  # See warning above!

  # Get prediction output
  df3_predictions <- tune::collect_predictions(df3_easy_eval)
  # Sort predictions and merge with the dataframe including NA (this is good for the merging of output in textTrainList)
  df3_predictions1 <- df3_predictions %>%
    rename(y2=y) %>%
    rename(id2=id) %>%
    arrange(.row) %>%
    cbind(df3_data)

  df3_data_preds <- merge(df2, df3_predictions1, by.x = "id1", by.y = "id1", all = TRUE)

  chi2 <- chisq.test(df3_data_preds$.pred_class, df3_data_preds$y.x)

  df3_predictions_save <- df3_data_preds %>%
    select(c("id1", ".pred_0", ".pred_1", ".pred_class", "y.y")) %>%
    rename(y=y.y)

  output_random_forest <- list(df3_predictions_save, chi2)
  names(output_random_forest) <- c("predictions", "results")
  output_random_forest
}


######################
########### END
######################




