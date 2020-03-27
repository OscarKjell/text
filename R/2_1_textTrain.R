
library(tidymodels)
# Basic example: https://rviews.rstudio.com/2019/06/19/a-gentle-intro-to-tidymodels/
# recipies: https://tidymodels.github.io/recipes/articles/Simple_Example.html
# rsample: https://tidymodels.github.io/rsample/articles/Basics.html
# parsnip: https://tidymodels.github.io/parsnip/articles/parsnip_Intro.html
# yardstick: https://tidymodels.github.io/yardstick/reference/index.html
# Two-day workshop: https://github.com/tidymodels/aml-training/tree/master/two%20day

# .rs.restartR()

# https://rstudio-conf-2020.github.io/applied-ml/Part_5.html#29
library(doParallel)
cl <- makeCluster(6)
registerDoParallel(cl)

stopCluster(cl)



library(text)
library(tidymodels)
library(rlang)
library(magrittr)

library(recipes)
library(workflows)
library(parsnip)
library(tune)

library(data.table)

library(tidyselect)
# test data
#x = solmini_sd300_tk_mean$movement[1:40,]
#y = solmini$phq_tot[1:40]



# textTrain function using Tidymodeels
textTrainTidy <- function(x, y){
  x1 <- dplyr::select(x, dplyr::starts_with("V"))
  df2 <- cbind(x1, y)
  nrow(df2)

  df3 <- df2#[complete.cases(df2),]
  nrow(df3)

  # Recipe: Preprocessing with pca help(recipe) help(step_naomit)
  df3_recipe <-
    recipes::recipe(y ~ .,
                    data = df3) %>%
    #  step_BoxCox(all_predictors()) %>%
    step_naomit(V1, skip = TRUE) %>%
    recipes::step_center(all_predictors()) %>%
    recipes::step_scale(all_predictors()) %>%
    recipes::step_pca(all_predictors(), threshold = .95) #%>%
  # recipes::step_pca(one_of(!!stations), num_comp = tune())
  # recipes::prep(training = bivariate_data_train)

  # Cross-validation
  set.seed(42)
  df3_cv_splits <- rsample::vfold_cv(df3, v = 10, repeats = 1, strata = NULL) # , ... ,  breaks = 4

  # Model
  df3_model <-
    parsnip::linear_reg(penalty = tune(), mixture = tune()) %>% # tune() uses the grid
    parsnip::set_engine("glmnet")

  # Tuning; parameters; grid for ridge regression. https://rstudio-conf-2020.github.io/applied-ml/Part_5.html#26
  df3_glmn_grid <- base::expand.grid(
    penalty = 10 ^ seq(-3, -1, length = 20),
    mixture = (0:5) / 5
    )

  ctrl <- control_grid(save_pred = TRUE)

  #cl <- makeCluster(10)
  #registerDoParallel(cl)
  # Tune_grid()
  df3_glmn_tune <- tune_grid(
    df3_recipe,
    model = df3_model,
    resamples = df3_cv_splits,
    grid = df3_glmn_grid,
    control = ctrl
    )
    #stopCluster(cl)

  # Select the best penelty and mixture based on rmsea
  best_glmn <-
    select_best(df3_glmn_tune, metric = "rmse", maximize = FALSE)

  # Get predictions and observed (https://rstudio-conf-2020.github.io/applied-ml/Part_5.html#32)
  df3_predictions <- tune::collect_predictions(df3_glmn_tune) %>%
    filter(penalty == best_glmn$penalty, mixture == best_glmn$mixture)
  df3_predictions

  # Evaluating the predictions using correlation
  correlation <- cor.test(df3_predictions$y, df3_predictions$.pred)
  output <- list(correlation, df3_predictions)
  names(output) <- c("correlation", "predictions")
  output
}
### End of textTrainTidy function
#test1 <- textTrainTidy(xs, y)



####### textTrainTidyList analyse list of text variables and tibble of numeric variables
# x  list of word embeddings
# y  tibble with numeric varaibles to predict



 x <- list(solmini_sd300_tk_mean1$dep_all[1:150,], solmini_sd300_tk_mean1$movement[1:150,]) # , solmini_sd300_tk_mean1$sleep[1:150,]
 y <- tibble(solmini$phq_tot[1:150], solmini$gad_tot[1:150]) #, solmini$hils_tot[1:150]
 y <- tibble(as.factor(solmini$minidep_diagnose[1:150]), as.factor(solmini$miniGAD_diagnose[1:150]))

 names(x) <- c("dep_all", "movement") #, "sleep"
 names(y) <- c("phq", "gad") # , "hils"
 names(y) <- c("minidep", "minigad")

 library(data.table)
 # x = list of word-embeddings
 # y = tibble with variablesto be predicted
textTrainTidyLists <- function(x, y, method = "randomForest", trees=500) { # method="regression"

  # Get variable names in the list of outcomes
  variables <- dput(names(y))
  # Duplicate variable names to as many different wordembeddings there are in x
  variables <- rep(variables, length(x))
  # Create data frame with duplicated variables
  y1 <- y[c(variables)]
  # Order columns alphabatically
  y1 <- y1[, order(colnames(y1))]

  # Creating descriptions of which variabeles are used in training, which is  added to the output help(paste)
  descriptions <- paste(rep(names(x), length(y)), "_", names(y), sep = "")

  if (method == "regression"){
    # Using mapply to loop of the word embeddings and the outcome variables help(mapply)
    output <- mapply(textTrainTidy, x, y1, SIMPLIFY = FALSE)   #MoreArgs = list(nrFolds_k = nrFolds_k, methodTrain = methodTrain, preProcessTrain = preProcessTrain, preProcessThresh = preProcessThresh, methodCor = methodCor, ...),

    # Sorting the outcomes
    output_p_r <- t(as.data.frame(lapply(output, function(output) unlist(output$correlation)[c(4, 2, 3, 1, 6)])))
    # Add Outcomes and Descriptions together; name the columns; and remove the rownames.
    output_ordered_named <- data.frame(cbind(descriptions, output_p_r))
    colnames(output_ordered_named) <- c("descriptions", "correlation", "df", "p_value", "t_statistics", "alternative")
    rownames(output_ordered_named) <- NULL
    output_ordered_named

    output_predscore <- as.data.frame(lapply(output, function(output) unlist(output$predictions)))
    output_predscore_reg <- output_predscore[rownames(output_predscore) %like% ".pred", ] # like comes from data.table
    colnames(output_predscore_reg) <- c(paste(descriptions, "_pred", sep=""))
    results <- list(output_predscore_reg, output_ordered_named)
    names(results) <- c("predscores", "results")
    results

  } else if (method == "randomForest"){
    output <- mapply(textTrainRandomForest, x, y1, SIMPLIFY = FALSE, MoreArgs = list(trees=trees))
    output_chi <- t(as.data.frame(lapply(output, function(output) unlist(output$results)[[1]][[1]])))
    output_df <- t(as.data.frame(lapply(output, function(output) unlist(output$results)[[2]][[1]])))
    output_p <- t(as.data.frame(lapply(output, function(output) unlist(output$results)[[3]][[1]])))
    output_p_r <- tibble(output_chi, output_df, output_p)
    # Add Outcomes and Descriptions together; name the columns; and remove the rownames.
    output_ordered_named <- data.frame(cbind(descriptions, output_p_r))
    colnames(output_ordered_named) <- c("descriptions", "chi2", "df", "p_value")
    output_ordered_named


    output_predscore <- as.data.frame(lapply(output, function(output) unlist(output$predictions)))
    output_predscore_c <- output_predscore[rownames(output_predscore) %like% ".pred_c", ] #like comes from data.table
    output_predscore_0 <- output_predscore[rownames(output_predscore) %like% ".pred_0", ]
    output_predscore_c0 <- cbind(output_predscore_0, output_predscore_c)
    colnames(output_predscore_c0) <- c(paste(descriptions, "_pred0", sep=""), paste(descriptions, "_class", sep=""))
    output_predscore_c0_1 <- as_tibble(output_predscore_c0)
    # output_predscore$original_row_number <- output[[1]][[2]]$.row
    # output_predscore <- output_predscore %>% dplyr::arrange(original_row_number)

    results <- list(output_predscore_c0_1, output_ordered_named)
    names(results) <- c("predscores", "results")
    results
    }
}


### End of textTrainTidyList function

# train_lists_test <- textTrainTidyLists(x, y, method = "regression")
# train_lists_test <- textTrainTidyLists(x, y, method = "randomForest")


# text11_num4 <- textTrainTidyLists(solmini_text_11 , solmini_n4, method = "regression")

text11_num4$predscores
text11_preds_tibble <- text11_num4$predscores %>%
  sapply(as.numeric) %>%
  as_tibble()

dep_mini_diagnose <- as.factor(solmini$minidep_diagnose)
CVpredsDepmini <- trainCVpredictions(text11_preds_tibble, dep_mini_diagnose)

chisq.test(solmini$minidep_diagnose, output$predictions$.pred_class)

gad_mini_diagnose <- as_factor(solmini$miniGAD_diagnose)
trainCVpredictions(text11_num4$predscores, gad_mini_diagnose)


##### Function to train predicted scores with rating scales (SHOULD BE USED ON TESTING DATA?)


# Sort out crossvalidated predictions and add above rating scales
train_lists_test$predscores
solmini_n4 <- solmini[c("phq_tot", "gad_tot", "swls_tot", "hils_tot")]
x <- cbind(train_lists_test$predscores, solmini_n4[1:150,])

y <- as_factor(solmini$minidep_diagnose[1:150])

x <- text11_preds_tibble
y <- dep_mini_diagnose

nrow(df2)
df3 <- df2#[complete.cases(df2),]
nrow(df3)
table(is.na(df3))

################
#### Training Cross-validated predicted data
################
# x = Tibble with predictor variables (e.g., cross-validated predictions from textTrainTidyLists, other rating scales etc)
# y = the outcome variable


trainCVpredictions <- function(x, y) {

  df3 <- cbind(x, y)
  df3 <- as_tibble(df3)
  df3 <- df3[complete.cases(df3),]
  # Recipe: Preprocessing with pca help(recipe) help(step_naomit)
  df3_recipe <-
    recipes::recipe(y ~ .,
                    data = df3) %>%
    #  step_BoxCox(all_predictors()) %>%
    recipes::step_knnimpute(all_predictors(), neighbors=round(sqrt(nrow(df3[complete.cases(df3),])))) %>% # See PERHAPS ROUND THIS;
    recipes::step_naomit(all_predictors(), skip = TRUE) %>%
    recipes::step_center(all_predictors()) %>%
    recipes::step_scale(all_predictors()) %>%
    recipes::step_pca(all_predictors(), threshold = .95) #%>%

  # Cross-validation
  set.seed(42)
  df3_cv_splits <- rsample::vfold_cv(df3, v = 10, repeats = 1, strata = y) # , ... ,  breaks = 4

  # Model
  df3_model <-
    #parsnip::linear_reg(penalty = tune(), mixture = tune()) %>% # tune() uses the grid
    parsnip::logistic_reg(mode = "classification", penalty = tune(), mixture = tune()) %>%
    parsnip::set_engine("glmnet")

  # Tuning; parameters; grid for ridge regression. https://rstudio-conf-2020.github.io/applied-ml/Part_5.html#26
  df3_glmn_grid <- base::expand.grid(
    penalty = 10 ^ seq(-3, -1, length = 20),
    mixture = (0:5) / 5
  )

  ctrl <- control_grid(save_pred = TRUE)

  # tune_grid() df3_glmn_tune$.notes
  df3_glmn_tune <- tune_grid(
    df3_recipe,
    model = df3_model,
    resamples = df3_cv_splits,
    grid = df3_glmn_grid,
    control = ctrl
  )

  # Select best predictions based on rmse tail(df3_glmn_tune$.metrics[[10]][3]).metric
  # best_glmn <-
  #  select_best(df3_glmn_tune, metric = "rmse", maximize = FALSE)

  # Accuracy could also be roc_auc:NEED TO PUT IN IF STATEMENT ABOVE
  best_glmn <-
    select_best(df3_glmn_tune, metric = "accuracy", maximize = FALSE)

  # Get predictions and observed (https://rstudio-conf-2020.github.io/applied-ml/Part_5.html#32)
  df3_predictions <- tune::collect_predictions(df3_glmn_tune) %>%
    filter(penalty == best_glmn$penalty, mixture == best_glmn$mixture)

  df3_predictions <- df3_predictions %>% arrange(.row)

  # Evaluating the predictions using correlation
  # result <- cor.test(df3_predictions$y, df3_predictions$.pred)

  #For categorical NEED TO PUT IN IF STATEMENT ABOVE
  result <- chisq.test(df3_predictions$y, df3_predictions$.pred_class)

  output <- list(df3_predictions, result)
  names(output) <- c("predictions", "results")
  output
}
######################
########### END trainCVpredictions
######################

outputCVpred <- trainCVpredictions(x, y)

# train on 90 percent and use the model to predict on 10 that is used to




# What the AI gets

#AI_category <- output$predictions %>%
#  mutate(AI_category1 = cut(.pred, breaks=c(-Inf, 0.5, Inf), labels=c("0","1")))

# solmini$phq_tot_diagnose10

#chisq.test(solmini$minidep_diagnose, AI_category$AI_category1)

chisq.test(solmini$minidep_diagnose, output$predictions$.pred_class)



cor.test(as.numeric(solmini$minidep_diagnose), as.numeric(output$predictions$.pred))




# What a clinician using 10 as cut of would get:
solmini <- solmini %>%
  mutate(phq_tot_diagnose10 = cut(phq_tot, breaks=c(-Inf, 10, Inf), labels=c("0","1")))

chisq.test(solmini$minidep_diagnose, solmini$phq_tot_diagnose10)
cor.test(as.numeric(solmini$minidep_diagnose), as.numeric(solmini$phq_tot_diagnose10))


