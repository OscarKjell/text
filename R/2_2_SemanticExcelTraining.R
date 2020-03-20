 
# NB! Two functions that are similar to analyses in  Semantic Excel (for comparison purposes only)! The functions are conceptualually the same as in Semantic Excel (however, the results are similar 
# but NOT excactly the same between the programs!) These functions have LOW priority and does not have to be reviewed at this stage or ever" 
# This file contain two functions; one to train word variables to numeric variables; and one 
#to save the training as a predictive models. 



library(tidyverse)
library(stringr)
library(caret) # good source: http://topepo.github.io/caret/model-training-and-tuning.html
library(dplyr)


################################################################################################################################################################
################################################################################################################################################################
##########1. Semantic training: Multiple regression using machine learning to test different number of dimensions; and cross-validation to evaluate the models and machine learning  
################################################################################################################################################################
################################################################################################################################################################
#dim1=FALSE removeves the attempt to only use the first dimensions, since some models require more than one predictor. 
#nrFolds=2
#nrFoldsOuter= 2
#methodTrain = "lm"
#methodCor = "pearson"
#dim1 = TRUE

semanticTraining <- function(x, y, nrFolds=10, nrFoldsOuter= 10, methodTrain = "lm", methodCor = "pearson", dim1 = TRUE, Ndim = 300, ...){
  #Getting random number that have been saved from Matlab randomization; not sure if this works though...
  # MatlabX <- as.numeric(read.csv("/Users/oscar/Desktop/0 Studies/5 R statistical semantics package/OLD/MatlabRandom10000.csv"))
  
  ##################Import data if x is character or as semreps (i.e., allready been imported)
  if (is.character(x) == TRUE) {
    df <- cbind(x, y)
    df1 <- importTibble(df)
    df2 <- cbind(df1, df[2])
    df2 <- as.tibble(df2)
  } else {
    df2 <- cbind(x, as.tibble(y))
    df2 <- rename(df2, y = value)
    df2 <- as.tibble(df2)
  }
  
  ##################Selecting dataset with different number of dimensions
  ## Sequence to select dimensions from the semreps. SM-article state: "Adding 1, then multiplying by 1.3 and finally rounding to the nearest integer (e.g., 1, 3, 5, 8, where the next number of dimensions to be tested are the first 12; in other words ([8 +􏰄 1*] 􏱡 1.3)
  stop = Ndim
  new_num = 1
  selection_vector = 1
  while(new_num < stop) {
    new_num  <-  round((new_num + 1) * 1.3)
    selection_vector  <-  c(selection_vector, new_num)
  }
  #Changing the last number to the maximum number of dimensions
  selection_vector[length(selection_vector)] <- Ndim 
  
  #dim1==FALSE removes the attempt to use only one dimensions, since in some models more than one predictor is required
  if (dim1 == FALSE) {
    selection_vector <- selection_vector[2:length(selection_vector)]
  } else {
    selection_vector <- selection_vector
  }

  ################################
  ############################# The OUTER/first k-fold cross-validation
  ################################
  
  ###Create lists to store models and correlations in
  #INNER cross-validation lists
  models_wdifDims <- list()
  correlations_list <- list()
  #OUTER cross-validation lists
  output <- list()
  predicted_values <- list()
  predicted_index <- list()
  semrepDims <- list()
  results_corr_output <- list() #NEW
  #LAST output
  results <- list()
  
  #Setting seed for selecting training-sets the same each time
  # set.seed(MatlabX) 
  # Creating folds for outer cross-validation, so that the data is split into 90% for inner cv, and 10% for testing with that particular semrep dimesion solution; see solution from https://stats.stackexchange.com/questions/125843/outer-crossvalidation-cycle-in-caret-package-r
  ytrain_df2 <- c(1:nrow(df2)) # number of observation that should be split in to the number of folds.
  ntrain <- length(ytrain_df2)    
  train_index <- createFolds(ytrain_df2, k=nrFoldsOuter, returnTrain=TRUE) #From caret package; creates indexes for the rows that should be used for training
  test_index <- lapply(train_index,function(x) (1:ntrain)[-x]) #Gets the rows not used in training but instead will be used for testing. 
  
  ###################################
  #####Outer cross-validation for-loop; an extra hold-out; e.g., in 10-folds, 90% of the data will be used to find best number of dimensions and 10% will then be used for prediction
  #i_outer=1 ; i=1; i=2
  for (i_outer in 1:nrFoldsOuter){
    df2a  <- df2[train_index[[i_outer]],]
    
    ###################
    #### INNER for-loop that taes 90% of data to find the best number of semantic dimensions 
    for (i in 1:(length(selection_vector))) {
      #Select dataframe with required number of dimensions to be tested
      df3<- as.tibble(df2a[,1:selection_vector[i]])
      df3$y <-  df2a$y
      
      #### Multiple Linear Regression  
      #Setting seed for selecting training sets the same each time
      # set.seed(MatlabX)
      #Define training control; verboseIter = TRUE show how long time each fold takes in the console.   
      train_control<- trainControl(method="cv", number=nrFolds, verboseIter = TRUE, savePredictions = 'final', ...) #
      
      options(warn=-1)  #Turn off warnings; i.e., Turning of function showing warning regarding rank deficiency since we're looking at final correlation between actual and predicted anyway. OK?
      #Train and save the model; the "." takes all variables except y.  
      model <- train(y~., data=df3, trControl=train_control, method= methodTrain, na.action = na.omit, ...) #
      models_wdifDims[[i]] <- model
      options(warn=1) #Turning on warnings again
      
      #Correlating actual with predicted values, and saving it in a list
      correlations_list[[i]] <- cor.test(model$pred$pred, model$pred$obs, method=methodCor, na.action=na.omit, ...)#  
    }
    ########End of INNER cross-validation for loop
    ##########################
    
    #Get t-statistics, dfs, p-values, correlation estimates and type of method from the correlations (This is not all shown in the current code, but will keep it in the code) 
    results_corr <- do.call("rbind", lapply(correlations_list, '[', c(1:4, 7)))
    #Get strongest correlation, so that we can select the model that created it. 
    cor <- max(as.vector(unlist(results_corr[,"estimate"])))
    #Select best model with strongest correlation by selecting which numeric position  the highest correlation is positioned in; and if several take the first as this uses fewest dimensions. 
    nr_model <- which(as.vector(unlist(results_corr[,"estimate"])) == max(as.vector(unlist(results_corr[,"estimate"]))))
    if (length(nr_model) > 1) {
      nr_model <- nr_model[1]
    }else{
      nr_model <- nr_model
    }
    #Find and save the model associated with the highest correlation, and save the correaltion result
    output[[i_outer]] <- list(models_wdifDims[[nr_model]], correlations_list[results_corr[,"estimate"]==cor])
    #Name the lists in the list
    names(output[[i_outer]]) <- c("modelTraining", "evaluation")
    help(predict)
    ######Predicting for the OUTER for-loop with the data not used to find what number of dimensions to use.
    predicted_values[[i_outer]] <- list(as.vector(predict(output[[i_outer]]$modelTraining, newdata = df2[test_index[[i_outer]],], na.action=na.pass)))
    #Saving which row indices that are connected with the predicted values so that they can be linked with actual y-values
    predicted_index[[i_outer]] <- list(test_index[[i_outer]])
    #Saving the actual number of dimensions used for the final predictions
    semrepDims[[i_outer]] <- length(attr(terms(output[[i_outer]]$modelTraining), "term.labels"))
  }
  #########End of OUTER cross-validation for loop
  ####################################
  
  #Arranging results from the OUTER for-loop
  predicted_cv_yvalues <- unlist(do.call("cbind", lapply(predicted_values, '[')))
  pred_outercv_Index <- unlist(do.call("cbind", lapply(predicted_index, '[')))
  predictions <- tibble(predicted_cv_yvalues, pred_outercv_Index)
  #Arrange according to index so that the actual y-values are added correctly to the dataframe
  predictions <- arrange(predictions, pred_outercv_Index)
  predictions$y_actual <- df2$y
  #Remove indices since it has been arranged to be 1:length
  predictions <- predictions %>% dplyr::select(-pred_outercv_Index)
  
  ####Arranging the results output 
  #Actual and predicted raw data
  results[[1]] <- predictions
  #Mean number of semrep dimensions used in each final prediction of the OUTER 
  semrepDims_v <- as.vector(unlist(do.call("cbind", lapply(semrepDims, '['))))
  results[[2]] <- mean(semrepDims_v)
  names(results[[2]]) <- "MeanDimensionsUsed"  
  #Correlation between predicted and actual values
  results[[3]] <- cor.test(predictions$predicted_cv_yvalues, predictions$y_actual, method = methodCor, na.action=na.omit, ...)
  results
}
################################################################################################################################################################
########### End of SemanticTraining function ###################################################################################################################
################################################################################################################################################################




################################################################################################################################################################
################################################################################################################################################################
########## 2. trainModelForPrediction: create a trained model; i.e. select number of dimensions not using OUTER cross-validation.
################################################################################################################################################################
################################################################################################################################################################

#####Function trainModelForPrediction
#dim1=FALSE removeves the attempt to only use the first dimensions, since some models require more than one predictor. 
#  nrFolds=10
#  methodTrain = "lm"
#  methodCor = "pearson"
#  dim1 = TRUE
  
trainModelForPrediction <- function(x, y, nrFolds=10, methodTrain = "lm", methodCor = "pearson", dim1 = TRUE, ...) {
    #Getting random number that have been saved from Matlab randomization; not sure if this works though...
    MatlabX <- as.numeric(read.csv("/Users/oscar/Desktop/0 Studies/5 R statistical semantics package/MatlabRandom10000.csv"))
    
    ##################Import data if x is character or as semreps (i.e., allready been imported)
    if (is.character(x) == TRUE) {
      df <- tibble(x, y)
      df1 <- importTibble(df)
      df2 <- cbind(df1, df[2])
      df2 <- as.tibble(df2)
    } else {
      df2 <- cbind(x, as.tibble(y))
      df2 <- rename(df2, y = value)
      df2 <- as.tibble(df2)
    }
    
    ##################Selecting dataset with different number of dimensions
    ## Sequence to select dimensions from the semreps. SM-article state: "Adding 1, then multiplying by 1.3 and finally rounding to the nearest integer (e.g., 1, 3, 5, 8, where the next number of dimensions to be tested are the first 12; in other words ([8 +􏰄 1*] 􏱡 1.3)
    stop = Ndim
    new_num = 1
    selection_vector = 1
    while(new_num < stop) {
      new_num  <-  round((new_num + 1) * 1.3)
      selection_vector  <-  c(selection_vector, new_num)
    }
    #Changing the last number to the maximum number of dimensions
    selection_vector[length(selection_vector)] <- Ndim 
    
    #dim1==FALSE removes the attempt to use only one dimensions, since in some models more than one predictor is required
    if (dim1 == FALSE) {
      selection_vector <- selection_vector[2:length(selection_vector)]
    } else {
      selection_vector <- selection_vector
    }
    
    ###Create lists to store models and correlations in
    #INNER cross-validation lists
    models_wdifDims <- list()
    correlations_list <- list()
    #OUTER cross-validation lists
    output <- list()
    predicted_values <- list()
    predicted_index <- list()
    semrepDims <- list()
    results_corr_output <- list() #NEW
    #LAST output
    results <- list()
    
    ###################
      #### INNER for-loop that takes 90% of data to find the best number of semantic dimensions 
      for (i in 1:(length(selection_vector))) {
        #Select dataframe with required number of dimensions to be tested
        df3<- as.tibble(df2[,1:selection_vector[i]])
        df3$y <-  df2$y
        
        #### Multiple Linear Regression  
        #Setting seed for selecting training sets the same each time
        set.seed(MatlabX)
        #Define training control; verboseIter = TRUE show how long time each fold takes in the console.   
        train_control<- trainControl(method="cv", number=nrFolds, verboseIter = TRUE, savePredictions = 'final', ...) 
        
        options(warn=-1)  #Turn off warnings; i.e., Turning of function showing warning regarding rank deficiency since we're looking at final correlation between actual and predicted anyway. OK?
        #Train and save the model; the "." takes all variables except y.  
        model <- train(y~., data=df3, trControl=train_control, method= methodTrain, na.action = na.omit, ...) 
        models_wdifDims[[i]] <- model
        options(warn=1) #Turning on warnings again
        
        #Correlating actual with predicted values, and saving it in a list
        correlations_list[[i]] <- cor.test(model$pred$pred, model$pred$obs, method=methodCor, na.action=na.omit, ...)  
      }
      ########End of INNER cross-validation for loop
      ##########################
      
      #Get t-statistics, dfs, p-values, correlation estimates and type of method from the correlations (This is not all shown in the current code, but will keep it in the code) 
      results_corr <- do.call("rbind", lapply(correlations_list, '[', c(1:4, 7)))
      #Get strongest correlation, so that we can select the model that created it. 
      cor <- max(as.vector(unlist(results_corr[,"estimate"])))
      #Select best model with strongest correlation by selecting which numeric position  the highest correlation is positioned in; and if several take the first as this uses fewest dimensions. 
      nr_model <- which(as.vector(unlist(results_corr[,"estimate"])) == max(as.vector(unlist(results_corr[,"estimate"]))))
      if (length(nr_model) > 1) {
        nr_model <- nr_model[1]
      }else{
        nr_model <- nr_model
      }
      #Find and save the model associated with the highest correlation, and save the correaltion result
      output <- models_wdifDims[[nr_model]]
      #output <- list(models_wdifDims[[nr_model]], correlations_list[results_corr[,"estimate"]==cor])
      #Name the lists in the list
      #names(output) <- c("modelTraining", "evaluation")
  }

################################################################################################################################################################
########### End of trainModelForPrediction ###################################################################################################################
################################################################################################################################################################



