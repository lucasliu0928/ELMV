############################################################################
############################################################################
###                                                                      ###
###                       PREPROCCESSING FUNCTIONS                       ###
###                                                                      ###
############################################################################
############################################################################
recode_YESandNO_Cols_func <- function (dataset , col_of_interest){
  yes_indexes <- which(dataset[ , col_of_interest] == "Yes")
  no_indexes <- which(dataset[ , col_of_interest] == "No")
  dataset[yes_indexes, col_of_interest] <- 1
  dataset[no_indexes, col_of_interest]  <- 0
  return(dataset)
}


recode_TandF_col_func <- function(dataset , col_of_interest){
  yes_indexes <- which(dataset[ , col_of_interest] == "T")
  no_indexes <- which(dataset[ , col_of_interest] == "F")
  dataset[yes_indexes, col_of_interest] <- 1
  dataset[no_indexes, col_of_interest]  <- 0
  return(dataset)
  
}



create_DummyVar_func <- function(data_to_recode , original_var_name, dummy_var_name){
  #data_to_recode  <- recode_data_with_dummy_variable
  #dummy_var_name  <- "WHITE"
  #original_var_name <- "RACE"
  indexes_toyes <- which(data_to_recode[ ,original_var_name] == dummy_var_name)
  indexes_stays_missing <- which(is.na(data_to_recode[ ,original_var_name] ) == T)
  
  data_to_recode[indexes_toyes, dummy_var_name] <- 1
  data_to_recode[indexes_stays_missing, dummy_var_name] <- NA
  data_to_recode[-c(indexes_toyes, indexes_stays_missing) , dummy_var_name] <- 0
  
  return(data_to_recode)
}



############################################################################
############################################################################
###                                                                      ###
###                    Model Performance Functions                       ###
###                                                                      ###
############################################################################
############################################################################
Find_optimal_cutoffvalue_inROC <- function(predprobs, actual_labels){
  #Find Optimal Cutoff Value (Optimal Decision Threshold) 
  #Return 1. AUC under ROC CURVE
  #Return 2. Optimal threhold
  
  roc_obj <- roc(actual_labels, predprobs,quiet = T)
  auc_score<-auc(roc_obj)
  cutoff_results<-coords(roc_obj, "best", ret=c("threshold", "specificity", "sensitivity", "accuracy",
                                                "precision", "recall"), transpose = FALSE)
  cut_off_thres<-cutoff_results$threshold[1]  ###best accuracy might happend at three point, so choose the first one,least threshold
  
  return(list(auc_score,cut_off_thres))
  
}

#Convert probability to integer labels using the Optimal Cutoff Value (Optimal Decision Threshold)
Convert_Predprob_To_IntLabel_func <- function(predprobs,cut_off_thres){
  pred_list <- NA
  for(i in 1:length(predprobs)){
    if(predprobs[i] > cut_off_thres){
      pred_list[i]<-1
    }else {
      pred_list[i]<-0
    }
  }
  return(pred_list)
}

#Perforamnce in ACC, PREC,RECALL, F1 
Compute_model_performance_fun <- function(final_predictions, actual){
  #return 1: comfusion matrix
  #return 2: performance table
  final_pred   <- factor(final_predictions, levels = c(0,1))
  final_actual <- factor(actual, levels = c(0,1))
  
  cm <- confusionMatrix(final_pred, final_actual,positive = "1", mode = "prec_recall")
  ACC <- cm$overall[1]
  prec <- cm$byClass[5]
  recall <- cm$byClass[6]
  f1 <- cm$byClass[7]
  
  cm2 <- confusionMatrix(final_pred, final_actual,positive = "0", mode = "prec_recall")
  ACC2 <- cm2$overall[1]
  prec2 <- cm2$byClass[5]
  recall2 <- cm2$byClass[6]
  f12 <- cm2$byClass[7]
  
  perf_df <- cbind.data.frame(ACC,prec,recall,f1,ACC2,prec2,recall2,f12)
  rownames(perf_df)<-NULL
  colnames(perf_df)<- c("Accuracy_C1" , "Precision_C1", "Recall_C1" , "F1_C1","Accuracy_C0" , "Precision_C0", "Recall_C0" , "F1_C0")
  
  return(list(cm,perf_df,cm2))
}





########S3_Prediction.R Fucntions and Simulation_data_baseline.R: 
#Functions
#Xgboost model
xgboost_model_func <- function(train_data,test_data,outcome_name,xgb_params,n_rounds){
  test_data_part  <- test_data[,!(names(test_data) %in% outcome_name)]
  test_label      <- test_data[, outcome_name]
  dtest           <- xgb.DMatrix(data = as.matrix(test_data_part), label=test_label)
  train_data_part <- train_data[, !(names(train_data) %in% outcome_name)]
  train_label     <- train_data[, outcome_name]
  dtrain          <- xgb.DMatrix(data = as.matrix(train_data_part), label = train_label)
  
  xgb_model <- xgb.train(params = xgb_params,data = dtrain,nrounds = n_rounds)
  xgb_pred<- predict(xgb_model, dtest)
  
  i_matrix <- xgb.importance(colnames(train_data_part), model = xgb_model)
  return(list(xgb_pred,i_matrix))
}

xgbooost_train_func <- function(train_data,outcome_name,xgb_params,n_rounds){
  train_data_part <- train_data[, !(names(train_data) %in% outcome_name)]
  train_label     <- train_data[, outcome_name]
  dtrain          <- xgb.DMatrix(data = as.matrix(train_data_part), label = train_label)
  
  xgb_model <- xgb.train(params = xgb_params,data = dtrain,nrounds = n_rounds)
  
  i_matrix <- xgb.importance(colnames(train_data_part), model = xgb_model)
  return(list(xgb_model,i_matrix))
}

perform_cv_func <- function(cv_data,outcome_name,sample_flag){
  #cv_data <- curr_train_df
  n_of_class <- length(unique(cv_data[ , outcome_name]))
  
  ## prediction performance CV on training data 
  xgb_params <- list(booster = "gbtree","objective" = "multi:softprob",
                     eval_metric = "merror",
                     num_class = n_of_class)
  n_rounds <- 10
  
  cv_predprob_list <-list()
  for (i in 1:nrow(cv_data)){
    cv_train <- cv_data[-i, ]
    cv_test <- cv_data[i,]
    
    if (sample_flag== 1){
      cv_train_sampled <- sampling_func(cv_train,outcome_name,1,i)
    }else if (sample_flag == 0) {
      cv_train_sampled <- sampling_func(cv_train,outcome_name,0,i)
    }else {
      cv_train_sampled <- cv_train
    }
    
    # make xgb.DMatrix
    label_index <- which(colnames(cv_train_sampled) == outcome_name)
    train_data   <- cv_train_sampled[, - label_index]
    train_label  <- cv_train_sampled[, label_index]
    train_matrix <- xgb.DMatrix(data = as.matrix(train_data), label = train_label)
    
    cv_model <- xgbooost_train_func(cv_train_sampled,outcome_name,xgb_params,10)
    xgb_res <- xgboost_model_func(cv_train_sampled,cv_test,outcome_name,xgb_params,n_rounds)
    cv_predprob_list[[i]] <- xgb_res[[1]]
    
  }
  
  cv_model_pred <- do.call(rbind,cv_predprob_list)
  rownames(cv_model_pred) <- rownames(cv_data)
  OOF_prediction <- data.frame(cv_model_pred) %>%
    mutate(max_prob = max.col(., ties.method = "last"),
           label = cv_data[,outcome_name] + 1)
  
  # confusion matrix
  CV_CM <- confusionMatrix(factor(OOF_prediction$max_prob),
                           factor(OOF_prediction$label),
                           mode = "prec_recall")
  CV_ACC <- round(CV_CM$overall[1],2)
  CV_byclass_performance <- round(CV_CM$byClass[,c(5,6,7)], 2)
  
  return(list(CV_ACC,CV_byclass_performance))
}


perform_cv_func2 <- function(train_df,outcome_name){
  #train_df <- curr_train_df
  n_of_class <- length(unique(train_df[ , outcome_name]))
  
  ##Hyperparameter tuning from CV
  # CV_folds <- 5 # number of folds 
  # CV_repeats <- 3 # number of repeats
  # minimum_resampling <- 5 # minimum number of resamples
  # training_set <- train_df
  # tuning_method <- "grid"
  # set.seed(2)
  # best_parameters <- tune_XGB_hyperparamter_func(CV_folds, CV_repeats,minimum_resampling , training_set, tuning_method)
  
  
  ## prediction performance CV on training data 
  xgb_params <- list(booster = "gbtree","objective" = "multi:softprob",
                     eval_metric = "merror",
                     num_class = n_of_class)
  n_rounds <- 10
  cv.nfold  <- nrow(train_df) #LOOCV
  
  # make xgb.DMatrix
  label_index <- which(colnames(train_df) == outcome_name)
  train_data   <- train_df[, - label_index]
  train_label  <- train_df[, label_index]
  train_matrix <- xgb.DMatrix(data = as.matrix(train_data), label = train_label)
  
  # Fit cv.nfold XGB models and save OOF predictions
  cv_model <- xgb.cv(params = xgb_params,
                     data = train_matrix, 
                     nrounds = n_rounds, # number of XGBoost rounds
                     nfold = cv.nfold,
                     verbose = FALSE,
                     prediction = TRUE)
  library("dplyr")    # for some data preperation
  OOF_prediction <- data.frame(cv_model$pred) %>%
    mutate(max_prob = max.col(., ties.method = "last"),
           label = train_label + 1)
  
  # confusion matrix
  CV_CM <- confusionMatrix(factor(OOF_prediction$max_prob,levels = seq(1,n_of_class, by =1)),
                           factor(OOF_prediction$label,seq(1,n_of_class, by =1)),
                           mode = "prec_recall",positive = as.character(max(unique(train_label))+1))
  CV_ACC <- round(CV_CM$overall[1],2)
  
  if(is.null(nrow(CV_CM$byClass))==F){
    CV_byclass_performance <- round(CV_CM$byClass[,c(5,6,7)], 2)
  }else{ #if only 2 class label
    CV_byclass_performance <- round(CV_CM$byClass[c(5,6,7)], 2)
  }
  return(list(CV_CM,CV_ACC,CV_byclass_performance))
}


#two class return AUC
perform_cv_func3 <- function(train_df,outcome_name,xgb_params,n_rounds){
  #train_df <- curr_train_df
  n_of_class <- length(unique(train_df[ , outcome_name]))
  
  ##Hyperparameter tuning from CV
  # CV_folds <- 5 # number of folds 
  # CV_repeats <- 3 # number of repeats
  # minimum_resampling <- 5 # minimum number of resamples
  # training_set <- train_df
  # tuning_method <- "grid"
  # set.seed(2)
  # best_parameters <- tune_XGB_hyperparamter_func(CV_folds, CV_repeats,minimum_resampling , training_set, tuning_method)
  
  ## prediction performance CV on training data 
  # xgb_params <- list(booster = "gblinear","objective" = "binary:logistic")
  # n_rounds <- 10
  cv.nfold  <- nrow(train_df) #LOOCV
  
  # make xgb.DMatrix
  label_index <- which(colnames(train_df) == outcome_name)
  train_data   <- train_df[, - label_index]
  train_label  <- train_df[, label_index]
  train_matrix <- xgb.DMatrix(data = as.matrix(train_data), label = train_label)
  
  # Fit cv.nfold XGB models and save OOF predictions
  cv_model <- xgb.cv(params = xgb_params,
                     data = train_matrix, 
                     nrounds = n_rounds, # number of XGBoost rounds
                     nfold = cv.nfold,
                     verbose = FALSE,
                     prediction = TRUE)
 
  #
  Final_preds_probs<-cv_model$pred
  actual <- train_label
  #Plot ROC curve
  roc_curve<- roc.curve(scores.class0 = Final_preds_probs, weights.class0 =  actual, curve=TRUE)  #1 for posstive class, #0 for negtive class
  #plot(roc_curve)
  
  #Use optimal threhold in AUC  ROC to get final prediction labels
  roc_restuls <- Find_optimal_cutoffvalue_inROC(Final_preds_probs, actual)
  AUC_score     <- as.numeric(roc_restuls[[1]])
  cut_off_thres <- roc_restuls[[2]]
  final_predictions <- Convert_Predprob_To_IntLabel_func(Final_preds_probs, cut_off_thres)
  
  #Perforamnce in ACC, PREC,RECALL, F1 using final prediction of optimal threshold
  perf_results <- Compute_model_performance_fun(final_predictions,actual)
  CV_CM <- perf_results[[1]]
  perf_df<- perf_results[[2]]
  perf_df$AUC <- AUC_score
  CV_ACC <- CV_CM$overall[1]
  CV_byclass_performance <- round(perf_df[-c(1,5)],2)

  return(list(CV_CM,CV_ACC,CV_byclass_performance))
}




#upsampling
sampling_func <-function(train_data,outcome_name,upsample_flag,seed_i){
  train_data$ID <- rownames(train_data) #keep the ID
  label_col <- which(colnames(train_data) == outcome_name)
  if(upsample_flag==1){
    set.seed(seed_i)
    up_train <- upSample(x = train_data[, -label_col],
                         y = as.factor(train_data[,label_col]), yname = outcome_name)                         
    table(up_train[,outcome_name]) 
    final_train <- up_train
  }else if(upsample_flag==0){ #downsample
    set.seed(seed_i)
    down_train <- downSample(x = train_data[, -label_col],
                             y = as.factor(train_data[,label_col]), yname = outcome_name)                         
    table(down_train[,outcome_name]) 
    final_train <- down_train
    
  }else{
    final_train <- train_data
  }
  
  rownames(final_train) <- final_train$ID
  final_train <- final_train[, -which(colnames(final_train) == "ID")]
  final_train[,outcome_name] <- as.numeric(final_train[,outcome_name])-1
  return(final_train)
}

perfrom_cv_and_fullmodel_func <- function(analysis_data,analysis_train_IDs,analysis_test_IDs,outcome_name){
  # analysis_data <- cor1_data
  # analysis_train_IDs <- train_IDs
  # analysis_test_IDs <- test_IDs
  
  train_indexes <- which(rownames(analysis_data) %in% analysis_train_IDs)
  train_df <- analysis_data[train_indexes, ]
  test_df <- analysis_data[-train_indexes, ]
  n_of_class <- length(unique(analysis_data[ , outcome_name]))
  
  ##Hyperparameter tuning from CV
  # CV_folds <- 5 # number of folds 
  # CV_repeats <- 3 # number of repeats
  # minimum_resampling <- 5 # minimum number of resamples
  # training_set <- train_df
  # tuning_method <- "grid"
  # set.seed(2)
  # best_parameters <- tune_XGB_hyperparamter_func(CV_folds, CV_repeats,minimum_resampling , training_set, tuning_method)
  
  
  ## prediction performance CV on training data 
  xgb_params <- list(booster = "gbtree","objective" = "multi:softprob",
                     eval_metric = "merror",
                     num_class = n_of_class,
                     max_depth = 10,
                     eta = 0.3,
                     gamma = 1,
                     colsample_bytree = 0.6,
                     min_child_weight = 3,
                     subsample = 1)
  n_rounds <- 10
  cv.nfold  <- nrow(train_df) #LOOCV
  
  # make xgb.DMatrix
  label_index <- which(colnames(train_df) == outcome_name)
  train_data   <- train_df[, - label_index]
  train_label  <- train_df[, label_index]
  train_matrix <- xgb.DMatrix(data = as.matrix(train_data), label = train_label)
  
  # Fit cv.nfold XGB models and save OOF predictions
  cv_model <- xgb.cv(params = xgb_params,
                     data = train_matrix, 
                     nrounds = n_rounds, # number of XGBoost rounds
                     nfold = cv.nfold,
                     verbose = FALSE,
                     prediction = TRUE)
  library("dplyr")    # for some data preperation
  OOF_prediction <- data.frame(cv_model$pred) %>%
    mutate(max_prob = max.col(., ties.method = "last"),
           label = train_label + 1)
  
  # confusion matrix
  CV_CM <- confusionMatrix(factor(OOF_prediction$max_prob),
                           factor(OOF_prediction$label),
                           mode = "prec_recall")
  CV_ACC <- round(CV_CM$overall[1],2)
  CV_byclass_performance <- round(CV_CM$byClass[,c(5,6,7)], 2)
  
  
  
  ## FULL Model
  ## External Validation 20% random sampled data
  #Use best paramters from tuning
  set.seed(123)
  xgb_params <- list(booster = "gbtree","objective" = "multi:softmax",
                     eval_metric = "merror",
                     num_class = n_of_class,
                     max_depth = 10,
                     eta = 0.3,
                     gamma = 1,
                     colsample_bytree = 0.6,
                     min_child_weight = 3,
                     subsample = 1)
  
  n_rounds<- 10
  xgb_res <- xgboost_model_func(train_df,test_df,outcome_name,xgb_params,n_rounds)
  
  pred<- xgb_res[[1]]
  im_matrix <- xgb_res[[2]]
  curr_top10_fs <- im_matrix$Feature[1:10]
  xgb.ggplot.importance(im_matrix,n_clusters = 1) + theme(legend.position = "none")
  im_matrix_df <- cbind.data.frame(im_matrix[, 1],round(im_matrix[,5],2))
  
  #Prediction Perforamnce
  pred   <- factor(pred, levels = c(0,1,2))
  actual <- factor(test_df[, outcome_name], levels = c(0,1,2))
  cm <- confusionMatrix(pred, actual,mode = "prec_recall")
  cm$overall[1]
  cm
  
  Full_Model_Pred_Df <- cbind.data.frame(pred,actual)
  rownames(Full_Model_Pred_Df) <- rownames(test_df)
  Full_model_ACC <- round(cm$overall[1],2)
  Full_model_byclass_performance <- round(cm$byClass[,c(5,6,7)], 2)
  rownames(Full_model_byclass_performance) <- rownames(CV_byclass_performance) #rename as orginal label 
  return(list(CV_ACC,CV_byclass_performance,Full_model_ACC,Full_model_byclass_performance,im_matrix_df,Full_Model_Pred_Df))
}


