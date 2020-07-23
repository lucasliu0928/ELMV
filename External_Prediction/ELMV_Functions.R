#######################################################################################
############       S1. Generate Maximal Subsets  using Dynamic Programming         #####
#######################################################################################
#### Author: Lucas Liu
#### Date: 05/22/2019
#### Upated 05/05/2020
#### Input: twoD map 
#### Output: heat: 228x78  
####         For each entry: the smallest number of missing values in 2Dmap when having this many patients and this many features
orderedBymissing_percentage_func<-function(inputData){
  n_features<-length(colnames(inputData))
  n_patients<-length(rownames(inputData))
  
  #compute percent (# NA features) / (total # features) for each patient
  pt_percentage<-NA
  for (i in 1:n_patients){
    pt_percentage[i]<-length(which(is.na(inputData[i,])==T))/n_features  #updated for NA check instead of ==0 for diabte data 05052020
  }
  p_max<-max(pt_percentage)
  
  #compute percent (# NA patients) / (total # patient) for each feaature
  f_percentage<-NA
  for (i in 1:n_features){
    f_percentage[i]<-length(which(is.na(inputData[,i])==T))/n_patients  #updated for NA check instead of ==0 for diabte data 05052020
  }
  f_max<-max(f_percentage)
  
  ###Order the twoD Map by pt percentage and f percentage
  ### the first patients is the one who has least missing NA features
  ### the first feature is the one who has least missing NA patients
  ordered_twoD_Map<-inputData[order(pt_percentage),order(f_percentage),drop=FALSE] #drop=FALSE will keep the variable name when only single column left
  
  return(list(ordered_twoD_Map,p_max,f_max))
  
}

generate_heat_map_func<-function(inputData){
  total_n_features<-ncol(inputData)  #41
  total_n_patients<- nrow(inputData)  #623
  
  heatmap<-data.frame(matrix(NA,nrow = total_n_patients,ncol = total_n_features))
  heatmap_dataframe<-rep(list(list(NA)),total_n_patients)
  sublist_df<-data.frame(matrix(NA,nrow = total_n_patients*total_n_features,ncol = 6))
  colnames(sublist_df)<-c("n_patients","n_features","n_missing_values","missing_ratio","IDs","Features")
  ct<-1
  
  current_inputData<-inputData
  
  total_start_time <- Sys.time()
  for (removed_p in 0:(total_n_patients-1)){
    print(removed_p)
    start_time <- Sys.time()
    for (removed_f in 0:(total_n_features-1)){
      if(removed_p==0){ #for the last row
        ordered_twoD_Map<-orderedBymissing_percentage_func(current_inputData)[[1]]
        #get patients ID list
        patient_IDs<-rownames(ordered_twoD_Map)
        #get feature list
        feature_list<-colnames(ordered_twoD_Map)
        #get number of missing value
        n_missing_value<-length(which(is.na(ordered_twoD_Map)==T)) #updated for NA check instead of ==0 for diabte data 05052020
        
        ###fill heatmap
        heatmap[total_n_patients-removed_p,total_n_features-removed_f]<-n_missing_value
        heatmap_dataframe[[total_n_patients-removed_p]][[total_n_features-removed_f]]<-ordered_twoD_Map
        
        ####Updaate sublist_df
        sublist_df$n_patients[ct]<-total_n_patients-removed_p
        sublist_df$n_features[ct]<-total_n_features-removed_f
        sublist_df$n_missing_values[ct]<-n_missing_value
        sublist_df$missing_ratio[ct]<-n_missing_value/(sublist_df$n_patients[ct]*sublist_df$n_features[ct]) #updated on Aug 1 #it was: n_missing_value/(total_n_patients*total_n_features)
        sublist_df$IDs[ct]<-paste(patient_IDs,collapse = ",")
        sublist_df$Features[ct]<- paste(feature_list,collapse = ",")
        ct<-ct+1
        
        #update current twoD_map
        current_inputData<-ordered_twoD_Map[,1:(ncol(ordered_twoD_Map)-1),drop=FALSE]
        
      }else if (removed_p!=0) {
        if(removed_f==0){
          previous_fullfeature_Data<-heatmap_dataframe[[total_n_patients-removed_p+1]][[total_n_features]]
          current_inputData<-previous_fullfeature_Data[-nrow(previous_fullfeature_Data),,drop=FALSE]
          
          ordered_twoD_Map<-orderedBymissing_percentage_func(current_inputData)[[1]]
          #get patients ID list
          patient_IDs<-rownames(ordered_twoD_Map)
          #get feature list
          feature_list<-colnames(ordered_twoD_Map)
          #get number of missing value
          n_missing_value<-length(which(is.na(ordered_twoD_Map)==T))   #updated for NA check instead of ==0 for diabte data 05052020
          
          ###fill heatmap
          heatmap[total_n_patients-removed_p,total_n_features-removed_f]<-n_missing_value
          heatmap_dataframe[[total_n_patients-removed_p]][[total_n_features-removed_f]]<-ordered_twoD_Map
          
          ####Updaate sublist_df
          sublist_df$n_patients[ct]<-total_n_patients-removed_p
          sublist_df$n_features[ct]<-total_n_features-removed_f
          sublist_df$n_missing_values[ct]<-n_missing_value
          sublist_df$missing_ratio[ct]<-n_missing_value/(sublist_df$n_patients[ct]*sublist_df$n_features[ct]) #updated on Aug 1
          sublist_df$IDs[ct]<-paste(patient_IDs,collapse = ",")
          sublist_df$Features[ct]<- paste(feature_list,collapse = ",")
          ct<-ct+1
          
        }else{ #removed_f !=0
          previousFromRight_missingPrec<-heatmap[total_n_patients-removed_p,total_n_features-removed_f+1]
          previousFromBottom_missingPrec<-heatmap[total_n_patients-removed_p+1,total_n_features-removed_f]
          
          previousFromRight__Data<-heatmap_dataframe[[total_n_patients-removed_p]][[total_n_features-removed_f+1]]
          previousFrombottom__Data<-heatmap_dataframe[[total_n_patients-removed_p+1]][[total_n_features-removed_f]]
          
          if(previousFromRight_missingPrec<previousFromBottom_missingPrec){#Use the relevant smaller df from last iteration
            previous_data<-previousFromRight__Data
            current_inputData<-previous_data[,-ncol(previous_data),drop=FALSE] #remove the last feature
          }else if(previousFromRight_missingPrec>=previousFromBottom_missingPrec){
            previous_data<-previousFrombottom__Data
            current_inputData<-previous_data[-nrow(previous_data),,drop=FALSE] #remove the last patient
          }
          
          ordered_twoD_Map<-orderedBymissing_percentage_func(current_inputData)[[1]]
          #get patients ID list
          patient_IDs<-rownames(ordered_twoD_Map)
          #get feature list
          feature_list<-colnames(ordered_twoD_Map)
          #get number of missing value
          n_missing_value<-length(which(is.na(ordered_twoD_Map)==T))  #updated for NA check instead of ==0 for diabte data 05052020
          
          
          ###fill heatmap
          heatmap[total_n_patients-removed_p,total_n_features-removed_f]<-n_missing_value
          heatmap_dataframe[[total_n_patients-removed_p]][[total_n_features-removed_f]]<-ordered_twoD_Map
          ####Updaate sublist_df
          sublist_df$n_patients[ct]<-total_n_patients-removed_p
          sublist_df$n_features[ct]<-total_n_features-removed_f
          sublist_df$n_missing_values[ct]<-n_missing_value
          sublist_df$missing_ratio[ct]<-n_missing_value/(sublist_df$n_patients[ct]*sublist_df$n_features[ct]) #updated on Aug 1
          sublist_df$IDs[ct]<-paste(patient_IDs,collapse = ",")
          sublist_df$Features[ct]<- paste(feature_list,collapse = ",")
          ct<-ct+1
          
        }
        
      }
      
      
    }
    end_time <- Sys.time()
    
    print(end_time - start_time)
    
  }
  
  print(paste0("total time eclapsed:"  ,Sys.time()-total_start_time))
  return(list(heatmap,sublist_df))
}


####################################################################################
####      S2.Filter out unqualifed subsets
###      For the subsets in the missing percentage catogory,                                    
####     Keep the subset that has max # of feature and max # of patients
####################################################################################
Filter_unqualified_subsets_func <- function(subset_list){
  #1. Round up Missing ratio in %
  for (mr_i in 1:nrow(subset_list)){
    curr_mr <- subset_list$missing_ratio[mr_i]
    if (curr_mr == 0 ){
      subset_list$missing_ratio[mr_i] <- round(curr_mr)
    } else if (curr_mr != 0 ){
      subset_list$missing_ratio[mr_i] <- round(curr_mr*100,2)
    }
  }
  
  all_missing_perc <- subset_list$missing_ratio 
  unique_missing_ratio_2dmap<-sort(unique(all_missing_perc))
  rounded_max <- round(max(all_missing_perc))
  res_divider <- rounded_max  #/rounded_max is incre by 1%
  incre<- (rounded_max - 0) / res_divider #/rounded_max is incre by 1%
  resolution_seq <- seq(0,max(all_missing_perc),incre) #c(0,1,2,3,4,5,6,7,8,9,10,11,max(all_missing_perc)
  missing_ratio_of_interest_2dmap<-unique_missing_ratio_2dmap[which(unique_missing_ratio_2dmap %in% c(resolution_seq,max(all_missing_perc)))] #also include the exact max
  missing_resulution_list <- missing_ratio_of_interest_2dmap
  all_missing_ratio_df<-list(NA)
  for (i in 1:length(missing_ratio_of_interest_2dmap)){
    current_ratio<-missing_ratio_of_interest_2dmap[i]
    current_sublist_info<-subset_list[which(subset_list$missing_ratio==current_ratio),]
    
    
    #For each unique number of patients, get the max number of feature sets
    unique_n_pt<-unique(current_sublist_info$n_patients)
    missing_ratio_df<-data.frame(matrix(NA,nrow =length(unique_n_pt),ncol = 6))
    colnames(missing_ratio_df)<-c("n_patients","n_features","n_missing_values","missing_ratio","IDs","Features")
    
    for(j in 1:length(unique_n_pt)){
      subset_indexes<-which(current_sublist_info$n_patients==unique_n_pt[j])
      subset_having_same_n_patients<-current_sublist_info[subset_indexes,]
      max_feature_index<-which(subset_having_same_n_patients$n_features==max(subset_having_same_n_patients$n_features))
      
      missing_ratio_df[j,]<-subset_having_same_n_patients[max_feature_index,]
    }
    
    #For each unique number of feature, get the max number of patients
    unique_n_f<-unique(missing_ratio_df$n_features)
    missing_ratio_df2<-data.frame(matrix(NA,nrow =length(unique_n_f),ncol = 6))
    colnames(missing_ratio_df2)<-c("n_patients","n_features","n_missing_values","missing_ratio","IDs","Features")
    
    for(k in 1:length(unique_n_f)){
      subset_indexes<-which(missing_ratio_df$n_features==unique_n_f[k])
      subset_having_same_n_features<-missing_ratio_df[subset_indexes,]
      max_pt_index<-which(subset_having_same_n_features$n_patients==max(subset_having_same_n_features$n_patients))
      missing_ratio_df2[k,]<-subset_having_same_n_features[max_pt_index,]
    }
    all_missing_ratio_df[[i]]<-missing_ratio_df2
    
    
  }
  
  all_togehter_df<-do.call(rbind, all_missing_ratio_df)
  all_togehter_df$missing_ratio <- factor(paste0(all_togehter_df$missing_ratio,"%"), levels = unique(paste0(all_togehter_df$missing_ratio,"%")))
  return(all_togehter_df)
}


####Plot Function
Plot_filtered_sublist_func <- function(filtered_sublist){
  filtered_sublist$missing_ratio <- as.numeric(gsub("%","",filtered_sublist$missing_ratio))
  filtered_sublist$missing_ratio <- as.factor(filtered_sublist$missing_ratio)
  library(ggplot2)
  library(tidyverse)
  library(plotly)
  gg <- ggplot(filtered_sublist, aes(x=n_patients, y=n_features)) +
    geom_point(aes(col=missing_ratio), size = 3)+
    xlim(c(0, max(filtered_sublist$n_patients))) +
    ylim(c(0, max(filtered_sublist$n_features))) +
    labs(y="number of features",
         x="number of patients",
         title= "Missing ratio Plot")+
    theme_bw()+
    theme(legend.title = element_blank()) +
    theme(axis.text.x  = element_text(size=12), axis.text.y  = element_text(size=12))
  #gg
  #ggplotly(gg)
  return(gg)
}


####################################################################################
####      S3. Generate Models and prediction
####################################################################################
source("Ultilities.R")
library(openxlsx)
library(randomForest)
library(PRROC)
library(pROC)
library(xgboost)
library(caret)
library(useful) #Mean imputation
library(naniar) #visual missingness
library(caret)
library("dplyr")    # for some data preperation

softmax <- function(x){
  score.exp <- exp(x)
  probs <- score.exp/sum(score.exp)
  return(probs)
}    

hamming_dist_func <- function(vector_of_interest,df_to_compare){
  #vector_of_interest <- curr_test_m_vector
  #df_to_compare <- hidden_data_m_df
  
  hamming_dist <-as.data.frame(matrix(NA,nrow = 1, ncol = nrow(df_to_compare)))
  colnames(hamming_dist) <- rownames(df_to_compare)
  rownames(hamming_dist) <- rownames(vector_of_interest)
  for (df_i in 1:nrow(df_to_compare)){
    hamming_dist[1,df_i] <- sum(vector_of_interest != df_to_compare[df_i,])
  }
  return(hamming_dist) 
}


#Check if each submodel is available to external patients, if pateints has all important features to each model
check_submodel_availibility_func <- function(data_tobe_predicted, nofmodels){
  #data_tobe_predicted <- test_data
  
  submodel_externalpts_check <- as.data.frame(matrix(NA, nrow = nrow(data_tobe_predicted), ncol = nofmodels))
  rownames(submodel_externalpts_check) <- rownames(data_tobe_predicted)
  colnames(submodel_externalpts_check) <- paste0("Model",colnames(submodel_externalpts_check))
  for (i in 1:length(important_features_list)){
    curr_important_fs <- important_features_list[[i]]
    
    curr_test_data <- data_tobe_predicted[,curr_important_fs]
    avaiable_pts_indexes<- which(complete.cases(curr_test_data)==TRUE)
    notavaiable_pts_indexes<- which(complete.cases(curr_test_data)==FALSE)
    submodel_externalpts_check[avaiable_pts_indexes,i] <- 1 #1 means avaiable
    submodel_externalpts_check[notavaiable_pts_indexes,i] <- 0 #0 means not
    
  }
  return(submodel_externalpts_check)
}

#Prediction Func
prediction_func <- function(train_sets,data_tobe_predicted,feature_selected,outcome_name){
  train_sets <- train_sets[ ,c(feature_selected,outcome_name)]
  data_tobe_predicted <-  data_tobe_predicted[ ,c(feature_selected,outcome_name)]
  ## FULL Model
  set.seed(123)
  n_of_class <- length(unique(train_sets[ , outcome_name]))
  xgb_params <- list(booster = "gbtree","objective" = "multi:softmax",
                     eval_metric = "merror",
                     num_class = n_of_class)
  
  n_rounds<- 10
  xgb_res <- xgboost_model_func(train_sets,data_tobe_predicted,outcome_name,xgb_params,n_rounds)
  
  pred<- xgb_res[[1]]
  
  
  return(pred)
}


ELMV_Prediction_func <- function(train_data,test_data,outcome_name,hidden_data,hamming_weight,fdist_weight,K_1,K_2){
  ## Get hidden data mising matrix 06/01 for external prediction use
  outcome_name <- "Species"
  hidden_data_without_outcome <- hidden_data[, -which(colnames(hidden_data)==outcome_name)]
  hidden_data_m_df <- as.data.frame(matrix(NA, nrow = nrow(hidden_data_without_outcome), ncol = ncol(hidden_data_without_outcome)))
  colnames(hidden_data_m_df) <- colnames(hidden_data_without_outcome)
  rownames(hidden_data_m_df) <- rownames(hidden_data_without_outcome)
  for (h_i in 1:nrow(hidden_data_without_outcome)){
    curr_hidden_f_vector <- hidden_data_without_outcome[h_i,]
    
    missing_f_indexes <- which(is.na(curr_hidden_f_vector)==T)
    if (length(missing_f_indexes) > 0){ #if there are missings
      curr_hidden_m_vector <- curr_hidden_f_vector #inital as the orignal feature vector
      curr_hidden_m_vector[missing_f_indexes] <- 1  #1 means missing
      curr_hidden_m_vector[-missing_f_indexes] <- 0 #0 means not missing
      
      hidden_data_m_df[h_i,] <- curr_hidden_m_vector
    }else {
      hidden_data_m_df[h_i,] <- 0 #0 means not missing
    }
    
  }
  
  ## Load subset list info
  subset_info_file_name <- all_togehter_df_file_names[which(grepl(paste0("\\b",missing_percs[mr],"perc"),all_togehter_df_file_names)==T)]
  all_togehter_df <- read.csv(paste0(all_togher_dir,resolution_dir,subset_info_file_name), stringsAsFactors = F)
  all_togehter_df <- all_togehter_df[, -1]
  
  ##remove patients < 10 subset
  if (length(which(all_togehter_df$n_patients <10)) > 0){
    all_togehter_df <- all_togehter_df[-which(all_togehter_df$n_patients <10),]
  }
  if (length(which(all_togehter_df$n_features <2))){
    all_togehter_df <- all_togehter_df[-which(all_togehter_df$n_features <2),]
  }
  
  #Check if subsets in small modesl have all three label class, if not, remove it from model list
  nof_class_check_flag <- NA
  for (i in 1:nrow(all_togehter_df)){
    current_subset_info <- all_togehter_df[i,]
    curr_subset_Ids <- unlist(strsplit(current_subset_info$IDs,","))
    curr_subset_fs <- c(unlist(strsplit(current_subset_info$Features,",")),outcome_name)
    f_indexes <- which(colnames(train_data) %in% curr_subset_fs)
    id_indexes <- which(rownames(train_data) %in% curr_subset_Ids)
    
    curr_train_df <- train_data[id_indexes, f_indexes]
    
    nof_unique_class <- length(unique(curr_train_df[,outcome_name]))
    nof_class_check_flag[i] <- nof_unique_class
  }
  
  model_indexes_to_remove <- which(nof_class_check_flag < n_of_label_class)
  if (length(model_indexes_to_remove) > 0){
    all_togehter_df <- all_togehter_df[-model_indexes_to_remove,]
  }
  nofmodels <- nrow(all_togehter_df)
  
  nof_aval_subsets[mr] <- nofmodels
  print(paste0("MR",missing_percs[mr],":",nof_aval_subsets[mr]))
  
  
  #training each submodel the entire subset
  #Find important features
  #Find LOOCV performance
  #Find Hidden set predition performance
  #' @important #Do not specify details in parameters, cuz the hyperparater learned before might results in no-tree model detected
  submodel_list <-list()
  imatrix_list <-list()
  all_features_list <- list()
  important_features_list <- list()
  nof_important_features <- NA
  nof_featuresInsubsets <- NA
  MissingRatio_subsets <- NA
  nof_PtsInsubsets <-NA
  train_subset_lists <- list()
  cv_acc<-NA
  cv_byclassperforamnce_list <-list(NA)
  cv_cm <- list(NA)
  
  for (i in 1:nofmodels){
    current_subset_info <- all_togehter_df[i,]
    curr_subset_Ids <- unlist(strsplit(current_subset_info$IDs,","))
    all_features_list[[i]] <- unlist(strsplit(current_subset_info$Features,","))
    
    curr_subset_fs <- c(unlist(strsplit(current_subset_info$Features,",")),outcome_name)
    nof_featuresInsubsets[i] <- length(all_features_list[[i]])
    nof_PtsInsubsets[i] <- length(curr_subset_Ids)
    
    f_indexes <- which(colnames(train_data) %in% curr_subset_fs)
    id_indexes <- which(rownames(train_data) %in% curr_subset_Ids)
    
    curr_train_df <- train_data[id_indexes, f_indexes]
    curr_test_df <- test_data[ , which(colnames(test_data) %in% curr_subset_fs)]
    
    #missing ratio exclude outomce column
    MissingRatio_subsets[i] <- sum(sapply(curr_train_df[,curr_subset_fs[-length(curr_subset_fs)]], function(x) sum(is.na(x))))/(nrow(curr_train_df)*(ncol(curr_train_df)-1))
    
    #Imputation if there are missing values
    #library(useful)
    #imputed_train_df <- simple.impute(curr_train_df, fun = mean,)
    #imputed_test_df <- simple.impute(curr_test_df, fun = mean,)
    
    n_of_class <- length(unique(curr_train_df[ , outcome_name]))
    xgb_params <- list(booster = "gbtree","objective" = "multi:softprob",
                       eval_metric = "merror",
                       num_class = n_of_class)
    n_rounds <- 10
    
    xgb_train_res <- xgbooost_train_func(curr_train_df,outcome_name,xgb_params,n_rounds)
    submodel_list[[i]] <- xgb_train_res[[1]]
    imatrix_list[[i]] <- xgb_train_res[[2]]
    curr_imatrix <- imatrix_list[[i]]
    important_f_indexes <- which(abs(curr_imatrix$Gain) >=  quantile(abs(curr_imatrix$Gain))[3]) #feature having gain >50% of all gains are considered as important features
    important_features_list[[i]] <- curr_imatrix$Feature[important_f_indexes]
    nof_important_features[i] <- length(important_f_indexes)
    
    #LOOCV 
    cv_res <- perform_cv_func2(curr_train_df,outcome_name)
    cv_cm[[i]]<-cv_res[[1]]
    cv_acc[i] <- cv_res[[2]]
    cv_byclassperforamnce_list[[i]] <- cv_res[[3]]
    train_subset_lists[[i]] <- curr_train_df
    
  }
  
  
  cv_byclass_perf_all <- as.data.frame(do.call(rbind,cv_byclassperforamnce_list))
  class1_perf <- cv_byclass_perf_all[which(grepl("Class..1",rownames(cv_byclass_perf_all))==T),]
  colnames(class1_perf) <- paste0("Class1_", colnames(class1_perf))
  class2_perf <- cv_byclass_perf_all[which(grepl("Class..2",rownames(cv_byclass_perf_all))==T),]
  colnames(class2_perf) <- paste0("Class2_", colnames(class2_perf))
  class3_perf <- cv_byclass_perf_all[which(grepl("Class..3",rownames(cv_byclass_perf_all))==T),]
  colnames(class3_perf) <- paste0("Class3_", colnames(class3_perf))
  
  Final_submodels_avg_acc[mr] <- mean(cv_acc,na.rm = T)
  
  #'Important feature and Available matrix
  #'Columns are all features for all subsets
  #'Rows are models, entry= "aval not important" or "important" or "not avl"
  availible_features_each_model <- all_features_list
  important_features_each_model <- important_features_list
  all_unique_features <- sort(unique(unlist(all_features_list)))
  model_feature_importance_df <- as.data.frame(matrix(NA,nrow = nofmodels,ncol = length(all_unique_features)))
  colnames(model_feature_importance_df) <- all_unique_features
  rownames(model_feature_importance_df) <- paste0("Model",seq(1,nofmodels, by = 1))
  
  for (model_i in 1:nofmodels){
    curr_aval_fs <- availible_features_each_model[[model_i]]
    curr_import_fs <- important_features_each_model[[model_i]]
    
    #unavaliable features indexes
    inaval_f_indexes <- which(!colnames(model_feature_importance_df) %in% curr_aval_fs)
    
    #Available features indexes
    aval_f_indexes<- which( colnames(model_feature_importance_df) %in% curr_aval_fs)
    
    #important features indexex
    important_f_indexes<- which(colnames(model_feature_importance_df) %in% curr_import_fs)
    
    #Available and important
    aval_Important_f_indexes <- intersect(aval_f_indexes,important_f_indexes)
    
    #Available and not important
    aval_notImportant_f_indexes <- setdiff(aval_f_indexes,important_f_indexes)
    
    
    model_feature_importance_df[model_i, aval_Important_f_indexes] <- "Available and Important"
    model_feature_importance_df[model_i, aval_notImportant_f_indexes] <- "Available, But Not Important"
    model_feature_importance_df[model_i, inaval_f_indexes] <- NA
    
    
  }
  
  
  ##' Final consusus Important feature 
  ##' # of time being important / # of time being Available
  final_importance_score <- NA
  for (model_f_j in 1: ncol(model_feature_importance_df)) {
    curr_f_ofinterest <- model_feature_importance_df[,model_f_j]
    aval_indexes <- which(grepl("Available",curr_f_ofinterest)==T)
    impor_indexes <- which(curr_f_ofinterest == "Available and Important")
    final_importance_score[model_f_j] <- length(impor_indexes)/length(aval_indexes) 
  }
  final_importance_score_df <- cbind.data.frame(colnames(model_feature_importance_df),final_importance_score)
  ordered_final_importance_score_df <- final_importance_score_df[order(final_importance_score_df$final_importance_score,decreasing = T),]
  
  pred_out_name <- gsub("/","",resolution_dir)
  pred_out_name <- gsub("olution|ided","",pred_out_name)
  output_name <- paste0(gsub("_missingSimData.csv","",file_names[mr]),"_Consensus_Important_features",pred_out_name,".csv")
  #write.csv(ordered_final_importance_score_df,output_name)
  
  
  ##For hidden data
  ###2. Predict for hidden pts
  #For each submodel, predict for its all hidden pts
  nof_curr_hiddenTest <- NA
  currModel_acc <- NA
  hidden_pred_df <- as.data.frame(matrix(NA,nrow = nrow(hidden_data) ,ncol = nofmodels))
  colnames(hidden_pred_df) <- paste0("Model",seq(1, nofmodels, by= 1))
  rownames(hidden_pred_df) <- rownames(hidden_data)
  
  for (hm_i in 1:nofmodels){ #for each model
    curr_train_df <- train_subset_lists[[hm_i]]
    #feature_selected <- colnames(curr_train_df)[-which(colnames(curr_train_df) == outcome_name)] #All features
    feature_selected <- important_features_list[[hm_i]] #top features
    curr_test_df <- hidden_data
    nof_curr_hiddenTest[hm_i] <- nrow(curr_test_df)
    curr_test_actual <- curr_test_df[,outcome_name]
    model_preds <- prediction_func(curr_train_df,curr_test_df,feature_selected,outcome_name)
    currModel_pred_df <- cbind.data.frame(factor(model_preds,levels = c(0,1,2)),factor(curr_test_actual,levels = c(0,1,2)))
    colnames(currModel_pred_df) <- c("model_preds","curr_test_actual")
    rownames(currModel_pred_df) <- rownames(curr_test_df)
    hidden_pred_df[,hm_i] <- currModel_pred_df$model_preds
    cm <- confusionMatrix(table(currModel_pred_df$model_preds,currModel_pred_df$curr_test_actual))
    currModel_acc[hm_i] <- cm$overall[1]
    
    
  }
  
  model_hidden_perf_df <- cbind.data.frame(currModel_acc,nof_curr_hiddenTest,cv_acc,
                                           class1_perf,class2_perf,class3_perf,
                                           nof_featuresInsubsets, nof_PtsInsubsets,nof_important_features,round(MissingRatio_subsets*100,2))
  colnames(model_hidden_perf_df) <- c("HiddenSet_ACC","NofHiddenCase","CV_ACC",
                                      colnames(class1_perf),colnames(class2_perf),colnames(class3_perf),
                                      "N_Fs","N_Ps","N_ImportanFs","MissingRatio")
  rownames(model_hidden_perf_df) <- paste0("Model",rownames(model_hidden_perf_df))
  
  pred_out_name <- gsub("/","",resolution_dir)
  pred_out_name <- gsub("olution|ided","",pred_out_name)
  output_name <- paste0(gsub("_missingSimData.csv","",file_names[mr]),"_Model_HiddenSet_perf_",pred_out_name,".csv")
  #write.csv(model_hidden_perf_df,output_name)
  
  ##find relationship between hidden ACC and CV acc and other 
  # lm_model <- lm(HiddenSet_ACC ~ CV_ACC+N_Fs+N_Ps+N_ImportanFs+MissingRatio, data = model_hidden_perf_df)
  # lm.res <- summary(lm_model)
  # lm_res_df <- lm.res$coefficients[2:6,c(1,4)]
  # pos_criterion_indexes <- as.vector(which(lm_res_df[,1]>0 & lm_res_df[,2]<=0.05))
  # neg_criterion__indexes<- as.vector(which(lm_res_df[,1]<0 & lm_res_df[,2]<=0.05))
  # pos_criterion_to_choose <- rownames(lm_res_df)[pos_criterion_indexes]
  # neg_criterion_to_choose <- rownames(lm_res_df)[neg_criterion__indexes]
  
  ##For each pts in hidden set, check which model predict right or wrong
  ##1: right, 0:wrong
  hidden_pred_rightOrWrong_df <- as.data.frame(matrix(NA,nrow = nrow(hidden_data) ,ncol = nofmodels))
  colnames(hidden_pred_rightOrWrong_df) <- paste0("Model",seq(1, nofmodels, by= 1))
  rownames(hidden_pred_rightOrWrong_df) <- rownames(hidden_data) 
  for (hid_i in 1:nrow(hidden_pred_df)){
    curr_pts_Id <- rownames(hidden_pred_df)[hid_i]
    curr_pts_allpreds <- hidden_pred_df[hid_i,]
    curr_pts_actual <- hidden_data[which(rownames(hidden_data) == curr_pts_Id) ,outcome_name]
    
    correct_pred_indexes <- which(curr_pts_allpreds == curr_pts_actual)
    wrong_pred_indexes <- which(curr_pts_allpreds != curr_pts_actual)
    hidden_pred_rightOrWrong_df[hid_i,correct_pred_indexes] <- 1
    hidden_pred_rightOrWrong_df[hid_i,wrong_pred_indexes] <- 0
    
  }
  
  output_name <- paste0(gsub("_missingSimData.csv","",file_names[mr]),"_HiddenSet_pred_rightOrWrong_df_",pred_out_name,".csv")
  #write.csv(hidden_pred_rightOrWrong_df,output_name)
  
  
  ##For external pts
  final_pred <- NA
  final_actual <-NA
  final_pred_by_copy_most_sim_pts <- NA
  final_pred3 <- NA
  avg_hamming_dist <- NA
  avg_f_dist <- NA
  intersect_model_indexes <- NA
  n_of_correct_pred_test_models <-NA
  pred_list <-list(NA)
  for (pts in 1:nrow(test_data)){     #For each pt
    #test data
    pts_Id <- rownames(test_data)[pts]
    final_test <- test_data[pts,]
    final_actual[pts] <- final_test[,outcome_name]
    
    #All Models predct
    pred <-NA
    for (m in 1:length(train_subset_lists)){
      final_train <- train_subset_lists[[m]]
      feature_selected <- important_features_list[[m]] #top features
      #feature_selected <- colnames(final_train)[-which(colnames(final_train) == outcome_name)] #all features
      pred[m] <- prediction_func(final_train,final_test,feature_selected,outcome_name)
    }
    pred_list[[pts]]<-pred
    
    ##'Compute simulatrity between test pts and hidden set pts
    ##GEt original feature vector 06/01 
    curr_test_f_vector <- final_test[, -which(colnames(final_test)==outcome_name)]
    
    ##Get test data mising vector  
    missing_f_indexes <- which(is.na(curr_test_f_vector)==T)
    curr_test_m_vector <- curr_test_f_vector #inital as the orignal feature vector
    curr_test_m_vector[missing_f_indexes] <- 1  #1 means missing
    curr_test_m_vector[-missing_f_indexes] <- 0 #0 means not missing
    
    ##sim 1, original distance norm2
    ##1.Attche the test data tot the hidden data frame, so dist function can be applied
    attached_f_df <- rbind(hidden_data_without_outcome,curr_test_f_vector)
    dist_df <- as.matrix(dist(attached_f_df, method = "euclidean")) #Missing vvalus are excluded from all computations involving the rows within which they occur
    curr_test_dist_df <- dist_df[which(rownames(dist_df) == rownames(curr_test_f_vector)),-which(colnames(dist_df) == rownames(curr_test_f_vector))]
    na_dist_indexes<-which(is.na(curr_test_dist_df)==T)
    if(length(na_dist_indexes)>0){
      curr_test_dist_df[na_dist_indexes] <- Inf #0 means no match non NA entrys
    }
    curr_test_sim_df <- -curr_test_dist_df #sim_df, larger the better, its negtive of distance
    
    avg_f_dist[pts] <- mean(as.numeric(curr_test_dist_df)) 
    
    
    ##Sim2, missing vector distance hamming dist: # of time they match in position
    curr_test_hamming_dist_df <- hamming_dist_func(curr_test_m_vector, hidden_data_m_df)
    curr_test_hamming_sim_df <- -curr_test_hamming_dist_df
    avg_hamming_dist[pts] <- mean(as.numeric(curr_test_hamming_dist_df)) 
    
    
    
    #Combine probability 
    curr_hamming_prob <- softmax(curr_test_hamming_sim_df)
    curr_fsim_prob <- softmax(curr_test_sim_df)
    #'@TODO: if mean(as.numeric(curr_test_hamming_dist_df)) is larger than 1/3 of ncol(curr_test_m_vector), do not weight it?
    if (missing_percs[mr] > 50 ){
      hamming_weight <- 0.2
      fdist_weight <- 0.8
    }else if (missing_percs[mr] <= 67){
      hamming_weight <- 0.5
      fdist_weight <- 0.5
    }
    curr_comb_prob <- curr_hamming_prob*hamming_weight+curr_fsim_prob*fdist_weight #weight combine probility
    curr_sort_comb_prob <- sort(curr_comb_prob,decreasing = T)
    
    #top 5 simimilar prob pts in hiddend set
    most_similar_pts_in_hidden <- colnames(curr_sort_comb_prob)[1:K_1]
    most_sim_pts_models_df <- hidden_pred_rightOrWrong_df[which(rownames(hidden_pred_rightOrWrong_df) %in% most_similar_pts_in_hidden),]
    most_sim_pts_model_indexes <- as.vector(which(colSums(most_sim_pts_models_df)> K_2*length(most_similar_pts_in_hidden))) #at least half of top sim pts choose these model
    
    #Check if we just copy the label from most similir pts
    #This line of code could be delete
    most_sim_pts_label <- hidden_data[which(rownames(hidden_data) == most_similar_pts_in_hidden[1]), outcome_name]
    final_pred_by_copy_most_sim_pts[pts] <- most_sim_pts_label
    ##Model selection:
    aval_model_perf <- model_hidden_perf_df[most_sim_pts_model_indexes, ]
    
    #Check
    index_of_model_corrected_pred_test <- which(pred==final_actual[pts])
    index_of_model_corrected_pred_mostSimHidden <- most_sim_pts_model_indexes
    intersect_model_indexes[pts] <- length(intersect(index_of_model_corrected_pred_test,index_of_model_corrected_pred_mostSimHidden))
    n_of_correct_pred_test_models[pts] <- length(index_of_model_corrected_pred_test)
    #Predict option1: 
    max_hiddentACC_models_perf_df <- aval_model_perf
    #condition: must have the most max for all criterion
    if (missing_percs[mr] >= 50 & missing_percs[mr] <= 68){
      drops <- c("NofHiddenCase","MissingRatio","N_Fs","N_Ps","N_ImportanFs",colnames(class1_perf),colnames(class2_perf),colnames(class3_perf))
      max_hiddentACC_models_perf_df <-  max_hiddentACC_models_perf_df[ , !(names(max_hiddentACC_models_perf_df) %in% drops)]
    }else if (missing_percs[mr] < 50  | missing_percs[mr] > 68){
      drops <- c("NofHiddenCase",colnames(class1_perf),colnames(class2_perf),colnames(class3_perf))
      max_hiddentACC_models_perf_df <-  max_hiddentACC_models_perf_df[ , !(names(max_hiddentACC_models_perf_df) %in% drops)]
      
    }
    
    max_check_df <- as.data.frame(matrix(NA,nrow = nrow(max_hiddentACC_models_perf_df), ncol = ncol(max_hiddentACC_models_perf_df)))
    rownames(max_check_df) <-  rownames(max_hiddentACC_models_perf_df)
    colnames(max_check_df) <- colnames(max_hiddentACC_models_perf_df)
    for (max_j in 1:(ncol(max_hiddentACC_models_perf_df))){
      sorted_col <- sort(max_hiddentACC_models_perf_df[,max_j],decreasing = T)
      max_indexes <- which(max_hiddentACC_models_perf_df[,max_j] %in% sorted_col[1:5]) #higest 5
      max_check_df[max_indexes,max_j] <- 1
      
    }
    count_times_being_max <- rowSums(max_check_df,na.rm = T)
    if (missing_percs[mr] >= 50){
      MostMAx_index <- which(count_times_being_max >= 1) #do not have to be most max, at least being one time max
    }else if (missing_percs[mr] < 50){
      MostMAx_index <- which(count_times_being_max == max(count_times_being_max))
    }
    MostMAx_model_name <- rownames(max_check_df)[MostMAx_index]
    
    #If there are still multiple, then do majority voting
    final_index <- which(rownames(model_hidden_perf_df) %in% MostMAx_model_name)
    pred[final_index]
    majority_voteofmaxes <- as.numeric(names(which.max(table(pred[final_index]))))
    final_pred[pts] <- majority_voteofmaxes
    majority_voteofmaxes
    final_actual[pts]
    final_pred3[pts] <- as.numeric(names(which.max(table(pred[index_of_model_corrected_pred_mostSimHidden])))) #all models pred from similari pts
    
    #Plot
    #outdir <- "/Users/lucasliu/Desktop/prediction_pts_plot/"
    #outfile_name <- paste0(missing_percs[mr],"Perc_",pts_Id,".png")
    #png(paste0(outdir, outfile_name))
    #plot(model_hidden_perf_df$N_ImportanFs[final_index],pred[final_index] ,title(paste0(missing_percs[mr],"%Missing Data","\n",pts_Id, " Actual Label:" ,final_test[,outcome_name])))
    #dev.off()
  }
  
  
  check <- cbind.data.frame(avg_hamming_dist,avg_f_dist,intersect_model_indexes,n_of_correct_pred_test_models)
  rownames(check) <- rownames(test_data)
  
  final_pred_df <- cbind.data.frame(final_pred,final_actual)
  rownames(final_pred_df) <- rownames(test_data)
  
  pred_out_name <- gsub("/","",resolution_dir)
  pred_out_name <- gsub("olution|ided","",pred_out_name)
  output_name <- paste0(gsub("_missingSimData.csv","",file_names[mr]),"_pred_tb",pred_out_name,".csv")
  #write.csv(final_pred_df,output_name)
  
  
  notNA_preds_indexes <- which(is.na(final_pred_df$final_pred)==F)
  cm <- confusionMatrix(table(factor(final_pred,levels = c(0,1,2)),factor(final_actual,levels = c(0,1,2))))
  Final_acc[mr] <- cm$overall[1]
  print( Final_acc[mr])
  n_of_pred_pts[mr] <- length(notNA_preds_indexes)
  
  
  #####Check, Just Copy Most Sim label performance
  final_pred_df2 <- cbind.data.frame(final_pred_by_copy_most_sim_pts,final_actual)
  rownames(final_pred_df2) <- rownames(test_data)
  
  pred_out_name <- gsub("/","",resolution_dir)
  pred_out_name <- gsub("olution|ided","",pred_out_name)
  output_name <- paste0(gsub("_missingSimData.csv","",file_names[mr]),"_predByCopyMostSim_tb",pred_out_name,".csv")
  #write.csv(final_pred_df2,output_name)
  
  
  notNA_preds_indexes2 <- which(is.na(final_pred_df2$final_pred_by_copy_most_sim_pts)==F)
  cm <- confusionMatrix(table(factor(final_pred_by_copy_most_sim_pts,levels = c(0,1,2)),factor(final_actual,levels = c(0,1,2))))
  Final_acc2[mr] <- cm$overall[1]
  n_of_pred_pts2[mr] <- length(notNA_preds_indexes2)
  
  return(final_pred)
}

