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

