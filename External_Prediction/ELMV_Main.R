rstudioapi::getSourceEditorContext()$path
working_dir <- dirname(rstudioapi::getSourceEditorContext()$path)
source("ELMV_Functions.R")
output_dir <- paste0(working_dir,"/Output/") #Output Location


###### User input Load data
train_Data <- 
test_data  <-
hidden_data <-
outcome_index <- 


####################################################################################
####      S1.Generate Maximal Subsets  using Dynamic Programming
####################################################################################
heatmap_sublist_output<-generate_heat_map_func(train_Data[,-outcome_index]) #Exlucde outcome ID
##Heat map
heatmap<-heatmap_sublist_output[[1]]
colnames(heatmap)<-paste0(seq(1,ncol(heatmap)),"_fts")
rownames(heatmap)<-paste0(seq(1,nrow(heatmap)),"_pts")

##Sublist info
subset_list<-heatmap_sublist_output[[2]]

write.csv(subset_list, paste0(output_dir,"subset_list.csv"))

####################################################################################
####      S2.Filter out unqualifed subsets
###      For the subsets in the missing percentage catogory,                                    
####     Keep the subset that has max # of feature and max # of patients
####################################################################################
filtered_sublist <- Filter_unqualified_subsets_func(subset_list)
write.csv(filtered_sublist,paste0(output_dir,5,"perc_all_togehter_df_resolution",incre,".csv"))
p <- Plot_filtered_sublist_func(filtered_sublist)
p


####################################################################################
#######                S3. Prediction 
#######                 Read Sim data
####################################################################################
start_time <- Sys.time()
n_of_label_class <- 3
outcome_name <- "Species"
hamming_weight <- 0.8
fdist_weight <- 0.2
K_1 <- 5
K_2 <- 0.5
Final_predictions <- ELMV_Prediction_func(train_data,test_data,outcome_name,n_of_label_class,hidden_data,hamming_weight,fdist_weight,K_1,K_2)
end_time <- Sys.time()
total_time <- end_time - start_time


