rstudioapi::getSourceEditorContext()$path
working_dir <- dirname(rstudioapi::getSourceEditorContext()$path)
source("ELMV_Functions.R")
##User input
output_dir <- paste0(working_dir,"/Output/")
data_dir <- ""
file_names <- ""

all_data <- read.csv(paste0(data_dir, file_names),stringsAsFactors = F)
rownames(all_data) <- all_data$X
all_data <- all_data[,-1]

train_Data <- all_data[which(rownames(all_data) %in% train_IDs),]
test_data <- all_data[which(rownames(all_data) %in% test_IDs),]
hidden_data <- all_data[which(rownames(all_data) %in% Hidden_IDs),]

train_Data<- train_Data[,-outcome_name_index] #Exlucde outcome ID


####################################################################################
####      S1.Generate Maximal Subsets  using Dynamic Programming
####################################################################################
heatmap_sublist_output<-generate_heat_map_func(train_Data)
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


