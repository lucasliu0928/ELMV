
data_dir <- "/Users/lucasliu/Desktop/DrChen_Projects/CA_Arrest _Project/Data/IRIS/IRIS_Correlation_ProportionMissing_SimData/MissingPerc_SimData"
ID_data_dir <- "/Users/lucasliu/Desktop/DrChen_Projects/CA_Arrest _Project/Data/IRIS"
All_data<- read.csv(paste0(data_dir, "/",5,"perc_missingSimData.csv"),stringsAsFactors = F)
rownames(All_data) <- All_data$X
All_data <- All_data[,-1]
Train_IDs<- read.csv(paste0(ID_data_dir, "/trainWithoutHidden_IDs.csv"),stringsAsFactors = F)
Train_Data <- All_data[which(rownames(All_data) %in% Train_IDs$x),]
outcome_name_index <- which(colnames(Train_Data) == "Species")


##################### Orgnized Code start here #####################
rstudioapi::getSourceEditorContext()$path
working_dir <- dirname(rstudioapi::getSourceEditorContext()$path)
source("ELMV_Functions.R")
##User input
output_dir <- paste0(working_dir,"/Output/")
Train_Data<- Train_Data[,-outcome_name_index] #Exlucde outcome ID


####################################################################################
####      S1.Generate Maximal Subsets  using Dynamic Programming
####################################################################################
heatmap_sublist_output<-generate_heat_map_func(Train_Data)
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
data_dir <- "/Users/lucasliu/Desktop/DrChen_Projects/CA_Arrest _Project/Data/IRIS/IRIS_Correlation_ProportionMissing_SimData/MissingPerc_SimData/"
file_names <- paste0(5,"perc_missingSimData.csv")

#Read filterd sublist
all_togher_dir <- "/Users/lucasliu/Desktop/DrChen_Projects/CA_Arrest _Project/Data/IRIS/IRIS_Correlation_ProportionMissing_SimData/Filtered_Sublist_HiddenSetEx/"
resolution_dir <- "resolution_dividedbymax/"
all_togehter_df_file_names <- list.files(paste0(all_togher_dir,resolution_dir))

#Read Ids
ID_data_dir <- "/Users/lucasliu/Desktop/DrChen_Projects/CA_Arrest _Project/Data/IRIS/"
train_IDs_df <- read.csv(paste0(ID_data_dir, "trainWithoutHidden_IDs.csv"),stringsAsFactors = F)
train_IDs <- train_IDs_df$x
test_IDs_df <- read.csv(paste0(ID_data_dir, "holdout_test_IDs.csv"),stringsAsFactors = F)
test_IDs <- test_IDs_df$x

Hidden_IDs_df <- read.csv(paste0(ID_data_dir, "hiddenSets_IDs.csv"),stringsAsFactors = F)
Hidden_IDs <- Hidden_IDs_df$x


all_data <- read.csv(paste0(data_dir, file_names),stringsAsFactors = F)
rownames(all_data) <- all_data$X
all_data <- all_data[,-1]
  
train_data <- all_data[which(rownames(all_data) %in% train_IDs),]
test_data <- all_data[which(rownames(all_data) %in% test_IDs),]
hidden_data <- all_data[which(rownames(all_data) %in% Hidden_IDs),]
  
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


