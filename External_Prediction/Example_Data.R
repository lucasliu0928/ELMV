data_dir <- "/Users/lucasliu/Desktop/DrChen_Projects/CA_Arrest _Project/Data/IRIS/IRIS_Correlation_ProportionMissing_SimData/MissingPerc_SimData"
ID_data_dir <- "/Users/lucasliu/Desktop/DrChen_Projects/CA_Arrest _Project/Data/IRIS"
All_data<- read.csv(paste0(data_dir, "/",5,"perc_missingSimData.csv"),stringsAsFactors = F)
rownames(All_data) <- All_data$X
All_data <- All_data[,-1]
Train_IDs<- read.csv(paste0(ID_data_dir, "/trainWithoutHidden_IDs.csv"),stringsAsFactors = F)
Train_Data <- All_data[which(rownames(All_data) %in% Train_IDs$x),]
outcome_name_index <- which(colnames(Train_Data) == "Species")

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

