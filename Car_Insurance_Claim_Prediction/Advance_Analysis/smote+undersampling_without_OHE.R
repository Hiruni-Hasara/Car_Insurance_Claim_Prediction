library(dplyr)
library(caret)
# Assuming your training dataset is named 'train_data'
train_data <- read.csv("C:/Users/Hiruni/OneDrive/Desktop/train_balanced.csv")  # Load data if not already in memory
str(train_data)

train_data$is_claim <- as.factor(train_data$is_claim)  # Ensure response variable is still a factor

train_data$area_cluster <- apply(train_data[, c("area_clusterHigh", "area_clusterLow", "area_clusterMedium")], 
                                 1, function(x) colnames(train_data[, c("area_clusterHigh", "area_clusterLow", "area_clusterMedium")])[which.max(x)])

# Rename properly
train_data$area_cluster <- gsub("area_clusterHigh", "High", train_data$area_cluster)
train_data$area_cluster <- gsub("area_clusterMedium", "Medium", train_data$area_cluster)
train_data$area_cluster <- gsub("area_clusterLow", "Low", train_data$area_cluster)

# Convert to factor with correct levels
train_data$area_cluster <- factor(train_data$area_cluster, levels = c("Low", "Medium", "High"))

str(train_data)

train_data$model <- apply(train_data[, c("modelM1", "modelM2", "modelM3", "modelM4", "modelM5", "modelM6", "modelM7", "modelM8", "modelM9", "modelM10", "modelM11")], 
                          1, function(x) colnames(train_data[, c("modelM1", "modelM2", "modelM3", "modelM4", "modelM5", "modelM6", "modelM7", "modelM8", "modelM9", "modelM10", "modelM11")])[which.max(x)])

# Remove "modelM" prefix to get clean model names
train_data$model <- gsub("modelM", "M", train_data$model)
train_data$model <- as.factor(train_data$model)
str(train_data)
head(train_data)

 
# Select relevant variables only
relevant_columns <- setdiff(names(train_data), c("area_clusterHigh", "area_clusterMedium", "area_clusterLow",
                                                 "modelM1", "modelM2", "modelM3", "modelM4", "modelM5", 
                                                 "modelM6", "modelM7", "modelM8", "modelM9", "modelM10", "modelM11"))

train_data <- train_data[, relevant_columns]
str(train_data)

# Reorder columns by column names
new_order <- c("policy_tenure", "age_of_car", "age_of_policyholder",'area_cluster','model','is_claim')  # Define the desired order by column names
train_data <- train_data[, new_order]
str(train_data)
write.csv(train_data, "C:/Users/Hiruni/OneDrive/Desktop/train_smote_without_OHE.csv", row.names = FALSE)


