### Load Required Libraries
install.packages(c("tidyverse", "cluster", "factoextra", "ggplot2", "caret", "randomForest", "fastcluster", "dbscan", "NbClust"))
install.packages("writexl")  # Install writexl if not already installed
library(writexl)
library(tidyverse)
library(cluster)
library(factoextra)
library(ggplot2)
library(caret)
library(randomForest)
library(fastcluster)  # Optimized hierarchical clustering
library(dbscan)       # For DBSCAN clustering
library(NbClust)      # For determining the optimal number of clusters

# Load Encoded Dataset
data <- read.csv("C:/Users/User/Desktop/Colombo uni/3year - 2nd sem/ST 3082/2nd_project/train_encoded.csv")

### Optimized Sampling
set.seed(123)
sample_size <- min(5000, nrow(data))  # Reduce sample size for speed
sample_indices <- sample(1:nrow(data), size = sample_size)
data_sampled <- data[sample_indices, ]

# Select relevant features for clustering
risk_features <- data_sampled %>% select(age_of_policyholder, policy_tenure, age_of_car, is_claim)

# Convert and Handle Missing Values
risk_features <- risk_features %>% mutate(across(everything(), as.numeric))
risk_features <- risk_features %>% mutate(across(everything(), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))

# Normalize Data
risk_features_scaled <- scale(risk_features)

### Dimensionality Reduction (PCA)
pca_result <- prcomp(risk_features_scaled, scale. = TRUE)
risk_features_pca <- pca_result$x[, 1:2]  # Use the first two principal components

### Determine Optimal Number of Clusters (Dendrogram & NbClust)
dist_matrix <- dist(risk_features_pca, method = "euclidean")  # Compute distance matrix
hc <- hclust(dist_matrix, method = "ward.D2")  # Perform hierarchical clustering

# Plot Dendrogram
plot(hc, labels = FALSE, main = "Dendrogram for Hierarchical Clustering", sub = "", xlab = "", ylab = "Height")
abline(h = 10, col = "red", lty = 2)  # Adjust height cutoff as needed

# Optimal Clusters using NbClust
nb_result <- NbClust(risk_features_pca, distance = "euclidean", min.nc = 2, max.nc = 10, method = "ward.D2")
num_clusters <- nb_result$Best.nc[1]  # Choose the best cluster number based on majority vote

### Cut the Hierarchical Tree to Form Clusters
data_sampled$Risk_Level <- cutree(hc, k = num_clusters)
data_sampled$Risk_Level <- as.factor(data_sampled$Risk_Level)

### Summary Table for Hierarchical Clusters
cluster_summary <- data_sampled %>%
  group_by(Risk_Level) %>%
  summarise(
    Count = n(),
    Avg_Age = mean(age_of_policyholder),
    Avg_Policy_Tenure = mean(policy_tenure),
    Avg_Car_Age = mean(age_of_car),
    Claim_Rate = mean(is_claim),
    SD_Age = sd(age_of_policyholder),
    SD_Policy_Tenure = sd(policy_tenure),
    SD_Car_Age = sd(age_of_car)
  )

# Print the summary table
print(cluster_summary)

# Export Hierarchical Clustering summary table to Excel
write_xlsx(cluster_summary, path = "C:/Users/User/Desktop/Colombo uni/3year - 2nd sem/ST 3082/2nd_project/hierarchical_cluster_after_summary.xlsx")

### Visualizing Risk Segmentation

# Clustered 2D Visualization (PCA-based)
fviz_cluster(list(data = risk_features_pca, cluster = data_sampled$Risk_Level)) +
  labs(title = "Risk Segmentation Using Hierarchical Clustering")

# Histogram of Policy Tenure across Risk Levels
ggplot(data_sampled, aes(x = policy_tenure, fill = Risk_Level)) +
  geom_histogram(bins = 30, alpha = 0.7, position = "identity") +
  labs(title = "Policy Tenure Distribution by Risk Level", x = "Policy Tenure (Years)", y = "Count") +
  theme_minimal()

# Claim Rate by Risk Level
ggplot(data_sampled, aes(x = Risk_Level, fill = as.factor(is_claim))) +
  geom_bar(position = "fill") +
  labs(title = "Claim Rate by Risk Level", x = "Risk Level", y = "Proportion of Claims", fill = "Claim Status") +
  theme_minimal()

# Scatter Plot (Age vs. Policy Tenure, Colored by Risk Level)
ggplot(data_sampled, aes(x = age_of_policyholder, y = policy_tenure, color = Risk_Level)) +
  geom_point(alpha = 0.6) +
  labs(title = "Clustered Policyholders by Age and Policy Tenure", x = "Age of Policyholder", y = "Policy Tenure (Years)") +
  theme_minimal()
