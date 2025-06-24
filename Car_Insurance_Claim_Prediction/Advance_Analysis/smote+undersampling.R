# Load necessary libraries
library(smotefamily)  # For SMOTE
library(ROSE)         # For undersampling
library(ggplot2)

# Load dataset
train_data_encoded <- read.csv('C:/Users/Hiruni/OneDrive/Desktop/train_encoded.csv')
train_data_encoded$is_claim <- as.factor(train_data_encoded$is_claim)

# Check original class distribution
print(table(train_data_encoded$is_claim))

# Apply SMOTE for oversampling the minority class
train_set_smote <- SMOTE(train_data_encoded[,-which(names(train_data_encoded)=="is_claim")], 
                         train_data_encoded$is_claim, K = 5, dup_size = 6)

# Convert back to dataframe and rename class column
smote_data <- train_set_smote$data
colnames(smote_data)[colnames(smote_data) == "class"] <- "is_claim"
smote_data$is_claim <- as.factor(smote_data$is_claim)

# Check class distribution after SMOTE
print("After SMOTE:")
print(table(smote_data$is_claim))

# Apply Undersampling to remove excess majority class instances
set.seed(123)  # For reproducibility
balanced_data <- ovun.sample(is_claim ~ ., data = smote_data, method = "under", 
                             N = 2 * table(smote_data$is_claim)[2], seed = 1)$data

# Check new class distribution after undersampling
print("After SMOTE + Undersampling:")
print(table(balanced_data$is_claim))

# Save the final balanced dataset
write.csv(balanced_data, 'C:/Users/Hiruni/OneDrive/Desktop/train_balanced.csv', row.names = FALSE)
print("Final balanced dataset saved as 'train_balanced.csv'.")

# Bar chart to visualize the balanced data
ggplot(balanced_data, aes(x = is_claim, fill = is_claim)) +
  geom_bar() +
  labs(title = "Bar Chart of is_claim Variable (After SMOTE + Undersampling)", 
       x = "is_claim", y = "Count") +
  scale_fill_manual(values = c("lightblue", "lightgreen")) +
  theme_minimal()
