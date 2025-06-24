###accuracy check for training set

train_new=read.csv("C:/Users/Hiruni/OneDrive/Desktop/train_smote_without_OHE.csv")
dim(train_new)

train_new$is_claim <- as.factor(train_new$is_claim) 
train_new$model <- as.factor(train_new$model) 
train_new$area_cluster <- as.factor(train_new$area_cluster) 
str(train_new)
levels(train_new$is_claim)

logistic_model <- glm(is_claim ~ ., data = train_new, family = binomial)
summary(logistic_model)

# Predict probabilities on training set
train_probs <- predict(logistic_model, newdata = train_new, type = "response")

# Convert probabilities to binary labels (Threshold = 0.5)
train_preds <- ifelse(train_probs > 0.5, 1, 0)

# Convert to factor
train_preds <- factor(train_preds, levels = c(0, 1))

library(caret)

# Compute confusion matrix
conf_matrix <- confusionMatrix(train_preds, train_new$is_claim)

# Print Accuracy
print(conf_matrix$overall["Accuracy"])


# Print Precision, Recall, and F1-score
print(conf_matrix$byClass[c("Precision", "Recall", "F1")])



###accuracy check for test set

test_set=read.csv("C:/Users/Hiruni/OneDrive/Desktop/test_set.csv")

test_set$model <- as.factor(test_set$model) 
test_set$area_cluster <- as.factor(test_set$area_cluster) 
test_set$is_claim=as.factor(test_set$is_claim)
str(test_set)

dim(test_set)

# Make predictions on test data (probabilities)
pred_probs <- predict(logistic_model, test_set, type = "response")
length(pred_probs) 
head(pred_probs)
# Convert probabilities to binary predictions (threshold = 0.5)
test_preds <- ifelse(pred_probs > 0.5, 1, 0)

# Convert to factor for evaluation
test_preds <- factor(test_preds, levels = c(0, 1))


# Add predictions to the test dataset
test_set$predicted_is_claim <- test_preds

# Save results to a CSV file
write.csv(test_set, "C:/Users/Hiruni/OneDrive/Desktop/predicted_test_results.csv", row.names = FALSE)

# Check the first few predictions
head(test_set)



# Ensure both predicted and actual values are factors with the same levels
test_set$is_claim <- factor(test_set$is_claim, levels = c(0, 1))
test_set$predicted_is_claim <- factor(test_set$predicted_is_claim, levels = c(0, 1))

# Load caret package
library(caret)

# Compute confusion matrix
conf_matrix <- confusionMatrix(test_set$predicted_is_claim, test_set$is_claim)

# Print Accuracy
accuracy <- conf_matrix$overall["Accuracy"]

print(conf_matrix$byClass[c("Precision", "Recall", "F1")])


print(paste("Test Set Accuracy:", round(accuracy, 4)))




