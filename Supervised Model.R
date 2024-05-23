rm(list = ls())
library(readr)
library(tidyverse)
library(dplyr)
library(randomForest)
library(caret)
library(pROC)
################################################################################
data <- read.csv("WA_Fn-UseC_-HR-Employee-Attrition.csv")
str(data)
set.seed(123)
random_index <- sample(nrow(data))
data <- data[random_index, ]

random_forest <- function(data, fold) {
  library(randomForest)
  library(caret)
  library(pROC)
  
  # Ensure the target variable is a factor with two levels
  data$Attrition <- factor(data$Attrition, levels = c("No", "Yes"))
  
  # Split the data into training and testing sets
  set.seed(42)
  trainIndex <- createDataPartition(data$Attrition, p = .7, 
                                    list = FALSE, 
                                    times = 1)
  train_data <- data[trainIndex, ]
  test_data <- data[-trainIndex, ]
  
  # Initialize vectors to store results
  validation_results <- data.frame(
    fold = integer(fold),
    accuracy = numeric(fold),
    precision = numeric(fold),
    recall = numeric(fold),
    f1 = numeric(fold)
  )
  
  # Create k folds for cross-validation
  k <- cut(seq(1, nrow(train_data)), breaks = fold, labels = FALSE, include.lowest = TRUE)
  
  for (i in 1:fold) {
    # Create training and validation sets
    validation_indices <- which(k == i)
    train_indices <- which(k != i)
    
    train_fold <- train_data[train_indices, ]
    validation_fold <- train_data[validation_indices, ]
    
    # Train the random forest model
    model <- randomForest(as.factor(Attrition) ~ ., data = train_fold, method = "class")
    
    # Predict on the validation set
    validation_pred <- predict(model, newdata = validation_fold, type = "class")
    validation_matrix <- confusionMatrix(validation_pred, as.factor(validation_fold$Attrition))
    
    # Store validation results
    validation_results$fold[i] <- i
    validation_results$accuracy[i] <- validation_matrix$overall["Accuracy"]
    validation_results$precision[i] <- validation_matrix$byClass["Pos Pred Value"]
    validation_results$recall[i] <- validation_matrix$byClass["Sensitivity"]
    validation_results$f1[i] <- validation_matrix$byClass["F1"]
    
    # Print validation metrics for the current fold
    # print(paste("Validation Fold", i, "Metrics:"))
    # print(validation_matrix)
  }
  
  # Calculate mean validation metrics
  mean_validation_metrics <- colMeans(validation_results[, -1], na.rm = TRUE)
  print("Mean Validation Metrics:")
  print(mean_validation_metrics)
  
  # Train the final model on the entire training set
  final_model <- randomForest(as.factor(Attrition) ~ ., data = train_data, method = "class")
  
  # Predict on the testing set
  test_pred <- predict(final_model, newdata = test_data, type = "class")
  test_pred_prob <- predict(final_model, newdata = test_data, type = "prob")[,2]
  test_matrix <- confusionMatrix(test_pred, as.factor(test_data$Attrition))
  
  # Null model (majority class)
  majority_class <- names(sort(table(train_data$Attrition), decreasing = TRUE))[1]
  null_pred <- rep(majority_class, nrow(test_data))
  null_pred_prob <- ifelse(null_pred == "Yes", 1, 0)
  null_matrix <- confusionMatrix(as.factor(null_pred), as.factor(test_data$Attrition))
  
  # Print testing metrics
  print("Testing Metrics (Random Forest):")
  print(test_matrix)
  
  print("Testing Metrics (Null Model):")
  print(null_matrix)
  
  # Compute ROC curves and AUC
  rf_roc <- roc(test_data$Attrition, test_pred_prob, levels = rev(levels(test_data$Attrition)))
  null_roc <- roc(test_data$Attrition, null_pred_prob, levels = rev(levels(test_data$Attrition)))
  
  # Print AUC
  print(paste("AUC (Random Forest):", auc(rf_roc)))
  print(paste("AUC (Null Model):", auc(null_roc)))
  
  # Compute ROC curves and AUC
  rf_roc <- roc(test_data$Attrition, test_pred_prob, levels = rev(levels(test_data$Attrition)))
  null_roc <- roc(test_data$Attrition, null_pred_prob, levels = rev(levels(test_data$Attrition)))
  
  # Print AUC
  print(paste("AUC (Random Forest):", auc(rf_roc)))
  print(paste("AUC (Null Model):", auc(null_roc)))
  
  # Plot ROC curves with adjusted parameters
  plot(rf_roc, col = "blue", main = "ROC Curves", xlim = c(1, 0), ylim = c(0, 1), asp = 1)
  plot(null_roc, col = "red", add = TRUE)
  legend("bottomright", legend = c("Random Forest", "Null Model"), col = c("blue", "red"), lwd = 2, cex = 0.6)
  
  return(list(
    validation_results = validation_results,
    mean_validation_metrics = mean_validation_metrics,
    test_matrix = test_matrix,
    null_matrix = null_matrix,
    rf_roc = rf_roc,
    null_roc = null_roc
  ))
}



