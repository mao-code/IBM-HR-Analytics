if (!require(themis)) install.packages('themis')
library(themis)
library(caret)
library(randomForest)
# https://rdrr.io/cran/themis/man/smotenc.html

ibm <- read.csv("IBM-HR-Analytics/WA_Fn-UseC_-HR-Employee-Attrition.csv")
# imbalanced data
# No: 1233, Yes: 237

dist <- table(ibm$Attrition)
print(table(ibm$Attrition))
n <- nrow(ibm)
print(paste0('No: ', round(dist[[1]]/n*100,2),'%'))
print(paste0('Yes: ', round(dist[[2]]/n*100, 2),'%'))
ibm$Attrition <- factor(ibm$Attrition)

# train_test_split
set.seed(123)
trainIndex <- createDataPartition(ibm$Attrition, p = 0.7, list = FALSE)
train_data <- ibm[trainIndex, ]
test_data <- ibm[-trainIndex, ]

train_smote <- smotenc(train_data, 'Attrition', k = 8, over_ratio = 1)
print(table(train_smote$Attrition))
n <- nrow(train_smote)
dist <- table(train_smote$Attrition)
print(paste0('No: ', round(dist[[1]]/n*100,2),'%'))
print(paste0('Yes: ', round(dist[[2]]/n*100, 2),'%'))

# traing model
rf_model <- randomForest(Attrition~., data=train_smote, ntree=500, mtry=8)



predicted <- predict(rf_model, test_data , type = "response", )
predicted_prob <- predict(rf_model, test_data, type = "prob")[,"No"]


# metrics for random forest
conf_matrix <- table(test_data$Attrition, predicted)
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
precision <- diag(conf_matrix) / rowSums(conf_matrix)
recall <- diag(conf_matrix) / colSums(conf_matrix)
f1 <- (2*precision*recall)/(precision + recall)
roc_obj <- roc(test_data$Attrition, predicted_prob)
auc_value <- auc(roc_obj)
print('-------------------------')
print("Confusion Matrix")
print(conf_matrix)
print(paste("Accuracy:", round(accuracy, 4)))
print(paste("Precision:", round(precision, 4)))
print(paste("Recall:", round(recall, 4)))
print(paste("f1:",round(f1, 4)))
print(paste("ROC_AUC:", round(auc_value, 4)))

# 這裡的precision, recall, f1各有兩個值，因為是二元分類問題，
# 代表不同的target，第一個代表分類為"No"的情況下，分類正確的機率，
# 第二個代表分類為"Yes"的情況下，分類正確的機率，
# 因此目前還沒有解決資料不平衡的問題，target為"Yes"的情況下，得到的precision, recall, f1
