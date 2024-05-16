rm(list = ls())
library(readr)
library(tidyverse)
library(dplyr)
################################################################################
data <- read.csv("WA_Fn-UseC_-HR-Employee-Attrition.csv")
str(data)
set.seed(123)
random_index <- sample(nrow(data))
data <- data[random_index, ]


decision_tree <-function(data,fold){

  library(rpart)
  
  k <- cut(seq(1,nrow(data)), breaks = fold, labels = FALSE, include.lowest = TRUE)
  train_result <-c()
  test_result <-c()
  valid_result <-c()
  
  for(i in 1:fold){

    test <- which(k == i)
    
    if (i == fold){
      validation <- which(k == 1)
    }else{
      validation <- which(k == i + 1)
    }
    train <- which(k != i & k != i+1)
    
    model <- rpart(as.factor(Attrition) ~., data=data[train,],method="class")
    
    #train
    train_pred <- predict(model, type="class")
    train_matrix <- table(real=data[train,]$Attrition, predict=train_pred)
    train_accuracy <- sum(diag(train_matrix))/sum(train_matrix)
    train_result <- c(train_result,train_accuracy)
    
    #valid
    valid_pred <- predict(model, newdata=data[validation,], type="class")
    valid_matrix <- table(real=data[validation,]$Attrition, predict=valid_pred)
    valid_accuracy <- sum(diag(valid_matrix))/sum(valid_matrix)
    valid_result <- c(valid_result,valid_accuracy)
    
    #test
    test_pred <- predict(model, newdata=data[test,], type="class")
    test_matrix <- table(real=data[test,]$Attrition, predict=test_pred)
    test_accuracy <- sum(diag(test_matrix))/sum(test_matrix)
    test_result <- c(test_result,test_accuracy)
  }
  
  set <- c(paste("fold",1:fold,sep=""),"average")
  
  #算平均
  train <- round(c(train_result,mean(train_result)),4)
  validation <- round(c(valid_result,mean(valid_result)),4)
  test <- round(c(test_result,mean(test_result)),4)
  
  output <- data.frame(set=set,training=train,validation=validation,test=test)[(fold+1),]
}

random_forest <-function(data,fold){
  
  library(randomForest)
  
  k <- cut(seq(1,nrow(data)), breaks = fold, labels = FALSE, include.lowest = TRUE)
  train_result <-c()
  test_result <-c()
  valid_result <-c()
  
  for(i in 1:fold){
    
    test <- which(k == i)
    
    if (i == fold){
      validation <- which(k == 1)
    }else{
      validation <- which(k == i + 1)
    }
    train <- which(k != i & k != i+1)
    
    model <- rpart(as.factor(Attrition) ~., data=data[train,],method="class")
    
    #train
    train_pred <- predict(model, type="class")
    train_matrix <- table(real=data[train,]$Attrition, predict=train_pred)
    train_accuracy <- sum(diag(train_matrix))/sum(train_matrix)
    train_result <- c(train_result,train_accuracy)
    
    #valid
    valid_pred <- predict(model, newdata=data[validation,], type="class")
    valid_matrix <- table(real=data[validation,]$Attrition, predict=valid_pred)
    valid_accuracy <- sum(diag(valid_matrix))/sum(valid_matrix)
    valid_result <- c(valid_result,valid_accuracy)
    
    #test
    test_pred <- predict(model, newdata=data[test,], type="class")
    test_matrix <- table(real=data[test,]$Attrition, predict=test_pred)
    test_accuracy <- sum(diag(test_matrix))/sum(test_matrix)
    test_result <- c(test_result,test_accuracy)
  }
  
  set <- c(paste("fold",1:fold,sep=""),"average")
  
  #算平均
  train <- round(c(train_result,mean(train_result)),4)
  validation <- round(c(valid_result,mean(valid_result)),4)
  test <- round(c(test_result,mean(test_result)),4)
  
  output <- data.frame(set=set,training=train,validation=validation,test=test)[(fold+1),]
}

XG_Boost <-function(data,fold){
  
  library(xgboost)

  k <- cut(seq(1,nrow(data)), breaks = fold, labels = FALSE, include.lowest = TRUE)
  train_result <-c()
  test_result <-c()
  valid_result <-c()

  for(i in 1:fold){

    test <- which(k == i)
    
    if (i == fold){
      validation <- which(k == 1)
    }else{
      validation <- which(k == i + 1)
    }
    train <- which(k != i & k != i+1)
    
    new_train <- Matrix::sparse.model.matrix(Attrition ~ .-1, data = data[train,])
    new_valid <- Matrix::sparse.model.matrix(Attrition ~ .-1, data = data[validation,])
    new_test <- Matrix::sparse.model.matrix(Attrition ~ .-1, data = data[test,])
    train_label = data[train,][,"Attrition"] == "Yes" 
    valid_label = data[validation,][,"Attrition"] == "Yes"
    test_label = data[test,][,"Attrition"] == "Yes"
    dtrain <- xgb.DMatrix(data = new_train, label=train_label)
    dvalid <- xgb.DMatrix(data = new_valid, label=valid_label)
    dtest <- xgb.DMatrix(data = new_test, label=test_label)
    model <- xgb.train(data = dtrain, max.depth=6, eta=0.3, nthread = 2,
                       nround = 15, eval.metric = "error", objective = "binary:logistic")
    
    #train
    train_pred <- predict(model, new_train)
    train_matrix <- table(real=data[train,]$Attrition, predict=train_pred>0.5)
    train_accuracy <- sum(diag(train_matrix))/sum(train_matrix)
    train_result <- c(train_result,train_accuracy)
    
    #valid
    valid_pred <- predict(model, new_valid)
    valid_matrix <- table(real=data[validation,]$Attrition, predict=valid_pred>0.5)
    valid_accuracy <- sum(diag(valid_matrix))/sum(valid_matrix)
    valid_result <- c(valid_result,valid_accuracy)
    
    #test
    test_pred <- predict(model, new_test)
    test_matrix <- table(real=data[test,]$Attrition, predict=test_pred>0.5)
    test_accuracy <- sum(diag(test_matrix))/sum(test_matrix)
    test_result <- c(test_result,test_accuracy)
    #pred <- prediction(valid_pred, valid$Attrition)
  }
  
  set <- c(paste("fold",1:fold,sep=""),"average")
  #算平均
  train <- round(c(train_result,mean(train_result)),4)
  validation <- round(c(valid_result,mean(valid_result)),4)
  test <- round(c(test_result,mean(test_result)),4)
  
  output <- data.frame(set=set,training=train,validation=validation,test=test)[(fold+1),]
}



# Important Features
importance_matrix <- xgb.importance(
  feature_names = colnames(xgboost_train), 
  model = xgb_model
)
importance_matrix
xgb.plot.importance(importance_matrix)