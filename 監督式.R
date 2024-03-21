rm(list = ls())
library(dplyr)
data<- read.csv("WA_Fn-UseC_-HR-Employee-Attrition.csv")

#有無離職皆抽八成當train，兩成當test
data$index =c(1:nrow(data))
train_data <- data %>% group_by(Attrition) %>% sample_frac(0.8)
test_data  <- anti_join(data, train_data, by = "index")
#移除切資料用的index
traind <-as.data.frame(train_data[-28])
testd <-as.data.frame(test_data[-28])


#方法一：決策樹
library(rpart)
library(rpart.plot)
library(caret)
tree <- rpart(Attrition ~. ,data=traind, method="class")
pred <- predict(tree, newdata=testd, type="class")
#length(pred)
#length(testd$Attrition)

#Confusion matrix
table(Real = testd$Attrition, Predict = pred)
#分類規則
rpart.plot(tree)
#輸出成png
png("decisiontree.png",units = "in",width=7,height=7,res=100)
rpart.plot(tree)
dev.off()
#分類標準
rpart.rules(tree)
#各項指標
confusionMatrix(pred,as.factor(testd$Attrition))
#找出重要變數
tree$variable.importance


#方法二：隨機森林
library(randomForest)
rf <- randomForest(as.factor(Attrition)~., data = traind, importance=TRUE, na.action = na.omit) 
pred2 <- predict(rf, newdata=testd, type="class")
#各項指標
confusionMatrix(pred2,as.factor(testd$Attrition))
#找出重要變數
sort(importance(rf)[,3],T)
sort(importance(rf)[,4],T)

#選樹的數量(增加每一棵決策樹，整體誤差的改變量)
#黑色線:整體的OOB error rate
#其他顏色虛線:各類別的OOB Error Rate
plot(rf)
legend("topright", colnames(rf$err.rate),col=1:4,cex=0.8,fill=1:4)
