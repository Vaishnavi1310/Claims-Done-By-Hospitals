library(readr)
Insurance_Dataset_ <- read_csv("D:/new/Insurance Dataset .csv")
#View(Insurance_Dataset_)
Insurance_Dataset_ <- Insurance_Dataset_[,-c(1:5,7,14,16,18,21,26:27,31:32)]
str(Insurance_Dataset_)
data_factor <- as.data.frame(lapply(Insurance_Dataset_[,-c(16,19)],factor))
str(data_factor)
normalize<-function(x){
  return ( (x-min(x))/(max(x)-min(x)))
}
data_norm<-as.data.frame(lapply(Insurance_Dataset_[,c(16,19)],FUN=normalize))
final_data <- data.frame(data_factor,data_norm)
str(final_data)
sum(is.na(final_data))
summary(final_data)
library(Hmisc)
final_data["Days_spend_hsptl"] <- with(final_data,impute(final_data$Days_spend_hsptl,mode))
final_data["Description_illness"] <- with(final_data,impute(final_data$Description_illness,mode))
final_data["Mortality_risk"] <- with(final_data,impute(final_data$Mortality_risk,mode))
summary(final_data)
sum(is.na(final_data))

set.seed(3)
final_data_1<- final_data[sample(nrow(final_data)),]
train <- final_data_1[1:as.integer(0.70*nrow(final_data)),]
library(caret)
train <- downSample(train,train$Result)
test <- final_data_1[-c(1:as.integer(0.70*nrow(final_data))),]

# Building a model using rpart on training data
library(rpart)
formula_nn <- paste("Result",paste(colnames(final_data[,-c(18,21)]),collapse ="+"),sep="~")
formula_nn <- paste("Result",paste(colnames(final_data[,-18]),collapse ="+"),sep="~")
rpart_train <- rpart(formula_nn,data=train) # decision tree model building

pred_train <- predict(rpart_train,train)
pred_train <- data.frame(pred_train)
library(dplyr)
pred_train <- if_else(pred_train$Fraudulent > 0.5,"Fraudulent","Genuine")
mean(pred_train==train$Result) # Training Accuracy = 75.03%  50.77%(up) 50.83%(down)

pred_train <- as.factor(pred_train)
library(caret)
confusionMatrix(pred_train,train$Result) # Training Accuracy = 75.03%  50.77%(up) 50.83%(down) 


pred_rpart_test <- predict(rpart_train,newdata=test) # predicting on test data
pred_rpart_test <- data.frame(pred_rpart_test)
pred_rpart_test <- if_else(pred_rpart_test$Fraudulent > 0.5,"Fraudulent","Genuine")
mean(pred_rpart_test==test$Result)  # Testing Accuracy = 74.91%  63.73%(up) 37.62%(down)

pred_rpart_test <- as.factor(pred_rpart_test)
confusionMatrix(pred_rpart_test,test$Result)  # Testing Accuracy = 74.91%  63.73%(up) 37.62%(down)

