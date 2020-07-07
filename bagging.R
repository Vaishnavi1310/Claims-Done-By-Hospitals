library(readr)
Insurance_Dataset_ <- read_csv("D:/new/Insurance Dataset .csv")
View(Insurance_Dataset_)
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
final_data <- final_data[1:1000,]

final_data_1<- final_data[sample(nrow(final_data)),]
train <- final_data_1[1:as.integer(0.75*nrow(final_data)),]
test <- final_data_1[-c(1:as.integer(0.75*nrow(final_data))),]





library(mlbench)
library(caret)
library(caretEnsemble)
control <- trainControl(method = "repeatedcv", number = 10,repeats = 3)
metric <- "Accuracy"
# Bagged CART
set.seed(7)
fit.treebag <- train(Result~.,data = train, method="treebag", metric=metric, trControl=control)
# Random Forest
set.seed(7)
fit.rf <- train(Result~.,data = train, method="rf", metric=metric, trControl=control)
# summarize results
bagging_results <- resamples(list(treebag=fit.treebag,rf=fit.rf))
summary(bagging_results)
dotplot(bagging_results)


control <- trainControl(method = "repeatedcv", number = 10,repeats = 3)
metric <- "Accuracy"
# Bagged CART
set.seed(7)
fit.treebag <- train(Result~.,data = test, method="treebag", metric=metric, trControl=control)
# Random Forest
set.seed(7)
fit.rf <- train(Result~.,data = test, method="rf", metric=metric, trControl=control)
# summarize results
bagging_results <- resamples(list(treebag=fit.treebag,rf=fit.rf))
summary(bagging_results)
dotplot(bagging_results)
