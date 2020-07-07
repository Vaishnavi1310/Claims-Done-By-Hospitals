library(readr)
Insurance_Dataset_ <- read_csv("D:/new/Insurance Dataset .csv")
#View(Insurance_Dataset_)
Insurance_Dataset_ <- Insurance_Dataset_[,-c(1:5,7,14,16,18,21,26:27,31:32)]
data_factor <- as.data.frame(lapply(Insurance_Dataset_[,-c(16,19)],factor))
str(data_factor)
normalize<-function(x){
  return ( (x-min(x))/(max(x)-min(x)))
}
data_norm<-as.data.frame(lapply(Insurance_Dataset_[,c(16,19)],FUN=normalize))
final_data <- data.frame(data_factor,data_norm)
library(Hmisc)
final_data["Days_spend_hsptl"] <- with(final_data,impute(final_data$Days_spend_hsptl,mode))
final_data["Description_illness"] <- with(final_data,impute(final_data$Description_illness,mode))
final_data["Mortality_risk"] <- with(final_data,impute(final_data$Mortality_risk,mode))
summary(final_data)
sum(is.na(final_data))

library(Boruta)
set.seed(7777)
boruta <- Boruta(Result~., data = final_data)
print(boruta)
plot(boruta, las = 2, cex.axis = 0.7)
plotImpHistory(boruta)
#getNonRejectedFormula(boruta)


# Tentative Fix
bor <- TentativeRoughFix(boruta)
print(bor)
attStats(boruta)
