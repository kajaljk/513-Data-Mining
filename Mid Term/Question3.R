

rm(list = ls())

## 1-Load the "IBM_Attrition_v3.csv"  
iav <- read.csv("D://MS/Spring 2018/CS-513/Assignment/Mid Term/IBM_Attrition_v3.csv")

iav_clean<-na.omit(iav)

## Use knn(k=3) to  predict "attrition rate" for a random sample(30%) of the data (test dataset)
index<-sort(sample(nrow(iav_clean),round(.30*nrow(iav_clean))))
Test_data<-iav_clean[index,]
Training_data<-iav_clean[-index,]
 
library(class)
knn_result<-knn(Training_data[,-c(3,6)],Test_data[,-c(3,6)] ,Training_data[,6],k=3)
table(Prediction = knn_result,Actual=Test_data[,6])

wrong<- (Test_data[,6]!=knn_result)
rate<-sum(wrong)/length(wrong)
rate

