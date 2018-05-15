#################################################
#  Purpose    : Assignment 9 SVM
#  First Name : Kajal
#  Last Name  : Khunt
#  Id			    : 10429885
#################################################


rm(list = ls())

data(iris)
library(e1071)


##  Store every fifth record in a "test" dataset
test_idx<-seq(1,nrow(iris),by=5)
Test_data<-iris[test_idx,]
Training_data<-iris[-test_idx,]

##  Predict the species for the test datasets using SVM. What is the error rate? 
svm.model <- svm(Species~ .,data=Training_data)
svm.pred<- predict(svm.model,Test_data)

table(actual=Test_data[,5],svm.pred)
SVM_Wrong<-(Test_data$Species!=svm.pred)
rate<-sum(SVM_Wrong)/length(SVM_Wrong)
rate