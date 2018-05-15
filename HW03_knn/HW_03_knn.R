################################################
#  Purpose    : Assignment 3 knn
#  First Name : Kajal
#  Last Name  : Khunt
#  Id			    : 10429885
#################################################


rm(list = ls())
library(class)


## 3.1 Using knn (k=2), EXCEL and the following training and test datasets, predict the species for the test datasets. 
## What is the error rate? DO NOT normalize the data. You can copy and paste the tables into excel.

dsn <- read.csv("D:\\MS\\Spring 2018\\CS-513\\Assignment\\HW03_knn\\training-test.csv")
#View(dsn)

test_idx<-1:3
Test_data<-dsn[test_idx,]
Training_data<-dsn[-test_idx,]

knn_result<-knn(Training_data[,-5],Test_data[,-5],Training_data[,5],k=2)
table(Prediction = knn_result,Actual=Test_data[,5])

Error_Rate <- mean(knn_result != Test_data[,5])
Error_Rate


## 3.2 Load the "breast-cancer-wisconsin.data.csv" from CANVAS (see the description bellow) 
rm(list = ls())

dsn <- read.csv("D:\\MS\\Spring 2018\\CS-513\\Assignment\\HW03_knn\\breast-cancer-wisconsin.data.csv")
#View(dsn)


## a.  Remove the rows with missing values
dsn[dsn=='?']<-NA

dsn_na<-na.omit(dsn)
nrow(dsn_na)


## b.  Store every fifth record in a "test" dataset starting with the first record
test_idx<-seq(1,nrow(dsn_na),by=5)
Test_data<-dsn_na[test_idx,]

## c.   Store the rest in the "training" dataset
Training_data<-dsn_na[-test_idx,]


## d.  Use knn with k=1 and classify the test dataset
knn_result<-knn(Training_data[,-11],Test_data[,-11],Training_data[,11],k=1)
table(Prediction = knn_result,Actual=Test_data[,11])


## e
wrong<-(test[,11]!=predict)
rate<-sum(wrong)/length(wrong)
rate

## f.   Repeat the above steps with k=2, k=5, k=10.

## k = 2
knn_result<-knn(Training_data[,-11],Test_data[,-11],Training_data[,11],k=2)
table(Prediction = knn_result,Actual=Test_data[,11])


##knn_result<-knn(Training_data[,c(-1,-11)],Test_data[,-c(-1,11)],Training_data[,11],k=2)


## k = 5
knn_result<-knn(Training_data[,-11],Test_data[,-11],Training_data[,11],k=5)
table(Prediction = knn_result,Actual=Test_data[,11])

## k = 10
knn_result<-knn(Training_data[,-11],Test_data[,-11],Training_data[,11],k=10)
table(Prediction = knn_result,Actual=Test_data[,11])
