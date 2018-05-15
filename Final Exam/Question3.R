#################################################
#  Purpose    : Final Exam Question 3
#  First Name : Kajal
#  Last Name  : Khunt
#  Id			    : 10429885
#################################################

rm(list = ls())

## Load the dataset
IBM <- read.csv("D:\\MS\\Spring 2018\\CS-513\\Assignment\\Final Exam\\IBM_Employee_Attrition_V2.csv")
IBM<- na.omit(dsn)

library(class) 
library(e1071)

## 1 Naive Bayes
IBM_NB<-naiveBayes(Attrition~Age+BusinessTravel+DistanceFromHome+Education+EnvironmentSatisfaction
                   +Gender+MaritalStatus+MonthlyIncome+NumCompaniesWorked+OverTime+TotalWorkingYears,IBM)
IBM_Predict<-predict(IBM_NB,IBM)

# calculating error rate
table(NBayes=IBM_Predict,Attrition=IBM$Attrition)
NB_wrong<-sum(IBM_Predict!=IBM$Attrition )
NB_error_rate<-NB_wrong/length(IBM_Predict)
NB_error_rate

set.seed(123)
index<-sort(sample(nrow(IBM),round(.30*nrow(IBM))))
training<-IBM[-index,]
test<-IBM[index,]

## 2 CART
library(rpart)

IBM_Cart<-rpart(Attrition~BusinessTravel+DistanceFromHome+Education+EnvironmentSatisfaction
                +Gender+MaritalStatus+MonthlyIncome+NumCompaniesWorked+OverTime,training,method="class")

# calculating error rate
CART_predict<-predict(IBM_Cart,test, type="class")
CART_wrong<-sum(test[,2]!=CART_predict)
CART_error_rate<-CART_wrong/length(test[,2])
CART_error_rate

## 3 Random Forest
library(randomForest)

fit <- randomForest( Attrition~., data=training, importance=TRUE, ntree=100, na.action = na.omit)
importance(fit)
varImpPlot(fit)

Prediction <- predict(fit, test)
table(actual=test[,2],Prediction)

wrong<- (test[,2]!=Prediction )
error_rate<-sum(wrong)/length(wrong)
error_rate


# 4. C50   
library('C50')
C50_class <- C5.0( Attrition~.,data=training )

summary(C50_class )
dev.off()
plot(C50_class)
C50_predict<-predict( C50_class ,test , type="class" )
table(actual=test[,2],C50=C50_predict)
wrong<- (test[,2]!=C50_predict)
c50_rate<-sum(wrong)/length(test[,2])
c50_rate



