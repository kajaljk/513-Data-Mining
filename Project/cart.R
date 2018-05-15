rm(list=ls())
H1B<- read.csv("C:/Users/rajpu/Desktop/KDDM/Project/1. Master H1B Dataset1_24apr.csv",header = TRUE)
#View(H1B)

# Checking for missing values
x<- is.na(H1B)

# column with missing values

Colnames<-names(which(sapply(H1B, anyNA)))

# summary Stats

#summary(H1B)

#str(H1B)

# Replacing the missing values with mode , mean , median and omitting values 
H1B<-H1B[,-c(13)]
#View(H1B)
# Omit
H1B1<- na.omit(H1B)

levels(H1B1$CASE_STATUS)<-list(CERTIFIED=c("CERTIFIEDWITHDRAWN","CERTIFIED"), DENIED=c("WITHDRAWN","DENIED"))
#View(H1B1)
H1B1$WAGE_YEARLY <- as.numeric(H1B1$WAGE_YEARLY)


View(H1B1)


#Segregating Year wise and taking 1 lakh rows in each case
H1B_2016<-H1B1[H1B1$CASE_SUBMITTED_YEAR==2016,]
H1B_2017<-H1B1[H1B1$CASE_SUBMITTED_YEAR==2017,]
H1B_2017<-H1B_2017[1:100161,]

#Calculating Cart for 2016

#Creating test & train dataset for 2016
index<-sort(sample(nrow(H1B_2016),round(.30*nrow(H1B_2016))))
# train dataset
training<-H1B_2016[-index,]
# test dataset
test<-H1B_2016[index,]

#Install relevant packages

#install.packages("rpart")
#install.packages("rpart.plot")     
#install.packages("rattle")         
#install.packages("RColorBrewer")

#Load relevant packages
library(rpart)
library(rpart.plot)
library(rattle)
library(RColorBrewer)

#Classification Tree with rpart:

#Grow the tree for H1B status
CART_class_1<-rpart(CASE_STATUS~CASE_SUBMITTED_YEAR+VISA_CLASS+EMPLOYER_STATE+EMPLOYER_STATE+SOC_NAME+
                      TOTAL_WORKERS+FULL_TIME_POSITION+H.1B_DEPENDENT+WILLFUL_VIOLATOR+WORKSITE_STATE
                      ,data=training,method="class",control =rpart.control(minsplit=2000,minbucket=1, cp=0))
printcp(CART_class_1) # display the results
#plotcp(CART_class_1) # visualize cross-validation results
summary(CART_class_1) # detailed summary of splits

# plot tree
rpart.plot(CART_class_1)
prp(CART_class_1)
#fancyRpartPlot(CART_class_1,cex=1)

# calculating error rate
CART_predict_1<-predict(CART_class_1,test, type="class")
CART_wrong_1<-sum(test[,12]!=CART_predict_1)
CART_error_rate_1<-CART_wrong_1/length(test[,12])
CART_error_rate_1
accuracy<-(1-CART_error_rate_1)*100
#Calculating Cart for 2017

#Creating test & train dataset for 2016
index<-sort(sample(nrow(H1B_2017),round(.30*nrow(H1B_2017))))
# train dataset
training<-H1B_2017[-index,]
# test dataset
test<-H1B_2017[index,]


#Classification Tree with rpart:

#Grow the tree for H1B status
CART_class_1<-rpart(CASE_STATUS~CASE_SUBMITTED_YEAR+VISA_CLASS+EMPLOYER_STATE+EMPLOYER_STATE+SOC_NAME+
                      TOTAL_WORKERS+FULL_TIME_POSITION+H.1B_DEPENDENT+WILLFUL_VIOLATOR+WORKSITE_STATE
                    ,data=training,method="class",control =rpart.control(minsplit =200,minbucket=1, cp=0))
printcp(CART_class_1) # display the results
#plotcp(CART_class_1) # visualize cross-validation results
summary(CART_class_1) # detailed summary of splits

# plot tree
rpart.plot(CART_class_1)
prp(CART_class_1)
#fancyRpartPlot(CART_class_1,cex=1)

# calculating error rate
CART_predict_1<-predict(CART_class_1,test, type="class")
CART_wrong_1<-sum(test[,12]!=CART_predict_1)
CART_error_rate_1<-CART_wrong_1/length(test[,12])
CART_error_rate_1
accuracy<-(1-CART_error_rate_1)*100
accuracy 
 
