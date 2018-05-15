rm(list=ls())
H1B<-read.csv("D:\\MS\\Spring 2018\\CS-513\\Project\\1. Master H1B Dataset1_24apr.csv", na.strings = c(""))
View(H1B)

# Checking for missing values
x<- is.na(H1B)

# column with missing values

Colnames<-names(which(sapply(H1B, anyNA)))

# summary Stats

summary(H1B)

str(H1B)

# Replacing the missing values with mode , mean , median and omitting values 
H1B<-H1B[,-c(13)]
View(H1B)
# Omit
H1B1<- na.omit(H1B)

levels(H1B1$CASE_STATUS)<-list(CERTIFIED=c("CERTIFIEDWITHDRAWN","CERTIFIED"), DENIED=c("WITHDRAWN","DENIED"))
View(H1B1)

#Segregating Year wise and taking 1 lakh rows in each case
H1B_2016<-H1B1[H1B1$CASE_SUBMITTED_YEAR==2016,]
H1B_2017<-H1B1[H1B1$CASE_SUBMITTED_YEAR==2017,]
H1B_2017<-H1B_2017[1:100161,]


library(class) 
library(e1071)

NB_2016 <- naiveBayes(CASE_STATUS~.,data=H1B_2016)
category_2016<-predict(NB_2016,H1B_2016  )
category_2016
#Calculating Error rate
table(nb_all=category_2016,Class=H1B_2016$CASE_STATUS)
NB_wrong_16<-sum(category_2016!=H1B_2016$CASE_STATUS)
NB_error_rate_16<-NB_wrong_16/length(category_2016)
NB_error_rate_16
accuracy_16 <- (1-NB_error_rate_16)*100
accuracy_16


NB_2017 <- naiveBayes(CASE_STATUS~.,data=H1B_2017)
category_2017<-predict(NB_2017,H1B_2017  )
#category_all<-predict(nb_all,bc[,-c(1)])
category_2017
#Calculating Error rate
table(nb_all=category_2017,Class=H1B_2017$CASE_STATUS)
NB_wrong_17<-sum(category_2017!=H1B_2017$CASE_STATUS)
NB_error_rate_17<-NB_wrong_17/length(category_2017)
NB_error_rate_17
accuracy_17 <- (1-NB_error_rate_17)*100
accuracy_17

