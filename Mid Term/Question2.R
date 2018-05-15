#################################################
#  Purpose    : Midterm Question 2 EDA
#  First Name : Kajal
#  Last Name  : Khunt
#  Id			    : 10429885
#################################################


rm(list = ls())

## 1-Load the "IBM_Attrition_v3.csv" to perform the EDA analysis by:
iav <- read.csv("D://MS/Spring 2018/CS-513/Assignment/Mid Term/IBM_Attrition_v3.csv")
#View(iav)

## I. Summarizing each column (e.g. min, max, mean )
summary(iav)

## II.	Identifying missing values
is.na(iav)
missing<-iav[is.na(iav$MonthlyIncome),]

## III.	Displaying the frequency table of "Attrition" vs. "MaritalStatus" 
table(Attrition=iav$Attrition,MaritalStatus=iav$MaritalStatus)

## IV.	Displaying the scatter plot of "Age", "MaritalStatus" and "YearsAtCompany", one pair at a time
pairs(cbind(iav$Age,iav$MaritalStatus,iav$YearsAtCompany), main = "IBM Attrition Graph",pch = 21, bg = c("red", "green"),
      labels = c("Age","MaritalStatus","YearsAtCompany"),na.action=na.omit)

## V.	Show histogram box plot for columns:  "Age", "MaritalStatus" and "YearsAtCompany"
iav$MaritalStatus <- as.numeric(iav$MaritalStatus)
hist(iav$Age,main = "IBM Attrition Column = Age")
hist(iav$MaritalStatus,main = "IBM Attrition Column = Marital Status") # can not show histogram box plot for non-numeric column
hist(iav$YearsAtCompany,main = "IBM Attrition Column = Years At Company")

## VI.	Replacing the missing values of "MonthlyIncome" with the "mean" of "MonthlyIncome"
library(modeest)
mfm <- mlv(iav$MonthlyIncome,method = "mfv",na.rm=TRUE)
mfm$M
iav[is.na(iav$MonthlyIncome),"MonthlyIncome"]<-mfm$M