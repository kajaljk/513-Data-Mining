################################################
#  Purpose    : Assignment C50 5.2
#  First Name : Kajal
#  Last Name  : Khunt
#  Id			    : 10429885
#################################################

rm(list = ls())

## Load the dataset
dsn <- read.csv("D:\\MS\\Spring 2018\\CS-513\\Assignment\\Final Exam\\IBM_Employee_Attrition_V2.csv")

#install.packages("C50", repos="http://R-Forge.R-project.org")
#install.packages("C50")
library('C50')
#View(dsn)


index<-sort(sample(nrow(dsn),round(.25*nrow(dsn))))
training<-dsn[-index,]

# Use the C5.0 methodology to develop a classification model for the Diagnosis.  

C50_class <- C5.0(x = training[,-11],y = as.factor(training$Class))
summary(C50_class)

dev.off()
plot(C50_class)
