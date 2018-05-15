#################################################
#  Purpose    : Assignment 2 EDA
#  First Name : Kajal
#  Last Name  : Khunt
#  Id			    : 10429885
#################################################


rm(list = ls())

## 1-Load the "breast-cancer-wisconsin.data.csv" from canvas into R and perform the EDA analysis by:
dsn <- read.csv("D://MS/Spring 2018/CS-513/Assignment/HW02_EDA/breast-cancer-wisconsin.data.csv")
View(dsn)
## read missing value
## dsn <- read.csv("D://MS/Spring 2018/CS-513/Assignment/HW02_EDA/breast-cancer-wisconsin.data.csv",na.strings="?")


##Summarizing each column (e.g. min, max, mean )
summary(dsn)
nrow(dsn)


## Identifying missing values
dsn$F6[dsn$F6=='?']<-NA
dsn$F6<-as.numeric(dsn$F6)
dsn$F6
sum(is.na(dsn))

## missing<-dsn[is.na(dsn$F6)] give missing value


## Replacing the missing values with the "mode" (most frequent value) of the column.
library(modeest)
mfv <- mlv(dsn$F6,method = "mfv",na.rm=TRUE)
mfv$M
dsn[is.na(dsn$F6),"F6"]<-mfv$M

## Displaying the frequency table of "Class" vs. F6
table(class=dsn$Class,F6=dsn$F6)
ftable(class=dsn$Class,F6=dsn$F6)
##freq = data.frame ( table ( dsn$Class, dsn$F6)[,])


## Displaying the scatter plot of F1 to F6, one pair at a time
pairs(dsn[2:3],main="Scatter Plot of F1 and F2",pch = 10)
pairs(dsn[3:4],main="Scatter Plot of F2 and F3",pch = 10)
pairs(dsn[4:5],main="Scatter Plot of F3 and F4",pch = 10)
pairs(dsn[5:6],main="Scatter Plot of F4 and F5",pch = 10)
pairs(dsn[6:7],main="Scatter Plot of F5 and F6",pch = 10)

##


## Show histogram box plot for columns F7 to F9
hist(dsn$F7)
hist(dsn$F8)
hist(dsn$F9)


## 2- Delete all the objects from your R- environment. Reload the "breast-cancer-wisconsin.data.csv" from canvas into R. Remove any row with a missing value in any of the columns.

rm(list = ls())

dsn <- read.csv("D://MS/Spring 2018/CS-513/Assignment/HW02_EDA/breast-cancer-wisconsin.data.csv")
View(dsn)

dsn[dsn=='?']<-NA

dsn_na<-na.omit(dsn)
nrow(dsn_na)