################################################
#  Purpose    : Assignment 6 NB RF
#  First Name : Kajal
#  Last Name  : Khunt
#  Id			    : 10429885
#################################################

rm(list = ls())


## Load the dataset
dataFile <- read.csv("D:\\MS\\Spring 2018\\CS-513\\Assignment\\HW06_NB_RF\\breast-cancer-wisconsin.data.csv")
dsn2<-na.omit(dataFile)

index<-sort(sample(nrow(dsn2),round(.25*nrow(dsn2))))
training<-dataFile[-index,]

##7.1 Use the Naïve Bayes methodology to develop a classification model for the Diagnosis.
#install.packages('e1071', dependencies = TRUE)

library(class) 
library(e1071)

features <- dataFile[-11][-1]
labels <- dataFile[,11]
nBayes_class <- naiveBayes(x = features,y = as.factor(labels))
category_class<-predict(nBayes_class,features  )
plot(category_class)


##7.2 Use the Random Forest methodology to develop a classification model for the Diagnosis. What are the top three important features?
library(randomForest)

#fit <- randomForest( Class~., data=dataFile, importance=TRUE)
#importance(fit)
#print(fit)
#varImpPlot(fit)

rf <- randomForest(features, as.factor(labels))
print(rf)
importance(rf)

plot(rf)
varImpPlot(rf)

#install.packages('caret', dependencies = TRUE)

library(caret)

#Print All impo
imp <- varImp(rf)
varImpPlot(rf)

## Print All Feature Importance
imp[order(imp$Overall, decreasing = TRUE), ,drop = FALSE]
## Print Major 3 Feature Importance
head(imp[order(imp$Overall, decreasing = TRUE), ,drop = FALSE],n = 3)
