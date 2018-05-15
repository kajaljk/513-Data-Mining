library(rpart)
library(rpart.plot)  			# Enhanced tree plots
library(rattle)           # Fancy tree plot
library(RColorBrewer) 
library(modeest)
library(stats)
library(caret)
library(utility)
library(DMwR)
library(randomForest)
library(unbalanced)
library("neuralnet")


rm(list = ls())

# Importing file 

H1B<-read.csv("D:\\MS\\Spring 2018\\CS-513\\Project\\1. Master H1B Dataset1_24apr.csv", na.strings = c(""))
NROW(H1B)
H1B<- na.omit(H1B)


# Smote using balanced library and Caret Library
# Smote using packagea
# train_balanced <- ubSMOTE(H1B, H1B$CASE_STATUS, perc.over = 200, perc.under = 200, k = 5)

#ctrl <- trainControl(method = "repeatedcv", 
#                     number = 10, 
#                     repeats = 10, 
#                     verboseIter = FALSE,
#                     sampling = "smote")

#H1B_2016<- na.omit(H1B_2016)

#set.seed(42)
#model_rf_smote <- caret::train(CASE_STATUS ~ .,
#                               data = H1B,
#                               method = "rf",
#                               preProcess = c("scale", "center"),
#                               trControl = ctrl)


# Removing Columns

H1B<-H1B[,-c(2,4)]
str(H1B_2016)


summary(H1B)


# Dividing state region wise

levels(H1B$FULL_TIME_POSITION)<- list("1"="Y","2"="N")

# Converting it to numeric


H1B$FULL_TIME_POSITION<-as.factor(H1B$FULL_TIME_POSITION)


# Dividing state region wise

levels(H1B$H.1B_DEPENDENT)<- list("1"="Y","0"="N")

# Converting it to numeric

H1B$H.1B_DEPENDENT<-as.factor(H1B$H.1B_DEPENDENT)

# Dividing state region wise

levels(H1B$WILLFUL_VIOLATOR)<- list("1"="Y","0"="N")

# Converting it to numeric

H1B$WILLFUL_VIOLATOR<-as.factor(H1B$WILLFUL_VIOLATOR)




# Dividing state region wise

levels(H1B$EMPLOYER_STATE) <- list("1"=c("CT", "ME","MA","NH","RI","VT"),"2"=c("NJ", "NY", "PA"),"3"=c("IL","IN","MI","OH","WI"),"4"=c("IA","KS","MN","MO","NE","ND","SD"),"5"=c("DE","FL","GA","MD","NC","SC","VA","DC"),"6"=c("AL","KY","MS","TN")
                                   ,"7"=c("AR","LA","OK","TX"),"8"=c("AZ","CO","ID","MT","NV","NM","UT","WY"),"9"=c("AK","CA","HI","OR","WA"))


# Converting it to numeric


H1B$EMPLOYER_STATE<- as.factor(H1B$EMPLOYER_STATE)


# Dividing Worksite regoin wise

levels(H1B$WORKSITE_STATE) <- list("1"=c("CT", "ME","MA","NH","RI","VT"),"2"=c("NJ", "NY", "PA"),"3"=c("IL","IN","MI","OH","WI"),"4"=c("IA","KS","MN","MO","NE","ND","SD"),"5"=c("DE","FL","GA","MD","NC","SC","VA","DC"),"6"=c("AL","KY","MS","TN")
                                   ,"7"=c("AR","LA","OK","TX"),"8"=c("AZ","CO","ID","MT","NV","NM","UT","WY"),"9"=c("AK","CA","HI","OR","WA"))


# Converting it to numeric


H1B$WORKSITE_STATE<- as.factor(H1B$WORKSITE_STATE)

# Dividing occupation into 8 sectors

levels(H1B$SOC_NAME) <- list("1"=c("ANALYSTS", "COMPUTER OCCUPATION", "IT MANAGERS", "GRAPHIC DESIGNERS" ,"DESIGNERS")
                             ,"2"=c("MARKETING","COMMUNICATIONS","CURATORS","MANAGERS","PUBLIC RELATIONS","MANAGEMENT","HUMAN RESOURCES","FIRST LINE SUPERVISORS")
                             ,"3"=c("AGRICULTURE","ANIMAL HUSBANDARY"),"4"=c("BUSINESS OPERATIONS SPECIALIST"),"5"=c("FINANCE","ACCOUNTANTS","INSURANCE"),"6"=c("MATHEMATICIANS AND STATISTICIANS","ACTUARIES","ECONOMISTS","WRITERS EDITORS AND AUTHORS","INTERPRETERS AND TRANSLATORS","LIBRARIANS","EDUCATION"),"7"=c("REPORTERS AND CORRESPONDENTS","ARCHITECTURE","SOCIAL WORKERS","CONSTRUCTION","EVENT PLANNERS","SURVEYORS","TRANSPORTATION","MECHANICS","HISTORIANS","COACHES AND SCOUTS","LAB TECHNICIANS","LAWYERS AND LEGAL SUPPORT WORKERS","LOGISTICIANS","COUNSELORS","MULTIMEDIA ARTISTS AND ANIMATORS","RELIGIOUS WORKERS","ENTERTAINMENT","FASHION DESIGNERS","REAL ESTATE"),"8"=c("FITNESS TRAINERS","DOCTORS","HEALTHCARE","INTERNIST","FOOD PREPARATION WORKERS"))



# Converting it to numeric

H1B$SOC_NAME<- as.factor(H1B$SOC_NAME)


# Dividing Case_status into 2 categories

levels(H1B$CASE_STATUS)<- list("1"=c("CERTIFIED","CERTIFIEDWITHDRAWN"), "0"=c("DENIED","WITHDRAWN"))

# Converting it to numeric

H1B$CASE_STATUS<- as.factor(H1B$CASE_STATUS)
sd

str(H1B)
H1B$WAGE_YEARLY <- (H1B$WAGE_YEARLY - mean(H1B$WAGE_YEARLY)) / sd(H1B$WAGE_YEARLY)  

norm <-function(x,minx,maxx) #Normalization
{
  z<-((x-minx)/(maxx-minx))
  return(z) 
} 


H1B$WAGE_YEARLY<- (H1B_2017$WAGE_YEARLY,min(H1B_2017$WAGE_YEARLY),max(H1B_2017$WAGE_YEARLY))



write.csv(train_balanced,"H1B_smote.csv")




H1B_2016<-H1B[H1B$CASE_SUBMITTED_YEAR==2016,]
NROW(H1B_2017)
H1B_2017<-H1B[H1B$CASE_SUBMITTED_YEAR==2017,]
H1B_2017<-H1B_2017[1:85945,]


set.seed(123)
index<-sort(sample(nrow(H1B_2016),round(.30*nrow(H1B_2016))))
training<-H1B_2016[-index,]
test<-H1B_2016[index,]


m_2016<-model.matrix(~CASE_STATUS+EMPLOYER_STATE+SOC_NAME+TOTAL_WORKERS+FULL_TIME_POSITION+H.1B_DEPENDENT+WILLFUL_VIOLATOR+WORKSITE_STATE+WAGE_YEARLY, data=training)




net_bc2_2016  <- neuralnet(CASE_STATUS~EMPLOYER_STATE+SOC_NAME+TOTAL_WORKERS+FULL_TIME_POSITION+H.1B_DEPENDENT+WILLFUL_VIOLATOR+WORKSITE_STATE+WAGE_YEARLY,m,hidden=2,t
                           hreshold=0.01)

library(class)
?knn()

# Classifying based on CASE_STATUS
Predict_h1b_2016 <- knn(training[,-10], test[,-10], training[,10], k=7)

# Classifying based on EmployerState
Predict_h1b_2016 <- knn(training[,-2], test[,-2], training[,2], k=7)

#######################################################
#Error value for kNN when classifying with CASE_STATUS
#######################################################
e_result <- cbind(Test_2016, as.character(Predict_h1b_2016)) 
err_rate_2016 <- sum(e_result[,10]!=e_result[,11])/length(e_result[,10]!=e_result[,11])
err_rate_2016

accuracy_2016=(1-err_rate_2016)*100;
accuracy_2016

fit <- randomForest( CASE_STATUS~., data=training, importance=TRUE, ntree=100, na.action = na.omit)
importance(fit)
varImpPlot(fit)

Prediction <- predict(fit, test)
table(actual=test[,9],Prediction)

wrong<- (test[,9]!=Prediction )
error_rate<-sum(wrong)/length(wrong)
error_rate

CART_class_1<-rpart(CASE_STATUS~CASE_SUBMITTED_YEAR+VISA_CLASS+EMPLOYER_STATE+EMPLOYER_STATE+SOC_NAME+
                      TOTAL_WORKERS+FULL_TIME_POSITION+H.1B_DEPENDENT+WILLFUL_VIOLATOR+WORKSITE_STATE,data=training,method="class",control =rpart.control(minsplit=2000,minbucket=1, cp=0))
printcp(CART_class_1) # display the results
#plotcp(CART_class_1) # visualize cross-validation results
summary(CART_class_1) # detailed summary of splits

# plot tree
rpart.plot(CART_class_1,main="2016")
prp(CART_class_1,main="2016")
#fancyRpartPlot(CART_class_1,cex=1)
#?prp
# calculating error rate
CART_predict_1<-predict(CART_class_1,test, type="class")

CART_wrong_1<-sum(test[,12]!=CART_predict_1)
CART_error_rate_1<-CART_wrong_1/length(test[,12])
CART_error_rate_1
accuracy<-(1-CART_error_rate_1)*100
accuracy


set.seed(123)
index<-sort(sample(nrow(H1B_2017),round(.30*nrow(H1B_2017))))
training<-H1B_2017[-index,]
test<-H1B_2017[index,]

str(H1B_2017)

m_2017<-model.matrix(~CASE_STATUS+EMPLOYER_STATE+SOC_NAME+TOTAL_WORKERS+FULL_TIME_POSITION+H.1B_DEPENDENT+WILLFUL_VIOLATOR+WORKSITE_STATE+WAGE_YEARLY, data=training)


net_bc2_2017<- neuralnet(CASE_STATUS1~ SOC_NAME7+TOTAL_WORKERS+FULL_TIME_POSITION+H.1B_DEPENDENT1+WORKSITE_STATE+WAGE_YEARLY,m,hidden=2,threshold=0.01)

fit <- randomForest( CASE_STATUS~., data=training, importance=TRUE, ntree=100, na.action = na.omit)
importance(fit)
varImpPlot(fit)

Prediction <- predict(fit, test)
table(actual=test[,9],Prediction)

wrong<- (test[,9]!=Prediction )
error_rate<-sum(wrong)/length(wrong)
error_rate


# Classifying based on CASE_STATUS
Predict_h1b_2017 <- knn(training[,-10], test[,-10], training[,10], k=7)

# Classifying based on EmployerState
Predict_h1b_2017_emp <- knn(training[,-2], test[,-2], training[,2], k=7)

#######################################################
#Error value for kNN when classifying with CASE_STATUS
#######################################################
e_result <- cbind(test, as.character(Predict_h1b_2017))
err_rate_2017 <- sum(e_result[,10]!=e_result[,11])/length(e_result[,10]!=e_result[,11])
err_rate_2017

accuracy_2017=(1-err_rate_2017)*100;
accuracy_2017
CART_class_1<-rpart(CASE_STATUS~CASE_SUBMITTED_YEAR+VISA_CLASS+EMPLOYER_STATE+EMPLOYER_STATE+SOC_NAME+
                      TOTAL_WORKERS+FULL_TIME_POSITION+H.1B_DEPENDENT+WILLFUL_VIOLATOR+WORKSITE_STATE
                    ,data=training,method="class",control =rpart.control(minsplit =2000,minbucket=1, cp=0))
printcp(CART_class_1) # display the results
#plotcp(CART_class_1) # visualize cross-validation results
summary(CART_class_1) # detailed summary of splits

# plot tree
#rpart.plot(CART_class_1)
prp(CART_class_1,main="2017")
#fancyRpartPlot(CART_class_1,cex=1)

# calculating error rate

CART_predict_1<-predict(CART_class_1,test, type="class")

CART_wrong_1<-sum(test[,12]!=CART_predict_1)
CART_error_rate_1<-CART_wrong_1/length(test[,12])
CART_error_rate_1
accuracy<-(1-CART_error_rate_1)*100
accuracy 