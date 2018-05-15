################## FINAL PROJECT ###################
# DONE BY:
#  AYUSH SHARMA
#  KAJAL KHUNT
#  SAURABH RAJPUT
###################################################

library(rpart)
library(rpart.plot)  			# Enhanced tree plots
library(rattle)           # Fancy tree plot
library(RColorBrewer) 
library(modeest)
library(stats)

rm(list = ls())

############################################
# Defining the normalization function
############################################
norm <-function(x,minx,maxx) #Normalization
{
  z<-((x-minx)/(maxx-minx))
  return(z) 
}

##############################
# Reading the DIABETIC Dataset 
##############################
H1B<-read.csv("D:\\MS\\Spring 2018\\CS-513\\Project\\1. Master H1B Dataset1_24apr.csv", na.strings = c(""))

H1B_2<-read.csv("D:\\MS\\Spring 2018\\CS-513\\Project\\1. Master H1B Dataset1_25.csv", na.strings = c(""))

#######################
# CREATING NEW DATASET:
#######################
# Dividing state region wise

levels(H1B$EMPLOYER_STATE) <- list(D1=c("CT", "ME","MA","NH","RI","VT"),
                                   D2=c("NJ", "NY", "PA"),
                                   D3=c("IL","IN","MI","OH","WI"),
                                   D4=c("IA","KS","MN","MO","NE","ND","SD"),
                                   D5=c("DE","FL","GA","MD","NC","SC","VA","DC"),
                                   D6=c("AL","KY","MS","TN"),
                                   D7=c("AR","LA","OK","TX"),
                                   D8=c("AZ","CO","ID","MT","NV","NM","UT","WY"),
                                   D9=c("AK","CA","HI","OR","WA"))


levels(H1B$WORKSITE_STATE) <- list(D1=c("CT", "ME","MA","NH","RI","VT"),
                                   D2=c("NJ", "NY", "PA"),
                                   D3=c("IL","IN","MI","OH","WI"),
                                   D4=c("IA","KS","MN","MO","NE","ND","SD"),
                                   D5=c("DE","FL","GA","MD","NC","SC","VA","DC"),
                                   D6=c("AL","KY","MS","TN"),
                                   D7=c("AR","LA","OK","TX"),
                                   D8=c("AZ","CO","ID","MT","NV","NM","UT","WY"),
                                   D9=c("AK","CA","HI","OR","WA"))


# Dividing occupation into 8 sectors

levels(H1B$SOC_NAME) <- list(IT=c("ANALYSTS", "COMPUTER OCCUPATION", "IT MANAGERS", "GRAPHIC DESIGNERS" ,"DESIGNERS"),
                             Management=c("MARKETING","COMMUNICATIONS","CURATORS","MANAGERS","PUBLIC RELATIONS","MANAGEMENT","HUMAN RESOURCES","FIRST LINE SUPERVISORS"),
                             Agriculture=c("AGRICULTURE","ANIMAL HUSBANDARY"),
                             Business=c("BUSINESS OPERATIONS SPECIALIST"),
                             Finance=c("FINANCE","ACCOUNTANTS","INSURANCE"),
                             Education=c("MATHEMATICIANS AND STATISTICIANS","ACTUARIES","ECONOMISTS","WRITERS EDITORS AND AUTHORS","INTERPRETERS AND TRANSLATORS","LIBRARIANS","EDUCATION"),
                             Others=c("REPORTERS AND CORRESPONDENTS","ARCHITECTURE","SOCIAL WORKERS","CONSTRUCTION","EVENT PLANNERS","SURVEYORS","TRANSPORTATION","MECHANICS","HISTORIANS","COACHES AND SCOUTS","LAB TECHNICIANS","LAWYERS AND LEGAL SUPPORT WORKERS","LOGISTICIANS","COUNSELORS","MULTIMEDIA ARTISTS AND ANIMATORS","RELIGIOUS WORKERS","ENTERTAINMENT","FASHION DESIGNERS","REAL ESTATE"),
                             Healthcare=c("FITNESS TRAINERS","DOCTORS","HEALTHCARE","INTERNIST","FOOD PREPARATION WORKERS"))

#Numerical values for Visa Class
levels(H1B$VISA_CLASS) <-  list("1"=c("H1B"),"2"=c("H1B1 Singapore"),"3"=c("E3 Australian"),"4"=c("H1B1 Singapore"))


# Dividing Case Status into binary
levels(H1B$CASE_STATUS)<- list(CERTIFIED=c("CERTIFIED","CERTIFIEDWITHDRAWN"), DENIED=c("DENIED","WITHDRAWN"))

###############  
# summary Stats
############### 
summary(H1B)
str(H1B)


############################################################################
# Replacing the missing values with mode , mean , median and omitting values 
############################################################################
# Checking for missing values
x<- is.na(H1B)
x

H1B<- na.omit(H1B)
nrow(H1B)

##########################################################
#Segregating Year wise and taking 1 lakh rows in each case
##########################################################
H1B_2016<-H1B[H1B$CASE_SUBMITTED_YEAR==2016,]
H1B_2017<-H1B[H1B$CASE_SUBMITTED_YEAR==2017,]
H1B_2017<-H1B_2017[1:85945,]

############################ 
# CREATING NEW DATASET 2016
############################
Employer_State<-as.numeric(H1B_2016$EMPLOYER_STATE)
Employer_Country<-as.numeric(H1B_2016$EMPLOYER_COUNTRY)
Soc_Name<-as.numeric(H1B_2016$SOC_NAME)
Full_Time_Position<-as.numeric(H1B_2016$FULL_TIME_POSITION)
H1B_Dependent<-as.numeric(H1B_2016$H.1B_DEPENDENT)
Worksite_State<-as.numeric(H1B_2016$WORKSITE_STATE)
Case_Status<-as.numeric(H1B_2016$CASE_STATUS)

H1B_2016_NewDataSet<-as.data.frame(cbind(VisaClass = H1B_2016$VISA_CLASS,
                           EmployerState=  Employer_State, 
                           EmployerCountry= Employer_Country,  
                           SocName= Soc_Name, 
                           TotalWorkers= H1B_2016$TOTAL_WORKERS, 
                           FullTimePosition=Full_Time_Position,
                           WageRateOfPayFrom=norm(H1B_2016$WAGE_YEARLY,min(H1B_2016$WAGE_YEARLY),max(H1B_2016$WAGE_YEARLY)),   
                           H1BDependent=H1B_Dependent,
                           WorksiteState=Worksite_State,
                           CaseStatus=Case_Status))

################################################################################
# Storing 70% of the new datsets as TRAINING DATA and the rest as TEST DATA  ###
################################################################################
temp <- sample(nrow(H1B_2016_NewDataSet),as.integer(0.70 * nrow(H1B_2016_NewDataSet)))
Training_2016 <- H1B_2016_NewDataSet[temp,]
Test_2016 <- H1B_2016_NewDataSet[-temp,]


#####################
#  Performing KNN   #
#####################
library(class)
?knn()

# Classifying based on CASE_STATUS
Predict_h1b_2016 <- knn(Training_2016[,-10], Test_2016[,-10], Training_2016[,10], k=7)

# Classifying based on EmployerState
Predict_h1b_region_2016 <- knn(Training_2016[,-2], Test_2016[,-2], Training_2016[,2], k=10)

#######################################################
#Error value for kNN when classifying with CASE_STATUS
#######################################################
e_result <- cbind(Test_2016, as.character(Predict_h1b_2016)) 
err_rate_2016 <- sum(e_result[,10]!=e_result[,11])/length(e_result[,10]!=e_result[,11])
table(Prediction = Predict_h1b_2016,Actual=Test_2016[,10]) # Frequency Table Case Status vs  Table Case Status

err_rate_2016

accuracy_2016=(1-err_rate_2016)*100;
accuracy_2016

############################ 
# CREATING NEW DATASET 2017
############################
Employer_State<-as.numeric(H1B_2017$EMPLOYER_STATE)
Employer_Country<-as.numeric(H1B_2017$EMPLOYER_COUNTRY)
Soc_Name<-as.numeric(H1B_2017$SOC_NAME)
Full_Time_Position<-as.numeric(H1B_2017$FULL_TIME_POSITION)
H1B_Dependent<-as.numeric(H1B_2017$H.1B_DEPENDENT)
Worksite_State<-as.numeric(H1B_2017$WORKSITE_STATE)
Case_Status<-as.numeric(H1B_2017$CASE_STATUS)

H1B_2017_NewDataSet<-as.data.frame(cbind(VisaClass = H1B_2017$VISA_CLASS,
                           EmployerState=  Employer_State, 
                           EmployerCountry= Employer_Country,  
                           SocName= Soc_Name, 
                           TotalWorkers= H1B_2017$TOTAL_WORKERS, 
                           FullTimePosition=Full_Time_Position,
                           WageRateOfPayFrom=norm(H1B_2017$WAGE_YEARLY,min(H1B_2017$WAGE_YEARLY),max(H1B_2017$WAGE_YEARLY)),   
                           H1BDependent=H1B_Dependent,
                           WorksiteState=Worksite_State,
                           CaseStatus=Case_Status))

################################################################################
# Storing 70% of the new datsets as TRAINING DATA and the rest as TEST DATA  ###
################################################################################
temp <- sample(nrow(H1B_2017_NewDataSet),as.integer(0.70 * nrow(H1B_2017_NewDataSet)))
Training_2017 <- H1B_2017_NewDataSet[temp,]
Test_2017 <- H1B_2017_NewDataSet[-temp,]


#####################
#  Performing KNN   #
#####################
library(class)
?knn()

# Classifying based on CASE_STATUS
Predict_h1b_2017 <- knn(Training_2017[,-10], Test_2017[,-10], Training_2017[,10], k=7 )

# Classifying based on EmployerState
Predict_h1b_region_2017 <- knn(Training_2017[,-2], Test_2017[,-2], Training_2017[,2], k=10)

#######################################################
#Error value for kNN when classifying with CASE_STATUS
#######################################################
table(Prediction = Predict_h1b_2017,Actual=Test_2017[,10]) # Frequency Table Case Status vs  Table Case Status

e_result <- cbind(Test_2017, as.character(Predict_h1b_2017))
err_rate_2017 <- sum(e_result[,10]!=e_result[,11])/length(e_result[,10]!=e_result[,11])
err_rate_2017

accuracy_2017=(1-err_rate_2017)*100;
accuracy_2017

#####################
#  Frequency tables #
#####################
table(Prediction = Predict_h1b_2016,Actual=Test_2016[,10]) # Frequency Table Case Status vs  Table Case Status
table(Prediction = Predict_h1b_2017,Actual=Test_2017[,10]) # Frequency Table Case Status vs  Table Case Status

table(Predict_h1b_2016,Test_2016[,2])  # Frequency Table Case Status vs  Employer State
table(Predict_h1b_2016,Test_2016[,4])  # Frequency Table Case Status vs  Department Name
table(Predict_h1b_2016,Test_2016[,8])  # Frequency Table Case Status vs  H1B dependentt

table(Predict_h1b_2017,Test_2017[,2])  # Frequency Table Case Status vs  Employer State
table(Predict_h1b_2017,Test_2017[,4])  # Frequency Table Case Status vs  Department Name
table(Predict_h1b_2017,Test_2017[,8])  # Frequency Table Case Status vs  H1B dependentt

library(ggplot2)
qplot(SOC_NAME, WAGE_YEARLY, data=H1B_2016 ,colour=SOC_NAME,ylim = c(10000,800000))
qplot(CASE_STATUS, WAGE_YEARLY, data=H1B_2016 ,colour=CASE_STATUS,ylim = c(10000,800000))

##########################
#  Clustering  & Kmeans  #
########################## 

memory.limit(size=90000)

h1b_dist_16<-dist(Test_2016[,-10])
hclust_results_16<-hclust(h1b_dist_16)
hclust_1<-cutree(hclust_results_16,2)
table(hclust_1,Test_2016[,10])
error_rate_16<-mean(hclust_1 != Test_2016[,10])
error_rate_16
accuracy_rate_16<-(1-error_rate_16)*100
accuracy_rate_16

h1b_dist_17<-dist(Test_2017[,-10])
hclust_results_17<-hclust(h1b_dist_17)
hclust_2<-cutree(hclust_results_17,2)
table(hclust_2,Test_2017[,10])
error_rate_17<-mean(hclust_2 != Test_2017[,10])
error_rate_17
accuracy_rate_17<-(1-error_rate_17)*100
accuracy_rate_17

kmeans_1<- kmeans(Test_2016[,-10],2,nstart = 10) 
table(kmeans_1$cluster,Test_2016[,10])

kmeans_2<- kmeans(Test_2017[,-10],2,nstart = 10) 
table(kmeans_2$cluster,Test_2017[,10])


