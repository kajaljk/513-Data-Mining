
#  First Name      : Khasha
#  Last Name       : Dehnad
#  Id              : 12345
#  purpose         : Accessing extrenal data and replacing missing value  
#                  : accessing data and perofrming EDA

remove(list=ls())

## Step 1 load the data
## changing ? to NA
#3.2 Load the "breast-cancer-wisconsin.data.csv"
# from CANVAS

 bc<- read.csv("D:\\MS\\Spring 2018\\CS-513\\Assignment\\HW03_knn\\breast-cancer-wisconsin.data.csv",
           na.strings = "?")

#a.	Remove the rows with missing values
 bc_clean<-na.omit(bc)

#b.	Store every fifth record in a "test" dataset starting with the first record
  indx<-seq(1,nrow(bc_clean),5)
  test<-bc_clean[indx,]
#c.	Store the rest in the "training" dataset
  training<-bc_clean[-indx,]
#d.	Use knn with k=1 and classify the test dataset
  library(class)
  predict<-knn(training[,c(-1,-11)],test[,c(-1,-11)],training[,11],k=1)
  
#e.	Measure the performance of knn
  
  wrong<- (test[,11]!=predict)
  rate<-sum(wrong)/length(wrong)
  rate
  
#f.	Repeat the above steps with k=2, k=5, k=10.
for(i in c(1,2,5,10)){
  predict<-knn(training[,c(-1,-11)],test[,c(-1,-11)],training[,11],k=i)
  wrong<- (test[,11]!=predict)
  rate<-sum(wrong)/length(wrong)
  print(rate)
}

