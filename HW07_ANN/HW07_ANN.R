################################################
#  Purpose    : Assignment Neuralnet
#  First Name : Kajal
#  Last Name  : Khunt
#  Id			    : 10429885
#################################################

rm(list = ls())

bc<-read.csv("D:\\MS\\Spring 2018\\CS-513\\Assignment\\HW03_knn\\breast-cancer-wisconsin.data.csv", na.strings = "?")

### remove all the records with missing value
dsn<-na.omit(dsn)


benign<-ifelse(bc$Class==2,1,0)
malignant<-ifelse(bc$Class==4,1,0)

bc2<- na.omit(data.frame(bc,benign,malignant))

index <- seq (1,nrow(bc2),by=5)
test<-bc2[index,]
training<-bc2[-index,]


library("neuralnet")
set.seed(321)

net_bc2  <- neuralnet(benign+malignant~F1+F2+F3+F4+F5+F6+F7+F8+F9
                      ,training, hidden=10,  err.fct = "ce",linear.output = FALSE,likelihood = TRUE)

#Plot the neural network
plot(net_bc2)