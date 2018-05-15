#  First Name      : Khasha
#  Last Name       : Dehnad
#  Id              : 12345
#  purpose         : Cluster analysis for Breast cancer


remove(list=ls())

bc<-
 read.csv("D:\\MS\\Spring 2018\\CS-513\\Assignment\\HW03_knn\\breast-cancer-wisconsin.data.csv"
,           na.strings = "?")
bc2<-na.omit(bc)


bc2_dist<-dist( bc2[,-c(1,11)])
hclust_resutls<-hclust(bc2_dist)
hclust_2<-cutree(hclust_resutls,2)
table(hclust_2,bc2[,11])

?kmeans

kmeans_2<- kmeans(bc2[,-c(1,11)],2,nstart = 10)
kmeans_2$cluster
table(kmeans_2$cluster,bc2[,11])



