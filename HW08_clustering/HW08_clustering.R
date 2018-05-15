################################################
#  Purpose    : Assignment Clustering
#  First Name : Kajal
#  Last Name  : Khunt
#  Id			    : 10429885
#################################################

rm(list = ls())

dsn<-read.csv("D:\\MS\\Spring 2018\\CS-513\\Assignment\\HW08_clustering\\breast-cancer-wisconsin.data.csv", na.strings = "?")

### remove all the records with missing value
dsn<-na.omit(dsn)

#dsn<-dsn[2:10]

dsn_dist<-dist(dsn[,-11])
hclust_results<-hclust(dsn_dist)
hclust_2<-cutree(hclust_results,2)
table(hclust_2,dsn[,11])


#average method
hclust_average<-hclust(dist(dsn[,-11]),method = "average")
hclustAvg_2<-cutree(hclust_average,2)
table(hclustAvg_2,dsn[,11])
plot(hclust_average)


kmeans_3<-kmeans(dsn[,-11],2,nstart = 10)
kmeans_3$cluster
table(kmeans_3$cluster,dsn[,11])
