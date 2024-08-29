#K-Means Clustering
setwd("~/Downloads/Machine Learning Course/Clustering/K-Means/Section 26")
dataset = read.csv('Mall_Customers.csv')
X<-dataset[4:5]

#Using the elbow method to find the optimal number of clusters
set.seed(6)
wcss<-vector()
for (i in 1:10) wcss[i] <-sum(kmeans(x = X, centers = i)$withinss)
plot(x = 1:10, y = wcss, type = "b", main = paste('Clusters of Clients'),
     xlab = "Number of clusters", ylab = "WCSS")

#Aplying k-means to the mall dataset
set.seed(29)
kmeans <- kmeans(x = X, centers = 5, iter.max = 300, nstart = 10)

#Visualising the clusters
library(cluster)
clusplot(x = X, kmeans$cluster, lines = 0, shade = T, color = T, labels = 2,
         plotchar = F, span = T, main = paste('Clasificación de Clientes'),
         xlab = "Ingresos Anuales", ylab = "Score de Gastos")