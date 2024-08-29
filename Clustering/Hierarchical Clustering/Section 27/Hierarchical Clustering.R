#Hierarchical Clustering
dataset = Mall_Customers
dataset
X<-dataset[4:5]

#Using the dendrogram to find the optinmal number of clusters
#The method ward.D minimices the variance
dendrogram = hclust(dist(X, method = 'euclidean'), method = 'ward.D' )
plot(dendrogram, main = paste('Dendrogram'), xlab = 'Customers', 
     ylab = 'Euclidean Distances')

#Fitting hierarchical clustering to the mall dataset
hc = hclust(dist(X, method = 'euclidean'), method = 'ward.D' )
y_hc = cutree(hc, 5)

#Visualising the clusters
library(cluster)
clusplot(X, y_hc, lines = 0, shade = T, color = T, labels = 2,
         plotchar = F, span = T, main = paste('Clusters of Clients'),
         xlab = "Anual Income", ylab = "Spending Score")