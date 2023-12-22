# Hierarchical Clustering

# Importing the dataset
dataset = read.csv('mall.csv')
X = dataset[4:5]

# Using the dendrogram to find optimal # of clusters
dendrogram = hclust(dist(X, method = 'euclidean'), method = 'ward.D')
plot(dendrogram,
     main = paste('Dendrogram'),
     xlab = 'Customers',
     ylab = 'Euclidean Distances')
#use the dendrogram by finding the largest vertical distance that doesn't cross 
#a horizontal line, then counting the number of vertical lines at that distance
#From this chart, the largest v. distance is the leftmost vertical
#at which we have 5 vertical lines, so we need 5 clusters

# Fitting th Hierarchical clustering algo to the dataset
hc = hclust(dist(X, method = 'euclidean'), method = 'ward.D')
y_hc = cutree(hc, 5)
#y_hc is our vector of clusters, type it into console

# Visualising our clusters
library(cluster)
clusplot(X,
         y_hc,
         lines = 0,
         shade = TRUE,
         color = TRUE,
         labels = 2,
         plotcolor = FALSE,
         span = TRUE,
         main = paste('Clusters of Clients'),
         xlab = "Annual Income",
         ylab = "Spending Score")


# [ALTERNATE VERSION] Visualising the clusters
library(cluster)
clusplot(x = X,
         clus = y_hc,
         lines = 0,
         shade = TRUE,
         color = TRUE,
         labels= 2,
         plotchar = FALSE,
         span = TRUE,
         main = paste('Clusters of customers'),
         xlab = 'Annual Income',
         ylab = 'Spending Score')