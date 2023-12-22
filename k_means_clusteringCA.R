# K-Means Clustering

# Importing the dataset
dataset = read.csv('mall.csv')
#We will clusted these mall customers based on annual income and spending score ONLY
X = dataset[4:5] 










# Using the elbow method to find the optimal number of clusters
# Because the user must set the number of clusters to use K-means
#we set the seed b/c elbow method begins by selecting 1 point at random
#wcss = elbow method
#we plot the elbow method chart and choose optimal # of clusters visually
#by picking the number of clusters where the slope becomes less severe
set.seed(50000)
wcss = vector()
for(i in 1:10) wcss[i] = sum(kmeans(X, i)$withins)
plot(1:10, wcss, type = "b", main = paste('Clusters of Clients'), xlab = 'Number of Clusters', ylab = 'WCSS')

#we just computed the wcss (within-clusters sum of squares) for different numbers of clusters (1-10) 
#and put these in a vector called wcss, using a for loop to populate our empty vector()
#(i in 1:10) means i will take the values from 1 to 10
#the wcss is computed for each i number of clusters with kmeans()
#kmeans(x, i) fits the kmeans to our dataset X using i number of clusters
#using kmeans returns an object of class "kmeans" which has a component called "withinss"
#"withinss" is a vector of within-cluster sum of squares
#we sum the $withinss for i=1 through i=10 to get the true wcss
#for the plot, x values are # of clusters (1-10) and y is the calculated wcss
#type = "b" specifies a type of plot with both lines and points
#NOTE: changing the seed# changes the plot, as initial centroid is randomly selected
#From this plot we will select 5 as our optimal # of clusters, it is where the slope becomes less steep


# Fitting K-Means to the dataset
set.seed(29)
kmeans = kmeans(X, 5, iter.max = 300, nstart = 10)

#nstart is the number of initial random sets



# Visualising the clusters
library(cluster)
clusplot(X,
         kmeans$cluster,
         lines = 0,
         shade = TRUE,
         color = TRUE,
         labels = 2,
         plotcolor = FALSE,
         span = TRUE,
         main = paste('Clusters of Clients'),
         xlab = "Annual Income",
         ylab = "Spending Score")
#kmeans$cluster is the vector of clusters to which each observation belongs
#lines = 0 specifies no distance lines will appear in the plot
#shade = TRUE will shade the clusters with repect to their density

#Note this visualization code will only work for 2 dimensions