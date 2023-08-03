library(cluster)
state.x77
dim(state.x77)
image(state.x77)

data2_matrix = state.x77
data2[,1:8]
data2 = scale(data2_matrix)
str(data2)
class(data2)
#converting to data frame
df2= as.data.frame(data2_matrix)
head(df2)

#different distance methods such as euclidean, standardised euclidean or manhattan distance can be applied

distance <- dist(df2, method = 'euclidean')

#applying single linkage. Average can also be applied
#applying hierarchial clustering

h_s = hclust(distance ,method='single')
h_s
plot(h_s)

#applying kmeans clustering algorithm
kcluster = kmeans(df2, 4, iter.max = 20000, nstart = 5)
print(kcluster)

#visualizing the size of each clusters
#checking to see which state got the cluster

kcluster$size
kcluster$cluster

library(ggplot2)

pca_result <- prcomp(df2)
df_pca <- data.frame(pca_result$x[, 1:2], cluster = as.factor(kcluster$cluster))
ggplot(df_pca, aes(x = PC1, y = PC2, color = cluster)) +
  geom_point() +
  labs(title = "K-means Clustering Results", x = "Principal Component 1", y = "Principal Component 2")

# Silhouette plot
library(cluster)
sil_width <- silhouette(kcluster$cluster, dist(df2))
plot(sil_width, main = "Silhouette Plot for K-means Clustering")


# Elbow method to find the optimal number of clusters
wss <- numeric(10)
for (i in 1:10) {
  kmeans_model <- kmeans(df2, centers = i, iter.max = 20000, nstart = 5)
  wss[i] <- kmeans_model$tot.withinss
}
plot(1:10, wss, type = "b", xlab = "Number of Clusters", ylab = "Total Within Sum of Squares",
     main = "Elbow Method to Determine Optimal Cluster Number")

# Creating a heatmap for state.x77 data
heatmap(state.x77, scale = "column", Rowv = NA, Colv = NA, 
        col = colorRampPalette(brewer.pal(n = 7, name = "RdYlBu"))(100), 
        main = "Heatmap of state.x77 Data")

# Apply PCA and create biplot
pca_result <- prcomp(state.x77)
biplot(pca_result, main = "PCA Biplot for State.x77 Data")
