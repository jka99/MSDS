set.seed(30)
clust = sample(1:2,6, replace=T)
Obs = c("A", "B", "C", "D", "E", "F") 
X1 = c(2, 1, 0, 5, 6, 4) 
X2 = c(4, 5, 4, 2, 2, 1) 
Observations = data.frame(Obs, X1, X2) 
Observations$Cluster <- c(2, 1, 2, 2, 1, 2)
cluster1 <- Observations[Observations$Cluster == 1,]
x11 <- mean(cluster1$X1)
x12 <- mean(cluster1$X2)
cluster2 <- Observations[Observations$Cluster == 2,]
x21 <- mean(cluster2$X1)
x22 <- mean(cluster2$X2)

distances <- cbind(
  cluster1 = sqrt((Observations$X1 - x11)^2 + (Observations$X2 - x12)^2),
  cluster2 = sqrt((Observations$X1 - x21)^2 + (Observations$X2 - x22)^2)
)
Observations$Cluster <- apply(distances, 1, which.min)
cluster1 <- Observations[Observations$Cluster == 1,]
x11 <- mean(cluster1$X1)
x12 <- mean(cluster1$X2)

cluster2 <- Observations[Observations$Cluster == 2,]
x21 <- mean(cluster2$X1)
x22 <- mean(cluster2$X2)



x1c = c(-1,-2,-3,2,3,1)
x2c = c(1,2,1,-1,-1,-2)
x3c = c(-2,5,4,-1,-3,-3)
x = matrix(c(x1c,x2c,x3c),ncol=3)
pc.info = prcomp(x)
round(pc.info$rotation[,1],4)
pc.info$sdev
vjs = pc.info$sdev^2
pve = vjs/sum(vjs)
round(pve,3)
pc1scores = round(pc.info$x[,1],4); pc1scores  # first principal component score vector
pc2scores = round(pc.info$x[,2],4); pc2scores  # second principal component score vector


library(MASS)
x = UScereal[1:7,c(2:10)]
x.scale = scale(x)
hc <- hclust(dist(x.scale), method = "complete")

# Cut the tree to obtain two clusters
clusters <- cutree(hc, k = 2)

# Get the observations in each cluster
cluster1 <- rownames(x.scale)[clusters == 1]
cluster2 <- rownames(x.scale)[clusters == 2]

set.seed(12)

# Perform K-means clustering with k = 2
km <- kmeans(x.scale, centers = 2)

# Get the observations in each cluster
cluster1 <- rownames(x.scale)[km$cluster == 1]
cluster2 <- rownames(x.scale)[km$cluster == 2]

pca_results <- prcomp(x, scale = TRUE)

# Get the number of principal components
num_pc <- ncol(pca_results$x)

num_pc
plot(pca_results, type = "lines")
abline(h = 0, v = 0, lty = 2)
axis(side = 1)
axis(side = 2)


# Calculate the cumulative proportion of variance explained
cum_var <- cumsum(pca_results$sdev^2 / sum(pca_results$sdev^2))

# Find the smallest number of principal components that explain at least 90% of the variance
num_pc <- min(which(cum_var >= 0.9))
num_pc
