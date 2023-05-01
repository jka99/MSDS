setwd("C:/Users/jeffe/Documents/MSDS/GitHub/MSDS/DS740/Lesson 12")

library(dplyr)
# for visuals
library(ggplot2)
library(gridExtra)
library(ggdendro)


wine <- read.csv("wine.csv")
n = dim(wine)[1]
p = dim(wine)[2]

### question 1
sapply(wine, mean)
sapply(wine, sd)
answer1 <- "It is necessary to scale variables before clustering to ensure that no one variable over powers the others in terms of importance. In this data set, the mean of Nonflavanoids is 0.362 while the mean of Proline is 746.9. Without scaling, Proline is many orders of magnitude larger than Nonflavanoids. Proline and Nonflavanoids have similarly different standard deviations. The ratio of standard deviations between the two is 2539.6."

### question 2
x <- wine
x.scale <- scale(x)
dist.x.scale = dist(x.scale, method="euclidean")
hc.fit = hclust(dist.x.scale,method="complete")
dend.form = as.dendrogram(hc.fit)
dend.merge <- ggdendrogram(dend.form, rotate = F,labels=F)
dend.merge

### question 3
hc.3 = round(hc.fit$height[(n-3)],2)

### question 4
hc.fit = hclust(dist.x.scale,method="single")
dend.form = as.dendrogram(hc.fit)
dend.merge <- ggdendrogram(dend.form, rotate = F,labels=F)
dend.merge
hc.fit = hclust(dist.x.scale,method="average")
dend.form = as.dendrogram(hc.fit)
dend.merge <- ggdendrogram(dend.form, rotate = F,labels=F)
dend.merge
answer4 <- "complete"

### question 5
answer5 <- "complete"

### question 6
answer6 <- "height, very similar, one, the other two clusters"

### question 7
hc.fit = hclust(dist.x.scale,method="complete")
nclust = 3
membclust = cutree(hc.fit,k=nclust)
ggplot(data = wine, aes(x=Dilution, y=Alcohol, color=factor(membclust))) +
  geom_point() + 
  labs(title = "Alcohol vs. Dilution",color = "Cluster") 
  

### question 8
K_values <- c(2, 3, 4, 5)
consistency_results <- matrix(NA, nrow = length(K_values), ncol = 2)

for (k_idx in 1:length(K_values)) {
  nclust <- K_values[k_idx]
  
  set.seed(12)
  clustA <- kmeans(x.scale, nclust)$cluster
  
  set.seed(24)
  clustB <- kmeans(x.scale, nclust)$cluster
  
  tablematch <- table(clustA, clustB)
  
  if (nclust == 2) {
    matchtotal <- sum(diag(tablematch))
  } else {
    matchtotal <- sum(apply(tablematch, 2, max))
  }
  
  consistency <- matchtotal / nrow(x.scale)
  consistency_results[k_idx, ] <- c(nclust, consistency)
}

# Print the consistency results
consistency_df <- as.data.frame(consistency_results)
colnames(consistency_df) <- c("K", "Consistency")
print(consistency_df)

# Check which K values produce consistent cluster memberships
consistency_threshold <- 0.95
consistent_K_values <- consistency_df$Consistency > consistency_threshold
print(consistent_K_values)
