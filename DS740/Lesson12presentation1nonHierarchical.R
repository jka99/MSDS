############# libraries used ############
# for methods
# for assumption-checking 
# for data organization
library(dplyr)
# for visuals
library(ggplot2)
library(gridExtra)
library(ggdendro)

############# Review data #############
# read in data and review
cancer = read.csv("WI_breastcancer_characteristics.csv")
names(cancer)
dim(cancer)

# measurements only (no diagnosis)
x <- cancer %>%
  select(-ID)
xselect <- cancer %>%
  select(Texture.mean, Area.extreme, Smoothness.extreme)
n = dim(x)[1]
p = dim(x)[2]
pselect = dim(xselect)[2]

# scaling visibly required to allow all measurements to contribute to similarity measure
x.scale = scale(x)
xselect.scale = scale(xselect)


############ fitting non-hierarchical models ############
# K-means with p = 3 measurements
nclust=2  # try 2 and 3 and 4 clusters as well

set.seed(12)
clustA = kmeans(xselect.scale,nclust)$cluster
membclust = clustA

#############  Visuals - scatterplot pairs #############  
pairs12 <- xselect %>%
  ggplot( aes(x=Texture.mean, y=Area.extreme)) +
  geom_point( color=membclust)
pairs13 <- xselect %>%
  ggplot( aes(x=Texture.mean, y=Smoothness.extreme))  +
  geom_point( color=membclust)
pairs23 <- xselect %>%
  ggplot( aes(x=Area.extreme, y=Smoothness.extreme))  +
  geom_point( color=membclust)
grid.arrange(pairs12, pairs13, pairs23, nrow=1)


############# consistency of clusterings with k clusters #############  
#repeat the following a few times

clustA = kmeans(xselect.scale,nclust)$cluster
membclust = clustA
# run the visual commands (above)

clustB = kmeans(xselect.scale,nclust)$cluster
membclust = clustB
# run the visual commands (above)

tablematch <- table(clustA,clustB); tablematch

# if two clusters, just look at diagonals
matchtotal <- sum(apply(tablematch,2,max))
matchtotal/n

############# final non-hierarchical model ############
# K-means with full p = 30 measurements
nclust=2
set.seed(12)
membfull = kmeans(x.scale,nclust)$cluster
membclustNonH = membfull

############# comparing results ############# 
HierResults = read.csv("HierResults.csv")
membclustHier = HierResults$membHier

table(membclustHier,membclustNonH)

# true diagnoses
cancerdiagnosis = read.csv("WI_breastcancer_response.csv")
actualdiagnosis = cancerdiagnosis$Diagnosis

table(actualdiagnosis,membclustHier)
table(actualdiagnosis,membclustNonH)
