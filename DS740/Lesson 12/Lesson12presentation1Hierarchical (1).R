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

# histograms of 3 selected variables
plot1 <- xselect %>%
  ggplot( aes(x=Texture.mean)) +
  geom_histogram( color="red", alpha=0.6, position = 'identity')
plot2 <- xselect %>%
  ggplot( aes(x=Area.extreme)) +
  geom_histogram( color="blue", alpha=0.6, position = 'identity')
plot3 <- xselect %>%
  ggplot( aes(x=Smoothness.extreme)) +
  geom_histogram( color="black", alpha=0.6, position = 'identity')
grid.arrange(plot1, plot2, plot3, nrow=3)

# scaling visibly required to allow all predictors to contribute to similarity measure
x.scale = scale(x)
xselect.scale = scale(xselect)

######################################################
############# Fitting hierarchical models ############
######################################################

# Euclidean distance
dist.xselect.scale = dist(xselect.scale, method="euclidean")
dist.x.scale = dist(x.scale, method="euclidean")
# Manhattan distance
dist.xselect.scale.man = dist(xselect.scale, method="manhattan")
dist.x.scale.man = dist(x.scale, method="manhattan")

############# complete  linkage ############
# using selected p = 3 measurements
#hc.fit = hclust(dist.xselect.scale,method="complete")  # Euclidean
hc.fit = hclust(dist.xselect.scale.man,method="complete")  # Manhattan
linktype = "Complete Linkage"
# distance at which merge via complete linkage occurs
hc.fit$height
hc.4321 = hc.fit$height[(n-4):(n-1)]
hc.avg = (hc.fit$height[(n-3):(n-1)]+hc.fit$height[(n-4):(n-2)])/2
# obtaining cluster labels
hc.fit$height[(n-4):(n-1)]
nclust=2
htclust = 7.56 # mean(hc.fit$height[(n-2):(n-1)])
membclust = cutree(hc.fit,k=nclust) # cutree(hc.fit,h = htclust)

############# single  linkage ############
# using selected p = 3 measurements
hc.fit = hclust(dist.xselect.scale,method="single")
linktype = "Single Linkage"
# distance at which merge via complete linkage occurs
hc.4321 = hc.fit$height[(n-4):(n-1)]
hc.avg = (hc.fit$height[(n-3):(n-1)]+hc.fit$height[(n-4):(n-2)])/2
# obtaining cluster labels
hc.fit$height[(n-4):(n-1)]
nclust=2 
htclust = mean(hc.fit$height[(n-2):(n-1)])
membclust = cutree(hc.fit,k=nclust) # cutree(hc.fit,h = htclust)

############# average  linkage ############
# using selected p = 3 measurements
hc.fit = hclust(dist.xselect.scale,method="average")
linktype = "Average Linkage"
# distance at which merge via complete linkage occurs
hc.4321 = hc.fit$height[(n-4):(n-1)]
hc.avg = (hc.fit$height[(n-3):(n-1)]+hc.fit$height[(n-4):(n-2)])/2
# obtaining cluster labels
hc.fit$height[(n-4):(n-1)]
nclust=2
htclust = mean(hc.fit$height[(n-2):(n-1)])
membclust = cutree(hc.fit,k=nclust) # cutree(hc.fit,h = htclust)

############# complete  linkage, p=30, Manhattan distance ############
# using all p = 30 variables
# try manhattan distance with so many measurements
hc.fit = hclust(dist.x.scale.man,method="complete")
linktype = "Complete Linkage, Manhattan Distance, p=30"
# distance at which merge via complete linkage occurs
hc.4321 = hc.fit$height[(n-4):(n-1)]
hc.avg = (hc.fit$height[(n-3):(n-1)]+hc.fit$height[(n-4):(n-2)])/2
# obtaining cluster labels
hc.fit$height[(n-4):(n-1)]
nclust=2
htclust = mean(hc.fit$height[(n-2):(n-1)])
membclust = cutree(hc.fit,k=nclust) # cutree(hc.fit,h = htclust)

#############  visualize in dendrogram ############# 
dend.form = as.dendrogram(hc.fit)
dend.merge <- ggdendrogram(dend.form, rotate = F,labels=F) + 
  labs(title = linktype) +
  geom_hline(yintercept=hc.4321, linetype="dashed", 
             color = c("red","blue","gold3","gray"))  
dend.merge
dend.merge +
  geom_hline(yintercept=hc.avg, size = 2,
             color = c(rgb(.5,0,1,0.5),rgb(.5,1,0,0.5),rgb(.5,.5,.2,0.5))) 

#############  scatterplot pairs of 3 selected variables #############  
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


#############  compare to actual diagnosis #############  
membclust
cancerdiagnosis = read.csv("WI_breastcancer_response.csv")
actualdiagnosis = cancerdiagnosis$Diagnosis

table(actualdiagnosis,membclust)

HierResults = data.frame(ID = cancerdiagnosis$ID, membHier = membclust)
write.csv(HierResults,"HierResults.csv")
