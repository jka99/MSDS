############# libraries used ############
# for methods 
# for assumption-checking 
# for data organization
library(dplyr)
# for visuals
library(ggplot2)
library(gridExtra)

############# Review data #############
# read in data and review
cancer = read.csv("WI_breastcancer_characteristics.csv")
names(cancer)
dim(cancer)

# measurements only (no diagnosis)
x <- cancer %>%
  select(-ID)
x.means <- cancer %>%  # mean measurements only 
  select(contains("mean"))
x.extremes <- cancer %>%  # mean measurements only 
  select(contains("extreme"))
n = dim(x)[1]
p = dim(x)[2]
p.means = dim(x.means)[2]
p.extremes = dim(x.means)[2]

############# scaling visibly required, as previously visualized ############# 
# histograms of 3 selected variables
plot1 <- x %>%
  ggplot( aes(x=Texture.mean)) +
  geom_histogram( color="red", alpha=0.6, position = 'identity')
plot2 <- x %>%
  ggplot( aes(x=Area.extreme)) +
  geom_histogram( color="blue", alpha=0.6, position = 'identity')
plot3 <- x %>%
  ggplot( aes(x=Smoothness.extreme)) +
  geom_histogram( color="black", alpha=0.6, position = 'identity')
grid.arrange(plot1, plot2, plot3, nrow=3)

######################################################
############ Fitting principal components ############
######################################################

############# fitting to full set of 30 measurements ############# 
# include scaling due to widely varied magnitudes
pc.info = prcomp(x,center=T,scale=T)

############# fitting to 10 mean measurements ############# 
# pc.info = prcomp(x.means,center=T,scale=T)

############# fitting to 10 extreme measurements ############# 
# pc.info = prcomp(x.extremes,center=T,scale=T)

############# PC summary of variance ############# 
summary(pc.info) # min(n-1,p) is number of components explaining variance (proportion of variance  > 0)
summary(pc.info)$importance
plot(pc.info)
# cumulative PVE by direct computation
  pc.info$sdev
  vjs = pc.info$sdev^2
  pve = vjs/sum(vjs); pve
  cumsum(pve)  
# cumulative PVE directly from output
CumulativePVE <- summary(pc.info)$importance[3,]; CumulativePVE

plot(CumulativePVE, type = "o", ylab="Cumulative PVE", xlab="Principal Component")

############# PC components and visuals ############# 
# loadings of principal components
pc.info$rotation  
pc.loadings1 = pc.info$rotation[,1]  # loadings for first principal component
pc.loadings2 = pc.info$rotation[,2]  # loadings for second principal component
pc1scores = pc.info$x[,1]  # first principal component score vector
pc2scores = pc.info$x[,2]  # second principal component score vector

# plotting score vectors + loadings of first two principal components
biplot(pc.info,choices=1:2,scale=0)

# scores for observation 79
cbind(pc1scores,pc2scores)[79,]
# loadings for Dimesion.means
pc.loadings1[10]; pc.loadings2[10]

