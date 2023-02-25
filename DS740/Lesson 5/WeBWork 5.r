library(dplyr)
library(glmnet)
library(MASS)
library(plotmo)
library(ggplot2)
library(boot)

### Problem 4
# Read in and Review data
BostonNew <- Boston %>%
    mutate(log.crim = log(crim), log.zn = log(zn + 1))

# remove crim and zn from BostonNew
BostonNew <- BostonNew[, -c(1, 2)]
x = model.matrix(log.crim~.,data=BostonNew)[,-1]
y = BostonNew$log.crim
n = dim(BostonNew)[1]
p = dim(BostonNew)[2]
lambdalist = (1:1000/1000)
RRfit = glmnet(x, y, alpha = 0,lambda=lambdalist)
RRcoef = coef(RRfit,s=0.05); RRcoef
LASSOfit = glmnet(x, y, alpha = 1,lambda=lambdalist)
LASSOcoef = coef(LASSOfit,s=0.05); LASSOcoef
ENETfit = glmnet(x, y, alpha = 0.5,lambda=lambdalist)
ENETcoef = coef(ENETfit,s=0.05); ENETcoef

# Problem 5 - (rewatch presentation 2 from lesson 2 for bootstrap refresser)
n=506
ncv = 10
groups=rep(1:10,length=n)
set.seed(5)
cvgroups = sample(groups,n)

cvRR = cv.glmnet(x, y, lambda=lambdalist, alpha = 0, nfolds=ncv, foldid=cvgroups)
min(cvRR$cvm)
RRbestlambda = cvRR$lambda.min
RRbestlambda
