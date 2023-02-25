library(dplyr)
library(glmnet)
library(ISLR)
library(ggplot2)

# Read in and Review data
Trees <- read.csv("TreesTransformed.csv")  
x = model.matrix(Volume~.,data=Trees)[,-1]
y = Trees$Volume
lambdalist = c((1:100)/100)

# question 1
MLfit = lm(y~x)
RRfit = glmnet(x, y, alpha = 0,lambda=lambdalist)
LASSOfit = glmnet(x, y, alpha = 1,lambda=lambdalist)
ENETfit = glmnet(x, y, alpha = 0.7,lambda=lambdalist)

# question 2
summary(MLfit)

# question 3
answer3 <- "Of the five predictor variables in the set, three are transformations of the other two. When taken all into account together, they remove their own significance. "

# question 4
# Ridge Regression
# LASSO

# question 5
LASSOcoef = coef(LASSOfit,s=0.1); LASSOcoef

# question 6
# Height
# Girth * Height
# Girth^2 * Height

# question 7
# LASSO

# question 8
0.35

# question 9
head(College)

# question 10
CollegeT <- College %>%
    mutate(Private = as.factor(Private),
        log.Enroll = log(Enroll),
        log.Apps = log(Apps),
        log.Accept = log(Accept),
        log.F.Undergrad = log(F.Undergrad),
        log.P.Undergrad = log(P.Undergrad)) %>%
    select(-Enroll, -Apps, -Accept, -F.Undergrad, -P.Undergrad)

# question 11
