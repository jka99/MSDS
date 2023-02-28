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
answer9 <- "The skewness of the variables Enroll, Apps, Accept, F.Undergrad, and P.Undergrad in the College dataset makes sense because colleges in the United States vary greatly in size. There are very small colleges with only a few hundred students, and there are very large colleges with tens of thousands of students.

The variables Enroll, Apps, Accept, F.Undergrad, and P.Undergrad are all measures of the size of a college, and as such, they are expected to have a wide range of values. Since there are relatively few very large colleges in the dataset compared to smaller colleges, the distribution of these variables will be right-skewed. This means that there will be more smaller colleges with lower values of these variables, and relatively fewer larger colleges with higher values."

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
# https://statisticsglobe.com/change-font-size-corrplot-r
library(corrplot)
library(RColorBrewer)
Coll.matrix <- cor(CollegeT[,-1])
corrplot(Coll.matrix, method = "circle", type = "upper", 
         col = rev(brewer.pal(n = 8 , name = "RdYlBu")), addCoef.col = 1, number.cex = 0.5)

# question 12
# log.Accept 

# question 13
answer13 <- "Per the College documentation, College$Enroll is the number of new students enrolled and College$Accept is the number of applications accepted. While there will be some students accepted that don't attend, colleges can't extend offers to more students, plus some margin, than they have spots to place them in. Plus the students counted in College$Enroll have to be counted in College$Accept."

# question 14
answer14 <- "The data set has a lot of predictors that vary wildly in terms of correlation. Some of these predictors won't add anything to the model. We need a method that will account for that and remove them from the model as appropriate. A less flexible method than multiple linear regression is also less likely to overfit."

# question 15
x = model.matrix(log.Enroll~.,data=CollegeT)[,-1]
y = CollegeT$log.Enroll
n = dim(CollegeT)[1]
p = dim(CollegeT)[2]
lambdalist = (1:1000/1000)
LASSOfit = glmnet(x, y, alpha = 1,lambda=lambdalist)
LASSOcoef = coef(LASSOfit,s=0.02); LASSOcoef
LASSOcoef = coef(LASSOfit,s=0.03); LASSOcoef
LASSOcoef = coef(LASSOfit,s=0.05); LASSOcoef
LASSOcoef = coef(LASSOfit,s=0.5); LASSOcoef

# question 16
# log.Accept
# log.F.Undergrad

# question 17, 18, and 19
ncollege = dim(CollegeT)[1]
groups = rep(1:10, length = ncollege)
set.seed(5)
cvgroups = sample(groups, ncollege)
ENETfit = glmnet(x, y, alpha = 0.75,lambda=lambdalist)
ENETcoef = coef(ENETfit,s=0.002); ENETcoef
cvENET = cv.glmnet(x, y, lambda=lambdalist, alpha = 0, nfolds=ncv, foldid=cvgroups)
min(cvENET$cvm)
ENETbestlambda = cvENET$lambda.min
ENETbestlambda
plot(cvENET$lambda, cvENET$cvm, type="l", xlab="lambda", ylab="CV(10) Error")


# answers
# 1 See code above
# 2 0
# 3 answer3
# 4 Ridge Regression, MLR
# 5 -1.714 (-1.707)
# 6 Height, Girth * Height, Girth^2 * Height
# 7 LASSO
# 8 0.35
# 9 answer9
# 10 See code above
# 11 See code above
# 12 log.Accept
# 13 answer13
# 14 answer14
# 15 See code above
# 16 log.Accept, log.F.Undergrad
# 17 See code above
# 18 0.002
# 19 See code above
