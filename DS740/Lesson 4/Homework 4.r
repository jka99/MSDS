### Homework 4
library(MASS)
library(pROC)
library(dplyr)
library(ISLR)
library(mvnormalTest)  
library(MVN)
library(MVTests)
library(ggformula)
library(ggplot2)
# jeeze, that's a lot of libraries

### question 1
NewAuto <- Auto %>%
  mutate(Domestic = as.numeric(origin == 1))
NewAuto %>%
    group_by(Domestic) %>%
    count()

### question 2
gf_boxplot(mpg ~ as.factor(Domestic), data = NewAuto, 
    fill = c("skyblue", "pink"))

### question 3 and 4
NewAuto %>%
    group_by(Domestic) %>%
    summarize(mean(mpg), sd(mpg))

### question 5
answer5 <- "Looking at the boxplots, the IQRs appear to be similar. The median of each is roughly centralized and not wildly skewed high or low. Looking at the qqplots, both data sets are mostly normal. Foreign cars are far more normal than Domestic, especially on the ends. Domestic stays normal-ish for almost 2 standard deviations. I think it is OK to use LDA with this data."

### question 6
model <- lda(Domestic ~ mpg, data = NewAuto)
predict(model, data = NewAuto)
confmat <- table(NewAuto$Domestic, predict(model, data = NewAuto)$class); confmat

### question 7
confmat[2,2] / (confmat[2,2] + confmat[2,1])

### FIX THIS
### question 8
TN = confmat[1,1]; TN
FN = confmat[1,2]; FN
TP = confmat[2,2]; TP
FP = confmat[2,1]; FP
TN / (TN + FP)

### question 9
gf_boxplot(displacement ~ as.factor(Domestic), data = NewAuto) ### <-- WINNER
gf_boxplot(horsepower ~ as.factor(Domestic), data = NewAuto)
gf_boxplot(acceleration ~ as.factor(Domestic), data = NewAuto)
# I believe displacement is what they are looking for as "discriminating" variable

### question 10
NewAuto1 <- NewAuto %>%
    filter(Domestic == 1)
library(car)
qqPlot(NewAuto1$displacement)
NewAuto2 <- NewAuto %>%
    filter(Domestic == 0)
qqPlot(NewAuto2$displacement)
NewAuto3 <- NewAuto %>%
    filter(Domestic == 1) %>%
    mutate(SD = sd(displacement))
NewAuto4 <- NewAuto %>%
    filter(Domestic == 0) %>%
    mutate(SD = sd(displacement))
head(NewAuto4)

answer10 <- "I think QDA is better for displacement. The two groups are very different on the boxplots with no overlap in their IQRs. Displacement is much higher and has a much broader range for domestic vehicles than it is for foreign cars. Upper-end outliers for foreign cars fall into the first quartile for domestic cars. The standard deviations for the two groups are very different, making LDA a bad choice for this model."

### question 11
model2 <- qda(Domestic ~ displacement, data = NewAuto)
mod2_pred <- predict(model2, data = NewAuto)$class
table(NewAuto$Domestic, mod2_pred)
ldaprob.displ = predict(lda(Domestic~displacement, data=NewAuto),data=NewAuto)$posterior[,2]
lda.roc.displ <- roc(response=NewAuto$Domestic, predictor=ldaprob.displ)
plot.roc(lda.roc.displ, main = "ROC for predictor displacement AUC = "); auc(lda.roc.displ)

### question 12
answer12 <- "Displacement is a better predictor for domestic. The AUC is higher than mpg, which is very good. The ROC curve is very close to the upper left corner, which is also very good. The model is very good at predicting the origin of a car based on its displacement."

### question 13
NewAutoOrigin <- Auto %>%
    mutate(origin = as.factor(origin))
gf_point(mpg ~ displacement, data = NewAutoOrigin, color = NewAutoOrigin$origin, alpha = 0.5)

### question 14, 15, 16, 17, 18, 19
Auto1 <- NewAutoOrigin %>%
    mutate(origin = as.factor(origin))

model1 <- lda(origin ~ mpg + cylinders + displacement + horsepower + weight, data = NewAutoOrigin)
predict1 <- predict(model1, data = NewAutoOrigin)$class
table(NewAutoOrigin$origin, predict1)
MSE = mean((predict1 != NewAutoOrigin$origin)^2); MSE
model2 <- qda(origin ~ mpg + cylinders + displacement + horsepower + weight, data = NewAutoOrigin)
predict2 <- predict(model2, data = NewAutoOrigin)$class
table(NewAutoOrigin$origin, predict2)
MSE2 = mean((predict2 != NewAutoOrigin$origin)^2); MSE2

### question 20
answer20 <- "It's not surprising that both models performed well when classifying American cars. The bulk were quite separate on the scatter plot. The LDA performed better overall, albeit ever so slightly, because there are some boundaries visible in the scatterplot (which are surely more evident when additional variables are added). The standard deviations for Japanese and European cars are also much closer to one another than either of them are to American cars. "

### question 21
American

### question 22

model1 <- lda(origin ~ displacement, data = NewAutoOrigin)
model2 <- lda(origin ~ mpg + cylinders + displacement + horsepower + weight,
               data = NewAutoOrigin)
model3 <- qda(origin ~ displacement, data = NewAutoOrigin)
model4 <- qda(origin ~ mpg + cylinders + displacement + horsepower + weight,
               data = NewAutoOrigin)

# ## Model 1
# nfolds = 10; n=392
# groups = rep(1:nfolds,length=n)
# set.seed(4)
# cvgroups = sample(groups,n); table(cvgroups)
# Model1predCVclass = factor(rep(NA,n),levels=c("1","2","3"))
# for (ii in 1:nfolds){
#   groupii = (cvgroups == ii) # create a logical vector for the test set
#   trainset = x[!groupii, ] # create the training set
#   testset = x[groupii, ] # create the test set
#   Model1predCVclass[groupii] = predict(model1, newdata = testset, 
#                                        type = "response")$class # predict
# }
# table(Model1predCVclass, x$origin)
# MSE1 = sum(Model1predCVclass != x$origin)/n; MSE1

# # MSE = sum(Model1predCVclass != clev$DiseaseStatus)/n; MSE

NewAutoOrigin <- Auto %>%
  mutate(origin = as.factor(origin))
x <- NewAutoOrigin

## Model 1
methodapplied = "LDA"
modelapplied = (origin ~ displacement)
y = NewAutoOrigin$origin

nfolds = 10; n=392
groups = rep(1:nfolds,length=n)
set.seed(4)
cvgroups = sample(groups,n); table(cvgroups)
ModelpredCVclass = factor(rep(NA,n),levels=c("1","2","3"))

for (ii in 1: nfolds) { 
  groupii = (cvgroups == ii)
  trainset = x[!groupii,]
  testset = x[groupii, ] 
  
  if (methodapplied == "LDA") {
    modelfitii = lda(modelapplied, data=trainset) }
  else if (methodapplied == "QDA") {
    modelfitii = qda(modelapplied, data=trainset)
  }
  
  predicted = as.character(predict(modelfitii, newdata=testset)$class)   
  ModelpredCVclass[groupii] = predicted              
}
CVError1 = sum(ModelpredCVclass!=y)/n; CVError1

## Model 2
methodapplied = "LDA"
modelapplied = (origin ~ mpg + cylinders + displacement + horsepower + weight)
y = NewAutoOrigin$origin

nfolds = 10; n=392
groups = rep(1:nfolds,length=n)
set.seed(4)
cvgroups = sample(groups,n); table(cvgroups)
ModelpredCVclass = factor(rep(NA,n),levels=c("1","2","3"))

for (ii in 1: nfolds) {   
  groupii = (cvgroups == ii)
  trainset = x[!groupii,] 
  testset = x[groupii, ] 
  
  if (methodapplied == "LDA") {
    modelfitii = lda(modelapplied, data=trainset) }
  else if (methodapplied == "QDA") {
    modelfitii = qda(modelapplied, data=trainset)
  }
  
  predicted = as.character(predict(modelfitii, newdata=testset)$class)   
  ModelpredCVclass[groupii] = predicted  
}
CVError2 = sum(ModelpredCVclass!=y)/n; CVError2


## Model 3
methodapplied = "QDA"
modelapplied = (origin ~ displacement)
y = NewAutoOrigin$origin

nfolds = 10; n=392
groups = rep(1:nfolds,length=n)
set.seed(4)
cvgroups = sample(groups,n); table(cvgroups)
ModelpredCVclass = factor(rep(NA,n),levels=c("1","2","3"))

for (ii in 1: nfolds) { 
  groupii = (cvgroups == ii)
  trainset = x[!groupii,]  
  testset = x[groupii, ] 
  
  if (methodapplied == "LDA") {
    modelfitii = lda(modelapplied, data=trainset) }
  else if (methodapplied == "QDA") {
    modelfitii = qda(modelapplied, data=trainset)
  }
  
  predicted = as.character(predict(modelfitii, newdata=testset)$class)   
  ModelpredCVclass[groupii] = predicted 
}
CVError3 = sum(ModelpredCVclass!=y)/n; CVError3

## Model 4
methodapplied = "QDA"
modelapplied = (origin ~ mpg + cylinders + displacement + horsepower + weight)
y = NewAutoOrigin$origin

nfolds = 10; n=392
groups = rep(1:nfolds,length=n)
set.seed(4)
cvgroups = sample(groups,n); table(cvgroups)
ModelpredCVclass = factor(rep(NA,n),levels=c("1","2","3"))

for (ii in 1: nfolds) { 
  groupii = (cvgroups == ii)
  trainset = x[!groupii,] 
  testset = x[groupii, ] 
  
  if (methodapplied == "LDA") {
    modelfitii = lda(modelapplied, data=trainset) }
  else if (methodapplied == "QDA") {
    modelfitii = qda(modelapplied, data=trainset)
  }
  
  predicted = predict(modelfitii, newdata=testset)$class 
  ModelpredCVclass[groupii] = predicted 
}
CVError4 = sum(ModelpredCVclass!=y)/n; CVError4

CVError1; CVError2; CVError3; CVError4


answers <- c(0.2933673, 0.2704082, 0.3035714, 0.2704082)

### FIX 26
### question 26
k = 3
p = 5

output = k * (p + 1) * (p/2 + 1); output
output = k+k*p+p*(p+1)/2; output

### question 28
answer28 <- "Model 2 and Model 4 have the same calculated CV10. Model 2 is the better of the two. It has fewer predictor variables than Model 4 making it a simpler model. With fewer variables, you have less variability in the model. With that in mind, Model 2 also uses LDA which creates far fewer parameters than the QDA of Model 4, which makes it more efficient computationally."


### question 29
# check the assumption of multivariate normality
# using the Shapiro-Wilk test
# H0: the data are normally distributed
# H1: the data are not normally distributed
# p-value < 0.05: reject H0
# p-value > 0.05: fail to reject H0

# full Xmatrix
xvar = NewAutoOrigin %>% 
  dplyr::select(mpg, cylinders, displacement, horsepower, weight)

# Xmatrix within each class
xAmerican = NewAutoOrigin %>% 
  filter(origin == 1) %>% 
  dplyr::select(mpg, cylinders, displacement, horsepower, weight)
xEuropean = NewAutoOrigin %>% 
  filter(origin == 2) %>% 
  dplyr::select(mpg, cylinders, displacement, horsepower, weight)
xJapanese = NewAutoOrigin %>% 
  filter(origin == 3) %>% 
  dplyr::select(mpg, cylinders, displacement, horsepower, weight)

# check for multivariate normality
mhz(xAmerican)$mv.test
mhz(xEuropean)$mv.test
mhz(xJapanese)$mv.test
#multivariate normality of the predictors is (close to) reasonable

# check for equal covariance
BoxM(xvar,NewAutoOrigin$origin)





# 1 -- 245
# 2 -- boxplot
# 3 -- 20
# 4 -- 29.1
# 5 -- answer5
# 6 -- check code above
# 7 -- 0.833
# 8 -- FIX 8 see code above
# 9 -- displacement
# 10 -- answer10
# 11 -- ROC Curve
# 12 -- answer12
# 13 -- scatterplot
# 14 -- 208
# 15 -- 27
# 16 -- 57
# 17 -- 206
# 18 -- 29
# 19 -- 69
# 20 -- answer20
# 21 -- American
# 22 -- see code above
# 23 -- 0.293
# 24 -- 0.270
# 25 -- lower than CV(10) for Model 1
# 26 -- FIX (answer is 33)
# 27 -- 63
