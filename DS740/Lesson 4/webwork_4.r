library(dplyr)
library(caret)
library(pROC)

dividends <- read.csv("Lesson 4/Dividends.csv")
View(dividends)

my_roc = roc(response=dividends$y, predictor=dividends$x)
my_roc
ldafit = lda(y~x, data=dividends)
pred = predict(ldafit,data=dividends)
confmat <- table(dividends$y,pred$class)
fitError = sum(y != pred)/dim(dividends)[1]; fitError
sensitivity(confmat)
specificity(confmat)

ldaprob = predict(ldafit,data=dividends)$posterior[,2]
rocout2 = roc(response=dividends$y, predictor=ldaprob)
plot(rocout2)
yhat = predict(ldafit,dividends)$class
table(dividends$y,yhat)
#### sensitivity and specificity
confmat = table(dividends$y,yhat)
sensitivity(confmat)
specificity(confmat)
confmat[2,2] / (confmat[2,2] + confmat[2,1])

### problem 6
clev <- read.csv("Lesson 4/Heart_disease_Cleveland.csv")
clev <- clev %>%
    select(Age, BloodPressure, Chol, MaxHeartRate, STdepress, DiseaseStatus)
model1 <- lda(DiseaseStatus~MaxHeartRate+STdepress, data=clev)
table(clev$DiseaseStatus, predict(model1, clev)$class)
newpt <- data.frame(MaxHeartRate=130, STdepress=2)
predict(model1, newpt, type="response")$class
model2 <- lda(DiseaseStatus~., data=clev)
table(clev$DiseaseStatus, predict(model2, clev)$class)

### problem 7
set.seed(4)


n = dim(clev)[1]; n
nfolds = 10
groups = c(rep(1:nfolds,length=n))
cvgroups = sample(groups,n)
Model1predCVclass = factor(rep(NA,n),levels=c("0","1","2","3","4"))
for (i in 1:nfolds) {
    test = which(cvgroups == i)
    train = which(cvgroups != i)
    model1 = lda(DiseaseStatus~ MaxHeartRate + STdepress, data=clev[train,])
    Model1predCVclass[test] = predict(model1, clev[test,])$class
}
MSE = sum(Model1predCVclass != clev$DiseaseStatus)/n; MSE

set.seed(4)
n = dim(clev)[1]; n
nfolds = 10
groups = c(rep(1:nfolds,length=n))
cvgroups = sample(groups,n)
Model2predCVclass = factor(rep(NA,n),levels=c("0","1","2","3","4"))
for (i in 1:nfolds) {
    test = which(cvgroups == i)
    train = which(cvgroups != i)
    model2 = lda(DiseaseStatus~., data=clev[train,])
    Model2predCVclass[test] = predict(model2, clev[test,])$class
}
MSE = sum(Model2predCVclass != clev$DiseaseStatus)/n; MSE

### problem 8
model3 <- qda(DiseaseStatus~ MaxHeartRate + STdepress, data=clev)
model3
predict(model3, newpt, type="response")$class
table(clev$DiseaseStatus, predict(model3, clev)$class)
predict(model3, newpt, type="response")$class
model4 <- qda(DiseaseStatus~., data=clev)
predict(model4, clev, type="response")$class
table(clev$DiseaseStatus, predict(model4, clev)$class)
qdafit3 = qda(DiseaseStatus ~ MaxHeartRate + STdepress, data=clev)
clev %>% 
  group_by(DiseaseStatus) %>%
  summarize(sdHrt = sd(MaxHeartRate),sdSTdepress = sd(STdepress))


### problem 9
k = 5
p = 5

output = k * (p + 1) * (p/2 + 1); output

set.seed(4)


n = dim(clev)[1]; n
nfolds = 10
groups = c(rep(1:nfolds,length=n))
cvgroups = sample(groups,n)
Model3predCVclass = factor(rep(NA,n),levels=c("0","1","2","3","4"))
for (i in 1:nfolds) {
    test = which(cvgroups == i)
    train = which(cvgroups != i)
    model3 = qda(DiseaseStatus~ MaxHeartRate + STdepress, data=clev[train,])
    Model3predCVclass[test] = predict(model3, clev[test,])$class
}
MSE = sum(Model3predCVclass != clev$DiseaseStatus)/n; MSE

set.seed(4)
n = dim(clev)[1]; n
nfolds = 10
groups = c(rep(1:nfolds,length=n))
cvgroups = sample(groups,n)
Model4predCVclass = factor(rep(NA,n),levels=c("0","1","2","3","4"))
for (i in 1:nfolds) {
    test = which(cvgroups == i)
    train = which(cvgroups != i)
    model4 = qda(DiseaseStatus~., data=clev[train,])
    Model4predCVclass[test] = predict(model4, clev[test,])$class
}
MSE = sum(Model4predCVclass != clev$DiseaseStatus)/n; MSE