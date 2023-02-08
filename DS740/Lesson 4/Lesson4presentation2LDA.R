############# libraries used ############
# for methods
library(MASS)  #help(lda)
library(pROC)
# for assumption-checking (none)
# for visuals
library(dplyr)
library(ggformula)

# added lines to specify version of iris data set
library(MVTests)
iris <- MVTests::iris  # added to require the version of the iris data set with capitalized Species names
levels(iris$Species)

############# Review data #############

data(iris)
n = dim(iris)[1]; n
names(iris)
levels(iris$Species)
K=length(levels(iris$Species)); K

# try different options: Sepal.Length, Sepal.Width, Petal.Length, Petal.Width
boxplot(Petal.Length ~ Species, data = iris)
gf_boxplot(Petal.Length ~ Species, data = iris,fill=c("skyblue","navy","purple"))
#both Petal.Length and Petal.Width look like good candidates for predictors

############# ROC of LDA predictions for two-level response ############# 
# just using the predictor values since decide off linear function of predictor
iris$Virginica = as.numeric(iris$Species == "Virginica")
lda.roc1 = roc(response=iris$Virginica, 
               predictor=iris$Petal.Length)
plot.roc(lda.roc1)

# using posterior probabilities
ldafitVirg = lda(Virginica~Petal.Length, data=iris)
# posterior probability of Virginica species
ldaprob = predict(ldafitVirg,data=iris)$posterior[,2]
# posterior probability way of ROC curve for LDA
lda.roc2 = roc(response=iris$Virginica, 
               predictor=ldaprob)
plot.roc(lda.roc2)  # exact same curve as above

############# apply LDA with three classes in Species #############
ldafit = lda(Species~Petal.Length, data=iris)
ldafit

# look at error, re-predicting data used to fit the model
y = iris$Species
predclass = predict(ldafit,data=iris)$class
table(y,predclass)
fitError = sum(y != predclass)/n; fitError

############# Honest prediction via cross-validation #############
CVpredclass = rep("NA",n)

# define cvgroups
nfolds = 10
groups = c(rep(1:nfolds,length=n))
set.seed(4)
cvgroups = sample(groups,n)

# loop through cvgroups, to compute honest predicted values for CV measure
for (ii in 1: nfolds) {    # ii is an easier string to search for index
  groupii = (cvgroups == ii)
  trainset = iris[!groupii,]  # all data EXCEPT for group ii
  testset = iris[groupii, ]   # data in group ii

  ldafitii = lda(Species~Petal.Length, data=trainset)

#  predicted = predict(ldafitii, newdata=testset)$class   # predict for test set
  predicted = as.character(predict(ldafitii, newdata=testset)$class)   # predict for test set
  CVpredclass[groupii] = predicted              # store in ordered locations
}

# compute CV measure as misclassification rate
table(y,CVpredclass)
CVError = sum(CVpredclass!=y)/n; CVError

############# Check Assumptions #############
xvar = iris$Petal.Length
xSetosa = xvar[iris$Species == "Setosa"]
xVersicolor = xvar[iris$Species == "Versicolor"]
xVirginica = xvar[iris$Species == "Virginica"]

shapiro.test(xSetosa); qqnorm(xSetosa)
shapiro.test(xVersicolor); qqnorm(xVersicolor)
shapiro.test(xVirginica); qqnorm(xVirginica)
#normality of the Petal.Length is reasonable

bartlett.test(xvar,iris$Species)
iris %>%
  group_by(Species) %>% 
  summarize(GroupSD = sd(Petal.Length))
#equal variances are definitely NOT reasonable, so QDA is the better option -- see next Lecture


############# [Bonus] Visualization of boundaries #############
#statistics by class of Species
pi.hat1 = length(xSetosa)/n
pi.hat2 = length(xVersicolor)/n
pi.hat3 = length(xVirginica)/n
mu.hat1 = mean(xSetosa)
mu.hat2 = mean(xVersicolor)
mu.hat3 = mean(xVirginica)
sigma2 = 1/(n-K)*(sum((xSetosa-mu.hat1)^2)+sum((xVersicolor-mu.hat2)^2)+sum((xVirginica-mu.hat3)^2))

#linear decision boundaries
slope1 = (mu.hat1/sigma2); int1 = (-(1/2)*mu.hat1^2/sigma2 + log(pi.hat1))
slope2 = (mu.hat2/sigma2); int2 = (-(1/2)*mu.hat2^2/sigma2 + log(pi.hat2))
slope3 = (mu.hat3/sigma2); int3 = (-(1/2)*mu.hat3^2/sigma2 + log(pi.hat3))
hist(xSetosa,col="skyblue", main = "Petal Length, split by Species",
     xlim=c(0,max(iris$Petal.Length)),ylim=c(0,90),
     ylab="Linear.k",xlab="Petal Length")
hist(xVersicolor,col=rgb(0,0,.3,0.6),add=T,breaks=20)
hist(xVirginica,col=rgb(0.4,0,0.6,0.5),add=T,breaks=20)

curve(slope1*x+int1,col="skyblue",lwd=2,add=T)
abline(int2,slope2,col="navy",lwd=2)
abline(int3,slope3,col="purple",lwd=2)
legend("topleft",c("setosa","versicolor","virginica"),col=c("skyblue","navy","purple"),lwd=2)

bound12 = (int1-int2)/(slope2-slope1); bound12; abline(v=bound12,lty=2)
bound23 = (int2-int3)/(slope3-slope2); bound23; abline(v=bound23,lty=2)
#so classify 1="setosa" for Petal.Length < 2.861; 
#            2="versicolor" for Petal.Length from 2.861 to 4.906;
#        and 3="virginica" for Petal.Length > 4.906

#looking at the original goal functions
p1num = function(x) 1/(sqrt(2*pi))/sqrt(sigma2)*exp(-.5*(x-mu.hat1)^2/sigma2)*pi.hat1
p2num = function(x) 1/(sqrt(2*pi))/sqrt(sigma2)*exp(-.5*(x-mu.hat2)^2/sigma2)*pi.hat1
p3num = function(x) 1/(sqrt(2*pi))/sqrt(sigma2)*exp(-.5*(x-mu.hat3)^2/sigma2)*pi.hat1
hist(xSetosa,col="skyblue", main = "Petal Length, split by Species",
     xlim=c(0,max(iris$Petal.Length)),ylim=c(0,2.5),
     ylab="p_k(x)",xlab="Petal Length",prob=T,breaks=5)
hist(xVersicolor,col=rgb(0,0,.3,0.6),add=T,breaks=10,prob=T)
hist(xVirginica,col=rgb(0.4,0,0.6,0.5),add=T,breaks=10,prob=T)
curve(p1num(x)/(p1num(x)+p2num(x)+p3num(x)),add=T,col="skyblue3")
curve(p2num(x)/(p1num(x)+p2num(x)+p3num(x)),add=T,col="navy")
curve(p3num(x)/(p1num(x)+p2num(x)+p3num(x)),add=T,col="purple")
abline(v=bound12,lty=2); abline(v=bound23,lty=2)

