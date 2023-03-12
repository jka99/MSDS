library(tree)
library(dplyr)
library(gbm)

setwd("C:/Users/jeffe/Documents/MSDS/GitHub/MSDS/DS740/Lesson 7")

# good luck if you are trying to follow this one. i'm all over the place here

cancer <- read.csv("CancerSurvival01.csv")

cancer_tree <- tree(factor(Survival01) ~., data = cancer)
plot(cancer_tree)
text(cancer_tree, pretty = 0)

preds = predict(cancer_tree, cancer_set, type = "class")
conf_mat = table(preds, cancer_set$Survival01)
sum(diag(conf_mat)) / dim(cancer_set)[1]
1 - sum(diag(conf_mat)) / dim(cancer_set)[1]
summary(cancer_tree)
cancer_tree
set.seed(400)
cancer.cv <- cv.tree(cancer_tree, FUN = prune.misclass)
cancer.cv
plot(cancer.cv)
prune_cancer <- prune.misclass(cancer_tree, best = 3)
plot(prune_cancer)
text(prune_cancer, pretty = 0)

library(gbm)
set.seed(400)
boost = gbm(Survival01~., data=cancer, distribution = "bernoulli", 
                    n.trees=5000, shrinkage = .001, interaction.depth=2)
summary(boost)

# cross validation
set.seed(400)
n = dim(cancer)[1]
k = 10
groups = rep(1:k, length = n)
cvgroups = sample(groups, n)
boost.predict = rep(0, n)
data.used = cancer

for (ii in 1:k) {
  groupi = (cvgroups == ii)
  boost = gbm(Survival01~., data=data.used[!groupi,], distribution = "bernoulli", 
                  n.trees=5000, shrinkage = .001, interaction.depth=2)
  #boost.predict[groupi] = predict(boost, newdata = data.used[groupi, ], n.trees=5000, type="response")
  boost.predict[groupi] = predict(boost, newdata=data.used[groupi, ],
                                    n.trees=5000, type="response")
}

conf_mat = table(boost.predict > .5, data.used$Survival01)
conf_mat
sum(diag(conf_mat)) / n # Accuracy
1- sum(diag(conf_mat))/n # Classification error rate

library(randomForest)
set.seed(400)
cancer_bag <- randomForest(factor(Survival01)~., data=cancer, mtry=3, importance=TRUE)

plot(cancer_bag)
importance(cancer_bag)
partialPlot(cancer_bag, pred.data=cancer, x.var="Nodes", which.class=0)

# (4, 7), (0, 10), (2, 6)
# (4), (10), (6)
# (9.5, 4)
A <- 9.5 - (4 * 0.1) # 9.1
a <- 4 * 0.1 # 0.4

B <- 4 - (10 * 0.1) # 3
b <- 10 * 0.1 # 1

C <- 9.5 - (6 * 0.1) # 8.9
c <- 6 * 0.1 # 0.6

A + B + C # 21
a + b + c # 2