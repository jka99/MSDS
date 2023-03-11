library(tree)
library(dplyr)
library(gbm)

setwd("C:/Users/jeffe/Documents/MSDS/GitHub/MSDS/DS740/Lesson 7")

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