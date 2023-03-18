library(ISLR)
library(dplyr)
library(tree)
library(gbm)
library(randomForest)

# question 1
data("OJ")
dim(OJ)
OJ_data <- OJ %>%
    mutate(STORE = as.factor(STORE),
            StoreID = as.factor(StoreID))

set.seed(7)
groups = c(rep(1, 800), rep(2, 270))
random_groups = sample(groups, 1070)
in.train = (random_groups == 1)

# question 2
OJ_tree <- tree(Purchase ~ ., data=OJ_data[in.train,])
summary(OJ_tree)
plot(OJ_tree, type = "uniform")
text(OJ_tree, pretty=0)

# question 3
tree_pred <- predict(OJ.tree, newdata = OJ_data[in.train,], type="class")
error.rate <- mean(tree_pred != OJ_data[in.train,]$Purchase)
error.rate
summary(OJ_tree)

# question 4
plot(OJ_tree, type = "uniform")
text(OJ_tree, pretty=0)

#summary(OJ_tree)

# question 5
answer5 <- "The initial split is made based on customer loyalty. This makes sense because a lot of people have a brand they pick up whenever they need xyz product. They may be paying slightly more for that product to begin with. Customer loyalty is the second split, on both sides. It isn't until a certain loyalty threshold is met that other factors come into play, specifically money. It isn't until the price difference reaches a certain point, 17 cents in this case, that loyal Citrus Hill customers switch to Minute Maid. This also makes sense because there is a tipping point in price that will lead loyal customers to look for an alternative."

# question 6
preds = predict(OJ_tree, OJ_data[!in.train,], type = "class")
conf_mat = table(preds, OJ_data[!in.train,]$Purchase)
sum(diag(conf_mat)) / dim(OJ_data[!in.train,])[1]
1 - sum(diag(conf_mat)) / dim(OJ_data[!in.train,])[1]

# question 7
set.seed(7)
OJ_tree <- tree(Purchase ~ ., data=OJ_data[in.train,])
OJ.cv = cv.tree(OJ_tree, FUN = prune.misclass, K = 10)
print(OJ.cv)

min.leaves <- OJ.cv$size[ which(OJ.cv$dev == min(OJ.cv$dev)) ]
min.leaves
plot(OJ.cv)

set.seed(7)
library(tree)
tree_model <- tree(Purchase ~ ., data = OJ_data[in.train,])
cv_results <- cv.tree(tree_model, FUN = prune.misclass, K = 10)
print(cv_results)
optimal_leaves <- cv_results$size[ which(cv_results$dev == min(cv_results$dev)) ]
cat("Optimal number of leaves:", optimal_leaves)
plot(OJ.cv)


set.seed(7)
library(tree)
OJ_tree <- tree(Purchase ~ ., data=OJ_data[in.train,])
OJ.cv <- cv.tree(OJ_tree, FUN = prune.misclass, K = 10)
print(cv_results)
optimal_leaves <- cv_results$size[ which(cv_results$dev == min(cv_results$dev)) ]
cat("Optimal number of leaves:", optimal_leaves)





#### prof's code

set.seed(728)
sonar.cv = cv.tree(my_tree, FUN = prune.misclass)
  # for regression, use prune.tree
sonar.cv

plot(sonar.cv)

## Extracting the optimal number of leaves
min(sonar.cv$dev)
which(sonar.cv$dev == min(sonar.cv$dev))
sonar.cv$size[ which(sonar.cv$dev == min(sonar.cv$dev)) ]



# question 8
prune_OJ = prune.misclass(OJ_tree, best = 9)
plot(prune_OJ)
text(prune_OJ, pretty = 0)
prune_OJ
summary(prune_OJ)

# question 9
data("Hitters")
Hitters <- na.omit(Hitters)
Hitters <- Hitters %>%
    mutate(log_Salary = log(Salary)) %>%
    select(-Salary)

# question 10
Hitters.boost = gbm(log_Salary~., data=Hitters, distribution = "gaussian", 
                    n.trees=5000, shrinkage = .001, interaction.depth=4)
Hitters.boost
summary(Hitters.boost)

# question 11
# boosted trees
set.seed(7)
n = dim(Hitters)[1]
k = 10
groups = rep(1:k, length = n)
cvgroups = sample(groups, n)
boost.predict = rep(0, n)
data.used = Hitters

for(ii in 1:k) {
    groupi = (cvgroups == ii)
    boost = gbm(log_Salary~., data=data.used[!groupi,], distribution = "gaussian", 
                n.trees=5000, shrinkage = .001, interaction.depth=4)
    boost.predict[groupi] = predict(boost, newdata=data.used[groupi, ],
                                    n.trees=5000, type="response")
}
MSE_boost = mean((Hitters$log_Salary - boost.predict)^2) # 0.221223

# linear regression
set.seed(7)
n = dim(Hitters)[1]
k = 10
groups = rep(1:k, length = n)
cvgroups = sample(groups, n)
lin.predict = rep(0, n)
data.used = Hitters

for(ii in 1:k) {
    groupi = (cvgroups == ii)
    lin = lm(log_Salary~., data=data.used[!groupi,])
    lin.predict[groupi] = predict(lin, newdata=data.used[groupi, ])
}
MSE_lm = mean((Hitters$log_Salary - lin.predict)^2) # 0.4358012

# question 12
MSE_boost

# question 13
MSE_lm

# question 14
MSE_boost

# question 15
answer15 = "The Hitters data set has many highly correlated variables. Using random forests can reduce the impact of the correlation by randomly choosing the available variables for decision-making at a given split. This will increase the differentiation of the tree models and leads to a reduced prediction correlation."

# question 16
Hitters_bag = randomForest(log_Salary~., data=Hitters, mtry=19, importance = TRUE)
Hitters_bag
preds = predict(Hitters_bag, newdata = Hitters, type = "response")
hitters_conf_mat = table(preds, Hitters$log_Salary)
sum(diag(conf_mat)) / dim(Hitters)[1]
1 - sum(diag(conf_mat)) / dim(Hitters)[1]
summary(preds)
hitters_conf_mat


plot(Hitters_bag)

# question 17
Hitters_rf = randomForest(log_Salary~., data=Hitters, mtry=6, importance = TRUE, 
                          ntree=5000)
Hitters_rf
varImpPlot(Hitters_rf)

# question 18
partialPlot(Hitters_rf, Hitters, x.var = "Years", plot = TRUE)
library(corrplot)
library(RColorBrewer)
library(car)
library(pROC)

Hitters.numeric <- select_if(Hitters, is.numeric)
correlations <- cor(Hitters.numeric, use = "pairwise.complete.obs")
corrplot(correlations, type = "upper", order = "hclust", 
         col = rev(brewer.pal(n = 8 , name = "RdYlBu")))

# question 19
answer19 = "The value add of Years picks up very quickly, at about 3.5 years. This rise continues until about 7 years. It holds, with a slight decrease, until about 9 years. After that, it falls off dramatically until it levels off around Years = 19."

### answers
#1 see code
#2 LoyalCH, PriceDiff, SalePriceCH, StoreID
#3 0.1625
#4 see code
#5 answer5
#6 1926 
#7 9, 4
#8 incorrect (0.1644)
#9 see code
#10 CAtBat
#11 see code
#12 0.2212
#13 0.4358
#14 Boosting
#15 answer15
#16 17.11 (76.47)
#17 Years
#18 see code
#19 answer19