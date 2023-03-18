################ data prep / model specifications ##############
Trees = read.csv("TreesTransformed.csv")
library(glmnet)

x = model.matrix(Volume~.,data=Trees)[,-1]
y = Trees[,1]
n = dim(x)[1]
lambdalist = exp((-1000:500)/100)
alphalist = c(0,.1,.2,.4,.6,.8,.9,1); n.alpha = length(alphalist)
################ Step 1. ##############
# split into cvgroups
nfolds = 5
groups = rep(1:nfolds,length=n)
set.seed(8)
cvgroups = sample(groups,n)

# run cross-validation for model selection 
alllambdabest = rep(NA,n.alpha)
allcv5best = rep(NA,n.alpha)
for (which.alpha in 1:n.alpha) {
  cvfit = cv.glmnet(x, y, lambda=lambdalist, alpha = alphalist[which.alpha], 
                    nfolds=nfolds, foldid=cvgroups)
  plot(cvfit$lambda, cvfit$cvm, xlim=c(0,2),ylim=c(0,15) ) 
  abline(v=cvfit$lambda[order(cvfit$cvm)[1]],col = "red")
  allcv5best[which.alpha] = cvfit$cvm[order(cvfit$cvm)[1]]
  alllambdabest[which.alpha] = cvfit$lambda[order(cvfit$cvm)[1]]
}
whichmodel = order(allcv5best)[1]
################ Step 2. ##############
bestalpha = alphalist[whichmodel]
bestlambda = alllambdabest[whichmodel]
bestmodel = glmnet(x, y, alpha = bestalpha,lambda=lambdalist)
############################################################
library(caret)
dataused = Trees
set.seed(8)
training = trainControl(method = "cv", number = 5)
fit_caret_ENET = train(Volume~.,
                        data = dataused,
                        method = "glmnet",
                        trControl = training,
                        tuneGrid = expand.grid(alpha=alphalist,lambda=lambdalist))
fit_caret_ENET$results
min(fit_caret_ENET$results$RMSE)
best_RMSE = which.min(fit_caret_ENET$results$RMSE)
best_alpha = fit_caret_ENET$results$alpha[best_RMSE]
best_lambda = fit_caret_ENET$results$lambda[best_RMSE]

glmnet()