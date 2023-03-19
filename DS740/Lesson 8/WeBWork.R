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
fit_caret_ENET$bestTune
############### Step 3. ###################
finallambda = fit_caret_ENET$bestTune$lambda
finalalpha = fit_caret_ENET$bestTune$alpha
finalfit.penalized <- glmnet(x, y, alpha = finalalpha,lambda=lambdalist); finalfit.penalized
finalcoef.penalized <- coef(finalfit.penalized,s=finallambda); finalcoef.penalized
##########################################
############# Problem 4 ##################
################ Step 1. ##############
# set up training method
set.seed(8)
training = trainControl(method = "cv", number = 5)

# cross-validation of penalized regression
dataused = Trees
fit_caret_penalized = train(Volume ~ . ,
                            data = dataused,
                            method = "glmnet",
                            trControl = training,
                            tuneGrid = expand.grid(alpha=alphalist,lambda=lambdalist))
min(fit_caret_penalized$results$RMSE)
plot(fit_caret_penalized$results$RMSE,
     ylim=c(.95*min(fit_caret_penalized$results$RMSE),
            1.2*min(fit_caret_penalized$results$RMSE)))

################ Step 2. (RENAMED) ##############
best_Pars = fit_caret_penalized$bestTune
print(min(fit_caret_penalized$results$RMSE))
################ Step 3. (RENAMED) ##############
best_Model <- glmnet(x, y, alpha = best_Pars$alpha,lambda=lambdalist)
############################################################


##### model assessment OUTER shell #####
# produce loops for 5-fold cross-validation for model ASSESSMENT
nfolds = 5
groups = rep(1:nfolds,length=n)  #produces list of group labels
set.seed(8)
cvgroups = sample(groups,n)  #orders randomly
# set up storage for predicted values from the double-cross-validation
allpredictedCV = rep(NA,n)
# set up storage to see what models are "best" from the inner loops
allbestPars = vector("list",nfolds)
# loop through outer splits
for (j in 1:nfolds)  {  #be careful not to re-use loop indices
  groupj = (cvgroups == j)
  traindata = Trees[!groupj,]
  trainx = model.matrix(Volume ~ ., data = traindata)[,-1]
  trainy = traindata$Volume
  validdata = Trees[groupj,]
  validx = model.matrix(Volume ~ ., data = validdata)[,-1]
  validy = validdata$Volume
  
  #specify data to be used
  dataused=traindata
  fit_caret_penalized = train(Volume ~ . ,
                              data = dataused,
                              method = "glmnet",
                              trControl = training,
                              tuneGrid = expand.grid(alpha=alphalist,lambda=lambdalist))
  best_Pars = fit_caret_penalized$bestTune
  print(min(fit_caret_penalized$results$RMSE))
  best_Model <- glmnet(trainx, trainy, alpha = best_Pars$alpha,lambda=lambdalist)
  # best type must be a penalized regression model
  allbestPars[[j]] = best_Pars
  # only considering penalized regression models, so specified the same
  lambda.out = best_Pars$lambda
  alpha.out = best_Pars$alpha
  allpredictedCV[groupj]  = predict(best_Model,newx=validx,s=lambda.out)
}
R2.assess = 1 - sum((allpredictedCV-y)^2)/sum((y-mean(y))^2); R2.assess
