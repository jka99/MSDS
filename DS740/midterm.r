library(dplyr)
library(caret)
library(glmnet)
library(corrplot)
library(RColorBrewer)
library(car) # for VIF
library(leaps) 
library(doParallel)

## Clean up, transform, predictor selection
myAuto = read.csv("04cars.csv")
myAuto <- myAuto %>% select(-c(Height,Length))
myAuto <- na.omit(myAuto)
myAuto <- myAuto %>%
  mutate(logRetailprice = log(Retailprice),
         logCylinders = log(Cylinders),
         TypeAWD = if_else(Type == "AWD", 1, 0),
         TypeRWD = if_else(Type == "RWD", 1, 0),
         logHorsepower = log(Horsepower)) %>% 
         select(-c(Type,Weight,CityMPG,HwyMPG,Retailprice,Cylinders,Horsepower))
head(myAuto)

## Check correlation
mycars_numeric = select_if(myAuto, is.numeric)
mycars_correlations <- cor(mycars_numeric, 
                    use = "pairwise.complete.obs")
corrplot(mycars_correlations, 
         type = "upper", order = "hclust", 
         col = rev(brewer.pal(n = 8, name = "RdYlBu")))

## Recursive Feature Elimination
set.seed(8)
control_rfe <- rfeControl(functions = lmFuncs,
                   method = "repeatedcv",
                   repeats = 5,
                   number = 5, #cv number
                   verbose = FALSE)

Model.rfe = (logRetailprice ~ .)
X_train = model.matrix(Model.rfe, data = myAuto)[,-1]
y_train = myAuto$logRetailprice
result_rfe = rfe(x = X_train, 
                 y = y_train, 
                 sizes = c(1:dim(myAuto)[2]-1),
                 rfeControl = control_rfe)

plot(result_rfe, type = c("g", "o"))
cbind(result_rfe$results$Variables,result_rfe$results$RMSE,result_rfe$results$Rsquared)
result_rfe$optVariables

## Check on VIF
testvif = lm(logRetailprice ~ ., data = myAuto)
vif(testvif)
plot(testvif) 

## Peeking on regsubset
regfit.full = regsubsets(logRetailprice ~ ., data = myAuto,
                         method = "exhaustive", nvmax = dim(myAuto)-1)

regfit.full.summary = summary(regfit.full)
regfit.full.summary$bic
plot(regfit.full.summary$bic, xlab = "Number of variables",
     ylab = "BIC", type = "l", lwd = 2)

which.min(regfit.full.summary$bic)
coef(regfit.full, which.min(regfit.full.summary$bic))
cls = makeCluster(detectCores()-1)
registerDoParallel(cls)

set.seed(8)
#tuning params for ENET
lambdalist = c((1:1000)/10000)

n = dim(myAuto)[1]
nfolds = 5
groups = rep(1:nfolds,length=n)
cvgroups = sample(groups,n)

# store predicted values from the double-cross-validation
allpredictedCV = rep(NA,n)
allpredictedMethod = rep(NA,n)
allpredictedMSE = rep(NA,n)

# set up storage to see what models are "best" on the inner loops
allbestRMSE = rep(NA,nfolds)
allbestTypes = rep(NA,nfolds)
allbestPars = vector("list",nfolds)
Model.Full = (logRetailprice ~ .)
Model.Penalized = (logRetailprice ~ .)

timing <- system.time({
for (j in 1:nfolds)  {     # loop through outer splits
  groupj = (cvgroups == j)
  
  # train data
  traindata = myAuto[!groupj,]
  trainx = model.matrix(Model.Full, data = traindata)[,-1]
  trainy = traindata$logRetailprice
  
  # test data
  validdata = myAuto[groupj,]
  validx = model.matrix(Model.Full, data = validdata)[,-1]
  validy = validdata$logRetailprice
  
  # all model-fitting process with traindata
  dataused=traindata
  
  training = trainControl(method = "cv", number = 5, allowParallel = TRUE)

  # cross-validation of penalized regression
  fit_caret_penalized = train(Model.Penalized,
                        data = dataused,
                        method = "glmnet",
                        trControl = training,
                        tuneGrid = expand.grid(alpha=1,lambda=lambdalist))
  
  all_best_Types = c("ELASTICNET")
  all_best_Pars = list(fit_caret_penalized$bestTune)
  all_best_Models = list(glmnet(trainx, trainy, alpha=fit_caret_penalized$bestTune$alpha, lambda=lambdalist))
  all_best_RMSE = c(min(fit_caret_penalized$results$RMSE))
  
  # the best model
  one_best_Type = all_best_Types[which.min(all_best_RMSE)]
  one_best_Pars = all_best_Pars[which.min(all_best_RMSE)]
  one_best_Model = all_best_Models[[which.min(all_best_RMSE)]]
  one_best_RSME = all_best_RMSE[[which.min(all_best_RMSE)]]
  
  # for checking later to see what's the best from each fold
  allbestTypes[j] = one_best_Type
  allbestPars[[j]] = one_best_Pars
  allbestRMSE[j] = one_best_RSME
  
  allpredictedMethod[groupj] = one_best_Type
  allpredictedMSE[groupj] = one_best_RSME
  
  if (one_best_Type == "ELASTICNET") {
    ENETLAMBDA = one_best_Pars[[1]]$lambda
    allpredictedCV[groupj]  = predict(one_best_Model,newx=validx,s=ENETLAMBDA)
  }
}
})
paste("Elapsed times: ", timing["elapsed"])
stopCluster(cls)

#see the best MSE
one_best_RSME

#see the best params
paste("alpha",one_best_Pars[[1]]$alpha,"lambda",one_best_Pars[[1]]$lambda)

#see the best Model
bestcoef = coef(one_best_Model, s = one_best_Pars[[1]]$lambda)
options(scipen=999)
round(bestcoef,4)

# print individually
for (j in 1:nfolds) {
  writemodel = paste("The best model at loop", j, 
                     "is of type", allbestTypes[j],
                     "with parameter(s)",allbestPars[[j]])
  print(writemodel, quote = FALSE)