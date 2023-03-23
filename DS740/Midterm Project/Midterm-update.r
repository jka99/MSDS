# Set path
setwd("C:/Users/jeffe/Documents/MSDS/GitHub/MSDS/DS740/Midterm Project")


# Read in libraries
library(dplyr)
library(glmnet)
library(caret)
library(MASS)
library(corrplot)
library(RColorBrewer)
library(ggformula)

# Set options
options(scipen = 999)

# Read in data
cars04 <- read.csv("04cars.csv")

# Clean the data
cars04 <- cars04 %>%
  dplyr::select(-c(Length, Height)) # to preserve Pickup Type

cars04 <- na.omit(cars04)

# Transform the data
cars04 <- cars04 %>% 
  mutate(
    AWD = if_else(Type == "AWD", 1, 0),
    RWD = if_else(Type == "RWD", 1, 0),
    Sport = as.factor(Sport),
    SUV = as.factor(SUV),
    Wagon = as.factor(Wagon),
    Minivan = as.factor(Minivan),
    Pickup = as.factor(Pickup),
    logRetailPrice = log(Retailprice), logHorsePower = log(Horsepower),
    logCityMPG = log(CityMPG), logHwyMPG = log(HwyMPG)) %>%
  mutate(Type = case_when(Sport == 1 ~ "Sport",
                          SUV == 1 ~ "SUV",
                          Wagon == 1 ~ "Wagon",
                          Minivan == 1 ~ "Minivan",
                          Pickup == 1 ~ "Pickup",
                          TRUE ~ "Other")) %>%
  dplyr::select(Retailprice, logRetailPrice, Engine, Cylinders, Horsepower, 
                logHorsePower, CityMPG, logCityMPG, HwyMPG, logHwyMPG, 
                Weight, Wheelbase, Type, AWD, RWD)

full.model.data <- cars04 %>%
  dplyr::select(Retailprice, logRetailPrice, Engine, Cylinders, Horsepower, 
                logHorsePower, CityMPG, logCityMPG, HwyMPG, logHwyMPG, 
                Weight, Wheelbase, Type, AWD, RWD)

### Data Assessment ###
# Check Normality, Transform if needed
#1
par(mfrow=c(2,2))
hist(cars04$Retailprice)
qqnorm(cars04$Retailprice); qqline(cars04$Retailprice)
#2
#par(mfrow=c(2,1))
hist(log(cars04$Retailprice)) # like this one better
qqnorm(log(cars04$Retailprice)); qqline(log(cars04$Retailprice))
#3
#par(mfrow=c(2,1))
hist(cars04$Engine)
qqnorm(cars04$Engine); qqline(cars04$Engine)
#4
#par(mfrow=c(2,1))
hist(cars04$Horsepower)
qqnorm(cars04$Horsepower); qqline(cars04$Horsepower)
#5
#par(mfrow=c(2,1))
hist(log(cars04$Horsepower)) # log transform for horsepower is better
qqnorm(log(cars04$Horsepower)); qqline(log(cars04$Horsepower))
#6
#par(mfrow=c(2,1))
hist(cars04$CityMPG)
qqnorm(cars04$CityMPG); qqline(cars04$CityMPG)

#7
hist(log(cars04$CityMPG))
qqnorm(log(cars04$CityMPG)); qqline(log(cars04$CityMPG))

#8
#par(mfrow=c(2,1))
hist(cars04$HwyMPG)
qqnorm(cars04$HwyMPG); qqline(cars04$HwyMPG)

#9
hist(log(cars04$HwyMPG))
qqnorm(log(cars04$HwyMPG)); qqline(log(cars04$HwyMPG))

#10
#par(mfrow=c(2,1))
hist(cars04$Weight)
qqnorm(cars04$Weight); qqline(cars04$Weight)

#11
#par(mfrow=c(2,1))
hist(cars04$Wheelbase)
qqnorm(cars04$Wheelbase); qqline(cars04$Wheelbase)

# Check correlations
par(mfrow = c(1,1))
cars04cor <- cor(select_if(cars04, is.numeric), use = "pairwise.complete.obs")
corrplot(cars04cor, type = "upper", order = "hclust", 
         col = rev(brewer.pal(n = 8, name = "RdYlBu")))

# Check for interactions
ggplot(data = cars04, aes(x = Horsepower, y = Retailprice, color = Type)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  labs(title = "Retailprice as a function of Horsepower")

ggplot(data = cars04, aes(x = Cylinders, y = Retailprice, color = Type)) +
  geom_point()

ggplot(data = cars04, aes(x = Type, y = Retailprice, fill = Type)) +
  geom_jitter() +
  labs(title = "Retailprice density as a function of Type")
  
# ### Feature Selection ###
# 
# 
# lambdalist = seq(0.001, 1, length = 100)
# alphalist = seq(0.1, 1, length = 100)
# model.data <- cars04 %>%
#   dplyr::select(logRetailPrice, Cylinders, logHorsePower, Type, AWD, RWD)
# 
# ENET.model <- (logRetailPrice ~ .)
# fit_caret_ENET = train(ENET.model, data = model.data,
#                        method = "glmnet", trControl = training,
#                        tuneGrid = expand.grid(alpha = alphalist, 
#                                               lambda = lambdalist),
#                        na.action = na.omit)
#fit_caret_ENET
#coef(fit_caret_ENET$finalModel, s = fit_caret_ENET$bestTune$lambda)


### Results ###
# Based on repeated runs of the ENET on a set seed, removing useless 
# predictors on each run, I can trim the model 
# to logRetailPrice, Cylinders, logHorsePower, Type, AWD, and RWD

##### model assessment OUTER 5-fold CV #####
##### (with model selection INNER 10-fold CV as part of model-fitting) #####


carsmatrix = model.matrix(logRetailPrice ~., data = model.data)
fulldata = data.frame(logRetailPrice = model.data$logRetailPrice, carsmatrix)
n = dim(fulldata)[1]
lambdalist = seq(0.001, 1, length = 100)
alphalist = seq(0.01, 1, length = 100)

# define the cross-validation splits 
nfolds = 5 
groups.out = rep(1:nfolds,length = n) 
set.seed(8)
cvgroups.out = sample(groups.out, n)  
allpredictedCV.out = rep(NA, n)

# define the models
ENET.model = (logRetailPrice ~ .)
RR.model = (logRetailPrice ~ .)

# define the place holders
allpred.CV = rep(NA,n)
allpred.Method = rep(NA,n)
allpred.RMSE = rep(NA,n)

all_best_RMSE = rep(NA,nfolds)
all_best_Types = rep(NA,nfolds)
all_best_Pars = vector("list",nfolds)
all_best_Model = vector("list",nfolds)

### model assessment OUTER shell ###
for (ii in 1:nfolds)  {  
  groupii = (cvgroups.out == ii)
  
  # define the training set for outer loop
  train = fulldata[!groupii,]
  dummyTrain = dummyVars(" ~. ", data = train)
  trainx = model.matrix(logRetailPrice ~ ., data = train)[,-1]
  #dummTrainX = dummyVars(" ~. ", data = trainx)
  trainy = train$logRetailPrice
  dummyTrainY = dummyVars(" ~. ", data = trainy)

  
  #define the validation set for outer loop
  test = fulldata[groupii,]
  dummyTest = dummyVars(" ~. ", data = test)
  testx = model.matrix(logRetailPrice ~ ., data = test)[,-1]
  #dummyTestX = dummyVars(" ~. ", data = testx)

  # training controls for ENET
  training = trainControl(method = "cv", number = 10)
  
  dataused = train
  
  
  # model selection
  # cross-validation of ENET model
  fit_caret_ENET = train(ENET.model,
                              data = dataused,   
                              method = "glmnet",
                              trControl = training,
                              tuneGrid = expand.grid(alpha = alphalist, 
                                                     lambda = lambdalist))
  
  # fit_caret_ENET.full = train(ENET.model,
  #                        data = dataused.full,   
  #                        method = "glmnet",
  #                        trControl = training,
  #                        tuneGrid = expand.grid(alpha = alphalist, 
  #                                               lambda = lambdalist))  
  # 
  # cross-validation of RR model
  fit_caret_RR = train(RR.model,
                       data = dataused,   
                       method = "rlm",
                       maxit = 50,
                       trControl = training)
  
  # fit_caret_RR.full = train(RR.model,
  #                      data = dataused.full,   
  #                      method = "rlm",
  #                      trControl = training)
  # 
  # All best models
  all_best_Types = c("ENET","RR")
                     
  all_best_Pars = list(fit_caret_ENET$bestTune, fit_caret_RR$bestTune)
  
  all_best_Models = list(glmnet(trainx, trainy, 
                                alpha=fit_caret_ENET$bestTune$alpha, 
                                lambda=lambdalist),
                                fit_caret_RR$finalModel)
  
  all_best_RMSE = c(min(fit_caret_ENET$results$RMSE),
                    min(fit_caret_RR$results$RMSE))
  
  # Best model, each fold
  one_best_Type = all_best_Types[which.min(all_best_RMSE)]
  one_best_Pars = all_best_Pars[which.min(all_best_RMSE)]
  one_best_Model = all_best_Models[[which.min(all_best_RMSE)]]
  one_best_RSME = all_best_RMSE[[which.min(all_best_RMSE)]]
  
  allpred.Method[ii] = one_best_Type
  allpred.RMSE[ii] = one_best_RSME
  
  if(one_best_Type == "ENET"){
    ENETlambda = one_best_Pars[[1]]$lambda
    allpred.CV[groupii] = predict(one_best_Model, newx = testx, s = ENETlambda)
  } else if(one_best_Type == "RR"){
    RRlambda = one_best_Pars[[1]]$lambda
    allpred.CV[groupii] = predict(one_best_Model, newdata = test)
  }

  ############# END ##############
  
  
}
coef(fit_caret_RR$finalModel, s = fit_caret_RR$bestTune$lambda)
one_best_Type
one_best_Pars
one_best_RSME

y <- cars04$logRetailprice

# Actual VS Predicted Retail Price plot
plot(exp(y)~exp(allpred.CV))


