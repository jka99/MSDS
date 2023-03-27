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
    logCityMPG = log(CityMPG), logHwyMPG = log(HwyMPG),
    logWeight = log(Weight)) %>%
  mutate(Type = case_when(Sport == 1 ~ "Sport",
                          SUV == 1 ~ "SUV",
                          Wagon == 1 ~ "Wagon",
                          Minivan == 1 ~ "Minivan",
                          Pickup == 1 ~ "Pickup",
                          TRUE ~ "Other")) %>%
  dplyr::select(Retailprice, logRetailPrice, Engine, Cylinders, Horsepower, 
                logHorsePower, CityMPG, logCityMPG, HwyMPG, logHwyMPG, 
                Weight, logWeight, Wheelbase, Type, AWD, RWD)

full.model.data <- cars04 %>%
  dplyr::select(Retailprice, logRetailPrice, Engine, Cylinders, Horsepower, 
                logHorsePower, CityMPG, logCityMPG, HwyMPG, logHwyMPG, 
                logWeight, Wheelbase, Type, AWD, RWD)

### Data Assessment ###
# Check Normality, Transform if needed
#1
par(mfrow=c(4,2))
hist(cars04$Retailprice)
qqnorm(cars04$Retailprice); qqline(cars04$Retailprice, col = "red")
#2
#par(mfrow=c(2,1))
hist(log(cars04$Retailprice)) # like this one better
qqnorm(log(cars04$Retailprice)); qqline(log(cars04$Retailprice), col = "red")

#4
#par(mfrow=c(2,1))
hist(cars04$Horsepower)
qqnorm(cars04$Horsepower); qqline(cars04$Horsepower, col = "red")
#5
#par(mfrow=c(2,1))
hist(log(cars04$Horsepower)) # log transform for horsepower is better
qqnorm(log(cars04$Horsepower)); qqline(log(cars04$Horsepower), col = "red")

#3
#par(mfrow=c(2,1))
hist(cars04$Engine)
qqnorm(cars04$Engine); qqline(cars04$Engine, col = "red")

#6
#par(mfrow=c(2,1))
hist(cars04$CityMPG)
qqnorm(cars04$CityMPG); qqline(cars04$CityMPG, col = "red")

#7
hist(log(cars04$CityMPG))
qqnorm(log(cars04$CityMPG)); qqline(log(cars04$CityMPG), col = "red")

#8
#par(mfrow=c(2,1))
hist(cars04$HwyMPG)
qqnorm(cars04$HwyMPG); qqline(cars04$HwyMPG, col = "red")

#9
hist(log(cars04$HwyMPG))
qqnorm(log(cars04$HwyMPG)); qqline(log(cars04$HwyMPG), col = "red")

#10
#par(mfrow=c(2,1))
hist(cars04$Weight)
qqnorm(cars04$Weight); qqline(cars04$Weight, col = "red")

hist(cars04$logWeight)
qqnorm(cars04$logWeight); qqline(cars04$logWeight, col = "red")

#11
#par(mfrow=c(2,1))
hist(cars04$Wheelbase)
qqnorm(cars04$Wheelbase); qqline(cars04$Wheelbase, col = "red")

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

ggplot(data = cars04, aes(x = Type, y = Retailprice, color = Type)) +
  geom_jitter() +
  labs(title = "Retailprice density as a function of Type")

# ### Feature Selection ###

set.seed(9)
# lambdalist = seq(0.001, 1, length = 100)
# alphalist = seq(0.1, 1, length = 100)
# model.data <- cars04 %>%
#   dplyr::select(logRetailPrice, Cylinders, logHorsePower, 
#                 logWeight, Type, AWD, RWD)
# 
# ENET.model <- (logRetailPrice ~ .)
# fit_caret_ENET = train(ENET.model, data = model.data,
#                        method = "glmnet", trControl = training,
#                        tuneGrid = expand.grid(alpha = alphalist,
#                                               lambda = lambdalist),
#                        na.action = na.omit)
# fit_caret_ENET
# coef(fit_caret_ENET$finalModel, s = fit_caret_ENET$bestTune$lambda)


### Results ###
# Based on repeated runs of the ENET on a set seed, removing useless 
# predictors on each run, I can trim the model 
# to logRetailPrice, Cylinders, logHorsePower, Type, AWD, and RWD

##### model assessment OUTER 5-fold CV #####
##### (with model selection INNER 10-fold CV as part of model-fitting) #####

model.data <- cars04 %>%
  dplyr::select(logRetailPrice, Engine, Cylinders, logHorsePower, 
                logWeight, Type, AWD, RWD)

carsmatrix = model.matrix(logRetailPrice ~., data = model.data)
fulldata = data.frame(logRetailPrice = model.data$logRetailPrice, carsmatrix)
n = dim(fulldata)[1]
lambdalist = seq(0.001, 1, length = 100)
alphalist = seq(0.01, 1, length = 100)

# define the cross-validation splits 
nfolds = 5 
groups.out = rep(1:nfolds,length = n) 
set.seed(99)
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

resamples = list("list")

### model assessment OUTER shell ###
for (ii in 1:nfolds)  {  
  groupii = (cvgroups.out == ii)
  
  # define the training set for outer loop
  train = fulldata[!groupii,]
  trainx = model.matrix(logRetailPrice ~ ., data = train)[,-1]
  trainy = train$logRetailPrice


  
  #define the validation set for outer loop
  test = fulldata[groupii,]
  testx = model.matrix(logRetailPrice ~ ., data = test)[,-1]


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

  # cross-validation of RR model
  fit_caret_RR = train(RR.model,
                       data = dataused,   
                       method = "rlm",
                       maxit = 50,
                       trControl = training)

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
  
  resample <- postResample(pred = allpred.CV[groupii], 
                           obs = test$logRetailPrice)
print(paste0("The RMSE for loop ", ii, " was : ", resample[1]))
print(paste0("The R^2 for loop ", ii, " was : ", resample[2]))
print(paste0("The MAE for loop ", ii, " was : ", resample[3]))


}
coef_enet <- coef(one_best_Model, s = one_best_Pars[[1]]$lambda)
print(coef_enet)
one_best_Type
one_best_Pars
one_best_RSME

# build a data frame for visuals
val = exp(cars04$logRetailPrice)
pred = exp(allpred.CV)
hp <- cars04$Horsepower
weight = cars04$Weight
compare <- data.frame(val, pred, hp, weight)

# make charts to display results
ggplot(data = compare, aes(x = pred, y = val, color = type)) +
  geom_point() +
  geom_smooth(color = "blue", size = 1.5, se = FALSE) +
  geom_abline(slope = 1, aes(color = "green"), size = 1.5) +
  labs(x = "Predicted Retail Price", y = "True Retail Price", 
       title = "Actual vs Predicted Reatail Price") 


ggplot(data = compare, aes(x = weight, y = pred, color = type)) +
  geom_point() +
  labs(x = "Weight", y = "Predicated Retailprice", 
       title = "Predicted Retail Price and Weight") 

ggplot(data = compare, aes(x = hp, y = pred, color = type)) +
  geom_point() +
  labs(title = "Predicted Retail Price and Horsepower", x = "Horsepower",
       y = "Predictecountd Retail Price")

