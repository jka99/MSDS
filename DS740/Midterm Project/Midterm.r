setwd("C:/Users/jeffe/Documents/MSDS/GitHub/MSDS/DS740/Midterm Project")
### DS740 Midterm Project
### Rubust Regression and Elastic Net
### of 04cars Dataset

# load the packages
library(dplyr)
library(glmnet)
library(caret)
library(MASS)
library(ggformula)

# load the data
cars04 <- read.csv("04cars.csv")

# clean the data
cars04 <- cars04 %>%
  dplyr::select(-c(Length, Height)) # to preserve Pickup Type

cars04 <- na.omit(cars04)

#transform the data
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
        Type = case_when(Sport==1 ~ "Sport", SUV==1 ~ "SUV", 
                         Wagon==1 ~ "Wagon", Minivan==1 ~ "Minivan",
                         Pickup==1 ~ "Pickup", TRUE ~ "Other")) %>%
  dplyr::select(Retailprice, logRetailPrice, Engine, Cylinders, Horsepower, 
                logHorsePower, CityMPG, logCityMPG, HwyMPG, logHwyMPG, 
                Weight, Wheelbase, Type, AWD, RWD)

#######################################
# check the data
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


#Use caret to conduct a single layer of 10-fold cross validation on both of the 
#modeling types indicated for the data set you chose. Tune at least one 
#parameter for at least one of the modeling types. (Tuning multiple parameters 
#may help improve your model’s performance.)

######## ENET #########
### Feature Selection ###
options(scipen = 999)
set.seed(99)
lambdalist = seq(0.001, 1, length = 100)
alphalist = seq(0.1, 1, length = 10)
ENET.data <- cars04 %>%
  dplyr::select(logRetailPrice, Cylinders, logHorsePower,Type, AWD, RWD)
ENET.model <- (logRetailPrice ~ .)
fit_caret_ENET = train(ENET.model, data = ENET.data,
                       method = "glmnet", trControl = training,
                       tuneGrid = expand.grid(alpha = alphalist, 
                                              lambda = lambdalist),
                       na.action = na.omit)
fit_caret_ENET
coef(fit_caret_ENET$finalModel, s = fit_caret_ENET$bestTune$lambda)
### Results ###
# Based on repeated runs of the ENET on a set seed, removing useless 
# predictors on each run, I can trim the model 
# to logRetailPrice, Cylinders, logHorsePower, Type, AWD, and RWD
#
# The coef for TypeOther, is also zero, but removing it will create a 
# ton of NAs. I will leave in for ease of model design
# I will use this same model for RR
# I will also use a full model for both to see how it compares to the 
# trimmed model

######
ENET.data <- cars04 %>%
  dplyr::select(logRetailPrice, Cylinders, logHorsePower,Type, AWD, RWD)

dmy <- dummyVars(logRetailPrice ~ ., data = ENET.data)

set.seed(100)
n = dim(ENET.data)[1]
lambdalist = seq(0.001, 1, length = 10)
alphalist = seq(0.01, 1, length = 10)
nfolds = 5

groups = rep(1:nfolds, length = n)
cvgroups.outer = sample(groups, n)

allpred.cv = rep(NA, n)
allpredType = rep(NA, 0)
allpredRMSE = rep(NA, 0)

ENET.model <- (logRetailPrice ~ .)
all_best_Types = rep(NA, 0)
all_best_Models = rep(NA, 0)
all_best_Pars = vector(mode = "list", nfolds)
all_best_RMSE = rep(NA, nfolds)

for (ii in 1:nfolds){
  groupii = (cvgroups.outer == ii)
  
  train.outer = ENET.data[!groupii,]
  test.outer = ENET.data[groupii,]
  ENET.model <- (logRetailPrice ~ .)
  
  dataused = train.outer
  training = trainControl(method = "cv", number = 5)
  
  fit_caret_ENET = train(ENET.model, data = train.outer,
                         method = "glmnet", trControl = training,
                         tuneGrid = expand.grid(alpha = alphalist, 
                                                lambda = lambdalist),
                         na.action = na.omit)

  all_best_Types_select = c("ENET", "RR")
  all_best_Pars_select = list(fit_caret_ENET$bestTune, fit_caret_RR$bestTune)
  all_best_Models_select = list(glmnet(train.x, train.y, alpha = fit_caret_ENET$bestTune$alpha,
                                       lambda = lambdalist), fit_caret_RR$finalModel)
  all_best_RMSE_select = c(min(fit_caret_ENET$result$RMSE), 
                           min(fit_caret_RR$results$RMSE))
  
  one_best_Type = all_best_Types_select[which.min(all_best_RMSE_select)]
  one_best_Pars = all_best_Pars_select[which.min(all_best_RMSE_select)]
  one_best_Model = all_best_Models_select[which.min(all_best_RMSE_select)]
  one_best_RMSE = all_best_RMSE_select[which.min(all_best_RMSE_select)]
  
  allpredType[ii] = one_best_Type
  allpredRMSE[ii] = one_best_RMSE
  

}
coef(fit_caret_ENET$finalModel, s = fit_caret_ENET$bestTune$lambda)
######## Robust Regression ##########
RR.data <- cars04 %>%
  mutate(logRetailPrice = log(Retailprice), logHorsePower = log(Horsepower),
         logCityMPG = log(CityMPG), logHwyMPG = log(HwyMPG),
         Type = ifelse(Sport==1, "Sport", 
                       ifelse(SUV==1, "SUV", 
                              ifelse(Wagon==1, "Wagon", 
                                     ifelse(Minivan==1, "Minivan",
                                            ifelse(Pickup==1, "Pickup", "Other")))))) %>%
  dplyr::select(logRetailPrice, Engine, Cylinders, logHorsePower, logCityMPG, logHwyMPG,
         Weight, Wheelbase, Length, Height, Type)

RR.fit = lm(logRetailPrice ~ ., data = RR.data)

bisquare = rlm(logRetailPrice ~ ., data = RR.data, psi = psi.bisquare)
summary(bisquare)

## based on the results of the summary(bisquare), logCityMPG, logHwyMPG,
## and Wheelbase are not significant and will be removed from the model

RR.data <- RR.data %>%
  dplyr::select(logRetailPrice, Engine, Cylinders, logHorsePower,
                Weight, Length, Height, Type)

RR.fit = lm(logRetailPrice ~ ., data = RR.data)

bisquare = rlm(logRetailPrice ~ ., data = RR.data, psi = psi.bisquare)
summary(bisquare)