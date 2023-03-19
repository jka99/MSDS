setwd("C:/Users/jeffe/Documents/MSDS/GitHub/MSDS/DS740/Midterm Project")
### DS740 Midterm Project
### Rubust Regression and Elastic Net
### of 04cars Dataset

# load the packages
library(dplyr)
library(glmnet)
library(MASS)
library(ggformula)

# load the data
cars04 <- read.csv("04cars.csv")
head(cars04)

# clean the data
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
        Height = as.numeric(Height)) %>%
    dplyr::select(-Type)

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

hist(log(cars04$CityMPG))
qqnorm(log(cars04$CityMPG)); qqline(log(cars04$CityMPG))
#7
#par(mfrow=c(2,1))
hist(cars04$HwyMPG)
qqnorm(cars04$HwyMPG); qqline(cars04$HwyMPG)

hist(log(cars04$HwyMPG))
qqnorm(log(cars04$HwyMPG)); qqline(log(cars04$HwyMPG))
#8
#par(mfrow=c(2,1))
hist(cars04$Weight)
qqnorm(cars04$Weight); qqline(cars04$Weight)
#9
#par(mfrow=c(2,1))
hist(cars04$Wheelbase)
qqnorm(cars04$Wheelbase); qqline(cars04$Wheelbase)
#10
#par(mfrow=c(2,1))
hist(cars04$Length)
qqnorm(cars04$Length); qqline(cars04$Length)
#11
#par(mfrow=c(2,1))
hist(cars04$Height)
qqnorm(cars04$Height); qqline(cars04$Height)

#Use caret to conduct a single layer of 10-fold cross validation on both of the 
#modeling types indicated for the data set you chose. Tune at least one 
#parameter for at least one of the modeling types. (Tuning multiple parameters 
#may help improve your model’s performance.)

######## ENET #########
ENET.data <- cars04 %>%
  mutate(logRetailPrice = log(Retailprice), logHorsePower = log(Horsepower),
         logCityMPG = log(CityMPG), logHwyMPG = log(HwyMPG),
         Type = ifelse(Sport==1, "Sport", 
                       ifelse(SUV==1, "SUV", 
                              ifelse(Wagon==1, "Wagon", 
                                     ifelse(Minivan==1, "Minivan",
                                            ifelse(Pickup==1, "Pickup", "Other")))))) %>%
  dplyr::select(logRetailPrice, Engine, Cylinders, logHorsePower, logCityMPG, logHwyMPG,
                Weight, Wheelbase, Length, Height, Type)
set.seed(99)
lambdalist = seq(0.001, 1, length = 1000)
alphalist = seq(0.01, 1, by = 0.01)
n = dim(ENET.data)[1]
nfolds = 5

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