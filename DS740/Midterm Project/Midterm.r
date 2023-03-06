### DS740 Midterm Project
### Rubust Regression and Elastic Net
### of 04cars Dataset

# load the packages
library(dplyr)



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
par(mfrow=c(2,1))
hist(cars04$Retailprice)
qqnorm(cars04$Retailprice); qqline(cars04$Retailprice)

par(mfrow=c(2,1))
hist(log(cars04$Retailprice))
qqnorm(log(cars04$Retailprice)); qqline(log(cars04$Retailprice))

par(mfrow=c(2,1))
hist(cars04$Engine)
qqnorm(cars04$Engine); qqline(cars04$Engine)

par(mfrow=c(2,1))
hist(cars04$Horsepower)
qqnorm(cars04$Horsepower); qqline(cars04$Horsepower)

par(mfrow=c(2,1))
hist(log(cars04$Horsepower)) # log transform for horsepower is better
qqnorm(log(cars04$Horsepower)); qqline(log(cars04$Horsepower))

par(mfrow=c(2,1))
hist(cars04$CityMPG)
qqnorm(cars04$CityMPG); qqline(cars04$CityMPG)

par(mfrow=c(2,1))
hist(cars04$HwyMPG)
qqnorm(cars04$HwyMPG); qqline(cars04$HwyMPG)

par(mfrow=c(2,1))
hist(cars04$Weight)
qqnorm(cars04$Weight); qqline(cars04$Weight)

par(mfrow=c(2,1))
hist(cars04$Wheelbase)
qqnorm(cars04$Wheelbase); qqline(cars04$Wheelbase)

par(mfrow=c(2,1))
hist(cars04$Length)
qqnorm(cars04$Length); qqline(cars04$Length)

par(mfrow=c(2,1))
hist(cars04$Height)
qqnorm(cars04$Height); qqline(cars04$Height)


