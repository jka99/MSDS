setwd("C:/Users/jeffe/Documents/MSDS/GitHub/MSDS/DS740/Final Project")

# Initiate Packages
library(dplyr)
library(corrplot)
library(RColorBrewer)
library(ggplot2)

# Read in data
air.travel.1 <- read.csv("air_travel_test.csv")
air.travel.2 <- read.csv("air_travel_train.csv")

air.travel <- rbind(air.travel.1,air.travel.2)

