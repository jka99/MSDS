setwd("C:/Users/jeffe/Documents/MSDS/GitHub/MSDS/DS740/lesson 9")

library(dplyr)
library(caret)
library(kernlab)
library(ggformula)
library(ggplot2)
library(ISLR)

log = read.csv("Oak_log.csv")

### question 1
log <- log %>%
  mutate(scale_logSize = scale(logSize),
         scale_logRange = scale(logRange))

ggplot(data = log, aes(x = scale_logRange, y = scale_logSize, 
                             color = Region)) +
  geom_point() +
  scale_color_discrete(name = "Region",
                       breaks = c("Atlantic", "California"))
                     
# insert plot
answer1 <- "insert plot- ques1"

### question 2
answer2 <- "A support vector classifier should work for this situation. There is a clear line between the two groups where maybe one point will be in the margin at ~(-0.75, -0.75)."

### question 3
ctrl = trainControl(method = "LOOCV")
fit_log = train(Region ~ logSize + logRange,
                 data = log,
                 method = "svmLinear",
                 tuneGrid = expand.grid(C = c(0.001, 0.01, 0.1, 1, 5, 10, 100)),
                 preProcess = c("center","scale"),
                 trControl = ctrl)
fit_log
answer3 <- "paste question 3 code"

### question 4
answer4 <- 1

### question 5
conf_mat = table(predict(fit_log, newdata = log), log$Region)
conf_mat
answer5 <- 2

### question 6
answer6 <- 0

### question 7
log <- log %>%
  tibble::rownames_to_column("Row") %>%
  mutate(is_SV = Row %in% attr(fit_log$finalModel, "SVindex"))

b = attr(fit_log$finalModel, "b")

coefs = attr(fit_log$finalModel, "coef")[[1]]
head(coefs)

log_SV <- log %>%
  filter(is_SV) %>%
  select(c(scale_logRange, scale_logSize)) %>%
  as.matrix()

w = colSums(coefs * log_SV)

log %>%
  gf_point(scale_logSize ~ scale_logRange,
           color =~ Region, pch =~Region, data = log) %>%
  gf_abline(intercept = b/w[2], slope = -w[1]/w[2]) %>%
  gf_abline(intercept = (b+1)/w[2], slope = -w[1]/w[2], lty = 2) %>%
  gf_abline(intercept = (b-1)/w[2], slope = -w[1]/w[2], lty = 2)

##### Unsure where i'm getting an error with these
# ggplot(data = log, aes(x = scale_logRange, y = scale_logSize, after_stat(fill = Range))) +
# geom_point() +
# geom_abline(intercept = b/w[2], slope = -w[1]/w[2]) +
# geom_abline(intercept = (b+1)/w[2], slope = -w[1]/w[2], lty = 2) +
# geom_abline(intercept = (b-1)/w[2], slope = -w[1]/w[2], lty = 2)
# 
# 
# gf_point(scale_logSize ~ scale_logRange, 
#          color =~ Range,
#          data = log) %>%
# gf_abline(intercept = b/w[2], slope = -w[1]/w[2]) %>%
# gf_abline(intercept = (b+1)/w[2], slope = -w[1]/w[2], lty = 2) %>%
# gf_abline(intercept = (b-1)/w[2], slope = -w[1]/w[2], lty = 2)

answer7 <- "insert plot"
### question 8

set.seed(9)
n=dim(log)[1]

nfolds = 10
groups = rep(1:nfolds, length = n)
cvgroups = sample(groups, n)

preds = factor(rep(NA,n),levels = c("Atlantic", "California"))
ctrl = trainControl(method = "LOOCV")

for(ii in 1:nfolds){
  
  groupii = (cvgroups == ii)
  train_set = log[!groupii, ]
  test_set = log[groupii, ]
  
  dataused = train_set
  
  fit_log.dcv = train(Region ~ logRange + logSize,
                      data = dataused,
                      method = "svmLinear",
                      tuneGrid = expand.grid(C = c(0.001, 0.01, 0.1, 
                                                   1, 5, 10, 100)),
                      preProcess = c("center","scale"),
                      trControl = ctrl)
  
  preds[groupii] = predict(fit_log.dcv, newdata = test_set)
  
}


answer8 <- "insert code"

### question 9
conf_matrix <- table(Predicted = preds, Actual = log$Region)
conf_matrix

answer9 <- sum(diag(conf_matrix)) / sum(conf_matrix)
accuracy


conf_mat = table(allpredscv, log$Region)
x <- sum(diag(conf_mat)) / dim(log)[1]
answer9

### question 10
auto <- data(Auto)
auto <- Auto %>%
  mutate(origin = as.factor(origin),
         gasmilage = as.factor(if_else(mpg >= median(mpg),
                                       "high", "low"))) %>%
  select(-name, -mpg)

answer10 <- "insert code"

### question 11
set.seed(9)
ctrl = trainControl(method = 'cv', number = 10)
fit_auto = train(gasmilage ~ .,
                 data = auto,
                 method = "svmRadial",
                 tuneGrid = expand.grid(C = c(0.001, 0.01, 0.1, 1, 5, 10, 100),
                                        sigma = c(0.5, 1, 2, 3, 4)),
                 preProcess = c("center", "scale"),
                 prob.model = TRUE,
                 trControl = ctrl)

answer11 <- "insert code"

### question 12
answer12 <- 1


### question 13
answer13 <- 1


### question 14
answer14 <- max(fit_auto$results$Accuracy)
answer14

### question 15
Sunbeam <- data.frame(cylinders = 4, displacement = 132.5, horsepower = 155,
                      weight = 2910, acceleration = 8.3, year = 77, 
                      origin = factor(1, levels = levels(auto$origin)))

answer15 <- predict(fit_auto, newdata = Sunbeam, type = "prob")[1]
answer15


### question 16
example.grid = data.frame(cylinders = rep(4, 100),
                          displacement = rep(median(auto$displacement),100),
                          horsepower = rep(median(auto$horsepower), 100),
                          weight = seq(min(auto$weight), max(auto$weight), length = 100),
                          acceleration = rep(median(auto$acceleration), 100),
                          year = rep(median(auto$year), 100),
                          origin = rep("1", 100))

predict(fit_auto, newdata = example.grid, type = "prob")

a16 <- predict(fit_auto, newdata = example.grid, type = "prob")[1]

answer16 <- "insert code"

### question 17

example.grid <- example.grid %>%
  mutate(prob_high = a16$high)

ggplot(data = example.grid, aes(x = weight, y = prob_high)) +
  geom_point() +
  labs(title = "The Relationship Between Weight and Fuel Efficiency",
       subtitle = "Using Predicted Probability",
       x = "Weight (lbs)",
       y = "Predicted Probability of 'High' Gas Milage")

answer17 <- "insert plot"


#############################
### answers #################
#############################
answer1
answer2
answer3
answer4
answer5
answer6
answer7
answer8
answer9
answer10
answer11
answer12
answer13
answer14
answer15
answer16
answer17