library(caret)
library(dplyr)
library(ISLR)
library(nnet)
library(randomForest)

data(OJ)
### question 1
set.seed(10)
ctrl = trainControl(method = "cv", number = 5)
fit_OJ = train(Purchase ~ LoyalCH + SalePriceMM + PriceDiff,
               data = OJ,
               method = "nnet",
               tuneGrid = expand.grid(size = 1, decay = 0),
               skip = FALSE,
               trace = FALSE,
               preProc = c("center", "scale"),
               trControl = ctrl)

### question 2
library(NeuralNetTools)
par(mar = c(.1,.1,.1,.1))
plotnet(fit_OJ) 

### question 3
a = "decreases"
b = "decrease"
c = "Minute Maid"

### question 4
summary(fit_OJ)
OJ[1,]

answer4 = predict(fit_OJ, newdata = OJ[1,], type = "prob")

### question 5
predict_prob = predict(fit_OJ, type = "prob")
prediction = (predict_prob > 0.5)

conf_mat1 = table(predict(fit_OJ), OJ$Purchase)
answer5 <- 1 - sum(diag(conf_mat1)/sum(conf_mat1));

### question 6
pred = predict(fit_OJ, type = "prob")
myOJ <- OJ %>%
  mutate(prob_MM = pred$MM)

myOJ <- myOJ %>%
  mutate(prob_MM99 = case_when(prob_MM > 0.9 ~ "MM",
                               prob_MM < 0.1 ~ "CH",
                               TRUE ~ NA))

conf_mat2 = table(myOJ$Purchase, myOJ$prob_MM99)
answer6 <- 1 - sum(diag(conf_mat2)/sum(conf_mat2)); answer6

### question 7
answer7 <- "insert code"

### question 8
pred = predict(fit_OJ, type = "prob")
myOJ <- OJ %>%
  mutate(prob_MM = pred$MM)

myOJ <- myOJ %>%
  mutate(prob_MM99 = case_when(prob_MM > 0.9 ~ "MM",
                               prob_MM < 0.1 ~ "CH",
                               TRUE ~ NA))
colSums(is.na(myOJ))[20]

### question 9
lekprofile(fit_OJ)

### question 10
data(Hitters)
myHitters <- Hitters %>%
  mutate(League01 = if_else(League == "A", 0, 1),
         Division01 = if_else(Division == "E", 0, 1),
         NewLeague01 = if_else(NewLeague == "A", 0, 1)) %>%
  select(-League, -Division, -NewLeague)

colSums(is.na(Hitters))
myHitters = na.omit(myHitters)

### question 11
set.seed(10)
ctrl = trainControl(method = "cv", number = 10)
fit_hitters = train(Salary ~ ., 
                    data = myHitters,
                    method = "nnet",
                    tuneGrid = expand.grid(size = 5, decay = seq(1,2, by = 0.1)),
                    skip = FALSE,
                    trace = FALSE,
                    linout = TRUE,
                    preProc = c("center", "scale"),
                    maxit = 2000,
                    trControl = ctrl)

answer12 <- "insert code"

### question 12
x = seq(1, 2, by = 0.1)
y = fit_hitters$results$RMSE
example = data.frame(x,y)
ggplot(data = example, aes(x = x, y = y)) +
  geom_point() + 
  labs(x = "Decay",
       y = "RMSE") 

### question 13
garson(fit_hitters)

### question 14
answer14 <- "CHits and PutOuts"

### question 15
find_mode <- function(x){
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

Hits <- sample(myHitters$Hits, 263, replace = T)

mydata.median <- myHitters %>% 
  select(-c(Salary,Hits,League01,Division01,NewLeague01))
mydata.median <- sapply(mydata.median, median)

mydata.mode <- myHitters %>% select(c(League01,Division01,NewLeague01))
mydata.mode <- sapply(mydata.mode, find_mode)

mydata.df <- as.data.frame(t(c(mydata.median, mydata.mode)))

mydata.list <- lapply(mydata.df, function(x) c(x))
mydata.list$Hits <- Hits
mydata.new <- do.call(expand.grid, mydata.list)
head(mydata.new)

predict(fit_hitters$finalModel, newdata = mydata.new)

