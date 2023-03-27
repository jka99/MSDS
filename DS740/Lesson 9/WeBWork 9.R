setwd("C:/Users/jeffe/Documents/MSDS/GitHub/MSDS/DS740/lesson 9")
library(readr)
library(dplyr)
library(caret)
library(kernlab)
library(ggformula)

# Problem 4
bank = read_delim("bank-additional.csv", delim = ";")

bank <- bank %>%
  mutate(scale_emp.var.rate = scale(emp.var.rate),
         scale_duration = scale(duration))

bank %>%
  gf_point(scale_duration ~ scale_emp.var.rate, 
           color =~ y, pch =~ y)

set.seed(999)
data_used = bank
ctrl = trainControl(method = "cv", number = 10)
fit_bank = train(y ~ emp.var.rate + duration,
                 data = data_used,
                 method = "svmLinear",
                 tuneGrid = expand.grid(C = 1),
                 preProcess = c("center","scale"),
                 trControl = ctrl)

fit_bank$results$Accuracy
conf_mat = table(predict(fit_bank), bank$y)
conf_mat

bank <- bank %>%
  tibble::rownames_to_column("Row")
bank <- bank %>%
  mutate(is_SV = Row %in% attr(fit_bank$finalModel, "SVindex"))
b = attr(fit_bank$finalModel, "b")
coefs = attr(fit_bank$finalModel, "coef")[[1]]
bank_SV <- bank %>%
  filter(is_SV) %>%
  select(c(scale_emp.var.rate, scale_duration)) %>%
  as.matrix()
w = colSums(coefs * bank_SV) # beta_1, ... beta_p
bank %>%
  gf_point(scale_duration ~ scale_emp.var.rate, 
           color =~ y, pch =~ y) %>%
  gf_abline(intercept = b/w[2], slope = -w[1]/w[2]) %>%
  gf_abline(intercept = (b+1)/w[2], slope = -w[1]/w[2], lty = 2) %>%
  gf_abline(intercept = (b-1)/w[2], slope = -w[1]/w[2], lty = 2)


set.seed(999)
dataused = bank
ctrl = trainControl(method = "cv", number = 5)
fit_bank = train(y ~ emp.var.rate + duration,
                 data = dataused,
                 method = "svmRadial",
                 tuneGrid = expand.grid(C = c(1, 10, 100), 
                                        sigma = c(1,2,3,4)),
                 preProcess = c("center", "scale"),
                 prob.model = TRUE,
                 trControl = ctrl)
fit_bank$finalModel
fit_bank
set.seed(999)
dataused = bank
ctrl = trainControl(method = "cv", number = 10)
fit_bank = train(y ~ emp.var.rate + duration,
                 data = dataused,
                 method = "svmRadial",
                 tuneGrid = expand.grid(C = c(0.01, 0.1, 1), 
                                        sigma = c(0.01, 0.1, 1)),
                 preProcess = c("center", "scale"),
                 prob.model = TRUE,
                 trControl = ctrl)
fit_bank$finalModel
fit_bank

newClient = data.frame(emp.var.rate = 1, duration = 250) 
predict(fit_bank, newdata = newClient)

xgrid = expand.grid(emp.var.rate = seq(-3.4, 1.4, by = .01),
                    duration = seq(0, 3650, by = 4))
preds <- predict(fit_bank, newdata = xgrid, type = "prob")
ggplot(data = xgrid, aes(x = emp.var.rate))
xgrid <- xgrid %>%
  mutate(prob_no = preds$no)
xgrid  
 
ggplot(xgrid, aes(emp.var.rate, duration, z = prob_no)) +
  geom_contour(breaks = .5) +
  geom_point(aes(emp.var.rate, duration, shape = y, color = y),
             data = bank, 
             inherit.aes = FALSE)


new_xgrid <- expand.grid(emp.var.rate = seq(-3.4, 1.4, by = 0.01),
                        duration = median(bank$duration))
preds = predict(fit_bank, newdata = new_xgrid, type = "prob")

new_xrid <- new_xrid %>%
  mutate(prob_yes = preds[,2])
ggplot(data = new_xrid, aes(x = emp.var.rate, y = prob_yes)) +
  geom_point()
new_xrid

conf_mat = table(predict(fit_bank), bank$y)
conf_mat
sum(diag(conf_mat)) / dim(bank)[1]

set.seed(999)
n = dim(bank)[1]
ngroups = 5
groups = rep(1:ngroups, n)
cvgroups = sample(groups, n)
preds = factor(levels = c("no", "yes"))
ctrl = trainControl(method = "cv", number = 5)

for(ii in 1:ngroups){
  groupii = (cvgroups == ii)
  train = bank[!groupii,]
  test = bank[groupii,]
  
  
  dataused = train
  
  ctrl = trainControl(method = "cv", number = 5)
  fit_bank.dcv = train(y ~ emp.var.rate + duration,
                   data = dataused,
                   method = "svmRadial",
                   tuneGrid = expand.grid(C = c(1, 10, 100),
                                          sigma = c(1, 2, 3, 4)),
                   preProcess = c("center", "scale"),
                   trControl = ctrl)
    
  #preds[groupii] = predict(fit_bank.dcv, nexdata = test)
}

conf_mat = table(predict(fit_bank.dcv), preds)
conf_mat
sum(diag(conf_mat)) / dim(bank)[1]


dim(fit_bank.dcv)


set.seed(999)
n = dim(bank)[1]
ngroups = 5 # 5-fold outer CV
groups = rep(1:ngroups, length = n) 
cv_groups = sample(groups, n)
preds = factor(levels = c("no", "yes"))
ctrl = trainControl(method = "cv", number = 5)

for(ii in 1:ngroups){
  groupii = (cv_groups == ii)
  train_set = bank[!groupii, ]
  test_set = bank[groupii, ]
  data_used = train_set
  fit_cv = train(y ~ emp.var.rate + duration,
                 data = data_used,
                 method = "svmRadial",
                 tuneGrid = expand.grid(C = c(1, 10, 100),
                                        sigma = c(1, 2, 3, 4)),
                 preProcess = c("center","scale"),
                 trControl = ctrl)
  preds[groupii] = predict(fit_cv, newdata = test_set)
}
conf_mat = table(preds, bank$y)
sum(diag(conf_mat)) / dim(bank)[1]