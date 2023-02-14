### Load packages
library(ggformula)
library(leaps)
library(dplyr)
### Load data
wisc_income <- read.csv("Wisconsin_income-1.csv")
wisc_income <- wisc_income %>% 
  mutate(CIT2 = as.factor(CIT2),
        COW = as.factor(COW),
        LANX = as.factor(LANX),
        MAR = as.factor(MAR),
        SEX = as.factor(SEX),
        DIS = as.factor(DIS),
        RAC = as.factor(RAC),
        Hispanic = as.factor(Hispanic),
# add log transformation of JWMP and PERNP
        JWMNP= log(JWMNP),
        PERNP = log(PERNP))

regfit1 = regsubsets(PERNP ~., data = wisc_income, nvmax = 39)


summary(regfit1)



plot(regfit1, scale = "adjr2")
regfit1.summary = summary(regfit1)
regfit1.summary$bic
plot(regfit1.summary$bic, xlab = "Number of Varianles", ylab = "BIC", type = "l")
which.min(regfit1.summary$bic)
which.min(regfit1.summary$cp)
which.max(regfit1.summary$adjr2)
coef(regfit1.summary)


### Question 7
# Define a predict() function for regsubsets objects
predict.regsubsets <- function(object, alldata, subset, id, ...){
    form = as.formula(object$call[[2]])
    mat = model.matrix(form, alldata)
    mat = mat[subset, ]
    
    if(sum(subset) == 1 | length(subset) == 1){
      # For LOOCV, convert mat to a matrix
      mat = t(as.matrix(mat))
    }
    
    coefi = coef(object, id=id)
    xvars = names(coefi)
    mat[ , xvars] %*% coefi
} # end function predict.regsubsets

#### Cross Validation with GLM
x <- wisc_income # dataframe with predictors
n = dim(x)[1] # number of observations
ngroups = 10 # number of folds
groups = rep(1:ngroups, length = n) # create a vector of group numbers
set.seed(3) # set seed for reproducibility
cvgroups = sample(groups, n) # randomize the groups
nvar = 39 # number of variables
group_error = matrix(NA, nr = ngroups, nc = nvar) # matrix to store errors
                      # row - fold
                      # column - model size (number of variables)
#allpredicted = rep(NA, n) # vector to store predicted values

for (ii in 1:ngroups){
    groupii = (cvgroups == ii) # create a logical vector for the test set
    train_data = x[!groupii, ] # create the training set
    test_data = x[groupii, ] # create the test set

    cv_fit = regsubsets(PERNP ~ ., 
                    data = train_data,
                    nvmax = nvar) # fit the model
    for(jj in 1:nvar){
        y_pred = predict(cv_fit, alldata = wisc_income,
                          subset = groupii, id = jj) # predict on the test set
        group_error[ii, jj] = mean((test_data$PERNP - y_pred)^2) # calculate MSE
    } # end for jj
} # end for ii

MSE_overall = apply(group_error, 2, mean) # calculate the average MSE for each model size

gf_point(MSE_overall ~ 1:nvar) 

low_MSE_model = which.min(MSE_overall) # find the model size with the lowest MSE

std_err = apply(group_error, 2, sd)/sqrt(ngroups)
std_err[low_MSE_model]
which(MSE_overall <= (MSE_overall[low_MSE_model] + std_err[low_MSE_model]))

coef(regfit1, 2)

### Question 11
library(ISLR)
Auto1 <- Auto %>%
  mutate(origin = as.factor(origin),
         efficiency = as.factor(ifelse(mpg >= median(mpg), "high", "low")))

library(corrplot)
library(RColorBrewer)
library(car)
library(pROC)

Auto.numeric <- select_if(Auto1, is.numeric)
correlations <- cor(Auto.numeric, use = "pairwise.complete.obs")
corrplot(correlations, type = "upper", order = "hclust", 
         col = rev(brewer.pal(n = 8 , name = "RdYlBu")))

fit = glm(efficiency ~ cylinders + displacement + horsepower + weight 
                 + acceleration + year + origin, data = Auto1, family = "binomial")
vif(fit)

Auto1 <- Auto1 %>%
  select(-displacement)

x <- Auto1 # dataframe with predictors
n = dim(x)[1] # number of observations
nfolds = 10 # number of folds
groups = rep(1:nfolds, length = n) # create a vector of group numbers
set.seed(1) # set seed for reproducibility
cvgroups = sample(groups, n) # randomize the groups
allpredicted = rep(NA, n) # vector to store predicted values

for (ii in 1:nfolds){
    groupii = (cvgroups == ii) # create a logical vector for the test set
    trainset = x[!groupii, ] # create the training set
    testset = x[groupii, ] # create the test set

    modelfit = glm(efficiency ~ cylinders + horsepower + weight + acceleration + year + origin, 
                    data = trainset, family = "binomial") # fit the model
    predicted = predict(modelfit, newdata = testset, 
                        type = "response") # predict
    allpredicted[groupii] = predicted # store the predicted values
}

my_roc = roc(response = Auto1$efficiency, predictor = allpredicted)
plot.roc(my_roc)
auc(my_roc)
