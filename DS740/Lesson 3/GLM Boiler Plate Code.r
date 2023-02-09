### GLM Boiler Plate Code

### find and replace 'dataframe' with the name of your dataframe
### find and replace 'response.variable' with the name of your response variable
### find and replace 'predictor.variables' with the names of your predictor variables

#### Libraries
library(ISLR)
library(dplyr)
library(ggformula)
library(gpk)

#### Cross Validation with GLM
x <- dataframe # dataframe with predictors
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

    modelfit = glm(response.variable ~ predictor.variables, 
                    data = trainset, family = "binomial") # fit the model
    predicted = predict(modelfit, newdata = testset, 
                        type = "response") # predict
    allpredicted[groupii] = predicted # store the predicted values
}
### Confusion Matrix, fixed threshold
table(allpredicted > 0.5, response.variable) # create a confusion matrix

# Confusion Matrix from Default dataset from ISLR where:
#   model_fit = glm(default ~ student + income,data = train_set, family="binomial")
#                 Actual
#                 No   Yes
# Predicted FALSE 9629 229
#           TRUE    38 104
#
# False Positive Rate = 0.003930899 (38/38+9629) *** Fall-out ***
# False Negative Rate = 0.1836735 (229/229+104) *** Miss Rate **
# True Positive Rate = 0.8163265 (104/104+229) *** Sensitivity ***
# True Negative Rate = 0.9960691 (9629/9629+38) *** Specificity ***
# Accuracy = 0.9815 (104+9629)/(104+9629+38+229) *** Overall Accuracy ***
# Error Rate = 0.0185 (38+229)/(104+9629+38+229) *** Overall Error Rate ***
# Sensitivity = 0.8163265 (104/104+229) *** True Positive Rate ***
# Specificity = 0.9960691 (9629/9629+38) *** True Negative Rate ***

### ROC Curve, rarely used on a single model
library(pROC)
my_roc = roc(response = response.variable, predictor = allpredicted)
plot.roc(my_roc)

# example of converting knn predictions to probabilities
predicted = knn(train = trainset, test = testset, 
                cl = trainset$response.variable, k = xx,
                prob = TRUE)
prob = attr(predicted, "prob") # Prob contains the probability of belonging to the predicted class.
# We want to convert this into the probability of defaulting.
prob_of_default = ifelse(predicted == "No", 1 - prob, prob) # default refers to Default dataset from ISLR
all_predicted_knn[groupii] = predicted
# Make the ROC curves
my_roc = roc(response = dataframe$response.variable, predictor = all_predicted)
my_roc_knn = roc(response = dataframe$response.variable, predictor = all_predicted_knn)
# Collect key values into a data frame:
results = data.frame(sensitivity = c(my_roc$sensitivities, my_roc_knn$sensitivities), 
                     specificity = c(my_roc$specificities, my_roc_knn$specificities))
# Add a column to specify the model.  While we're at it, compute the false positive rate:
results <- results %>%
  mutate(model = c(rep("Logistic", length(my_roc$thresholds)), 
            rep("KNN", length(my_roc_knn$thresholds))),
            false_positive_rate = 1 - specificity)
# Plot the ROC curves
ggplot(results, aes(x = false_positive_rate, y = sensitivity, color = model)) +
  geom_line() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  labs(x = "False Positive Rate", y = "True Positive Rate", color = "Model")

### Cost Function
cost = matrix(c(100, -10, -100, 10), nc = 2)
cost
Conf_matrix = table(allpredicted > 0.5, response.variable)
Conf_matrix
Conf_matrix * cost # element-wise multiplication of the matrices
sum(Conf_matrix * cost) # total cost of the model, larger is better
# Optimize the total cost of the model
prob_threshold = seq(0.01, 0.98, by = 0.01)
total_cost = numeric(length = length(prob_threshold))
for (ii in 1:length(prob_threshold)){
    Conf_matrix = table(allpredicted > prob_threshold[ii], response.variable)
    total_cost[ii] = sum(Conf_matrix * cost)
}
max(total_cost) # maximum total cost
prob_threshold[which.max(total_cost)] # probability threshold that maximizes the total cost
# Plot the total cost as a function of the probability threshold
ggplot(data.frame(prob_threshold, total_cost), aes(x = prob_threshold, y = total_cost)) +
  geom_line() +
  labs(x = "Probability Threshold", y = "Total Cost")

### Genalized Linear Model for Count Data
# Poisson Regression

data("elephant")
elephant %>% 
    gf_point(jitter(Number_of_Matings) ~ Age_in_Years) %>%
    gf_refine(scale_y_continuous(breaks = seq(0,10,2)))

fit_elephant = glm(Number_of_Matings ~ Age_in_Years, data = elephant, family = poisson)

elephant2 <- elephant %>%
  mutate(log_means = predict(fit_elephant))

elephant2 %>%
  gf_point(log(Number_of_Matings) ~ Age_in_Years) %>%
  gf_line(log_means ~ Age_in_Years)

elephant2 <- elephant2 %>%
  mutate(mean_matings = exp(log_means))

elephant2 %>%
  gf_point(Number_of_Matings ~ Age_in_Years) %>%
  gf_line(mean_matings ~ Age_in_Years)

# Summary
# Logistic regression is a good choice for binary response variables when:
# * you want an iterpetable model
# * the classes are not well separated
# * the relationship between the predictors and log(p(x)/1-p(x)) response variable is linear
# ROC curves summarize the relationship between true positive rate and false positive rate for a range of probability thresholds
# Higher ROC curves indicate better models/methods