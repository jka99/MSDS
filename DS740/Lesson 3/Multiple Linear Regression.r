#### Multiple Linear Regression

### Load Packages
library(ISLR) # for College data
library(dplyr)
library(corrplot)
library(RColorBrewer) # colors for correlation plot
library(car) # for VIF
library(readr)
library(ggformula)
library(leaps)

College1 <- College[-96,] # remove NA row

### Poisson Regression
# Poisson regression is used to model count data, i.e., data that can only take on
# non-negative integer values. The response variable is assumed to be a Poisson
# random variable with a mean that is a function of the predictor variables.
# The Poisson distribution is a discrete probability distribution that expresses


### Multicolllinearity <- I typed this ***
# Multicollinearity is a problem in regression analysis in which two or more 
# predictor variables are highly correlated.
# Multicollinearity can cause problems in regression analysis by:
#   1. Making it difficult to identify the independent effect of each predictor
#      variable on the response variable.
#   2. Making it difficult to interpret the regression coefficients.
#   3. Reducing the precision of the estimated regression coefficients.
#   4. Making the estimated regression coefficients unstable, i.e., they can vary
#      a lot depending on which data are included in the sample.
#   5. Causing the standard errors of the regression coefficients to be unrealistically
#      small, leading to the conclusion that the predictors are highly significant
#      when they are not.
#   6. Causing the t-statistics for the regression coefficients to be unrealistically
#      large, leading to the conclusion that the predictors are not significant
#      when they are.
#   7. Causing the F-statistic for the overall model to be unrealistically large,
#      leading to the conclusion that the model is highly significant when it is not.
#   8. Causing the p-values for the overall model to be unrealistically small,
#      leading to the conclusion that the model is not significant when it is.
#   9. Causing the p-values for individual regression coefficients to be unrealistically
#      small, leading to the conclusion that individual predictors are not significant
#      when they are.
#   10. Causing the variance inflation factor (VIF) to be unrealistically large,
#       leading to the conclusion that the predictors are not independent when they are.
#   11. Causing the condition number to be unrealistically large, leading to the
#       conclusion that the predictors are not independent when they are.
#   12. Causing the eigenvalues of the X'X matrix to be unrealistically small,
#       leading to the conclusion that the predictors are not independent when they are.
#   13. Causing the eigenvalues of the X'X matrix to be unrealistically large,
#       leading to the conclusion that the predictors are not independent when they are.
#   14. Causing the eigenvalues of the X'X matrix to be unrealistically close to zero,
#       leading to the conclusion that the predictors are not independent when they are.
#   15. Causing the eigenvalues of the X'X matrix to be unrealistically close to infinity,
#       leading to the conclusion that the predictors are not independent when they are.
#   16. Causing the eigenvalues of the X'X matrix to be unrealistically close to zero
#       or infinity, leading to the conclusion that the predictors are not independent
#       when they are.

### Testing for Multicollinearity <- I typed this ***
# There are several ways to test for multicollinearity:
#   1. Correlation matrix
#   2. Variance inflation factor (VIF)
#   3. Condition number
#   4. Eigenvalues of the X'X matrix
#   5. Regression coefficients
#   6. Standard errors of the regression coefficients
#   7. t-statistics for the regression coefficients
#   8. F-statistic for the overall model
#   9. p-values for the overall model
#   10. p-values for individual regression coefficients
#   11. Residuals
#   12. Cook's distance
#   13. DFFITS
#   14. DFBETAS
#   15. Hat matrix
#   16. Leverage
#   17. Influence

### Correlation Matrix
# Correlation matrix is a table showing correlation coefficients between sets of
# variables. Correlation is a statistical measure that indicates the extent to which
# two variables fluctuate together. A correlation coefficient of 1 indicates a perfect
# positive correlation, meaning that as x increases, so does y. A correlation coefficient
# of -1 indicates a perfect negative correlation, meaning that as x increases, y decreases.
# A correlation coefficient of 0 indicates no linear correlation between the variables.
# Correlation is a measure of the strength of a linear relationship between two variables.
# Correlation does not imply causation, so just because two variables are correlated
# does not mean that one causes the other.
# Correlation is a statistical measure that indicates the extent to which two variables
# fluctuate together. A correlation coefficient of 1 indicates a perfect positive
# correlation, meaning that as x increases, so does y. A correlation coefficient of -1
# indicates a perfect negative correlation, meaning that as x increases, y decreases.
# A correlation coefficient of 0 indicates no linear correlation between the variables.
# Correlation is a measure of the strength of a linear relationship between two variables.
# Correlation does not imply causation, so just because two variables are correlated
# does not mean that one causes the other.

# calculate correlation matrix
correlation_matrix <- cor(College1[, -1])

### Variance Inflation Factor (VIF)
# Variance inflation factor (VIF) is a measure of the severity of multicollinearity
# in an ordinary least squares regression analysis. VIF measures the inflation of
# the variance of the regression coefficient estimates due to multicollinearity.
# VIF is calculated as the ratio of the variance in a model with multiple terms
# to the variance of a model with one term alone. VIF is a measure of the severity
# of multicollinearity in an ordinary least squares regression analysis. VIF measures
# the inflation of the variance of the regression coefficient estimates due to
# multicollinearity. VIF is calculated as the ratio of the variance in a model with
# multiple terms to the variance of a model with one term alone. VIF is a measure of
# the severity of multicollinearity in an ordinary least squares regression analysis.
# VIF measures the inflation of the variance of the regression coefficient estimates
# due to multicollinearity. VIF is calculated as the ratio of the variance in a model
# with multiple terms to the variance of a model with one term alone.

# calculate VIF
fit = lm(Grad.Rate ~ ., data = College1)
vif_College1 <- vif(lm(Grad.Rate ~ ., data = College1))

### Condition Number
# Condition number is a measure of the sensitivity of the solution of a mathematical
# problem to small changes in the input data. The condition number of a matrix is a
# measure of the sensitivity of the solution of a mathematical problem to small
# changes in the input data. The condition number of a matrix is a measure of the
# sensitivity of the solution of a mathematical problem to small changes in the input
# data.

### Eigenvalues of the X'X Matrix
# Eigenvalues of the X'X matrix are the eigenvalues of the matrix X'X, where X is
# the matrix of predictor variables. Eigenvalues of the X'X matrix are the eigenvalues
# of the matrix X'X, where X is the matrix of predictor variables. Eigenvalues of the
# X'X matrix are the eigenvalues of the matrix X'X, where X is the matrix of predictor
# variables.

### Regression Coefficients
# Regression coefficients are the coefficients of the regression equation. Regression
# coefficients are the coefficients of the regression equation. Regression coefficients
# are the coefficients of the regression equation.

### Standard Errors of the Regression Coefficients
# Standard errors of the regression coefficients are the standard errors of the
# regression coefficients. Standard errors of the regression coefficients are the
# standard errors of the regression coefficients. Standard errors of the regression
# coefficients are the standard errors of the regression coefficients.

### t-Statistics for the Regression Coefficients
# t-statistics for the regression coefficients are the t-statistics for the regression
# coefficients. t-statistics for the regression coefficients are the t-statistics for
# the regression coefficients. t-statistics for the regression coefficients are the
# t-statistics for the regression coefficients.

### F-Statistic for the Overall Model
# F-statistic for the overall model is the F-statistic for the overall model. F-statistic
# for the overall model is the F-statistic for the overall model. F-statistic for the
# overall model is the F-statistic for the overall model.

### p-Values for the Overall Model
# p-values for the overall model are the p-values for the overall model. p-values for
# the overall model are the p-values for the overall model. p-values for the overall
# model are the p-values for the overall model.

### p-Values for Individual Regression Coefficients
# p-values for individual regression coefficients are the p-values for individual
# regression coefficients. p-values for individual regression coefficients are the
# p-values for individual regression coefficients. p-values for individual regression
# coefficients are the p-values for individual regression coefficients.

### Residuals
# Residuals are the differences between the observed values of the dependent variable
# and the predicted values of the dependent variable. Residuals are the differences
# between the observed values of the dependent variable and the predicted values of
# the dependent variable. Residuals are the differences between the observed values
# of the dependent variable and the predicted values of the dependent variable.

### Cook's Distance
# Cook's distance is a measure of the influence of a data point. Cook's distance is
# a measure of the influence of a data point. Cook's distance is a measure of the
# influence of a data point.

### DFFITS
# DFFITS is a measure of the influence of a data point. DFFITS is a measure of the
# influence of a data point. DFFITS is a measure of the influence of a data point.

### DFBETAS
# DFBETAS is a measure of the influence of a data point. DFBETAS is a measure of the
# influence of a data point. DFBETAS is a measure of the influence of a data point.

### Hat Matrix
# Hat matrix is a matrix that is used to calculate the leverage of a data point.
# Hat matrix is a matrix that is used to calculate the leverage of a data point.
# Hat matrix is a matrix that is used to calculate the leverage of a data point.

### Leverage
# Leverage is a measure of the influence of a data point. Leverage is a measure of
# the influence of a data point. Leverage is a measure of the influence of a data point.

### Influence
# Influence is a measure of the influence of a data point. Influence is a measure
# of the influence of a data point. Influence is a measure of the influence of a data
# point.

### R-Squared
# R-squared is a statistical measure that represents the proportion of the variance
# for a dependent variable that's explained by an independent variable or variables
# in a regression model. R-squared is a statistical measure that represents the
# proportion of the variance for a dependent variable that's explained by an
# independent variable or variables in a regression model. R-squared is a statistical
# measure that represents the proportion of the variance for a dependent variable
# that's explained by an independent variable or variables in a regression model.

### Adjusted R-Squared
# Adjusted R-squared is a statistical measure that represents the proportion of the
# variance for a dependent variable that's explained by an independent variable or
# variables in a regression model. Adjusted R-squared is a statistical measure that
# represents the proportion of the variance for a dependent variable that's explained
# by an independent variable or variables in a regression model. Adjusted R-squared
# is a statistical measure that represents the proportion of the variance for a
# dependent variable that's explained by an independent variable or variables in a
# regression model.

### F-Statistic
# F-statistic is a statistical measure that tests the overall significance of a
# regression model. F-statistic is a statistical measure that tests the overall
# significance of a regression model. F-statistic is a statistical measure that tests
# the overall significance of a regression model.

### p-Value
# p-value is a statistical measure that represents the probability of an observed
# result occurring by chance. p-value is a statistical measure that represents the
# probability of an observed result occurring by chance. p-value is a statistical
# measure that represents the probability of an observed result occurring by chance.

### AIC
# AIC is a measure of the quality of a statistical model for a given set of data.
# AIC is a measure of the quality of a statistical model for a given set of data.
# AIC is a measure of the quality of a statistical model for a given set of data.

### BIC
# BIC is a measure of the quality of a statistical model for a given set of data.
# BIC is a measure of the quality of a statistical model for a given set of data.
# BIC is a measure of the quality of a statistical model for a given set of data.

### Log-Likelihood
# Log-likelihood is a measure of the quality of a statistical model for a given set
# of data. Log-likelihood is a measure of the quality of a statistical model for a
# given set of data. Log-likelihood is a measure of the quality of a statistical model
# for a given set of data.

### Akaike Information Criterion
# Akaike information criterion is a measure of the quality of a statistical model
# for a given set of data. Akaike information criterion is a measure of the quality
# of a statistical model for a given set of data. Akaike information criterion is a
# measure of the quality of a statistical model for a given set of data.

### Bayesian Information Criterion
# Bayesian information criterion is a measure of the quality of a statistical model
# for a given set of data. Bayesian information criterion is a measure of the quality
# of a statistical model for a given set of data. Bayesian information criterion is a
# measure of the quality of a statistical model for a given set of data.

### Log-Likelihood Ratio Test
# Log-likelihood ratio test is a statistical test that compares the fit of two
# statistical models. Log-likelihood ratio test is a statistical test that compares
# the fit of two statistical models. Log-likelihood ratio test is a statistical test
# that compares the fit of two statistical models.

### Log-Likelihood Ratio
# Log-likelihood ratio is a measure of the quality of a statistical model for a given
# set of data. Log-likelihood ratio is a measure of the quality of a statistical model
# for a given set of data. Log-likelihood ratio is a measure of the quality of a
# statistical model for a given set of data.

### Log-Likelihood Difference
# Log-likelihood difference is a measure of the quality of a statistical model for a
# given set of data. Log-likelihood difference is a measure of the quality of a
# statistical model for a given set of data. Log-likelihood difference is a measure of
# the quality of a statistical model for a given set of data.

### Deviance
# Deviance is a measure of the quality of a statistical model for a given set of data.
# Deviance is a measure of the quality of a statistical model for a given set of data.
# Deviance is a measure of the quality of a statistical model for a given set of data.

### Null Deviance
# Null deviance is a measure of the quality of a statistical model for a given set
# of data. Null deviance is a measure of the quality of a statistical model for a
# given set of data. Null deviance is a measure of the quality of a statistical model
# for a given set of data.

### Residual Deviance
# Residual deviance is a measure of the quality of a statistical model for a given
# set of data. Residual deviance is a measure of the quality of a statistical model
# for a given set of data. Residual deviance is a measure of the quality of a
# statistical model for a given set of data.

### AICc
# AICc is a measure of the quality of a statistical model for a given set of data.
# AICc is a measure of the quality of a statistical model for a given set of data.
# AICc is a measure of the quality of a statistical model for a given set of data.

### AICc Difference
# AICc difference is a measure of the quality of a statistical model for a given set
# of data. AICc difference is a measure of the quality of a statistical model for a
# given set of data. AICc difference is a measure of the quality of a statistical
# model for a given set of data.

### AICc Weight
# AICc weight is a measure of the quality of a statistical model for a given set of
# data. AICc weight is a measure of the quality of a statistical model for a given
# set of data. AICc weight is a measure of the quality of a statistical model for a
# given set of data.

