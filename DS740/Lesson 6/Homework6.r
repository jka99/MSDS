library(dplyr)
library(smss)
library(MASS)
library(nlme)
library(pROC)
library(ggformula)
data("crime2005")

setwd("C:/Users/jeffe/Documents/MSDS/GitHub/MSDS/DS740/Lesson 6")

# question 1
model <- lm(VI2 ~ ME + PO, data = crime2005)
summary(model)
question_1 <- model$coefficients
# question 2
par(mfrow = c(2,2))
plot(model)

# question 3
model <- lm(VI2 ~ ME + PO, data = crime2005)
oldcoef = rep(0,length(model$coef))
newcoef = model$coef
iter = 0

while(sum(abs(oldcoef-newcoef)) > .0001 & iter < 100){
  MAR = median(abs(model$residuals))
  sigma = MAR/0.6745
  k = 1.345 * sigma
  w = pmin(k/(abs(model$residuals)), 1)

  model = lm(VI2 ~ ME + PO, data = crime2005, weights=w)

  iter = iter + 1
  oldcoef = newcoef
  newcoef = model$coef
}

# question 4
question_4 <- newcoef

# question 5

#model <- lm(VI2 ~ ME + PO, data = crime2005)
#oldcoef = rep(0,length(model$coef))
#newcoef = model$coef
#iter = 0
#
#while(sum(abs(oldcoef - newcoef)) > 0.0001 & iter < 100){
#  MAR = median(abs(model$residuals))
#  sigma = MAR/0.6745
#  k = 4.685 * sigma
#  w = pmax(1-(model$residuals/k)^2, 0)
#
#  model = rlm(VI2 ~ ME + PO, data = crime2005, weights=w)
#
#  iter = iter + 1
#  oldcoef = newcoef
#  newcoef = model$coef
#}
#newcoef

fit_bisquare <- rlm(VI2 ~ ME + PO, data = crime2005, psi = psi.bisquare) #<- this is the CORRECT answer
fit_bisquare$coefficients

# question 6
summary(fit_bisquare)

# question 7
# lower
# higher
# higher

# question 8
crime2005_2 <- crime2005 %>%
  tibble::rownames_to_column("ABBREV")

crime2005_2 <- crime2005_2 %>%
  mutate(STATE = as.character(STATE),
          my_label = case_when(fit_bisquare$w < 0.8 ~ STATE,
          TRUE ~ ""))
gf_point(fit_bisquare$w ~ 1:dim(crime2005)[1]) %>%
  gf_text(fit_bisquare$w - .02 ~ 1:dim(crime2005)[1], 
           label = crime2005_2$my_label)

# question 9
elnino = read.csv("elnino.csv")
elnino <- na.omit(elnino)

# question 10
par(mfrow = c(1,1))

fit.elnino.A <- lm(air.temp ~ zon.winds + mer.winds + humidity + s.s.temp, data = elnino)
res <- resid(fit.elnino.A)
plot(fitted(fit.elnino.A), res, main = "Fitted vs Residuals", xlab = "Fitted Values", ylab = "Residuals")
abline(h = 0, col = "red")
# question 11
qqnorm(res)
qqline(res)

# question 12
answer12 <- "The variability of the residuals expands as the predicted value of the predictor variable increases. The variability is generally symmetrical around 0, though slightly more negative than positive. The Normal Q-Q plot shows that the data are generally normal. A linear model may be appropriate for this data."

# question 13
fit.elnino.B <- gls(air.temp ~ zon.winds + mer.winds + humidity + s.s.temp, data = elnino)
res <- resid(fit.elnino.B)
plot(fitted(fit.elnino.B), res, main = "Fitted vs Residuals", xlab = "Fitted Values", ylab = "Residuals")
abline(h = 0, col = "red")
qqnorm(res)
qqline(res)

fit.elnino.A$coefficients
fit.elnino.B$coefficients
answer13 <- "The coefficients for Model B are lower than they are for Model A. Model B (the gls) takes into account the correlation among the errors and adjusts for the heterogeneity of variance. This results in more accurate and efficient coefficient estimates."

# question 14
resid_elnino <- elnino %>%
  mutate(residuals = resid(fit.elnino.B))

resid_elnino <- resid_elnino %>%
  mutate(prev_residuals = lag(residuals, 1)) %>%
  filter(buoy == 3)

  resid_elnino <- resid_elnino[-1,]
gf_point(residuals ~ prev_residuals, data = resid_elnino)

# question 15
answer15 <- "The graph shows a positive correlation between residuals and the prev_residuals. That makes sense. Air temperature tends to work in patterns. Days get warmer as we progress into Spring and Summer then cooler into Fall and Winter. It is reasonable to assume that tomorrows temperature will fall within a given band based on today's temperature."

# question 16
fit.elnino.C <- gls(air.temp ~ zon.winds + mer.winds + humidity + s.s.temp, 
  data = elnino, correlation = corAR1(form = ~ day | buoy))

# question 17
summary(fit.elnino.C)

# question 18
fit.elnino.C <- gls(air.temp ~ zon.winds + mer.winds + humidity + s.s.temp,
  data = elnino, correlation = corAR1(form = ~ day | buoy))

# question 19
AIC(fit.elnino.A, fit.elnino.B, fit.elnino.C)

# question 20
answer20 <- "Based on AIC, Model C is much better than either A or B. Model B is not a reasonable substitute for Model C."

# answers
# 1 -56.8231943   0.8154662   4.2599725
# 2 Washington DC, South Carolina, Delaware, West Virginia, Mississippi
# 3 see code above
# 4 -40.3926844   0.6794144   3.2950743
# 5 see code above
# 6 -31.2926,     0.6114,     2.8081
# 7 lower, higher, higher
# 8 see code above
# 9 see code above
# 10 see code above
# 11 see code above
# 12 answer12
# 13 answer13
# 14 see code above
# 15 answer15
# 16 see code above
# 17 0.8087
# 18 0.6540
# 19 Model A and Model B?
# 20 answer20