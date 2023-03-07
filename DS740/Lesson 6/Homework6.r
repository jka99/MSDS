library(dplyr)
library(smss)
library(MASS)
library(ggformula)
data("crime2005")

setwd("C:/Users/jeffe/Documents/MSDS/GitHub/MSDS/DS740/Lesson 6")

# question 1
model <- lm(VI2 ~ ME + PO, data = crime2005)
summary(model)

# question 2
par(mfrow = c(2,2))
plot(model)

# question 3

oldcoef = rep(0,length(model$coef))
newcoef = model$coef
iter = 0

while(sum(abs(oldcoef-newcoef)) > .0001 & iter < 100){
  w = 1/(model$fitted.values^2)
  model2 <- lm(VI2 ~ ME + PO, data = crime2005, weights=w)
  
  iter = iter + 1
  oldcoef = newcoef
  newcoef = model$coef
}

# question 4
summary(model2)

# question 5
while(sum(abs(oldcoef - newcoef)) > 0.0001 & iter < 100){
  MAR = median(abs(model2$residuals))
  sigma = MAR/0.6745
  k = 4.685 * sigma
  w = pmax(1-(model2$residuals/k)^2, 0)

  fit.w = rlm(VI2 ~ ME + PO, data = crime2005, weights=w)

  iter = iter + 1
  oldcoef = newcoef
  newcoef = model$coef
}

fit_bisquare <- rlm(VI2 ~ ME + PO, data = crime2005, psi = psi.bisquare)


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
fit.elnino.B <- gls(air.temp ~ zon.winds + mer.winds + humidity + s.s.temp, data = elnino, correlation = corAR1(form = ~ 1))
res <- resid(fit.elnino.B)
plot(fitted(fit.elnino.B), res, main = "Fitted vs Residuals", xlab = "Fitted Values", ylab = "Residuals")
abline(h = 0, col = "red")
qqnorm(res)
qqline(res)
