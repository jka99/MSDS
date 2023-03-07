# just a heads up, this thing is a mess
# I'm not sure what I was thinking when I wrote it



library(MASS)
library(dplyr)
library(ggformula)

# Read in and Review data
data <- read.csv("bodyfat.csv")
Tukey = (pmax(1-(r/4.685)^2,0))^2
fitbodyfat <- rlm(BodyFatBrozek ~ Weight)
head(data)

r=seq(-6,6,.01)
wi = pmax(1-abs(r)/3, 0) 
plot(r, wi, type="l", col="black", lwd=2, las =1, cex.axis=1.2, xlab="Residual", ylab="Weight")
# or
gf_line(wi ~ r)

Tukey = (pmax(1-(r/4.685)^2,0))^2
to_plot = data.frame(residuals = c(r, r),
                     weights = c(wi, Tukey),
                     method = c(rep("Linear", length(r)),
                                rep("Tukey", length(r))))
to_plot %>%
  gf_line(weights ~ residuals, color =~ method)

fat = read.csv("bodyfat.csv")
library(MASS)
fit_Tukey = rlm(BodyFatBrozek ~ Weight, data = fat, psi = psi.bisquare)  
summary(fit_Tukey)
fit_Tukey


fit.w = lm(BodyFatBrozek ~ Weight, data = fat)
oldcoef = rep(0, length(fit.w$coef))
newcoef = fit.w$coef
iter = 0


while(sum(abs(oldcoef-newcoef)) > .0001 & iter < 100){
    w = pmax(1-abs(fit.w$residuals)/3, 0)
    fit.w = lm(BodyFatBrozek ~ Weight, data = fat, weights=w)
 
    iter = iter + 1
    oldcoef = newcoef
    newcoef = fit.w$coef 
}
Sigmax[2,1]
acffit <- acf(fit.w$residuals, lag.max = 1, main = "ACF of residuals", col = "blue")
acffit$acf
sqrt(acffit$acf[2])

n = 100
Sigmax = matrix(NA, nr=n, nc=n) # initialize the covariance matrix
Sigmax[1,1] = 1
for(i in 2:n){
    Sigmax[i,i] = 1
    for(j in 1:(i-1)){
        Sigmax[i, j] = .9^abs(i-j)
        Sigmax[j, i] = Sigmax[i, j] # make the covariance matrix 
                                    # symmetric across the diagonal
    } #end iter over j
} # end iter over i

x = runif(n, 0, 1)
y = 2*x + 3 + mvrnorm(1, rep(0,n), Sigmax) # generate 1 random vector 
                                                # of n noise terms
library(MASS)
test <- mvrnorm(1, rep(0,n), Sigmax)
test
summary(test)
y

set.seed(15)
x = runif(n, 0, 1)
y = 2*x + 3 + mvrnorm(1, rep(0,n), Sigmax) # generate 1 random vector 
                                                # of n noise terms
m1 <- lm(y ~ x)
plot(m1)
resid <- resid(m1)
cor(resid[1:99], resid[2:100])
acf(resid, lag.max = 10, ci.type = "ma")
plot(m1$residuals)
library(nlme)
m2 <- gls(y ~ x, correlation = corAR1(form = ~1))
m2
summary(m2)
m1_rem1 = gls(y~x)
AIC(m2, m1_rem1)
