### Homework 4
library(MASS)
library(pROC)
library(dplyr)
library(ISLR)
library(mvnormalTest)  
library(MVN)
library(MVTests)
library(ggformula)
# jeeze, that's a lot of libraries

### question 1
NewAuto <- Auto %>%
  mutate(Domestic = as.numeric(origin == 1))
NewAuto %>%
    group_by(Domestic) %>%
    count()

### question 2
gf_boxplot(mpg ~ as.factor(Domestic), data = NewAuto, fill = c("skyblue", "pink"))

### question 3 and 4
NewAuto %>%
    group_by(Domestic) %>%
    summarize(mean(mpg), sd(mpg))

### question 5
answer5 <- "Looking at the boxplots, the IQRs appear to be similar. The median of each is roughly centralized and not wildly skewed high or low. Looking at the qqplots, both data sets are mostly normal. Foreign cars are far more normal than Domestic, especially on the ends. Domestic stays normal-ish for almost 2 standard deviations. I think it is OK to use LDA with this data."
### question 6
model <- lda(Domestic ~ mpg, data = NewAuto)
predict(model, data = NewAuto)
confmat <- table(NewAuto$Domestic, predict(model, data = NewAuto)$class); confmat

### question 7
confmat[2,2] / (confmat[2,2] + confmat[2,1])

### question 8
TN = confmat[1,1]; TN
FN = confmat[1,2]; FN
TP = confmat[2,2]; TP
FP = confmat[2,1]; FP
TN / (TN + FP)

### question 9
gf_boxplot(displacement ~ as.factor(Domestic), data = NewAuto)
gf_boxplot(horsepower ~ as.factor(Domestic), data = NewAuto)
gf_boxplot(acceleration ~ as.factor(Domestic), data = NewAuto)
# I believe displacement is what they are looking for as "discriminating" variable

### question 10
NewAuto1 <- NewAuto %>%
    filter(Domestic == 1)
library(car)
qqPlot(NewAuto1$displacement)
NewAuto2 <- NewAuto %>%
    filter(Domestic == 0)
qqPlot(NewAuto2$displacement)
NewAuto3 <- NewAuto %>%
    filter(Domestic == 1) %>%
    mutate(SD = sd(displacement))
NewAuto4 <- NewAuto %>%
    filter(Domestic == 0) %>%
    mutate(SD = sd(displacement))
head(NewAuto4)

answer10 <- "I think QDA is better for displacement. The two groups are very different on the boxplots with no overlap in their IQRs. Displacement is much higher and has a much broader range for domestic vehicles than it is for foreign cars. Upper-end outliers for foreign cars fall into the first quartile for domestic cars. The standard deviations for the two groups are very different, making LDA a bad choice for this model."

### question 11
model2 <- qda(Domestic ~ displacement, data = NewAuto)
mod2_pred <- predict(model2, data = NewAuto)$class
table(NewAuto$Domestic, mod2_pred)
ldaprob.displ = predict(lda(Domestic~displacement, data=NewAuto),data=NewAuto)$posterior[,2]
lda.roc.displ <- roc(response=NewAuto$Domestic, predictor=ldaprob.displ)
plot.roc(lda.roc.displ, main = "ROC for predictor displacement"); auc(lda.roc.displ)
