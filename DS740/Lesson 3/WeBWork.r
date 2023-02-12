#### Problem 8

library(dplyr)
library(pROC)
library(ggformula)

data <- read.csv("C:/Users/jeffe/Documents/MSDS/GitHub/MSDS/DS740/Lesson 3/BadRegression.csv")

bad_roc <- roc(response = data$y, predictor = data$predictvals)
bad_roc
auc(bad_roc)

data %>% gf_boxplot(predictvals ~ factor(y))
data2 = roc(response = data$y, 
                   predictor = data$predictvals,
                   direction = "<")

heart <- read.csv("C:/Users/jeffe/Documents/MSDS/GitHub/MSDS/DS740/Lesson 3/Heart_disease_Cleveland.csv")
summary(heart)

heart <- heart %>% 
    mutate(Sex = as.factor(Sex), 
        ChestPain = as.factor(ChestPain), 
        HighBloodSugar = as.factor(HighBloodSugar), 
        ECG = as.factor(ECG), 
        ExerAngina = as.factor(ExerAngina), 
        Slope = as.factor(Slope), 
        Thal = as.factor(Thal),
        HD = ifelse(DiseaseStatus == 0, 0, 1))

fit = glm(HD ~ ., data = heart, family = "binomial")
summary(fit)
fit1 = glm(STdepress ~. -HD, data = heart)
par(mfrow = c(2, 2))
plot(fit) # where fit is what you called the regression model
gf_histogram(STdepress ~ HD, data = heart)
fit2 = lm(log(STdepress+1) ~ .-HD, data = heart)
par(mfrow = c(2, 2))
plot(fit2)
AIC(fit1)
AIC(fit2)