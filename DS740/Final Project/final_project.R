#1. check feedback - done
#2. decide on R - done
#3. Read the data into R or another program and do an exploratory data analysis 
#   and data cleaning.

setwd("C:/Users/jeffe/Documents/MSDS/GitHub/MSDS/DS740/Final Project")

# Initiate Packages
library(dplyr)
library(corrplot)
library(RColorBrewer)
library(ggplot2)
library(caret)
library(pROC)
library(nnet)
library(NeuralNetTools)
library(ggformula)
library(reshape2)
library(doParallel)
library(randomForest)
library(gridExtra)

# Read in data and combine sets
air.travel.1 <- read.csv("air_travel_test.csv")
air.travel.2 <- read.csv("air_travel_train.csv")
air.travel <- rbind(air.travel.1,air.travel.2)

# Remove NAs
air.travel.data <- na.omit(air.travel)

# Remove id Column
air.travel.data <- air.travel.data %>%
  select(-id)

# Fix data types
air.travel.data <- air.travel.data %>%
  mutate(Gender = as.factor(Gender),
         Customer.Type = as.factor(Customer.Type),
         Type.of.Travel = as.factor(Type.of.Travel),
         Class = as.factor(Class),
         satisfaction = as.factor(satisfaction))

# Correlations
mycor = cor(select_if(air.travel.data, is.numeric), 
            use = "pairwise.complete.obs")
corrplot(mycor, type = "upper", order = "hclust", 
         col = rev(brewer.pal(n = 8, name = "RdYlBu")))

# Train/Test split
ind.full <- sample(2, nrow(air.travel.data), replace = TRUE, prob = c(0.7, 0.3))
air.travel.full.train <- air.travel.data[ind.full==1,]
air.travel.full.test <- air.travel.data[ind.full==2,]

# Subgroups
air.travel.data.Loyal <- air.travel.data %>%
  filter(Customer.Type == "Loyal Customer")
air.travel.data.disloyal <- air.travel.data %>%
  filter(Customer.Type == "disloyal Customer")
air.travel.data.Business.travel <- air.travel.data %>%
  filter(Type.of.Travel == "Business travel")
air.travel.data.Personal <- air.travel.data %>%
  filter(Type.of.Travel == "Personal Travel")
air.travel.data.Business.class <- air.travel.data %>%
  filter(Class == "Business")
air.travel.data.Eco.class <- air.travel.data %>%
  filter(Class == "Eco")
air.travel.data.Eco.Plus.class <- air.travel.data %>%
  filter(Class == "Eco Plus")
air.travel.data.Female <- air.travel.data %>%
  filter(Gender == "Female")
air.travel.data.Male <- air.travel.data %>%
  filter(Gender == "Male")



p1 <- ggplot(data = air.travel.data, aes(x = satisfaction, fill = Class)) +
  geom_bar(position = "dodge") +
  labs(title = "Level of Satisfaction by Class")

p2 <- ggplot(data = air.travel.data, aes(x = satisfaction, fill = Type.of.Travel)) +
  geom_bar(position = "dodge") +
  labs(title = "Level of Satisfaction by Type of Travel")

p3 <- ggplot(data = air.travel.data, aes(x = satisfaction, fill = Gender)) +
  geom_bar(position = "dodge") +
  labs(title = "Level of Satisfaction by Gender")

p4 <- ggplot(data = air.travel.data, aes(x = satisfaction, fill = Customer.Type)) +
  geom_bar(position = "dodge") +
  labs(title = "Level of Satisfaction by Customer Type")

grid.arrange(p1, p2, p3, p4, ncol = 2)

### Random Forest
# system.time({
# set.seed(99)
# c1 <- makeCluster(15)
# registerDoParallel(c1)
# data_used = air.travel.data
# ctrl = trainControl(method = "cv", number = 5)
# air.travel.tree = train(satisfaction ~ .,
#                         data = data_used,
#                         method = "rf",
#                         tuneGrid = expand.grid(mtry = c(1,2,3,4,5,6,7,8,9,10,15,20,22)),
#                         trControl = ctrl)
# stopCluster(c1)
# })
# bestTune -- mtry = 9

### Main Forest
system.time({
air.travel.forest = randomForest(satisfaction ~ ., data = air.travel.full.train,
                              mtry = 9, importance = TRUE,
                              ntree = 300)
})

# Variable Importance
p5 <- varImpPlot(air.travel.forest, type = 1)


# Partial Dependence Plots
# par(mfrow = c(1,3))
partialPlot(air.travel.forest, pred.data = air.travel.data, 
            x.var = Inflight.wifi.service, which.class = "satisfied")
partialPlot(air.travel.forest, pred.data = air.travel.data, 
            x.var = Online.boarding, which.class = "satisfied")
partialPlot(air.travel.forest, pred.data = air.travel.data, 
            x.var = Checkin.service, which.class = "satisfied")

partialPlot(air.travel.forest, pred.data = air.travel.data, 
            x.var = Gender, which.class = "satisfied")
partialPlot(air.travel.forest, pred.data = air.travel.data, 
            x.var = Customer.Type, which.class = "satisfied")
partialPlot(air.travel.forest, pred.data = air.travel.data, 
            x.var = Type.of.Travel, which.class = "satisfied")
partialPlot(air.travel.forest, pred.data = air.travel.data, 
            x.var = Class, which.class = "satisfied")


partialPlot(air.travel.forest, pred.data = air.travel.data, 
            x.var = Departure.Arrival.time.convenient, 
            which.class = "satisfied")
partialPlot(air.travel.forest, pred.data = air.travel.data, 
            x.var = Ease.of.Online.booking, which.class = "satisfied")
partialPlot(air.travel.forest, pred.data = air.travel.data, 
            x.var = Gate.location, which.class = "satisfied")
partialPlot(air.travel.forest, pred.data = air.travel.data, 
            x.var = Food.and.drink, which.class = "satisfied")

partialPlot(air.travel.forest, pred.data = air.travel.data, 
            x.var = Seat.comfort, which.class = "satisfied")
partialPlot(air.travel.forest, pred.data = air.travel.data, 
            x.var = Inflight.entertainment, which.class = "satisfied")
partialPlot(air.travel.forest, pred.data = air.travel.data, 
            x.var = On.board.service, which.class = "satisfied")
partialPlot(air.travel.forest, pred.data = air.travel.data, 
            x.var = Leg.room.service, which.class = "satisfied")
partialPlot(air.travel.forest, pred.data = air.travel.data, 
            x.var = Baggage.handling, which.class = "satisfied")
partialPlot(air.travel.forest, pred.data = air.travel.data, 
            x.var = Inflight.service, which.class = "satisfied")
partialPlot(air.travel.forest, pred.data = air.travel.data, 
            x.var = Cleanliness, which.class = "satisfied")
partialPlot(air.travel.forest, pred.data = air.travel.data, 
            x.var = Age, which.class = "satisfied")
partialPlot(air.travel.forest, pred.data = air.travel.data, 
            x.var = Flight.Distance, which.class = "satisfied")
partialPlot(air.travel.forest, pred.data = air.travel.data, 
            x.var = Departure.Delay.in.Minutes, which.class = "satisfied")
partialPlot(air.travel.forest, pred.data = air.travel.data, 
            x.var = Arrival.Delay.in.Minutes, which.class = "satisfied")



# prediction and confmat
air.travel.forest.test.predict = predict(air.travel.forest, 
                                         air.travel.full.test)
confmat_rf = confusionMatrix(air.travel.forest.test.predict,
                          air.travel.full.test$satisfaction)
confmat_rf

# AUC
auc.predict = predict(air.travel.forest, air.travel.full.test, 
                      type = "prob")[,2]
roc <- roc(air.travel.full.test$satisfaction, auc.predict)
auc <- auc(roc)
plot.roc(roc)
text(auc)
###

### Subset Forests
par(mfrow = c(2,1))
system.time({
  air.travel.Loyal.forest = randomForest(satisfaction ~ ., 
                                         data = air.travel.data.Loyal,
                                         mtry = 9, importance = TRUE,
                                         ntree = 300)
  varImpPlot(air.travel.Loyal.forest, type = 1)
})

system.time({
  air.travel.disloyal.forest = randomForest(satisfaction ~ ., 
                                            data = air.travel.data.disloyal,
                                            mtry = 9, importance = TRUE,
                                            ntree = 300)
  varImpPlot(air.travel.disloyal.forest, type = 1)
})

system.time({
  air.travel.Business.travel.forest = randomForest(satisfaction ~ ., 
                                                   data = air.travel.data.Business.travel,
                                                   mtry = 9, importance = TRUE,
                                                   ntree = 300)
  varImpPlot(air.travel.Business.travel.forest, type = 1)
})

system.time({
  air.travel.Personal.forest = randomForest(satisfaction ~ ., 
                                            data = air.travel.data.Personal,
                                            mtry = 9, importance = TRUE,
                                            ntree = 300)
  varImpPlot(air.travel.Personal.forest, type = 1)
})

system.time({
  air.travel.Business.class.forest = randomForest(satisfaction ~ ., 
                                                  data = air.travel.data.Business.class,
                                                  mtry = 9, importance = TRUE,
                                                  ntree = 300)
  varImpPlot(air.travel.Business.class.forest, type = 1)
})

system.time({
  air.travel.Eco.class.forest = randomForest(satisfaction ~ ., 
                                             data = air.travel.data.Eco.class,
                                             mtry = 9, importance = TRUE,
                                             ntree = 300)
  varImpPlot(air.travel.Eco.class.forest, type = 1)
})

system.time({
  air.travel.Eco.Plus.class.forest = randomForest(satisfaction ~ ., 
                                                  data = air.travel.data.Eco.Plus.class,
                                                  mtry = 9, importance = TRUE,
                                                  ntree = 300)
  varImpPlot(air.travel.Eco.Plus.class.forest, type = 1)
})

system.time({
  air.travel.Female.forest = randomForest(satisfaction ~ ., 
                                          data = air.travel.data.Female,
                                          mtry = 9, importance = TRUE,
                                          ntree = 300)
  varImpPlot(air.travel.Female.forest, type = 1)
})

system.time({
  air.travel.Male.forest = randomForest(satisfaction ~ ., 
                                        data = air.travel.data.Male,
                                        mtry = 9, importance = TRUE,
                                        ntree = 300)
  varImpPlot(air.travel.Male.forest, type = 2)
})
varMale <- varImp(air.travel.Male.forest)

### Neural Network
# system.time({
# c1 <- makeCluster(15)
# registerDoParallel(c1)
# set.seed(99)
# ctrl = trainControl(method = "cv", number = 5)
# fit_air.travel.data = train(satisfaction ~ ., data = air.travel.data,
#                             method = "nnet",
#                             tuneGrid = expand.grid(size = seq(1,10, by = 1),
#                                                    decay = seq(0.1,1,by = 0.1)),
#                             skip = FALSE,
#                             trace = FALSE,
#                             preProc = c("center", "scale"),
#                             maxit = 5000,
#                             trControl = ctrl)
# stopCluster(c1)
# })
# bestTune -- decay = 0.3, size = 10

system.time({
c1 <- makeCluster(15)
registerDoParallel(c1)
set.seed(99)
ctrl = trainControl(method = "cv", number = 5)
fit_air.travel.data = train(satisfaction ~ ., data = air.travel.full.train,
                            method = "nnet",
                            tuneGrid = expand.grid(size = 10,
                                                   decay = 0.3),
                            skip = FALSE,
                            trace = FALSE,
                            preProc = c("center", "scale"),
                            maxit = 1000,
                            trControl = ctrl)
stopCluster(c1)
})


### Predict and confmat
fit_air.travel.data.predict = predict(fit_air.travel.data,
                                      air.travel.full.test)

confmat_nnet = confusionMatrix(fit_air.travel.data.predict,
                          air.travel.full.test$satisfaction)
confmat_nnet


summary(fit_air.travel.data)
par(mar = c(0.1, 0.1, 0.1, 0.1), mfrow = c(1,1))
plotnet(fit_air.travel.data)

olden(fit_air.travel.data) + 
  labs(title = "Variable Importance") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))