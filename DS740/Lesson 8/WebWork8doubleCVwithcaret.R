# define data frame heart as in WebWork Lesson 8, Problem 4

input = read.csv("Heart_Disease_Cleveland.csv")
names(input)
heart = input[,c(1,4,5,8,10)]
heart$HD = rep(0, length(input$DiseaseStatus))
heart$HD[which(input$DiseaseStatus > 0)] = 1
heart$HD = factor(heart$HD)

# read in libraries
library(MASS)

###################### More consistent way (with caret) ######################

library(caret)
library(dplyr)

##### model assessment OUTER 10-fold CV (with model selection INNER 10-fold CV as part of model-fitting) #####

xy.out = heart
n.out = dim(xy.out)[1]

#define the cross-validation splits 
k.out = 10 
groups.out = rep(1:k.out,length=n.out)  #produces list of group labels
set.seed(8)
cvgroups.out = sample(groups.out,n.out)  #orders randomly, with seed (8) 

allpredictedCV.out = rep(NA,n.out)

### model assessment OUTER shell ###
for (j in 1:k.out)  {  #be careful not to re-use loop indices
  groupj.out = (cvgroups.out == j)
  
  # define the training set for outer loop
  trainxy.out = xy.out[!groupj.out,]
  
  #define the validation set for outer loop
  testxy.out = xy.out[groupj.out,]
  testx2.out <- testxy.out %>% select(MaxHeartRate, STdepress)
  testx5.out <- testxy.out %>% select(-HD)
  
  ModelList = list(HD ~ MaxHeartRate + STdepress,HD ~.)
  
  #  set.seed(8)
  training = trainControl(method = "cv", number = 10)
  
  ##############################################
  ###   model selection on trainxy.out       ###
  ##############################################
  
  # cross-validation of LDA model two predictors
  fit_caret_LDA2 = train(ModelList[[1]],
                         data = trainxy.out,   # important, only run caret on training data from split in out loop
                         method = "lda",
                         trControl = training)
  # cross-validation of LDA model full
  fit_caret_LDA5 = train(ModelList[[2]],
                         data = trainxy.out,   # important, only run caret on training data from split in out loop
                         method = "lda",
                         trControl = training)
  # cross-validation of QDA model two predictors
  fit_caret_QDA2 = train(ModelList[[1]],
                         data = trainxy.out,   # important, only run caret on training data from split in out loop
                         method = "qda",
                         trControl = training)
  # cross-validation of QDA model full
  fit_caret_QDA5 = train(ModelList[[2]],
                         data = trainxy.out,   # important, only run caret on training data from split in out loop
                         method = "qda",
                         trControl = training)
  
  # cross-validation of logistic model two predictors
  fit_caret_logistic2 = train(ModelList[[1]],
                              data = trainxy.out,   # important, only run caret on training data from split in out loop
                              method = "glm",
                              trControl = training)
  # cross-validation of logistic model full
  fit_caret_logistic5 = train(ModelList[[2]],
                              data = trainxy.out,   # important, only run caret on training data from split in out loop
                              method = "glm",
                              trControl = training)
  
  
  ############# identify selected model to fit to full data #############
  # all best models
  all_Accuracy = c(fit_caret_LDA2$results$Accuracy,
                   fit_caret_LDA5$results$Accuracy,
                   fit_caret_QDA2$results$Accuracy,
                   fit_caret_QDA5$results$Accuracy,
                   fit_caret_logistic2$results$Accuracy,
                   fit_caret_logistic5$results$Accuracy)
  all_Error = 1-all_Accuracy
  bestmodels = (1:6)[all_Error == min(all_Error)]
  bestmodel = ifelse(length(bestmodels)==1,bestmodels,sample(bestmodels,1))
  print(all_Error)
  print(paste("Best model at outer loop",j,"is",bestmodel))
  
  ##############################################
  ###   resulting in bestmodels              ###
  ##############################################
  
  #relabel as original values, not factor levels
  if (bestmodel == 1)   predictvalid = as.numeric(predict(fit_caret_LDA2$finalModel, newdata = testx2.out)$class)-1
  if (bestmodel == 2)   predictvalid = as.numeric(predict(fit_caret_LDA5$finalModel, newdata = testx5.out)$class)-1
  if (bestmodel == 3)   predictvalid = as.numeric(predict(fit_caret_QDA2$finalModel, newdata = testx2.out)$class)-1
  if (bestmodel == 4)   predictvalid = as.numeric(predict(fit_caret_QDA5$finalModel, newdata = testx5.out)$class)-1
  if (bestmodel == 5)   predictvalid = as.numeric(predict(fit_caret_logistic2$finalModel, newdata = testx2.out, type = "response")>0.5)
  if (bestmodel == 6)   predictvalid = as.numeric(predict(fit_caret_logistic5$finalModel, newdata = testx5.out, type = "response")>0.5)
  #print(length(predictvalid))  
  allpredictedCV.out[groupj.out] = predictvalid
  print(Sys.time())
}

#Purpose of double cross-validation:
# assessment - what proportion of the cross-validated classifications (valid predictions of 
# new observations, based on model selected using the entire model-selection process)
# match the actual observations?
table(heart$HD,allpredictedCV.out)
CV10.out = sum(heart$HD!=allpredictedCV.out)/n.out
p.out = 1-CV10.out; p.out  

