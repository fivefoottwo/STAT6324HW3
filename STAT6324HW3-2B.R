#Nicole Ng
#Part 2/2 of code

#Clear environment
remove(list = ls())

#load general libraries
library(dplyr)
library(ggplot2)

#load analytics libraries
library(caret)  # confusion matrix and stratified sampling
library(InformationValue) #missclassification calc
library(DMwR) #for SMOTE

set.seed(1)

#Set working directory
setwd("/Users/nicoleng/Dropbox/SMU/MASDA/STAT 6324/data")

#Read csv data
training <- read.csv("training.csv")
test <- read.csv("test.csv")



#model 0
# #base model using variable that were identified by previous researcher
# # note: base model works without one-hot encoding data
# #       final model uses one-hot encoding data
# model0 <- glm(is_canceled ~ meal + stays_in_weekend_nights + stays_in_week_nights +adults + children + assigned_room_type, family = "binomial", data=training)
# 
# #disable scientific notation for model summary
# options(scipen=999)
# 
# #view model summary
# summary(model0)
# 
# #note : country did not have statistically significant predictors
# 
# #predict using test set
# probabs0 <- predict(model0, test, type="response")
# 
# 
# #predicted0 <- ifelse(probabs0 > 0.5, 1, 0)
# predicted0 <- factor(predicted0)
# 
# 
# #confusion matrix
# cm <- confusionMatrix(test$is_canceled, probabs0)
# cm

#AUC and ROC
#plotROC(test$is_canceled, probabs0)


#model 1
# final model
#the goal of model is to improve on the base model
model1 <- glm(is_canceled ~ ., family = "binomial", data=training)


#disable scientific notation for model summary
options(scipen=999)

#view model summary
summary(model1)


#validate model with test set

#predict using test set
probabs <- predict(model1, test, type="response")


predicted <- ifelse(probabs > 0.5, 1, 0)
predicted <- factor(predicted)

#note: need to check if the following calc should be probabs or predicted
#

#confusion matrix
cm <- confusionMatrix(test$is_canceled, probabs)
cm

#calculate sensitivity
sensitivity(test$is_canceled, probabs)


#calculate specificity
specificity(test$is_canceled, probabs)


#calculate total misclassification error rate
misClassError(test$is_canceled, probabs, threshold=0.5)

#AUC and ROC
plotROC(test$is_canceled, probabs)



