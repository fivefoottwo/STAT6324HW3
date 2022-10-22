#Clear environment
remove(list = ls())

#load general libraries
library(dplyr)
library(ggplot2)
library(tidyverse)
library(RColorBrewer)
library(vtable)

#load analytics libraries
library(car)
library(PerformanceAnalytics)
library(caret)  #confusion matrix and stratified sampling
library(InformationValue) #misclassification calc
library(DMwR) #SMOTE

#set working directory
setwd("/Users/nicoleng/Dropbox/SMU/MASDA/STAT 6324/HW3")

#Read csv data
hotel <- read.csv("hotel_bookings.csv", stringsAsFactors = TRUE)

str(hotel)
summary(hotel)

#Check for imbalanced dependent variable: is_canceled
table(hotel$is_canceled)

#Check for missing values for each variable
colSums(is.na(hotel))

#Replacing NAs in children with 0
hotel$children[is.na(hotel$children)] <- 0

#Replacing > 1 babies to 1
hotel$babies_dummy <- ifelse(hotel$babies > 0, 1, 0)
hotel$babies_dummy <- factor(hotel$babies_dummy)

#subset data
hotel <- hotel[c(-4,-6,-7,-12,-14,-15,-20,-24,-25,-29,-31,-32)]

#remove assigned room type L and P due to insufficient observations
hotel2 <- subset(hotel,assigned_room_type != "L")
hotel2 <- subset(hotel2,assigned_room_type != "P")
table(hotel2$assigned_room_type)
hotel <- hotel2 

#summary statistics for continuous variables
hotel_continuous <- hotel[c(3,5,6,7,8,12,13,15,17,19,20)]
st(hotel_continuous)

#plot bar graphs for hotel type
ggplot(hotel, aes(x=factor(hotel)))+
  geom_bar(stat="count", width=0.7, fill="steelblue")+
  theme_minimal()+
xlab('Hotel') +
  ylab('Count of bookings')

#Calculate percentages of booking canceled by month
monthcancelation <- (   hotel
                        %>% group_by(hotel,arrival_date_month )
                        %>% summarise(total = n(),totalcancel = sum(is_canceled == 1))
                        %>% as.data.frame
)

monthcancelation$percentage <- monthcancelation$totalcancel/monthcancelation$total

#Plot line graphs 
ggplot(monthcancelation, aes(x=factor(arrival_date_month, level = c('January', 'February', 'March','April',
                                                             'May', 'June', 'July', 'August',
                                                            'September','October','November','December')),
  y=percentage, group=hotel)) +
  geom_line(aes(linetype=hotel))+
  geom_point(aes(shape=hotel)) +
  xlab('Month') +
  ylab('Percentage of bookings canceled by month')

#Calculate percentages of booking canceled by change of room type
roomtypecancel <- (   hotel
                        %>% group_by(hotel,assigned_room_type)
                        %>% summarise(total = n(),totalcancel = sum(is_canceled == 1))
                        %>% as.data.frame
)

roomtypecancel$percentage <- roomtypecancel$totalcancel/roomtypecancel$total

#Plot bar graphs
ggplot(roomtypecancel, aes(x=assigned_room_type, y=percentage, fill=hotel)) + 
  geom_bar(stat="identity",position=position_dodge())+
xlab('Assigned Room Type') +
  ylab('Percentage of bookings canceled') +
  scale_fill_brewer(palette="Set1")+
  theme_minimal()

#Deposit type
table(hotel$deposit_type)

#Piechart 
x <-  c(104628, 14587, 162)
labels <-  c("No Deposit","Non Refund","Refundable")
pie(x, labels = labels, main = "Deposit Type")

#Check for multicollinearity
hotel_vis1 <- hotel[, c(3,8,9,10,11,18)]
chart.Correlation(hotel_vis1, histogram=TRUE, pch=19)

hotel_vis2 <- hotel[, c(19,22,26,28,30)]
chart.Correlation(hotel_vis2, histogram=TRUE, pch=19)

#Train 80%, test 20%
#for stratified sampling 
set.seed(1)
rows <- sample(nrow(hotel))
hotel <- hotel[rows, ]
train_index <- createDataPartition(hotel$is_canceled, p = .8, list = FALSE)
hotel_train <- hotel[train_index, ]  
hotel_test <- hotel[-train_index, ]

#Smote
hotel_train_original <- hotel_train
hotel_train <- hotel_train %>% mutate_if(is.character, as.factor) #magical code to fix all the issues for smote
hotel_train <- SMOTE(is_canceled ~ ., data  = hotel_train, dup_size = 0, perc.over=150, K = 5)
table(hotel_train_original$is_canceled)
table(hotel_train$is_canceled)

#model 0
#base model using variable that were identified by previous researcher
model0 <- glm(is_canceled ~ meal + stays_in_weekend_nights + stays_in_week_nights +adults + children + assigned_room_type, family = "binomial", data=hotel_train)

#disable scientific notation for model summary
options(scipen=999)

#view model summary
summary(model0)

#note : country did not have statistically significant predictors

#predict using test set
probabs0 <- predict(model0, hotel_test, type="response")

predicted0 <- ifelse(probabs0 > 0.5, 1, 0)
predicted0 <- factor(predicted0)

#confusion matrix
cm <- confusionMatrix(hotel_test$is_canceled, probabs0)
cm

#AUC and ROC
plotROC(hotel_test$is_canceled, probabs0)

#model 1
#the goal of model is to improve on the base model
model1 <- glm(is_canceled ~ hotel+meal + distribution_channel  +days_in_waiting_list+adr +previous_cancellations + customer_type  +deposit_type +hotel +total_of_special_requests + stays_in_weekend_nights + stays_in_week_nights +adults + children + assigned_room_type, family = "binomial", data=hotel_train)

#disable scientific notation for model summary
options(scipen=999)

#view model summary
summary(model1)

#test multicollinearity
#VIF values above 5 indicate severe multicollinearity
car::vif(model1)

#predict using test set
probabs <- predict(model1, hotel_test, type="response")

predicted <- ifelse(probabs > 0.5, 1, 0)
predicted <- factor(predicted)

#confusion matrix
cm <- confusionMatrix(hotel_test$is_canceled, probabs)
cm

#calculate sensitivity
sensitivity(hotel_test$is_canceled, probabs)

#calculate specificity
specificity(hotel_test$is_canceled, probabs)

#calculate total misclassification error rate
misClassError(hotel_test$is_canceled, probabs, threshold=0.5)

#AUC and ROC
plotROC(hotel_test$is_canceled, probabs)




