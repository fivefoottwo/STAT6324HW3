#Nicole Ng
#HW3
#Part 1/2 of code


#Clear environment
remove(list = ls())

#load general libraries
library(dplyr)
library(ggplot2)

#load analytics libraries
library(caret)  # confusion matrix and stratified sampling
library(InformationValue) #missclassification calc
library(DMwR) #for SMOTE

#Set working directory
setwd("/Users/nicoleng/Dropbox/SMU/MASDA/STAT 6324/HW3")

#Read csv data
data1 <- read.csv("hotel_bookings.csv")

#Preliminary inspection of data
str(data1)

summary(data1)

# one-hot encoding

#define variables to keep
selectkeep <- c("is_canceled","meal","stays_in_weekend_nights","stays_in_week_nights",
                "adults","children","assigned_room_type")
data2<- subset(data1,select=selectkeep)

#define one-hot encoding function
dummy <- dummyVars(" ~ .", data=data2)

#perform one-hot encoding on data frame
data3 <- data.frame(predict(dummy, newdata=data2))

#data1 <- data3

#convert to factors
# note: need to figure out if can convert to factor before doing two-way contingency table. in sample tutorial they did it after.

data1$is_canceled <- factor(data1$is_canceled)

data1$babiesdummy <- ifelse(data1$babies > 0, 1, 0)
data1$babiesdummy <- factor(data1$babiesdummy)

#compare
#data1$roomcomp <- ifelse(data1$reserved_room_type == data1$assigned_room_type, 1, 0)
# split training and test set using stratified sampling
# note: important to maintain class ratio using stratified sampling

#EDA

#cancellation by month

monthcancelation <- (   data1
                    %>% group_by(hotel,arrival_date_month )
                    %>% summarise(total = n(),totalcancel = sum(is_canceled == 1))
                    %>% as.data.frame
)

monthcancelation$percentage <- monthcancelation$totalcancel/monthcancelation$total

depositcancelation <- (   data1
                        %>% group_by(hotel,deposit_type )
                        %>% summarise(total = n(),totalcancel = sum(is_canceled == 1))
                        %>% as.data.frame
)

depositcancelation$percentage <- depositcancelation$totalcancel/depositcancelation$total


#need to investigate non refund cancelations
nonrefund <- subset(data1,deposit_type=="Non Refund") 
table(data1$deposit_type)

#remove assigned room type L and P due to insufficient observations
data_old <- data1


data2 <- subset(data1,assigned_room_type != "L") ## need to redo because of hot encoding
data2 <- subset(data2,assigned_room_type != "P") ## need to redo because of hot encoding
table(data2$assigned_room_type)
data1 <- data2 #change data1 

# one-hot encoding

#define variables to keep in two separate dataframes
selectkeep <- c("hotel","meal","deposit_type"
                )

selectkeep2 <- c("is_canceled","distribution_channel","days_in_waiting_list","adr","previous_cancellations",
                "customer_type","total_of_special_requests","stays_in_weekend_nights",
                "stays_in_week_nights","adults","children","assigned_room_type"
)

data8<- subset(data1,select=selectkeep)
data9 <-subset(data1,select=selectkeep2)

#define one-hot encoding
dummy <- dummyVars(" ~ .", data=data8)

#perform one-hot encoding
data8b <- data.frame(predict(dummy, newdata=data8))

#merge dataset of one-hot encoded data with the rest of the dataset
data7 <- cbind(data9,data8b)
data1 <- data7
##

#for stratified sampling 
set.seed(1)
rows <- sample(nrow(data1))
data1 <- data1[rows, ]
train_index <- createDataPartition(data1$is_canceled, p = .8, list = FALSE)
training <- data1[train_index, ]  
test <- data1[-train_index, ]


#check for imbalance in training set

#fix imblance in training set
trainorigin <- training


#multi column factor
training <- training %>% mutate_if(is.character, as.factor) #magical code to fix all the issues for smote

training <- SMOTE(is_canceled ~ ., data  = training, dup_size = 0, perc.over=150, K = 5)

table(trainorigin$is_canceled)
table(training$is_canceled)

#smote takes a really long time (15 mins)
#save training and test as CSV to split code
#
write.csv(training, file=sprintf("training.csv"))
write.csv(test, file=sprintf("test.csv"))

#modeling is done on second file so that it can be repeated without running smote