# clear environment workspace
rm(list=ls())

library(caret)
require(data.table)
# load data
data<- fread("F:/Donloads/Train_Fyxd0t8.csv", header = T)
# Set a random seed so we can reproduce the Crop_Damages
set.seed(1234)

# Create training and validation partitions
train_in <- createDataPartition(y = data$Crop_Damage, p = 0.75, list = FALSE)
train <- data[train_in,]

validation <- data[-train_in,]
str(validation)
##Replace NA with Mean
validation[is.na(validation)] <- 29
test<-validation
str(validation)
str(test)



test2 <- fread("F:/Donloads/Test_C1XBIYq.csv", header= T)

train[is.na(train)] <- 29
test2[is.na(test2)] <- 29
data[is.na(data)] <- 29
mean(test2$Number_Weeks_Used)
str(train)
str(test2)

## Divide data set by mean
meandose<- mean(data$Number_Doses_Week,na.rm = TRUE)
meandose

data$Number_Doses_Week<- ceiling(data$Number_Doses_Week/meandose,na.rm = TRUE)
data$Number_Doses_Week

meaninsectcount<- mean(data$Estimated_Insects_Count,na.rm = TRUE)
meaninsectcount
data$Estimated_Insects_Count<-ceiling(data$Estimated_Insects_Count/meaninsectcount)
data$Estimated_Insects_Count


meanused<- mean(data$Number_Weeks_Used)
meanused
data$Number_Weeks_Used <-ceiling(data$Number_Weeks_Used/meanused)
data$Number_Weeks_Used

meanquit<- mean(data$Number_Weeks_Quit)
meanquit
data$Number_Weeks_Quit <-ceiling(data$Number_Weeks_Quit/meanused)
data$Number_Weeks_Quit




## Divide training set by mean
meandose<- mean(train$Number_Doses_Week)
meandose

train$Number_Doses_Week<- ceiling(train$Number_Doses_Week/meandose)
train$Number_Doses_Week

meaninsectcount<- mean(train$Estimated_Insects_Count)
meaninsectcount
train$Estimated_Insects_Count<-ceiling(train$Estimated_Insects_Count/meaninsectcount)
train$Estimated_Insects_Count


meanused<- mean(train$Number_Weeks_Used)
meanused
train$Number_Weeks_Used <-ceiling(train$Number_Weeks_Used/meanused)
train$Number_Weeks_Used

meanquit<- mean(train$Number_Weeks_Quit)
meanquit
train$Number_Weeks_Quit <-ceiling(train$Number_Weeks_Quit/meanused)
train$Number_Weeks_Quit




## Divide Validation set by mean
meandose<- mean(test$Number_Doses_Week)
meandose

test$Number_Doses_Week<- ceiling(test$Number_Doses_Week/meandose)
test$Number_Doses_Week

meaninsectcount<- mean(test$Estimated_Insects_Count)
meaninsectcount
test$Estimated_Insects_Count<-ceiling(test$Estimated_Insects_Count/meaninsectcount)
test$Estimated_Insects_Count


meanused<- mean(test$Number_Weeks_Used)
meanused
test$Number_Weeks_Used <-ceiling(test$Number_Weeks_Used/meanused)
test$Number_Weeks_Used

meanquit<- mean(test$Number_Weeks_Quit)
meanquit
test$Number_Weeks_Quit <-ceiling(test$Number_Weeks_Quit/meanused)
test$Number_Weeks_Quit


## Divide test set by mean
meandose<- mean(test2$Number_Doses_Week)
meandose

test2$Number_Doses_Week<- ceiling(test2$Number_Doses_Week/meandose)
test2$Number_Doses_Week

meaninsectcount<- mean(test2$Estimated_Insects_Count)
meaninsectcount
test2$Estimated_Insects_Count<-ceiling(test2$Estimated_Insects_Count/meaninsectcount)
test2$Estimated_Insects_Count


meanused<- mean(test2$Number_Weeks_Used)
meanused
test2$Number_Weeks_Used <-ceiling(test2$Number_Weeks_Used/meanused)
test2$Number_Weeks_Used

meanquit<- mean(test2$Number_Weeks_Quit)
meanquit
test2$Number_Weeks_Quit <-ceiling(test2$Number_Weeks_Quit/meanused)
test2$Number_Weeks_Quit

## As Factor
train$Crop_Type <-as.factor(train$Crop_Type)
train$Soil_Type <-as.factor(train$Soil_Type)
train$Pesticide_Use_Category <-as.factor(train$Pesticide_Use_Category)
train$Season <-as.factor(train$Season)
train$Crop_Damage <-as.factor(train$Crop_Damage)
train$Estimated_Insects_Count<-as.factor(train$Estimated_Insects_Count)
train$Number_Doses_Week<-as.factor(train$Number_Doses_Week)
train$Number_Weeks_Used<-as.factor(train$Number_Weeks_Used)
train$Number_Weeks_Quit<-as.factor(train$Number_Weeks_Quit)

test$Crop_Type <-as.factor(test$Crop_Type)
test$Soil_Type <-as.factor(test$Soil_Type)
test$Pesticide_Use_Category <-as.factor(test$Pesticide_Use_Category)
test$Season <-as.factor(test$Season)
test$Estimated_Insects_Count<-as.factor(test$Estimated_Insects_Count)
test$Number_Doses_Week<-as.factor(test$Number_Doses_Week)
test$Number_Weeks_Used<-as.factor(test$Number_Weeks_Used)
test$Number_Weeks_Quit<-as.factor(test$Number_Weeks_Quit)


test2$Crop_Type <-as.factor(test2$Crop_Type)
test2$Soil_Type <-as.factor(test2$Soil_Type)
test2$Pesticide_Use_Category <-as.factor(test2$Pesticide_Use_Category)
test2$Season <-as.factor(test2$Season)
test2$Estimated_Insects_Count<-as.factor(test2$Estimated_Insects_Count)
test2$Number_Doses_Week<-as.factor(test2$Number_Doses_Week)
test2$Number_Weeks_Used<-as.factor(test2$Number_Weeks_Used)
test2$Number_Weeks_Quit<-as.factor(test2$Number_Weeks_Quit)

data$Crop_Type <-as.factor(data$Crop_Type)
data$Soil_Type <-as.factor(data$Soil_Type)
data$Pesticide_Use_Category <-as.factor(data$Pesticide_Use_Category)
data$Season <-as.factor(data$Season)
data$Estimated_Insects_Count<-as.factor(data$Estimated_Insects_Count)
data$Number_Doses_Week<-as.factor(data$Number_Doses_Week)
data$Number_Weeks_Used<-as.factor(data$Number_Weeks_Used)
data$Number_Weeks_Quit<-as.factor(data$Number_Weeks_Quit)
data$Crop_Damage <-as.factor(data$Crop_Damage)


str(train)
str(test)
str(test2)


# remove id column so it doesn't get picked up by the random forest classifier
train2 <- train
train2$ID=NULL
train2$Season=NULL
train2$Soil_Type=NULL

test$Season=NULL
test$Soil_Type=NULL

test2$Season=NULL
test2$Soil_Type=NULL
# install randomForest package

#install.packages('randomForest')
library(randomForest)


set.seed(12)
# create a random forest model 
fit <- randomForest(as.factor(Crop_Damage ) ~ ., data=train2, importance=TRUE, ntree=100)

# create a dotchart of variable/feature importance as measured by a Random Forest
varImpPlot(fit)

# use the random forest model to create a prediction
pred <- predict(fit,test)
validate <- data.frame(id = test$ID, pred)

confusionMatrix(validate$pred, validation$Crop_Damage)

data$ID=NULL
set.seed(12)
# create a random forest model 
fit <- randomForest(as.factor(Crop_Damage ) ~ ., data=data, importance=TRUE, ntree=100)

# create a dotchart of variable/feature importance as measured by a Random Forest
varImpPlot(fit)
Crop_Damage <- predict(fit,test2)
submit <- data.frame(ID = test2$ID, Crop_Damage)
str(submit)
write.csv(submit, file = "H:\\Donloads\\LastManStanding\\eightsubmit.csv", row.names = FALSE)
