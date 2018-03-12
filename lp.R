
library(caret)
data <- read.csv('D:/1 DataScience Code and Data/1. AV/LoanPrediction/train_u6lujuX_CVtuZ9i.csv', h=TRUE)

str(data)
head(data)
data[is.na(data)] <- 0
# Set a random seed so we can reproduce the Results
set.seed(1234)

# Create training and testing partitions
train_in <- createDataPartition(y = data$Loan_Status, p = 0.75, list = FALSE)
training <- data[train_in,]
testing <- data[-train_in,]


##################  ####################
# trainControl for Boosted Logisitic Regression
fitControl <- trainControl(method = 'repeatedcv', repeats = 5,
                           number = 5, verboseIter = T)

# Run a Boosted logisitic regression over the training set
log.fit <- train(Loan_Status ~ .,  data = training, 
                 method = "LogitBoost", trControl = fitControl,
                 tuneLength = 5)

# Predict the testing target
log.predict <- predict(log.fit, testing[,-13])

confusionMatrix(log.predict, testing$Loan_Status)
####################### Random Forest ########################

# trainControl for Random Forest
fitControl = trainControl(method = "repeatedcv", repeats = 5,
                          number = 5, verboseIter = T)

# Run a Random Forest classification over the training set
rf.fit <- train(Loan_Status ~ .,  data = training, method = "rf",
                importance = T, trControl = fitControl,
                tuneLength = 5)

# Predict the testing target
rf.predict <- predict(rf.fit, testing[,-13])

confusionMatrix(rf.predict, testing$Loan_Status)

plot(varImp(rf.fit))

