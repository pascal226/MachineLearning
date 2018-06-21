setwd("D://R Coursera/Machine Learning")

fileURL1 <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
destfile="./traindata.csv" 
download.file(fileURL1 , destfile, method="auto")
traindata <- read.csv("traindata.csv", sep = ",", header = TRUE)

fileURL2 <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
destfile="./testdata1.csv" 
download.file(fileURL2 , destfile, method="auto")
testdata <- read.csv("testdata1.csv", sep = ",", header = TRUE)

## 

library(caret)
library(randomForest)
library(ggplot2)

inTrain  <- createDataPartition(y = traindata$classe, p = 0.75, list = F)
train    <- traindata[inTrain,]
test     <- traindata[-inTrain,]
dim(train); dim(test);dim(testdata)

NZV      <- nearZeroVar(train)
train    <- train[, -NZV]
test     <- test[, -NZV]
testdata <- testdata[, -NZV]
dim(train);dim(test);dim(testdata)


train          <- train[,colSums(is.na(train)) == 0]
test           <- test[,colSums(is.na(test)) == 0]
testdata       <- testdata[,colSums(is.na(testdata)) == 0]
dim(train);dim(test);dim(testdata)


train      = train[, -c(1:6)]
test       = test [, -c(1:6)]
testdata   = testdata [, -c(1:6)]
dim(train);dim(test);dim(testdata)

set.seed(365)
controlRF  <- trainControl(method="cv", number=3, verboseIter=FALSE)
modFitRF   <- train(classe ~ ., data=train, method="rf", trControl=controlRF)
sol        <- predict(modFitRF, newdata = test)                          
solconf    <- confusionMatrix(sol, test$classe)
solconf

predictRF  <- predict(modFitRF, testdata)
predictRF

pred <- predict(modFitRF, newdata = testdata)
testdata$predictright <- pred==testdata$problem_id


set.seed(555)
controlGBM <- trainControl(method="repeatedcv", number=3, verboseIter=TRUE)
PCFit      <- train(classe ~., data = train, method ="gbm", trControl=controlGBM, verbose = FALSE)
sol2       <- predict(PCFit, newdata = test)
sol2conf   <- confusionMatrix(sol2, test$classe)
sol2conf

predictGBM <- predict(PCFit, testdata)
predictGBM

#### Ergebnisse
solconf
predictRF
sol2conf
predictGBM


