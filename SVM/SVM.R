library("rpart")
library("caret")
library("klaR")
library("e1071")

#Load Data: Change the link in the url for each dataset
url <- "http://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data"
dataset <- read.table( file=url, header=FALSE, sep="," )

sizes <- 0.80*nrow(dataset)
splitData <- sample(1:nrow(dataset),size =sizes)
trainingData <- dataset[splitData,]
testData <- dataset[-splitData,]

model <- svm(Class~., data = trainingData,cost = 100,gamma=10) 


prediction <- predict(model, testData[,-15])

conf <- table(pred = prediction, true = testData[,15])

accuracy <-sum(diag(conf))/sum(conf)

accuracy*100