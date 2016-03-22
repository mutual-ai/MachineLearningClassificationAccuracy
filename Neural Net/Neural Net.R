
library("rpart")
library("caret")
library("klaR")
library("e1071")
library("neuralnet")
library("nnet")
library("MASS")

#Load Data: Change the link in the url for each dataset
url <- "http://archive.ics.uci.edu/ml/machine-learning-databases/credit-screening/crx.data"
dataset <- read.table( file=url, header=FALSE, sep="," )

sizes <- 0.80*nrow(dataset)
splitData <- sample(1:nrow(dataset),size =sizes)
train <- dataset[splitData,]
test <- dataset[-splitData,]

model <- nnet(V16 ~.,data=train,size=1,maxit=10000,decay=.001,trace=FALSE)
predictions = predict(model,test, type="class")

accuracy <- mean(predictions == test[,16])

accuracy*100