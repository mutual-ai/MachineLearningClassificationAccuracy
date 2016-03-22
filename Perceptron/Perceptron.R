
library("rpart")
library("caret")
library("klaR")
library("e1071")
library("neuralnet")
library("nnet")
library("MASS")
library("glmnet")

#Load Data: Change the link in the url for each dataset
url <- "http://archive.ics.uci.edu/ml/machine-learning-databases/credit-screening/crx.data"
dataset <- read.table( file=url, header=FALSE, sep="," )

sizes <- 0.80*nrow(dataset)
splitData <- sample(1:nrow(dataset),size =sizes)
train <- dataset[splitData,]
test <- dataset[-splitData,]
x_test = test[,1:15]
y_test = test[,16]

model <- mlp(train, train, size=5, learnFuncParams=c(0.1),
             maxit=50, inputsTest=x_test, targetsTest=y_test)

predictions <- predict(model, x_test)
perceptron <- table(predictions$class, y_test)
accuracy <- (sum(diag(perceptron))/sum(perceptron))*100
accuracy