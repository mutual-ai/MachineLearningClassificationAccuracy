library("rpart")
library("caret")
library("klaR")
library("e1071")

#Load Data: Change the link in the url for each dataset
url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/pima-indians-diabetes/pima-indians-diabetes.data"
dataset <- read.table( file=url, header=FALSE, sep="," )

#Splitting data using random sampling : 80/20 split
sizes <- 0.80*nrow(dataset)
splitdata <- sample(1:nrow(dataset), size=sizes)
trainData <- dataset[splitdata,]
testData <- dataset[-splitdata,]

model <- rpart(V9 ~ ., data=trainData, method="class")
pred <- predict(model, testData, decision.values = TRUE, type = "class")

accuracy <- sum(testData[,9]==pred)/length(pred)
accuracy*100
