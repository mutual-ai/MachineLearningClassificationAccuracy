library("klaR")
library("caret")
library("e1071")

#Load Data: Change the link in the url for each dataset
url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/pima-indians-diabetes/pima-indians-diabetes.data"
dataset <- read.table(file=url, header = FALSE, sep = ",")

#Splitting data using random sampling : 80/20 split
sizes <- 0.80*nrow(dataset)
train <- sample(1:nrow(dataset),size =sizes)
trainingData <- dataset[train,]
testData <- dataset[-train,]

#Training a Naive Bayes model: Values change based on the no. of attributes
model <- NaiveBayes(V9~., data=trainingData)
x_test <- testData[,1:8]
y_test <- testData[,9]
predictions <- predict(model, x_test)

#Calculating accuracy
tab <- table(predictions$class, y_test)  
accuracy <- sum(diag(tab))/sum(tab)
accuracy*100
