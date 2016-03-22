#-------------------------#
#Assignment-3#vxv140330#
#-------------------------#

#required packages: Run this part only once
install.packages("rpart")
install.packages("caret")
install.packages("klaR")
install.packages("e1071")
install.packages("neuralnet")
install.packages("nnet")
install.packages("MASS")
install.packages("glmnet")
install.packages("RSNNS")
install.packages("lattice")

#loading libaries
library("rpart")
library("caret")
library("klaR")
library("e1071")
library("neuralnet")
library("nnet")
library("MASS")
library("glmnet")
library("RSNNS")
library("lattice")

url <- c("http://archive.ics.uci.edu/ml/machine-learning-databases/car/car.data","http://archive.ics.uci.edu/ml/machine-learning-databases/credit-screening/crx.data","http://archive.ics.uci.edu/ml/machine-learning-databases/tic-tac-toe/tic-tac-toe.data","https://archive.ics.uci.edu/ml/machine-learning-databases/spambase/spambase.data","http://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data")
percent = c(0.8,0.9)
cids <- c(7,16,10,58,15)
b <- c(6,15,9,57,14)
formulae <- c("V7 ~ .","V16 ~ .","V10 ~ .","V58 ~ .","V15 ~ .")
for(k in 1:5) 
{
  cat("Dataset",k,"\n")
  data <- read.table( file=url[k], header=FALSE, sep="," )
  i = 1
  c_id <- as.integer(cids[k])
  c_ids <- as.integer(b[k])
  form <- as.formula(formulae[k])
  for(j in 1:2) 
  {
    cat("c_id =",c_id,";c_ids =",c_ids,";form =",formulae[k],";")
    
    splitvar <- percent[i]
    splitdata <- sample(1:nrow(data), size=splitvar*nrow(data))
    train <- data[splitdata,]
    test <- data[-splitdata,]
    cat("Split =",splitvar,"\n")
    
    #Decision Trees
    method = "Decision Tree"
    model <- rpart(form, data=train, method="class")
    pred <- predict(model, test, decision.values = TRUE, type = "class")
    accuracy <- (sum(test[,c_id]==pred)/length(pred))*100
    cat("   Model : ", method,"; Accuracy : ", accuracy,"\n")
    
    #NaiveBayes
    method = "Naive Bayes"
    model <- NaiveBayes(form, data=train, method="class")
    x_test <- test[,1:c_ids]
    y_test <- test[,c_id]
    predictions <- predict(model, x_test)
    tab <- table(predictions$class, y_test)
    accuracy <- (sum(diag(tab)) / sum(tab))*100
    cat("   Model : ", method,"; Accuracy : ", accuracy,"\n")
    
    #SVM
    method = "Support Vector Machine"
    model <- svm(form, data = train, cost = 100, gamma=1)
    prediction <- predict(model, test[,-c_id])
    conf <- table(pred = prediction, true = test[,c_id])
    accuracy <-(sum(diag(conf))/sum(conf))*100
    cat("   Model : ", method,"; Accuracy : ", accuracy,"\n")
    
    #Neural Nets
    method = "Neurl Nets"
    model <- nnet(form, data=train, size=1, maxit=10000, decay=.001, trace=FALSE)
    predictions = predict(model, test, type="class")
    accuracy <- (mean(predictions == test[,c_id]))*100
    cat("   Model : ", method,"; Accuracy : ", accuracy,"\n")
    
    #Perceptron
    method = "Perceptron"
    x_test <- test[,1:b[1]]
    y_test <- test[,c_id]
    model <- mlp(train, train, size=5, learnFuncParams=c(0.1), maxit=50, inputsTest=x_test, targetsTest=y_test)
    predictions <- predict(model, x_test)
    perceptron <- table(predictions$class, y_test)
    accuracy <- (sum(diag(perceptron))/sum(perceptron))*100
    cat("   Model : ", method,"; Accuracy : ", accuracy,"\n")
    
    i = i + 1
  }
}