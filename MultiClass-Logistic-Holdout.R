library(caret)
require(nnet)

drawStackedConfusionMat <- function(confusionMat){
  n = dim(confusionMat$table)[1]
  classificationMat = matrix(0, nrow = 2, ncol = n)
  classNames = c()
  for(i in 1:n){
    classNames = cbind(classNames, sprintf("C%i", i))
    classificationMat[1, i] = confusionMat$table[i, i]
    classificationMat[2, i] = sum(confusionMat$table[, i]) - confusionMat$table[i, i]
  }
  #classNames
  colnames(classificationMat) = classNames
  rownames(classificationMat) = c("Correct classifications", "Incorrect Classifications")
  #classificationMat
  barplot(data, 
          col = c("green","red"), 
          border="white",
          space=0.04, 
          font.axis=2, 
          xlab="Class", ylab = "Classification count")
}

build_test <- function(model){
  # the function expects any model that works with the predict function.
  # the function also expects to be that the test.csv file supplied by kaggle is 
  # present in the same folder as the file from where this call is being made.
  # OUTPUT - Predicts the data points from test.csv and writes the data to 
  # "KaggleSubmission.csv" file. - This file is ready to be submitted. 
  
  # read the test data
  kaggle_test =  read.csv('test.csv', header = TRUE)
  for(i in 1:ncol(kaggle_test)){
    kaggle_test[is.na(kaggle_test[,i]), i] <- mean(kaggle_test[,i], na.rm = TRUE)
  }
  ids = kaggle_test[1];
  kaggle_test = kaggle_test[, -1]
  predictions = predict(model, kaggle_test)
  
  submission_frame = data.frame(ids, predictions)
  names(submission_frame) = c("Id", "Response");
  
  write.csv(submission_frame, file = "KaggleSubmission.csv", row.names=FALSE)
  print("Test output successfully writtten to KaggleSubmission.csv");
}

train = read.csv('Data_NA-Cleaned_Subset-Selected.csv')
train = train[, -1]
set.seed(7)
totalRows = dim(train)[1]
trainIndices = sample( c(1:totalRows), totalRows*0.80)
finalTrain = train[trainIndices, ]
finalTest = train[-trainIndices, ]

hist(finalTrain$Response, main = "Class distribution(Training Data)")
hist(finalTest$Response, main = "Class distribution(Test Data)")

model <- multinom(Response ~ ., data = finalTrain)
prediction = predict(model, finalTest)
#predicted.classes <- model %>% predict(test.data)
confMat = confusionMatrix(data = as.factor(prediction),as.factor(finalTest$Response))
drawStackedConfusionMat(confMat)

build_test(model)
