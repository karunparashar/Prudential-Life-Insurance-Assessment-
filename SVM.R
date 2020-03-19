rm(list = ls())
library(ISLR)
library(e1071) #install.packages("e1071")
library(caret)

# load the data
train = read.csv('Data_NA-Cleaned_Subset-Selected.csv')

# Divide into test and training
set.seed(1)
totalRows = dim(train)[1]
trainIndices = sample( c(1:totalRows), totalRows*0.75)
finalTrain = train[trainIndices, ]
finalTrain = finalTrain[1:5000, ]
finalTest = train[-trainIndices, ]
finalTest = finalTest[1:5000, ]

#svmfit = svm(Response ~ ., data = finalTrain, kernel="linear", cost = 10, scale= FALSE, type = "C")
#print(svmfit)
#prediction = predict(svmfit, finalTest)
#confusionMatrix(data = as.factor(prediction),as.factor(finalTest$Response))
#saveRDS(final_model, "./final_model.rds")

finalTrain = train[trainIndices, ]
finalTest = train[trainIndices, ]
svmfit = svm(Response ~ ., data = finalTrain, kernel="linear", cost = 10, scale= FALSE, type = "C")
print(svmfit)
prediction = predict(svmfit, finalTest)
confusionMatrix(data = as.factor(prediction),as.factor(finalTest$Response))



