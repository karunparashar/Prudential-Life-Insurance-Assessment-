# read the data
data = read.csv('Final-Train.csv')

k = 5
set.seed(11)
rep = rep(1:k, length = nrow(data))
folds = sample(rep)
table(folds)
missClassifications = rep(0, k)
for(k in 1:10){
  foldTrainingData = data[folds != k, ]
  foldTestData  = data[folds == k, ]
  
  # train for the kth fold.
  model <- multinom(Response ~ ., data = foldTrainingData)
  prediction = predict(model, foldTestData)
  #predicted.classes <- model %>% predict(test.data)
  confusionMatrix(data = as.factor(prediction),as.factor(foldTestData$Response))
}
