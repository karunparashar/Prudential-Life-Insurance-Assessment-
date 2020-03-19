rm(list = ls())
train = read.csv('train.csv')
head(train)
library(caret)
sapply(train, function(x) sum(is.na(x)))
sapply(train_imp, function(x) sum(is.na(x)))
which(is.na(train$Employment_Info_4))
summary(train$Employment_Info_4)
cor(train$Response,train$Employment_Info_4)
rm(train_imp)
#removing the columns with NAs greater than 6000(more than 10% of data)
train_imp=train
x = list()
count =1
for (i in 1:128){
  if (sum(is.na(train[,i]))>6000){
   x[count] = i
   count = count+1
  } 
}
x = unlist(x)
train_imp = train[,-x]
train_imp = na.omit(train_imp)
train_imp = train_imp[,-1]


set.seed(7)
smpl = sample(1:nrow(train_imp),nrow(train_imp)/5)
train_sub =train_imp[-smpl,] 
test_sub = train_imp[smpl,] 
str(train_imp$Product_Info_2)
train_imp$Product_Info_2 = as.numeric(train_imp$Product_Info_2 )
lm_model = lm(Response~.,data = train_sub)
lm_summary = summary(lm_model)
lm_coef = as.data.frame(lm_summary$coefficients)
lm_coef_p = as.data.frame(lm_coef[lm_coef[,4]<0.05,4])
name_coef = as.data.frame(row.names(lm_coef)[lm_coef[,4] < 0.05])
lm_pred = predict(lm_model,test_sub)
lm_mse = mean((lm_pred-test_sub$Response)^2)





####################################
###Cross validation
####################################
set.seed(123)
tr_ctl = trainControl(method = 'cv', number = 10)
model_train = train(Response~.,data = train_sub, method = 'knn', trControl = tr_ctl)
model_train

####################################
###LDA
####################################
lda_model = glm(Response~., data =train_sub,family = 'gaussian' )
lda_pred = predict(logistic_model,test_sub)
lda_test_mse = mean((lda_pred-test_sub$Response)^2)

####################################
###PCR
####################################
prc = prcomp(train_imp[,-1])
pcc = pca(train_imp)
library(pls)
train_matrix = model.frame(Response~.,data = train_imp)
pcr_fit = pcr(Response~., data = train_matrix,method = 'none',scale =FALSE)
pls_fit = plsr(Response~., data = train_imp,method = 'CV',scale =FALSE)

chk = data.frame(prc$rotation)
str(train_imp)
set.seed(7443)
smpl = sample(1:nrow(train_imp),nrow(train_imp)/6)
train_sub =train_imp[-smpl,] 
test_sub = train_imp[smpl,] 
scaled_train = scale(train_sub)
library(class)
knn_model = knn(train_sub,test_sub,train_sub$Response,k=9)
confusionMatrix(data = as.factor(knn_model),reference = as.factor(test_sub$Response))





