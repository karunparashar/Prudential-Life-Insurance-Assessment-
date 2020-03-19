rm(list = ls())

"train = read.csv('train.csv')
head(train)
sapply(train, function(x) sum(is.na(x)))
rm(train_imp)
#removing the columns with NAs greater than 6000(more than 10% of data)
train_imp=train[,-1]
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
str(train_imp$Product_Info_2)
train_imp$Product_Info_2 = as.numeric(train_imp$Product_Info_2)"
#write.csv(train_imp, "Final-Train.csv")

#install.packages("nnet")
library(nnet)
data = read.csv('Final-Train.csv')

###################################################################################
# Subset selection in the insurancedata
###################################################################################

install.packages("leaps")
library(leaps)
insdata<- data
dim(insdata)
names(insdata)
regfit.forward.full <- regsubsets(Response~. , data=insdata ,method="forward" , nvmax=116)
my_sum<-summary(regfit.forward.full)
my_sum
my_sum$bic
plot(my_sum$bic,xlab="Number of variables",ylab="Bic" ,type="l") # model size 35 
plot(my_sum$cp,xlab="Number of variables",ylab="Cp" ,type="l") # model size 35
coefi<- coef(regfit.forward.full , id = 35)


#################################################################################
data = read.csv('Final-Train.csv')
install.packages("rpart")
library(rpart)
library(MASS)
insdata<- data

model.control<-rpart.control(minsplit=1000,xval=10,cp=0)
fit.insdata<- rpart(Response~. , data=insdata,method="class",control=model.control)
names(fit.insdata)
fit.insdata$cptable
x11()
plot(fit.insdata,uniform=T,compress=T)
text(fit.insdata,cex=.8)

insdata<- data
v =c(1:35);
v[1]= which(colnames(insdata)=="Product_Info_4")
v[2]= which(colnames(insdata)=="Ins_Age")
v[3]= which(colnames(insdata)=="BMI")
v[4]= which(colnames(insdata)=="Employment_Info_3")
v[5]= which(colnames(insdata)=="InsuredInfo_1")
v[6]= which(colnames(insdata)=="InsuredInfo_2")
v[7]= which(colnames(insdata)=="InsuredInfo_5")
v[8]= which(colnames(insdata)=="InsuredInfo_6")
v[9]= which(colnames(insdata)=="InsuredInfo_7")
v[10]= which(colnames(insdata)=="Insurance_History_1")
v[11]= which(colnames(insdata)=="Insurance_History_2")
v[12]= which(colnames(insdata)=="Insurance_History_3")
v[13]= which(colnames(insdata)=="Medical_History_3")
v[14]= which(colnames(insdata)=="Medical_History_4")
v[15]= which(colnames(insdata)=="Medical_History_5")
v[16]= which(colnames(insdata)=="Medical_History_7")
v[17]= which(colnames(insdata)=="Medical_History_11")
v[18]= which(colnames(insdata)=="Medical_History_13")
v[19]= which(colnames(insdata)=="Medical_History_17")
v[20]= which(colnames(insdata)=="Insurance_History_18")
v[21]= which(colnames(insdata)=="Insurance_History_19")
v[22]= which(colnames(insdata)=="Medical_History_20")
v[23]= which(colnames(insdata)=="Medical_History_23")
v[24]= which(colnames(insdata)=="Medical_History_27")
v[25]= which(colnames(insdata)=="Medical_History_28")
v[26]= which(colnames(insdata)=="Medical_History_30")
v[27]= which(colnames(insdata)=="Medical_History_31")
v[28]= which(colnames(insdata)=="Medical_History_33")
v[29]= which(colnames(insdata)=="Medical_History_35")
v[30]= which(colnames(insdata)=="Medical_History_40")
v[31]= which(colnames(insdata)=="Medical_Keyword_3")
v[32]= which(colnames(insdata)=="Medical_Keyword_9")
v[33]= which(colnames(insdata)=="Medical_Keyword_25")
v[34]= which(colnames(insdata)=="Medical_Keyword_38")
v[35]= which(colnames(insdata)=="Medical_Keyword_41")
install.packages("caret")
library(caret)
best_model_df = cbind(insdata[,v],insdata$Response)

names(best_model_df)[36] = "Response"

# store the data after subset-selection has been done
write.csv(best_model_df, "Data_NA-Cleaned_Subset-Selected.csv")

k=30
n <- nrow(best_model_df)
set.seed(123)
datay=best_model_df[,36] #response variable

#partition the data into K subsets
f <- ceiling(n/k)
s <- sample(rep(1:k, f), n)  
#generate indices 1:10 and sample n of them  
# K fold cross-validated error

CV=NULL
CVMSE=NULL

for (i in 1:k) { #i=1
  test.index <- seq_len(n)[(s == i)] #test data
  train.index <- seq_len(n)[(s != i)] #training data
  instesty <- best_model_df[test.index, 36]
  model.control<-rpart.control(minsplit=1000,xval=10,cp=0)
  best_subset<- rpart(Response~. , data=best_model_df[train.index,],method="class",control=model.control)
  best_subset$cptable
  plot(best_subset,uniform=T,compress=T)
  text(best_subset,cex=.8)
  names(best_model_df)
  plot(best_subset$cptable[,4],main="Cp for model selection" ,ylab ="cp error")
  
  
  ###########################################
  # Prune the tree
  ###########################################
  min.cp= which.min(best_subset$cptable[,4])
  pruned.fit.ins=prune(best_subset,cp=best_subset$cptable[min.cp,1])
  
  ##########################################
  #Plot the pruned and full tree
  ##########################################
  
  plot(pruned.fit.ins,compress=T,main="Pruned tree for Insurance data")
  text(pruned.fit.ins,cex=.8)
  
  pred_train<-predict(pruned.fit.ins,newdata=best_model_df[test.index,],type="class")
  accuracy <- ifelse(pred_train == best_model_df[test.index,]$Response ,1,0)
  percetageaccuracy<-sum(accuracy)/nrow(best_model_df[test.index,])
  print(accuracy)
  CV=c(CV,percetageaccuracy)
  MSE_Error<-1/length(best_model_df[test.index,]$Response)*(sum(abs(as.numeric(pred_train)-as.numeric(best_model_df[test.index,]$Response)^2)))
  CVMSE=c(CVMSE,MSE_Error)
  #table(pred_train,insdata$Response)
  confusionMatrix(data=as.factor(pred_train),reference=as.factor(instesty))
  
}

mean(CV)
mean(CVMSE)
###################################################################################
#Random Forest
##################################################################################
install.packages("randomForest")
library(randomForest)

set.seed(1234)
index=sample(1:nrow(best_model_df),.20*nrow(best_model_df))
datay=best_model_df[,36] #response variable
library(MASS)

CV=NULL
testdata<- best_model_df[index,]
dim(testdata)
traindata<-best_model_df[-index,]
truey <- best_model_df[index, 36]
traindata$Response<-as.factor(traindata$Response)
rf.fit<-randomForest(Response~. , data=traindata,ntree=500)
class(traindata$Response)
pred_test_rf<-predict(rf.fit,newdata=testdata,type="response")
#confusionMatrix(as.reference)
impvariable<-importance(rf.fit)
varImpPlot(rf.fit)
as.numeric(pred_test_rf)
pred_test_rf
accuracy = mean(pred_test_rf == truey)
CV=c(CV,accuracy)
MSE_Error<-1/length(truey)*(sum(abs(as.numeric(pred_test_rf)-as.numeric(truey)^2)))
MSE_Error
mean(CV)


#table(pred_train,insdata$Response)
#confusionMatrix(data=as.factor(pred_train),reference=as.factor(insdata$Response))
varnames <- rownames(impvariable)
varimp<-impvariable[,1]
class(impvariable)
df<- data.frame(varnames,varimp)
df<-df[order(df$varimp),]
x11()
obj_a <- ggplot (df, aes (x=varnames, y=varimp))
obj_a <- obj_a + geom_bar (position = position_dodge(), stat="identity")
obj_a <- obj_a + coord_flip () 
obj_a <- obj_a + xlab ("")
print(obj_a)

###################################################################################
#Boosting
##################################################################################

install.packages("gbm")
library(gbm)
install.packages("maboost")
library(maboost)

set.seed(12345)
index=sample(1:nrow(best_model_df),.20*nrow(best_model_df))
datay=best_model_df[,36] #response variable
datatrain<-best_model_df[-index,]
datatest<-best_model_df[index,]
datatrain$Response<-as.factor(datatrain$Response)
datatest$Response<-as.factor(datatest$Response)
true_y<-datatest$Response
boost.fit<-maboost(Response~.,data=datatrain,iter=50,nu=2,breg="l2", type="sparse",bag.frac=1,random.feature=FALSE,random.cost=FALSE, C50tree=FALSE, maxdepth=6,verbose=TRUE)
pred.boost= predict(boost.fit,datatest,type="class")

accuracy = mean(pred.boost == true_y)
