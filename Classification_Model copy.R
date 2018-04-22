#Analytics Vidya - mcKinsy Exercise
rm(list=ls())
getwd()
setwd("/Users/Saatwik/Documents/avidya/CapeGemini")


#Load the Required packages
require(DMwR)
require(C50)
require(vegan)
require(randomForest)
require(infotheo)

#Read the training data and pre-process the data
train<-read.csv("sample_data.csv",header=T,sep=",")
dim(train)
str(train)
sum(is.na(train))
summary(train)

#Read the train response data
train.resp<-read.csv("sample_output.csv",header=T,sep=",")
train<-merge(train,train.resp,by=intersect(names(train),names(train.resp)))
#Function to pre-process the data, so that we can apply the same to train/test
data_process<-function(data){
  #Remove application ID
  data<-subset(data,select=-c(1))
  #Conevert credit history into factor
  data$Credit_History<-as.factor(as.character(data$Credit_History))
  data<-centralImputation(data)
  return (data)
}

train<-data_process(train)
str(train)

#Build using random forest
Model_rf<-randomForest(Loan_Status~.,data=train,importance=T)
Model_rf
summary(Model_rf)

#Read Test Data
test<-read.csv("test_data.csv",header=T,sep=",")
test.appl<-subset(test,select=c(1))
test<-data_process(test)
str(test)

test$Married<-as.character(test$Married)
test$Married[test$Married == ""]<-"No"
test$Married<-as.factor(test$Married)

test$Dependents<-as.character(test$Dependents)
test$Dependents[test$Dependents == ""]<-"0"
test$Dependents<-as.factor(test$Dependents)

str(test.appl)

pred_rf<-predict(Model_rf,newdata=test,type="response",predict.all=T)
summary(pred_rf$aggregate)
res_rf<-data.frame(Application_ID=test.appl$Application_ID,Loan_Status=pred_rf$aggregate)
write.csv(res_rf,file="test_output.csv",row.names=F)



