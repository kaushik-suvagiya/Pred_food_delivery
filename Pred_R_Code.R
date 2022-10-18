# Predicting Food Delivery Time: Hackathon By IMS Proschool
library(MASS)
#install.packages('readxl')
#install.packages('glmnet')
library(readxl)
library(caret)
library(glmnet)
train1<-read_excel("D:/Kaushik_Data/ML Hackathon/Prediction delivery time/Participants Data/Data_Train.xlsx")
names(train1)
train<-train1[c(-1,-2,-3)]
str(train)


train$Average_Cost<-as.numeric(gsub("\\\u20b9","", train$Average_Cost))
train$Average_Cost<- as.numeric(gsub(",","",train$Average_Cost))

train$Minimum_Order<-as.numeric(gsub("\\\u20b9","", train$Minimum_Order))

test1<-read_excel("D:/Kaushik_Data/ML Hackathon/Prediction delivery time/Participants Data/Data_Test.xlsx")
names(test1)
test<-test1[c(-1,-2,-3)]
str(test)
#summary(train)
#hist(train$Delivery_Time)

test$Average_Cost<-as.numeric(gsub("\\\u20b9","", test$Average_Cost))
test$Average_Cost<- as.numeric(gsub(",","",test$Average_Cost))
test$Minimum_Order<-as.numeric(gsub("\\\u20b9","", test$Minimum_Order))

train$Rating[train$Rating==""||train$Rating=="-"||train$Rating=="NEW"||train$Rating=="Opening Soon"||train$Rating=="Temporarily Closed"]<-NA
train$Average_Cost[is.na(train$Average_Cost)]<-mean(train$Average_Cost,na.rm=TRUE)
train$Minimum_Order[is.na(train$Minimum_Order)]<-mean(train$Minimum_Order,na.rm=TRUE)

train$Votes<-as.numeric(train$Votes)
train$Rating[is.na(train$Rating)]<-median(train$Rating,na.rm=TRUE)
train$Votes[is.na(train$Votes)]<-median(train$Votes,na.rm=TRUE)
train$Reviews<-as.numeric(train$Reviews)
train$Reviews[is.na(train$Reviews)]<-median(train$Reviews,na.rm=TRUE)
train<-na.omit(train)
head(train$Delivery_Time)
train$Delivery_Time<- as.numeric(gsub(" minutes","",train$Delivery_Time))
tail(train$Delivery_Time)
model<-lm(log(Delivery_Time)~Average_Cost+Minimum_Order+Rating+Votes+Reviews,data=train)
summary(model)
pred.probs<-predict(model,test,type = "response")
#pred1 <- predict(test, new_data_test)
#cbind(test,pre1)

Predict_DT<-predict(model,test,"probs") #predict new data
head(Predict_DT)



