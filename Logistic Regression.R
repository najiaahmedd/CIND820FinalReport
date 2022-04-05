## Reading data file
data1<-read.csv("Desktop/projectfiles.csv")
## Creating dataframe and changing data type 
dataclean<-subset(data1, select = c(Year,Disaster.Group,Disaster.Subgroup, Disaster.Type, Disaster.Subtype, Country, Region, Total.Deaths, No.Injured, No.Affected, No.Homeless, Total.Affected, Total.Damages...000.US.., Reconstruction.Costs...000.US.., Insured.Damages...000.US..))
dataclean1<-subset(data1, select = c(Year,Total.Deaths, No.Injured, No.Affected, No.Homeless, Total.Affected, Total.Damages...000.US.., Reconstruction.Costs...000.US.., Insured.Damages...000.US..))
data2<-subset(dataclean, select = c(Year,Disaster.Group,Disaster.Subgroup, Disaster.Type, Disaster.Subtype, Country, Region, Total.Deaths, No.Injured, No.Homeless, Total.Affected, Total.Damages...000.US.., Reconstruction.Costs...000.US.., Insured.Damages...000.US..))
data2$Year <- as.character(data2$Year)
## Installing the MICE package 
install.packages("mice")
##Call mice package
library(mice)
## Data imputation using MICE
imputedData<-mice (data2, m=3, maxit=30, meth='cart', seed=600)
##Merging dataset to have imputed data to fill in NA values 
completedata<-complete(imputedData, 1)
completedata$Year <- as.integer(completedata$Year)
completedata$Region <- as.factor(completedata$Region)
completedata$Disaster.Type <- as.factor(completedata$Disaster.Type)
completedata$Disaster.Subtype <- as.factor(completedata$Disaster.Subtype)
completedata$Disaster.Subgroup <- as.factor(completedata$Disaster.Subgroup)
completedata$Country <- as.factor(completedata$Country)
##dropping Disaster.Subtype to avoid NAs
complete2<-subset(completedata, select = c(Disaster.Subgroup, Disaster.Type, Country, Region, Total.Deaths, No.Injured, No.Homeless, Total.Affected, Total.Damages...000.US.., Reconstruction.Costs...000.US.., Insured.Damages...000.US..))

## Splitting training and testing sets
train_index <- sample(1:nrow(complete2), 0.7 * nrow(complete2))
train.set <- complete2[train_index,]
test.set  <- complete2[-train_index,]
train.set_new <- train.set[-1]
test.set_new <- test.set[-1]
cl <- train.set
dis_train_labels <- train.set$Disaster.Type
dis_test_labels <- test.set$Disaster.Type
cl <- dis_train_labels
##Prediction model
install.packages("ISLR")
library(ISLR)
disaster.model<-glm(formula = Disaster.Type ~ Region, family="binomial", data=test.set)
summary(disaster.model)
disaster.prediction <- predict(disaster.model,type = "response")
predicted_disaster <- ifelse(disaster.prediction>=0.5, 'pass', 'fail')
ConfusionMatrix <- table(actual = test.set$Disaster.Type, predicted = predicted_flood)
##Predicting where a specific natural disaster will occur for floods, storms, droughts, epidemics, and earthquakes
##Floods w/ accuracy calculation
flood.model<-glm(formula = Disaster.Type == 'Flood'~ Region, family="binomial", data=test.set)
summary(flood.model)
flood.prediction <- predict(flood.model,type = "response")
predicted_flood <- ifelse(flood.prediction>=0.5, 'pass', 'fail')
ConfusionMatrixFlood <- table(actual = test.set$Disaster.Type=='Flood', predicted = predicted_flood)
sum(diag(ConfusionMatrixFlood))/nrow(test.set)
##Storms w/ accuracy calculation
storm.model<-glm(formula = Disaster.Type == 'Storm'~ Region, family="binomial", data=test.set)
summary(storm.model)
storm.prediction <- predict(storm.model,type = "response")
predicted_storm <- ifelse(storm.prediction>=0.5, 'pass', 'fail')
ConfusionMatrix3 <- table(actual = test.set$Disaster.Type=='Storm', predicted = predicted_storm)
sum(diag(ConfusionMatrix3))/nrow(test.set)
##Earthquakes w/ accuracy calculation
ew.model<-glm(formula = Disaster.Type == 'Earthquake'~ Region, family="binomial", data=test.set)
summary(eq.model)
eq.prediction <- predict(eq.model,type = "response")
predicted_eq <- ifelse(eq.prediction>=0.05, 'pass', 'fail')
ConfusionMatrix4 <- table(actual = test.set$Disaster.Type=='Earthquake', predicted = predicted_eq)
sum(diag(ConfusionMatrix4))/nrow(test.set)
##Epidemics w/ accuracy calculation
ep.model<-glm(formula = Disaster.Type == 'Epidemic'~ Region, family="binomial", data=test.set)
summary(ep.model)
ep.prediction <- predict(ep.model,type = "response")
predicted_ep <- ifelse(ep.prediction>=0.05, 'pass', 'fail')
ConfusionMatrix5 <- table(actual = test.set$Disaster.Type=='Epidemic', predicted = predicted_ep)
sum(diag(ConfusionMatrix5))/nrow(test.set)
