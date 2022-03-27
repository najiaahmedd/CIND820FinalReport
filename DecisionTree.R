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
complete2<-subset(completedata, select = c(Disaster.Subgroup, Disaster.Type=="Flood", Country, Region, Total.Deaths, No.Injured, No.Homeless, Total.Affected, Total.Damages...000.US.., Reconstruction.Costs...000.US.., Insured.Damages...000.US..))
#Decision Tree model
install.packages("partykit")
library(partykit)
## Classification Decision Tree
train_tree <- sample(1:nrow(completedata), 0.9 * nrow(completedata))
train.set <- completedata[train_tree,]
test.set  <- completedata[-train_tree,]
##Model 
disaster_model <- ctree(Disaster.Type ~ Region, data=train.set)
disaster_model
##Prediction
disasterprediction <- predict(disaster_model, test.set)
#Visualization 
head(disasterprediction)
plot(disaster_model, type="simple")

##Confusion Matrix

install.packages("caret")
library(caret)
cm<-confusionMatrix(disasterprediction, test.set$Disaster.Type)

