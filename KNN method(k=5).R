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
complete2<-subset(completedata, select = c(Disaster.Subgroup, Disaster.Type, Country, Region, Total.Deaths, No.Injured, No.Homeless, Total.Affected, Total.Damages...000.US.., Reconstruction.Costs...000.US.., Insured.Damages...000.US..))
##Encoding variables 
install.packages('fastDummies')
library(fastDummies)
complete2 <- dummy_cols(complete2, select_columns = c('Country', 'Region', 'Disaster.Subgroup'),
                        +                         remove_selected_columns = TRUE)
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
library(class)
suppressWarnings(dis_knn_prediction <- knn(train = train.set_new, test = test.set_new, cl= dis_train_labels, k =5 ))
##Prediction 
head(dis_knn_prediction)
##Confusion matrix using CARET package 
install.packages("caret")
library(caret)
result <- confusionMatrix(dis_knn_prediction, test.set$Disaster.Type)



