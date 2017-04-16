#loading required libraries
library('randomForest')

library('Metrics')

#set random seed
set.seed(101)

#loading dataset

data<-read.csv("train.csv",stringsAsFactors= T)

#checking dimensions of data
dim(data)

## [1] 3000  101

#specifying outcome variable as factor


 data$Y<-as.factor(data$Y)

#dividing the dataset into train and test
train<-data[1:2000,]
test<-data[2001:3000,]

#applying randomForest 
model_rf<-randomForest(Y~.,data=train)

preds<-predict(object=model_rf,test[,-101])

table(preds)

## preds
##  -1   1
## 453 547

#checking accuracy

auc(preds,test$Y)

## [1] 0.4522703
So, the accuracy we get is 0.45. Now let’s create five clusters based on values of independent variables using k-means clustering and reapply randomforest.

#combing test and train

all<-rbind(train,test)

#creating 5 clusters using K- means clustering

Cluster <- kmeans(all[,-101], 5)

#adding clusters as independent variable to the dataset.
all$cluster<-as.factor(Cluster$cluster)

#dividing the dataset into train and test
train<-all[1:2000,]
test<-all[2001:3000,]

#applying randomforest
model_rf<-randomForest(Y~.,data=train)

preds2<-predict(object=model_rf,test[,-101])

table(preds2)

## preds2

## -1   1 

##548 452 

auc(preds2,test$Y)

## [1] 0.5345908