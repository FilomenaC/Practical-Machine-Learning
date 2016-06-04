# Read the data
trainingRaw<-read.csv("pml-training.csv", na.strings = c("","NA","#DIV/0!"))
finaltestRaw<-read.csv("pml-testing.csv",na.strings = c("","NA","#DIV/0!"))


# Exploratory data analysis

training<-trainingRaw[,-c(1:7)]
testing<- finaltestRaw [, -c(1:7)]
NAindex<-which (colSums(is.na(training))>nrow(training)/2)
training <- training[, -NAindex]
testing <- testing[, -NAindex]

# Load library
library(caret)
library(randomForest);library(rpart)
library(rattle);library(scales);libray(lda)
set.seed(1971)

# Partitioning training data

inTrain<- createDataPartition(training$classe, p=0.7, list= FALSE)
trainingDS<-training[inTrain,]
testing <- training [-inTrain,]
summary(trainingDS$classe)

#Model 1 - LDA

ldaMod<-train(trainingDS$classe~.,data=trainingDS,method="lda")
ldaPredict<- predict(ldaMod,newdata=testing[,-classe])
ldaCM<- confusionMatrix(ldaPredict,testing[,classe])
ldaAcc<-ldaCM$overall[[1]]
percent(ldaAcc)
percent(1.00-ldaAcc)

#Model 2 - Recursive Partitioning 
rpartMod<-train(trainingDS$classe~.,data=trainingDS,method="rpart", tuneLength =30)
#tuneLength = n creates a grid of values with the combinations of all parameters requested by the particular method 
#and with a length of n values for each parameter.
rpartPredict<- predict(rpartMod,newdata=testing[,-53])
rpartCM<- confusionMatrix(rpartPredict,testing$classe)
rpartAcc<-rpartCM$overall[[1]]
percent(rpartAcc)
percent(1.00 - rpartAcc)

#Model 3 - Random Forests
set.seed(1972)
rfMod<-randomForest(classe~.,data=trainingDS,ntree=200,importance= TRUE)
rfPredict<- predict(rfMod,newdata=testing[,-53])
rfCM<- confusionMatrix(rfPredict,testing$classe)
rfAcc<-rfCM$overall[[1]]
percent(rfAcc)
percent(1.00 - rfAcc)


#Submit prediction for project quiz

SubmitPredict <-predict(rfMod,newdata=finaltest,type="class")
SubmitPredict # all cases were correct

