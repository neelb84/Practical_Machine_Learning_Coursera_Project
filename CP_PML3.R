
download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv",destfile="./acctrain.csv")

download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv",destfile="./acctest.csv")

acctrain<-read.csv("acctrain.csv",na.strings=c("NA","#DIV/0!",""))

acctest<-read.csv("acctest.csv",na.strings=c("NA","#DIV/0!",""))

dim(acctrain)

dim(acctest)

colNA<-colMeans(is.na(acctrain))

acctrain2<-acctrain[,colMeans(is.na(acctrain))<.97]

dim(acctrain2)

acctest2<-acctest[,colMeans(is.na(acctest))<.97]

dim(acctest2)

acctrain3<-acctrain2[,c(grep("belt|forearm|arm|dumbbell",names(acctrain2)),ncol(acctrain2))]

acctest3<-acctest2[,c(grep("belt|forearm|arm|dumbbell",names(acctest2)))]

dim(acctrain3)
dim(acctest3)

library(psych)
fit <- principal(acctrain3[,1:ncol(acctrain3)-1], nfactors=20, rotate="varimax",sort=TRUE)
fit
fa.sort(fit,polar=FALSE)

library(caret)
set.seed(15390)

inTrain <- createDataPartition(acctrain3$classe, p = .6, list=FALSE)
trainingdata <- acctrain3[inTrain,]
testingdata <- acctrain3[-inTrain,]

dim(trainingdata)



#modFit <- train(classe ~., method="rf", data=trainingdata, 
                #sampsize=2000, 
  #              ntree=250,importance=TRUE, prox=TRUE,do.trace=TRUE)

library(randomForest)
set.seed(15390)

ctrl=trainControl(method="cv",10,repeats=5)
modFit <- randomForest(classe ~., 
                       trControl=ctrl,
                       data=trainingdata, 
                   ntree=500, imp=TRUE)
                   #, sampsize=5000)
                   #, do.trace=TRUE)


varImpPlot(modFit)

trpred <- predict(modFit, trainingdata)

confusionMatrix( trainingdata$classe,trpred)

testpred <- predict(modFit, testingdata)

confusionMatrix( testingdata$classe, testpred)


finaltestpred <- as.character(predict(modFit, acctest3))

finaltestpred

pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

setwd("F:\\_IB\\Coursera\\PML\\outfiles")
pml_write_files(finaltestpred)
