#accuracy <- 1
cv <- 5
trainSet <- training1.1set5.2
cvDivider <- floor(nrow(training1.1set5.2)/(cv+1))

for (mtry1 in c(20,24,28,30)){
  for (ntree1 in c(600,700,1000)){
     indexCount <-1 
     for (cv in seq(1:cv)){
    #assign data chunk to data set  
      datatestIndex <- c((cv*cvDivider):(cv*cvDivider +cvDivider)) 
      dataTest <- trainSet[datatestIndex,]
    #Other Chunks into Train
      dataTrain <- trainSet[-datatestIndex,]
    #train
    rf.cv.1 <- randomForest(x=dataTrain[,predictors1],y=dataTrain[,outcomename1], ntree = ntree1,mtry = mtry1,importance=TRUE)
    gc()
    #predict
    predictions <- predict(rf.cv.1,dataTest[,predictors1],type="prob")
    
    #accuracy by ROC and AUC
    predictions.y <- predictions[,2]
    pred.1 <- prediction(predictions.y,dataTest[,outcomename1])
    auc <- performance(pred.1,"auc")
    auc <- unlist(slot(auc,"y.values"))
    
    #print(auc)
    print(paste(mtry1,ntree1,cv,auc))
     }
  }
}
