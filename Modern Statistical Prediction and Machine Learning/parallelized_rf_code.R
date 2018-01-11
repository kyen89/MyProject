
library(Matrix)
wordMatrix = readMM("final_word_feature_matrix2.mtx") #will be a dataframe
featureNames = strsplit(readLines('final_word_feature_names2.txt'),',')[[1]]

library(tree)
library(randomForest)


wordMatrix = as.data.frame(as.matrix(wordMatrix))
wordMatrix[,1] = as.factor(wordMatrix[,1])
colnames(wordMatrix) = c("class",featureNames[-1])

set.seed(10)
testIndices = sample(nrow(wordMatrix),nrow(wordMatrix)*.2)

#ptm1 <- proc.time() #start time
#set.seed(0)
#word.rf = randomForest(x=wordMatrix[,-1],y=wordMatrix[,1],ntree=100,mtry=sqrt(ncol(wordMatrix[,-1])), importance=TRUE) 
#proc.time() - ptm1 #time elapsed last try: 2507

library(doParallel)
set.seed(0)
ptm1 <- proc.time() #start time
cl <- makePSOCKcluster(4)
clusterEvalQ(cl, library(foreach))
registerDoParallel(cl)
rf = foreach(ntree=rep(25,4), .combine=combine, .packages='randomForest') %dopar%
  randomForest(x=wordMatrix[-testIndices,-1],y=wordMatrix[-testIndices,1],ntree=ntree,mtry=sqrt(ncol(wordMatrix[,-1])))
closeAllConnections()
proc.time() - ptm1 #end time: 
predicted = predict(rf,wordMatrix[testIndices,-1])
correctness = (predicted==wordMatrix[testIndices,1])
mean(correctness)

#library(caret)
#library(doParallel)
#library(e1071)

#ptm1 <- proc.time() #start time
#set.seed(0)
#trCtrl = trainControl(method='none',allowParallel=T)
#cl <- makePSOCKcluster(2)
#clusterEvalQ(cl, library(foreach))
#registerDoParallel(cl)
#rfParam = expand.grid(mtry=45)
#train(x=wordMatrix[,-1],y=wordMatrix[,1],method="parRF",trControl = trCtrl,tuneGrid=rfParam)
#closeAllConnections()
#proc.time() - ptm1 #end time: 
