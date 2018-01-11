library(Matrix)
library(caret)
library(xgboost)
library(Metrics)
library(ROCR)
word = readMM("stemmed_sparse_word_feature_matrix3.mtx")
#word = as.data.frame(as.matrix(word))

featureNames = strsplit(readLines('stemmed_feature_names3.txt'),',')[[1]]
colnames(word) = c("CLASS",featureNames[-1])
any(is.na(word))
wordFreqNumBooks = colSums(word[,-1]!=0)
wordFreqTotalAppearances = colSums(word[,-1])

classes = as.character(seq(0,3,by=1))
classnames = c("Children","History","Religion","Science")

#problem with PCA is if the data has 0 variance (entire column has 0 variance) or very little variance will hurt PCA
#so we need to check before we go to further steps.
#it will tell us that 
nzv <- nearZeroVar(word[,c(-1,-2)],saveMetrics = T)
print(paste('Range:',range(nzv$percentUnique)))
dim(nzv[nzv$percentUnique > 0.1,])
word_nzv = word[,c(rownames(nzv[nzv$percentUnique>0.1,]))]
print(paste("Column count after cut off:",ncol(word_nzv)))

EvaluateAUC <- function(dfEvaluate) {
  CVs <- 5
  cvDivider <- floor(nrow(dfEvaluate) / (CVs+1))
  indexCount <- 1
  outcomeName <- c('cluster')
  predictors <- names(dfEvaluate)[!names(dfEvaluate) %in% outcomeName]
  lsErr <- c()
  lsAUC <- c()
  for (cv in seq(1:CVs)) {
    print(paste('cv',cv))
    dataTestIndex <- c((cv * cvDivider):(cv * cvDivider + cvDivider))
    dataTest <- dfEvaluate[dataTestIndex,]
    dataTrain <- dfEvaluate[-dataTestIndex,]
    
    bst <- xgboost(data = as.matrix(dataTrain[,predictors]),
                   label = dataTrain[,outcomeName],
                   max.depth=6, eta = 1, verbose=0,
                   nround=5, nthread=4, 
                   objective = "reg:linear")
    
    predictions <- predict(bst, as.matrix(dataTest[,predictors]), outputmargin=TRUE)
    err <- rmse(dataTest[,outcomeName], predictions)
    auc <- auc(dataTest[,outcomeName],predictions)
    
    lsErr <- c(lsErr, err)
    lsAUC <- c(lsAUC, auc)
    gc()
  }
  print(paste('Mean Error:',mean(lsErr)))
  print(paste('Mean AUC:',mean(lsAUC)))
}

dfEvaluate <- cbind(as.data.frame(sapply(word_nzv,as.numeric)),cluster=word[,1])
EvaluateAUC(dfEvaluate)

princ <- prcomp(word_nzv)
#default:scale=F, center =T
head(princ)
nComp <- 1
dfComponents <- predict(princ, newdata=pmatrix)[,1:nComp]
dfEvaluate <- cbind(as.data.frame(dfComponents),cluster=word[,c(1,-2)])
head(dfEvaluate)
EvaluateAUC(dfEvaluate)
#As we increase nComp number, we can check the Mean AUC.
#The bigger Mean AUC, the better it explains the variance
#Not always the big number of nComp does not result to the best result..(it might reduce the Mean AUC)










dff= as.data.frame(as.matrix(word))
dff[,1] = as.factor(dff[,1])


pve.pool = rep(0,3000)
index.pool = rep(0,3000)
nfold = 10


for(i in 1:nfold){
  
  index = sample(3:ncol(word), 2000)
  df = dff[,index]
  pr.out = prcomp(df[,-1],center = T,scale=F)
  
  pr.var = pr.out$sdev^2
  pve = pr.var/sum(pr.var)
  
  line = order(pve,decreasing=T)
  highest = line[1:300]
  pve.pool[i] = sort(pve)[1:300]
  index.pool[i] = highest
}
pve.pool
index.pool

plot(pve, type = "o",ylab="PVE",xlab = "Principal Component", col="blue",xlim=c(0,10))
plot(cumsum(pve))

#Finally you can create a truncated version of your data by using only the leading (important) PCs:









pc.use <- 40
trunc <- pr.out$x[,1:pc.use] %*% t(pr.out$rotation[,1:pc.use])
#and add the center (and re-scale) back to data
if(pr.out$scale != FALSE){
  trunc <- scale(trunc, center = FALSE, scale=1/pr.out$scale)
}
if(pr.out$center != FALSE){
  trunc <- scale(trunc, center = -1 * pr.out$center, scale = FALSE)
}

dim(trunc); dim(word)
RANG = range(cbind(df[,-1],trunc))
BRK = seq(RANG[1],RANG[2],100)
COLS = rainbow(length(BRK)-1)
par(mfcol = c(1,2), mar=c(1,1,2,1))

image(df[,-1], main="Original matrix", xlab="", ylab="", xaxt="n", yaxt="n", breaks=BRK, col=COLS)
box()
image(trunc, main="Truncated matrix (3 PCs)", xlab="", ylab="", xaxt="n", yaxt="n", breaks=BRK, col=COLS)
box()



