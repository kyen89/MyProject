####### Part 3 Unsupervised Learning #######
#first read in the word matrix
library(Matrix)
word = readMM("sparse_word_feature_matrix.mtx") #will be a dataframe
any(is.na(word))# and other features to check legitimacy of word matrix
wordFreq = apply(word[1:100],2,function(x) sum(x!=0))
hist(wordFreq) 

#identify words on lower and upper end of spectrum (mention some in report)

#give reasoning to set a threshold and apply filter to word matrix

#give dimensions
dim(word)

####### Part 6 Classification on Filtered Word Feature Matrix ######
#note, the y-value column is named "class" and appears as the first column 
#in the matrix "word" in the following commands
#Using Random Forest
library(tree)
library(randomForest)
set.seed(0) #set some seed for reproducibility, try different seeds
#maybe need to make sure the class arguments are of type factor to do classification tree
#################################
####Simplest case: test to see everything works on a simple randomForest #####
ptm1 <- proc.time() #start time
word.rf = randomForest( class ~ ., data=word, ntree=10,mtry=4, importance=TRUE) 
proc.time() - ptm1 #end time: 

word.rf
plot(word.rf) #if you really wanna try....

#prediction on training set
ptm2 <- proc.time()
rf.pred = predict( word.rf, newdata=word )#don't include class column
rf.MSE = mean((word$class - rf.pred )^2)
print(rf.MSE) 
proc.time() - ptm2 #record time it took:

#compute and visualize variable importance measure
irf = importance( word.rf )
print( irf[ order( irf[,1] ), ] )
varImpPlot(carseats.rf)
#################################
#The following three sets of scripts tunes for number of parameters and # of trees by
#CV, validation set, and OOB, respectively

##################### CV ##########################
#Implement CV to find best ntree value by fixing mtry for a set of ntree values:
#we justify first setting mtry=sqrt(# of predictors) as that is the optimal convention.
#later we will tune for mtry.
numpreds = sqrt(ncol(word)) #take large steps
numtree = c(50,100,150,200,250,300,350,400) #change this to test different ntree values
n=nrow(word)
nfolds = 10
fold_elements=c(rep(1:nfolds,each=floor(n/nfolds)),1:(n %% nfolds))
cv_fold=sample(fold_elements,n)
ntree.CVerror = matrix(NA, nrow = length(numtree), ncol = nfolds)
for(i in 1:length(numtree)){
  for(j in 1:nfolds){
    test = word[fold_elements==j,]
    train = word[fold_elements!=j,] 
    cv.rf = randomForest(class~.,data=train, mtry = numpreds, ntree = numtree[i])
    cv.pred = predict(cv.rf, newdata = test[,-1]) #first row contains the true classifications
    ntree.CVerr[i, j] = sum(cv.pred != test$class) / nrow(test)
  }
}
#find average error across folds per number of tree (ntree parameter)
ntree.err = apply(ntree.CVerr, 1, mean)
#Plot error vs. number of tree
plot(numtree, ntree.CVerr, xlab = "number of trees", ylab = "Error rate", main = "Random Forest w/ CV", type = "l")

# Best num of trees
best.numtree = numtrees[which.min(ntree.CVerr)]
best.numtree

#### Implement CV to train for the number of predictors (mtry)
numpreds = c() #give a vector and take large steps
numtree = best.numtree
n=nrow(word)
nfolds = 10
#assuming we don't need to randomize again to tune mtry after tuning ntree 
#(uses same train and test data set)
#fold_elements=c(rep(1:nfolds,each=floor(n/nfolds)),1:(n %% nfolds))
#cv_fold=sample(fold_elements,n)
mtry.CVerr = matrix(NA, nrow = length(numpreds), ncol = nfolds)
for(i in 1:length(numpreds)){
  for(j in 1:nfolds){
    test = word[fold_elements==j,]
    train = word[fold_elements!=j,] 
    cv.rf = randomForest(class~.,data=train, mtry = numtree, ntree = numpreds[i])
    cv.pred = predict(cv.rf, newdata = test[,-1]) #first row contains the true classifications
    mtry.CVerr[i, j] = sum(cv.pred != test$class) / nrow(test)
  }
}
#find average error across folds per number of tree (ntree parameter)
mtry.err = apply(mtry.CVerr, 1, mean)
#Plot error vs. number of tree
plot(numpreds, mtry.CVerr, xlab = "number of predictors", ylab = "Error rate", main = "Random Forest w/ CV", type = "l")

# Best num of trees
best.mtry = numpreds[which.min(mtry.CVerr)]
best.mtry

##################### VS ##########################
#use VS to tune first for number of trees using sqrt(# predictors) for mtry
numpreds = sqrt(ncol(word)) #take large steps
numtree = c(50,100,150,200,250,300,350,400) #change this to test different ntree values
n=nrow(word)
trainIndex = sample(nrow(word), (0.8) * nrow(word)) # feel free to change the 0.8 to desired proportion
train = word[trainIndex,]
test = word[-trainIndex,] 
ntree.VSerror = rep(NA, length(numtree))
for(i in 1:length(numtree)){
  vs.rf = randomForest(class~.,data=train, mtry = numpreds, ntree = numtree[i])
  cv.pred = predict(vs.rf, newdata = test[,-1]) #first row contains the true classifications
  ntree.VSerr[i] = sum(cv.pred != test$class) / nrow(test)
}

#Plot error vs. number of tree
plot(numtree, ntree.VSerr, xlab = "number of trees", ylab = "Error rate", main = "Random Forest w/ VS", type = "l")

# Best num of trees
best.numtree = numtrees[which.min(ntree.VSerr)]
best.numtree

#then tune for number of predictors
numpreds = c() #take large steps
numtree = best.numtree #change this to test different ntree values
n=nrow(word)
#trainIndex = sample(nrow(word), (0.8) * nrow(word)) # feel free to change the 0.8 to desired proportion
#train = word[trainIndex,]
#test = word[-trainIndex,] 
mtry.VSerror = rep(NA, length(numpreds))
for(i in 1:length(numpreds)){
  vs.rf = randomForest(class~.,data=train, mtry = numpreds[i], ntree = numtree)
  cv.pred = predict(vs.rf, newdata = test[,-1]) #first row contains the true classifications
  mtry.VSerr[i] = sum(cv.pred != test$class) / nrow(test)
}

#Plot error vs. number of tree
plot(numtree, mtry.VSerr, xlab = "number of predictors", ylab = "Error rate", main = "Random Forest w/ VS", type = "l")

# Best num of trees
best.ntree = numtrees[which.min(mtry.VSerr)]
best.ntree

##################### OOB ##########################
x = worrd[, -1]
y = word[,1]
numpreds = c() #no need to specify ntree value because the fit function will give it as output
OOBerr =  matrix(NA, 500, length(numpreds)) #using 500 as max ntree value
for (i in 1 : length(numpreds)) {
  rf.cv = randomForest(x, y, mtry = numpreds[i], ntree = 500)
  OOBerr[,i] = rf.cv$err.rate[,1]
}

color = c(1:length(numpreds))
# Plot of error rate with out of bag data
plot(1:500, OOBerr[,1], main="Random Forest w/ OOB", type = 'l', col = color[1], xlab = "num of trees", ylab = "OOB error rate" )
for (i in 2 : length(numpreds)) {
  points(1:500,OOBerr[,i], type = "b", pch = i, col = color[i])
}
legend.text = c("pred = 2", "pred = 5", "pred = 8", "pred = 11")
legend("topright", legend.text, lty = 1, col=color)

# Best pred numbers
#look at graph to first find best number of predictors
best.mtry = #insert index in numpreds of best number of predictors
best.ntree = which.min(OOBerr[,best.mtry])
best.ntree


