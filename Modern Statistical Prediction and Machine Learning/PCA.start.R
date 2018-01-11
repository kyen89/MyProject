library(Matrix)
word = readMM("stemmed_sparse_word_feature_matrix3.mtx")
#word = as.data.frame(as.matrix(word))

featureNames = strsplit(readLines('stemmed_feature_names3.txt'),',')[[1]]
colnames(word) = c("CLASS",featureNames[-1])
any(is.na(word))
wordFreqNumBooks = colSums(word[,-1]!=0)
wordFreqTotalAppearances = colSums(word[,-1])

classes = as.character(seq(0,3,by=1))
classnames = c("Children","History","Religion","Science")

set.seed(1)
index = sample(1:ncol(word), 1000)
time.check.1 = proc.time()
df = as.data.frame(as.matrix(word[,c(1,index)]))
df[,1] = as.factor(df[,1])
proc.time() - time.check.1
time.check.1 = proc.time()

pr.out = prcomp(df[,-1],scale=T)
proctime() - time.check.1


pr.var = pr.out$sdev^2
pve = pr.var/sum(pr.var)





index = sample(1:ncol(cutWord),1999) #take out most frequent ones
ptm1 <- proc.time() #start time 2:
wordFrame = as.data.frame(as.matrix(cutWord[,c(1,index)]))
wordFrame[,1] = as.factor(wordFrame[,1])
proc.time() - ptm1 #end time: 3.9 seconds
ptm1 <- proc.time() #start time 2:
word.rf = randomForest( x=wordFrame[,-1],y=as.factor(wordFrame[,1]),
                        ntree=1,mtry=round(sqrt(ncol(wordFrame)-1)), importance=TRUE) 
proc.time() - ptm1 #end time: 42003.425 seconds =