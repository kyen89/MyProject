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

set.seed(20)
index = sample(3:ncol(word), 2000)
time.check.1 = proc.time()
df = as.data.frame(as.matrix(word[,c(1,index)]))
df[,1] = as.factor(df[,1])
proc.time() - time.check.1
time.check.1 = proc.time()

pr.out = prcomp(df[,-1],center = T,scale=F)
proc.time() - time.check.1

pr.var = pr.out$sdev^2
pve = pr.var/sum(pr.var)

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

