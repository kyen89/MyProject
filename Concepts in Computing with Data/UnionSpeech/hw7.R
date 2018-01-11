######################################################
##### Homework 7 due Wedensday Nov 26 at noon.
## Please read through the whole assignment before starting it.

## For the assingment you will work with the full text of the 
## State of the Union speeches from 1790 until 2012.
## The speeches are all in the file "stateoftheunion1790-2012.txt".

## You will first read the text into R and manipulate it in order to 
## create a dataframe with information about each speech 
## (President's name, year and month, length of the speech, #sentences, #words)
## You will also create a list that contains the full text of each speech,  
## which in turn is used to create a word vector of *all* words that appear
## in *any* if the speeches - and to count their occurrances in each speech.

## You will then be able to make some comparisons of the speeches/presidents.

## The package SnowballC has a function to find the "stem" of dictionary words.
## Please install it on your computer, using: install.packages("SnowballC")
## but do NOT include that install statement in this file.
## Load the library:
#install.packages("SnowballC")
library("SnowballC")

## We provide a function [ computeSJDistance() ] to calculate the 
## Shannon-Jensen divergence between two word vectors.
## The function is the file computeSJDistance.R, please *keep* the source
## statement in the file:
source("computeSJDistance.R")

######################################################
## Use regular expression to create: 
## [speechYr], [speechMo], [presidents]

# We start by reading the textfile into the variable [speeches] using readLines().
# Note that readLines() first argument is a connection, 
# and that you can use the R command file() to open a connection.
# Read the help for readLines() and file().
# Check the class and dimension of [speeches].  Open the textfile in 
# an editor and compare it to [speeches]
con <- file('stateoftheunion1790-2012.txt','r')
speeches <- readLines(con)
class(speeches)
# The speeches are separated by a line with three stars (***).
# Create a numeric vector [breaks] with the line numbers of ***.
# Create the variable [n.speeches] a numeric variable with the number of speeches
# Question: Does every single *** in the file indicate the beginning of a speech?

 
proto.breaks <- grep("\\*\\*\\*",speeches)
breaks <- grep("\\*\\*\\*",speeches)[-length(proto.breaks)]
#이거 마지막 별도 포함시켜야되냐고 묻기!!
n.speeches <- length(breaks)

# Use the vector [breaks] and [speeches] to create a 
# character vector [presidents]
# with the name of the president delivering the address

presidents <- speeches[breaks+3]

# Use [speeches] and the vector [breaks] to create [tempDates], 
# a character vector with the dates of each speech
# Now, using tempDates create:
# a numeric vector [speechYr] with the year of each speech, and
# a character vector [speechMo] with the month of each speech
# Note: you may need to use two lines of code to create one/both variables.
  
tempDates <- speeches[breaks+4]
split_dates <- unlist(strsplit(tempDates," "))
speechYr <- as.numeric(split_dates[seq(3,length(split_dates),by=3)])
speechMo <- split_dates[seq(1,length(split_dates),by=3)]

  
# Create a list variable [speechesL] which has the full text of each speech.
# The variable [speechesL] should have one element for each speech.
# Each element in [speechesL] should be a character vector, where each
# element in the vector is a character string corresponding to one sentence.
# Note: The line breaks in the text file do not correspond to sentences so you have to
# -- collapse all the lines of a speech into one long character string (use paste())
# -- and then split up that string on punctuation marks [.?!]
# Use a for-loop over the number of speeches to do these two steps.
# We define speechesL as an empty list before the for-loop and in
# step i of the for-loop you assign the value of speechesL[[i]]

# Before creating [speechesL] run the following commands to remove 
# some fullstops that are not at the end of a sentence:


#그리고 그랩에 들어가는 \\랑 별 3개 확인해보고
#마지막으로 new_split[1] 확인해보기

speeches <- gsub("Mr.", "Mr", speeches)
speeches <- gsub("Mrs.", "Mrs", speeches)
speeches <- gsub("U.S.", "US", speeches)

speeches_1<-speeches[grep("\\*\\*\\*",speeches)[1]:length(speeches)]
#collapse <- paste(speeches_1,collapse = " ")

#grep은 벡터일때만 작동한다. 이거 컬렙스한상태는 벡터가 아니라 캐릭터라서 안됨
#new_split <- unlist(strsplit(collapse,"(\\.|\\?|\\!|^(\\*\\*\\*))"))
#proto.new_breaks <- grep("\\*\\*\\*",new_split)
#semi_breaks <- grep("\\*\\*\\*",new_split)[-length(proto.new_breaks)]
n.s.new_breaks <- grep("\\*\\*\\*",speeches_1)


#speechesL <- list(rep(0, n.speeches))
#for(i in 1:n.speeches){
#speechesL[[i]] <- speeches_1[n.s.new_breaks[i]+10:n.s.new_breaks[i+1]-4]
#return(speechesL)}
speechesL <- list(rep(0, n.speeches))
for(i in 1:n.speeches){
speechesL[[i]] <- speeches_1[(n.s.new_breaks[i]+6):(n.s.new_breaks[i+1]-1)]
speechesL[[i]] <- paste(speechesL[[i]],collapse = " ")
speechesL[[i]] <- unlist(strsplit(speechesL[[i]],"(\\.|\\?|\\!)"))
#speechesL[[i]] <- gsub("^[[:blank:]]*","",speechesL[[i]])

}


#### Word Vectors 
# For each speech we are going to collect the following information:
# -- # of sentences
# -- # of words
# -- # of characters
# 
# We will also create a word vector for every speech.  The word vector 
# should have one entry for every word that appears in *any* speech
# and the value of the entry should be the number of times the word appears.
# The entries should be in alphabetical order.  
# Once all the word vectors are in place we will combine them into a matrix
# with one row for every word that appears and one column for every speech.
#
# Do this in a few steps:
# Write a function, [speechToWords], that takes a character vector and 
# creates a word vector of all the words in the input variable.  
# The function should have :
# Input  : sentences, a character string
# Output : words, a character vector where each element is one word 

# In other words it should take a string of text and:
# -- cut it into words
# -- remove all punctuation marks (anything in :punct:)
# -- make all characters lower case
# -- Remove the phrase "Applause."
# -- use the function wordStem() from the package SnowballC to 
#    get the stem of each work
# -- finally, remove all empty words, i.e. strings that match "" 
#    both BEFORE running wordStem() *and* AFTER

#### The function wordStem() returns the "stem" of each word, e.g.:
#> wordStem(c("national", "nationalistic", "nation"))
#[1] "nation"      "nationalist" "nation"     

speechToWords = function(sentences) {
cut.words <- unlist(strsplit(sentences,split ="[[:blank:]]|[-]+"))
rm.punct <- gsub("[[:punct:]]","",cut.words)
lower.words <- tolower(rm.punct)
app.lause <- gsub("applause","",lower.words)
rm.number.s <- gsub("[[:digit:]]+[[:alpha:]]*","",app.lause)
rm.number.sn <- rm.number.s[!rm.number.s==""]
wordsmart<-wordStem(rm.number.sn)
wordsmart_1<-wordsmart[!wordsmart ==""]
return(wordsmart_1)}

#**split에서 sep하고 collapse잘 구분하는거 공부하기.


# Input  : sentences, a character string
# Output : words, a character vector where each element is one word 
  
  # return a character vector of all words in the speech


#### Apply the function speechToWords() to each speach
# Create a list, [speechWords], where each element of the list is a vector
# with the words from that speech.
speechWords <- lapply(speechesL,function(x){speechToWords(x)})
#speechWords<-grep("[[:digit:]]+",proto.speechWords)
#speechWords<- gsub("[[:digit:]]+","",proto.speechWords)
#speechWords <- proto.speechWords[-number.elim]

# 왜 숫자를 지우라는거야 여기서?

# Unlist the variable speechWords (use unlist()) to get a list of all words in all speeches, the create:
# [uniqueWords] : a vector with every word that appears in the speeches in alphabetic order

uniqueWords<- sort(unique(unlist(speechWords)))
not.unique.Words <- sort(unlist(speechWords))

# Create a matrix [wordMat]

# the number of rows should be the same as the length of [uniqueWords]
# the number of columns should be the same as the number of speeches (i.e. the length of [speechesL])
# the element wordCounts[i,j] should be the number of times the word i appears in speech j


# Use the function table() to count how often each word appears in each speech
# Then you have to match up the words in the speech to the words in [uniqueWords]
# To do that use assignment/indexing and remember : 
# if counts = table(x) then names(counts) is a vector with the elements of x
# and counts a vector with the number of times each element appeared, e.g.
# > x <- c("a", "b", "a", "c", "c", "c")
# > counts <- table(x)
# > counts
# x
# a b c 
# 2 1 3 
# > names(counts)
# [1] "a" "b" "c"

# You may want to use an apply statment to first create a list of word vectors, one for each speech.

# your code to create [wordMat] here:

#wordMat <-matrix(0,nrow = length(uniqueWords),ncol = length(speechesL))
#rownames(wordMat) <- uniqueWords

#empty.matrix_2 <- cbind(c(names.unique),empty.matrix)

#function(x){
#for(i in 1:length(speechesL))
#  if (speechWords[[i]]) == uniqueWords{

#for(i in 1:length(speechesL))
# match[i] <- 1*(speechWords[[i]] == uniqueWords)
#n.match[i] <- 0*(speechWords[[i]] != uniqueWords)
#match[i]<-match[i] + empty.matrix})

#    t.speechWords<-sapply(speechWords,function(x){
#      counts <-table(x)
#      temp <- empty.matrix
#      temp[names(counts)] <- counts
#      return(counts)
#    })
#}

#AA <- sapply(speechWords,function(x){
#  counts = table(x)
#  temp = a
#  temp[names(table)] <- counts
#  return(temp)
#})

#table.speechWords <- sapply(speechWords,function(x)table(x))
#for (i in 1:length(table.speechWords)){
  #wordMat[,i] <- match(uniqueWords,names(table.speechWords[[i]]),nomatch = 0)}
#for (i in 1:length(uniqueWords)){
  #for (j in 1:length(table.speechWords)){
    #if (wordMat[i,j] != 0){ 
     # wordMat[i,j] <- table.speechWords[[j]][as.numeric(wordMat[i,j])]}
  #}
#}

#temp <- empty.matrix
#for (i in 1:length(speechWords))
#counts = table(speechWords[i])



#or (j in 1:length(speechWords))
#  temp[names(counts),j] <- counts
#return(counts)}

#t.speechWords<-sapply(speechWords,function(x){
#  counts <-table(x)
  
#  return(counts)
#})

#13007이어야한다.
# Load the dataframe [speechesDF] which has two variables,
# president and party affiliation (make sure to keep this line in your code):

#presidents라고 한거 다 $$pres로 바꿔라.!!
load("speeches_dataframe_new.Rda")

## Now add the following variables to the  dataframe [speechesDF]:
# yr - year of the speech (numeric) (i.e. [speechYr], created above)
# month - month of the speech (numeric) (i.e. [speechMo], created above)
## Using wordVecs calculate the following 
# words - number of words in the speech (use [speechWords] to calculate)
# chars - number of letters in the speech (use [speechWords] to calculate)
# sent - number of sentences in the speech (use [speechesL] to calculate this)
yr <- cbind(speechesDF,speechYr)
month <- cbind(yr,speechMo)
words <- sapply(speechWords,function(x){length(unlist(x))})
chars <- sapply(speechWords,function(x){sum(nchar(x))})
sent <- sapply(speechesL,function(x){length(x)})

# Update the data frame
speechesDF <- data.frame(month,words,chars,sent)

######################################################################
## Create a matrix [presidentWordMat] 
# This matrix should have one column for each president (instead of one for each speech)
# and that colum is the sum of all the columns corresponding to speeches make by said president.

# note that your code will be a few lines...

  
#presidentWordMat <- 
#NN<-match(presidents,unique(presidents))
#BB<-c()
#for(i in 1:length(NN)){
#BB[i]<-c(NN[i]!=NN[i+1])
#}
#BB<-BB[-length(BB)]
#wordMat[,which(BB==T)]

#pres.match<-match(presidents,unique(presidents))
#BB<-c()
#for(i in 1:length(pres.match)){
#  BB[i]<-c(pres.match[i]!=pres.match[i+1])}
#BB<-BB[-length(BB)]
#YY<-c(0,which(BB==T))
#A<-YY+1
#B<-c(which(BB==T),222)
#CC<- list()
#for(j in 1:length(B)){
#CC[[j]]<-rowSums(wordMat[,(A[j]:B[j]),drop=F])
#presidentWordMat<-do.call(cbind,CC)
#}
#do.call 이라는 펑션이 흐름상보아하니 리스트를 메트릭스로 바꿔주면서 펑션을 실행시켜주는듯.

#presidentWordMat<- matrix(0,nrow=length(uniqueWords),ncol= length(unique(presidents))
#for(j in 1:length(B)){
#presidentWordMat[j]<-rowSums(wordMat[,A[j]:B[j]])}
#presidentWordMat

# At the beginning of this file we sourced in a file "computeSJDistance.R"
# It has the following function:
# computeSJDistance = (tf, df, terms, logdf = TRUE, verbose = TRUE)
# where
# terms - a character vector of all the unique words, length numTerms (i.e. uniqueWords)
# df - a numeric vector, length numTerms, number of docs that contains term (i.e. df)
# tf - a matrix, with numTerms rows and numCols cols (i.e. the word matrix)
  
# Document Frequency
# [docFreq]: vector of the same length as [uniqueWords], 
# count the number of presidents that used the word

#docFreq <-apply(presidentWordMat,1,function(x)sum(x!=0))   
# 문제가 참..언클리어하다. 단순히 쭉 다더하는게 아니라 각 대통령이 100번의 같은단어를 썼어도 하나로 쳐서
# 생각하면된다. 그 단어를 대통령들이 썻냐 안썻냐 즉 0이냐 1이냐로 보면됨.
# Call the function computeSJDistance() with the arguments
# presidentWordMat, docFreq and uniqueWords
# and save the return value in the matrix [presDist]

#presDist <- computeSJDistance(presidentWordMat,docFreq,uniqueWords,logdf = TRUE, verbose = TRUE)

## Visuzlise the distance matrix using multidimensional scaling.
# Call the function cmdscale() with presDist as input.
# Store the result in the variable [mds] by 

#mds <- cmdscale(presDist)

# First do a simple plot the results:
#plot(mds)
#각각 플랏에 대해서 커맨트 달아라 pdf 
# Customize this plot by:
# -- remove x and y labels and put the title "Presidents" on the plot
# -- color the observations by party affiliation 
# -- using the presidents name as a plotting symbol

# Create a variable presParty, a vector of length 41 where each element
# is the party affiliation and the names attribute has the names of the presidents.
# Hint: the info is in speechesDF$party and speechesDF$Pres

presParty <- tapply(speechesDF$party,speechesDF$Pres,function(x) x[1])
#여기서 x인인 speechesDF$party는 index인자인 speechesDF$Pres랑 어떻게 섭셋을 하든같이 붙어서 나온다.
#예를 들어서 파티 벡터가 쭈욱 DFDR이런식으로 쭈욱 나오면 각각의 대통령 이름하고 파티가 매칭이 되서
#나온다. 섭셋으로 x[숫자] 하면 매칭되서 같이 붙어서 나온다.
#이문제를 이해하는데 가장 큰 공로자는 function(x){x}이다. 얘를 치면 이해하기가 수월해짐.
#우리가 알고싶은거는 대통령이 어느 파티인지알고싶은것이기 때문에 일단 이 tapply로 적용이 되면
#두개의 x인자와 index 인자가 매칭이 되면서 팩터화 되고 각 종류 or species가 1,2,3,4 등으로 구분이된다.
#우리가 섭셋을 1로 하면 [1]로 된게 나오되 [1]에 있는애들이 어떤 종류의 팩터인지가 숫자로 나온다.
#여기서 1은 - D, 2는 DR로 3은 F, 그리고 기타등..
#따라서 순서는 [1]섭세팅 하면 일단 각 [1]에 있는애들이 섭셋팅 되는데 이게 D였던애들은 테이블에
#아웃풋으로 1로 뜨고 F였던애들은 3으로 나온다.


cols <- rainbow(length(unique(speechesDF$party)))


# use rainbow() to pick one unique color for each party (there are 6 parties)
# Now we are ready to plot again.
# First plot mds by calling plot() with type='n' (it will create the axes but not plot the points)
# you set the title and axes labels in the call to plot()
# then call text() with the presidents' names as labels and the color argument
# col = cols[presParty[rownames(presDist)]]
  
#plot(mds,type = 'n',xlab='',ylab='',main="Presidents")
#text(mds, labels=unique(speechesDF$initial),col = cols[presParty[rownames(presParty)]], cex = 0.6,adj = 1.8)



### Use hierarchical clustering to produce a visualization of  the results.
# Compare the two plots.
#hc = hclust(as.dist(presDist))
#plot(hc)
## Final part 
# Use the data in the dataframe speechesDF to create the plots:
# x-axis: speech year, y-axis: # of sentences
# x-axis: speech year, y-axis: # of words
# x-axis: speech year, y-axis: # of characters
# x-axis: speech year, y-axis: average word length (char/word)
# x-axis: speech year, y-axis: average sentence length (word/sent)

# your plot statements below:

plot(speechesDF$speechYr,speechesDF$sent,xlab = 'Year',ylab = 'Sentences',col="red",main="Year vs Sentences")
plot(speechesDF$speechYr,speechesDF$words,xlab = 'Year',ylab = 'Words',col="blue", main = "Year vs Words")
plot(speechesDF$speechYr,speechesDF$chars,xlab = 'Year',ylab = 'Characters',col="grey",main="Year vs Characters")
plot(speechesDF$speechYr,(speechesDF$chars)/(speechesDF$words),xlab = 'Year',ylab = 'Average Word Length', main = "Year vs Avg Word Length")
plot(speechesDF$speechYr,(speechesDF$words)/(speechesDF$sent),xlab = 'Year',ylab = 'Average Sentence Length',col="green", main = "Year vs Avg Sentence Length")


