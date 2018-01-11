from scipy.io import mmread, mmwrite
from scipy.sparse import csr_matrix
import matplotlib.pyplot as plot
from math import log
import numpy as np
import random
from sklearn.feature_selection import SelectKBest, chi2
from itertools import compress

#read feature names, matrix
with open('stemmed_feature_names2.txt', 'r') as wordFile:
	contents = wordFile.read()
featureNames = contents.split(',')[1:]
data = csr_matrix(mmread('stemmed_sparse_word_feature_matrix2.mtx'))

#get number of books each feature appears in
docFreq = data.getnnz(0)[1:].tolist()

#produce histogram, save to file
plot.hist(docFreq,bins=100,log=True)
plot.title("Document Frequency Distribution")
plot.xlabel("Document Frequency")
plot.ylabel("Number of Words (log scale)")
fig1 = plot.gcf()
fig1.savefig('document_frequency.png',dpi=300)

plot.clf()

plot.hist([log(i,10) for i in docFreq],bins=100,log=True)
plot.title("Document Frequency Distribution")
plot.xlabel("Document Frequency (log base 10 scale)")
plot.ylabel("Number of Words (log scale)")
fig1 = plot.gcf()
fig1.savefig('document_frequency_logx.png',dpi=300)

#order by frequency
order = np.argsort(docFreq)[::-1]
orderedDocFreq = [docFreq[i] for i in order]
orderedFeatureNames = [featureNames[i] for i in order]

#remove additional stop words
print(str(data.shape[0]) + ' documents, 50% threshold is ' + str(data.shape[0]/2))
numOverHalf = sum([i>=data.shape[0]/2 for i in orderedDocFreq])
#words make sense, common and probably don't differentiate between classes much
print(orderedFeatureNames[:numOverHalf])
#also drop the bottom words, <10 docs based on histogram - super rare
numUnderTen = sum([i<10 for i in orderedDocFreq])
#delete those, make new matrix
indicesToKeep = order[numOverHalf:len(order)-numUnderTen]
indicesToKeep2 = [i+1 for i in indicesToKeep]
indicesToKeep2.insert(0,0)
data = data[:,indicesToKeep2]
featureNames = orderedFeatureNames[numOverHalf:len(orderedFeatureNames)-numUnderTen]

#sample for supervised feature filtering, n% of data?
#looking for high inclusion ratio for words, if a word appears nowhere in the sample
#it's probably too rare anyway
#10% gives >90%, 15% gives >95%, 20% gives >98% inclusion
#using 10% for now to conserve training data
random.seed(0)
sampleIndices = random.sample(range(0,data.shape[0]),data.shape[0]//10)
filterSet = data[sampleIndices,:]
filterSetDocFreq = filterSet.getnnz(0)[1:].tolist()
inclusionRatio = sum([i!=0 for i in filterSetDocFreq])/len(filterSetDocFreq)

#remove filterSet from data, we won't touch the filterSet for future model training
sampleIndicesSet = set(sampleIndices)
data = data[[i for i in range(0,data.shape[0]) if i not in sampleIndicesSet],:]

#select 2000 features based on chisq
selector = SelectKBest(chi2,k=2002)
X = filterSet[:,1:]
y = filterSet[:,0].toarray().transpose().tolist()[0]
selector.fit(X,y)
selected = selector.get_support()
finalFeatureNames = list(compress(featureNames,selected))
finalFeatureNames.insert(0,'_CLASSIFICATION')

#found a problem - word 'gutenberg', 'gutenberg-tm' appears in the selected list, manually removing
problemIndex = featureNames.index('gutenberg-tm')
selected[problemIndex] = False
problemIndex = featureNames.index('gutenberg')
selected[problemIndex] = False

finalFeatureNames = list(compress(featureNames,selected))
finalFeatureNames.insert(0,'_CLASSIFICATION')

finalFeatureIndices = list(compress(range(1,data.shape[1]),selected))
finalFeatureIndices.insert(0,0)
finalData = data[:,finalFeatureIndices]

#write list of feature names, in same order
with open('final_word_feature_names.txt', 'w+') as outputFile:
	outputFile.write(','.join(finalFeatureNames))
#write sparse matrix to Matrix Market Exchange Format
mmwrite('final_word_feature_matrix.mtx', finalData)
#write as csv
#temp = finalData.toarray()
#temp.tofile("final_word_feature_matrix.csv",sep=',')