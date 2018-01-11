import os, re, time
from collections import Counter
from csv import DictWriter


start = time.time()

with open('word.feature.list.txt', 'r') as wordFile:
	contents = wordFile.read()
wordFeatures = contents.split(',')

count = 0
fileList = []
for folder in os.listdir('.'):
	if not os.path.isfile(folder):
		for file in os.listdir(folder):
			if file == '.DS_Store':
				continue
			with open(os.path.join(folder,file),'r') as currentFile:
				contents = currentFile.read()
			contents = re.sub('[^0-9a-zA-Z]+',' ',contents)
			contents = contents.lower()
			wordList = contents.split()
			wordFreq = Counter(wordList)
			wordFreq['_CLASSIFICATION'] = int(folder[-2])
			fileList.append(wordFreq)
			count += 1

with open('word_features.csv', 'w+', newline = '') as csvfile:
	writer = DictWriter(csvfile, fieldnames=wordFeatures, restval = 0, extrasaction='ignore')
	writer.writeheader()
	for i in fileList:
		writer.writerow(i)
		
print('Execution on ' + str(count) + ' files took ' + str(time.time() - start) + ' seconds')
print('Wrote word feature matrix with ' + str(len(wordFeatures)-1) + ' features and ' + str(count) + ' observations')
