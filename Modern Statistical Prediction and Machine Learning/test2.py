import os, re
from collections import Counter
import time
start = time.time()

#baseDir = os.getcwd()
fileList = []
with open('common-english-words.txt') as wordFile:
	contents = wordFile.read()
stopWords = contents.split(',')
for folder in os.listdir('.'):
	if not os.path.isfile(folder):
		#currentDir = os.path.join(baseDir,folder)
		for file in os.listdir(folder):
			if file == '.DS_Store':
				continue
			with open(os.path.join(folder,file)) as currentFile:
				contents = currentFile.read()
			contents = re.sub('[^0-9a-zA-Z]+',' ',contents)
			contents = contents.lower()
			wordList = contents.split()
			wordList = [i for i in wordList if i not in stopWords]
			wordFreq = Counter(wordList)
			fileList.append(wordFreq)

			
print('Execution took ' + str(time.time() - start) + ' seconds')