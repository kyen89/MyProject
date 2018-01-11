import os

#currently just iterates through all the files, puts their contents into a big list.
baseDir = os.getcwd()
fileList = []
for folder in os.listdir('.'):
	if not os.path.isfile(folder):
		#currentDir = os.path.join(baseDir,folder)
		for file in os.listdir(folder):
			if file == '.DS_Store':
				continue
			with open(os.path.join(folder,file)) as currentFile:
				contents = currentFile.read()
				fileList.append(contents)