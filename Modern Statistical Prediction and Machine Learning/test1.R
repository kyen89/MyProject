#Scratch 
readLines("041.txt")
file("stateoftheunion1790-2012.txt")
readLines(con=file("stateoftheunion1790-2012.txt"))


#load the data
folder = c("Child(0)","History(1)","Religion(2)","Science(3)")
filenames <- list.files(folder,pattern="*",full.names=T)
ldf <- lapply(filenames, readLines)





