library(R.utils)
library(stringi)
library(quanteda)
library(data.table)
library(readtext)

rm(list=ls())
#load parallel function
source('doParallel.R')
cluster <- startCluster()
cluster

#read data
Sample <- readLines('/Users/shovitraj/Data/Sample/sample.txt', encoding='UTF-8', skipNul=TRUE)

#create corpus
Sample <- corpus(Sample)
corp <- tolower(Sample)

rm(Sample)
#run parallel this is when everything takes time
cluster


start <- Sys.time()
#create token
t <- tokens(corp,
            what="word",
            remove_numbers = TRUE,
            remove_punct = TRUE,
            remove_url =TRUE,
            remove_separators = TRUE,
            remove_symbols = TRUE,
            remove_twitter = TRUE,
            verbose = TRUE)

# Remove stopwords 
t <- tokens_replace(t,pattern =stopwords("english"),replacement=stopwords("english")) 
#Set lower case for every word
t <- tokens_tolower(t)  
rm(corp)
#Apply stemmer to words
t <- tokens_wordstem(t, language = "english") 


#download profanity word list 
if(!file.exists("./swearWords.txt")){
        download.file(url = "https://www.cs.cmu.edu/~biglou/resources/bad-words.txt",
                      destfile= "./swearWords.txt",
                      method = "curl")
}
profanity <- readLines("./swearWords.txt",warn=FALSE, encoding = "UTF-8") 

#create tokens in sequence n=1:7 from tokenized object.
t.1gram <- tokens_ngrams(t, n = 1, concatenator = " ")
t.2gram <- tokens_ngrams(t, n = 2, concatenator = " ")
t.3gram <- tokens_ngrams(t, n = 3, concatenator = " ")
t.4gram <- tokens_ngrams(t, n = 4, concatenator = " ")
t.5gram <- tokens_ngrams(t, n = 5, concatenator = " ")
t.6gram <- tokens_ngrams(t, n = 6, concatenator = " ")
t.7gram <- tokens_ngrams(t, n = 7, concatenator = " ")

#construct a sparse document-feature matrix, from a token. remove ngrams to clear memory. 

unigram <- dfm(t.1gram, verbose = FALSE,remove = profanity )
rm(t.1gram)
bigram <- dfm(t.2gram, verbose = FALSE, remove = profanity)
rm(t.2gram)
trigram <- dfm(t.3gram, verbose = FALSE, remove = profanity)
rm(t.3gram)
quadgram <- dfm(t.4gram, verbose = FALSE, remove = profanity)
rm(t.4gram)
quintgram <- dfm(t.5gram, verbose = FALSE, remove = profanity)
rm(t.5gram)
sexgram <- dfm(t.6gram, verbose = FALSE, remove = profanity)
rm(t.6gram)
septgram <- dfm(t.7gram, verbose = FALSE, remove = profanity)
rm(t.7gram)

#produce counts and document frequencies summaties of the features in dfm
uniGram <- textstat_frequency(unigram)
rm(unigram)
biGram <- textstat_frequency(bigram)
rm(bigram)
triGram <- textstat_frequency(trigram)
rm(trigram)
quadGram <- textstat_frequency(quadgram)
rm(quadgram)
quintGram <- textstat_frequency(quintgram)
rm(quintgram)
sexGram <- textstat_frequency(sexgram)
rm(sexgram)
septGram <- textstat_frequency(septgram)
rm(septgram)

end<- Sys.time()
tokentime <- end- start


#write the count and document frequencies as csv file
write.csv(uniGram, file = '/Users/shovitraj/Data/nGram/uniGram.csv', row.names = F)
rm(uniGram)
write.csv(biGram, file = '/Users/shovitraj/Data/nGram/biGram.csv', row.names = F)
rm(biGram)
write.csv(triGram, file = '/Users/shovitraj/Data/nGram/triGram.csv', row.names = F)
rm(triGram)
write.csv(quadGram, file = '/Users/shovitraj/Data/nGram/quadGram.csv', row.names = F)
rm(quadGram)
write.csv(quintGram, file = '/Users/shovitraj/Data/nGram/quintGram.csv', row.names = F)
rm(quintGram)
write.csv(sexGram, file = '/Users/shovitraj/Data/nGram/sexGram.csv', row.names = F)
rm(sexGram)
write.csv(septGram, file = '/Users/shovitraj/Data/nGram/septGram.csv', row.names = F)
rm(septGram)








