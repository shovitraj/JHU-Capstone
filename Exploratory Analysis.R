
library(dplyr)
library(data.table)
library(wordcloud)
library(stringi)
library(R.utils)
library(quanteda)
library(ggplot2)
library(wordcloud)  
library(gridExtra)

rm(list=ls())
if(!file.exists("~/Desktop/Data")){
        dir.create("~/Desktop/Data")
}
url <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
filedest= "~/Desktop/Data/Coursera-SwiftKey.zip"
if(!file.exists("~/Desktop/Data/Coursera-SwiftKey.zip")){
        download.file(url,filedest, mode = "wb")
}

if(!file.exists("~/Desktop/Data/final")){
        unzip(zipfile="~/Desktop/Data/Coursera-SwiftKey.zip",exdir="~/Desktop/Data")
}       

#file Path
blogs_path <- "~/Desktop/Data/final/en_US/en_US.blogs.txt"
twitter_path <- "~/Desktop/Data/final/en_US/en_US.twitter.txt"
news_path <- "~/Desktop/Data/final/en_US/en_US.news.txt"

#file size in Mb
sb <- file.info(blogs_path)$size/1024^2
st <- file.info(twitter_path)$size/1024^2
sn <- file.info(news_path)$size/1024^2

#read lines
blogs<-readLines(blogs_path,warn=FALSE,encoding="UTF-8")
twitter<-readLines(twitter_path,warn=FALSE,encoding="UTF-8")
news<-readLines(news_path,warn=FALSE,encoding="UTF-8")

# count words per line
nwlblogs <- stri_count_words(blogs)
nwltwitter <- stri_count_words(twitter)
nwlnews<- stri_count_words(news)

#count number of words
nwblogs  <- wordcount(blogs, sep = " ")
nwtwitter <- wordcount(twitter, sep = " ")
nwnews  <- wordcount(news, sep = " ")

#count number of lines
nlblogs <- countLines(blogs_path)
nltwitter <- countLines(twitter_path)
nlnews <- countLines(news_path)


data <- data.table(
        Items = c("Blogs", "Twitter", "News"),
        FileName=c("en_US.blogs.txt","en_US.twitter.txt", "en_US.news.txt "),
        Size_MB = c(sb, st, sn),
        Words = c(nwblogs, nwtwitter, nwnews),
        Lines = c(nlblogs, nltwitter, nlnews),
        AWP = c(mean(nwlblogs), mean(nwltwitter), mean(nwlnews)),
        MWP = c(median(nwlblogs), median(nwltwitter), median(nwlnews))
        
)

data

ratio = data.frame(ratio=c(nwblogs/nlblogs, nwtwitter/nltwitter, nwnews/nlnews), media=as.factor(c("Blogs", "Twitter", "News")))
ggplot(data = ratio, aes(x=media, y= ratio, fill =media)) + 
        geom_bar(stat="identity") + 
        labs(title="Ratio of Words/Line in different media sources", x="Media Source",y="Ratio of Words/Line")


set.seed(165)
#selecting ten percent of data as training data
samplesize <- 0.2

#Sample for each source
blogsS <- blogs[sample(1:length(blogs), length(blogs) * samplesize)]
twitterS<- twitter[sample(1:length(twitter), length(twitter) * samplesize)]
newsS <- news[sample(1:length(news), length(news) * samplesize)]
writeLines(c(blogsS, twitterS, newsS), 'sample.txt')

if(!file.exists("./swearWords.txt")){
        download.file(url = "https://www.cs.cmu.edu/~biglou/resources/bad-words.txt",
                      destfile= "./swearWords.txt",
                      method = "curl")
}
profanity <- readLines("./swearWords.txt",warn=FALSE, encoding = "UTF-8")

Sample <- readLines('sample.txt', encoding='UTF-8', skipNul=TRUE)
Sample <- tolower(Sample)


summary_Sample <- data.frame(
        file = './Clean/Analysissample.txt', 
        size = file.info('./Clean/Analysissample.txt')$size/(1024*1024),
        line_count = length(Sample), 
        word_count = sum(sapply(gregexpr('\\W+', Sample), length)), 
        char_count = sum(nchar(Sample)))
kable(summary_Sample, row.names = NA, col.names = c('File', 'Size (MB)', 'Line Count', 'Word Count', 'Character Count'))


t <- tokens(Sample,
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
rm(Sample)
#Apply stemmer to words
t <- tokens_wordstem(t, language = "english") 
t.1gram <- tokens_ngrams(t, n = 1, concatenator = " ")
t.2gram <- tokens_ngrams(t, n = 2, concatenator = " ")
t.3gram <- tokens_ngrams(t, n = 3, concatenator = " ")
t.4gram <- tokens_ngrams(t, n = 4, concatenator = " ")
t.5gram <- tokens_ngrams(t, n = 5, concatenator = " ")
t.6gram <- tokens_ngrams(t, n = 6, concatenator = " ")
t.7gram <- tokens_ngrams(t, n = 7, concatenator = " ")

unigram <- dfm(t.1gram, verbose = FALSE,remove = profanity )
bigram <- dfm(t.2gram, verbose = FALSE, remove = profanity)
trigram <- dfm(t.3gram, verbose = FALSE, remove = profanity)
quadgram <- dfm(t.4gram, verbose = FALSE, remove = profanity)
quintgram <- dfm(t.5gram, verbose = FALSE, remove = profanity)
sexgram <- dfm(t.6gram, verbose = FALSE, remove = profanity)
quintgram <- dfm(t.6gram, verbose = FALSE, remove = profanity)

uniGram <- textstat_frequency(unigram)
biGram <- textstat_frequency(bigram)
triGram <- textstat_frequency(trigram)
quadGram <- textstat_frequency(quadgram)
quintGram <- textstat_frequency(quintgram)
sexGram <- textstat_frequency(sexgram)
quintGram <- textstat_frequency(quintgram)


write.csv(uniGram, file = 'uniGram.csv', row.names = F)
write.csv(biGram, file = 'biGram.csv', row.names = F)
write.csv(triGram, file = 'triGram.csv', row.names = F)
write.csv(quadGram, file = 'quadGram.csv', row.names = F)

generatePred <- function(inputFile, thresh = 1L) {
        
        ## This function makes the prediction look up table
        ## inputFile: the ngram csv file generated from quanteda
        ## thresh: threshold to remove low frequency words (default is 1)
        
        nGram <- fread(inputFile, select = c('feature', 'frequency'))
        nGram <- nGram[nGram$frequency > thresh]
        
        nGram <- nGram[, query := strsplit(feature, " [^ ]+$")][]
        nGram <- nGram[, predict := sub('.* (.*)$','\\1', feature)][]
        
        fwrite(nGram, paste0(sub('.csv', '', inputFile), 'Pred.csv'))
        
}


# Generate nGrams (n = 2:4)
generatePred('uniGram.csv')
generatePred('biGram.csv')
generatePred('triGram.csv')
generatePred('quadGram.csv')


# Read in all predictions

nGram <- fread('nGramPred.csv', select = c('query', 'predict', 'frequency'))
head(nGram)
tail(nGram)
nGram <- nGram[order(-frequency)]
head(nGram)
tail(nGram)


# Filter out frequency < 5 word combinations

nGramFilt <- nGram[frequency >= 5]
fwrite(nGramFilt, file = 'predictionTableFull.csv')

# Only keep the unique queries (for predicting one word) and frequency >= 5

nGramUni <- nGram[(!duplicated(nGram$query)) & (frequency >= 5)]
fwrite(nGramUni, file = 'predictionTableUni.csv')

