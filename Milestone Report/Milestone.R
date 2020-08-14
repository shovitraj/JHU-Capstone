
library(dplyr)
library(wordcloud)
library(ngram)
library(stringi)
library(R.utils)
library(quanteda)
library(ggplot2)
library(tidytext)
library(plotly)
library(data.table)
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
samplesize <- 0.1 

#Sample for each source
blogsS <- blogs[sample(1:length(blogs), length(blogs) * samplesize)]
twitterS<- twitter[sample(1:length(twitter), length(twitter) * samplesize)]
newsS <- news[sample(1:length(news), length(news) * samplesize)]

# remove all non-English characters from the sampled data
blogsS <- iconv(blogsS, "latin1", "ASCII", sub = "")
newsS  <- iconv(newsS , "latin1", "ASCII", sub = "")
twitterS <- iconv(twitterS, "latin1", "ASCII", sub = "")
Sample <- c(blogsS, twitterS, newsS)

if(!file.exists("./swearWords.txt")){
        download.file(url = "https://www.cs.cmu.edu/~biglou/resources/bad-words.txt",
                      destfile= "./swearWords.txt",
                      method = "curl")
}
profanity <- readLines("./swearWords.txt",warn=FALSE, encoding = "UTF-8")

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
#Apply stemmer to words
t <- tokens_wordstem(t, language = "english") 
t.1gram <- tokens_ngrams(t, n = 1, concatenator = " ")
t.2gram <- tokens_ngrams(t, n = 2, concatenator = " ")
t.3gram <- tokens_ngrams(t, n = 3, concatenator = " ")
t.4gram <- tokens_ngrams(t, n = 4, concatenator = " ")

unigram <- dfm(t.1gram, verbose = FALSE,remove = profanity )
bigram <- dfm(t.2gram, verbose = FALSE, remove = profanity)
trigram <- dfm(t.3gram, verbose = FALSE, remove = profanity)
quadgram <- dfm(t.4gram, verbose = FALSE, remove = profanity)

#' Save separated words for prediction
saveRDS(unigram, "~/Desktop/Data/final/en_US/uni_words.rds")
saveRDS(bigram, "~/Desktop/Data/final/en_US/bi_words.rds")
saveRDS(trigram, "~/Desktop/Data/final/en_US/tri_words.rds")
saveRDS(quadgram, "~/Desktop/Data/final/en_US/quad_words.rds")


topUniVector <- topfeatures(unigram, 10)
topUniVector <- sort(topUniVector, decreasing = FALSE)
topUniDf <- data.frame(words = names(topUniVector), freq = topUniVector)
topUniPlot <- ggplot(data = topUniDf, aes(x = reorder(words, freq), y = freq, fill = freq)) + 
        geom_bar(stat = "identity") +
        theme_minimal() +
        labs(x = "Unigram", y = "Frequency") +
        labs(title = "Ten most common Unigrams") +
        theme(plot.title = element_text(hjust = 0.5))+ 
        coord_flip() +
        guides(fill=FALSE) 
g1<- ggplotly(topUniPlot)
g1

topBiVector <- topfeatures(bigram, 10)
topBiVector <- sort(topBiVector, decreasing = FALSE)
topBiDf <- data.frame(words = names(topBiVector), freq = topBiVector)
topBiPlot <- ggplot(data = topBiDf, aes(x = reorder(words, freq), y = freq, fill = freq)) + 
        geom_bar(stat = "identity") +
        theme_minimal() +
        labs(x = "Bigram", y = "Frequency") +
        labs(title = "Ten most common Bigrams") +
        theme(plot.title = element_text(hjust = 0.5)) +
        coord_flip() +
        guides(fill=FALSE) 
g2<- ggplotly(topBiPlot)
g2


topTriVector <- topfeatures(trigram, 10)
topTriVector <- sort(topTriVector, decreasing = FALSE)
topTriDf <- data.frame(words = names(topTriVector), freq = topTriVector)
topTriPlot <- ggplot(data = topTriDf, aes(x = reorder(words, freq), y = freq, fill = freq)) + 
        geom_bar(stat = "identity") +
        theme_minimal() +
        labs(x = "Trigram", y = "Frequency") +
        labs(title = "Ten most common Trigrams") +
        theme(plot.title = element_text(hjust = 0.5)) +
        coord_flip() +
        guides(fill=FALSE) 
g3<- ggplotly(topTriPlot)
g3

topQuadVector <- topfeatures(quadgram, 10)
topQuadVector <- sort(topQuadVector, decreasing = FALSE)
topQuadDf <- data.frame(words = names(topQuadVector), freq = topQuadVector)
topQuadPlot <- ggplot(data = topQuadDf, aes(x = reorder(words, freq), y = freq, fill = freq)) + 
        geom_bar(stat = "identity") +
        theme_minimal() +
        labs(x = "Quadgram", y = "Frequency") +
        labs(title = "Ten most common Quadgrams") +
        theme(plot.title = element_text(hjust = 0.5)) +
        coord_flip() +
        guides(fill=FALSE) 
g4<- ggplotly(topQuadPlot)
g4

grid.arrange(g1,g2, g3, g4, nrow = 2, ncol = 2)