# This file was created to extract the Sample data from 3 different files provided. 
# First all the Lines from 3 different files were read and combined into one. 
# Then 25% of the data was chosen to create a corpus. A large corpus takes longer processing time. 
# I tried creating corpus from 5% to 25%  of the sampledata. Prediction got better with higher sample text.
# With 


library(stringi)
library(R.utils)
library(readtext)


rm(list=ls())
if(!file.exists("/Users/shovitraj/Data")){
        dir.create("/Users/shovitraj/Data")
}
url <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
filedest= "/Users/shovitraj/Data/Coursera-SwiftKey.zip"
if(!file.exists("/Users/shovitraj/Data/Coursera-SwiftKey.zip")){
        download.file(url,filedest, mode = "wb")
}

if(!file.exists("/Users/shovitraj/Data/final")){
        unzip(zipfile="/Users/shovitraj/Data/Coursera-SwiftKey.zip",exdir="/Users/shovitraj/Data")
}       

#file Path
blogs_path <- "/Users/shovitraj/Data/final/en_US/en_US.blogs.txt"
twitter_path <- "/Users/shovitraj/Data/final/en_US/en_US.twitter.txt"
news_path <- "/Users/shovitraj/Data/final/en_US/en_US.news.txt"

#read lines
blogs<-readLines(blogs_path,warn=FALSE,encoding="UTF-8")
twitter<-readLines(twitter_path,warn=FALSE,encoding="UTF-8")
news<-readLines(news_path,warn=FALSE,encoding="UTF-8")



set.seed(165)
#selecting ten percent of data as training data
samplesize <- 0.25

#Sample for each source

blogsS <- blogs[sample(1:length(blogs), length(blogs) * samplesize)]
twitterS<- twitter[sample(1:length(twitter), length(twitter) * samplesize)]
newsS <- news[sample(1:length(news), length(news) * samplesize)]
if(!file.exists("/Users/shovitraj/Data/Sample")){
        dir.create("/Users/shovitraj/Data/Sample")
}

#write sample file
writeLines(c(blogsS, twitterS, newsS), '/Users/shovitraj/Data/Sample/sample.txt')

