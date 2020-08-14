

library(quanteda)
library(data.table)
library(readtext)

Prediction <- function(inputFile, rmv = 1) {
        
        ## This function makes the prediction look up table
        ## inputFile: the ngram csv file generated from quanteda
        ## rmv: threshold to remove low frequency words (default is 1)
        
        nGram <- fread(inputFile, select = c('feature', 'frequency'))
        nGram <- nGram[nGram$frequency > rmv]
        
        nGram <- nGram[, query := strsplit(feature, " [^ ]+$")][]
        nGram <- nGram[, predict := sub('.* (.*)$','\\1', feature)][]
        
        fwrite(nGram, paste0(sub('.csv', '', inputFile), 'Pred.csv'))
        
}


# Generate nGrams (n = 2:7)
Prediction('/Users/shovitraj/Data/nGram/uniGram.csv')
Prediction('/Users/shovitraj/Data/nGram/biGram.csv')
Prediction('/Users/shovitraj/Data/nGram/triGram.csv')
Prediction('/Users/shovitraj/Data/nGram/quadGram.csv')
Prediction('/Users/shovitraj/Data/nGram/quintGram.csv')
Prediction('/Users/shovitraj/Data/nGram/sexGram.csv')
Prediction('/Users/shovitraj/Data/nGram/septGram.csv')


uni<- read.csv('uniGramPred.csv')
bi<- read.csv('biGramPred.csv')
tri<- read.csv('triGramPred.csv')
quad<- read.csv('quadGramPred.csv')
quint<- read.csv('quintGramPred.csv')
six<- read.csv('sexGramPred.csv')
sept<- read.csv('septGramPred.csv')


nGramPrediction <- rbind(uni, bi, tri, quad, quint, six, sept )

dim(nGramPrediction)
names(nGramPrediction)

write.csv(nGramPrediction, file = 'nGramPrediction.csv', row.names = F)

# Read in all predictions

nGram <- fread('nGramPrediction.csv', select = c('query', 'predict', 'frequency'))
nGram <- nGram[order(-frequency)]

# Filter out frequency < 5 word combinations to avoid most common short words like "the"
nGram_filter <- nGram[frequency >= 5]
fwrite(nGram_filter, file = 'predictionTable.csv')

# Filter the unique queries and frequency >=5
nGramUnique <- nGram[(!duplicated(nGram$query)) & (frequency >= 5)]
fwrite(nGramUnique, file = 'predictionTableUnique.csv')


