library(data.table)


# Read in the prediction table


nGram <- fread('./nGram/predictionTableFull.csv')

nextWordsApp <- function(input_string, n) {
        
        ## step1: remove numbers and punctuations
        filteredList <- gsub('[[:punct:]]|[[:digit:]]', "", tolower(input_string))
        # step2: strsplit by all white spaces
        filteredList <- unlist(strsplit(filteredList, "\\s+"))
        
        ## step3: extract last six words of the query
        if (length(filteredList) > 6) {
                filteredList <- filteredList[(length(filteredList)-5):length(filteredList)] #make query length 6
                filteredStr <- paste(filteredList, collapse = " ") #combine back to sentence
        } else {
                filteredStr <- paste(filteredList, collapse = " ") #combine back to sentence
        }
        
        ## step4: return all the matched words
        predicted <- nGram[nGram$query %like% filteredStr]$predict
        if (length(predicted) > 0) {
                #tries septgram
                result <- predicted
        } else {
                #no result
                filteredStr <- paste(filteredList[2:length(filteredList)], collapse = " ") #remove 1st word
                predicted <- nGram[nGram$query %like% filteredStr]$predict
                if (length(predicted) > 0) {
                        #tries sexgarm
                        result <- predicted
                } else {
                        #no result
                        filteredStr <- paste(filteredList[3:length(filteredList)], collapse = " ") #remove 2nd word
                        predicted <- nGram[nGram$query %like% filteredStr]$predict
                        if (length(predicted) > 0) {
                                #tries quintgram
                                result <- predicted
                        } else {
                                #no result
                                filteredStr <- paste(filteredList[4:length(filteredList)], collapse = " ") #remove 3rd word
                                predicted <- nGram[nGram$query %like% filteredStr]$predict
                                if (length(predicted) > 0) {
                                        #tries quintgram
                                        result <- predicted
                                } else {
                                        #no result
                                        filteredStr <- paste(filteredList[5:length(filteredList)], collapse = " ") #remove 4th word
                                        predicted <- nGram[nGram$query %like% filteredStr]$predict
                                        if (length(predicted) > 0) {
                                                #tries trigram
                                                result <- predicted
                                        } else {
                                                #no result
                                                filteredStr <- paste(filteredList[6:length(filteredList)], collapse = " ") #remove 5th word (one word left)
                                                predicted <- nGram[nGram$query %like% filteredStr]$predict
                                                if (length(predicted) > 0) {
                                                        #tries bigram
                                                        result <- predicted
                                                } else {
                                                        #no result
                                                        result <- 'the' #most common word
                                                }
                                        }
                                }
                        }
                }  
        }
        return(result[1:n])
} #end 

nextWordsApp("i am the", 1)