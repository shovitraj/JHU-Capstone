library(shiny)
library(shinydashboard)
library(data.table)

shinyServer(function(input,output){
    # Load the prediction function
    source("nextWordsApp.R")
    # Load the prediction table
    nGram <- fread('predictionFreqTable.csv')
    
    # Predict next word
    observe({ #need this for reactive mode!!!
        textin <- as.character(input$textin)
        n <- input$wordN
        result <- nextWordsApp(textin, n, nGram)
        
        if (textin == '') {
            output$predicted <- renderPrint(cat(''))
        } else {
            output$predicted <- renderPrint(cat(result, sep = '\n'))
        }
        
    })
    
})