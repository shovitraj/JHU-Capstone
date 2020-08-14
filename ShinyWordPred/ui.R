library(shiny)
library(shinydashboard)

shinyUI(
    dashboardPage(skin="blue",
        dashboardHeader(title="Predict the next word"),
        dashboardSidebar(disable = TRUE),
        dashboardBody(
            fluidRow(
                box(
                    strong('Instructions'),
                    tags$ul(
                        tags$li(p('Enter word/phrase')),
                        tags$li(p('Select number of words to be predicted (1 to 6)')),
                    ),
                    strong('Output'),
                    p('The predicted next word will be displayed in the descending order of frequency'),
                ),
                box(
                    title= "Enter text", solidHeader=TRUE, status="primary",background="black",
                    textAreaInput('textin', "Enter text", value =" ",cols=50, rows=3),
                ),
                box(
                    title="Number of predictions",solidHeader=TRUE,status="primary",background="black",
                    sliderInput('wordN', '',
                                min = 1, max = 6, value = 1, step = 1),
                ),
                box(
                    title="Next Predicted Word", solidHeader = TRUE, status="primary",background="black",
                    verbatimTextOutput('predicted')
                )
            )
        )
    )        
)
  
