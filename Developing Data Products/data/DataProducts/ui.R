library(shiny)

shinyUI(fluidPage(
    
    titlePanel("Select Species"),
    
    sidebarLayout(
        sidebarPanel(
            helpText("Different species have different Sepals sizes"),
            
            helpText("A graphic of the sizes of the Sepal can be easily created"),
            
            selectInput("spc", "Choose a Specie:", choices = c('virginica','versicolor','setosa'))
            
            
        ),
        
        mainPanel(
            plotOutput("plot")
        )
    )
))