library(shiny)

shinyUI(pageWithSidebar(
    headerPanel("Predictive text model"),
    sidebarPanel( width = 4.5, 
                  
                  h3('Write your word here'),
                  h6('Two words result on a faster output.'),
                  
                  tags$textarea(id="word", rows=3, cols=40, "--- Wait, loading data ---"),
                  tags$head(tags$style(type="text/css", "#word {width: 500px}")),
                  h6('Wait for the word "!!!TRY ME!!!" to show up.')
                  
    ),
    mainPanel(
        
        h4('You entered:'),
        verbatimTextOutput("oid1"),
        h4('Possible next three words:'),
        verbatimTextOutput("oid2"),
        h3('How does this app works:'),
        p(''),
        includeMarkdown("explanation.Rmd"),
        
        code('Made by: Rodrigo Bertollo de Alexandre')
        
    )
))

# shinyUI(pageWithSidebar(
#     headerPanel("Example plot"),
#     sidebarPanel(
#         sliderInput('mu', 'Guess at the mu',value = 70, min = 60, max = 80, step = 0.05,)
#     ),
#     mainPanel(
#         plotOutput('newHist')
#     )
# ))
