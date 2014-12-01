library(shiny)
library(datasets)
data(iris)
irisdataset <- iris

shinyServer(function(input, output) {
    output$plot <- renderPlot({
        plant <- input$spc
        data <- subset(irisdataset, iris$Species == plant)   
        
        plot(data$Sepal.Length, data$Sepal.Width, main = "A plot of the Sepal size", xlab = "Sepal Length", 
             ylab = "Sepal Width",  xaxt="n")
        
        
    })
})