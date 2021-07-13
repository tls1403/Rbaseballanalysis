#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(Lahman)

# Define server logic required to draw a histogram
shinyServer(function(input, output) ({
    x <- reactive({dat[,as.numeric(input$var1)]})
    y <- reactuve({dat[,as.numeric(input$var2)]})
    output$correlation <- renderPrint({cor(x(),y())})
    output$plot <- renderPlot(({plot(x(),y())}))
        }))
