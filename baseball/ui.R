#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Correlation between hit and Run scored",aline = "center"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            selectInput("var1","select x variable",choices = c("H"=1,"R"=2,"ERA"=3)),
            selectInput("var2","select Y variable",choices = c("H"=1,"R"=2,"ERA"=3))
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput(h4("The correlation coefficient between the two bariables is"),
                       textOutput("correlation"),br(),
                       h4("Scatterplot between the two variables"),
                       plotOutput("plot"))
        )))
