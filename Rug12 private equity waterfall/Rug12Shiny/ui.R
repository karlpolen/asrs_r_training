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
  titlePanel("PE Structure Analysis"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
       sliderInput("pref",
                   "Preferred Return:",
                   min = 0,
                   max = 20,
                   value = 8),
       sliderInput("carry",
                   "Carry",
                   min=0,
                   max=100,
                   value=20),
       sliderInput("catchup",
                   "Catchup",
                   min=0,
                   max=100,
                   value=50),
       sliderInput("amfee",
                   "Asset Management Fee",
                   min=0,
                   max=5,
                   value=1.5,
                   step=.25)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
       plotOutput("distPlot",width=500,height=1000)
    )
  )
))
