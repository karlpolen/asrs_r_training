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
  titlePanel("Bond Analysis"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
       sliderInput("coupon",
                   "Coupon:",
                   min = 1,
                   max = 20,
                   value = 10),
       sliderInput("term",
                   "Term",
                   min=1,
                   max=30,
                   value=10)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
       plotOutput("distPlot",width=500,height=1000)
    )
  )
))
