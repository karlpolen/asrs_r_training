#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(gridExtra)
require(scales)
require(Deriv)
bondval=function(r,C,n,freq=1) {
  #simplified bond valuation
  #values bond at issuance or immediately after a coupon payment normalized to a face value of 1
  #where:
  #r is the market interest rate, decimal value (i.e. .02 for 2%)
  #C is the contractual coupon rate, decimal value
  #n is the number of time periods in years, can be fractional
  #freq is the frequency of payments, one for annual, two for semi-annual, etc
  #example call: bondval(.02,.04,2+1/2,2) #evaluates 4% coupon, semi-annual payments, 2% market rate, 2 1/2 years left
  r=r/freq
  C=C/freq
  n=n*freq
  C/r+((1-C/r)*(1+r)^-n)
}

bond1deriv=Deriv(bondval,"r") #take the first derivative with respect to r
duration=function(r,C,n) {
  (-1/bondval(r,C,n))*(bond1deriv(r,C,n))
}

bond2deriv=Deriv(bondval,"r",nderiv=2)
convexity=function(r,C,n) {
  (1/bondval(r,C,n))*(bond2deriv(r,C,n))
}

r=seq(.01,.15,.001)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  output$distPlot <- renderPlot({
    
    # generate plot
    C=input$coupon/100
    n=input$term
    bond1val=sapply(r,bondval,C,n)
    valplot=ggplot()+geom_line(aes(x=r,y=bond1val))+
      xlab("Market Interest Rate")+
      ylab("Bond Value")+
      scale_x_continuous(labels=scales::percent)+ #note use of scales to format x axis as percent
      ggtitle(paste0("Value of a ",n," year bond with ",100*C,"% annual coupon"))
    bond1dur=sapply(r,duration,C,n)
    durplot=ggplot()+geom_line(aes(x=r,y=bond1dur))+
      xlab("Market Interest Rate")+
      ylab("Duration")+
      scale_x_continuous(labels=scales::percent)+ #note use of scales to format x axis as percent
      ggtitle(paste0("Duration of a ",n," year bond with ",100*C,"% annual coupon"))
    bond1convex=sapply(r,convexity,C,n)
    conplot=ggplot()+geom_line(aes(x=r,y=bond1convex))+
      xlab("Market Interest Rate")+
      ylab("Convexity")+
      scale_x_continuous(labels=scales::percent)+ #note use of scales to format x axis as percent
      ggtitle(paste0("Convexity of a ",n," year bond with ",100*C,"% annual coupon"))
    grid.arrange(valplot,durplot,conplot,nrow=3)
    
  })
  
})
