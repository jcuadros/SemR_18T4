#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggthemes)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  output$distPlot <- renderPlot({
    
    # generate bins based on input$bins from ui.R
    #x    <- faithful[, 2] 
    nbins <- input$bins
    coloresc <- input$radio
    
    # draw the histogram with the specified number of bins
    #hist(x, breaks = bins, col = 'darkgray', border = 'white')
    ggplot(faithful,aes(x=waiting))+geom_histogram(bins = nbins, fill=coloresc ,col="darkgray")+
      theme_bw() + labs(title="Histograma del temps d'espera") +
      theme(plot.title=element_text(size=24))
    
  }) 
  
})
