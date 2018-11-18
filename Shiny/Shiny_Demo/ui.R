library(shiny)

shinyUI(fluidPage(
  titlePanel("DSaR - Sessió 4"),
  
  # Escull el layout
  sidebarLayout(
    
    # Panell lateral
    sidebarPanel(
      sliderInput("obs",
                  "Number of observations:",
                  min = 10,
                  max = 1000,
                  value = 500)
    ),
    
    # Panell central
    mainPanel(
      plotOutput("distPlot")
    )
  )
))
      
  