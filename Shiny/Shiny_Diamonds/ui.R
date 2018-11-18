library(shiny); library(ggplot2)

numvars <- names(diamonds[, sapply(diamonds, is.numeric)])
factvars <- c("None", names(diamonds[, sapply(diamonds, is.factor)]))

shinyUI(fluidPage(
  titlePanel("Diamonds"),
  
  # Escull el layout
  sidebarLayout(
    
    # Panell lateral
    sidebarPanel(
      actionButton("start", "Calculate"),
      sliderInput("sampleSize", "Mida de la mostra",
                  value = 5000, min = 1000, max = nrow(diamonds), step = 500, round = 0),
      selectInput('x', 'X', numvars),
      selectInput('y', 'Y', numvars, numvars[2]),
      selectInput("color", "Tria color i variable sobre la que es fa el boxplot", factvars, "None"),
      selectInput('facet', 'Facets', factvars),
      sliderInput("alpha", "Transparency", min = 0, max = 1, value = 0.8),
      radioButtons(inputId = "geom", label = "Tipus de grafic:",
                   choices = c("X Y" = "points",
                               "Boxplot" = "boxplot",
                               "Jitter" = "jitter"), 
                   selected = "X Y"),
      radioButtons(inputId = "method", label = "Regression method:",
                   choices = c("None" = "None",
                               "Simple Linear Regression" = "lm",
                               "Local Regression" = "loess"), 
                   selected = "None")
    ),
    
    # Panell central
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Grafic", plotOutput("DiamondsPlot")),
                  tabPanel("Dades", dataTableOutput("DiamondsTable"))
      )
    )
  )
))
      
  