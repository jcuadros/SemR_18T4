##=========================================================================##
##                                                                         ##
##  Creació d'aplicacions interactives amb Shiny                           ##
##  @authors: Vanessa Serrano, Jordi Cuadros, Francesc Martori             ##
##                                                                         ##
##=========================================================================##

#### GESTIÓ DE PAQUETS ####
# Especialment útil per a scripts d'execució remota

options(install.packages.check.source = "no")

# Canviar el llistat de paquets segons calgui
pckgs<-c("tidyverse","shiny","ggthemes","RColorBrewer","carData")
pckgs2Install<-pckgs[!(pckgs %in% library()$results[,1])]
pckgs2Load<-pckgs[!(pckgs %in% (.packages()))]
for(pckg in pckgs2Install) {install.packages(pckg,repos="https://cloud.r-project.org/",
                                             quiet=TRUE, type="binary")}
for(pckg in pckgs2Load) {library(pckg,character.only = TRUE)}


#### DATA PRODUCTS ####
# Corresponen al resultat d'un projecte de dades.

# Els més comuns son:
# - Informes i presentacions ==>> Rmarkdown
#   https://rmarkdown.rstudio.com/ 
# - Aplicacions interactives ==>> Shiny
#   http://shiny.rstudio.com/tutorial/


#### APLICACIONS INTERACTIVES SHINY ####
### Exemple 1.1: Aplicació mínima ####

ui <- fluidPage(
  "Hello world!"
)
server <- function(input, output) {}
shinyApp(ui = ui, server = server)

? fluidPage


### Exemple 1.2: Creació d'aplicacions interactives amb Shiny (amb títol i estructura) ####

# Layouts 
# https://shiny.rstudio.com/articles/layout-guide.html

ui <- fluidPage(
  
  # Application title
  titlePanel("Creació d'aplicacions interactives amb Shiny"),
  
  sidebarLayout(
    
    sidebarPanel(),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Plot"), 
        tabPanel("Summary"), 
        tabPanel("Table")
      )
    )
  )
)  
 
server <- function(input, output) {}

shinyApp(ui = ui, server = server)


### Exemple 1.3: Creació d'aplicacions interactives amb Shiny (amb widgets) ####

# Widgets
# Input: http://shiny.rstudio.com/gallery/widget-gallery.html
# Output: https://shiny.rstudio.com/reference/shiny/latest/ 

ui <- fluidPage(
  
  # Application title
  titlePanel("Creació d'aplicacions interactives amb Shiny"),
  
  sidebarLayout(
    
    sidebarPanel(
      radioButtons("inGenere", label = "Gender",
                   choices = list("ALL" = "ALL", "Female" = "Female", "Male" = "Male"), 
                   selected = "ALL")
    ), 
    
    mainPanel(
      tabsetPanel(
        tabPanel("Plot",
                 plotOutput("ouGrafic")), 
        tabPanel("Summary",
                 verbatimTextOutput("ouResums")       
        ), 
        tabPanel("Table",
                 dataTableOutput("ouDades"))
      )
    )
  ) 
)  

server <- function(input, output, session) {}
shinyApp(ui = ui, server = server)


### Exemple 1.4: Creació d'aplicacions interactives amb Shiny (amb dades i outputs) ####

# Rendering functions: https://shiny.rstudio.com/reference/shiny/latest/ 

ui <- fluidPage(
  
  # Application title
  titlePanel("Creació d'aplicacions interactives amb Shiny"),
  
  sidebarLayout(
    
    sidebarPanel(
      radioButtons("inGenere", label = "Gender",
                   choices = list("ALL", "Female", "Male"), 
                   selected = "ALL")
    ), 
    
    mainPanel(
      tabsetPanel(
        tabPanel("Plot",
                 plotOutput("ouGrafic")), 
        tabPanel("Summary",
                 verbatimTextOutput("ouResums")       
        ), 
        tabPanel("Table",
                 dataTableOutput("ouDades"))
      )
    )
  ) 
)  

server <- function(input, output, session) {
  dfSLID <- carData::SLID

  dfSLIDsel <- reactive({
    if(input$inGenere == "ALL") dfSLID else dfSLID %>% filter(sex==input$inGenere)
  })  
  
  output$ouGrafic <- renderPlot({
    ggplot(dfSLIDsel(),aes(x=wages,color=language)) + geom_density(size=1) + theme_bw()
  })
  
  output$ouResums <- renderText(paste("Mean: ", mean(dfSLIDsel()$wages, na.rm = TRUE), sep=""))
  
  output$ouDades <- renderDataTable(dfSLIDsel())
  
}
shinyApp(ui = ui, server = server)


#### EXERCICIS 1 #### 
# 1- Crea una aplicación Shiny base des de RStudio (Demo), estructurada 
# en dos fitxers. Explora-la i mira com està programada

# 2- Canvia el control del nombre de barres del histograma a una entrada 
# numèrica (numericInput)

# 3- Refés l'histograma amb ggplot2

# 4- Afegeix un conjunt de botons d'opció per a canviar el color de les
# barres (radioButtons)


#### NOUS EXEMPLES ####

shinyAppDir("Shiny_Diamonds")
shinyAppDir("Shiny_Arbres")

#### REACTIVITAT ####


#### GESTIÓ DE VARIABLES i PAQUETS ####


#### EXERCICIS 2 #### 
# 1- Millora el Shiny_Diamonds per a corregir els errors que detectis

# 2- Amplia el Shiny creat per a analitzar el conjunt de dades SLID,
# fent-ne un Shiny de dos fitxers




#### COMPARTIR SHINY APPS ####

