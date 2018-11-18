options(install.packages.check.source = "no")

# Canviar el llistat de paquets segons calgui
pckgs<-c("tidyverse","shiny","ggthemes","RColorBrewer","leaflet")
pckgs2Install<-pckgs[!(pckgs %in% library()$results[,1])]
pckgs2Load<-pckgs[!(pckgs %in% (.packages()))]
for(pckg in pckgs2Install) {install.packages(pckg,repos="https://cloud.r-project.org/",
                                             quiet=TRUE, type="binary")}
for(pckg in pckgs2Load) {library(pckg,character.only = TRUE)}


# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  title="Arbres monumentals de Catalunya",
  
  # Sidebar with a slider input for number of bins 
  fixedPanel(
      leafletOutput("mapArbres", width="100%", height="100%"),
      left=0, top=0, right=0, bottom=0
  )
))
