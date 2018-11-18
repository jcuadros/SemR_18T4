#Extracting Coordinates and ID from KML  
kml.text <- readLines("arbres.kml") 
Encoding(kml.text) <- "UTF-8"
kml.text <- paste(kml.text,collapse="")
kml.text <- unlist(strsplit(kml.text,"</Placemark>"))

re <- " *([^<]+?) *<\\/coordinates>"  
coords <- grep(re,kml.text)  
re3 <- " *([^<]+?) *<\\/name>[^<]*?<Point>"  
Name <- grep(re3,kml.text)  

kml.coordinates <- matrix(0,length(coords),3,dimnames=list(c(),c("LAT","LON","ELEV")))  
kml.names <- matrix(0,length(coords),1)  


for(i in 1:length(coords)){  
  sub.coords <- coords[i]  
  temp0 <- gsub("<\\/coord.*","",kml.text[sub.coords])  
  temp1 <- gsub("\t+","",temp0)  
  temp2 <- gsub("\\s+","",temp1)  
  temp3 <- gsub(".*nates>","",temp2)
  coordinates <- as.numeric(unlist(strsplit(temp3,",")))  
  
  sub.Name <- Name[i]  
  temp0 <- gsub("<\\/name>[^<]*?<Point>.*$","",kml.text[sub.Name])  
  temp1 <- gsub(".*<name>","",temp0)  
  temp2 <- gsub(".*[[]","",temp1)  
  temp3 <- gsub("[]].*$","",temp2)  
  ID <- temp3  
  
  kml.coordinates[i,] <- matrix(c(coordinates),ncol=3)  
  kml.names[i,] <- matrix(c(ID),ncol=1)  
}  


shinyServer(function(input, output, session) {
  output$mapArbres <- renderLeaflet({
      mapa <- leaflet() %>%
      addProviderTiles(providers$OpenTopoMap,
                       options = providerTileOptions(noWrap = TRUE)) %>%
      addMarkers(lng=kml.coordinates[,1], lat=kml.coordinates[,2]) %>%
      setView(lat=42.0420507,lng=3.118198, zoom=10)
  })  
  
  observe({
    if (!is.null(input$mapArbres_zoom))
      if (input$mapArbres_zoom > 12) {
        
        leafletProxy("mapArbres") %>%
          clearShapes() %>% clearPopups() %>%
          addPopups(lng=kml.coordinates[,1], lat=kml.coordinates[,2],
                    popup=kml.names[,1],
                    options =popupOptions(autoPan=FALSE))
        } else
        leafletProxy("mapArbres") %>%
          clearPopups() %>% clearShapes() %>%
          addMarkers(lng=kml.coordinates[,1], lat=kml.coordinates[,2])
  })
})

