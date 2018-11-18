dataset <- diamonds

shinyServer(function(input, output) {
  
  dataset <- reactive({
    diamonds[sample(nrow(diamonds), input$sampleSize),]
  })
  
  DrawChart <- eventReactive(input$start, {
    chart <- ggplot(dataset()) 
    
    if(input$geom == "points") {
      chart <- chart + aes_string(x = input$x, y = input$y) + geom_point(alpha = input$alpha)
    } else if(input$geom == "boxplot") {
      chart <- chart + aes_string(x = input$color, y = input$y) + geom_boxplot()
    } 
    
    if(input$color != "None")
      chart <- chart + aes_string(color=input$color)

    if(input$facet != "None")
      chart <- chart + facet_wrap(c(input$facet))
    
    if(input$method != "None") 
      chart <- chart + geom_smooth(method=input$method)
    
    print(chart)
      
  })
  
  output$DiamondsPlot <- renderPlot({
    DrawChart()
  }, width = 1200, height = 720)
  
  output$DiamondsTable <- renderDataTable(dataset())
})