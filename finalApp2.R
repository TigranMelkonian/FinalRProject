

library(shiny)

# Define UI for application 
ui <- fluidPage(
  pageWithSidebar(
    headerPanel('Residential fire(caused by electrical failures) k-means clustering'),
    sidebarPanel(
      selectInput('xcol', 'X Variable', "Year"),
      selectInput('ycol', 'Y Variable', "number Of Occurances"),
                  selected=names(kmeansPlot)[[2]]),
      numericInput('clusters', 'Cluster count', 8,
                   min = 1, max = 8)
    ),
    mainPanel(
      plotOutput('plot1')
    )
  )


# Define server logic required
server <- function(input, output) {
  # Combine the selected variables into a new data frame
  selectedData <- reactive({
    kmeansPlot[, c( "Year", "NumberOfOccurances")]
  })
  
  clusters <- reactive({
    kmeans(selectedData(), input$clusters)
  })
  
  output$plot1 <- renderPlot({
    palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
              "#FF7F00", "#FFFF33", "#A65628", "#999999"))
    
    par(mar = c(5.1, 4.1, 0, 1))
    plot(selectedData(),
         col = clusters()$cluster,
         pch = 20, cex = 3)
    points(clusters()$centers, pch = 4, cex = 4, lwd = 4)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

