############
############ this is an application to provide a sample of the datasets I was mainly working 
library(shiny)

# Define UI for application
ui <- fluidPage(
   
  title = 'Examples of DataTables',
  sidebarLayout( ## adding attributes to application
    sidebarPanel(
      conditionalPanel(
        'input.dataset === "main"', 
        helpText('Click the column header to sort the dataset.'),
        checkboxGroupInput('show_vars', 'Columns in main to show:',
                           names(main), selected = names(main))
      ),
      conditionalPanel(
        'input.dataset === "electricalFail1"',
        helpText('Click the column header to sort the dataset.'),
        checkboxGroupInput('show_vars', 'Columns in main to show:',
                          names(electricalFail1), selected = names(electricalFail1))
      ),
      conditionalPanel(
        'input.dataset === "misuse"',
        helpText('Click the column header to sort the dataset.'),
        checkboxGroupInput('show_vars', 'Columns in misuse to show:',
                          names(misuse), selected = names(misuse))
      )
    ),
    mainPanel(
      tabsetPanel(
        id = 'dataset',
        tabPanel('main', DT::dataTableOutput('mytable1')),
        tabPanel('electricalFail1', DT::dataTableOutput('mytable2')),
        tabPanel('misuse', DT::dataTableOutput('mytable3'))
      )
    )
  )
)

## define server for Application
server <- function(input, output) {
  # choose sample columns to display
  main = main[sample(nrow(main), 1000), ]
  output$mytable1 <- DT::renderDataTable({
    DT::datatable(main[, input$show_vars, drop = FALSE])
  })
  # choose sample columns to display
  electricalFail1 = electricalFail1[sample(nrow(electricalFail1), 1000), ]
  output$mytable2 <- DT::renderDataTable({
    DT::datatable(electricalFail1, options = list(orderClasses = TRUE))
  })
  # choose sample columns to display
  misuse = misuse[sample(nrow(misuse), 1000), ]
  output$mytable3 <- DT::renderDataTable({
    DT::datatable(misuse, options = list(lengthMenu = c(5, 30, 50), pageLength = 5))
  })
  
}




# Run the application 
shinyApp(ui = ui, server = server)

