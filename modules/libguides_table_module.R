libguidesTableUI <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    titlePanel("LibGuides Data Table"),
    
    fluidRow(
      column(12,
             box(
               title = "LibGuides Usage Data", 
               status = "primary",
               solidHeader = TRUE,
               width = NULL,
               DT::dataTableOutput(ns("table"))
             )
      )
    )
  )
}

libguidesTableServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    output$table <- DT::renderDataTable({
      tryCatch({
        df <- read.csv('data/libguides-stats - guide-stats.csv')
        DT::datatable(df, options = list(scrollX = TRUE, pageLength = 15))
      }, error = function(e) {
        DT::datatable(data.frame(Error = "Could not load data"))
      })
    })
  })
}