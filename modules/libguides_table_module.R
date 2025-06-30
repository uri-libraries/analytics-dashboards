libguidesTableUI <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    titlePanel("LibGuides Data Table"),
    
    fluidRow(
      column(12,
             p("LibGuides usage data with export options.")
      )
    ),
    
    # Download button
    fluidRow(
      column(12,
             wellPanel(
               fluidRow(
                 column(9,
                        h4("LibGuides Usage Data")
                 ),
                 column(3,
                        downloadButton(ns("download_libguides"), "Download CSV", 
                                      class = "btn-primary", style = "margin-top: 5px;")
                 )
               )
             )
      )
    ),
    
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
    
    # Reactive data
    libguides_data <- reactive({
      tryCatch({
        read.csv('data/libguides-stats - guide-stats.csv')
      }, error = function(e) {
        data.frame(Error = "Could not load data")
      })
    })
    
    output$table <- DT::renderDataTable({
      DT::datatable(libguides_data(), options = list(scrollX = TRUE, pageLength = 15))
    })
    
    # Download handler
    output$download_libguides <- downloadHandler(
      filename = function() {
        paste("libguides-data-", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        write.csv(libguides_data(), file, row.names = FALSE)
      }
    )
  })
}