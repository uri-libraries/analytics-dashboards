devicesTableUI <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    titlePanel("Device Usage Data Table"),
    
    fluidRow(
      column(12,
             p("Raw device usage data with filtering and export options.")
      )
    ),
    
    # Filters
    fluidRow(
      column(12,
             wellPanel(
               fluidRow(
                 column(3,
                        selectInput(ns("location_filter"), "Filter by Location:", 
                                    choices = c("All"),
                                    selected = "All")
                 ),
                 column(3,
                        selectInput(ns("device_filter"), "Filter by Device Type:", 
                                    choices = c("All"),
                                    selected = "All")
                 ),
                 column(3,
                        dateRangeInput(ns("date_range"), "Date Range:",
                                       start = Sys.Date() - 30,
                                       end = Sys.Date())
                 ),
                 column(3,
                        downloadButton(ns("download_devices"), "Download CSV", 
                                       class = "btn-primary", style = "margin-top: 25px;")
                 )
               )
             )
      )
    ),
    
    # Data table
    fluidRow(
      column(12,
             box(
               title = "Device Usage Data", 
               status = "success",
               solidHeader = TRUE,
               width = NULL,
               div(style = "overflow-x: auto;",
                   DT::dataTableOutput(ns("device_table"))
               )
             )
      )
    )
  )
}

devicesTableServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Read device data
    device_data <- reactive({
      tryCatch({
        # Replace 'device_usage_data.csv' with your actual file name
        data <- read.csv('data/device_usage_data.csv')
        return(data)
      }, error = function(e) {
        return(data.frame(
          Message = "No device data file found",
          Instructions = "Please add your device usage CSV to the data/ folder"
        ))
      })
    })
    
    # Render table
    output$device_table <- DT::renderDataTable({
      DT::datatable(
        device_data(),
        options = list(
          pageLength = 25,
          scrollX = TRUE,
          autoWidth = TRUE
        ),
        filter = 'top',
        rownames = FALSE
      )
    })
    
    # Download handler
    output$download_devices <- downloadHandler(
      filename = function() {
        paste("device_usage_", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        write.csv(device_data(), file, row.names = FALSE)
      }
    )
  })
}