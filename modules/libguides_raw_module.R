libguidesRawUI <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    titlePanel("LibGuides Raw Data"),
    
    fluidRow(
      column(12,
             p("This section provides access to the raw LibGuides usage data in table format."),
             br()
      )
    ),
    
    # Filters
    fluidRow(
      column(12,
             wellPanel(
               fluidRow(
                 column(3,
                        selectInput(ns("usage_filter"), "Filter by Usage Category:", 
                                    choices = c("All"),
                                    selected = "All")
                 ),
                 column(3,
                        numericInput(ns("min_views"), "Minimum Views:", 
                                     value = 0, min = 0, step = 1)
                 ),
                 column(3,
                        selectInput(ns("sort_by"), "Sort by:", 
                                    choices = c("Total Views" = "Total",
                                                "Guide Name" = "Guide.Name"),
                                    selected = "Total")
                 ),
                 column(3,
                        radioButtons(ns("sort_order"), "Sort Order:",
                                     choices = c("Descending" = "desc", "Ascending" = "asc"),
                                     selected = "desc")
                 )
               )
             )
      )
    ),
    
    # Summary boxes
    fluidRow(
      valueBoxOutput(ns("total_guides")),
      valueBoxOutput(ns("total_views")), 
      valueBoxOutput(ns("avg_views"))
    ),
    
    # Data table
    fluidRow(
      column(12,
             box(
               title = "LibGuides Usage Data", 
               status = "primary",
               solidHeader = TRUE,
               width = NULL,
               div(style = "overflow-x: auto;",
                   DT::dataTableOutput(ns("raw_table"))
               )
             )
      )
    ),
    
    # Download section
    fluidRow(
      column(12,
             box(
               title = "Export Data",
               status = "info",
               width = NULL,
               p("Download the filtered data in various formats:"),
               br(),
               downloadButton(ns("download_csv"), "Download CSV", class = "btn-primary"),
               br(), br(),
               downloadButton(ns("download_excel"), "Download Excel", class = "btn-success")
             )
      )
    )
  )
}

libguidesRawServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Read data
    df <- reactive({
      tryCatch({
        data <- read.csv('data/libguides-stats - guide-stats.csv')
        
        # Add usage categories
        data$Usage_Category <- case_when(
          data$Total >= 5000 ~ 'High (5000+)',
          data$Total >= 1000 ~ 'Medium (1000-4999)',
          data$Total >= 500 ~ 'Low (500-999)',
          TRUE ~ 'Very Low (<500)'
        )
        
        return(data)
      }, error = function(e) {
        showNotification("Error loading LibGuides data", type = "error")
        return(data.frame())
      })
    })
    
    # Update filter choices
    observe({
      req(df())
      if (nrow(df()) > 0) {
        updateSelectInput(session, "usage_filter",
                          choices = c("All", unique(df()$Usage_Category)))
      }
    })
    
    # Filtered and sorted data
    filtered_data <- reactive({
      req(df())
      data <- df()
      
      # Filter by usage category
      if (input$usage_filter != "All") {
        data <- data %>% filter(Usage_Category == input$usage_filter)
      }
      
      # Filter by minimum views
      data <- data %>% filter(Total >= input$min_views)
      
      # Sort data
      if (input$sort_order == "desc") {
        data <- data %>% arrange(desc(!!sym(input$sort_by)))
      } else {
        data <- data %>% arrange(!!sym(input$sort_by))
      }
      
      return(data)
    })
    
    # Value boxes
    output$total_guides <- renderValueBox({
      valueBox(
        value = nrow(filtered_data()),
        subtitle = "Total Guides",
        icon = icon("book"),
        color = "blue"
      )
    })
    
    output$total_views <- renderValueBox({
      valueBox(
        value = format(sum(filtered_data()$Total, na.rm = TRUE), big.mark = ","),
        subtitle = "Total Views",
        icon = icon("eye"),
        color = "green"
      )
    })
    
    output$avg_views <- renderValueBox({
      valueBox(
        value = round(mean(filtered_data()$Total, na.rm = TRUE), 1),
        subtitle = "Average Views",
        icon = icon("chart-bar"),
        color = "yellow"
      )
    })
    
    # Data table
    output$raw_table <- DT::renderDataTable({
      req(filtered_data())
      
      DT::datatable(
        filtered_data(),
        options = list(
          pageLength = 25,
          scrollX = TRUE,
          autoWidth = TRUE,
          columnDefs = list(
            list(className = 'dt-right', targets = which(sapply(filtered_data(), is.numeric)) - 1)
          )
        ),
        filter = 'top',
        rownames = FALSE
      ) %>%
        DT::formatStyle(
          'Total',
          background = styleColorBar(range(filtered_data()$Total, na.rm = TRUE), 'lightblue'),
          backgroundSize = '100% 90%',
          backgroundRepeat = 'no-repeat',
          backgroundPosition = 'center'
        )
    })
    
    # Download handlers
    output$download_csv <- downloadHandler(
      filename = function() {
        paste("libguides_data_", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        write.csv(filtered_data(), file, row.names = FALSE)
      }
    )
    
    output$download_excel <- downloadHandler(
      filename = function() {
        paste("libguides_data_", Sys.Date(), ".xlsx", sep = "")
      },
      content = function(file) {
        if (requireNamespace("openxlsx", quietly = TRUE)) {
          openxlsx::write.xlsx(filtered_data(), file)
        } else {
          showNotification("openxlsx package required for Excel export", type = "warning")
          write.csv(filtered_data(), file, row.names = FALSE)
        }
      }
    )
  })
}