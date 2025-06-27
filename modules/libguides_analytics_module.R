libguidesAnalyticsUI <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    div(
      style = "border-top: 3px solid #007bff; padding-top: 20px; margin-top: 20px;",
      titlePanel("LibGuides Analytics Dashboard"),
      
      # Back button
      fluidRow(
        column(12,
               actionButton(ns("back_to_overview"), "← Back to Overview", 
                            class = "btn-secondary", style = "margin-bottom: 20px;")
        )
      ),
      
      # Filters row
      fluidRow(
        column(12,
               wellPanel(
                 fluidRow(
                   column(4,
                          numericInput(ns("top_n"), "Number of Top Guides:", 
                                       value = 10, min = 5, max = 20, step = 1)
                   ),
                   column(4,
                          selectInput(ns("usage_filter"), "Usage Category:", 
                                      choices = c("All"),
                                      selected = "All")
                   ),
                   column(4,
                          p(strong("Dashboard Status:"), style = "margin-top: 25px;"),
                          textOutput(ns("status"))
                   )
                 )
               )
        )
      ),
      
      # Charts in 2x2 grid
      fluidRow(
        column(6,
               box(
                 title = "Top LibGuides by Total Usage",
                 status = "primary",
                 solidHeader = TRUE,
                 width = NULL,
                 height = "450px",
                 plotOutput(ns("top_guides"), height = "380px")
               )
        ),
        column(6,
               box(
                 title = "Monthly Usage Trends (Top 5 Guides)",
                 status = "primary", 
                 solidHeader = TRUE,
                 width = NULL,
                 height = "450px",
                 plotOutput(ns("monthly_trends"), height = "380px")
               )
        )
      ),
      
      fluidRow(
        column(6,
               box(
                 title = "Distribution of Guide Usage",
                 status = "primary",
                 solidHeader = TRUE,
                 width = NULL,
                 height = "450px",
                 plotOutput(ns("usage_distribution"), height = "380px")
               )
        ),
        column(6,
               box(
                 title = "Monthly Usage Heatmap (Top 15 Guides)",
                 status = "primary",
                 solidHeader = TRUE,
                 width = NULL,
                 height = "450px",
                 plotOutput(ns("heatmap"), height = "380px")
               )
        )
      ),
      
      # Summary statistics
      fluidRow(
        column(12,
               box(
                 title = "Summary Statistics",
                 status = "info",
                 solidHeader = TRUE,
                 width = NULL,
                 verbatimTextOutput(ns("summary_stats"))
               )
        )
      )
    ),
    
    # JavaScript for back button
    tags$script(HTML(paste0("
      $(document).on('click', '#", ns("back_to_overview"), "', function() {
        $('#", session$ns(""), "libguides_dashboard').hide();
        $('html, body').animate({scrollTop: 0}, 500);
      });
    ")))
  )
}

libguidesAnalyticsServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Status output
    output$status <- renderText({
      "Loading data..."
    })
    
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
        return(NULL)
      })
    })
    
    # Update status
    observe({
      if (is.null(df())) {
        output$status <- renderText({"Error: Cannot load data"})
      } else {
        output$status <- renderText({paste("Data loaded:", nrow(df()), "guides")})
      }
    })
    
    # Define months
    months <- c('X2024.06', 'X2024.07', 'X2024.08', 'X2024.09', 'X2024.10', 'X2024.11', 
                'X2024.12', 'X2025.01', 'X2025.02', 'X2025.03', 'X2025.04', 'X2025.05', 'X2025.06')
    
    # Update filter choices
    observe({
      req(df())
      updateSelectInput(session, "usage_filter",
                        choices = c("All", unique(df()$Usage_Category)))
    })
    
    # Filtered data
    filtered_data <- reactive({
      req(df())
      data <- df()
      if (input$usage_filter != "All") {
        data <- data %>% filter(Usage_Category == input$usage_filter)
      }
      data
    })
    
    # All your existing plot outputs go here (top_guides, monthly_trends, etc.)
    # I'll include just one as an example - add the rest from your previous code
    
    output$top_guides <- renderPlot({
      req(filtered_data())
      
      top_guides <- filtered_data() %>% 
        arrange(desc(Total)) %>% 
        head(input$top_n) %>%
        mutate(Guide.Name = ifelse(nchar(Guide.Name) > 30, 
                                   paste0(substr(Guide.Name, 1, 30), "..."), 
                                   Guide.Name))
      
      ggplot(top_guides, aes(x = reorder(Guide.Name, Total), y = Total)) +
        geom_col(fill = "steelblue", alpha = 0.8) +
        coord_flip() +
        labs(x = "Guide Name", y = "Total Views") +
        theme_minimal() +
        theme(axis.text.y = element_text(size = 10))
    })
    
    # Add your other plots here (monthly_trends, usage_distribution, heatmap, summary_stats)
  })
}