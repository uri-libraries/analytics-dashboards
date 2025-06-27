overviewUI <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    fluidRow(
      column(12,
             h1("Library Analytics Hub", class = "text-center"),
             br(),
             p("Welcome to the central dashboard for library analytics.", class = "text-center")
      )
    ),
    
    fluidRow(
      column(4,
             actionButton(ns("show_libguides"), 
                          label = div(
                            icon("book", class = "fa-4x", style = "color: #007bff; margin-bottom: 15px;"),
                            h3("LibGuides Analytics", style = "color: #007bff; margin-bottom: 10px;"),
                            p("Interactive visualizations and trends analysis", style = "font-size: 14px;"),
                            div(
                              style = "background-color: #007bff; color: white; padding: 8px 16px; border-radius: 5px; display: inline-block; margin-top: 10px;",
                              strong("Click to View Dashboard")
                            )
                          ),
                          style = "width: 100%; height: auto; padding: 30px; border: 3px solid #007bff; border-radius: 15px; background: white;",
                          class = "btn"
             )
      ),
      column(4,
             div(
               style = "text-align: center; padding: 30px; border: 2px solid #6c757d; border-radius: 15px; margin: 10px; opacity: 0.7;",
               icon("laptop", class = "fa-4x", style = "color: #6c757d; margin-bottom: 15px;"),
               h3("Device Usage", style = "color: #6c757d; margin-bottom: 10px;"),
               p("Monitor computer and device utilization", style = "font-size: 14px;"),
               div(
                 style = "background-color: #6c757d; color: white; padding: 8px 16px; border-radius: 5px; display: inline-block; margin-top: 10px;",
                 em("Coming Soon")
               )
             )
      ),
      column(4,
             div(
               style = "text-align: center; padding: 30px; border: 2px solid #6c757d; border-radius: 15px; margin: 10px; opacity: 0.7;",
               icon("chart-bar", class = "fa-4x", style = "color: #6c757d; margin-bottom: 15px;"),
               h3("More Analytics", style = "color: #6c757d; margin-bottom: 10px;"),
               p("Additional analytics modules in development", style = "font-size: 14px;"),
               div(
                 style = "background-color: #6c757d; color: white; padding: 8px 16px; border-radius: 5px; display: inline-block; margin-top: 10px;",
                 em("Coming Soon")
               )
             )
      )
    ),
    
    br(),
    
    # Navigation guide
    fluidRow(
      column(12,
             div(
               class = "alert alert-info",
               style = "text-align: center;",
               h4("Navigation Guide"),
               p("• Click the ", strong("LibGuides Analytics"), " button above for interactive visualizations"),
               p("• Use ", strong("LibGuides Table"), " in the top menu for raw data tables"),
               p("• Return to ", strong("Overview"), " anytime using the top navigation")
             )
      )
    ),
    
    br(),
    
    # LibGuides Dashboard (initially hidden)
    conditionalPanel(
      condition = paste0("input['", ns("show_libguides"), "'] > 0"),
      div(
        style = "border-top: 3px solid #007bff; padding-top: 20px; margin-top: 20px;",
        
        fluidRow(
          column(12,
                 h2("LibGuides Analytics Dashboard"),
                 actionButton(ns("hide_libguides"), "← Back to Overview", 
                              class = "btn-secondary", style = "margin-bottom: 20px;")
          )
        ),
        
        # Filters
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
                            p(strong("Status:"), style = "margin-top: 25px;"),
                            textOutput(ns("status"))
                     )
                   )
                 )
          )
        ),
        
        # Charts
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
        
        # Summary
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
      )
    )
  )
}

overviewServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Data loading
    df <- reactive({
      tryCatch({
        data <- read.csv('data/libguides-stats - guide-stats.csv')
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
    
    # Define months
    months <- c('X2024.06', 'X2024.07', 'X2024.08', 'X2024.09', 'X2024.10', 'X2024.11', 
                'X2024.12', 'X2025.01', 'X2025.02', 'X2025.03', 'X2025.04', 'X2025.05', 'X2025.06')
    
    # Update status
    output$status <- renderText({
      if (is.null(df())) {
        "Error loading data"
      } else {
        paste("Loaded:", nrow(df()), "guides")
      }
    })
    
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
    
    # 1. Top guides plot
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
    
    # 2. Monthly trends
    output$monthly_trends <- renderPlot({
      req(filtered_data())
      
      top_5 <- filtered_data() %>% 
        arrange(desc(Total)) %>% 
        head(5)
      
      trend_data <- top_5 %>%
        select(Guide.Name, all_of(months)) %>%
        pivot_longer(cols = all_of(months), names_to = "Month", values_to = "Views") %>%
        mutate(Month = gsub("X", "", Month),
               Month = gsub("\\.", "-", Month),
               Guide.Name = ifelse(nchar(Guide.Name) > 20, 
                                   paste0(substr(Guide.Name, 1, 20), "..."), 
                                   Guide.Name))
      
      ggplot(trend_data, aes(x = Month, y = Views, color = Guide.Name, group = Guide.Name)) +
        geom_line(linewidth = 1.2) +
        geom_point(size = 2) +
        labs(x = "Month", y = "Views", color = "Guide Name") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              legend.position = "bottom",
              legend.text = element_text(size = 8)) +
        scale_color_brewer(type = "qual", palette = "Set2")
    })
    
    # 3. Usage distribution
    output$usage_distribution <- renderPlot({
      req(filtered_data())
      
      data <- filtered_data()
      mean_total <- mean(data$Total, na.rm = TRUE)
      median_total <- median(data$Total, na.rm = TRUE)
      
      ggplot(data, aes(x = Total)) +
        geom_histogram(bins = 30, fill = "lightblue", color = "black", alpha = 0.7) +
        geom_vline(xintercept = mean_total, color = "red", linetype = "dashed", linewidth = 1) +
        geom_vline(xintercept = median_total, color = "green", linetype = "dashed", linewidth = 1) +
        labs(x = "Total Views", y = "Number of Guides") +
        theme_minimal() +
        annotate("text", x = mean_total * 1.1, y = Inf, 
                 label = paste("Mean:", round(mean_total, 0)), 
                 hjust = 0, vjust = 2, color = "red", size = 3) +
        annotate("text", x = median_total * 1.1, y = Inf, 
                 label = paste("Median:", round(median_total, 0)), 
                 hjust = 0, vjust = 4, color = "green", size = 3)
    })
    
    # 4. Heatmap
    output$heatmap <- renderPlot({
      req(filtered_data())
      
      top_15 <- filtered_data() %>% 
        arrange(desc(Total)) %>% 
        head(15) %>%
        mutate(Guide.Name = ifelse(nchar(Guide.Name) > 25, 
                                   paste0(substr(Guide.Name, 1, 25), "..."), 
                                   Guide.Name))
      
      heatmap_data <- top_15 %>%
        select(Guide.Name, all_of(months)) %>%
        pivot_longer(cols = all_of(months), names_to = "Month", values_to = "Views") %>%
        mutate(Month = gsub("X", "", Month),
               Month = gsub("\\.", "-", Month))
      
      ggplot(heatmap_data, aes(x = Month, y = reorder(Guide.Name, Views), fill = Views)) +
        geom_tile() +
        scale_fill_gradient(low = "white", high = "darkblue") +
        labs(x = "Month", y = "Guide Name", fill = "Views") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              axis.text.y = element_text(size = 8))
    })
    
    # 5. Summary stats
    output$summary_stats <- renderText({
      req(filtered_data())
      
      data <- filtered_data()
      most_popular <- data$Guide.Name[which.max(data$Total)]
      least_popular <- data$Guide.Name[which.min(data$Total)]
      
      # Calculate monthly averages
      monthly_averages <- data %>%
        select(all_of(months)) %>%
        summarise_all(mean, na.rm = TRUE) %>%
        pivot_longer(everything(), names_to = "Month", values_to = "Average_Views") %>%
        mutate(Month = gsub("X", "", Month),
               Month = gsub("\\.", "-", Month))
      
      highest_month <- monthly_averages$Month[which.max(monthly_averages$Average_Views)]
      lowest_month <- monthly_averages$Month[which.min(monthly_averages$Average_Views)]
      
      paste0(
        "=== LibGuides Usage Summary ===\n",
        "Total number of guides: ", nrow(data), "\n",
        "Total views across all guides: ", format(sum(data$Total, na.rm = TRUE), big.mark = ","), "\n",
        "Average views per guide: ", round(mean(data$Total, na.rm = TRUE), 1), "\n",
        "Median views per guide: ", round(median(data$Total, na.rm = TRUE), 1), "\n",
        "Most popular guide: ", most_popular, " (", format(max(data$Total, na.rm = TRUE), big.mark = ","), " views)\n",
        "Least popular guide: ", least_popular, " (", format(min(data$Total, na.rm = TRUE), big.mark = ","), " views)\n\n",
        "=== Monthly Trends ===\n",
        "Highest usage month: ", highest_month, " (avg: ", round(max(monthly_averages$Average_Views, na.rm = TRUE), 1), " views)\n",
        "Lowest usage month: ", lowest_month, " (avg: ", round(min(monthly_averages$Average_Views, na.rm = TRUE), 1), " views)"
      )
    })
  })
}