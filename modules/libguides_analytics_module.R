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
                   column(3,
                          numericInput(ns("top_n"), "Number of Top Guides:", 
                                       value = 10, min = 5, max = 20, step = 1)
                   ),
                   column(3,
                          selectInput(ns("usage_filter"), "Usage Category:", 
                                      choices = c("All"),
                                      selected = "All")
                   ),
                   column(3,
                          p(strong("Dashboard Status:"), style = "margin-top: 25px;"),
                          textOutput(ns("status"))
                   ),
                   column(3,
                          downloadButton(ns("download_raw_csv"), "Download Raw CSV", 
                                       class = "btn-success", style = "margin-top: 25px;")
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
                 div(
                   style = "cursor: pointer;",
                   onclick = paste0("Shiny.setInputValue('", ns("expand_top_guides"), "', Math.random())"),
                   plotOutput(ns("top_guides"), height = "380px")
                 )
               )
        ),
        column(6,
               box(
                 title = "Monthly Usage Trends (Top 5 Guides)",
                 status = "primary", 
                 solidHeader = TRUE,
                 width = NULL,
                 height = "450px",
                 div(
                   style = "cursor: pointer;",
                   onclick = paste0("Shiny.setInputValue('", ns("expand_monthly_trends"), "', Math.random())"),
                   plotOutput(ns("monthly_trends"), height = "380px")
                 )
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
                 div(
                   style = "cursor: pointer;",
                   onclick = paste0("Shiny.setInputValue('", ns("expand_usage_distribution"), "', Math.random())"),
                   plotOutput(ns("usage_distribution"), height = "380px")
                 )
               )
        ),
        column(6,
               box(
                 title = "Monthly Usage Heatmap (Top 15 Guides)",
                 status = "primary",
                 solidHeader = TRUE,
                 width = NULL,
                 height = "450px",
                 div(
                   style = "cursor: pointer;",
                   onclick = paste0("Shiny.setInputValue('", ns("expand_heatmap"), "', Math.random())"),
                   plotOutput(ns("heatmap"), height = "380px")
                 )
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
    )
  )
}

libguidesAnalyticsServer <- function(id, parent_session = NULL, current_view = NULL) {
  moduleServer(id, function(input, output, session) {
    
    # Back button navigation
    observeEvent(input$back_to_overview, {
      if (!is.null(current_view)) {
        current_view("overview")
        updateTabsetPanel(session = parent_session, inputId = "navbar", selected = "overview")
      } else if (!is.null(parent_session)) {
        updateTabsetPanel(session = parent_session, inputId = "navbar", selected = "overview")
      }
    })
    
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
    
    # Top Guides Chart
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
    
    # Monthly Trends Chart
    output$monthly_trends <- renderPlot({
      req(filtered_data())
      
      # Get top 5 guides for monthly trends
      top_5_guides <- filtered_data() %>% 
        arrange(desc(Total)) %>% 
        head(5)
      
      # Reshape data for plotting
      top_5_long <- top_5_guides %>%
        select(Guide.Name, X2024.06, X2024.07, X2024.08, X2024.09, X2024.10, X2024.11, X2024.12, X2025.01) %>%
        pivot_longer(cols = c(X2024.06, X2024.07, X2024.08, X2024.09, X2024.10, X2024.11, X2024.12, X2025.01), 
                     names_to = "Month", values_to = "Views") %>%
        mutate(Month = case_when(
          Month == "X2024.06" ~ "Jun",
          Month == "X2024.07" ~ "Jul", 
          Month == "X2024.08" ~ "Aug",
          Month == "X2024.09" ~ "Sep",
          Month == "X2024.10" ~ "Oct",
          Month == "X2024.11" ~ "Nov",
          Month == "X2024.12" ~ "Dec",
          Month == "X2025.01" ~ "Jan",
          TRUE ~ Month
        ))
      
      ggplot(top_5_long, aes(x = Month, y = Views, color = Guide.Name, group = Guide.Name)) +
        geom_line(size = 1.2) +
        geom_point(size = 2) +
        labs(x = "Month", y = "Views", color = "Guide") +
        theme_minimal() +
        theme(legend.position = "bottom",
              legend.text = element_text(size = 8)) +
        scale_color_brewer(type = "qual", palette = "Set1")
    })
    
    # Usage Distribution Chart
    output$usage_distribution <- renderPlot({
      req(filtered_data())
      
      usage_counts <- filtered_data() %>%
        count(Usage_Category) %>%
        mutate(Usage_Category = factor(Usage_Category, 
                                     levels = c("Very Low (<500)", "Low (500-999)", 
                                               "Medium (1000-4999)", "High (5000+)")))
      
      ggplot(usage_counts, aes(x = Usage_Category, y = n, fill = Usage_Category)) +
        geom_col(alpha = 0.8) +
        labs(x = "Usage Category", y = "Number of Guides", fill = "Category") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              legend.position = "none") +
        scale_fill_manual(values = c("Very Low (<500)" = "#fee5d9",
                                   "Low (500-999)" = "#fcae91", 
                                   "Medium (1000-4999)" = "#fb6a4a",
                                   "High (5000+)" = "#cb181d"))
    })
    
    # Monthly Heatmap
    output$heatmap <- renderPlot({
      req(filtered_data())
      
      # Get top 15 guides for heatmap
      top_15_guides <- filtered_data() %>% 
        arrange(desc(Total)) %>% 
        head(15)
      
      # Reshape data for heatmap
      top_15_long <- top_15_guides %>%
        select(Guide.Name, X2024.06, X2024.07, X2024.08, X2024.09, X2024.10, X2024.11, X2024.12, X2025.01) %>%
        mutate(Guide.Name = ifelse(nchar(Guide.Name) > 25, 
                                 paste0(substr(Guide.Name, 1, 25), "..."), 
                                 Guide.Name)) %>%
        pivot_longer(cols = c(X2024.06, X2024.07, X2024.08, X2024.09, X2024.10, X2024.11, X2024.12, X2025.01), 
                     names_to = "Month", values_to = "Views") %>%
        mutate(Month = case_when(
          Month == "X2024.06" ~ "Jun",
          Month == "X2024.07" ~ "Jul", 
          Month == "X2024.08" ~ "Aug",
          Month == "X2024.09" ~ "Sep",
          Month == "X2024.10" ~ "Oct",
          Month == "X2024.11" ~ "Nov",
          Month == "X2024.12" ~ "Dec",
          Month == "X2025.01" ~ "Jan",
          TRUE ~ Month
        ))
      
      ggplot(top_15_long, aes(x = Month, y = reorder(Guide.Name, Views), fill = Views)) +
        geom_tile(color = "white", size = 0.1) +
        scale_fill_gradient(low = "lightblue", high = "darkblue", name = "Views") +
        labs(x = "Month", y = "Guide Name") +
        theme_minimal() +
        theme(axis.text.y = element_text(size = 8),
              axis.text.x = element_text(angle = 45, hjust = 1))
    })
    
    # Summary Statistics
    output$summary_stats <- renderText({
      req(filtered_data())
      
      data <- filtered_data()
      total_guides <- nrow(data)
      total_views <- sum(data$Total, na.rm = TRUE)
      avg_views <- round(mean(data$Total, na.rm = TRUE), 1)
      median_views <- median(data$Total, na.rm = TRUE)
      top_guide <- data %>% arrange(desc(Total)) %>% slice(1)
      
      paste(
        "=== LibGuides Summary Statistics ===\n",
        "Total Guides: ", format(total_guides, big.mark = ","), "\n",
        "Total Views: ", format(total_views, big.mark = ","), "\n",
        "Average Views per Guide: ", format(avg_views, big.mark = ","), "\n",
        "Median Views per Guide: ", format(median_views, big.mark = ","), "\n",
        "Most Popular Guide: ", top_guide$Guide.Name, " (", format(top_guide$Total, big.mark = ","), " views)\n",
        "\n=== Usage Category Breakdown ===\n",
        "High Usage (5000+): ", sum(data$Usage_Category == "High (5000+)", na.rm = TRUE), " guides\n",
        "Medium Usage (1000-4999): ", sum(data$Usage_Category == "Medium (1000-4999)", na.rm = TRUE), " guides\n",
        "Low Usage (500-999): ", sum(data$Usage_Category == "Low (500-999)", na.rm = TRUE), " guides\n",
        "Very Low Usage (<500): ", sum(data$Usage_Category == "Very Low (<500)", na.rm = TRUE), " guides"
      )
    })
    
    # Download Raw CSV Handler
    output$download_raw_csv <- downloadHandler(
      filename = function() {
        paste("libguides_raw_data_", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        write.csv(df(), file, row.names = FALSE)
      }
    )
    
    # Modal observers for expandable charts
    observeEvent(input$expand_top_guides, {
      showModal(modalDialog(
        title = "Top LibGuides by Total Usage - Expanded View",
        plotOutput(session$ns("top_guides_modal"), height = "600px"),
        size = "l",
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
    })
    
    observeEvent(input$expand_monthly_trends, {
      showModal(modalDialog(
        title = "Monthly Usage Trends (Top 5 Guides) - Expanded View",
        plotOutput(session$ns("monthly_trends_modal"), height = "600px"),
        size = "l",
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
    })
    
    observeEvent(input$expand_usage_distribution, {
      showModal(modalDialog(
        title = "Distribution of Guide Usage - Expanded View",
        plotOutput(session$ns("usage_distribution_modal"), height = "600px"),
        size = "l",
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
    })
    
    observeEvent(input$expand_heatmap, {
      showModal(modalDialog(
        title = "Monthly Usage Heatmap (Top 15 Guides) - Expanded View",
        plotOutput(session$ns("heatmap_modal"), height = "600px"),
        size = "l",
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
    })
    
    # Modal plot outputs (same as regular plots but larger)
    output$top_guides_modal <- renderPlot({
      req(filtered_data())
      
      top_guides <- filtered_data() %>% 
        arrange(desc(Total)) %>% 
        head(input$top_n) %>%
        mutate(Guide.Name = ifelse(nchar(Guide.Name) > 40, 
                                   paste0(substr(Guide.Name, 1, 40), "..."), 
                                   Guide.Name))
      
      ggplot(top_guides, aes(x = reorder(Guide.Name, Total), y = Total)) +
        geom_col(fill = "steelblue", alpha = 0.8) +
        coord_flip() +
        labs(x = "Guide Name", y = "Total Views") +
        theme_minimal() +
        theme(axis.text.y = element_text(size = 12),
              axis.text.x = element_text(size = 12),
              axis.title = element_text(size = 14))
    })
    
    output$monthly_trends_modal <- renderPlot({
      req(filtered_data())
      
      # Get top 5 guides for monthly trends
      top_5_guides <- filtered_data() %>% 
        arrange(desc(Total)) %>% 
        head(5)
      
      # Reshape data for plotting
      top_5_long <- top_5_guides %>%
        select(Guide.Name, X2024.06, X2024.07, X2024.08, X2024.09, X2024.10, X2024.11, X2024.12, X2025.01) %>%
        pivot_longer(cols = c(X2024.06, X2024.07, X2024.08, X2024.09, X2024.10, X2024.11, X2024.12, X2025.01), 
                     names_to = "Month", values_to = "Views") %>%
        mutate(Month = case_when(
          Month == "X2024.06" ~ "Jun",
          Month == "X2024.07" ~ "Jul", 
          Month == "X2024.08" ~ "Aug",
          Month == "X2024.09" ~ "Sep",
          Month == "X2024.10" ~ "Oct",
          Month == "X2024.11" ~ "Nov",
          Month == "X2024.12" ~ "Dec",
          Month == "X2025.01" ~ "Jan",
          TRUE ~ Month
        ))
      
      ggplot(top_5_long, aes(x = Month, y = Views, color = Guide.Name, group = Guide.Name)) +
        geom_line(size = 1.5) +
        geom_point(size = 3) +
        labs(x = "Month", y = "Views", color = "Guide") +
        theme_minimal() +
        theme(legend.position = "bottom",
              legend.text = element_text(size = 10),
              axis.text = element_text(size = 12),
              axis.title = element_text(size = 14)) +
        scale_color_brewer(type = "qual", palette = "Set1")
    })
    
    output$usage_distribution_modal <- renderPlot({
      req(filtered_data())
      
      usage_counts <- filtered_data() %>%
        count(Usage_Category) %>%
        mutate(Usage_Category = factor(Usage_Category, 
                                     levels = c("Very Low (<500)", "Low (500-999)", 
                                               "Medium (1000-4999)", "High (5000+)")))
      
      ggplot(usage_counts, aes(x = Usage_Category, y = n, fill = Usage_Category)) +
        geom_col(alpha = 0.8) +
        labs(x = "Usage Category", y = "Number of Guides", fill = "Category") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
              axis.text.y = element_text(size = 12),
              axis.title = element_text(size = 14),
              legend.position = "none") +
        scale_fill_manual(values = c("Very Low (<500)" = "#fee5d9",
                                   "Low (500-999)" = "#fcae91", 
                                   "Medium (1000-4999)" = "#fb6a4a",
                                   "High (5000+)" = "#cb181d"))
    })
    
    output$heatmap_modal <- renderPlot({
      req(filtered_data())
      
      # Get top 15 guides for heatmap
      top_15_guides <- filtered_data() %>% 
        arrange(desc(Total)) %>% 
        head(15)
      
      # Reshape data for heatmap
      top_15_long <- top_15_guides %>%
        select(Guide.Name, X2024.06, X2024.07, X2024.08, X2024.09, X2024.10, X2024.11, X2024.12, X2025.01) %>%
        mutate(Guide.Name = ifelse(nchar(Guide.Name) > 30, 
                                 paste0(substr(Guide.Name, 1, 30), "..."), 
                                 Guide.Name)) %>%
        pivot_longer(cols = c(X2024.06, X2024.07, X2024.08, X2024.09, X2024.10, X2024.11, X2024.12, X2025.01), 
                     names_to = "Month", values_to = "Views") %>%
        mutate(Month = case_when(
          Month == "X2024.06" ~ "Jun",
          Month == "X2024.07" ~ "Jul", 
          Month == "X2024.08" ~ "Aug",
          Month == "X2024.09" ~ "Sep",
          Month == "X2024.10" ~ "Oct",
          Month == "X2024.11" ~ "Nov",
          Month == "X2024.12" ~ "Dec",
          Month == "X2025.01" ~ "Jan",
          TRUE ~ Month
        ))
      
      ggplot(top_15_long, aes(x = Month, y = reorder(Guide.Name, Views), fill = Views)) +
        geom_tile(color = "white", size = 0.1) +
        scale_fill_gradient(low = "lightblue", high = "darkblue", name = "Views") +
        labs(x = "Month", y = "Guide Name") +
        theme_minimal() +
        theme(axis.text.y = element_text(size = 10),
              axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
              axis.title = element_text(size = 14))
    })
  })
}