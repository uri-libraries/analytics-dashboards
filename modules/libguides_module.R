libguidesUI <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    titlePanel("LibGuides Analytics Dashboard"),
    
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
  )
}

libguidesServer <- function(id) {
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
    
    # 1. Top Guides Plot
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
    
    # 2. Monthly Trends
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
    
    # 3. Usage Distribution
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
    
    # 5. Summary Statistics
    output$summary_stats <- renderText({
      req(filtered_data())
      
      data <- filtered_data()
      most_popular <- data$Guide.Name[which.max(data$Total)]
      least_popular <- data$Guide.Name[which.min(data$Total)]
      
      paste0(
        "=== LibGuides Usage Summary ===\n",
        "Total number of guides: ", nrow(data), "\n",
        "Total views across all guides: ", format(sum(data$Total, na.rm = TRUE), big.mark = ","), "\n",
        "Average views per guide: ", round(mean(data$Total, na.rm = TRUE), 1), "\n",
        "Median views per guide: ", round(median(data$Total, na.rm = TRUE), 1), "\n",
        "Most popular guide: ", most_popular, " (", format(max(data$Total, na.rm = TRUE), big.mark = ","), " views)\n",
        "Least popular guide: ", least_popular, " (", format(min(data$Total, na.rm = TRUE), big.mark = ","), " views)"
      )
    })
  })
}