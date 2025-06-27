overviewUI <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    fluidRow(
      column(12,
             h1("URI Library Analytics Hub", class = "text-center"),
             br(),
             p("Welcome to the central dashboard for library analytics.", class = "text-center")
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
               p("• Click the dashboard buttons below for interactive visualizations"),
               p("• Use the table links in the top menu for raw data tables"),
               p("• Switch between dashboards anytime using the buttons below")
             )
      )
    ),
    
    br(),
    
    # Dynamic content area (includes navigation + dashboards)
    uiOutput(ns("dashboard_content"))
  )
}

overviewServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Reactive values to track dashboard state
    dashboard_state <- reactiveValues(
      current = "overview"  # Can be "overview", "libguides", or "devices"
    )
    
    # Button observers
    observeEvent(input$show_libguides, {
      dashboard_state$current <- "libguides"
    })
    
    observeEvent(input$show_devices, {
      dashboard_state$current <- "devices"
    })
    
    # Back to overview button observers
    observeEvent(input$back_to_overview_lib, {
      dashboard_state$current <- "overview"
    })
    
    observeEvent(input$back_to_overview_dev, {
      dashboard_state$current <- "overview"
    })
    
    # ===================== DEVICE DATA PARSING =====================
    
    # Device data parsing with proper CSV reading
    device_parsed_data <- reactive({
      tryCatch({
        # Check if file exists first
        file_path <- 'data/device-usage.csv'
        if (!file.exists(file_path)) {
          stop("Device usage CSV file not found at: ", file_path)
        }
        
        # Read the CSV file with proper settings
        raw_data <- read.csv(file_path, header = FALSE, stringsAsFactors = FALSE, skip = 0)
        
        # Helper function to clean numbers - handle commas and quotes
        clean_number <- function(x) {
          if (is.na(x) || x == "" || is.null(x)) return(0)
          # Remove quotes, commas, and any non-numeric characters except digits
          cleaned <- gsub('[",\\s]', '', as.character(x))
          as.numeric(cleaned)
        }
        
        # Create data based on your exact CSV structure
        # Desktop/Laptop data (rows 4-7)
        desktop_data <- data.frame(
          Category = "Desktop/Laptop",
          Device = c("ChromeOS", "Linux", "MacOS", "Windows"),
          Sessions = c(
            clean_number(raw_data[4, 2]),  # ChromeOS: "1,266"
            clean_number(raw_data[5, 2]),  # Linux: 395
            clean_number(raw_data[6, 2]),  # MacOS: "31,000"
            clean_number(raw_data[7, 2])   # Windows: "33,000"
          ),
          stringsAsFactors = FALSE
        )
        
        # Mobile data (rows 10-12)
        mobile_data <- data.frame(
          Category = "Mobile",
          Device = c("Android", "iOS iPad", "iOS iPhone"),
          Sessions = c(
            clean_number(raw_data[10, 2]), # Android: "1,039"
            clean_number(raw_data[11, 2]), # iOS iPad: 304
            clean_number(raw_data[12, 2])  # iOS iPhone: "2,199"
          ),
          stringsAsFactors = FALSE
        )
        
        # Browser data (rows 15-21)
        browser_data <- data.frame(
          Category = "Browser",
          Device = c("Chrome", "Edge", "Firefox", "Safari", "Brave", "Opera", "Silk"),
          Sessions = c(
            clean_number(raw_data[15, 2]), # Chrome: "46,918"
            clean_number(raw_data[16, 2]), # Edge: "5,164"
            clean_number(raw_data[17, 2]), # Firefox: "5,295"
            clean_number(raw_data[18, 2]), # Safari: "12,403"
            clean_number(raw_data[19, 2]), # Brave: 3
            clean_number(raw_data[20, 2]), # Opera: 304
            clean_number(raw_data[21, 2])  # Silk: 42
          ),
          stringsAsFactors = FALSE
        )
        
        # Remove any rows with 0 sessions (failed parsing)
        desktop_data <- desktop_data[desktop_data$Sessions > 0, ]
        mobile_data <- mobile_data[mobile_data$Sessions > 0, ]
        browser_data <- browser_data[browser_data$Sessions > 0, ]
        
        # Combine all data
        all_data <- rbind(desktop_data, mobile_data, browser_data)
        
        # Validation check
        if (nrow(all_data) == 0) {
          stop("No valid data parsed from CSV")
        }
        
        return(all_data)
        
      }, error = function(e) {
        # Log the specific error
        message("Error parsing device data: ", e$message)
        
        # Return fallback hardcoded data based on your CSV
        desktop_data <- data.frame(
          Category = "Desktop/Laptop",
          Device = c("ChromeOS", "Linux", "MacOS", "Windows"),
          Sessions = c(1266, 395, 31000, 33000),
          stringsAsFactors = FALSE
        )
        
        mobile_data <- data.frame(
          Category = "Mobile",
          Device = c("Android", "iOS iPad", "iOS iPhone"),
          Sessions = c(1039, 304, 2199),
          stringsAsFactors = FALSE
        )
        
        browser_data <- data.frame(
          Category = "Browser",
          Device = c("Chrome", "Edge", "Firefox", "Safari", "Brave", "Opera", "Silk"),
          Sessions = c(46918, 5164, 5295, 12403, 3, 304, 42),
          stringsAsFactors = FALSE
        )
        
        return(rbind(desktop_data, mobile_data, browser_data))
      })
    })
    
    # Dynamic dashboard content with persistent navigation
    output$dashboard_content <- renderUI({
      ns <- session$ns
      
      # Navigation buttons that are always visible
      navigation_buttons <- fluidRow(
        column(4,
               actionButton(ns("show_libguides"), 
                            label = div(
                              icon("book", class = "fa-3x", style = "color: #007bff; margin-bottom: 10px;"),
                              h4("LibGuides Analytics", style = "color: #007bff; margin: 8px 0;"),
                              p("Interactive visualizations and trends", style = "font-size: 13px; margin: 5px 0;"),
                              div(
                                style = paste0("background-color: ", if(dashboard_state$current == "libguides") "#0056b3" else "#007bff", 
                                               "; color: white; padding: 8px 12px; border-radius: 5px; font-size: 13px;"),
                                strong(if(dashboard_state$current == "libguides") "Currently Viewing" else "View Dashboard")
                              )
                            ),
                            style = paste0("width: 100%; height: auto; padding: 20px; border: 3px solid ", 
                                           if(dashboard_state$current == "libguides") "#0056b3" else "#007bff", 
                                           "; border-radius: 12px; background: white; margin-bottom: 15px;"),
                            class = "btn"
               )
        ),
        column(4,
               actionButton(ns("show_devices"), 
                            label = div(
                              icon("laptop", class = "fa-3x", style = "color: #28a745; margin-bottom: 10px;"),
                              h4("Device Usage Analytics", style = "color: #28a745; margin: 8px 0;"),
                              p("Donut charts for device and browser usage", style = "font-size: 13px; margin: 5px 0;"),
                              div(
                                style = paste0("background-color: ", if(dashboard_state$current == "devices") "#1e7e34" else "#28a745", 
                                               "; color: white; padding: 8px 12px; border-radius: 5px; font-size: 13px;"),
                                strong(if(dashboard_state$current == "devices") "Currently Viewing" else "View Dashboard")
                              )
                            ),
                            style = paste0("width: 100%; height: auto; padding: 20px; border: 3px solid ", 
                                           if(dashboard_state$current == "devices") "#1e7e34" else "#28a745", 
                                           "; border-radius: 12px; background: white; margin-bottom: 15px;"),
                            class = "btn"
               )
        ),
        column(4,
               div(
                 style = "text-align: center; padding: 20px; border: 3px solid #6c757d; border-radius: 12px; margin-bottom: 15px; opacity: 0.7;",
                 icon("chart-bar", class = "fa-3x", style = "color: #6c757d; margin-bottom: 10px;"),
                 h4("More Analytics", style = "color: #6c757d; margin: 8px 0;"),
                 p("Additional analytics modules in development", style = "font-size: 13px; margin: 5px 0;"),
                 div(
                   style = "background-color: #6c757d; color: white; padding: 8px 12px; border-radius: 5px; font-size: 13px;",
                   em("Coming Soon")
                 )
               )
        )
      )
      
      if (dashboard_state$current == "libguides") {
        # LibGuides Dashboard
        div(
          navigation_buttons,
          hr(style = "border-top: 2px solid #007bff; margin: 20px 0;"),
          
          div(
            style = "border-top: 3px solid #007bff; padding-top: 20px;",
            
            fluidRow(
              column(12,
                     h2("LibGuides Analytics Dashboard", style = "color: #007bff;"),
                     p("Comprehensive analysis of LibGuides usage patterns and trends", class = "lead")
              )
            ),
            
            # Filters
            fluidRow(
              column(12,
                     wellPanel(
                       style = "background-color: #f8f9fa; border: 1px solid #007bff;",
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
                                textOutput(ns("libguides_status"))
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
                       verbatimTextOutput(ns("libguides_summary_stats"))
                     )
              )
            )
          )
        )
        
      } else if (dashboard_state$current == "devices") {
        # Device Dashboard
        div(
          navigation_buttons,
          hr(style = "border-top: 2px solid #28a745; margin: 20px 0;"),
          
          div(
            style = "border-top: 3px solid #28a745; padding-top: 20px;",
            
            fluidRow(
              column(12,
                     h2("Device Usage Analytics Dashboard", style = "color: #28a745;"),
                     p("Interactive donut charts showing device usage patterns from Primo sessions data", class = "lead")
              )
            ),
            
            # Device controls
            fluidRow(
              column(12,
                     wellPanel(
                       style = "background-color: #f8f9fa; border: 1px solid #28a745;",
                       fluidRow(
                         column(4,
                                p(strong("Data Status:"), style = "margin-top: 5px;"),
                                textOutput(ns("device_status"))
                         ),
                         column(4,
                                selectInput(ns("chart_theme"), "Chart Theme:",
                                            choices = c("Default" = "default", 
                                                        "Colorful" = "colorful", 
                                                        "Professional" = "professional"),
                                            selected = "colorful")
                         ),
                         column(4,
                                checkboxInput(ns("show_percentages"), "Show Percentages", value = TRUE)
                         )
                       )
                     )
              )
            ),
            
            # Device Donut Charts
            fluidRow(
              column(4,
                     box(
                       title = "Laptop/Desktop Usage by OS",
                       status = "success",
                       solidHeader = TRUE,
                       width = NULL,
                       height = "500px",
                       plotOutput(ns("desktop_donut"), height = "420px")
                     )
              ),
              column(4,
                     box(
                       title = "Mobile Client Usage",
                       status = "success", 
                       solidHeader = TRUE,
                       width = NULL,
                       height = "500px",
                       plotOutput(ns("mobile_donut"), height = "420px")
                     )
              ),
              column(4,
                     box(
                       title = "Browser Usage Distribution",
                       status = "success",
                       solidHeader = TRUE,
                       width = NULL,
                       height = "500px",
                       plotOutput(ns("browser_donut"), height = "420px")
                     )
              )
            ),
            
            # Device Summary statistics
            fluidRow(
              column(12,
                     box(
                       title = "Device Usage Summary Statistics",
                       status = "info",
                       solidHeader = TRUE,
                       width = NULL,
                       verbatimTextOutput(ns("device_summary_stats"))
                     )
              )
            ),
            
            # Device data table
            fluidRow(
              column(12,
                     box(
                       title = "Parsed Device Data Preview",
                       status = "success",
                       solidHeader = TRUE,
                       width = NULL,
                       collapsible = TRUE,
                       collapsed = TRUE,
                       DT::dataTableOutput(ns("device_data_table"))
                     )
              )
            )
          )
        )
        
      } else {
        # Overview state - show just navigation buttons with welcome message
        div(
          navigation_buttons,
          br(),
          fluidRow(
            column(12,
                   div(
                     class = "alert alert-success",
                     style = "text-align: center;",
                     h4("Welcome to the Analytics Hub"),
                     p("Select a dashboard above to begin exploring your library's analytics data.")
                   )
            )
          )
        )
      }
    })
    
    # ===================== LIBGUIDES DATA AND ANALYTICS =====================
    
    # LibGuides data loading
    libguides_df <- reactive({
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
    
    # Define months for LibGuides
    months <- c('X2024.06', 'X2024.07', 'X2024.08', 'X2024.09', 'X2024.10', 'X2024.11', 
                'X2024.12', 'X2025.01', 'X2025.02', 'X2025.03', 'X2025.04', 'X2025.05', 'X2025.06')
    
    # LibGuides status
    output$libguides_status <- renderText({
      if (is.null(libguides_df())) {
        "Error loading LibGuides data"
      } else {
        paste("Loaded:", nrow(libguides_df()), "guides")
      }
    })
    
    # Update LibGuides filter choices
    observe({
      req(libguides_df())
      updateSelectInput(session, "usage_filter",
                        choices = c("All", unique(libguides_df()$Usage_Category)))
    })
    
    # LibGuides filtered data
    libguides_filtered_data <- reactive({
      req(libguides_df())
      data <- libguides_df()
      if (!is.null(input$usage_filter) && input$usage_filter != "All") {
        data <- data %>% filter(Usage_Category == input$usage_filter)
      }
      data
    })
    
    # LibGuides plots
    output$top_guides <- renderPlot({
      req(libguides_filtered_data())
      req(input$top_n)
      
      top_guides <- libguides_filtered_data() %>% 
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
    
    output$monthly_trends <- renderPlot({
      req(libguides_filtered_data())
      
      top_5 <- libguides_filtered_data() %>% 
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
        scale_color_manual(values = c("#E74C3C", "#3498DB", "#2ECC71", "#F39C12", "#9B59B6"))
    })
    
    output$usage_distribution <- renderPlot({
      req(libguides_filtered_data())
      
      data <- libguides_filtered_data()
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
    
    output$heatmap <- renderPlot({
      req(libguides_filtered_data())
      
      top_15 <- libguides_filtered_data() %>% 
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
    
    output$libguides_summary_stats <- renderText({
      req(libguides_filtered_data())
      
      data <- libguides_filtered_data()
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
    
    # ===================== DEVICE ANALYTICS =====================
    
    # Device status
    output$device_status <- renderText({
      data <- device_parsed_data()
      if (nrow(data) == 0) {
        "Error loading device data - using fallback data"
      } else {
        paste("Loaded:", nrow(data), "device categories")
      }
    })
    
    # Simple color palettes
    get_colors <- reactive({
      theme <- if (is.null(input$chart_theme)) "colorful" else input$chart_theme
      
      switch(theme,
             "default" = c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf"),
             "colorful" = c("#FF6B6B", "#4ECDC4", "#45B7D1", "#96CEB4", "#FFEAA7", "#DDA0DD", "#98D8C8", "#F7DC6F", "#AED6F1", "#F8C471"),
             "professional" = c("#2C3E50", "#3498DB", "#E74C3C", "#F39C12", "#9B59B6", "#1ABC9C", "#34495E", "#E67E22", "#95A5A6", "#16A085")
      )
    })
    
    # Function to create donut chart
    create_donut <- function(data, title_suffix = "") {
      # Ensure we have data
      if (nrow(data) == 0) return(NULL)
      
      # Calculate percentages
      data$Percentage <- round(data$Sessions / sum(data$Sessions) * 100, 1)
      
      # Create labels - with fallback for input
      show_pct <- if (is.null(input$show_percentages)) TRUE else input$show_percentages
      
      if (show_pct) {
        data$Label <- paste0(data$Device, "\n", format(data$Sessions, big.mark = ","), 
                             " (", data$Percentage, "%)")
      } else {
        data$Label <- paste0(data$Device, "\n", format(data$Sessions, big.mark = ","))
      }
      
      # Get colors - simple vector without names
      colors <- get_colors()
      colors_needed <- colors[1:nrow(data)]
      
      # Create donut chart
      ggplot(data, aes(x = 2, y = Sessions, fill = Device)) +
        geom_col(width = 1, color = "white", size = 0.5) +
        coord_polar(theta = "y", start = 0) +
        xlim(0.5, 2.5) +  # Creates the donut hole
        theme_void() +
        theme(
          legend.position = "bottom",
          legend.text = element_text(size = 9),
          plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
          legend.title = element_blank()
        ) +
        scale_fill_manual(values = setNames(colors_needed, data$Device), labels = data$Label) +
        guides(fill = guide_legend(ncol = 2))
    }
    
    # Device donut charts
    output$desktop_donut <- renderPlot({
      data <- device_parsed_data()
      if (nrow(data) == 0) return(NULL)
      
      desktop_subset <- data %>% filter(Category == "Desktop/Laptop")
      if (nrow(desktop_subset) > 0) {
        create_donut(desktop_subset, "Desktop/Laptop")
      } else {
        ggplot() + theme_void() + 
          annotate("text", x = 0, y = 0, label = "No Desktop/Laptop data available", size = 5)
      }
    })
    
    output$mobile_donut <- renderPlot({
      data <- device_parsed_data()
      if (nrow(data) == 0) return(NULL)
      
      mobile_subset <- data %>% filter(Category == "Mobile")
      if (nrow(mobile_subset) > 0) {
        create_donut(mobile_subset, "Mobile")
      } else {
        ggplot() + theme_void() + 
          annotate("text", x = 0, y = 0, label = "No Mobile data available", size = 5)
      }
    })
    
    output$browser_donut <- renderPlot({
      data <- device_parsed_data()
      if (nrow(data) == 0) return(NULL)
      
      browser_subset <- data %>% filter(Category == "Browser")
      if (nrow(browser_subset) > 0) {
        create_donut(browser_subset, "Browser")
      } else {
        ggplot() + theme_void() + 
          annotate("text", x = 0, y = 0, label = "No Browser data available", size = 5)
      }
    })
    
    # Device summary statistics
    output$device_summary_stats <- renderText({
      data <- device_parsed_data()
      if (nrow(data) == 0) {
        return("No device data available - using fallback data")
      }
      
      desktop_total <- sum(data$Sessions[data$Category == "Desktop/Laptop"], na.rm = TRUE)
      mobile_total <- sum(data$Sessions[data$Category == "Mobile"], na.rm = TRUE)
      browser_total <- sum(data$Sessions[data$Category == "Browser"], na.rm = TRUE)
      
      # Top devices
      top_desktop <- data %>% filter(Category == "Desktop/Laptop") %>% arrange(desc(Sessions)) %>% slice(1)
      top_mobile <- data %>% filter(Category == "Mobile") %>% arrange(desc(Sessions)) %>% slice(1)
      top_browser <- data %>% filter(Category == "Browser") %>% arrange(desc(Sessions)) %>% slice(1)
      
      paste0(
        "=== Primo Sessions Summary ===\n",
        "Total Desktop/Laptop Sessions: ", format(desktop_total, big.mark = ","), "\n",
        "Total Mobile Sessions: ", format(mobile_total, big.mark = ","), "\n",
        "Total Browser Sessions: ", format(browser_total, big.mark = ","), "\n\n",
        "=== Top Performers ===\n",
        "Most Used Desktop OS: ", top_desktop$Device, " (", format(top_desktop$Sessions, big.mark = ","), " sessions)\n",
        "Most Used Mobile Platform: ", top_mobile$Device, " (", format(top_mobile$Sessions, big.mark = ","), " sessions)\n",
        "Most Used Browser: ", top_browser$Device, " (", format(top_browser$Sessions, big.mark = ","), " sessions)\n\n",
        "=== Platform Distribution ===\n",
        "Desktop/Laptop: ", round(desktop_total / (desktop_total + mobile_total) * 100, 1), "%\n",
        "Mobile: ", round(mobile_total / (desktop_total + mobile_total) * 100, 1), "%"
      )
    })
    
    # Device data table
    output$device_data_table <- DT::renderDataTable({
      data <- device_parsed_data()
      if (nrow(data) == 0) {
        return(DT::datatable(data.frame(Message = "No device data available")))
      }
      
      DT::datatable(
        data,
        options = list(pageLength = 15, scrollX = TRUE),
        rownames = FALSE
      )
    })
  })
}