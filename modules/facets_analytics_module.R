facetsAnalyticsUI <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    div(
      style = "border-top: 3px solid #007bff; padding-top: 20px; margin-top: 20px;",
      titlePanel("Facets by Frequency Dashboard"),
      
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
                          numericInput(ns("top_n"), "Number of Top Facets:", 
                                       value = 15, min = 5, max = 30, step = 1)
                   ),
                   column(3,
                          numericInput(ns("min_frequency"), "Minimum Frequency:", 
                                       value = 50, min = 1, max = 1000, step = 10)
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
                 title = "Top Facets by Frequency",
                 status = "primary",
                 solidHeader = TRUE,
                 width = NULL,
                 height = "450px",
                 div(
                   style = "cursor: pointer;",
                   onclick = paste0("Shiny.setInputValue('", ns("expand_top_facets"), "', Math.random())"),
                   plotOutput(ns("top_facets"), height = "380px")
                 )
               )
        ),
        column(6,
               box(
                 title = "Facet Frequency Distribution",
                 status = "primary", 
                 solidHeader = TRUE,
                 width = NULL,
                 height = "450px",
                 div(
                   style = "cursor: pointer;",
                   onclick = paste0("Shiny.setInputValue('", ns("expand_frequency_dist"), "', Math.random())"),
                   plotOutput(ns("frequency_distribution"), height = "380px")
                 )
               )
        )
      ),
      
      fluidRow(
        column(6,
               box(
                 title = "Facet Categories Breakdown",
                 status = "primary",
                 solidHeader = TRUE,
                 width = NULL,
                 height = "450px",
                 div(
                   style = "cursor: pointer;",
                   onclick = paste0("Shiny.setInputValue('", ns("expand_categories"), "', Math.random())"),
                   plotOutput(ns("categories_breakdown"), height = "380px")
                 )
               )
        ),
        column(6,
               box(
                 title = "Cumulative Frequency Analysis",
                 status = "primary",
                 solidHeader = TRUE,
                 width = NULL,
                 height = "450px",
                 div(
                   style = "cursor: pointer;",
                   onclick = paste0("Shiny.setInputValue('", ns("expand_cumulative"), "', Math.random())"),
                   plotOutput(ns("cumulative_analysis"), height = "380px")
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

facetsAnalyticsServer <- function(id, parent_session = NULL, current_view = NULL) {
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
        # Try to read from the data directory first
        data_path <- 'data/facets-selected.csv'
        if (file.exists(data_path)) {
          data <- read.csv(data_path)
        } else {
          # If not found, create sample data based on the provided CSV
          data <- data.frame(
            Facets.Selected = c(7287, 3713, 2642, 2096, 1826, 1622, 331, 302, 284, 200, 198, 198, 177, 110, 63, 59, 54, 47, 44, 41, 36, 28, 26, 20, 17, 15, 15, 11, 9, 7, 7, 2, 1),
            Facet.Value = c("None", "Books", "Peer Reviewed Journals", "Articles", "Available in the Library", "Full Text Online", "Reviews", "Journals", "book_chapters", "Newspaper Articles", "Audio-Visual", "magazinearticle", "open_access", "newsletterarticle", "videos", "reports", "Reference Entries", "archival_material_manuscripts", "Dissertations", "web_resources", "Government Documents", "Images", "Text Resources", "Conference Proceedings", "Databases", "audios", "datasets", "Other", "Maps", "Scores", "newspapers", "standards", "Books."),
            Year = rep(2025, 33)
          )
        }
        
        # Add frequency categories
        data$Frequency_Category <- case_when(
          data$Facets.Selected >= 5000 ~ 'Very High (5000+)',
          data$Facets.Selected >= 1000 ~ 'High (1000-4999)',
          data$Facets.Selected >= 100 ~ 'Medium (100-999)',
          data$Facets.Selected >= 10 ~ 'Low (10-99)',
          TRUE ~ 'Very Low (<10)'
        )
        
        # Create resource type categories
        data$Resource_Type <- case_when(
          grepl("Book|book", data$Facet.Value, ignore.case = TRUE) ~ "Books & Chapters",
          grepl("Journal|Article|magazinearticle|newsletterarticle", data$Facet.Value, ignore.case = TRUE) ~ "Journals & Articles",
          grepl("Audio|Video|Image|audios|videos", data$Facet.Value, ignore.case = TRUE) ~ "Multimedia",
          grepl("Government|Dissertation|Conference|Report|standards", data$Facet.Value, ignore.case = TRUE) ~ "Special Collections",
          grepl("Database|web_resources|open_access|Full Text", data$Facet.Value, ignore.case = TRUE) ~ "Digital Resources",
          grepl("None|Other|Available", data$Facet.Value, ignore.case = TRUE) ~ "General/Other",
          TRUE ~ "Reference Materials"
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
        output$status <- renderText({paste("Data loaded:", nrow(df()), "facets")})
      }
    })
    
    # Filtered data
    filtered_data <- reactive({
      req(df())
      data <- df()
      data %>% filter(Facets.Selected >= input$min_frequency)
    })
    
    # Top Facets Chart
    output$top_facets <- renderPlot({
      req(filtered_data())
      
      top_facets <- filtered_data() %>% 
        arrange(desc(Facets.Selected)) %>% 
        head(input$top_n) %>%
        mutate(Facet.Value = ifelse(nchar(Facet.Value) > 25, 
                                   paste0(substr(Facet.Value, 1, 25), "..."), 
                                   Facet.Value)) %>%
        mutate(Facet.Value = factor(Facet.Value, levels = rev(Facet.Value)))
      
      ggplot(top_facets, aes(x = Facet.Value, y = Facets.Selected)) +
        geom_col(fill = "#007bff", alpha = 0.8) +
        geom_text(aes(label = comma(Facets.Selected)), 
                  hjust = -0.1, size = 3.5, color = "black") +
        coord_flip() +
        labs(title = paste("Top", input$top_n, "Facets by Selection Frequency"),
             x = "Facet Value",
             y = "Number of Selections") +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 14, face = "bold"),
          axis.text = element_text(size = 10),
          axis.title = element_text(size = 12, face = "bold"),
          panel.grid.minor = element_blank()
        ) +
        scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.15)))
    })
    
    # Frequency Distribution Chart
    output$frequency_distribution <- renderPlot({
      req(filtered_data())
      
      ggplot(filtered_data(), aes(x = Facets.Selected)) +
        geom_histogram(bins = 20, fill = "#28a745", alpha = 0.7, color = "white") +
        geom_vline(aes(xintercept = mean(Facets.Selected)), 
                   color = "red", linetype = "dashed", size = 1) +
        geom_vline(aes(xintercept = median(Facets.Selected)), 
                   color = "blue", linetype = "dashed", size = 1) +
        labs(title = "Distribution of Facet Selection Frequencies",
             x = "Number of Selections",
             y = "Count of Facets",
             caption = "Red line: Mean | Blue line: Median") +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 14, face = "bold"),
          axis.text = element_text(size = 10),
          axis.title = element_text(size = 12, face = "bold"),
          plot.caption = element_text(size = 9, color = "gray50")
        ) +
        scale_x_continuous(labels = comma) +
        scale_y_continuous(labels = comma)
    })
    
    # Categories Breakdown Chart
    output$categories_breakdown <- renderPlot({
      req(filtered_data())
      
      category_summary <- filtered_data() %>%
        group_by(Resource_Type) %>%
        summarise(
          Total_Selections = sum(Facets.Selected),
          Count = n(),
          .groups = 'drop'
        ) %>%
        arrange(desc(Total_Selections))
      
      ggplot(category_summary, aes(x = reorder(Resource_Type, Total_Selections), 
                                   y = Total_Selections)) +
        geom_col(aes(fill = Resource_Type), alpha = 0.8) +
        geom_text(aes(label = paste0(comma(Total_Selections), "\n(", Count, " facets)")), 
                  hjust = -0.1, size = 3, color = "black") +
        coord_flip() +
        labs(title = "Facet Selections by Resource Type Category",
             x = "Resource Type",
             y = "Total Selections") +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 14, face = "bold"),
          axis.text = element_text(size = 10),
          axis.title = element_text(size = 12, face = "bold"),
          legend.position = "none",
          panel.grid.minor = element_blank()
        ) +
        scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.15))) +
        scale_fill_brewer(type = "qual", palette = "Set2")
    })
    
    # Cumulative Analysis Chart
    output$cumulative_analysis <- renderPlot({
      req(filtered_data())
      
      cumulative_data <- filtered_data() %>%
        arrange(desc(Facets.Selected)) %>%
        mutate(
          Rank = row_number(),
          Cumulative_Selections = cumsum(Facets.Selected),
          Cumulative_Percent = (Cumulative_Selections / sum(Facets.Selected)) * 100
        )
      
      ggplot(cumulative_data, aes(x = Rank)) +
        geom_line(aes(y = Cumulative_Percent), color = "#dc3545", size = 1.2) +
        geom_point(aes(y = Cumulative_Percent), color = "#dc3545", size = 2) +
        geom_hline(yintercept = 80, linetype = "dashed", color = "gray50", alpha = 0.7) +
        geom_hline(yintercept = 50, linetype = "dashed", color = "gray50", alpha = 0.7) +
        labs(title = "Cumulative Distribution of Facet Selections",
             x = "Facet Rank (by frequency)",
             y = "Cumulative Percentage of Total Selections") +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 14, face = "bold"),
          axis.text = element_text(size = 10),
          axis.title = element_text(size = 12, face = "bold"),
          panel.grid.minor = element_blank()
        ) +
        scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(0, 100)) +
        annotate("text", x = max(cumulative_data$Rank) * 0.7, y = 85, 
                 label = "80% threshold", color = "gray50", size = 3) +
        annotate("text", x = max(cumulative_data$Rank) * 0.7, y = 55, 
                 label = "50% threshold", color = "gray50", size = 3)
    })
    
    # Summary Statistics
    output$summary_stats <- renderText({
      req(filtered_data())
      
      data <- filtered_data()
      total_selections <- sum(data$Facets.Selected)
      total_facets <- nrow(data)
      avg_selections <- mean(data$Facets.Selected)
      median_selections <- median(data$Facets.Selected)
      
      # Top 10 percentage
      top_10_selections <- sum(head(arrange(data, desc(Facets.Selected)), 10)$Facets.Selected)
      top_10_percent <- (top_10_selections / total_selections) * 100
      
      # Most common resource type
      most_common_type <- data %>%
        count(Resource_Type, sort = TRUE) %>%
        slice(1) %>%
        pull(Resource_Type)
      
      paste(
        "FACETS ANALYTICS SUMMARY",
        "========================",
        paste("Total Facets:", comma(total_facets)),
        paste("Total Selections:", comma(total_selections)),
        paste("Average Selections per Facet:", comma(round(avg_selections, 1))),
        paste("Median Selections per Facet:", comma(median_selections)),
        "",
        "DISTRIBUTION INSIGHTS",
        "====================",
        paste("Top 10 facets account for", round(top_10_percent, 1), "% of all selections"),
        paste("Most common resource type:", most_common_type),
        paste("Facets with 1000+ selections:", sum(data$Facets.Selected >= 1000)),
        paste("Facets with <100 selections:", sum(data$Facets.Selected < 100)),
        "",
        "FREQUENCY BREAKDOWN",
        "==================",
        paste(data %>% count(Frequency_Category, sort = TRUE) %>% 
              mutate(text = paste(Frequency_Category, ":", n, "facets")) %>% 
              pull(text), collapse = "\n"),
        sep = "\n"
      )
    })
    
    # Download handler
    output$download_raw_csv <- downloadHandler(
      filename = function() {
        paste("facets-frequency-data-", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        write.csv(df(), file, row.names = FALSE)
      }
    )
    
    # Full screen modal handlers (you can add these if needed)
    observeEvent(input$expand_top_facets, {
      showModal(modalDialog(
        title = "Top Facets by Frequency - Full Screen",
        plotOutput(ns("top_facets_full"), height = "600px"),
        size = "l",
        easyClose = TRUE
      ))
    })
    
    output$top_facets_full <- renderPlot({
      req(filtered_data())
      
      top_facets <- filtered_data() %>% 
        arrange(desc(Facets.Selected)) %>% 
        head(input$top_n) %>%
        mutate(Facet.Value = ifelse(nchar(Facet.Value) > 40, 
                                   paste0(substr(Facet.Value, 1, 40), "..."), 
                                   Facet.Value)) %>%
        mutate(Facet.Value = factor(Facet.Value, levels = rev(Facet.Value)))
      
      ggplot(top_facets, aes(x = Facet.Value, y = Facets.Selected)) +
        geom_col(fill = "#007bff", alpha = 0.8) +
        geom_text(aes(label = comma(Facets.Selected)), 
                  hjust = -0.1, size = 4, color = "black") +
        coord_flip() +
        labs(title = paste("Top", input$top_n, "Facets by Selection Frequency"),
             x = "Facet Value",
             y = "Number of Selections") +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 18, face = "bold"),
          axis.text = element_text(size = 12),
          axis.title = element_text(size = 14, face = "bold"),
          panel.grid.minor = element_blank()
        ) +
        scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.15)))
    })
  })
}
