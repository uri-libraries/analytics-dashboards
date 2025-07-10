# Zero Search Analytics Module

# Load required libraries
library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(dplyr)
library(ggplot2)

# UI Function
zero_search_analytics_ui <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    # Add custom CSS for dark mode compatibility
    tags$head(
      tags$style(HTML("
        .dark-mode h2 {
          color: #ffffff !important;
        }
        .dark-mode .box-title {
          color: #ffffff !important;
        }
        .dark-mode p {
          color: #ffffff !important;
        }
      "))
    ),
    
    # Title
    h2("Zero Search Analytics", style = "color: #2c3e50; text-align: center; margin-bottom: 30px;"),
    
    # Summary boxes
    fluidRow(
      box(
        title = "Key Metrics", 
        status = "primary", 
        solidHeader = TRUE,
        width = 12,
        height = 200,
        fluidRow(
          column(3, 
                 h3(textOutput(ns("total_searches")), style = "color: #3498db; text-align: center;"),
                 p("Total Failed Searches", style = "text-align: center; font-weight: bold;")
          ),
          column(3,
                 h3(textOutput(ns("unique_queries")), style = "color: #e74c3c; text-align: center;"),
                 p("Unique Search Queries", style = "text-align: center; font-weight: bold;")
          ),
          column(3,
                 h3(textOutput(ns("top_user_group")), style = "color: #2ecc71; text-align: center;"),
                 p("Most Active User Group", style = "text-align: center; font-weight: bold;")
          ),
          column(3,
                 h3(textOutput(ns("avg_searches_per_query")), style = "color: #f39c12; text-align: center;"),
                 p("Avg. Searches per Query", style = "text-align: center; font-weight: bold;")
          )
        )
      )
    ),
    
    # Charts
    fluidRow(
      # User Group Distribution
      box(
        title = "Failed Searches by User Group",
        status = "info",
        solidHeader = TRUE,
        width = 6,
        height = 400,
        plotlyOutput(ns("user_group_chart"))
      ),
      
      # Search Scope Distribution
      box(
        title = "Failed Searches by Search Scope",
        status = "info", 
        solidHeader = TRUE,
        width = 6,
        height = 400,
        plotlyOutput(ns("search_scope_chart"))
      )
    ),
    
    # Add spacing between chart rows
    br(),
    br(),
    
    fluidRow(
      # Top Failed Searches
      box(
        title = "Top Failed Search Queries",
        status = "warning",
        solidHeader = TRUE,
        width = 6,
        height = 400,
        plotlyOutput(ns("top_searches_chart"))
      ),

      br(),
      
      # Sticky Facets Usage
      box(
        title = "Sticky Facets Usage",
        status = "success",
        solidHeader = TRUE,
        width = 6,
        height = 400,
        plotlyOutput(ns("sticky_facets_chart"))
      )
    ),
    
    # Add spacing between chart rows
    br(),
    br(),
    
    # Data Table
    fluidRow(
      box(
        title = "Zero Search Data Table",
        status = "primary",
        solidHeader = TRUE,
        width = 12,
        height = 600,
        DT::dataTableOutput(ns("zero_search_table"))
      )
    )
  )
}


# Server Function
zero_search_analytics_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Load and clean data
    zero_search_data <- reactive({
      # Read the CSV file
      df <- read.csv("data/zero-search2425.csv", stringsAsFactors = FALSE)
      
      # Skip the first few header rows and get the actual data
      # Find the row where actual data starts (after "Searches,Search String,User Group...")
      start_row <- which(df[,1] == "Searches")[1]
      if (is.na(start_row)) {
        start_row <- 4  # fallback if header not found
      }
      
      # Extract the actual data
      df_clean <- df[(start_row + 1):nrow(df), ]
      
      # Set proper column names
      colnames(df_clean) <- c("Searches", "Search_String", "User_Group", "Field_Searched", 
                             "Sticky_Facets_Used", "Search_Scope_Type", "Search_Scope")
      
      # Convert searches to numeric and filter out empty rows
      df_clean$Searches <- as.numeric(df_clean$Searches)
      df_clean <- df_clean[!is.na(df_clean$Searches) & df_clean$Searches > 0, ]
      
      # Clean up the data
      df_clean$Search_String <- trimws(df_clean$Search_String)
      df_clean$User_Group <- trimws(df_clean$User_Group)
      df_clean$Sticky_Facets_Used <- as.numeric(df_clean$Sticky_Facets_Used)
      
      return(df_clean)
    })
    
    # Key Metrics
    output$total_searches <- renderText({
      format(sum(zero_search_data()$Searches, na.rm = TRUE), big.mark = ",")
    })
    
    output$unique_queries <- renderText({
      format(length(unique(zero_search_data()$Search_String)), big.mark = ",")
    })
    
    output$top_user_group <- renderText({
      user_group_summary <- zero_search_data() %>%
        group_by(User_Group) %>%
        summarise(Total_Searches = sum(Searches, na.rm = TRUE), .groups = 'drop') %>%
        arrange(desc(Total_Searches))
      
      if (nrow(user_group_summary) > 0) {
        user_group_summary$User_Group[1]
      } else {
        "N/A"
      }
    })
    
    output$avg_searches_per_query <- renderText({
      avg_searches <- mean(zero_search_data()$Searches, na.rm = TRUE)
      format(round(avg_searches, 1), big.mark = ",")
    })
    
    # User Group Chart
    output$user_group_chart <- renderPlotly({
      user_group_data <- zero_search_data() %>%
        group_by(User_Group) %>%
        summarise(Total_Searches = sum(Searches, na.rm = TRUE), .groups = 'drop') %>%
        arrange(desc(Total_Searches))
      
      p <- ggplot(user_group_data, aes(x = reorder(User_Group, Total_Searches), y = Total_Searches)) +
        geom_bar(stat = "identity", fill = "#3498db") +
        coord_flip() +
        labs(title = "Failed Searches by User Group",
             x = "User Group",
             y = "Total Failed Searches") +
        theme_minimal() +
        theme(
          plot.title = element_text(hjust = 0.5, color = "white"),
          axis.title = element_text(color = "white"),
          axis.text = element_text(color = "white"),
          panel.background = element_rect(fill = "transparent", color = NA),
          plot.background = element_rect(fill = "transparent", color = NA),
          panel.grid.major = element_line(color = "grey40"),
          panel.grid.minor = element_line(color = "grey40")
        )
      
      ggplotly(p, tooltip = "none") %>%
        config(displayModeBar = FALSE) %>%
        layout(
          paper_bgcolor = 'rgba(0,0,0,0)',
          plot_bgcolor = 'rgba(0,0,0,0)',
          font = list(color = "white")
        )
    })
    
    # Search Scope Chart
    output$search_scope_chart <- renderPlotly({
      scope_data <- zero_search_data() %>%
        group_by(Search_Scope) %>%
        summarise(Total_Searches = sum(Searches, na.rm = TRUE), .groups = 'drop') %>%
        arrange(desc(Total_Searches))
      
      p <- ggplot(scope_data, aes(x = reorder(Search_Scope, Total_Searches), y = Total_Searches)) +
        geom_bar(stat = "identity", fill = "#e74c3c") +
        coord_flip() +
        labs(title = "Failed Searches by Search Scope",
             x = "Search Scope",
             y = "Total Failed Searches") +
        theme_minimal() +
        theme(
          plot.title = element_text(hjust = 0.5, color = "white"),
          axis.title = element_text(color = "white"),
          axis.text = element_text(color = "white"),
          panel.background = element_rect(fill = "transparent", color = NA),
          plot.background = element_rect(fill = "transparent", color = NA),
          panel.grid.major = element_line(color = "grey40"),
          panel.grid.minor = element_line(color = "grey40")
        )
      
      ggplotly(p, tooltip = "none") %>%
        config(displayModeBar = FALSE) %>%
        layout(
          paper_bgcolor = 'rgba(0,0,0,0)',
          plot_bgcolor = 'rgba(0,0,0,0)',
          font = list(color = "white")
        )
    })
    
    # Top Failed Searches Chart
    output$top_searches_chart <- renderPlotly({
      top_searches <- zero_search_data() %>%
        arrange(desc(Searches)) %>%
        head(10) %>%
        mutate(Search_String_Short = ifelse(nchar(Search_String) > 50, 
                                           paste0(substr(Search_String, 1, 50), "..."), 
                                           Search_String))
      
      p <- ggplot(top_searches, aes(x = reorder(Search_String_Short, Searches), y = Searches)) +
        geom_bar(stat = "identity", fill = "#f39c12") +
        coord_flip() +
        labs(title = "Top 10 Failed Search Queries",
             x = "Search Query",
             y = "Number of Searches") +
        theme_minimal() +
        theme(
          plot.title = element_text(hjust = 0.5, color = "white"),
          axis.title = element_text(color = "white"),
          axis.text = element_text(color = "white"),
          panel.background = element_rect(fill = "transparent", color = NA),
          plot.background = element_rect(fill = "transparent", color = NA),
          panel.grid.major = element_line(color = "grey40"),
          panel.grid.minor = element_line(color = "grey40")
        )
      
      ggplotly(p, tooltip = "none") %>%
        config(displayModeBar = FALSE) %>%
        layout(
          paper_bgcolor = 'rgba(0,0,0,0)',
          plot_bgcolor = 'rgba(0,0,0,0)',
          font = list(color = "white")
        )
    })
    
    # Sticky Facets Chart
    output$sticky_facets_chart <- renderPlotly({
      sticky_data <- zero_search_data() %>%
        mutate(Sticky_Facets_Label = ifelse(Sticky_Facets_Used == 1, "Used", "Not Used")) %>%
        group_by(Sticky_Facets_Label) %>%
        summarise(Total_Searches = sum(Searches, na.rm = TRUE), .groups = 'drop')
      
      p <- ggplot(sticky_data, aes(x = Sticky_Facets_Label, y = Total_Searches)) +
        geom_bar(stat = "identity", fill = "#2ecc71") +
        labs(title = "Sticky Facets Usage in Failed Searches",
             x = "Sticky Facets",
             y = "Total Failed Searches") +
        theme_minimal() +
        theme(
          plot.title = element_text(hjust = 0.5, color = "white"),
          axis.title = element_text(color = "white"),
          axis.text = element_text(color = "white"),
          panel.background = element_rect(fill = "transparent", color = NA),
          plot.background = element_rect(fill = "transparent", color = NA),
          panel.grid.major = element_line(color = "grey40"),
          panel.grid.minor = element_line(color = "grey40")
        )
      
      ggplotly(p, tooltip = "none") %>%
        config(displayModeBar = FALSE) %>%
        layout(
          paper_bgcolor = 'rgba(0,0,0,0)',
          plot_bgcolor = 'rgba(0,0,0,0)',
          font = list(color = "white")
        )
    })
    
    # Data Table
    output$zero_search_table <- DT::renderDataTable({
      df <- zero_search_data()
      
      # Format the data for display
      df_display <- df %>%
        mutate(
          Search_String = ifelse(nchar(Search_String) > 100, 
                                paste0(substr(Search_String, 1, 100), "..."), 
                                Search_String),
          Sticky_Facets_Used = ifelse(Sticky_Facets_Used == 1, "Yes", "No")
        ) %>%
        arrange(desc(Searches))
      
      DT::datatable(
        df_display,
        options = list(
          pageLength = 15,
          scrollX = TRUE,
          columnDefs = list(
            list(className = 'dt-center', targets = c(0, 3, 4, 5, 6))
          )
        ),
        colnames = c("Searches", "Search String", "User Group", "Field Searched", 
                    "Sticky Facets", "Scope Type", "Search Scope"),
        rownames = FALSE
      )
    })
    
  })
}
