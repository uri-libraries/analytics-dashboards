library(shiny)
library(bslib)

overviewUI <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    
    # Page Header
    div(
      class = "container-fluid text-center",
      style = "padding: 40px 20px;",
      h1("URI Library Analytics Hub"),
      p("Welcome to the central dashboard for library analytics.", class = "lead")
    ),
    
    # Dashboard Navigation Cards
    layout_columns(
      col_widths = c(3, 3, 3, 3),
      
      # LibGuides Analytics Card
      card(
        full_screen = TRUE,
        card_header("LibGuides Analytics"),
        card_body(
          div(class = "text-center",
              icon("book", class = "fa-5x", style = "color: #007bff; margin-bottom: 20px;"),
              p("Interactive visualizations and trends for LibGuides usage.")
          )
        ),
        card_footer(
          actionButton(ns("go_to_libguides"), 
                       label = "View Dashboard",
                       class = "btn btn-primary w-100")
        )
      ),
      
      # Device Usage Analytics Card
      card(
        full_screen = TRUE,
        card_header("Device Usage Analytics"),
        card_body(
          div(class = "text-center",
              icon("laptop", class = "fa-5x", style = "color: #28a745; margin-bottom: 20px;"),
              p("Donut charts for device and browser usage from Primo sessions.")
          )
        ),
        card_footer(
          actionButton(ns("go_to_devices"), 
                       label = "View Dashboard",
                       class = "btn btn-success w-100")
        )
      ),
      
      # Facets Analytics Card
      card(
        full_screen = TRUE,
        card_header("Facets by Frequency"),
        card_body(
          div(class = "text-center",
              icon("filter", class = "fa-5x", style = "color: #dc3545; margin-bottom: 20px;"),
              p("Analysis of search facet usage and selection patterns.")
          )
        ),
        card_footer(
          actionButton(ns("go_to_facets"), 
                       label = "View Dashboard",
                       class = "btn btn-danger w-100")
        )
      ),
      
      # Zero Search Analytics Card
      card(
        full_screen = TRUE,
        card_header("Zero Search Analytics"),
        card_body(
          div(class = "text-center",
              icon("search-minus", class = "fa-5x", style = "color: #6f42c1; margin-bottom: 20px;"),
              p("Analysis of failed search queries and user search patterns.")
          )
        ),
        card_footer(
          actionButton(ns("go_to_zero_search"), 
                       label = "View Dashboard",
                       class = "btn btn-purple w-100",
                       style = "background-color: #6f42c1; border-color: #6f42c1;")
        )
      )
    ),
    
    # GitHub Link Footer
    hr(),
    div(
      class = "text-center",
      style = "padding: 20px; color: #6c757d;",
      p(
        icon("github", style = "margin-right: 8px;"),
        "View source code on ",
        tags$a(href = "https://github.com/uri-libraries/analytics-dashboards", 
               target = "_blank",
               "GitHub",
               style = "color: #007bff; text-decoration: none;")
      )
    )
  )
}

overviewServer <- function(id, parent_session, current_view = NULL) {
  moduleServer(id, function(input, output, session) {
    
    # Navigate to LibGuides dashboard
    observeEvent(input$go_to_libguides, {
      if (!is.null(current_view)) {
        current_view("libguides_analytics")
      } else {
        updateTabsetPanel(session = parent_session, inputId = "navbar", selected = "libguides_analytics")
      }
    })
    
    # Navigate to Devices dashboard
    observeEvent(input$go_to_devices, {
      if (!is.null(current_view)) {
        current_view("devices_analytics")
      } else {
        updateTabsetPanel(session = parent_session, inputId = "navbar", selected = "devices_analytics")
      }
    })
    
    # Navigate to Facets dashboard
    observeEvent(input$go_to_facets, {
      if (!is.null(current_view)) {
        current_view("facets_analytics")
      } else {
        updateTabsetPanel(session = parent_session, inputId = "navbar", selected = "facets_analytics")
      }
    })
    
    # Navigate to Zero Search dashboard
    observeEvent(input$go_to_zero_search, {
      if (!is.null(current_view)) {
        current_view("zero_search_analytics")
      } else {
        updateTabsetPanel(session = parent_session, inputId = "navbar", selected = "zero_search_analytics")
      }
    })
    
  })
}