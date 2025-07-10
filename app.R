library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)
library(RColorBrewer)
library(bslib)
library(plotly)

# Source modules
source("modules/overview_module.R")
source("modules/libguides_table_module.R")
source("modules/devices_table_module.R")
source("modules/devices_analytics_module.R")
source("modules/libguides_analytics_module.R")
source("modules/facets_analytics_module.R")
source("modules/zero_search_analytics_module.R")

ui <- fluidPage(
  # Dynamic theme based on dark mode toggle
  theme = bs_theme(version = 5, bootswatch = "minty"),
  
  # Add custom CSS for dark mode
  tags$head(
    tags$style(HTML("
      .dark-mode {
        background-color: #1a1a1a !important;
        color: #ffffff !important;
      }
      .dark-mode .navbar {
        background-color: #2d2d2d !important;
      }
      .dark-mode .box {
        background-color: #2d2d2d !important;
        border-color: #444444 !important;
      }
      .dark-mode .box-header {
        background-color: #333333 !important;
        color: #ffffff !important;
      }
      .dark-mode .content-wrapper {
        background-color: #1a1a1a !important;
      }
      .dark-mode .card {
        background-color: #2d2d2d !important;
        border-color: #444444 !important;
        color: #ffffff !important;
      }
      .dark-mode .card-header {
        background-color: #333333 !important;
        color: #ffffff !important;
      }
      .dark-mode .dataTables_wrapper {
        color: #ffffff !important;
      }
      .dark-mode .dataTables_wrapper .dataTables_length,
      .dark-mode .dataTables_wrapper .dataTables_filter,
      .dark-mode .dataTables_wrapper .dataTables_info,
      .dark-mode .dataTables_wrapper .dataTables_paginate {
        color: #ffffff !important;
      }
      .dark-mode table.dataTable {
        background-color: #2d2d2d !important;
        color: #ffffff !important;
      }
      .dark-mode table.dataTable thead th {
        background-color: #333333 !important;
        color: #ffffff !important;
      }
      .dark-mode table.dataTable tbody tr {
        background-color: #2d2d2d !important;
        color: #ffffff !important;
      }
      .dark-mode table.dataTable tbody tr:hover {
        background-color: #404040 !important;
      }
      .dark-mode .plotly {
        background-color: #2d2d2d !important;
      }
      .dark-mode .plotly .bg {
        fill: #2d2d2d !important;
      }
      .dark-mode .plotly .main-svg {
        background-color: #2d2d2d !important;
      }
      .dark-mode .plotly text {
        fill: #ffffff !important;
      }
      .dark-mode .plotly .xtick text,
      .dark-mode .plotly .ytick text {
        fill: #ffffff !important;
      }
      .dark-mode .plotly .xtitle,
      .dark-mode .plotly .ytitle {
        fill: #ffffff !important;
      }
      .dark-mode .plotly .gtitle {
        fill: #ffffff !important;
      }
      .dark-mode h1, .dark-mode h2, .dark-mode h3, .dark-mode h4, .dark-mode h5, .dark-mode h6 {
        color: #ffffff !important;
      }
      .dark-mode .js-plotly-plot .plotly .modebar {
        display: none !important;
      }
      .dark-mode .js-plotly-plot .plotly .svg-container {
        background-color: transparent !important;
      }
      .theme-toggle {
        position: fixed;
        top: 10px;
        right: 10px;
        z-index: 1000;
      }
    "))
  ),
  
  # Dark mode toggle button
  div(class = "theme-toggle",
      actionButton("dark_mode_toggle", 
                   icon("moon"), 
                   class = "btn btn-outline-secondary btn-sm",
                   title = "Toggle Dark Mode")
  ),
  
  # Add JavaScript to handle view switching and dark mode
  tags$script(HTML("
    Shiny.addCustomMessageHandler('updateCurrentView', function(view) {
      Shiny.setInputValue('current_view', view);
    });
    
    Shiny.addCustomMessageHandler('toggleDarkMode', function(isDark) {
      if (isDark) {
        document.body.classList.add('dark-mode');
        document.getElementById('dark_mode_toggle').innerHTML = '<i class=\"fa fa-sun\"></i>';
      } else {
        document.body.classList.remove('dark-mode');
        document.getElementById('dark_mode_toggle').innerHTML = '<i class=\"fa fa-moon\"></i>';
      }
    });
  ")),
  
  navbarPage(
    title = "Library Analytics Hub",
    id = "navbar",
    
    tabPanel("Overview", value = "overview",
             overviewUI("overview")
    )
  ),
  
  # Dynamic content area for analytics modules
  conditionalPanel(
    condition = "input.current_view == 'libguides_analytics'",
    libguidesAnalyticsUI("libguides_analytics")
  ),
  
  conditionalPanel(
    condition = "input.current_view == 'devices_analytics'",
    devicesAnalyticsUI("devices_analytics")
  ),
  
  conditionalPanel(
    condition = "input.current_view == 'facets_analytics'",
    facetsAnalyticsUI("facets_analytics")
  ),
  
  conditionalPanel(
    condition = "input.current_view == 'zero_search_analytics'",
    zero_search_analytics_ui("zero_search_analytics")
  )
)

server <- function(input, output, session) {
  # Reactive value to control which view is shown
  current_view <- reactiveVal("overview")
  
  # Reactive value for dark mode
  dark_mode <- reactiveVal(FALSE)
  
  # Send current_view to client for conditionalPanel
  observe({
    session$sendCustomMessage("updateCurrentView", current_view())
  })
  
  # Handle dark mode toggle
  observeEvent(input$dark_mode_toggle, {
    current_mode <- dark_mode()
    dark_mode(!current_mode)
    session$sendCustomMessage("toggleDarkMode", dark_mode())
  })
  
  overviewServer("overview", session, current_view)
  # Keep analytics servers for navigation from overview
  libguidesAnalyticsServer("libguides_analytics", session, current_view)
  devicesAnalyticsServer("devices_analytics", session, current_view)
  facetsAnalyticsServer("facets_analytics", session, current_view)
  zero_search_analytics_server("zero_search_analytics")
}

shinyApp(ui = ui, server = server)