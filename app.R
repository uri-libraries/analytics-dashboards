library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)
library(RColorBrewer)
library(bslib)

# Source modules
source("modules/overview_module.R")
source("modules/libguides_table_module.R")
source("modules/devices_table_module.R")
source("modules/devices_analytics_module.R")
source("modules/libguides_analytics_module.R")
source("modules/facets_analytics_module.R")

ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "minty"),
  
  # Add JavaScript to handle view switching
  tags$script(HTML("
    Shiny.addCustomMessageHandler('updateCurrentView', function(view) {
      Shiny.setInputValue('current_view', view);
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
  )
)

server <- function(input, output, session) {
  # Reactive value to control which view is shown
  current_view <- reactiveVal("overview")
  
  # Send current_view to client for conditionalPanel
  observe({
    session$sendCustomMessage("updateCurrentView", current_view())
  })
  
  overviewServer("overview", session, current_view)
  # Keep analytics servers for navigation from overview
  libguidesAnalyticsServer("libguides_analytics", session, current_view)
  devicesAnalyticsServer("devices_analytics", session, current_view)
  facetsAnalyticsServer("facets_analytics", session, current_view)
}

shinyApp(ui = ui, server = server)