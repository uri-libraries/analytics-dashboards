library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)
library(RColorBrewer)

# Source modules
source("modules/overview_module.R")
source("modules/libguides_analytics_module.R")
source("modules/libguides_table_module.R")

ui <- navbarPage(
  title = "Library Analytics Hub",
  id = "navbar",
  
  tabPanel("Overview", value = "overview",
           overviewUI("overview")
  ),
  
  tabPanel("LibGuides Table", value = "libguides_table",
           libguidesTableUI("libguides_table")
  ),
  
  tabPanel("Device Table", value = "device_table",
           fluidPage(
             h2("Device Usage Table"),
             p("Coming soon - device usage data table")
           )
  )
)

server <- function(input, output, session) {
  overviewServer("overview")
  libguidesTableServer("libguides_table")
}

shinyApp(ui = ui, server = server)