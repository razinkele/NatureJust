# Shiny Server entry point for NatureJust-EU
# Deployed to /srv/shiny-server/naturejust on laguna.ku.lt

app_dir <- getwd()

# Load dependencies
suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(bsicons)
  library(leaflet)
  library(sf)
  library(plotly)
  library(ggplot2)
  library(dplyr)
  library(DT)
  library(shinyWidgets)
  library(rnaturalearth)
  library(rnaturalearthdata)
  library(htmltools)
  library(config)
  library(eurostat)
  library(giscoR)
  for (pkg in c("httr2", "jsonlite")) {
    tryCatch(library(pkg, character.only = TRUE), error = function(e) NULL)
  }
})

# app_sys helper (golem convention)
app_sys <- function(...) {
  file.path(app_dir, "inst", ...)
}

# Source all R files in dependency order
r_files <- c(
  "R/utils_helpers.R",
  "R/utils_data_helpers.R",
  "R/fct_fallback_data.R",
  "R/fct_real_data.R",
  "R/mod_home.R",
  "R/mod_narratives.R",
  "R/mod_stakeholders.R",
  "R/mod_pathways.R",
  "R/mod_spatial.R",
  "R/mod_scenarios.R",
  "R/mod_justice.R",
  "R/mod_governance.R",
  "R/mod_dashboard.R"
)

for (f in r_files) {
  source(file.path(app_dir, f), local = FALSE)
}

# Inline golem_add_external_resources (bypass golem dependency)
golem_add_external_resources <- function() {
  www_path <- file.path(app_dir, "inst", "app", "www")
  shiny::addResourcePath("www", www_path)
  shiny::tags$head(
    shiny::tags$link(
      rel = "preconnect", href = "https://fonts.googleapis.com"
    ),
    shiny::tags$link(
      rel = "preconnect", href = "https://fonts.gstatic.com", crossorigin = NA
    ),
    shiny::tags$link(
      rel = "stylesheet",
      href = "https://fonts.googleapis.com/css2?family=Playfair+Display:ital,wght@0,400;0,500;0,600;0,700;1,400&family=Figtree:wght@300;400;500;600;700&display=swap"
    ),
    shiny::tags$link(rel = "stylesheet", href = "www/custom.css"),
    shiny::tags$script(src = "www/nff_triangle.js"),
    shiny::tags$title("NatureJust-EU")
  )
}

# Stub golem functions that app_ui.R references
favicon <- function(...) shiny::tags$link(rel = "icon", href = "")
add_resource_path <- shiny::addResourcePath
bundle_resources <- function(path, app_title, ...) {
  shiny::tags$head(
    shiny::tags$link(rel = "stylesheet", href = "www/custom.css"),
    shiny::tags$script(src = "www/nff_triangle.js")
  )
}

# Source the UI and server
source(file.path(app_dir, "R/app_ui.R"), local = FALSE)
source(file.path(app_dir, "R/app_server.R"), local = FALSE)

shinyApp(ui = app_ui, server = app_server)
