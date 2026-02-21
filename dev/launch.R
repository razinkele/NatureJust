# Standalone launcher - sources all R files directly
# Use this if devtools::load_all() is unavailable

# Ensure user library is on the path
.libPaths(c("C:/Users/arturas.baziukas/AppData/Local/R/win-library/4.4", .libPaths()))

app_dir <- "C:/Users/arturas.baziukas/OneDrive - ku.lt/HORIZON_EUROPE/Antonello/NatureJust"
setwd(app_dir)

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
  library(tidyr)
  library(purrr)
  library(DT)
  library(shinyWidgets)
  library(rnaturalearth)
  library(rnaturalearthdata)
  library(htmltools)
  library(config)
})

# app_sys helper (needed by golem_add_external_resources)
app_sys <- function(...) {
  file.path(app_dir, "inst", ...)
}

# Source all R files in order
r_files <- c(
  "R/utils_helpers.R",
  "R/fct_mock_data.R",
  "R/mod_home.R",
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

cat("Starting NatureJust-EU on http://127.0.0.1:7700\n")

shiny::shinyApp(
  ui = app_ui,
  server = app_server,
  options = list(port = 7700, host = "127.0.0.1", launch.browser = FALSE)
)
