.libPaths(c("C:/Users/arturas.baziukas/AppData/Local/R/win-library/4.4", .libPaths()))
app_dir <- "C:/Users/arturas.baziukas/OneDrive - ku.lt/HORIZON_EUROPE/Antonello/NatureJust"
setwd(app_dir)

suppressPackageStartupMessages({
  library(testthat)
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
  library(jsonlite)
})

app_sys <- function(...) file.path(app_dir, "inst", ...)

# Source all R files in order
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

for (f in r_files) source(file.path(app_dir, f), local = FALSE)

# Stub golem functions needed by app_ui.R / test-golem-recommended.R
favicon <- function(...) htmltools::tags$link(rel = "icon", href = "")
add_resource_path <- shiny::addResourcePath
bundle_resources <- function(path, app_title, ...) {
  htmltools::tags$head(
    htmltools::tags$link(rel = "stylesheet", href = "www/custom.css"),
    htmltools::tags$script(src = "www/nff_triangle.js")
  )
}

# Source app_ui.R and app_server.R
source(file.path(app_dir, "R/app_ui.R"), local = FALSE)
source(file.path(app_dir, "R/app_server.R"), local = FALSE)

test_results <- test_dir(file.path(app_dir, "tests/testthat"), reporter = "summary")
