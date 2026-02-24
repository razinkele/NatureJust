# Standalone launcher - sources all R files directly
# Use this for local development outside Shiny Server

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
  library(DT)
  library(shinyWidgets)
  library(rnaturalearth)
  library(rnaturalearthdata)
  library(htmltools)
  library(config)
  library(eurostat)
  library(giscoR)
  # icesSAG only needed by data-raw/prepare_data.R, not app runtime
  for (pkg in c("httr2", "jsonlite")) {
    tryCatch(library(pkg, character.only = TRUE), error = function(e) NULL)
  }
})

# Source all R files in dependency order
# app_sys and golem stubs are defined in R/app_ui.R
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
  "R/mod_dashboard.R",
  "R/app_ui.R",
  "R/app_server.R"
)

for (f in r_files) {
  source(file.path(app_dir, f), local = FALSE)
}

app_port <- as.integer(Sys.getenv("NATUREJUST_PORT", "7701"))
cat("Starting NatureJust-EU on http://127.0.0.1:", app_port, "\n", sep = "")

shiny::shinyApp(
  ui = app_ui,
  server = app_server,
  options = list(port = app_port, host = "127.0.0.1", launch.browser = TRUE)
)
