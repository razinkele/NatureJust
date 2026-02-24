# Shiny Server entry point for NatureJust-EU
# Deployed to /srv/shiny-server/naturejust on laguna.ku.lt

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
  library(jsonlite)
  library(eurostat)
  library(giscoR)
  for (pkg in c("httr2")) {
    tryCatch(library(pkg, character.only = TRUE), error = function(e) NULL)
  }
})

# Source all R files in dependency order
app_dir <- getwd()
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

shinyApp(ui = app_ui, server = app_server)
