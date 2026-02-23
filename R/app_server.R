#' The application server-side
#'
#' @param input,output,session Internal parameters for `{shiny}`.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  mod_home_server("home")
  mod_spatial_server("spatial")
  mod_scenarios_server("scenarios")
  mod_justice_server("justice")
  mod_governance_server("governance")
  mod_dashboard_server("dashboard")
}
