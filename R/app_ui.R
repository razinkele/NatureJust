#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    golem_add_external_resources(),
    bslib::page_navbar(
      title = tags$span(
        bsicons::bs_icon("globe-europe-africa"),
        "NatureJust-EU"
      ),
      id = "main_nav",
      theme = bslib::bs_theme(
        version = 5,
        primary = "#1B4965",
        secondary = "#9C9587",
        success = "#0E7C7B",
        info = "#5FA8D3",
        warning = "#F2CC8F",
        danger = "#E07A5F",
        "font-size-base" = "0.9rem",
        "body-bg" = "#FAFAF5",
        "body-color" = "#3D3B37",
        "card-border-color" = "rgba(10,22,40,0.05)"
      ),
      fillable = TRUE,
      bslib::nav_panel(
        title = "Home",
        icon = bsicons::bs_icon("house"),
        mod_home_ui("home")
      ),
      bslib::nav_panel(
        title = "Narratives",
        icon = bsicons::bs_icon("book-half"),
        mod_narratives_ui("narratives")
      ),
      bslib::nav_panel(
        title = "Stakeholders",
        icon = bsicons::bs_icon("people"),
        mod_stakeholders_ui("stakeholders")
      ),
      bslib::nav_panel(
        title = "Pathways",
        icon = bsicons::bs_icon("signpost-split"),
        mod_pathways_ui("pathways")
      ),
      bslib::nav_panel(
        title = "Spatial Equity",
        icon = bsicons::bs_icon("map"),
        mod_spatial_ui("spatial")
      ),
      bslib::nav_panel(
        title = "Scenarios",
        icon = bsicons::bs_icon("sliders"),
        mod_scenarios_ui("scenarios")
      ),
      bslib::nav_panel(
        title = "Justice",
        icon = bsicons::bs_icon("shield-check"),
        mod_justice_ui("justice")
      ),
      bslib::nav_panel(
        title = "Governance",
        icon = bsicons::bs_icon("bank"),
        mod_governance_ui("governance")
      ),
      bslib::nav_panel(
        title = "Indicators",
        icon = bsicons::bs_icon("graph-up"),
        mod_dashboard_ui("dashboard")
      ),
      bslib::nav_spacer(),
      bslib::nav_menu(
        title = "Info",
        icon = bsicons::bs_icon("info-circle"),
        align = "right",
        bslib::nav_item(
          actionLink("show_about", label = tagList(bsicons::bs_icon("person-badge"), " About"))
        ),
        bslib::nav_item(
          actionLink("show_help", label = tagList(bsicons::bs_icon("book"), " Help & Methodology"))
        )
      ),
      bslib::nav_item(
        bslib::input_dark_mode(id = "dark_mode")
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path("www", app_sys("app/www"))
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "NatureJust-EU"
    )
  )
}
