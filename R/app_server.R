#' The application server-side
#'
#' @param input,output,session Internal parameters for `{shiny}`.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # ---- Shared NFF weights reactive ----
  # Writeable from Home triangle and Scenarios sliders
  nff_weights <- reactiveVal(c(NfN = 34, NfS = 33, NaC = 33))

  mod_home_server("home", nff_weights = nff_weights)
  mod_narratives_server("narratives", nff_weights = nff_weights)
  mod_stakeholders_server("stakeholders", nff_weights = nff_weights)
  mod_pathways_server("pathways", nff_weights = nff_weights)
  mod_spatial_server("spatial", nff_weights = nff_weights)
  mod_scenarios_server("scenarios", nff_weights = nff_weights)
  mod_justice_server("justice")
  mod_governance_server("governance")
  mod_dashboard_server("dashboard")

  # ---- Navigate to narrative from triangle double-click ----
  # Note: "narratives-narrative_id" couples to mod_narratives_ui's selectInput ID.
  # If that input is renamed, update this reference too.
  observeEvent(input$navigate_to_narrative, {
    updateSelectInput(session, "narratives-narrative_id",
                      selected = input$navigate_to_narrative)
    bslib::nav_select("main_nav", "Narratives", session = session)
  })

  # ---- About modal ----
  observeEvent(input$show_about, {
    app_version <- tryCatch(
      as.character(utils::packageVersion("NatureJust")),
      error = function(e) {
        desc_file <- file.path(app_sys(".."), "DESCRIPTION")
        if (file.exists(desc_file)) {
          desc <- read.dcf(desc_file, fields = "Version")
          as.character(desc[1, "Version"])
        } else {
          "0.1.0"
        }
      }
    )
    showModal(modalDialog(
      title = tagList(bsicons::bs_icon("globe-europe-africa"), " NatureJust-EU"),
      size = "m",
      easyClose = TRUE,
      footer = modalButton("Close"),
      div(
        class = "text-center py-3",
        tags$h4(
          class = "mb-1",
          style = "font-family: var(--font-display); font-weight: 700; color: var(--nj-navy);",
          "NatureJust-EU"
        ),
        tags$p(
          class = "text-muted mb-3",
          paste0("Version ", app_version)
        ),
        tags$hr(),
        tags$p(
          class = "mb-1",
          tags$strong("Decision-Support Tool for Equitable Biodiversity Governance")
        ),
        tags$p(
          class = "text-muted small mb-3",
          "Built on the Nature Futures Framework, Global Biodiversity Framework,",
          tags$br(),
          "and environmental justice principles."
        ),
        tags$hr(),
        tags$div(
          class = "mb-2",
          bsicons::bs_icon("person-circle"), " ",
          tags$strong("Arturas Razinkovas-Baziukas")
        ),
        tags$div(
          class = "text-muted small mb-2",
          bsicons::bs_icon("mortarboard"), " Klaipeda University, Marine Research Institute"
        ),
        tags$hr(),
        tags$p(
          class = "text-muted small mb-0",
          bsicons::bs_icon("shield-check"), " Licensed under MIT"
        )
      )
    ))
  })

  # ---- Help & Methodology modal ----
  observeEvent(input$show_help, {
    showModal(modalDialog(
      title = tagList(bsicons::bs_icon("book"), " Help & Methodology"),
      size = "l",
      easyClose = TRUE,
      footer = modalButton("Close"),
      bslib::accordion(
        id = "help_accordion",
        open = "Data Sources",
        bslib::accordion_panel(
          title = "Data Sources",
          icon = bsicons::bs_icon("database"),
          tags$table(
            class = "table table-sm table-striped mb-0",
            tags$thead(
              tags$tr(
                tags$th("Source"),
                tags$th("Provides"),
                tags$th("Coverage"),
                tags$th("Update")
              )
            ),
            tags$tbody(
              tags$tr(
                tags$td(tags$strong("Eurostat")),
                tags$td("GDP per capita, population density (NUTS-2)"),
                tags$td("EU-27 + NO"),
                tags$td("Annual")
              ),
              tags$tr(
                tags$td(tags$strong("ICES SAG")),
                tags$td("Fish stock assessments (F/F", tags$sub("MSY"), ", SSB)"),
                tags$td("NE Atlantic"),
                tags$td("Annual")
              ),
              tags$tr(
                tags$td(tags$strong("HELCOM HOLAS III")),
                tags$td("Baltic Sea environmental status indicators"),
                tags$td("Baltic Sea"),
                tags$td("Periodic")
              ),
              tags$tr(
                tags$td(tags$strong("GFCM")),
                tags$td("Mediterranean stock status & fishing effort"),
                tags$td("Mediterranean"),
                tags$td("Annual")
              ),
              tags$tr(
                tags$td(tags$strong("EEA / Natura 2000")),
                tags$td("Marine protected area boundaries"),
                tags$td("EU marine"),
                tags$td("As updated")
              ),
              tags$tr(
                tags$td(tags$strong("giscoR")),
                tags$td("NUTS-2 region geometries"),
                tags$td("EU-27 + NO"),
                tags$td("Stable")
              )
            )
          )
        ),
        bslib::accordion_panel(
          title = "Methodology",
          icon = bsicons::bs_icon("gear"),
          tags$h6("Nature Futures Framework (NFF)"),
          tags$p(
            "The NFF organises biodiversity values into three perspectives:",
            tags$strong("Nature for Nature"), "(intrinsic value),",
            tags$strong("Nature for People"), "(ecosystem services), and",
            tags$strong("Nature as Culture"), "(relational values).",
            "Users assign weights (summing to 1.0) via sliders to express governance priorities.",
            "The composite index for each region is the weighted sum of normalised sub-indicators."
          ),
          tags$h6("Scenario Projections"),
          tags$p(
            "Scenarios apply stochastic cumulative-sum trajectories to baseline indicator values",
            "over a user-defined horizon (default 2025\u20132050). Each trajectory is bounded by",
            "realistic floor/ceiling values. Multiple Monte Carlo runs are summarised as",
            "median \u00B1 interquartile range ribbons."
          ),
          tags$h6("GBF Compliance Traffic Lights"),
          tags$p(
            "Each indicator is evaluated against Global Biodiversity Framework targets using",
            "threshold-based traffic lights:",
            tags$span(class = "traffic-light green", style = "display:inline-block;"),
            tags$strong("On track"), " (\u2265 target),",
            tags$span(class = "traffic-light amber", style = "display:inline-block;"),
            tags$strong("Near target"), " (\u2265 80%),",
            tags$span(class = "traffic-light red", style = "display:inline-block;"),
            tags$strong("Off track"), " (< 80%)."
          )
        ),
        bslib::accordion_panel(
          title = "Indicators",
          icon = bsicons::bs_icon("speedometer2"),
          tags$p(
            "Indicators displayed in the dashboard come from multiple sources.",
            "Suffixes in indicator names denote their origin:"
          ),
          tags$ul(
            tags$li(tags$strong("(M)"), " \u2014 Modelled/scenario-projected value"),
            tags$li(tags$strong("(H)"), " \u2014 HELCOM data, Baltic Sea region only"),
            tags$li("No suffix \u2014 direct time-series from Eurostat or ICES")
          ),
          tags$p(
            "All indicators are normalised to [0, 1] for cross-comparison.",
            "The Spatial Equity map shows per-region composite scores computed from",
            "the weighted NFF combination of all available sub-indicators."
          )
        ),
        bslib::accordion_panel(
          title = "How to Cite",
          icon = bsicons::bs_icon("quote"),
          tags$p("If you use NatureJust-EU in your work, please cite:"),
          tags$blockquote(
            class = "blockquote border-start border-3 ps-3",
            style = "border-color: var(--nj-ocean) !important; font-size: .9rem;",
            tags$p(
              class = "mb-1",
              "Razinkovas-Baziukas, A. (2026).",
              tags$em("NatureJust-EU: A Decision-Support Tool for Equitable Biodiversity Governance."),
              "Klaipeda University, Marine Research Institute."
            )
          ),
          tags$p(
            class = "text-muted small",
            "For the underlying methodology, please also cite the",
            "Nature Futures Framework (PBL, 2023) and the",
            "Kunming-Montreal Global Biodiversity Framework (CBD, 2022)."
          )
        )
      )
    ))
  })
}
