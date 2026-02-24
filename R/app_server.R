#' The application server-side
#'
#' @param input,output,session Internal parameters for `{shiny}`.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # ---- Shared NFF weights reactive ----
  # Writeable from Home triangle and Scenarios sliders
  nff_weights <- reactiveVal(c(NfN = 34, NfS = 33, NaC = 33))

  # ---- Shared reactive for narrative navigation ----
  # The narratives module listens to this reactive instead of app_server
  # reaching into the module's internal selectInput ID.
  selected_narrative <- reactiveVal(NULL)

  mod_home_server("home", nff_weights = nff_weights)
  mod_narratives_server("narratives", nff_weights = nff_weights,
                        selected_narrative = selected_narrative)
  mod_stakeholders_server("stakeholders", nff_weights = nff_weights)
  mod_pathways_server("pathways", nff_weights = nff_weights)
  mod_spatial_server("spatial", nff_weights = nff_weights)
  mod_scenarios_server("scenarios", nff_weights = nff_weights)
  # Load intervention choices once (shared by justice + governance)
  intervention_choices <- tryCatch(load_interventions(), error = function(e) "MPA Establishment")
  mod_justice_server("justice", intervention_choices = intervention_choices)
  mod_governance_server("governance", intervention_choices = intervention_choices)
  mod_dashboard_server("dashboard", nff_weights = nff_weights)

  # ---- Send canonical narrative data to JS (single-source) ----
  # No reactive inputs → fires once on session start
  observe({
    narr <- tryCatch(load_narratives(), error = function(e) NULL)
    if (!is.null(narr) && is.data.frame(narr) && nrow(narr) > 0) {
      js_narr <- lapply(seq_len(nrow(narr)), function(i) {
        row <- narr[i, ]
        w <- if ("weights" %in% names(row) && is.data.frame(row$weights)) {
          list(NfN = row$weights$NfN[1], NfS = row$weights$NfS[1],
               NaC = row$weights$NaC[1])
        } else if ("weights" %in% names(row) && is.list(row$weights)) {
          ww <- row$weights[[1]]
          if (is.list(ww)) ww else list(NfN = 34, NfS = 33, NaC = 33)
        } else {
          list(NfN = 34, NfS = 33, NaC = 33)
        }
        bary <- c(w$NfN, w$NfS, w$NaC)
        bary <- bary / max(sum(bary), 1) # normalize to 0-1
        list(
          id   = row$id,
          name = row$name,
          pos  = if ("nff_position" %in% names(row)) row$nff_position else "",
          bary = bary,
          desc = if ("description" %in% names(row)) row$description else "",
          gov  = if ("governance_model" %in% names(row)) row$governance_model else ""
        )
      })
      names(js_narr) <- narr$id
      session$sendCustomMessage("set-narratives", js_narr)
    }
  })

  # ---- Navigate to narrative from triangle double-click ----
  # Uses a shared reactiveVal so the narratives module manages its own inputs.
  observeEvent(input$navigate_to_narrative, {
    selected_narrative(input$navigate_to_narrative)
    bslib::nav_select("main_nav", "Narratives", session = session)
  })

  # ---- About modal ----
  observeEvent(input$show_about, {
    app_version <- tryCatch(
      as.character(utils::packageVersion("NatureJust")),
      error = function(e) {
        desc <- utils::packageDescription("NatureJust", fields = "Version")
        if (!is.na(desc)) desc else "0.1.0"
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
            "The dashboard tracks 13 indicators across five sources.",
            "Suffixes in the chart legend denote origin:",
            tags$strong("(M)"), "= modelled,",
            tags$strong("(H)"), "= HELCOM (Baltic only),",
            "no suffix = observed time-series."
          ),
          tags$h6("Modelled Indicators (M)"),
          tags$ul(
            tags$li("Marine Biodiversity Index"),
            tags$li("Habitat Condition"),
            tags$li("Ecosystem Services"),
            tags$li("Community Wellbeing Index"),
            tags$li("Governance Effectiveness")
          ),
          tags$h6("ICES SAG / GFCM"),
          tags$ul(
            tags$li("Fish Stock Biomass \u2014 aggregate F/F", tags$sub("MSY"), " status"),
            tags$li("Sustainable Fishing \u2014 share of stocks fished sustainably")
          ),
          tags$h6("Eurostat"),
          tags$ul(
            tags$li("Offshore Wind Capacity \u2014 nrg_inf_epcrw (renewable capacity)"),
            tags$li("Coastal Tourism Pressure \u2014 tour_occ_nin2c (nights at coastal NUTS-2)"),
            tags$li("Bathing Water Quality \u2014 sdg_14_40 (% excellent)")
          ),
          tags$h6("HELCOM HOLAS III (Baltic Sea only) (H)"),
          tags$ul(
            tags$li("Contaminant Status"),
            tags$li("Eutrophication Status"),
            tags$li("Underwater Noise")
          ),
          tags$p(
            class = "text-muted small",
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
