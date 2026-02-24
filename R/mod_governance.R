#' Governance & Funding module UI
#' @param id Module namespace id
#' @noRd
mod_governance_ui <- function(id) {
  ns <- NS(id)

  tagList(
    bslib::navset_card_tab(
      title = "Governance & Funding Alignment",

      bslib::nav_panel(
        "Funding Matrix",
        bslib::card_body(
          p(class = "text-muted",
            "Eligibility of conservation interventions across EU funding instruments."),
          DT::dataTableOutput(ns("funding_table"))
        )
      ),

      bslib::nav_panel(
        "CFP Alignment",
        bslib::layout_sidebar(
          sidebar = bslib::sidebar(
            title = "Check Alignment",
            selectInput(ns("cfp_measure"), "Conservation Measure",
                        choices = "MPA Establishment")
          ),
          bslib::card(
            bslib::card_header("Common Fisheries Policy Alignment"),
            bslib::card_body(
              uiOutput(ns("cfp_result"))
            )
          )
        )
      ),

      bslib::nav_panel(
        "Stakeholder Tracker",
        bslib::card_body(
          p(class = "text-muted",
            "Track stakeholder consultation against GBF Target 22 requirements."),
          uiOutput(ns("stakeholder_progress")),
          div(class = "mt-3",
            checkboxGroupInput(
              ns("consulted_stakeholders"),
              label = NULL,
              choices = c(
                "National Fisheries Authorities",
                "Regional/Local Government",
                "Small-Scale Fishers Associations",
                "Industrial Fishing Fleet Representatives",
                "Environmental NGOs",
                "Indigenous & Local Community Representatives",
                "Marine Scientists & Researchers",
                "Tourism & Recreation Sector",
                "Port Authorities",
                "Youth & Future Generations Representatives"
              )
            ),
            actionButton(ns("reset_stakeholders"), "Reset All",
                         class = "btn-outline-secondary btn-sm mt-2",
                         icon = icon("rotate-left"))
          )
        )
      ),

      bslib::nav_panel(
        "Elliott's 10 Tenets",
        bslib::layout_sidebar(
          sidebar = bslib::sidebar(
            title = "Assessment",
            selectInput(ns("tenet_intervention"), "Select Intervention",
                        choices = "MPA Establishment"),
            p(class = "text-muted small mt-2",
              "Based on Elliott (2013): 10 tenets for integrated,",
              "successful and sustainable marine management.")
          ),
          tagList(
            h5("Tenet Radar Profile", class = "mb-3"),
            plotly::plotlyOutput(ns("tenet_radar"), height = "400px"),
            h5("Tenet Scorecard", class = "mt-4 mb-3"),
            bslib::layout_column_wrap(width = 1/2, uiOutput(ns("tenet_cards"))),
            bslib::card(
              class = "mt-3",
              bslib::card_header("Gap Analysis & Recommendations"),
              bslib::card_body(uiOutput(ns("tenet_gaps")))
            )
          )
        )
      )
    )
  )
}

#' Governance & Funding module server
#' @param id Module namespace id
#' @noRd
mod_governance_server <- function(id, intervention_choices = NULL) {
  moduleServer(id, function(input, output, session) {

    # Populate intervention choices from data (deferred from UI build time)
    observe({
      choices <- intervention_choices %||%
        tryCatch(load_interventions(), error = function(e) "MPA Establishment")
      updateSelectInput(session, "cfp_measure", choices = choices)
      updateSelectInput(session, "tenet_intervention", choices = choices)
    }) |> bindEvent(TRUE, once = TRUE)

    # Funding matrix — loaded once (static data)
    funding_data <- reactive({ load_funding_matrix() })

    # Funding matrix table
    output$funding_table <- DT::renderDataTable({
      df <- funding_data()
      DT::datatable(
        df,
        options = list(
          pageLength = 10,
          dom = "ft",
          columnDefs = list(list(className = "dt-center", targets = 1:5))
        ),
        rownames = FALSE
      ) |>
        DT::formatStyle(
          columns = c("EMFAF", "LIFE", "Cohesion Fund", "EAFRD",
                       "Just Transition Fund"),
          backgroundColor = DT::styleEqual(
            c("Eligible", "Partial", "Not eligible"),
            c("#d4edda", "#fff3cd", "#f8d7da")
          )
        )
    })

    # CFP alignment data — reactive (re-evaluates only when input changes)
    cfp_data <- reactive({
      req(input$cfp_measure)
      load_cfp_alignment(input$cfp_measure)
    })

    # CFP alignment check — uses evidence-based data from cfp_alignment.csv
    output$cfp_result <- renderUI({
      cfp <- cfp_data()
      alignment <- cfp$alignment[1]

      status_class <- switch(alignment,
                             aligned = "alert-success",
                             partial = "alert-warning",
                             conflict = "alert-danger")

      status_icon <- switch(alignment,
                            aligned = "check-circle",
                            partial = "exclamation-triangle",
                            conflict = "x-circle")

      status_text <- switch(alignment,
                            aligned = "This measure is fully aligned with current CFP management plans.",
                            partial = "This measure requires modifications to align with CFP quota allocations.",
                            conflict = "This measure conflicts with existing CFP management plans and requires derogation.")

      details <- list(cfp$detail_1[1], cfp$detail_2[1], cfp$detail_3[1])

      tagList(
        div(class = paste("alert", status_class),
            bsicons::bs_icon(status_icon),
            paste0(" ", status_text)),
        tags$ul(
          lapply(details, function(d) tags$li(d))
        )
      )
    })

    # --- Elliott's 10 Tenets ---
    tenet_scores <- reactive({
      req(input$tenet_intervention)
      load_elliott_tenets(input$tenet_intervention)
    })

    # Tenet radar chart
    output$tenet_radar <- plotly::renderPlotly({
      df <- tenet_scores()
      plotly::plot_ly(type = "scatterpolar", fill = "toself") |>
        plotly::add_trace(
          r = c(df$score, df$score[1]),
          theta = c(df$tenet, df$tenet[1]),
          name = input$tenet_intervention,
          fillcolor = "rgba(27,73,101,0.2)",
          line = list(color = "#1B4965")
        ) |>
        plotly::layout(
          polar = list(radialaxis = list(visible = TRUE, range = c(0, 1))),
          showlegend = FALSE
        )
    })

    # Tenet scorecard cards
    output$tenet_cards <- renderUI({
      df <- tenet_scores()
      tenet_icons <- c(
        "Ecologically sustainable" = "tree",
        "Technologically feasible" = "gear",
        "Economically viable" = "currency-euro",
        "Socially desirable" = "people",
        "Ethically defensible" = "shield-check",
        "Culturally inclusive" = "globe",
        "Legally permissible" = "book",
        "Administratively achievable" = "building",
        "Effectively communicable" = "megaphone",
        "Politically expedient" = "flag"
      )

      tagList(
        lapply(seq_len(nrow(df)), function(i) {
          status <- df$status[i]
          score_pct <- round(df$score[i] * 100)
          icon_name <- tenet_icons[[df$tenet[i]]]

          bslib::card(
            class = paste("justice-card", paste0("status-", status)),
            bslib::card_header(
              traffic_light(status),
              if (!is.null(icon_name)) bsicons::bs_icon(icon_name),
              df$tenet[i]
            ),
            bslib::card_body(
              h3(paste0(score_pct, "%"), class = "mb-1"),
              p(class = "text-muted mb-1", status_to_label(status)),
              p(class = "small", df$description[i])
            )
          )
        })
      )
    })

    # Tenet gap analysis
    output$tenet_gaps <- renderUI({
      df <- tenet_scores()
      gaps <- df[df$status != "green", ]

      if (nrow(gaps) == 0) {
        return(div(
          class = "alert alert-success",
          bsicons::bs_icon("check-circle"),
          " All 10 tenets are adequately addressed for this intervention."
        ))
      }

      recommendations <- list(
        "Ecologically sustainable" = list(
          "Conduct comprehensive Environmental Impact Assessment (EIA)",
          "Integrate cumulative effects assessment with existing pressures",
          "Establish long-term ecological monitoring programme"
        ),
        "Technologically feasible" = list(
          "Commission technology readiness assessment for proposed measures",
          "Pilot technologies at small scale before full deployment",
          "Ensure monitoring and enforcement technologies are operational"
        ),
        "Economically viable" = list(
          "Conduct full cost-benefit analysis including ecosystem service valuation",
          "Identify EU funding instruments (EMFAF, LIFE, Cohesion Fund)",
          "Develop benefit-sharing mechanisms for affected communities"
        ),
        "Socially desirable" = list(
          "Conduct social impact assessment with affected communities",
          "Establish transparent compensation or transition support schemes",
          "Build community ownership through co-management arrangements"
        ),
        "Ethically defensible" = list(
          "Apply precautionary principle where scientific uncertainty exists",
          "Ensure intergenerational equity in decision-making frameworks",
          "Address disproportionate burden on vulnerable communities"
        ),
        "Culturally inclusive" = list(
          "Integrate traditional ecological knowledge in management plans",
          "Recognise cultural heritage values of marine and coastal spaces",
          "Ensure multilingual and culturally appropriate consultation processes"
        ),
        "Legally permissible" = list(
          "Review compatibility with EU environmental acquis and UNCLOS",
          "Address cross-border legal coordination requirements",
          "Ensure Aarhus Convention compliance for public participation"
        ),
        "Administratively achievable" = list(
          "Assess institutional capacity and coordination needs across agencies",
          "Simplify permitting procedures and reduce administrative bottlenecks",
          "Build enforcement and monitoring capacity in competent authorities"
        ),
        "Effectively communicable" = list(
          "Develop clear public communication strategy with visual materials",
          "Engage local media and community networks for outreach",
          "Translate scientific evidence into accessible policy briefs"
        ),
        "Politically expedient" = list(
          "Build cross-party political coalitions around shared objectives",
          "Align intervention with current EU policy priorities (Green Deal, REPowerEU)",
          "Engage Regional Advisory Councils and stakeholder platforms early"
        )
      )

      tagList(
        div(class = "alert alert-warning",
            bsicons::bs_icon("exclamation-triangle"),
            paste0(" ", nrow(gaps), " of 10 tenets require attention.")),
        lapply(seq_len(nrow(gaps)), function(i) {
          tenet_name <- gaps$tenet[i]
          recs <- recommendations[[tenet_name]]
          div(
            class = "mb-3",
            h6(
              traffic_light(gaps$status[i]),
              tenet_name
            ),
            tags$ul(
              lapply(recs, function(r) tags$li(r))
            )
          )
        })
      )
    })

    # Stakeholder tracker — interactive checkboxes
    observeEvent(input$reset_stakeholders, {
      updateCheckboxGroupInput(session, "consulted_stakeholders", selected = character(0))
    })

    output$stakeholder_progress <- renderUI({
      n_consulted <- length(input$consulted_stakeholders)
      n_total <- 10L
      pct <- round(n_consulted / n_total * 100)

      bslib::value_box(
        title = "Consultation Progress",
        value = paste0(n_consulted, " / ", n_total),
        showcase = bsicons::bs_icon("people-fill"),
        theme = if (pct >= 80) "success" else if (pct >= 50) "warning" else "danger",
        p(paste0(pct, "% of stakeholder groups consulted"))
      )
    })
  })
}
