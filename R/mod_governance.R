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
                        choices = mock_interventions())
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
          uiOutput(ns("stakeholder_checklist"))
        )
      )
    )
  )
}

#' Governance & Funding module server
#' @param id Module namespace id
#' @noRd
mod_governance_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    # Funding matrix table
    output$funding_table <- DT::renderDataTable({
      df <- mock_funding_matrix()
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

    # CFP alignment check
    output$cfp_result <- renderUI({
      measure <- input$cfp_measure
      set.seed(nchar(measure))

      alignment <- sample(c("aligned", "partial", "conflict"), 1,
                          prob = c(0.4, 0.35, 0.25))

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

      mock_details <- switch(alignment,
                             aligned = list(
                               "Compatible with existing Total Allowable Catch (TAC) regulations",
                               "Supports CFP discard ban implementation",
                               "Aligns with Maximum Sustainable Yield (MSY) objectives"
                             ),
                             partial = list(
                               "May require adjustment to regional fishing effort limits",
                               "Partial overlap with EMFAF fleet adaptation measures",
                               "Needs coordination with Regional Advisory Councils"
                             ),
                             conflict = list(
                               "Conflicts with allocated fishing quotas in target area",
                               "Requires Article 11 MSFD/CFP coordination procedure",
                               "May trigger compensation obligations under EMFAF Article 17"
                             ))

      tagList(
        div(class = paste("alert", status_class),
            bsicons::bs_icon(status_icon),
            paste0(" ", status_text)),
        tags$ul(
          lapply(mock_details, function(d) tags$li(d))
        )
      )
    })

    # Stakeholder tracker
    output$stakeholder_checklist <- renderUI({
      stakeholders <- c(
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

      set.seed(42)
      consulted <- sample(c(TRUE, FALSE), length(stakeholders),
                          replace = TRUE, prob = c(0.5, 0.5))

      n_consulted <- sum(consulted)
      n_total <- length(stakeholders)
      pct <- round(n_consulted / n_total * 100)

      tagList(
        bslib::value_box(
          title = "Consultation Progress",
          value = paste0(n_consulted, " / ", n_total),
          showcase = bsicons::bs_icon("people-fill"),
          theme = if (pct >= 80) "success" else if (pct >= 50) "warning" else "danger",
          p(paste0(pct, "% of stakeholder groups consulted"))
        ),
        div(class = "mt-3",
            lapply(seq_along(stakeholders), function(i) {
              div(
                class = "d-flex align-items-center mb-2 p-2 border rounded",
                tags$span(
                  class = paste("traffic-light",
                                if (consulted[i]) "green" else "red")
                ),
                tags$span(stakeholders[i]),
                if (!consulted[i]) {
                  tags$span(class = "ms-auto badge bg-warning",
                            "Not yet consulted")
                }
              )
            })
        )
      )
    })
  })
}
