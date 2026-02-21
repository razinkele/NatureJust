#' Justice Impact Assessment module UI
#' @param id Module namespace id
#' @noRd
mod_justice_ui <- function(id) {
  ns <- NS(id)

  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      title = "Intervention",
      width = 280,
      selectInput(
        ns("intervention"), "Select Intervention",
        choices = mock_interventions()
      ),
      selectInput(
        ns("target_area"), "Target Area",
        choices = c("Baltic Sea", "North Sea", "Atlantic Coast",
                    "Mediterranean", "Black Sea", "Adriatic",
                    "Aegean", "Celtic Sea")
      ),
      hr(),
      downloadButton(ns("download_report"), "Download Report",
                     class = "btn-primary w-100")
    ),

    tagList(
      # Justice Scorecard
      h5("Justice Scorecard", class = "mb-3"),
      bslib::layout_column_wrap(
        width = 1/2,
        uiOutput(ns("scorecard"))
      ),

      # Gap Analysis
      bslib::card(
        class = "mt-3",
        bslib::card_header("Gap Analysis & Recommendations"),
        bslib::card_body(
          uiOutput(ns("gap_analysis"))
        )
      )
    )
  )
}

#' Justice Impact Assessment module server
#' @param id Module namespace id
#' @noRd
mod_justice_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    scores <- reactive({
      mock_justice_scores(input$intervention)
    })

    # Justice scorecard cards
    output$scorecard <- renderUI({
      df <- scores()
      icons <- c(
        Distributional = "pie-chart",
        Procedural = "people",
        Recognitional = "eye",
        Restorative = "arrow-counterclockwise"
      )

      tagList(
        lapply(seq_len(nrow(df)), function(i) {
          status <- df$status[i]
          score_pct <- round(df$score[i] * 100)
          status_label <- switch(status,
                                 green = "Adequate",
                                 amber = "Needs Attention",
                                 red = "Critical Gap")

          bslib::card(
            class = paste("justice-card", paste0("status-", status)),
            bslib::card_header(
              tags$span(class = paste("traffic-light", status)),
              df$dimension[i]
            ),
            bslib::card_body(
              h3(paste0(score_pct, "%"), class = "mb-1"),
              p(class = "text-muted mb-1", status_label),
              p(class = "small", df$description[i])
            )
          )
        })
      )
    })

    # Gap analysis
    output$gap_analysis <- renderUI({
      df <- scores()
      gaps <- df[df$status != "green", ]

      if (nrow(gaps) == 0) {
        return(div(
          class = "alert alert-success",
          bsicons::bs_icon("check-circle"),
          " All justice dimensions are adequately addressed."
        ))
      }

      recommendations <- list(
        Distributional = list(
          "Consider EMFAF compensation schemes for affected fishing communities",
          "Explore Just Transition Fund eligibility for coastal regions",
          "Implement benefit-sharing mechanisms for ecosystem service gains"
        ),
        Procedural = list(
          "Establish Aarhus Convention-compliant consultation procedures",
          "Include small-scale fishers in management advisory councils",
          "Create participatory mapping exercises with local communities"
        ),
        Recognitional = list(
          "Integrate traditional ecological knowledge in impact assessments",
          "Recognise indigenous and local community territorial rights",
          "Document cultural heritage values in conservation planning"
        ),
        Restorative = list(
          "Assess historical displacement from existing protected areas",
          "Design targeted support for communities with legacy pollution exposure",
          "Establish monitoring of cumulative impact on marginalised groups"
        )
      )

      tagList(
        div(class = "alert alert-warning",
            bsicons::bs_icon("exclamation-triangle"),
            paste0(" ", nrow(gaps), " justice dimension(s) require attention.")),
        lapply(seq_len(nrow(gaps)), function(i) {
          dim_name <- gaps$dimension[i]
          recs <- recommendations[[dim_name]]
          div(
            class = "mb-3",
            h6(
              tags$span(class = paste("traffic-light", gaps$status[i])),
              dim_name, " Justice"
            ),
            tags$ul(
              lapply(recs, function(r) tags$li(r))
            )
          )
        })
      )
    })

    # Download report
    output$download_report <- downloadHandler(
      filename = function() {
        paste0("justice-assessment-",
               gsub(" ", "-", tolower(input$intervention)),
               "-", Sys.Date(), ".html")
      },
      content = function(file) {
        df <- scores()
        # Simple HTML report
        html <- paste0(
          "<html><head><title>Justice Impact Assessment</title>",
          "<style>body{font-family:sans-serif;max-width:800px;margin:auto;padding:2rem}",
          ".green{color:#41ae76}.amber{color:#f0ad4e}.red{color:#d9534f}",
          "table{border-collapse:collapse;width:100%}td,th{border:1px solid #ddd;padding:8px}</style>",
          "</head><body>",
          "<h1>Justice Impact Assessment</h1>",
          "<p><strong>Intervention:</strong> ", input$intervention, "</p>",
          "<p><strong>Target Area:</strong> ", input$target_area, "</p>",
          "<p><strong>Date:</strong> ", Sys.Date(), "</p>",
          "<table><tr><th>Dimension</th><th>Score</th><th>Status</th></tr>",
          paste0(
            "<tr><td>", df$dimension, "</td>",
            "<td>", round(df$score * 100), "%</td>",
            "<td class='", df$status, "'>",
            ifelse(df$status == "green", "Adequate",
                   ifelse(df$status == "amber", "Needs Attention", "Critical Gap")),
            "</td></tr>",
            collapse = ""
          ),
          "</table>",
          "<p><em>Generated by NatureJust-EU</em></p>",
          "</body></html>"
        )
        writeLines(html, file)
      }
    )
  })
}
