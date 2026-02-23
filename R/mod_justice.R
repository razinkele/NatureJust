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
        choices = tryCatch(load_interventions(), error = function(e) "MPA Establishment")
      ),
      selectInput(
        ns("target_area"), "Target Area",
        choices = list(
          "Baltic" = c("Baltic Sea"),
          "North Sea" = c("North Sea"),
          "Atlantic" = c("Atlantic Coast", "Celtic Sea"),
          "Mediterranean" = c("Mediterranean", "Adriatic", "Aegean"),
          "Black Sea" = c("Black Sea")
        )
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

    # Geographic context modifiers — regional governance/equity conditions
    # shift base scores to reflect known disparities across EU sea basins
    area_modifiers <- list(
      "Baltic Sea"       = c(Distributional = 0.05,  Procedural = 0.08,
                             Recognitional = 0.03,   Restorative = 0.00),
      "North Sea"        = c(Distributional = 0.08,  Procedural = 0.10,
                             Recognitional = 0.05,   Restorative = 0.04),
      "Atlantic Coast"   = c(Distributional = 0.03,  Procedural = 0.05,
                             Recognitional = 0.02,   Restorative = 0.01),
      "Mediterranean"    = c(Distributional = -0.05, Procedural = -0.08,
                             Recognitional = -0.03,  Restorative = -0.06),
      "Black Sea"        = c(Distributional = -0.08, Procedural = -0.10,
                             Recognitional = -0.05,  Restorative = -0.07),
      "Adriatic"         = c(Distributional = -0.03, Procedural = -0.04,
                             Recognitional = 0.00,   Restorative = -0.03),
      "Aegean"           = c(Distributional = -0.06, Procedural = -0.07,
                             Recognitional = -0.02,  Restorative = -0.05),
      "Celtic Sea"       = c(Distributional = 0.04,  Procedural = 0.06,
                             Recognitional = 0.03,   Restorative = 0.02)
    )

    scores <- reactive({
      df <- load_justice_scores(input$intervention)
      area <- input$target_area
      mods <- area_modifiers[[area]]
      if (!is.null(mods)) {
        for (i in seq_len(nrow(df))) {
          mod <- mods[df$dimension[i]]
          if (!is.na(mod)) {
            df$score[i] <- pmin(pmax(df$score[i] + mod, 0), 1)
            df$status[i] <- if (df$score[i] >= 0.7) "green"
                            else if (df$score[i] >= 0.4) "amber"
                            else "red"
          }
        }
      }
      df
    })

    # Justice scorecard cards
    output$scorecard <- renderUI({
      df <- scores()
      area <- input$target_area

      tagList(
        p(class = "text-muted small mb-2",
          paste0("Scores adjusted for ", area, " regional context.")),
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
              traffic_light(status),
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

    # Region-specific recommendation context
    area_context <- list(
      "Baltic Sea"       = "HELCOM coordination and Baltic RAC",
      "North Sea"        = "OSPAR framework and North Sea RAC",
      "Atlantic Coast"   = "South Western Waters and North Western Waters RACs",
      "Mediterranean"    = "GFCM and MEDAC advisory structures",
      "Black Sea"        = "Bucharest Convention and BlackSea4Fish",
      "Adriatic"         = "GFCM and bilateral IT-HR cooperation",
      "Aegean"           = "GFCM and Aegean-Levantine sub-region",
      "Celtic Sea"       = "Celtic Seas Partnership and NWW RAC"
    )

    # Build area-specific justice recommendations (shared by UI and report download)
    justice_recommendations <- reactive({
      area <- input$target_area
      context <- area_context[[area]]
      list(
        Distributional = c(
          "Consider EMFAF compensation schemes for affected fishing communities",
          paste0("Explore Just Transition Fund eligibility for ", area, " coastal regions"),
          "Implement benefit-sharing mechanisms for ecosystem service gains"
        ),
        Procedural = c(
          "Establish Aarhus Convention-compliant consultation procedures",
          paste0("Engage ", context, " for structured stakeholder input"),
          "Create participatory mapping exercises with local communities"
        ),
        Recognitional = c(
          "Integrate traditional ecological knowledge in impact assessments",
          paste0("Document cultural heritage values specific to ", area, " communities"),
          "Recognise indigenous and local community territorial rights"
        ),
        Restorative = c(
          paste0("Assess historical displacement from existing protected areas in ", area),
          "Design targeted support for communities with legacy pollution exposure",
          "Establish monitoring of cumulative impact on marginalised groups"
        )
      )
    })

    # Gap analysis
    output$gap_analysis <- renderUI({
      df <- scores()
      gaps <- df[df$status != "green", ]
      area <- input$target_area

      if (nrow(gaps) == 0) {
        return(div(
          class = "alert alert-success",
          bsicons::bs_icon("check-circle"),
          paste0(" All justice dimensions are adequately addressed in the ",
                 area, " context.")
        ))
      }

      recommendations <- justice_recommendations()

      tagList(
        div(class = "alert alert-warning",
            bsicons::bs_icon("exclamation-triangle"),
            paste0(" ", nrow(gaps), " justice dimension(s) require attention",
                   " in the ", area, " context.")),
        lapply(seq_len(nrow(gaps)), function(i) {
          dim_name <- gaps$dimension[i]
          recs <- recommendations[[dim_name]]
          div(
            class = "mb-3",
            h6(
              traffic_light(gaps$status[i]),
              dim_name, " Justice"
            ),
            tags$ul(
              lapply(recs, function(r) tags$li(r))
            )
          )
        })
      )
    })

    # Download report — includes scorecard + gap analysis
    output$download_report <- downloadHandler(
      filename = function() {
        paste0("justice-assessment-",
               gsub(" ", "-", tolower(input$intervention)),
               "-", Sys.Date(), ".html")
      },
      content = function(file) {
        df <- scores()
        area <- input$target_area
        context <- area_context[[area]]
        gaps <- df[df$status != "green", ]
        recommendations <- justice_recommendations()

        # Build gap analysis HTML
        gap_html <- ""
        if (nrow(gaps) > 0) {
          gap_html <- paste0(
            "<h2>Gap Analysis &amp; Recommendations</h2>",
            "<p><strong>", nrow(gaps), "</strong> justice dimension(s) require attention ",
            "in the ", area, " context.</p>"
          )
          for (i in seq_len(nrow(gaps))) {
            dim_name <- gaps$dimension[i]
            recs <- recommendations[[dim_name]]
            gap_html <- paste0(gap_html,
              "<h3 class='", gaps$status[i], "'>", dim_name, " Justice</h3>",
              "<ul>", paste0("<li>", recs, "</li>", collapse = ""), "</ul>"
            )
          }
        } else {
          gap_html <- paste0(
            "<h2>Gap Analysis</h2>",
            "<p class='green'>All justice dimensions are adequately addressed ",
            "in the ", area, " context.</p>"
          )
        }

        html <- paste0(
          "<html><head><title>Justice Impact Assessment</title>",
          "<style>body{font-family:sans-serif;max-width:800px;margin:auto;padding:2rem}",
          ".green{color:#41ae76}.amber{color:#f0ad4e}.red{color:#d9534f}",
          "table{border-collapse:collapse;width:100%}td,th{border:1px solid #ddd;padding:8px}",
          "h3{margin-top:1.5rem}</style>",
          "</head><body>",
          "<h1>Justice Impact Assessment</h1>",
          "<p><strong>Intervention:</strong> ", input$intervention, "</p>",
          "<p><strong>Target Area:</strong> ", area, "</p>",
          "<p><strong>Regional Context:</strong> ", context, "</p>",
          "<p><strong>Date:</strong> ", Sys.Date(), "</p>",
          "<h2>Justice Scorecard</h2>",
          "<table><tr><th>Dimension</th><th>Score</th><th>Status</th><th>Description</th></tr>",
          paste0(
            "<tr><td>", df$dimension, "</td>",
            "<td>", round(df$score * 100), "%</td>",
            "<td class='", df$status, "'>",
            ifelse(df$status == "green", "Adequate",
                   ifelse(df$status == "amber", "Needs Attention", "Critical Gap")),
            "</td>",
            "<td>", df$description, "</td></tr>",
            collapse = ""
          ),
          "</table>",
          gap_html,
          "<hr><p><em>Generated by NatureJust-EU &mdash; ",
          "Sources: Kunming-Montreal GBF, EU environmental justice literature</em></p>",
          "</body></html>"
        )
        writeLines(html, file)
      }
    )
  })
}
