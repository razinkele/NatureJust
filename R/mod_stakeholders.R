#' Participatory Stakeholder Positioning module UI
#'
#' Three-column layout: Add Stakeholder, NFF Triangle, Analysis.
#' Allows users to position stakeholders on the NFF triangle and
#' compute collective positioning statistics.
#'
#' @param id Module namespace id
#' @import shiny
#' @noRd
mod_stakeholders_ui <- function(id) {
  ns <- NS(id)

  tagList(
    # ---- Hero header ----
    div(
      class = "stakeholders-hero",
      h2("Participatory Positioning"),
      p(class = "text-muted",
        "Map stakeholder preferences onto the Nature Futures Framework.",
        tags$br(),
        tags$small(
          "Each stakeholder clicks a position in the triangle to express",
          " their biodiversity governance priorities."
        ))
    ),

    # ---- Three-column layout ----
    bslib::layout_column_wrap(
      width = 1/3,

      # ══════════════════════════════════════════════
      # Column 1: Add Stakeholder
      # ══════════════════════════════════════════════
      bslib::card(
        bslib::card_header(
          class = "bg-primary text-white",
          bsicons::bs_icon("person-plus"),
          " Add Stakeholder"
        ),
        bslib::card_body(
          textInput(
            ns("stakeholder_name"),
            label = "Stakeholder Name",
            placeholder = "e.g., Fisher Cooperative"
          ),
          selectInput(
            ns("stakeholder_group"),
            label = "Stakeholder Group",
            choices = c(
              "Government",
              "Industry",
              "Civil Society",
              "Academia",
              "Indigenous/Local Community",
              "NGO",
              "Other"
            ),
            selected = "Government"
          ),
          # Preview badges for current triangle position
          tags$h6(class = "mt-2 mb-2", "Current Position Preview"),
          div(
            class = "d-flex gap-2 flex-wrap mb-3",
            tags$span(
              class = "badge rounded-pill",
              style = paste0("background-color: ", NFF_COLORS$NfN, "; font-size: 0.85rem;"),
              textOutput(ns("preview_nfn"), inline = TRUE)
            ),
            tags$span(
              class = "badge rounded-pill",
              style = paste0("background-color: ", NFF_COLORS$NfS, "; font-size: 0.85rem;"),
              textOutput(ns("preview_nfs"), inline = TRUE)
            ),
            tags$span(
              class = "badge rounded-pill",
              style = paste0("background-color: ", NFF_COLORS$NaC, "; font-size: 0.85rem;"),
              textOutput(ns("preview_nac"), inline = TRUE)
            )
          ),
          actionButton(
            ns("add_stakeholder"),
            label = "Add Stakeholder",
            class = "btn-primary w-100",
            icon = icon("plus")
          ),
          tags$hr(),
          div(
            class = "d-flex gap-2",
            actionButton(
              ns("clear_all"),
              label = "Clear All",
              class = "btn-outline-danger flex-fill",
              icon = icon("trash")
            ),
            downloadButton(
              ns("export_csv"),
              label = "Export CSV",
              class = "btn-outline-secondary flex-fill"
            )
          )
        )
      ),

      # ══════════════════════════════════════════════
      # Column 2: NFF Triangle (stakeholder mode)
      # ══════════════════════════════════════════════
      bslib::card(
        bslib::card_header(
          class = "bg-primary text-white",
          bsicons::bs_icon("triangle"),
          " Collective NFF Positioning"
        ),
        bslib::card_body(
          class = "nff-triangle-container",
          div(
            id = ns("nff_stakeholder_widget"),
            class = "nff-triangle-widget nff-stakeholder-mode",
            `data-input-id` = ns("triangle_position"),
            HTML(nff_triangle_svg("s", mode = "stakeholder"))
          ),
          # Weight readout (populated by JS initTriangle)
          div(
            class = "nff-weight-readout",
            id = ns("nff_stakeholder_readout")
          )
        ),
        bslib::card_footer(
          class = "text-muted text-center",
          "Click to position. Colored dots show all stakeholder positions."
        )
      ),

      # ══════════════════════════════════════════════
      # Column 3: Analysis
      # ══════════════════════════════════════════════
      bslib::card(
        bslib::card_header(
          class = "bg-primary text-white",
          bsicons::bs_icon("bar-chart-line"),
          " Analysis"
        ),
        bslib::card_body(
          uiOutput(ns("stakeholder_stats")),
          tags$hr(),
          tags$h6(
            class = "d-flex align-items-center gap-1 mb-2",
            bsicons::bs_icon("people"),
            "Stakeholder List"
          ),
          DT::DTOutput(ns("stakeholder_table"))
        )
      )
    )
  )
}


#' Participatory Stakeholder Positioning module server
#'
#' Manages stakeholder data, NFF triangle positioning, collective
#' statistics (centroid, spread, consensus), CSV export, and optional
#' integration with the shared nff_weights reactiveVal.
#'
#' @param id Module namespace id
#' @param nff_weights A reactiveVal holding named numeric c(NfN, NfS, NaC),
#'   or NULL if not connected to the shared state.
#' @import shiny
#' @noRd
mod_stakeholders_server <- function(id, nff_weights = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ---- State ----
    stakeholders <- reactiveVal(
      data.frame(
        name  = character(0),
        group = character(0),
        NfN   = numeric(0),
        NfS   = numeric(0),
        NaC   = numeric(0),
        stringsAsFactors = FALSE
      )
    )

    current_pos <- reactiveVal(c(NfN = 34, NfS = 33, NaC = 33))

    # ---- Listen to triangle clicks ----
    observeEvent(input$triangle_position, {
      pos <- input$triangle_position
      if (!is.null(pos)) {
        current_pos(c(
          NfN = as.integer(pos$NfN),
          NfS = as.integer(pos$NfS),
          NaC = as.integer(pos$NaC)
        ))
      }
    })

    # ---- Preview badges ----
    output$preview_nfn <- renderText({
      paste0("NfN: ", current_pos()[["NfN"]], "%")
    })
    output$preview_nfs <- renderText({
      paste0("NfS: ", current_pos()[["NfS"]], "%")
    })
    output$preview_nac <- renderText({
      paste0("NaC: ", current_pos()[["NaC"]], "%")
    })

    # ── Add stakeholder ──
    observeEvent(input$add_stakeholder, {
      name <- trimws(input$stakeholder_name)
      if (nchar(name) == 0) {
        showNotification("Please enter a stakeholder name.", type = "warning")
        return()
      }

      pos <- current_pos()
      new_row <- data.frame(
        name  = name,
        group = input$stakeholder_group,
        NfN   = pos[["NfN"]],
        NfS   = pos[["NfS"]],
        NaC   = pos[["NaC"]],
        stringsAsFactors = FALSE
      )
      stakeholders(rbind(stakeholders(), new_row))

      # Send dot to JS for rendering on the SVG
      session$sendCustomMessage("stakeholder-add-dot", list(
        NfN   = pos[["NfN"]] / 100,
        NfS   = pos[["NfS"]] / 100,
        NaC   = pos[["NaC"]] / 100,
        name  = name,
        group = input$stakeholder_group
      ))

      # Clear name input
      updateTextInput(session, "stakeholder_name", value = "")
      showNotification(
        paste0("Added '", name, "' (", input$stakeholder_group, ")"),
        type = "message"
      )
    })

    # ── Clear all stakeholders ──
    observeEvent(input$clear_all, {
      stakeholders(data.frame(
        name  = character(0),
        group = character(0),
        NfN   = numeric(0),
        NfS   = numeric(0),
        NaC   = numeric(0),
        stringsAsFactors = FALSE
      ))
      session$sendCustomMessage("stakeholder-clear-dots", list())
      showNotification("All stakeholders cleared.", type = "message")
    })

    # ── Statistics panel ──
    output$stakeholder_stats <- renderUI({
      df <- stakeholders()
      n <- nrow(df)

      if (n == 0) {
        return(div(
          class = "alert alert-secondary",
          bsicons::bs_icon("info-circle"),
          " No stakeholders added yet. Click a position in the triangle",
          " and add a stakeholder to begin analysis."
        ))
      }

      # Centroid
      centroid_nfn <- mean(df$NfN)
      centroid_nfs <- mean(df$NfS)
      centroid_nac <- mean(df$NaC)

      # Spread (sqrt of sum of variances of bary coords)
      spread_val <- if (n >= 2) {
        sqrt(stats::var(df$NfN) + stats::var(df$NfS) + stats::var(df$NaC))
      } else {
        NA_real_
      }

      # Consensus zone: % within euclidean distance 15 of centroid in NfN/NfS/NaC space
      consensus_pct <- if (n >= 2) {
        dists <- sqrt(
          (df$NfN - centroid_nfn)^2 +
          (df$NfS - centroid_nfs)^2 +
          (df$NaC - centroid_nac)^2
        )
        round(sum(dists <= 15) / n * 100, 1)
      } else {
        100
      }

      # Group breakdown
      group_counts <- table(df$group)
      group_items <- lapply(names(group_counts), function(g) {
        tags$li(paste0(g, ": ", group_counts[[g]]))
      })

      tagList(
        # Count
        bslib::value_box(
          title = "Stakeholders",
          value = n,
          showcase = bsicons::bs_icon("people-fill"),
          theme = "primary",
          class = "mb-3"
        ),

        # Centroid
        tags$h6(
          class = "d-flex align-items-center gap-1 mb-2 mt-2",
          bsicons::bs_icon("bullseye"),
          "Centroid (Mean Position)"
        ),
        nff_badge_set(c(
          NfN = round(centroid_nfn, 1),
          NfS = round(centroid_nfs, 1),
          NaC = round(centroid_nac, 1)
        )),
        tags$div(class = "mb-3"),

        # Spread
        if (!is.na(spread_val)) {
          div(
            class = "mb-2",
            tags$h6(
              class = "d-flex align-items-center gap-1 mb-1",
              bsicons::bs_icon("arrows-angle-expand"),
              "Spread"
            ),
            tags$p(
              class = "mb-1",
              paste0(round(spread_val, 2),
                     " (std dev of barycentric coordinates)")
            )
          )
        },

        # Consensus zone
        div(
          class = "mb-2",
          tags$h6(
            class = "d-flex align-items-center gap-1 mb-1",
            bsicons::bs_icon("check2-circle"),
            "Consensus Zone"
          ),
          tags$p(
            class = "mb-1",
            paste0(consensus_pct,
                   "% of stakeholders within 15 units of centroid")
          )
        ),

        # Group breakdown
        tags$h6(
          class = "d-flex align-items-center gap-1 mb-1 mt-2",
          bsicons::bs_icon("diagram-3"),
          "Groups"
        ),
        tags$ul(class = "small mb-3", group_items),

        # Use Centroid button (only if n >= 2 and nff_weights connected)
        if (n >= 2 && !is.null(nff_weights)) {
          actionButton(
            ns("use_centroid"),
            label = "Use Centroid as Weights",
            class = "btn-outline-primary w-100",
            icon = icon("crosshairs")
          )
        }
      )
    })

    # ── Use centroid as shared weights ──
    observeEvent(input$use_centroid, {
      df <- stakeholders()
      if (nrow(df) < 2 || is.null(nff_weights)) return()

      mean_nfn <- mean(df$NfN)
      mean_nfs <- mean(df$NfS)
      mean_nac <- mean(df$NaC)

      # Normalize to sum = 100
      total <- mean_nfn + mean_nfs + mean_nac
      w_nfn <- round(mean_nfn / total * 100)
      w_nfs <- round(mean_nfs / total * 100)
      w_nac <- 100L - w_nfn - w_nfs  # ensure exact sum of 100

      nff_weights(c(NfN = as.integer(w_nfn),
                    NfS = as.integer(w_nfs),
                    NaC = as.integer(w_nac)))

      showNotification(
        paste0("NFF weights updated to centroid: NfN=", w_nfn,
               "%, NfS=", w_nfs, "%, NaC=", w_nac, "%"),
        type = "message"
      )
    })

    # ── Stakeholder data table ──
    output$stakeholder_table <- DT::renderDT({
      df <- stakeholders()
      if (nrow(df) == 0) {
        return(DT::datatable(
          data.frame(
            Name = character(0), Group = character(0),
            `NfN %` = numeric(0), `NfS %` = numeric(0), `NaC %` = numeric(0),
            check.names = FALSE
          ),
          options = list(
            dom = "t",
            language = list(emptyTable = "No stakeholders added yet.")
          ),
          rownames = FALSE
        ))
      }

      display_df <- data.frame(
        Name     = df$name,
        Group    = df$group,
        `NfN %`  = df$NfN,
        `NfS %`  = df$NfS,
        `NaC %`  = df$NaC,
        check.names = FALSE
      )

      DT::datatable(
        display_df,
        options = list(
          pageLength = 10,
          scrollY = "280px",
          scrollCollapse = TRUE,
          dom = "ft",
          columnDefs = list(
            list(className = "dt-center", targets = 2:4)
          )
        ),
        rownames = FALSE,
        class = "compact stripe hover"
      )
    })

    # ── Export CSV ──
    output$export_csv <- downloadHandler(
      filename = function() {
        paste0("naturejust_stakeholders_", format(Sys.Date(), "%Y%m%d"), ".csv")
      },
      content = function(file) {
        utils::write.csv(stakeholders(), file, row.names = FALSE)
      }
    )
  })
}
