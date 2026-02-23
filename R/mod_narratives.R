#' Narrative Deep-Dive module UI
#'
#' @param id Module namespace id
#' @import shiny
#' @noRd
mod_narratives_ui <- function(id) {
  ns <- NS(id)

  tagList(
    div(
      class = "narratives-hero",
      h2("Marine Governance Narratives"),
      p(class = "text-muted",
        "Six illustrative narratives for nature futures in marine governance",
        tags$br(),
        tags$small("Based on Dur\u00e1n et al. (2023) and Pereira et al. (2023)"))
    ),

    bslib::layout_column_wrap(
      width = 1/2,

    # ---- Left card: Select Narrative ----
    bslib::card(
      bslib::card_header(
        class = "bg-primary text-white",
        bsicons::bs_icon("book"),
        " Select Narrative"
      ),
      bslib::card_body(
        selectInput(
          ns("narrative_id"),
          label = "NFF Narrative",
          choices = c(
            "Arcology" = "arcology",
            "Sharing through Sparing" = "sharing",
            "Optimizing Nature" = "optimizing",
            "Innovative Commons" = "commons",
            "Reciprocal Stewardship" = "stewardship",
            "Dynamic Natures" = "dynamic"
          ),
          selected = "arcology"
        ),
        tags$h6(class = "mt-3 mb-2", "NFF Position"),
        uiOutput(ns("nff_badges")),
        tags$hr(),
        actionButton(
          ns("run_scenario"),
          label = "Run as Scenario",
          class = "btn-primary w-100",
          icon = icon("play")
        )
      ),
      bslib::card_footer(
        class = "text-muted text-center small",
        "Sets NFF weights to this narrative's position and navigates to the Scenarios tab."
      )
    ),

    # ---- Right card: Narrative Detail ----
    bslib::card(
      bslib::card_header(
        class = "bg-primary text-white",
        bsicons::bs_icon("journal-richtext"),
        " ",
        textOutput(ns("detail_title"), inline = TRUE)
      ),
      bslib::card_body(
        class = "overflow-auto",
        uiOutput(ns("narrative_detail"))
      )
    )
    )
  )
}

#' Narrative Deep-Dive module server
#'
#' @param id Module namespace id
#' @param nff_weights A reactiveVal holding named numeric c(NfN, NfS, NaC)
#' @import shiny
#' @noRd
mod_narratives_server <- function(id, nff_weights = NULL) {
  moduleServer(id, function(input, output, session) {

    # Load narrative data once at startup
    narratives <- load_narratives()

    # ---- Current selected narrative (single row) ----
    current <- reactive({
      req(input$narrative_id)
      idx <- which(narratives$id == input$narrative_id)
      if (length(idx) == 0) return(NULL)
      narratives[idx, , drop = FALSE]
    })

    # ---- Helper: extract weights from the nested column ----
    current_weights <- reactive({
      narr <- current()
      req(narr)
      w <- narr$weights
      # weights is a data.frame column from jsonlite; extract the single row
      if (is.data.frame(w)) {
        c(NfN = as.numeric(w$NfN[1]),
          NfS = as.numeric(w$NfS[1]),
          NaC = as.numeric(w$NaC[1]))
      } else if (is.list(w)) {
        # Could be a list of lists from JSON
        ww <- w[[1]]
        if (is.list(ww)) {
          c(NfN = as.numeric(ww$NfN),
            NfS = as.numeric(ww$NfS),
            NaC = as.numeric(ww$NaC))
        } else {
          c(NfN = as.numeric(ww[["NfN"]]),
            NfS = as.numeric(ww[["NfS"]]),
            NaC = as.numeric(ww[["NaC"]]))
        }
      } else {
        # Fallback for minimal fallback data (no weights column)
        c(NfN = 34, NfS = 33, NaC = 33)
      }
    })

    # ---- NFF Position Badges ----
    output$nff_badges <- renderUI({
      w <- current_weights()
      div(
        class = "d-flex gap-2 flex-wrap",
        tags$span(
          class = "badge rounded-pill",
          style = "background-color: #0E7C7B; font-size: 0.85rem;",
          paste0("NfN ", w[["NfN"]], "%")
        ),
        tags$span(
          class = "badge rounded-pill",
          style = "background-color: #2A6F97; font-size: 0.85rem;",
          paste0("NfS ", w[["NfS"]], "%")
        ),
        tags$span(
          class = "badge rounded-pill",
          style = "background-color: #E07A5F; font-size: 0.85rem;",
          paste0("NaC ", w[["NaC"]], "%")
        )
      )
    })

    # ---- Dynamic header for right card ----
    output$detail_title <- renderText({
      narr <- current()
      if (is.null(narr)) return("Narrative Detail")
      paste0(narr$name[1], " \u2014 Detail")
    })

    # ---- Narrative Detail Panel ----
    output$narrative_detail <- renderUI({
      narr <- current()
      if (is.null(narr)) {
        return(div(class = "alert alert-secondary",
                   "Select a narrative to view details."))
      }

      # Safely extract list columns, unlisting for display
      extract_list <- function(col) {
        if (is.null(col)) return(character(0))
        val <- col
        # For a single-row data.frame slice, list columns give a list of length 1
        if (is.list(val) && !is.data.frame(val)) {
          val <- unlist(val, recursive = TRUE)
        }
        as.character(val)
      }

      key_policies <- extract_list(narr$key_policies)
      marine_examples <- extract_list(narr$marine_examples)
      trade_offs <- extract_list(narr$trade_offs)
      gbf_targets <- extract_list(narr$gbf_targets_prioritized)

      description <- if (!is.null(narr$description)) narr$description[1] else ""
      governance <- if (!is.null(narr$governance_model)) narr$governance_model[1] else ""
      source_ref <- if (!is.null(narr$source)) narr$source[1] else ""
      nff_pos <- if (!is.null(narr$nff_position)) narr$nff_position[1] else ""

      # Build a bullet list from a character vector
      make_ul <- function(items) {
        if (length(items) == 0 || all(items == "")) {
          return(tags$p(class = "text-muted fst-italic", "No data available."))
        }
        tags$ul(
          class = "mb-0",
          lapply(items, function(item) tags$li(item))
        )
      }

      tagList(
        # NFF position subtitle
        tags$p(
          class = "text-muted small mb-3",
          bsicons::bs_icon("triangle"),
          paste0(" NFF Position: ", nff_pos)
        ),

        # Description
        tags$h6(
          class = "d-flex align-items-center gap-1 mb-2",
          bsicons::bs_icon("info-circle"),
          "Description"
        ),
        tags$p(description),

        tags$hr(),

        # Governance Model
        tags$h6(
          class = "d-flex align-items-center gap-1 mb-2",
          bsicons::bs_icon("bank"),
          "Governance Model"
        ),
        tags$p(governance),

        tags$hr(),

        # Key Policies
        tags$h6(
          class = "d-flex align-items-center gap-1 mb-2",
          bsicons::bs_icon("list-check"),
          "Key Policies"
        ),
        make_ul(key_policies),

        tags$hr(),

        # Real-World Marine Examples
        tags$h6(
          class = "d-flex align-items-center gap-1 mb-2",
          bsicons::bs_icon("globe-europe-africa"),
          "Real-World Marine Examples"
        ),
        make_ul(marine_examples),

        tags$hr(),

        # Trade-offs
        tags$h6(
          class = "d-flex align-items-center gap-1 mb-2",
          bsicons::bs_icon("arrow-left-right"),
          "Trade-offs"
        ),
        make_ul(trade_offs),

        tags$hr(),

        # GBF Targets Prioritized
        tags$h6(
          class = "d-flex align-items-center gap-1 mb-2",
          bsicons::bs_icon("bullseye"),
          "GBF Targets Prioritized"
        ),
        if (length(gbf_targets) > 0 && !all(gbf_targets == "")) {
          div(
            class = "d-flex gap-2 flex-wrap",
            lapply(gbf_targets, function(tgt) {
              tags$span(
                class = "badge bg-secondary rounded-pill",
                style = "font-size: 0.85rem;",
                paste0("Target ", tgt)
              )
            })
          )
        } else {
          tags$p(class = "text-muted fst-italic", "No data available.")
        },

        tags$hr(),

        # Source reference
        tags$div(
          class = "text-muted small mt-2",
          bsicons::bs_icon("journal-text"),
          paste0(" ", source_ref)
        )
      )
    })

    # ---- Run as Scenario: set weights and navigate ----
    observeEvent(input$run_scenario, {
      w <- current_weights()
      narr <- current()

      # Update the shared NFF weights
      if (!is.null(nff_weights)) {
        nff_weights(c(
          NfN = as.integer(w[["NfN"]]),
          NfS = as.integer(w[["NfS"]]),
          NaC = as.integer(w[["NaC"]])
        ))
      }

      # Navigate to Scenarios tab via the parent session
      # nav_select needs access to the top-level session, so we walk up
      # from the module session (session$ns is the namespace function).
      # The parent session is available as session$userData or we can use
      # the root session trick via session$sendCustomMessage + JS.
      session$sendCustomMessage(
        "nj-nav-select",
        list(nav_id = "main_nav", tab = "Scenarios")
      )

      showNotification(
        paste0("Loaded '", narr$name[1],
               "' weights (NfN=", w[["NfN"]],
               " NfS=", w[["NfS"]],
               " NaC=", w[["NaC"]], "%). Switch to Scenarios tab."),
        type = "message"
      )
    })
  })
}
