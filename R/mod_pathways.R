#' Three Horizons Pathway Visualization module UI
#'
#' Two-column layout: Pathway Settings (left), NFF Triangle visualization (right).
#' Below: full-width Three Horizons Analysis panel.
#' Allows users to draw governance transition pathways on the NFF triangle
#' and animate progress along the timeline.
#'
#' @param id Module namespace id
#' @import shiny
#' @noRd
mod_pathways_ui <- function(id) {
  ns <- NS(id)

  tagList(
    # ---- Hero header ----
    div(
      class = "pathways-hero",
      h2("Three Horizons Pathways"),
      p(class = "text-muted",
        "Visualise governance transitions from current state to desired futures",
        tags$br(),
        tags$small(
          "Based on Pereira et al. (2020) and the Three Horizons Framework"
        ))
    ),

    # ---- Two-column layout ----
    bslib::layout_column_wrap(
      width = 1/2,

      # ======================================================
      # Left card: Pathway Settings
      # ======================================================
      bslib::card(
        bslib::card_header(
          class = "bg-primary text-white",
          bsicons::bs_icon("signpost-split"),
          " Pathway Settings"
        ),
        bslib::card_body(
          # -- Current State --
          tags$h6(class = "mb-2", "Current State (Now)"),
          tags$small(
            class = "text-muted d-block mb-2",
            "Where is marine governance today in your region?"
          ),
          div(
            class = "d-flex gap-2 flex-wrap mb-3",
            div(
              style = "width: 80px;",
              numericInput(
                ns("now_nfn"),
                label = "NfN %",
                value = 20,
                min = 0, max = 100, step = 1
              )
            ),
            div(
              style = "width: 80px;",
              numericInput(
                ns("now_nfs"),
                label = "NfS %",
                value = 60,
                min = 0, max = 100, step = 1
              )
            ),
            div(
              style = "width: 80px;",
              numericInput(
                ns("now_nac"),
                label = "NaC %",
                value = 20,
                min = 0, max = 100, step = 1
              )
            )
          ),

          tags$hr(),

          # -- Desired Future --
          tags$h6(class = "mb-2", "Desired Future"),
          selectInput(
            ns("future_preset"),
            label = "Future Narrative Preset",
            choices = c(
              "Custom"                    = "custom",
              "Arcology (100/0/0)"        = "arcology",
              "Sharing through Sparing (50/50/0)" = "sharing",
              "Optimizing Nature (0/100/0)"       = "optimizing",
              "Innovative Commons (0/50/50)"      = "commons",
              "Reciprocal Stewardship (0/0/100)"  = "stewardship",
              "Dynamic Natures (50/0/50)"         = "dynamic",
              "Balanced (34/33/33)"               = "balanced"
            ),
            selected = "custom"
          ),
          conditionalPanel(
            condition = sprintf("input['%s'] == 'custom'", ns("future_preset")),
            div(
              class = "d-flex gap-2 flex-wrap mb-3",
              div(
                style = "width: 80px;",
                numericInput(
                  ns("future_nfn"),
                  label = "NfN %",
                  value = 40,
                  min = 0, max = 100, step = 1
                )
              ),
              div(
                style = "width: 80px;",
                numericInput(
                  ns("future_nfs"),
                  label = "NfS %",
                  value = 30,
                  min = 0, max = 100, step = 1
                )
              ),
              div(
                style = "width: 80px;",
                numericInput(
                  ns("future_nac"),
                  label = "NaC %",
                  value = 30,
                  min = 0, max = 100, step = 1
                )
              )
            )
          ),

          tags$hr(),

          # -- Timeline slider --
          sliderInput(
            ns("timeline"),
            label = "Timeline",
            min = 2025,
            max = 2050,
            value = 2025,
            step = 1,
            sep = "",
            animate = animationOptions(interval = 200, loop = FALSE)
          ),

          tags$hr(),

          # -- Action buttons --
          div(
            class = "d-flex gap-2 flex-wrap",
            actionButton(
              ns("draw_pathway"),
              label = "Draw Pathway",
              class = "btn-primary",
              icon = icon("route")
            ),
            actionButton(
              ns("clear_pathways"),
              label = "Clear All Pathways",
              class = "btn-outline-secondary btn-sm",
              icon = icon("eraser")
            )
          )
        )
      ),

      # ======================================================
      # Right card: Pathway Visualization (NFF Triangle)
      # ======================================================
      bslib::card(
        bslib::card_header(
          class = "bg-primary text-white",
          bsicons::bs_icon("triangle"),
          " Pathway Visualization"
        ),
        bslib::card_body(
          class = "nff-triangle-container",
          div(
            id = ns("nff_pathway_widget"),
            class = "nff-triangle-widget nff-pathway-mode",
            `data-input-id` = ns("pathway_position"),
            HTML('
              <svg class="nff-svg" viewBox="-50 0 500 400"
                   xmlns="http://www.w3.org/2000/svg" role="img"
                   aria-label="Pathway NFF positioning triangle">
                <defs>
                  <!-- Clip to triangle boundary -->
                  <clipPath id="nff-tri-clip-p">
                    <polygon points="200,35 365,335 35,335"/>
                  </clipPath>
                  <!-- Radial gradients (suffix -p for pathway mode) -->
                  <radialGradient id="nff-grad-nfn-p" cx="200" cy="35" r="200"
                                  gradientUnits="userSpaceOnUse">
                    <stop offset="0%"  stop-color="#0E7C7B" stop-opacity="0.7"/>
                    <stop offset="45%" stop-color="#0E7C7B" stop-opacity="0.25"/>
                    <stop offset="100%" stop-color="#0E7C7B" stop-opacity="0"/>
                  </radialGradient>
                  <radialGradient id="nff-grad-nfs-p" cx="365" cy="335" r="200"
                                  gradientUnits="userSpaceOnUse">
                    <stop offset="0%"  stop-color="#2A6F97" stop-opacity="0.7"/>
                    <stop offset="45%" stop-color="#2A6F97" stop-opacity="0.25"/>
                    <stop offset="100%" stop-color="#2A6F97" stop-opacity="0"/>
                  </radialGradient>
                  <radialGradient id="nff-grad-nac-p" cx="35" cy="335" r="200"
                                  gradientUnits="userSpaceOnUse">
                    <stop offset="0%"  stop-color="#E07A5F" stop-opacity="0.7"/>
                    <stop offset="45%" stop-color="#E07A5F" stop-opacity="0.25"/>
                    <stop offset="100%" stop-color="#E07A5F" stop-opacity="0"/>
                  </radialGradient>
                  <!-- Glow for position marker -->
                  <filter id="nff-marker-glow-p" x="-50%" y="-50%" width="200%" height="200%">
                    <feGaussianBlur in="SourceGraphic" stdDeviation="3"/>
                  </filter>
                </defs>

                <!-- 3-colour gradient: radial fills clipped to triangle -->
                <g clip-path="url(#nff-tri-clip-p)" class="nff-gradient-bg">
                  <!-- Base light fill -->
                  <polygon points="200,35 365,335 35,335"
                           fill="#f0eeeb" opacity="0.5"/>
                  <!-- Three radial gradients overlaid -->
                  <polygon points="200,35 365,335 35,335"
                           fill="url(#nff-grad-nfn-p)"/>
                  <polygon points="200,35 365,335 35,335"
                           fill="url(#nff-grad-nfs-p)"/>
                  <polygon points="200,35 365,335 35,335"
                           fill="url(#nff-grad-nac-p)"/>
                </g>

                <!-- Triangle outline -->
                <polygon class="nff-tri-outline" points="200,35 365,335 35,335"
                         fill="none"/>

                <!-- Vertex glow rings -->
                <circle class="vertex-glow" cx="200" cy="35" r="20"/>
                <circle class="vertex-glow" cx="365" cy="335" r="20"
                        style="animation-delay:1.2s"/>
                <circle class="vertex-glow" cx="35"  cy="335" r="20"
                        style="animation-delay:2.4s"/>

                <!-- Vertices (non-navigating, display only) -->
                <circle class="nff-vertex-static" cx="200" cy="35" r="11"
                        fill="#1B4965" opacity="0.7"/>
                <circle class="nff-vertex-static" cx="365" cy="335" r="11"
                        fill="#1B4965" opacity="0.7"/>
                <circle class="nff-vertex-static" cx="35"  cy="335" r="11"
                        fill="#1B4965" opacity="0.7"/>

                <!-- Vertex labels -->
                <text class="vertex-label" x="200" y="16"
                      text-anchor="middle">Nature for Nature</text>
                <text class="vertex-label" x="365" y="365"
                      text-anchor="middle">Nature for Society</text>
                <text class="vertex-label" x="35" y="365"
                      text-anchor="middle">Nature as Culture</text>

                <!-- Pathway elements will be inserted by JS -->
              </svg>
            ')
          ),
          # Weight readout (populated by JS initTriangle)
          div(
            class = "nff-weight-readout",
            id = ns("nff_pathway_readout")
          )
        ),
        bslib::card_footer(
          class = "text-muted text-center",
          "Dotted line shows governance transition pathway."
        )
      )
    ),

    # ---- Full-width Three Horizons Analysis ----
    bslib::card(
      bslib::card_header(
        class = "bg-primary text-white",
        bsicons::bs_icon("layers"),
        " Three Horizons Analysis"
      ),
      bslib::card_body(
        uiOutput(ns("horizons_analysis"))
      )
    )
  )
}


#' Three Horizons Pathway Visualization module server
#'
#' Manages pathway drawing, timeline animation, Three Horizons classification,
#' and optional integration with the shared nff_weights reactiveVal.
#'
#' @param id Module namespace id
#' @param nff_weights A reactiveVal holding named numeric c(NfN, NfS, NaC),
#'   or NULL if not connected to the shared state.
#' @import shiny
#' @noRd
mod_pathways_server <- function(id, nff_weights = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ---- Narrative preset weights ----
    narrative_weights <- list(
      arcology    = c(NfN = 100, NfS = 0,   NaC = 0),
      sharing     = c(NfN = 50,  NfS = 50,  NaC = 0),
      optimizing  = c(NfN = 0,   NfS = 100, NaC = 0),
      commons     = c(NfN = 0,   NfS = 50,  NaC = 50),
      stewardship = c(NfN = 0,   NfS = 0,   NaC = 100),
      dynamic     = c(NfN = 50,  NfS = 0,   NaC = 50),
      balanced    = c(NfN = 34,  NfS = 33,  NaC = 33)
    )

    # ---- Reactive: future position ----
    future_pos <- reactive({
      preset <- input$future_preset %||% "custom"
      if (preset == "custom") {
        c(
          NfN = input$future_nfn %||% 40,
          NfS = input$future_nfs %||% 30,
          NaC = input$future_nac %||% 30
        )
      } else {
        narrative_weights[[preset]] %||% c(NfN = 34, NfS = 33, NaC = 33)
      }
    })

    # ---- Reactive: current (now) position ----
    now_pos <- reactive({
      c(
        NfN = input$now_nfn %||% 20,
        NfS = input$now_nfs %||% 60,
        NaC = input$now_nac %||% 20
      )
    })

    # ---- Draw pathway observer ----
    observeEvent(input$draw_pathway, {
      now <- now_pos()
      future <- future_pos()

      # Normalize to 0-1 fractions
      now_total <- sum(now)
      future_total <- sum(future)

      # Guard against zero sums
      if (now_total == 0) now_total <- 1
      if (future_total == 0) future_total <- 1

      session$sendCustomMessage("pathway-draw", list(
        now_NfN    = now[["NfN"]] / now_total,
        now_NfS    = now[["NfS"]] / now_total,
        now_NaC    = now[["NaC"]] / now_total,
        future_NfN = future[["NfN"]] / future_total,
        future_NfS = future[["NfS"]] / future_total,
        future_NaC = future[["NaC"]] / future_total
      ))

      showNotification(
        paste0(
          "Pathway drawn: Now (",
          round(now[["NfN"]]), "/",
          round(now[["NfS"]]), "/",
          round(now[["NaC"]]),
          ") \u2192 Future (",
          round(future[["NfN"]]), "/",
          round(future[["NfS"]]), "/",
          round(future[["NaC"]]), ")"
        ),
        type = "message"
      )
    })

    # ---- Timeline animation observer ----
    observe({
      req(input$timeline)
      progress <- (input$timeline - 2025) / 25
      progress <- max(0, min(1, progress))
      session$sendCustomMessage("pathway-animate", list(
        progress = progress
      ))
    })

    # ---- Clear pathways observer ----
    observeEvent(input$clear_pathways, {
      session$sendCustomMessage("pathway-clear", list())
      showNotification("All pathways cleared.", type = "message")
    })

    # ---- Three Horizons Analysis panel ----
    output$horizons_analysis <- renderUI({
      now <- now_pos()
      future <- future_pos()

      # Normalize to percentages (ensure sums are handled)
      now_total <- sum(now)
      future_total <- sum(future)
      if (now_total == 0) now_total <- 1
      if (future_total == 0) future_total <- 1

      now_pct <- now / now_total * 100
      future_pct <- future / future_total * 100

      delta_nfn <- future_pct[["NfN"]] - now_pct[["NfN"]]
      delta_nfs <- future_pct[["NfS"]] - now_pct[["NfS"]]
      delta_nac <- future_pct[["NaC"]] - now_pct[["NaC"]]

      # Classification function
      classify_horizon <- function(delta) {
        if (delta > 10) {
          list(
            horizon = "H3 Emerging",
            class   = "text-success",
            icon    = "arrow-up-circle",
            badge_bg = "#0E7C7B"
          )
        } else if (delta < -10) {
          list(
            horizon = "H1 Declining",
            class   = "text-danger",
            icon    = "arrow-down-circle",
            badge_bg = "#E07A5F"
          )
        } else {
          list(
            horizon = "H2 Transitioning",
            class   = "text-warning",
            icon    = "arrow-left-right",
            badge_bg = "#2A6F97"
          )
        }
      }

      h_nfn <- classify_horizon(delta_nfn)
      h_nfs <- classify_horizon(delta_nfs)
      h_nac <- classify_horizon(delta_nac)

      # Policy implication generator
      policy_text <- function(perspective, delta, horizon_label) {
        direction <- if (delta > 10) {
          "increasing"
        } else if (delta < -10) {
          "decreasing"
        } else {
          "relatively stable"
        }

        if (perspective == "Nature for Nature") {
          if (direction == "increasing") {
            paste0(
              "The intrinsic value of biodiversity is gaining prominence in governance. ",
              "This suggests expanding marine protected areas, strengthening no-take zones, ",
              "and prioritizing ecosystem recovery over extractive activities."
            )
          } else if (direction == "decreasing") {
            paste0(
              "Conservation-centred governance is declining, indicating a shift away from ",
              "strict preservation. Policy should ensure minimum ecological safeguards remain ",
              "even as governance priorities shift toward utilitarian or relational values."
            )
          } else {
            paste0(
              "Conservation priorities remain steady through the transition period. ",
              "Existing marine protected area commitments are likely maintained, but ",
              "transformative biodiversity gains may require more ambitious targets."
            )
          }
        } else if (perspective == "Nature for Society") {
          if (direction == "increasing") {
            paste0(
              "Instrumental values and ecosystem services are becoming more central. ",
              "This pathway favours sustainable blue economy initiatives, MSY-based fisheries ",
              "management, and economic valuation of marine natural capital."
            )
          } else if (direction == "decreasing") {
            paste0(
              "The utilitarian perspective is receding, potentially indicating a move toward ",
              "more holistic governance. Transition plans should address economic dependencies ",
              "on marine resources and ensure livelihoods are not abruptly disrupted."
            )
          } else {
            paste0(
              "Ecosystem service provision remains a stable governance priority. ",
              "Current fisheries management and blue economy frameworks are likely to persist, ",
              "with incremental rather than transformative adjustments."
            )
          }
        } else {
          # Nature as Culture
          if (direction == "increasing") {
            paste0(
              "Relational and cultural values are emerging as governance priorities. ",
              "This pathway supports recognition of indigenous and local community rights, ",
              "cultural seascape protections, and biocultural diversity programmes."
            )
          } else if (direction == "decreasing") {
            paste0(
              "Cultural and relational values are losing ground in governance frameworks. ",
              "There is a risk of marginalizing traditional knowledge holders. Policy should ",
              "safeguard participatory rights and cultural heritage in marine governance."
            )
          } else {
            paste0(
              "Cultural-relational governance priorities remain in equilibrium. ",
              "Community-based management and traditional ecological knowledge are neither ",
              "expanding nor contracting, suggesting institutional stability in this domain."
            )
          }
        }
      }

      # Build a single horizon column
      make_horizon_col <- function(perspective, delta, h_info) {
        sign_str <- if (delta >= 0) paste0("+", round(delta, 1)) else round(delta, 1)

        div(
          class = "text-center",
          # Icon + name
          div(
            class = paste("mb-2", h_info$class),
            bsicons::bs_icon(h_info$icon, size = "2rem")
          ),
          tags$h6(class = "mb-2", perspective),
          # Delta badge
          tags$span(
            class = "badge rounded-pill mb-2",
            style = paste0(
              "background-color: ", h_info$badge_bg, "; ",
              "font-size: 0.85rem;"
            ),
            paste0(sign_str, "%")
          ),
          # Horizon classification
          tags$p(
            class = paste("fw-bold small mb-2", h_info$class),
            h_info$horizon
          ),
          # Policy implication
          tags$p(
            class = "small text-start",
            style = "line-height: 1.65;",
            policy_text(perspective, delta, h_info$horizon)
          )
        )
      }

      tagList(
        bslib::layout_column_wrap(
          width = 1/3,
          make_horizon_col("Nature for Nature", delta_nfn, h_nfn),
          make_horizon_col("Nature for Society", delta_nfs, h_nfs),
          make_horizon_col("Nature as Culture", delta_nac, h_nac)
        ),
        tags$hr(),
        tags$p(
          class = "text-muted small mb-0",
          "The Three Horizons Framework (Sharpe et al., 2016) describes transitions as ",
          "interplay between a declining dominant system (H1), emerging innovations (H3), ",
          "and the transitional space in between (H2). Applied here to NFF governance ",
          "perspectives following Pereira et al. (2020).",
          tags$br(),
          tags$em(
            "Source: Pereira, L.M. et al. (2020). Developing multiscale and integrative ",
            "nature-people scenarios using the Nature Futures Framework. ",
            "People and Nature, 2(4), 1172\u20131195."
          )
        )
      )
    })
  })
}
