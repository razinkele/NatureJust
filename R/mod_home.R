#' Home module UI
#'
#' @param id Module namespace id
#' @noRd
mod_home_ui <- function(id) {
  ns <- NS(id)

  tagList(
    div(
      class = "home-hero",
      h1("NatureJust-EU"),
      p(class = "subtitle",
        "A Decision-Support Tool for Equitable Biodiversity Governance"),
      p(class = "text-muted",
        "Integrating the Nature Futures Framework, Global Biodiversity Framework,",
        "and environmental justice principles for marine spatial planning.")
    ),

    bslib::layout_column_wrap(
      width = 1/2,

      # NFF Triangle — interactive blurred-circle gradient
      bslib::card(
        bslib::card_header(
          class = "bg-primary text-white",
          "Nature Futures Framework"
        ),
        bslib::card_body(
          class = "nff-triangle-container",
          div(
            id = ns("nff_widget"),
            class = "nff-triangle-widget",
            `data-input-id` = ns("nff_position"),
            HTML('
              <svg class="nff-svg" viewBox="-50 0 500 400"
                   xmlns="http://www.w3.org/2000/svg" role="img"
                   aria-label="Interactive Nature Futures Framework triangle">
                <defs>
                  <!-- Clip to triangle boundary -->
                  <clipPath id="nff-tri-clip-h">
                    <polygon points="200,35 365,335 35,335"/>
                  </clipPath>
                  <!-- Radial gradients from each vertex -->
                  <radialGradient id="nff-grad-nfn-h" cx="200" cy="35" r="200"
                                  gradientUnits="userSpaceOnUse">
                    <stop offset="0%"  stop-color="#0E7C7B" stop-opacity="0.7"/>
                    <stop offset="45%" stop-color="#0E7C7B" stop-opacity="0.25"/>
                    <stop offset="100%" stop-color="#0E7C7B" stop-opacity="0"/>
                  </radialGradient>
                  <radialGradient id="nff-grad-nfs-h" cx="365" cy="335" r="200"
                                  gradientUnits="userSpaceOnUse">
                    <stop offset="0%"  stop-color="#2A6F97" stop-opacity="0.7"/>
                    <stop offset="45%" stop-color="#2A6F97" stop-opacity="0.25"/>
                    <stop offset="100%" stop-color="#2A6F97" stop-opacity="0"/>
                  </radialGradient>
                  <radialGradient id="nff-grad-nac-h" cx="35" cy="335" r="200"
                                  gradientUnits="userSpaceOnUse">
                    <stop offset="0%"  stop-color="#E07A5F" stop-opacity="0.7"/>
                    <stop offset="45%" stop-color="#E07A5F" stop-opacity="0.25"/>
                    <stop offset="100%" stop-color="#E07A5F" stop-opacity="0"/>
                  </radialGradient>
                </defs>

                <!-- 3-colour gradient: radial fills clipped to triangle -->
                <g clip-path="url(#nff-tri-clip-h)" class="nff-gradient-bg">
                  <!-- Base light fill -->
                  <polygon points="200,35 365,335 35,335"
                           fill="#f0eeeb" opacity="0.5"/>
                  <!-- Three radial gradients overlaid -->
                  <polygon points="200,35 365,335 35,335"
                           fill="url(#nff-grad-nfn-h)"/>
                  <polygon points="200,35 365,335 35,335"
                           fill="url(#nff-grad-nfs-h)"/>
                  <polygon points="200,35 365,335 35,335"
                           fill="url(#nff-grad-nac-h)"/>
                </g>

                <!-- Triangle outline -->
                <polygon class="nff-tri-outline" points="200,35 365,335 35,335"
                         fill="none"/>

                <!-- Edge relationship labels -->
                <text class="edge-label" x="270" y="193"
                      text-anchor="middle" transform="rotate(61,270,193)">
                  Biodiversity &amp; Livelihoods</text>
                <text class="edge-label" x="130" y="193"
                      text-anchor="middle" transform="rotate(-61,130,193)">
                  Cultural &amp; Ecological</text>
                <text class="edge-label" x="200" y="320"
                      text-anchor="middle">Social &amp; Cultural Values</text>

                <!-- 6 Narrative markers (Dur\u00e1n et al. 2023) -->
                <!-- Corner narratives: offset 30px toward centroid -->
                <polygon class="nff-narrative-marker" data-narrative="arcology"
                         points="0,-7 7,0 0,7 -7,0" transform="translate(200,65)"/>
                <circle class="nff-narrative-hit" data-narrative="arcology"
                        cx="200" cy="65" r="14" fill="transparent"
                        tabindex="0" role="button" aria-label="Arcology narrative"/>

                <polygon class="nff-narrative-marker" data-narrative="optimizing"
                         points="0,-7 7,0 0,7 -7,0" transform="translate(339,320)"/>
                <circle class="nff-narrative-hit" data-narrative="optimizing"
                        cx="339" cy="320" r="14" fill="transparent"
                        tabindex="0" role="button" aria-label="Optimizing Nature narrative"/>

                <polygon class="nff-narrative-marker" data-narrative="stewardship"
                         points="0,-7 7,0 0,7 -7,0" transform="translate(61,320)"/>
                <circle class="nff-narrative-hit" data-narrative="stewardship"
                        cx="61" cy="320" r="14" fill="transparent"
                        tabindex="0" role="button" aria-label="Reciprocal Stewardship narrative"/>

                <!-- Edge narratives: at midpoints -->
                <polygon class="nff-narrative-marker" data-narrative="sharing"
                         points="0,-7 7,0 0,7 -7,0" transform="translate(282.5,185)"/>
                <circle class="nff-narrative-hit" data-narrative="sharing"
                        cx="282.5" cy="185" r="14" fill="transparent"
                        tabindex="0" role="button" aria-label="Sharing through Sparing narrative"/>

                <polygon class="nff-narrative-marker" data-narrative="commons"
                         points="0,-7 7,0 0,7 -7,0" transform="translate(200,335)"/>
                <circle class="nff-narrative-hit" data-narrative="commons"
                        cx="200" cy="335" r="14" fill="transparent"
                        tabindex="0" role="button" aria-label="Innovative Commons narrative"/>

                <polygon class="nff-narrative-marker" data-narrative="dynamic"
                         points="0,-7 7,0 0,7 -7,0" transform="translate(117.5,185)"/>
                <circle class="nff-narrative-hit" data-narrative="dynamic"
                        cx="117.5" cy="185" r="14" fill="transparent"
                        tabindex="0" role="button" aria-label="Dynamic Natures narrative"/>

                <!-- Vertex glow rings -->
                <circle class="vertex-glow" cx="200" cy="35" r="20"/>
                <circle class="vertex-glow" cx="365" cy="335" r="20"
                        style="animation-delay:1.2s"/>
                <circle class="vertex-glow" cx="35"  cy="335" r="20"
                        style="animation-delay:2.4s"/>

                <!-- Vertices (clickable \u2192 navigate to module) -->
                <circle class="nff-vertex" data-target="Spatial Equity"
                        cx="200" cy="35" r="11"
                        tabindex="0" role="button"
                        aria-label="Nature for Nature - navigate to Spatial Equity"/>
                <circle class="nff-vertex" data-target="Scenarios"
                        cx="365" cy="335" r="11"
                        tabindex="0" role="button"
                        aria-label="Nature for Society - navigate to Scenarios"/>
                <circle class="nff-vertex" data-target="Justice"
                        cx="35"  cy="335" r="11"
                        tabindex="0" role="button"
                        aria-label="Nature as Culture - navigate to Justice"/>

                <!-- Vertex labels -->
                <text class="vertex-label" x="200" y="16"
                      text-anchor="middle">Nature for Nature</text>
                <text class="vertex-label" x="365" y="365"
                      text-anchor="middle">Nature for Society</text>
                <text class="vertex-label" x="35" y="365"
                      text-anchor="middle">Nature as Culture</text>

                <!-- Position marker + ring inserted by JS -->
              </svg>
            ')
          ),
          # Weight readout (populated by JS)
          div(
            class = "nff-weight-readout",
            id = ns("nff_readout")
          )
        ),
        bslib::card_footer(
          class = "text-muted text-center",
          "Click inside the triangle to set governance priorities,",
          "or click a vertex to navigate to its module"
        )
      ),

      # Framework explanation
      bslib::card(
        bslib::card_header(
          class = "bg-primary text-white",
          "How It Works"
        ),
        bslib::card_body(
          bslib::accordion(
            id = ns("framework_accordion"),
            open = TRUE,
            multiple = TRUE,
            bslib::accordion_panel(
              "Nature Futures Framework (NFF)",
              icon = bsicons::bs_icon("tree"),
              p("The NFF organises biodiversity futures along three value",
                "perspectives: Nature for Nature (intrinsic), Nature for Society",
                "(instrumental), and Nature as Culture (relational)."),
              p("The triangle visualises these as a ", tags$strong("continuous state space"),
                "\u2014 any position inside represents a unique blend of all three",
                "perspectives. Six ", tags$strong("illustrative narratives"),
                "(Dur\u00e1n et al. 2023) mark key positions: three at the vertices",
                "and three at the edges, each with distinct marine governance",
                "implications."),
              p(class = "text-muted mb-0",
                tags$small("Click a diamond marker to explore a narrative,",
                  "or click anywhere inside to set custom priorities."))
            ),
            bslib::accordion_panel(
              "Global Biodiversity Framework (GBF)",
              icon = bsicons::bs_icon("globe"),
              p("The Kunming-Montreal GBF sets 23 targets for 2030, including",
                "the 30x30 target to protect 30% of land and sea areas.",
                "NatureJust-EU tracks compliance and equity implications.")
            ),
            bslib::accordion_panel(
              "Environmental Justice",
              icon = bsicons::bs_icon("briefcase"),
              p("Four dimensions of justice guide assessment: distributional",
                "(fair sharing of costs/benefits), procedural (inclusive",
                "decision-making), recognitional (diverse knowledge systems),",
                "and restorative (remedying historical harms).")
            ),
            bslib::accordion_panel(
              "Sources",
              icon = bsicons::bs_icon("journal-text"),
              p(class = "text-muted mb-0",
                tags$small(
                  "Adapted from Pereira et al. (2020) ",
                  tags$em("Developing multiscale and integrating nature-people scenarios"),
                  " and Dur\u00e1n et al. (2023) ",
                  tags$em("Illustrative narratives for nature futures."),
                  " See also IPBES (2025) ",
                  tags$em("NFF Methodological Guidance"), "."
                ))
            )
          )
        )
      )
    )
  )
}

#' Home module server
#'
#' @param id Module namespace id
#' @noRd
mod_home_server <- function(id, nff_weights = NULL) {
  moduleServer(id, function(input, output, session) {
    # Receive NFF position from JS triangle interaction
    observeEvent(input$nff_position, {
      pos <- input$nff_position
      if (!is.null(pos) && !is.null(nff_weights)) {
        nff_weights(c(
          NfN = as.integer(pos$NfN),
          NfS = as.integer(pos$NfS),
          NaC = as.integer(pos$NaC)
        ))
      }
    })

    # If external code changes nff_weights, update the JS triangle
    observe({
      if (is.null(nff_weights)) return()
      w <- nff_weights()
      session$sendCustomMessage("nff-update-position", list(
        NfN = w[["NfN"]], NfS = w[["NfS"]], NaC = w[["NaC"]]
      ))
    })
  })
}
