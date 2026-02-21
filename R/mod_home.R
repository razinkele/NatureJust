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
      heights_equal = "row",

      # NFF Triangle
      bslib::card(
        bslib::card_header(
          class = "bg-primary text-white",
          "Nature Futures Framework"
        ),
        bslib::card_body(
          class = "nff-triangle-container",
          HTML('
            <div class="nff-triangle">
              <svg viewBox="0 0 400 380" xmlns="http://www.w3.org/2000/svg">
                <defs>
                  <linearGradient id="nffGrad" x1="0%" y1="0%" x2="100%" y2="100%">
                    <stop offset="0%" stop-color="#1B4965"/>
                    <stop offset="50%" stop-color="#0E7C7B"/>
                    <stop offset="100%" stop-color="#E07A5F"/>
                  </linearGradient>
                </defs>

                <!-- Gradient fill -->
                <polygon class="triangle-fill" points="200,35 365,335 35,335"/>

                <!-- Triangle stroke -->
                <polygon class="triangle-stroke" points="200,35 365,335 35,335"/>

                <!-- Edge dashes -->
                <line class="edge-dash" x1="200" y1="35" x2="365" y2="335"/>
                <line class="edge-dash" x1="365" y1="335" x2="35" y2="335"/>
                <line class="edge-dash" x1="35" y1="335" x2="200" y2="35"/>

                <!-- Glow rings -->
                <circle class="vertex-glow" cx="200" cy="35" r="20"/>
                <circle class="vertex-glow" cx="365" cy="335" r="20" style="animation-delay:1.2s"/>
                <circle class="vertex-glow" cx="35" cy="335" r="20" style="animation-delay:2.4s"/>

                <!-- Vertices -->
                <circle class="nff-vertex vertex" data-target="Spatial Equity"
                        cx="200" cy="35" r="11"/>
                <circle class="nff-vertex vertex" data-target="Scenarios"
                        cx="365" cy="335" r="11"/>
                <circle class="nff-vertex vertex" data-target="Justice"
                        cx="35" cy="335" r="11"/>

                <!-- Vertex labels -->
                <text class="vertex-label" x="200" y="16"
                      text-anchor="middle">Nature for Nature</text>
                <text class="vertex-label" x="390" y="350"
                      text-anchor="start">Nature for Society</text>
                <text class="vertex-label" x="10" y="350"
                      text-anchor="end">Nature as Culture</text>

                <!-- Edge labels -->
                <text class="edge-label" x="292" y="178"
                      text-anchor="middle" transform="rotate(29,292,178)">
                  Biodiversity &amp; Livelihoods</text>
                <text class="edge-label" x="108" y="178"
                      text-anchor="middle" transform="rotate(-29,108,178)">
                  Cultural &amp; Ecological</text>
                <text class="edge-label" x="200" y="370"
                      text-anchor="middle">Social &amp; Cultural Values</text>
              </svg>
            </div>
          ')
        ),
        bslib::card_footer(
          class = "text-muted text-center",
          "Click a vertex to navigate to the relevant module"
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
            bslib::accordion_panel(
              "Global Biodiversity Framework (GBF)",
              icon = bsicons::bs_icon("globe"),
              p("The Kunming-Montreal GBF sets 23 targets for 2030, including",
                "the 30x30 target to protect 30% of land and sea areas.",
                "NatureJust-EU tracks compliance and equity implications.")
            ),
            bslib::accordion_panel(
              "Nature Futures Framework (NFF)",
              icon = bsicons::bs_icon("tree"),
              p("The NFF organises biodiversity futures along three value",
                "perspectives: Nature for Nature (intrinsic), Nature for Society",
                "(instrumental), and Nature as Culture (relational).",
                "Scenarios explore trade-offs between these perspectives.")
            ),
            bslib::accordion_panel(
              "Environmental Justice",
              icon = bsicons::bs_icon("briefcase"),
              p("Four dimensions of justice guide assessment: distributional",
                "(fair sharing of costs/benefits), procedural (inclusive",
                "decision-making), recognitional (diverse knowledge systems),",
                "and restorative (remedying historical harms).")
            )
          )
        )
      )
    ),

    # Module overview cards
    h4("Explore the Modules", class = "text-center mt-4 mb-3"),
    bslib::layout_column_wrap(
      width = 1/3,
      bslib::value_box(
        title = "Spatial Equity",
        value = "Diagnostic",
        showcase = bsicons::bs_icon("map"),
        theme = "primary",
        p("Map conservation needs against socio-economic vulnerability")
      ),
      bslib::value_box(
        title = "Scenario Explorer",
        value = "NFF Futures",
        showcase = bsicons::bs_icon("sliders"),
        theme = "info",
        p("Compare biodiversity futures under different value perspectives")
      ),
      bslib::value_box(
        title = "Justice Assessment",
        value = "4 Dimensions",
        showcase = bsicons::bs_icon("shield-check"),
        theme = "success",
        p("Evaluate interventions against environmental justice criteria")
      )
    )
  )
}

#' Home module server
#'
#' @param id Module namespace id
#' @param parent_session The parent session for navbar navigation
#' @noRd
mod_home_server <- function(id, parent_session = NULL) {
  moduleServer(id, function(input, output, session) {
    # Navigation from NFF triangle is handled via JS in nff_triangle.js
    # The JS sets the navbar tab directly
  })
}
