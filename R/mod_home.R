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
            HTML(nff_triangle_svg("h", mode = "home"))
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
