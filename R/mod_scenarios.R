#' Scenarios module UI
#' @param id Module namespace id
#' @noRd
mod_scenarios_ui <- function(id) {
  ns <- NS(id)

  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      title = "Scenario Settings",
      width = 300,
      h6("NFF Perspective Weights"),
      p(class = "text-muted small",
        "Adjust sliders to set your preferred balance. Values are normalised to 100%."),
      sliderInput(ns("nfn"), "Nature for Nature",
                  min = 0, max = 100, value = 34, step = 1),
      sliderInput(ns("nfs"), "Nature for Society",
                  min = 0, max = 100, value = 33, step = 1),
      sliderInput(ns("nac"), "Nature as Culture",
                  min = 0, max = 100, value = 33, step = 1),
      verbatimTextOutput(ns("weight_sum")),
      hr(),
      selectInput(ns("region"), "Region",
                  choices = c("Baltic", "North Sea", "Atlantic",
                              "Mediterranean", "Black Sea")),
      selectInput(ns("horizon"), "Time Horizon",
                  choices = c("2030", "2040", "2050"),
                  selected = "2050"),
      hr(),
      actionButton(ns("save_scenario"), "Save Scenario",
                   class = "btn-primary w-100",
                   icon = icon("plus")),
      actionButton(ns("clear_scenarios"), "Clear All",
                   class = "btn-outline-secondary w-100 mt-2",
                   icon = icon("trash"))
    ),

    bslib::navset_card_tab(
      title = "Scenario Results",

      bslib::nav_panel(
        "Projections",
        bslib::layout_column_wrap(
          width = 1/2,
          bslib::card(
            bslib::card_header("Indicator Projections"),
            plotly::plotlyOutput(ns("projection_plot"), height = "400px")
          ),
          bslib::card(
            bslib::card_header("GBF Target Compliance"),
            uiOutput(ns("gbf_compliance"))
          )
        )
      ),

      bslib::nav_panel(
        "Compare Scenarios",
        bslib::layout_column_wrap(
          width = 1/2,
          bslib::card(
            bslib::card_header("Radar Comparison (Ecological)"),
            plotly::plotlyOutput(ns("radar_plot"), height = "400px")
          ),
          bslib::card(
            bslib::card_header("Justice Dimensions"),
            plotly::plotlyOutput(ns("bar_plot"), height = "400px")
          )
        )
      )
    )
  )
}

#' Scenarios module server
#' @param id Module namespace id
#' @noRd
mod_scenarios_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    # Normalised weights
    weights <- reactive({
      total <- input$nfn + input$nfs + input$nac
      if (total == 0) total <- 1
      c(NfN = round(input$nfn / total * 100),
        NfS = round(input$nfs / total * 100),
        NaC = round(input$nac / total * 100))
    })

    output$weight_sum <- renderText({
      w <- weights()
      paste0("Normalised: NfN=", w["NfN"], "% NfS=", w["NfS"],
             "% NaC=", w["NaC"], "%")
    })

    # Current scenario data
    current_data <- reactive({
      mock_scenario_data(
        nff_weights = weights(),
        region = input$region
      )
    })

    # Saved scenarios (up to 4)
    saved <- reactiveVal(list())

    observeEvent(input$save_scenario, {
      current <- saved()
      if (length(current) >= 4) {
        showNotification("Maximum 4 scenarios. Clear to add more.",
                         type = "warning")
        return()
      }
      w <- weights()
      label <- paste0("S", length(current) + 1, ": ",
                       w["NfN"], "/", w["NfS"], "/", w["NaC"])
      current[[label]] <- current_data()
      saved(current)
      showNotification(paste("Saved:", label), type = "message")
    })

    observeEvent(input$clear_scenarios, {
      saved(list())
      showNotification("All scenarios cleared", type = "message")
    })

    # Projection plot (built directly in plotly for clean legend)
    output$projection_plot <- plotly::renderPlotly({
      data <- current_data()
      horizon_year <- as.integer(input$horizon)
      data <- data[data$year <= horizon_year, ]

      indicator_colors <- c(
          "Habitat Condition" = "#2c7fb8",
          "Ecosystem Services" = "#41ae76",
          "Livelihoods & Employment" = "#f0ad4e",
          "Equity Score" = "#d9534f",
          "Offshore Wind Capacity" = "#17becf",
          "Bathing Water Quality" = "#9467bd"
      )
      has_bands <- all(c("lower", "upper") %in% names(data))

      p <- plotly::plot_ly()
      for (ind in names(indicator_colors)) {
        d <- data[data$indicator == ind, ]
        if (nrow(d) == 0) next
        col <- indicator_colors[[ind]]

        # Confidence band (no legend entry)
        if (has_bands) {
          p <- p |> plotly::add_ribbons(
            data = d, x = ~year, ymin = ~lower, ymax = ~upper,
            line = list(color = "transparent"),
            fillcolor = paste0("rgba(", paste(col2rgb(col), collapse = ","), ",0.15)"),
            showlegend = FALSE, hoverinfo = "skip"
          )
        }

        # Line + markers (single legend entry)
        p <- p |> plotly::add_trace(
          data = d, x = ~year, y = ~value,
          type = "scatter", mode = "lines+markers",
          line = list(color = col, width = 2),
          marker = list(color = col, size = 4),
          name = ind
        )
      }

      p |> plotly::layout(
        xaxis = list(title = "Year"),
        yaxis = list(title = "Index Value"),
        legend = list(title = list(text = "Indicator"))
      )
    })

    # GBF compliance
    output$gbf_compliance <- renderUI({
      w <- weights()
      set.seed(sum(w))
      targets <- data.frame(
        target = paste("Target", c(3, 8, 10, 14, 22)),
        description = c(
          "30x30 Protected Areas",
          "Climate Change Adaptation",
          "Sustainable Agriculture/Fisheries",
          "Mainstreaming Biodiversity",
          "Inclusive Decision-Making"
        ),
        status = sample(c("green", "amber", "red"), 5, replace = TRUE,
                        prob = c(w["NfN"]/100, 0.3, 1 - w["NfN"]/100))
      )

      tagList(
        lapply(seq_len(nrow(targets)), function(i) {
          div(
            class = "d-flex align-items-center mb-2",
            tags$span(class = paste("traffic-light", targets$status[i])),
            tags$strong(targets$target[i]),
            tags$span(class = "ms-2 text-muted", targets$description[i])
          )
        })
      )
    })

    # Radar comparison plot
    output$radar_plot <- plotly::renderPlotly({
      scenarios <- saved()
      if (length(scenarios) == 0) {
        return(plotly::plotly_empty(type = "scatterpolar") |>
                 plotly::layout(title = "Save scenarios to compare"))
      }

      categories <- c("Habitat Condition", "Ecosystem Services",
                       "Livelihoods & Employment", "Equity Score",
                       "Offshore Wind Capacity", "Bathing Water Quality")

      p <- plotly::plot_ly(type = "scatterpolar", fill = "toself")

      for (nm in names(scenarios)) {
        df <- scenarios[[nm]]
        end_vals <- df |>
          dplyr::filter(year == max(year)) |>
          dplyr::pull(value)
        # Normalise to 0-1 range for radar
        end_vals <- pmin(pmax(end_vals, 0), 1)

        p <- p |> plotly::add_trace(
          r = c(end_vals, end_vals[1]),
          theta = c(categories, categories[1]),
          name = nm
        )
      }

      p |> plotly::layout(
        polar = list(radialaxis = list(visible = TRUE, range = c(0, 1)))
      )
    })

    # Stacked bar comparison
    output$bar_plot <- plotly::renderPlotly({
      scenarios <- saved()
      if (length(scenarios) == 0) {
        return(plotly::plotly_empty() |>
                 plotly::layout(title = "Save scenarios to compare"))
      }

      df_list <- lapply(names(scenarios), function(nm) {
        d <- scenarios[[nm]]
        d |>
          dplyr::filter(year == max(year)) |>
          dplyr::mutate(scenario = nm)
      })

      df <- do.call(rbind, df_list)

      p <- ggplot2::ggplot(df, ggplot2::aes(
        x = scenario, y = value, fill = indicator
      )) +
        ggplot2::geom_col(position = "dodge") +
        ggplot2::theme_minimal() +
        ggplot2::labs(x = "Scenario", y = "Final Value", fill = "Dimension") +
        ggplot2::coord_flip()

      plotly::ggplotly(p)
    })
  })
}
