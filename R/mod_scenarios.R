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
            bslib::card_header("Indicator Comparison"),
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
      load_scenario_data(
        nff_weights = weights(),
        region = input$region
      )
    })

    # Saved scenarios (up to 4) — store data + horizon
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
      current[[label]] <- list(
        data = current_data(),
        horizon = as.integer(input$horizon)
      )
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
          "Bathing Water Quality" = "#9467bd",
          "Contaminant Status" = "#e377c2",
          "Eutrophication Status" = "#8c564b",
          "Underwater Noise" = "#7f7f7f"
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

    # GBF compliance — compare projected values against gbf_targets.csv thresholds
    output$gbf_compliance <- renderUI({
      data <- current_data()
      horizon_year <- as.integer(input$horizon)

      # Get projected end-values at the chosen horizon
      end_vals <- data[data$year == horizon_year, c("indicator", "value")]

      # Load GBF target thresholds
      gbf <- tryCatch(
        load_extdata("gbf_targets.csv"),
        error = function(e) NULL
      )

      if (is.null(gbf) || nrow(end_vals) == 0) {
        return(div(class = "alert alert-secondary",
                   "GBF targets data unavailable."))
      }

      # Join projections with targets on indicator name
      merged <- merge(end_vals, gbf, by = "indicator", all.x = TRUE)
      merged <- merged[!is.na(merged$gbf_target_value), ]

      if (nrow(merged) == 0) {
        return(div(class = "alert alert-secondary",
                   "No matching GBF targets for current indicators."))
      }

      # Traffic-light status based on gap to target
      merged$gap <- merged$value - merged$gbf_target_value
      merged$status <- ifelse(merged$gap >= 0, "green",
                       ifelse(merged$gap >= -0.10, "amber", "red"))

      tagList(
        p(class = "text-muted small mb-3",
          paste0("Projected values at ", horizon_year,
                 " vs. Kunming-Montreal GBF thresholds.")),
        lapply(seq_len(nrow(merged)), function(i) {
          div(
            class = "d-flex align-items-center mb-2",
            traffic_light(merged$status[i]),
            tags$strong(class = "me-2", merged$indicator[i]),
            tags$span(class = "text-muted small",
              paste0(round(merged$value[i], 2), " / ",
                     merged$gbf_target_value[i], " — ",
                     merged$gbf_target_name[i]))
          )
        })
      )
    })

    # Radar comparison plot — uses saved horizon and explicit indicator matching
    output$radar_plot <- plotly::renderPlotly({
      scenarios <- saved()
      if (length(scenarios) == 0) {
        return(plotly::plotly_empty(type = "scatterpolar") |>
                 plotly::layout(title = "Save scenarios to compare"))
      }

      # Dynamically include HELCOM indicators when data is available
      base_categories <- c("Habitat Condition", "Ecosystem Services",
                           "Livelihoods & Employment", "Equity Score",
                           "Offshore Wind Capacity", "Bathing Water Quality")
      helcom_categories <- c("Contaminant Status", "Eutrophication Status",
                             "Underwater Noise")
      has_helcom <- any(sapply(scenarios, function(sc) {
        any(helcom_categories %in% sc$data$indicator)
      }))
      categories <- if (has_helcom) c(base_categories, helcom_categories) else base_categories

      p <- plotly::plot_ly(type = "scatterpolar", fill = "toself")

      for (nm in names(scenarios)) {
        sc <- scenarios[[nm]]
        df <- sc$data
        horizon_year <- sc$horizon
        end_df <- df[df$year == horizon_year, ]
        # Explicit matching: align values to categories vector
        idx <- match(categories, end_df$indicator)
        end_vals <- end_df$value[idx]
        end_vals[is.na(end_vals)] <- 0
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

    # Bar comparison — uses saved horizon
    output$bar_plot <- plotly::renderPlotly({
      scenarios <- saved()
      if (length(scenarios) == 0) {
        return(plotly::plotly_empty() |>
                 plotly::layout(title = "Save scenarios to compare"))
      }

      df_list <- lapply(names(scenarios), function(nm) {
        sc <- scenarios[[nm]]
        d <- sc$data
        horizon_year <- sc$horizon
        d |>
          dplyr::filter(year == horizon_year) |>
          dplyr::mutate(scenario = nm)
      })

      df <- do.call(rbind, df_list)

      p <- ggplot2::ggplot(df, ggplot2::aes(
        x = scenario, y = value, fill = indicator
      )) +
        ggplot2::geom_col(position = "dodge") +
        ggplot2::theme_minimal() +
        ggplot2::labs(x = "Scenario", y = "Final Value", fill = "Indicator") +
        ggplot2::coord_flip()

      plotly::ggplotly(p)
    })
  })
}
