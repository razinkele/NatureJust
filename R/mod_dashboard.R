#' Indicator Dashboard module UI
#' @param id Module namespace id
#' @noRd
mod_dashboard_ui <- function(id) {
  ns <- NS(id)

  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      title = "Settings",
      width = 260,
      selectInput(
        ns("region"), "Region",
        choices = c("Baltic", "North Sea", "Atlantic",
                    "Mediterranean", "Black Sea")
      ),
      shinyWidgets::pickerInput(
        ns("indicators"), "Indicators",
        choices = c(
          "Marine Biodiversity Index (M)",
          "Habitat Condition (M)",
          "Ecosystem Services (M)",
          "Community Wellbeing Index (M)",
          "Governance Effectiveness (M)",
          "Fish Stock Biomass",
          "Sustainable Fishing",
          "Offshore Wind Capacity",
          "Coastal Tourism Pressure",
          "Bathing Water Quality"
        ),
        selected = c("Marine Biodiversity Index (M)", "Habitat Condition (M)"),
        multiple = TRUE,
        options = shinyWidgets::pickerOptions(actionsBox = TRUE)
      ),
      hr(),
      downloadButton(ns("download_csv"), "Export CSV",
                     class = "btn-outline-primary w-100")
    ),

    tagList(
      # Time series
      bslib::card(
        full_screen = TRUE,
        bslib::card_header("Indicator Time Series"),
        bslib::card_body(
          plotly::plotlyOutput(ns("timeseries_plot"), height = "400px"),
          p(class = "text-muted small mt-2",
            "Indicators marked (M) are modelled from regional assessment baselines.",
            "Unmarked indicators are derived from Eurostat time series.")
        )
      ),

      bslib::layout_column_wrap(
        width = 1/2,

        # GBF compliance
        bslib::card(
          bslib::card_header("GBF Target Compliance"),
          bslib::card_body(
            DT::dataTableOutput(ns("compliance_table"))
          )
        ),

        # Equity trend
        bslib::card(
          bslib::card_header("Equity Trend Decomposition"),
          bslib::card_body(
            plotly::plotlyOutput(ns("equity_plot"), height = "300px")
          )
        )
      )
    )
  )
}

#' Indicator Dashboard module server
#' @param id Module namespace id
#' @noRd
mod_dashboard_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    data <- reactive({
      mock_indicator_timeseries(input$region)
    })

    filtered_data <- reactive({
      df <- data()
      if (!is.null(input$indicators) && length(input$indicators) > 0) {
        # Strip "(M)" suffix for matching against data indicator names
        selected <- gsub(" \\(M\\)$", "", input$indicators)
        df <- df[df$indicator %in% selected, ]
      }
      df
    })

    # Time series with confidence bands
    output$timeseries_plot <- plotly::renderPlotly({
      df <- filtered_data()
      if (nrow(df) == 0) return(plotly::plotly_empty())

      p <- ggplot2::ggplot(df, ggplot2::aes(x = year, color = indicator,
                                             fill = indicator)) +
        ggplot2::geom_ribbon(ggplot2::aes(ymin = lower, ymax = upper),
                             alpha = 0.15, color = NA) +
        ggplot2::geom_line(ggplot2::aes(y = value), linewidth = 1) +
        ggplot2::geom_point(ggplot2::aes(y = value), size = 1.5) +
        ggplot2::theme_minimal() +
        ggplot2::labs(x = "Year", y = "Index Value",
                      color = "Indicator", fill = "Indicator")

      plotly::ggplotly(p)
    })

    # GBF compliance table
    output$compliance_table <- DT::renderDataTable({
      df <- data()
      latest <- df[df$year == max(df$year), ]

      compliance <- data.frame(
        Indicator = latest$indicator,
        `Current Value` = round(latest$value, 3),
        `GBF Target` = latest$gbf_target,
        Status = ifelse(latest$value >= latest$gbf_target, "Met",
                        ifelse(latest$value >= latest$gbf_target * 0.8,
                               "Partly Met", "Not Met")),
        check.names = FALSE
      )

      DT::datatable(
        compliance,
        options = list(dom = "t", pageLength = 10),
        rownames = FALSE
      ) |>
        DT::formatStyle(
          "Status",
          backgroundColor = DT::styleEqual(
            c("Met", "Partly Met", "Not Met"),
            c("#d4edda", "#fff3cd", "#f8d7da")
          )
        )
    })

    # Equity trend decomposition
    output$equity_plot <- plotly::renderPlotly({
      df <- data()
      eco <- df[grepl("^Habitat Condition", df$indicator), ]
      equity <- df[df$indicator == "Community Wellbeing Index", ]

      if (nrow(eco) == 0 || nrow(equity) == 0) {
        return(plotly::plotly_empty())
      }

      combined <- data.frame(
        year = eco$year,
        ecological = eco$value,
        equity = equity$value
      )

      p <- plotly::plot_ly(combined, x = ~year) |>
        plotly::add_trace(y = ~ecological, name = "Ecological",
                          type = "scatter", mode = "lines+markers",
                          line = list(color = "#2c7fb8")) |>
        plotly::add_trace(y = ~equity, name = "Equity",
                          type = "scatter", mode = "lines+markers",
                          line = list(color = "#41ae76")) |>
        plotly::layout(
          yaxis = list(title = "Index Value"),
          xaxis = list(title = "Year"),
          legend = list(orientation = "h", y = -0.15)
        )

      p
    })

    # CSV export
    output$download_csv <- downloadHandler(
      filename = function() {
        paste0("naturejust-indicators-", input$region, "-",
               Sys.Date(), ".csv")
      },
      content = function(file) {
        utils::write.csv(filtered_data(), file, row.names = FALSE)
      }
    )
  })
}
