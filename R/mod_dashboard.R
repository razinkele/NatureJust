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
          "Bathing Water Quality",
          "Contaminant Status (H)",
          "Eutrophication Status (H)",
          "Underwater Noise (H)"
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
            "Legend: (M) = Modelled from regional assessment baselines. ",
            "(H) = HELCOM HOLAS III assessment (Baltic only). ",
            "Unmarked = Eurostat / ICES SAG / GFCM time series.")
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
      ),

      # Data provenance footer
      uiOutput(ns("provenance_footer"))
    )
  )
}

#' Indicator Dashboard module server
#' @param id Module namespace id
#' @noRd
mod_dashboard_server <- function(id, nff_weights = NULL) {
  moduleServer(id, function(input, output, session) {

    data <- reactive({
      req(input$region)
      load_indicator_timeseries(input$region)
    })

    # Map display names (with provenance suffixes) to data indicator names
    indicator_display_map <- c(
      "Marine Biodiversity Index (M)" = "Marine Biodiversity Index",
      "Habitat Condition (M)"         = "Habitat Condition",
      "Ecosystem Services (M)"        = "Ecosystem Services",
      "Community Wellbeing Index (M)"  = "Community Wellbeing Index",
      "Governance Effectiveness (M)"   = "Governance Effectiveness",
      "Fish Stock Biomass"             = "Fish Stock Biomass",
      "Sustainable Fishing"            = "Sustainable Fishing",
      "Offshore Wind Capacity"         = "Offshore Wind Capacity",
      "Coastal Tourism Pressure"       = "Coastal Tourism Pressure",
      "Bathing Water Quality"          = "Bathing Water Quality",
      "Contaminant Status (H)"         = "Contaminant Status",
      "Eutrophication Status (H)"      = "Eutrophication Status",
      "Underwater Noise (H)"           = "Underwater Noise"
    )

    filtered_data <- reactive({
      df <- data()
      if (!is.null(input$indicators) && length(input$indicators) > 0) {
        selected <- unname(indicator_display_map[input$indicators])
        selected <- selected[!is.na(selected)]
        df <- df[df$indicator %in% selected, ]
      }
      
      # Sort indicators by NFF relevance when weights are available
      # nff_weights is a reactiveVal (a function); call it to get the numeric vector
      if (is.function(nff_weights)) {
        w <- nff_weights()
        inds <- unique(df$indicator)
        scores <- vapply(inds, function(ind) {
          # Divide by 100: nff_weight_modifier expects fractions (0-1), not %
          nff_weight_modifier(ind, w[["NfN"]] / 100, w[["NfS"]] / 100, w[["NaC"]] / 100)
        }, numeric(1))
        sorted_inds <- inds[order(scores, decreasing = TRUE)]
        df$indicator <- factor(df$indicator, levels = sorted_inds)
        df <- df[order(df$indicator, df$year), ]
        df$indicator <- as.character(df$indicator) # drop factor for downstream code
      }
      
      df
    })

    # Time series with confidence bands
    output$timeseries_plot <- plotly::renderPlotly({
      df <- filtered_data()
      if (nrow(df) == 0) return(plotly::plotly_empty())

      # Build native plotly traces per indicator for performance
      indicators <- unique(df$indicator)
      p <- plotly::plot_ly()
      for (ind in indicators) {
        ind_df <- df[df$indicator == ind, ]
        # Confidence ribbon
        p <- p |>
          plotly::add_ribbons(
            data = ind_df, x = ~year, ymin = ~lower, ymax = ~upper,
            name = ind, legendgroup = ind,
            line = list(width = 0),
            fillcolor = plotly::toRGB(ind, alpha = 0.15),
            showlegend = FALSE
          ) |>
          plotly::add_trace(
            data = ind_df, x = ~year, y = ~value,
            name = ind, legendgroup = ind,
            type = "scatter", mode = "lines+markers",
            line = list(width = 2),
            marker = list(size = 5)
          )
      }
      p <- p |> plotly::layout(
        xaxis = list(title = "Year"),
        yaxis = list(title = "Index Value")
      )
      p
    })

    # GBF compliance table
    output$compliance_table <- DT::renderDataTable({
      df <- data()
      latest <- df[df$year == max(df$year), ]

      # Defensive: ensure gbf_target column exists
      if (!"gbf_target" %in% names(latest) || all(is.na(latest$gbf_target))) {
        return(DT::datatable(
          data.frame(Note = "GBF target data unavailable"),
          options = list(dom = "t"), rownames = FALSE
        ))
      }

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

      combined <- base::merge(
        data.frame(year = eco$year, ecological = eco$value),
        data.frame(year = equity$year, equity = equity$value),
        by = "year"
      )
      if (nrow(combined) == 0) return(plotly::plotly_empty())

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

    # Data provenance footer
    output$provenance_footer <- renderUI({
      ts_data <- data()
      prov <- attr(ts_data, "provenance")
      region <- input$region

      if (identical(prov, "fallback")) {
        div(class = "alert alert-warning mt-2 small",
            bsicons::bs_icon("exclamation-triangle"),
            " Data source: Synthetic (cache files missing \u2014 run data-raw/prepare_data.R)")
      } else {
        sources <- "Eurostat"
        if (region %in% c("Baltic", "North Sea", "Atlantic")) {
          sources <- paste(sources, "/ ICES SAG")
        }
        if (region == "Baltic") {
          sources <- paste(sources, "/ HELCOM HOLAS III")
        }
        if (region %in% c("Mediterranean", "Black Sea")) {
          sources <- paste(sources, "/ GFCM")
        }
        div(class = "text-muted small mt-2",
            bsicons::bs_icon("database"),
            paste(" Data sources:", sources))
      }
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
