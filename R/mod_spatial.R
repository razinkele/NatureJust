#' Spatial Equity module UI
#' @param id Module namespace id
#' @noRd
mod_spatial_ui <- function(id) {
  ns <- NS(id)

  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      title = "Filters",
      width = 280,
      shinyWidgets::pickerInput(
        ns("country"), "Country",
        choices = NULL,
        multiple = TRUE,
        options = shinyWidgets::pickerOptions(
          actionsBox = TRUE,
          liveSearch = TRUE,
          selectedTextFormat = "count > 3"
        )
      ),
      selectInput(
        ns("sea_basin"), "Sea Basin",
        choices = c("All", "Baltic", "North Sea", "Atlantic",
                    "Mediterranean", "Black Sea", "Inland")
      ),
      selectInput(
        ns("ecosystem"), "Ecosystem Type",
        choices = c("All", "Coastal", "Pelagic", "Deep-sea",
                    "Estuarine", "Reef", "Inland")
      ),
      hr(),
      h6("NFF Composite"),
      shinyWidgets::prettyCheckbox(
        ns("show_nff_composite"), "NFF-Weighted Equity Index",
        value = TRUE, status = "success"
      ),
      hr(),
      h6("Map Layers"),
      shinyWidgets::prettyCheckbox(
        ns("show_mpa"), "Show MPAs",
        value = TRUE, status = "primary"
      ),
      shinyWidgets::prettyCheckbox(
        ns("show_vulnerability"), "Vulnerability Index",
        value = TRUE, status = "warning"
      ),
      shinyWidgets::prettyCheckbox(
        ns("show_fisheries"), "Fisheries Dependency",
        value = FALSE, status = "info"
      ),
      shinyWidgets::prettyCheckbox(
        ns("show_poverty"), "Poverty Rate",
        value = FALSE, status = "danger"
      ),
      shinyWidgets::prettyCheckbox(
        ns("show_income"), "Income Disparity",
        value = FALSE, status = "success"
      ),
      hr(),
      h6("Blue Economy"),
      shinyWidgets::prettyCheckbox(
        ns("show_offshore_wind"), "Offshore Wind",
        value = FALSE, status = "info"
      ),
      shinyWidgets::prettyCheckbox(
        ns("show_coastal_tourism"), "Coastal Tourism",
        value = FALSE, status = "warning"
      ),
      shinyWidgets::prettyCheckbox(
        ns("show_shipping"), "Shipping",
        value = FALSE, status = "danger"
      ),
      shinyWidgets::prettyCheckbox(
        ns("show_aquaculture"), "Aquaculture",
        value = FALSE, status = "primary"
      ),
      shinyWidgets::prettyCheckbox(
        ns("show_bathing"), "Bathing Water",
        value = FALSE, status = "success"
      ),
      shinyWidgets::prettyCheckbox(
        ns("show_blue_jobs"), "Blue Economy Jobs",
        value = FALSE, status = "info"
      )
    ),

    bslib::layout_column_wrap(
      width = 1,
      heights_equal = "row",

      # Map
      bslib::card(
        full_screen = TRUE,
        bslib::card_header("Spatial Equity Map"),
        bslib::card_body(
          padding = 0,
          leaflet::leafletOutput(ns("map"), height = "500px")
        )
      ),

      # Overlap analysis
      bslib::card(
        bslib::card_header("Overlap Analysis: Vulnerability vs Population Pressure"),
        bslib::card_body(
          plotly::plotlyOutput(ns("overlap_plot"), height = "350px")
        )
      )
    )
  )
}

#' Spatial Equity module server
#' @param id Module namespace id
#' @noRd
mod_spatial_server <- function(id, nff_weights = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Gate: prevent leafletProxy calls before the map is rendered.
    # Leaflet sets input$map_bounds once the map widget initialises in the DOM,
    # which only happens when the Spatial Equity tab becomes visible.
    map_ready <- reactiveVal(FALSE)
    observeEvent(input$map_bounds, { map_ready(TRUE) }, once = TRUE)

    # Load full data once (cached)
    all_data <- reactive({ load_nuts2_data() })
    mpas <- reactive({ load_mpa_data() })

    # Filtered data (responds to filter inputs)
    regions <- reactive({
      data <- all_data()
      if (!is.null(input$country) && length(input$country) > 0) {
        data <- data[data$sovereignt %in% input$country, ]
      }
      if (!is.null(input$sea_basin) && input$sea_basin != "All") {
        data <- data[data$sea_basin == input$sea_basin, ]
      }
      if (!is.null(input$ecosystem) && input$ecosystem != "All") {
        data <- data[data$ecosystem_type == input$ecosystem, ]
      }
      data
    })

    # NFF-weighted composite equity index
    composite_data <- reactive({
      data <- regions()
      if (nrow(data) == 0 || is.null(nff_weights)) return(data)
      w <- nff_weights()
      nfn <- w[["NfN"]] / 100
      nfs <- w[["NfS"]] / 100
      nac <- w[["NaC"]] / 100

      # NfN indicators: ecological/conservation
      nfn_score <- rowMeans(data.frame(
        data$vulnerability,
        data$mpa_coverage,
        data$bathing_quality
      ), na.rm = TRUE)

      # NfS indicators: economic/instrumental
      nfs_score <- rowMeans(data.frame(
        data$blue_economy_jobs,
        data$offshore_wind,
        data$fisheries_dep
      ), na.rm = TRUE)

      # NaC indicators: relational/cultural
      nac_score <- rowMeans(data.frame(
        data$coastal_tourism,
        data$aquaculture,
        1 - data$income_disparity
      ), na.rm = TRUE)

      data$nff_composite <- nfn * nfn_score + nfs * nfs_score + nac * nac_score
      data$nff_composite <- round(data$nff_composite, 3)
      data
    })

    # Update country choices from data
    observe({
      data <- all_data()
      countries <- sort(unique(data$sovereignt))
      shinyWidgets::updatePickerInput(
        session, "country",
        choices = countries
      )
    })

    # Render base map ONCE
    output$map <- leaflet::renderLeaflet({
      leaflet::leaflet() |>
        leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) |>
        leaflet::setView(lng = 15, lat = 50, zoom = 4)
    })

    # Layer definitions: checkbox id -> column, palette, group name
    layer_defs <- list(
      list(input_id = "show_vulnerability", col = "vulnerability",
           pal_name = "YlOrRd", group = "Vulnerability"),
      list(input_id = "show_fisheries", col = "fisheries_dep",
           pal_name = "Blues", group = "Fisheries"),
      list(input_id = "show_poverty", col = "poverty_rate",
           pal_name = "Purples", group = "Poverty Rate"),
      list(input_id = "show_income", col = "income_disparity",
           pal_name = "Oranges", group = "Income Disparity"),
      list(input_id = "show_offshore_wind", col = "offshore_wind",
           pal_name = "Greens", group = "Offshore Wind"),
      list(input_id = "show_coastal_tourism", col = "coastal_tourism",
           pal_name = "YlOrBr", group = "Coastal Tourism"),
      list(input_id = "show_shipping", col = "shipping_intensity",
           pal_name = "Reds", group = "Shipping"),
      list(input_id = "show_aquaculture", col = "aquaculture",
           pal_name = "BuGn", group = "Aquaculture"),
      list(input_id = "show_bathing", col = "bathing_quality",
           pal_name = "BuPu", group = "Bathing Water"),
      list(input_id = "show_blue_jobs", col = "blue_economy_jobs",
           pal_name = "PuBu", group = "Blue Economy Jobs")
    )

    all_groups <- c("NFF Composite", vapply(layer_defs, `[[`, "", "group"), "MPAs")

    # Helper: add a single indicator layer to the proxy
    add_indicator_layer <- function(proxy, data, ldef) {
      pal <- leaflet::colorNumeric(ldef$pal_name, domain = c(0, 1))
      col_vals <- data[[ldef$col]]
      label_text <- paste0(
        region_display_name(data), " (", data$sovereignt, "): ",
        ldef$group, " ", round(col_vals, 2)
      )
      proxy |>
        leaflet::addPolygons(
          data = data, fillColor = pal(col_vals), fillOpacity = 0.6,
          weight = 1, color = "#666", label = label_text, group = ldef$group
        ) |>
        leaflet::addLegend(
          pal = pal, values = col_vals,
          title = ldef$group, position = "bottomright",
          layerId = paste0("legend_", ldef$group)
        )
    }

    # Per-layer observers — only clear & redraw the toggled group.
    # map_ready() in the trigger ensures the observer re-fires once the map
    # is actually rendered (tab visible) and prevents leafletProxy spam.
    lapply(layer_defs, function(ldef) {
      observeEvent(list(input[[ldef$input_id]], composite_data(), map_ready()), {
        if (!map_ready()) return()
        data <- composite_data()
        proxy <- leaflet::leafletProxy(ns("map")) |>
          leaflet::clearGroup(ldef$group) |>
          leaflet::removeControl(paste0("legend_", ldef$group))
        if (nrow(data) == 0) return()
        if (!isTRUE(input[[ldef$input_id]])) return()
        if (!ldef$col %in% names(data)) return()
        add_indicator_layer(proxy, data, ldef)
      }, ignoreInit = FALSE)
    })

    # NFF composite layer observer.
    # Guard: nff_weights is NULL when module is used standalone (tests, etc.).
    if (!is.null(nff_weights)) {
      observeEvent(list(input$show_nff_composite, composite_data(), nff_weights(), map_ready()), {
        if (!map_ready()) return()
        data <- composite_data()
        proxy <- leaflet::leafletProxy(ns("map")) |>
          leaflet::clearGroup("NFF Composite") |>
          leaflet::removeControl("legend_NFF Composite")
        if (nrow(data) == 0) return()
        if (!isTRUE(input$show_nff_composite) || !"nff_composite" %in% names(data)) return()

        pal_nff <- leaflet::colorNumeric(
          palette = c("#E07A5F", "#F2CC8F", "#0E7C7B"), domain = c(0, 1)
        )
        w <- nff_weights()
        proxy |>
          leaflet::addPolygons(
            data = data, fillColor = pal_nff(data$nff_composite),
            fillOpacity = 0.7, weight = 1, color = "#333",
            label = paste0(
              region_display_name(data),
              " \u2014 NFF Equity: ", round(data$nff_composite, 2),
              " (NfN=", w[["NfN"]], "%",
              " NfS=", w[["NfS"]], "%",
              " NaC=", w[["NaC"]], "%)"),
            group = "NFF Composite"
          ) |>
          leaflet::addLegend(
            position = "bottomright",
            pal = pal_nff, values = data$nff_composite,
            title = "NFF Equity Index",
            layerId = "legend_NFF Composite"
          )
      }, ignoreInit = FALSE)
    }

    # MPA layer observer
    observeEvent(list(input$show_mpa, mpas(), map_ready()), {
      if (!map_ready()) return()
      proxy <- leaflet::leafletProxy(ns("map")) |>
        leaflet::clearGroup("MPAs")
      if (!isTRUE(input$show_mpa)) return()
      mpa_data <- mpas()
      if (nrow(mpa_data) == 0) return()
      proxy |>
        leaflet::addPolygons(
          data = mpa_data, fillColor = "#41ae76", fillOpacity = 0.3,
          weight = 1, color = "#2c7fb8",
          label = ~paste0(name, " (", designation, ")"),
          group = "MPAs"
        )
    }, ignoreInit = FALSE)

    # Overlap scatter plot
    output$overlap_plot <- plotly::renderPlotly({
      data <- regions()
      if (nrow(data) == 0) return(plotly::plotly_empty())

      df <- sf::st_drop_geometry(data)

      df$region_label <- region_display_name(df)

      # Equity hotspot rectangle
      hotspot_rect <- list(
        type = "rect", x0 = 0.6, x1 = 1, y0 = 0.6, y1 = 1,
        fillcolor = "rgba(255,0,0,0.1)", line = list(width = 0)
      )
      hotspot_label <- list(
        x = 0.8, y = 0.95, text = "Equity\nHotspots",
        showarrow = FALSE, font = list(color = "red", size = 11)
      )

      p <- plotly::plot_ly(
        data = df, x = ~population_pressure, y = ~vulnerability,
        color = ~sea_basin, type = "scatter", mode = "markers",
        text = ~paste0(region_label, " (", sovereignt, ")\nBasin: ", sea_basin),
        hoverinfo = "text",
        marker = list(size = 10, opacity = 0.7)
      ) |> plotly::layout(
        xaxis = list(title = "Population Pressure"),
        yaxis = list(title = "Socio-economic Vulnerability"),
        shapes = list(hotspot_rect),
        annotations = list(hotspot_label)
      )
      p
    })
  })
}
