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
mod_spatial_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

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

    all_groups <- c(vapply(layer_defs, `[[`, "", "group"), "MPAs")

    # Update layers via proxy when filters or checkboxes change
    observe({
      data <- regions()
      proxy <- leaflet::leafletProxy(ns("map"))

      # Clear all overlay groups and legends
      proxy <- proxy |>
        leaflet::clearGroup(all_groups) |>
        leaflet::clearControls()

      if (nrow(data) == 0) return()

      # Add each checked indicator layer
      for (ldef in layer_defs) {
        if (!isTRUE(input[[ldef$input_id]])) next
        if (!ldef$col %in% names(data)) next

        pal <- leaflet::colorNumeric(ldef$pal_name, domain = c(0, 1))
        col_vals <- data[[ldef$col]]

        proxy <- proxy |>
          leaflet::addPolygons(
            data = data,
            fillColor = pal(col_vals),
            fillOpacity = 0.6,
            weight = 1,
            color = "#666",
            label = ~paste0(
                           ifelse(!is.null(data$NUTS_NAME) & !is.na(data$NUTS_NAME),
                                  data$NUTS_NAME, sovereignt),
                           " (", sovereignt, "): ", ldef$group, " ",
                           round(col_vals, 2)),
            group = ldef$group
          )

        # Add legend for the first active layer only (avoid clutter)
        proxy <- proxy |>
          leaflet::addLegend(
            pal = pal, values = col_vals,
            title = ldef$group, position = "bottomright",
            layerId = paste0("legend_", ldef$group)
          )
      }

      # MPA layer
      if (isTRUE(input$show_mpa)) {
        mpa_data <- mpas()
        proxy <- proxy |>
          leaflet::addPolygons(
            data = mpa_data,
            fillColor = "#41ae76",
            fillOpacity = 0.3,
            weight = 1,
            color = "#2c7fb8",
            label = ~paste0(name, " (", designation, ")"),
            group = "MPAs"
          )
      }
    })

    # Overlap scatter plot
    output$overlap_plot <- plotly::renderPlotly({
      data <- regions()
      if (nrow(data) == 0) return(plotly::plotly_empty())

      df <- sf::st_drop_geometry(data)

      # Use NUTS_NAME if available, fallback to sovereignt
      df$region_label <- if ("NUTS_NAME" %in% names(df)) {
        ifelse(is.na(df$NUTS_NAME), df$sovereignt, df$NUTS_NAME)
      } else {
        df$sovereignt
      }

      p <- ggplot2::ggplot(df, ggplot2::aes(
        x = population_pressure,
        y = vulnerability,
        color = sea_basin,
        text = paste0(region_label, " (", sovereignt, ")\nBasin: ", sea_basin)
      )) +
        ggplot2::geom_point(size = 3, alpha = 0.7) +
        ggplot2::geom_smooth(method = "lm", se = FALSE, color = "grey40",
                             linetype = "dashed") +
        ggplot2::labs(
          x = "Population Pressure",
          y = "Socio-economic Vulnerability",
          color = "Sea Basin"
        ) +
        ggplot2::theme_minimal() +
        ggplot2::annotate(
          "rect", xmin = 0.6, xmax = 1, ymin = 0.6, ymax = 1,
          alpha = 0.1, fill = "red"
        ) +
        ggplot2::annotate(
          "text", x = 0.8, y = 0.95, label = "Equity\nHotspots",
          size = 3, color = "red"
        )

      plotly::ggplotly(p, tooltip = "text")
    })
  })
}
