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
                    "Mediterranean", "Black Sea")
      ),
      selectInput(
        ns("ecosystem"), "Ecosystem Type",
        choices = c("All", "Coastal", "Pelagic", "Deep-sea",
                    "Estuarine", "Reef")
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
        bslib::card_header("Overlap Analysis: Vulnerability vs Conservation Pressure"),
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
    # Load mock data
    regions <- reactive({
      data <- mock_nuts2_data()

      # Apply filters
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

    mpas <- reactive({ mock_mpa_data() })

    # Update country choices from data
    observe({
      data <- mock_nuts2_data()
      countries <- sort(unique(data$sovereignt))
      shinyWidgets::updatePickerInput(
        session, "country",
        choices = countries
      )
    })

    # Leaflet map
    output$map <- leaflet::renderLeaflet({
      data <- regions()

      pal_vuln <- leaflet::colorNumeric("YlOrRd", domain = c(0, 1))
      pal_fish <- leaflet::colorNumeric("Blues", domain = c(0, 1))

      m <- leaflet::leaflet() |>
        leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) |>
        leaflet::setView(lng = 15, lat = 50, zoom = 4)

      if (isTRUE(input$show_vulnerability) && nrow(data) > 0) {
        m <- m |>
          leaflet::addPolygons(
            data = data,
            fillColor = ~pal_vuln(vulnerability),
            fillOpacity = 0.6,
            weight = 1,
            color = "#666",
            label = ~paste0(sovereignt, ": Vulnerability ", vulnerability),
            group = "Vulnerability"
          ) |>
          leaflet::addLegend(
            pal = pal_vuln, values = data$vulnerability,
            title = "Vulnerability", position = "bottomright"
          )
      }

      if (isTRUE(input$show_fisheries) && nrow(data) > 0) {
        m <- m |>
          leaflet::addPolygons(
            data = data,
            fillColor = ~pal_fish(fisheries_dep),
            fillOpacity = 0.5,
            weight = 1,
            color = "#333",
            label = ~paste0(sovereignt, ": Fisheries Dep. ", fisheries_dep),
            group = "Fisheries"
          )
      }

      if (isTRUE(input$show_poverty) && nrow(data) > 0 &&
          "poverty_rate" %in% names(data)) {
        pal_pov <- leaflet::colorNumeric("Purples", domain = c(0, 1))
        m <- m |>
          leaflet::addPolygons(
            data = data,
            fillColor = ~pal_pov(poverty_rate),
            fillOpacity = 0.5,
            weight = 1,
            color = "#555",
            label = ~paste0(sovereignt, ": Poverty Rate ", poverty_rate),
            group = "Poverty Rate"
          )
      }

      if (isTRUE(input$show_income) && nrow(data) > 0 &&
          "income_disparity" %in% names(data)) {
        pal_inc <- leaflet::colorNumeric("Oranges", domain = c(0, 1))
        m <- m |>
          leaflet::addPolygons(
            data = data,
            fillColor = ~pal_inc(income_disparity),
            fillOpacity = 0.5,
            weight = 1,
            color = "#555",
            label = ~paste0(sovereignt, ": Income Disparity ", income_disparity),
            group = "Income Disparity"
          )
      }

      if (isTRUE(input$show_offshore_wind) && nrow(data) > 0 &&
          "offshore_wind" %in% names(data)) {
        pal_wind <- leaflet::colorNumeric("Greens", domain = c(0, 1))
        m <- m |>
          leaflet::addPolygons(
            data = data,
            fillColor = ~pal_wind(offshore_wind),
            fillOpacity = 0.5,
            weight = 1,
            color = "#555",
            label = ~paste0(sovereignt, ": Offshore Wind ", offshore_wind),
            group = "Offshore Wind"
          )
      }

      if (isTRUE(input$show_coastal_tourism) && nrow(data) > 0 &&
          "coastal_tourism" %in% names(data)) {
        pal_tour <- leaflet::colorNumeric("YlOrBr", domain = c(0, 1))
        m <- m |>
          leaflet::addPolygons(
            data = data,
            fillColor = ~pal_tour(coastal_tourism),
            fillOpacity = 0.5,
            weight = 1,
            color = "#555",
            label = ~paste0(sovereignt, ": Coastal Tourism ", coastal_tourism),
            group = "Coastal Tourism"
          )
      }

      if (isTRUE(input$show_shipping) && nrow(data) > 0 &&
          "shipping_intensity" %in% names(data)) {
        pal_ship <- leaflet::colorNumeric("Reds", domain = c(0, 1))
        m <- m |>
          leaflet::addPolygons(
            data = data,
            fillColor = ~pal_ship(shipping_intensity),
            fillOpacity = 0.5,
            weight = 1,
            color = "#555",
            label = ~paste0(sovereignt, ": Shipping ", shipping_intensity),
            group = "Shipping"
          )
      }

      if (isTRUE(input$show_aquaculture) && nrow(data) > 0 &&
          "aquaculture" %in% names(data)) {
        pal_aqua <- leaflet::colorNumeric("BuGn", domain = c(0, 1))
        m <- m |>
          leaflet::addPolygons(
            data = data,
            fillColor = ~pal_aqua(aquaculture),
            fillOpacity = 0.5,
            weight = 1,
            color = "#555",
            label = ~paste0(sovereignt, ": Aquaculture ", aquaculture),
            group = "Aquaculture"
          )
      }

      if (isTRUE(input$show_bathing) && nrow(data) > 0 &&
          "bathing_quality" %in% names(data)) {
        pal_bath <- leaflet::colorNumeric("BuPu", domain = c(0, 1))
        m <- m |>
          leaflet::addPolygons(
            data = data,
            fillColor = ~pal_bath(bathing_quality),
            fillOpacity = 0.5,
            weight = 1,
            color = "#555",
            label = ~paste0(sovereignt, ": Bathing Quality ", bathing_quality),
            group = "Bathing Water"
          )
      }

      if (isTRUE(input$show_blue_jobs) && nrow(data) > 0 &&
          "blue_economy_jobs" %in% names(data)) {
        pal_blue <- leaflet::colorNumeric("PuBu", domain = c(0, 1))
        m <- m |>
          leaflet::addPolygons(
            data = data,
            fillColor = ~pal_blue(blue_economy_jobs),
            fillOpacity = 0.5,
            weight = 1,
            color = "#555",
            label = ~paste0(sovereignt, ": Blue Economy Jobs ", blue_economy_jobs),
            group = "Blue Economy Jobs"
          )
      }

      if (isTRUE(input$show_mpa)) {
        mpa_data <- mpas()
        m <- m |>
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

      m |>
        leaflet::addLayersControl(
          overlayGroups = c("Vulnerability", "Fisheries", "Poverty Rate",
                           "Income Disparity", "Offshore Wind", "Coastal Tourism",
                           "Shipping", "Aquaculture", "Bathing Water",
                           "Blue Economy Jobs", "MPAs"),
          options = leaflet::layersControlOptions(collapsed = FALSE)
        )
    })

    # Overlap scatter plot
    output$overlap_plot <- plotly::renderPlotly({
      data <- regions()
      if (nrow(data) == 0) return(plotly::plotly_empty())

      df <- sf::st_drop_geometry(data)

      p <- ggplot2::ggplot(df, ggplot2::aes(
        x = conservation_pressure,
        y = vulnerability,
        color = sea_basin,
        text = paste0(sovereignt, "\nBasin: ", sea_basin)
      )) +
        ggplot2::geom_point(size = 3, alpha = 0.7) +
        ggplot2::geom_smooth(method = "lm", se = FALSE, color = "grey40",
                             linetype = "dashed") +
        ggplot2::labs(
          x = "Conservation Pressure",
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
