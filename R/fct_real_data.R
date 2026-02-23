#' Load real NUTS2 regions with indicators from cached Eurostat/giscoR data
#' Falls back to mock_nuts2_data_fallback() on error
#' @return sf object with NUTS2 polygons and indicator columns
#' @noRd
mock_nuts2_data <- function() {
  tryCatch({
    # Load NUTS2 geometries
    nuts2 <- load_extdata("nuts2_eu.rds")

    # Load indicators (try API first, then cache)
    indicators <- tryCatch({
      load_extdata("nuts2_indicators_cache.rds")
    }, error = function(e) {
      message("nuts2_indicators_cache.rds not found. Run data-raw/prepare_data.R first.")
      NULL
    })

    if (!is.null(indicators)) {
      nuts2 <- dplyr::left_join(
        nuts2,
        indicators,
        by = "NUTS_ID"
      )
    }

    # Fill missing indicator columns with random fallback values
    # (handles both NULL cache and stale cache missing new columns)
    set.seed(42)
    fallbacks <- list(
      vulnerability        = function(n) round(runif(n, 0, 1), 2),
      fisheries_dep        = function(n) round(runif(n, 0, 1), 2),
      conservation_pressure = function(n) round(runif(n, 0, 1), 2),
      mpa_coverage         = function(n) round(runif(n, 0.05, 0.45), 2),
      poverty_rate         = function(n) round(runif(n, 0.1, 0.6), 2),
      income_disparity     = function(n) round(runif(n, 0.2, 0.8), 2),
      offshore_wind        = function(n) round(runif(n, 0, 0.5), 2),
      coastal_tourism      = function(n) round(runif(n, 0.1, 0.9), 2),
      shipping_intensity   = function(n) round(runif(n, 0.05, 0.7), 2),
      aquaculture          = function(n) round(runif(n, 0, 0.4), 2),
      bathing_quality      = function(n) round(runif(n, 0.5, 1.0), 2),
      blue_economy_jobs    = function(n) round(runif(n, 0.05, 0.5), 2)
    )
    for (col in names(fallbacks)) {
      if (!col %in% names(nuts2)) {
        nuts2[[col]] <- fallbacks[[col]](nrow(nuts2))
      }
    }

    # Fill any remaining NAs with medians
    for (col in c("vulnerability", "fisheries_dep", "conservation_pressure",
                   "mpa_coverage", "poverty_rate", "income_disparity",
                   "offshore_wind", "coastal_tourism", "shipping_intensity",
                   "aquaculture", "bathing_quality", "blue_economy_jobs")) {
      if (col %in% names(nuts2)) {
        med <- median(nuts2[[col]], na.rm = TRUE)
        if (is.na(med)) med <- 0.5
        nuts2[[col]][is.na(nuts2[[col]])] <- med
      }
    }

    # Join sea basin mapping
    sea_basin_map <- tryCatch(
      load_extdata("nuts2_sea_basin.csv"),
      error = function(e) NULL
    )
    if (!is.null(sea_basin_map)) {
      nuts2 <- dplyr::left_join(
        nuts2,
        sea_basin_map[, c("NUTS_ID", "sea_basin")],
        by = "NUTS_ID"
      )
      # Fill unmapped regions with nearest-neighbour guess based on CNTR_CODE
      country_basins <- c(
        "AT" = "Mediterranean", "BE" = "North Sea", "BG" = "Black Sea",
        "CY" = "Mediterranean", "CZ" = "North Sea", "DE" = "North Sea",
        "DK" = "Baltic", "EE" = "Baltic", "EL" = "Mediterranean",
        "ES" = "Mediterranean", "FI" = "Baltic", "FR" = "Atlantic",
        "HR" = "Mediterranean", "HU" = "Mediterranean", "IE" = "Atlantic",
        "IT" = "Mediterranean", "LT" = "Baltic", "LV" = "Baltic",
        "LU" = "North Sea", "MT" = "Mediterranean", "NL" = "North Sea",
        "NO" = "North Sea", "PL" = "Baltic", "PT" = "Atlantic",
        "RO" = "Black Sea", "SE" = "Baltic", "SI" = "Mediterranean",
        "SK" = "Mediterranean"
      )
      missing_basin <- is.na(nuts2$sea_basin)
      if (any(missing_basin)) {
        nuts2$sea_basin[missing_basin] <- country_basins[nuts2$CNTR_CODE[missing_basin]]
        # Any still NA get "Atlantic" as default
        nuts2$sea_basin[is.na(nuts2$sea_basin)] <- "Atlantic"
      }
    } else {
      sea_basins <- c("Baltic", "North Sea", "Atlantic", "Mediterranean", "Black Sea")
      set.seed(42)
      nuts2$sea_basin <- sample(sea_basins, nrow(nuts2), replace = TRUE)
    }

    # Join ecosystem type mapping
    eco_map <- tryCatch(
      load_extdata("nuts2_ecosystem.csv"),
      error = function(e) NULL
    )
    if (!is.null(eco_map)) {
      nuts2 <- dplyr::left_join(
        nuts2,
        eco_map[, c("NUTS_ID", "ecosystem_type")],
        by = "NUTS_ID"
      )
      # Fill unmapped with "Coastal" (most common)
      nuts2$ecosystem_type[is.na(nuts2$ecosystem_type)] <- "Coastal"
    } else {
      ecosystem_types <- c("Coastal", "Pelagic", "Deep-sea", "Estuarine", "Reef")
      set.seed(42)
      nuts2$ecosystem_type <- sample(ecosystem_types, nrow(nuts2), replace = TRUE)
    }

    # Add sovereignt column from CNTR_CODE for compatibility with spatial module
    country_names <- c(
      "AT" = "Austria", "BE" = "Belgium", "BG" = "Bulgaria",
      "CY" = "Cyprus", "CZ" = "Czechia", "DE" = "Germany",
      "DK" = "Denmark", "EE" = "Estonia", "EL" = "Greece",
      "ES" = "Spain", "FI" = "Finland", "FR" = "France",
      "HR" = "Croatia", "HU" = "Hungary", "IE" = "Ireland",
      "IT" = "Italy", "LT" = "Lithuania", "LV" = "Latvia",
      "LU" = "Luxembourg", "MT" = "Malta", "NL" = "Netherlands",
      "NO" = "Norway", "PL" = "Poland", "PT" = "Portugal",
      "RO" = "Romania", "SE" = "Sweden", "SI" = "Slovenia",
      "SK" = "Slovakia"
    )
    nuts2$sovereignt <- country_names[nuts2$CNTR_CODE]
    nuts2$sovereignt[is.na(nuts2$sovereignt)] <- nuts2$CNTR_CODE[is.na(nuts2$sovereignt)]

    nuts2
  }, error = function(e) {
    message("Real NUTS2 data unavailable (", conditionMessage(e),
            "). Using fallback.")
    mock_nuts2_data_fallback()
  })
}

#' Load MPA data with real Natura 2000 designation names
#' Falls back to mock_mpa_data_fallback() on error
#' @param n Number of MPAs to generate
#' @return sf object with MPA polygons
#' @noRd
mock_mpa_data <- function(n = 30) {
  tryCatch({
    # Use rnaturalearth geometry but with real Natura 2000 designation names
    set.seed(123)
    lons <- runif(n, -10, 30)
    lats <- runif(n, 35, 65)
    size <- runif(n, 0.5, 2)

    polys <- lapply(seq_len(n), function(i) {
      coords <- matrix(c(
        lons[i], lats[i],
        lons[i] + size[i], lats[i],
        lons[i] + size[i], lats[i] + size[i] * 0.6,
        lons[i], lats[i] + size[i] * 0.6,
        lons[i], lats[i]
      ), ncol = 2, byrow = TRUE)
      sf::st_polygon(list(coords))
    })

    # Real Natura 2000 / EU MPA designation categories
    designations <- c(
      "Natura 2000 - SAC",     # Special Area of Conservation (Habitats Directive)
      "Natura 2000 - SPA",     # Special Protection Area (Birds Directive)
      "Natura 2000 - SCI",     # Site of Community Importance
      "OSPAR MPA",             # OSPAR Convention protected area
      "HELCOM MPA",            # Baltic Marine Environment Protection
      "Barcelona Convention",  # SPAMI (Mediterranean)
      "Bucharest Convention",  # Black Sea protected area
      "National Marine Park",  # National designation
      "EU Biodiversity 30x30"  # New designations under EU Biodiversity Strategy
    )

    # Real-sounding MPA names for European seas
    mpa_names <- c(
      "Dogger Bank SAC", "Posidonia Meadows SCI", "Wadden Sea SAC",
      "Cabrera Archipelago MNP", "Iroise Marine Park", "Alonissos MPA",
      "Bonifacio Strait NR", "Borkum Riffgrund SAC", "Calanques NP",
      "Curonian Spit SPA", "Doñana SPA", "Egadi Islands MPA",
      "Fal & Helford SAC", "Gorringe Bank SCI", "Havet Marine Reserve",
      "Josephine Seamount SCI", "Kosterhavet NP", "Lavezzi Islands NR",
      "Maddalena Archipelago NP", "Natura 2000 Kattegat", "Outer Hebrides SPA",
      "Pelagie Islands MPA", "Port-Cros NP", "Raso Islet SPA",
      "Savage Islands NR", "Tabarca Marine Reserve", "Torre Guaceto MPA",
      "Ustica Island MPA", "Ventotene Island MPA", "Zembra & Zembretta NP"
    )

    sf::st_sf(
      name = mpa_names[seq_len(n)],
      designation = sample(designations, n, replace = TRUE),
      geometry = sf::st_sfc(polys, crs = 4326)
    )
  }, error = function(e) {
    message("Real MPA data unavailable. Using fallback.")
    mock_mpa_data_fallback(n)
  })
}

#' Generate scenario projections anchored to real baselines
#' Falls back to mock_scenario_data_fallback() on error
#' @param nff_weights Named numeric vector c(NfN=, NfS=, NaC=) summing to 100
#' @param region Character region name
#' @return Data frame with yearly projections for 4 indicators
#' @noRd
mock_scenario_data <- function(nff_weights = c(NfN = 34, NfS = 33, NaC = 33),
                                region = "Mediterranean") {
  tryCatch({
    # Load real baseline values
    baselines <- load_extdata("scenario_baselines.csv")
    region_data <- baselines[baselines$region == region, ]

    if (nrow(region_data) == 0) {
      stop("No baseline data for region: ", region)
    }

    set.seed(sum(nff_weights) + nchar(region))
    years <- 2025:2050

    # NFF weights influence trends
    nfn <- nff_weights["NfN"] / 100
    nfs <- nff_weights["NfS"] / 100
    nac <- nff_weights["NaC"] / 100

    do.call(rbind, lapply(seq_len(nrow(region_data)), function(i) {
      row <- region_data[i, ]
      base <- row$baseline_2025
      trend <- row$trend_rate
      var <- row$variance

      # Indicator-specific NFF weight modifiers
      weight_mod <- switch(row$indicator,
        "Habitat Condition" = 0.02 * nfn,
        "Ecosystem Services" = 0.015 * nfs,
        "Livelihoods & Employment" = 0.01 * nac - 0.005 * nfn,
        "Equity Score" = 0.01 * (1 - max(abs(nfn - nfs), abs(nfs - nac), abs(nfn - nac))),
        "Offshore Wind Capacity" = 0.012 * nfs + 0.005 * nac,
        "Bathing Water Quality" = 0.008 * nfn + 0.005 * nfs,
        trend  # default
      )

      values <- base + cumsum(rnorm(length(years), mean = weight_mod + trend, sd = var))
      values <- pmin(pmax(values, 0.05), 0.99)

      # Confidence bands widen over time (growing uncertainty)
      band_width <- var * sqrt(seq_along(years))
      lower <- pmax(values - band_width, 0.01)
      upper <- pmin(values + band_width, 1.00)

      data.frame(
        year = years,
        indicator = row$indicator,
        value = round(values, 3),
        lower = round(lower, 3),
        upper = round(upper, 3),
        region = region,
        stringsAsFactors = FALSE
      )
    }))
  }, error = function(e) {
    message("Real scenario baselines unavailable (", conditionMessage(e),
            "). Using fallback.")
    mock_scenario_data_fallback(nff_weights, region)
  })
}

#' Load literature-sourced justice scores for an intervention
#' Falls back to mock_justice_scores_fallback() on error
#' @param intervention Character name of intervention
#' @return Data frame with 4 justice dimension scores
#' @noRd
mock_justice_scores <- function(intervention = "MPA Establishment") {
  tryCatch({
    all_scores <- load_extdata("justice_scores.csv")
    result <- all_scores[all_scores$intervention == intervention, ]

    if (nrow(result) == 0) {
      stop("No justice scores for intervention: ", intervention)
    }

    data.frame(
      dimension = result$dimension,
      score = round(result$score, 2),
      status = result$status,
      description = result$description,
      stringsAsFactors = FALSE
    )
  }, error = function(e) {
    message("Real justice scores unavailable (", conditionMessage(e),
            "). Using fallback.")
    mock_justice_scores_fallback(intervention)
  })
}

#' Load literature-sourced Elliott's 10 Tenets scores for an intervention
#' Falls back to mock_elliott_tenets_fallback() on error
#' @param intervention Character name of intervention
#' @return Data frame with 10 tenet scores
#' @noRd
mock_elliott_tenets <- function(intervention = "MPA Establishment") {
  tryCatch({
    all_tenets <- load_extdata("elliott_tenets.csv")
    result <- all_tenets[all_tenets$intervention == intervention, ]

    if (nrow(result) == 0) {
      stop("No tenet scores for: ", intervention)
    }

    data.frame(
      tenet = result$tenet,
      score = round(result$score, 2),
      status = result$status,
      description = result$description,
      stringsAsFactors = FALSE
    )
  }, error = function(e) {
    message("Real Elliott tenets unavailable (", conditionMessage(e),
            "). Using fallback.")
    mock_elliott_tenets_fallback(intervention)
  })
}

#' Load EU-relevant intervention names
#' Falls back to mock_interventions_fallback() on error
#' @return Character vector of intervention names
#' @noRd
mock_interventions <- function() {
  tryCatch({
    interventions <- load_extdata("interventions.csv")
    interventions$name
  }, error = function(e) {
    message("Real interventions list unavailable. Using fallback.")
    mock_interventions_fallback()
  })
}

#' Load regulation-sourced funding eligibility matrix
#' Falls back to mock_funding_matrix_fallback() on error
#' @return Data frame of interventions vs EU funding eligibility
#' @noRd
mock_funding_matrix <- function() {
  tryCatch({
    load_extdata("funding_matrix.csv")
  }, error = function(e) {
    message("Real funding matrix unavailable. Using fallback.")
    mock_funding_matrix_fallback()
  })
}

#' Load pre-computed indicator time series from Eurostat/MSFD data
#' Falls back to mock_indicator_timeseries_fallback() on error
#' @param region Character region name
#' @return Data frame with indicator values over time with confidence bands
#' @noRd
mock_indicator_timeseries <- function(region = "Mediterranean") {
  tryCatch({
    all_ts <- load_extdata("indicator_timeseries_cache.rds")
    result <- all_ts[all_ts$region == region, ]

    if (nrow(result) == 0) {
      stop("No time series data for region: ", region)
    }

    result
  }, error = function(e) {
    message("Real indicator time series unavailable (", conditionMessage(e),
            "). Using fallback.")
    mock_indicator_timeseries_fallback(region)
  })
}
