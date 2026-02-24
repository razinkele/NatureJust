#' In-memory cache for expensive data loads (survives across reactive invalidations)
#' @noRd
.data_cache <- cachem::cache_mem(max_age = 600, max_size = 200 * 1024^2)

#' Load real NUTS2 regions with indicators from cached Eurostat/giscoR data
#' Falls back to mock_nuts2_data_fallback() on error (synthetic fallback)
#' Results are cached in memory for 10 minutes to avoid repeated I/O.
#' @return sf object with NUTS2 polygons and indicator columns
#' @noRd
load_nuts2_data <- function() {
  cached <- .data_cache$get("nuts2")
  if (!cachem::is.key_missing(cached)) return(cached)

  result <- .load_nuts2_data_impl()
  .data_cache$set("nuts2", result)
  result
}

#' Internal implementation for load_nuts2_data (not cached)
#' @noRd
.load_nuts2_data_impl <- function() {
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
      # Migrate old column name from pre-regenerated caches
      if ("conservation_pressure" %in% names(indicators) &&
          !"population_pressure" %in% names(indicators)) {
        names(indicators)[names(indicators) == "conservation_pressure"] <- "population_pressure"
      }
      nuts2 <- dplyr::left_join(
        nuts2,
        indicators,
        by = "NUTS_ID"
      )
    }

    # Fill missing indicator columns with random fallback values
    # (handles both NULL cache and stale cache missing new columns)
    fallbacks <- list(
      vulnerability        = function(n) round(runif(n, 0, 1), 2),
      fisheries_dep        = function(n) round(runif(n, 0, 1), 2),
      population_pressure = function(n) round(runif(n, 0, 1), 2),
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
    withr::with_seed(42, {
      for (col in names(fallbacks)) {
        if (!col %in% names(nuts2)) {
          message("Column '", col, "' missing from cache, using random fallback")
          nuts2[[col]] <- fallbacks[[col]](nrow(nuts2))
        }
      }
    })

    # Fill any remaining NAs with medians
    for (col in c("vulnerability", "fisheries_dep", "population_pressure",
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
        "AT" = "Inland", "BE" = "North Sea", "BG" = "Black Sea",
        "CY" = "Mediterranean", "CZ" = "Inland", "DE" = "North Sea",
        "DK" = "Baltic", "EE" = "Baltic", "EL" = "Mediterranean",
        "ES" = "Mediterranean", "FI" = "Baltic", "FR" = "Atlantic",
        "HR" = "Mediterranean", "HU" = "Inland", "IE" = "Atlantic",
        "IT" = "Mediterranean", "LT" = "Baltic", "LV" = "Baltic",
        "LU" = "Inland", "MT" = "Mediterranean", "NL" = "North Sea",
        "NO" = "North Sea", "PL" = "Baltic", "PT" = "Atlantic",
        "RO" = "Black Sea", "SE" = "Baltic", "SI" = "Mediterranean",
        "SK" = "Inland"
      )
      missing_basin <- is.na(nuts2$sea_basin)
      if (any(missing_basin)) {
        nuts2$sea_basin[missing_basin] <- country_basins[nuts2$CNTR_CODE[missing_basin]]
        # Any still NA get "Inland" as default
        nuts2$sea_basin[is.na(nuts2$sea_basin)] <- "Inland"
      }
    } else {
      sea_basins <- c("Baltic", "North Sea", "Atlantic", "Mediterranean", "Black Sea")
      withr::with_seed(42, {
        nuts2$sea_basin <- sample(sea_basins, nrow(nuts2), replace = TRUE)
      })
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
      # Fill unmapped with "Inland" (landlocked / non-coastal regions)
      nuts2$ecosystem_type[is.na(nuts2$ecosystem_type)] <- "Inland"
    } else {
      ecosystem_types <- c("Coastal", "Pelagic", "Deep-sea", "Estuarine", "Reef")
      withr::with_seed(42, {
        nuts2$ecosystem_type <- sample(ecosystem_types, nrow(nuts2), replace = TRUE)
      })
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

    # Provenance "rds" means loaded from cached RDS file. Individual columns
    # may contain Eurostat real data or rnorm() fallback values depending on
    # which API calls succeeded when prepare_data.R was last run.
    attr(nuts2, "provenance") <- "rds"
    nuts2
  }, error = function(e) {
    message("Real NUTS2 data unavailable (", conditionMessage(e),
            "). Using fallback.")
    result <- mock_nuts2_data_fallback()
    attr(result, "provenance") <- "fallback"
    result
  })
}

#' Load MPA data — real Natura 2000 boundaries when available, else synthetic
#' Loads from natura2000_marine.rds if present (real EEA data).
#' Falls back to mock_mpa_data_fallback() on error (synthetic rectangles).
#' @param n Number of MPAs to generate
#' @return sf object with MPA polygons
#' @noRd
load_mpa_data <- function(n = 30) {
  tryCatch({
    # Load real Natura 2000 marine boundaries from EEA cache
    mpa_sf <- load_extdata("natura2000_marine.rds")

    # Ensure required columns exist
    if (!"name" %in% names(mpa_sf) && "SITENAME" %in% names(mpa_sf)) {
      mpa_sf$name <- mpa_sf$SITENAME
    }
    if (!"designation" %in% names(mpa_sf) && "SITETYPE" %in% names(mpa_sf)) {
      mpa_sf$designation <- dplyr::case_when(
        mpa_sf$SITETYPE == "A" ~ "Natura 2000 - SPA",
        mpa_sf$SITETYPE == "B" ~ "Natura 2000 - SAC/SCI",
        mpa_sf$SITETYPE == "C" ~ "Natura 2000 - SPA + SAC/SCI",
        TRUE ~ paste0("Natura 2000 - ", mpa_sf$SITETYPE)
      )
    }

    # With GEOS-simplified data (~0.8 MB), show all sites on the map.
    # Only sample if dataset is unusually large (un-simplified fallback).
    if (nrow(mpa_sf) > 5000 && n < nrow(mpa_sf)) {
      set.seed(123)
      mpa_sf <- mpa_sf[sample(nrow(mpa_sf), n), ]
    }

    attr(mpa_sf, "provenance") <- "rds"
    mpa_sf
  }, error = function(e) {
    message("Real Natura 2000 MPA data unavailable (", conditionMessage(e),
            "). Using synthetic fallback.")
    result <- mock_mpa_data_fallback(n)
    attr(result, "provenance") <- "fallback"
    result
  })
}

#' Load scenario projections anchored to real baselines
#' Falls back to mock_scenario_data_fallback() on error (synthetic fallback)
#' @details Projections use stochastic modeling (rnorm with cumsum) to simulate
#'   uncertainty in future trajectories. set.seed() ensures reproducibility for
#'   the same inputs. This is intentional — not a fallback for missing data.
#' @param nff_weights Named numeric vector c(NfN=, NfS=, NaC=) summing to 100
#' @param region Character region name
#' @return Data frame with yearly projections for 4 indicators
#' @noRd
load_scenario_data <- function(nff_weights = c(NfN = 34, NfS = 33, NaC = 33),
                                region = "Mediterranean") {
  tryCatch({
    # Load real baseline values
    baselines <- load_extdata("scenario_baselines.csv")
    region_data <- baselines[baselines$region == region, ]

    if (nrow(region_data) == 0) {
      stop("No baseline data for region: ", region)
    }

    # Use all 3 weight components individually to avoid seed collisions
    seed_val <- nff_weights["NfN"] * 10000L + nff_weights["NfS"] * 100L +
                nff_weights["NaC"] + nchar(region)
    years <- 2025:2050

    # NFF weights influence trends
    nfn <- nff_weights["NfN"] / 100
    nfs <- nff_weights["NfS"] / 100
    nac <- nff_weights["NaC"] / 100

    result <- withr::with_seed(seed_val, {
      do.call(rbind, lapply(seq_len(nrow(region_data)), function(i) {
      row <- region_data[i, ]
      base <- row$baseline_2025
      trend <- row$trend_rate
      var <- row$variance

      # Indicator-specific NFF weight modifiers (shared helper)
      weight_mod <- nff_weight_modifier(row$indicator, nfn, nfs, nac)

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
    })  # end withr::with_seed
    attr(result, "provenance") <- "csv"
    result
  }, error = function(e) {
    message("Real scenario baselines unavailable (", conditionMessage(e),
            "). Using fallback.")
    result <- mock_scenario_data_fallback(nff_weights, region)
    attr(result, "provenance") <- "fallback"
    result
  })
}

#' Load literature-sourced justice scores for an intervention
#' Falls back to mock_justice_scores_fallback() on error (synthetic fallback)
#' @param intervention Character name of intervention
#' @return Data frame with 4 justice dimension scores
#' @noRd
load_justice_scores <- function(intervention = "MPA Establishment") {
  tryCatch({
    all_scores <- load_extdata("justice_scores.csv")
    result <- all_scores[all_scores$intervention == intervention, ]

    if (nrow(result) == 0) {
      stop("No justice scores for intervention: ", intervention)
    }

    df <- data.frame(
      dimension = result$dimension,
      score = round(result$score, 2),
      status = result$status,
      description = result$description,
      stringsAsFactors = FALSE
    )
    attr(df, "provenance") <- "csv"
    df
  }, error = function(e) {
    message("Real justice scores unavailable (", conditionMessage(e),
            "). Using fallback.")
    result <- mock_justice_scores_fallback(intervention)
    attr(result, "provenance") <- "fallback"
    result
  })
}

#' Load literature-sourced Elliott's 10 Tenets scores for an intervention
#' Falls back to mock_elliott_tenets_fallback() on error (synthetic fallback)
#' @param intervention Character name of intervention
#' @return Data frame with 10 tenet scores
#' @noRd
load_elliott_tenets <- function(intervention = "MPA Establishment") {
  tryCatch({
    all_tenets <- load_extdata("elliott_tenets.csv")
    result <- all_tenets[all_tenets$intervention == intervention, ]

    if (nrow(result) == 0) {
      stop("No tenet scores for: ", intervention)
    }

    df <- data.frame(
      tenet = result$tenet,
      score = round(result$score, 2),
      status = result$status,
      description = result$description,
      stringsAsFactors = FALSE
    )
    attr(df, "provenance") <- "csv"
    df
  }, error = function(e) {
    message("Real Elliott tenets unavailable (", conditionMessage(e),
            "). Using fallback.")
    result <- mock_elliott_tenets_fallback(intervention)
    attr(result, "provenance") <- "fallback"
    result
  })
}

#' Load EU-relevant intervention names
#' Falls back to mock_interventions_fallback() on error (synthetic fallback)
#' @return Character vector of intervention names
#' @noRd
load_interventions <- function() {
  tryCatch({
    interventions <- load_extdata("interventions.csv")
    result <- interventions$name
    attr(result, "provenance") <- "csv"
    result
  }, error = function(e) {
    message("Real interventions list unavailable. Using fallback.")
    result <- mock_interventions_fallback()
    attr(result, "provenance") <- "fallback"
    result
  })
}

#' Load regulation-sourced funding eligibility matrix
#' Falls back to mock_funding_matrix_fallback() on error (synthetic fallback)
#' @return Data frame of interventions vs EU funding eligibility
#' @noRd
load_funding_matrix <- function() {
  tryCatch({
    result <- load_extdata("funding_matrix.csv")
    attr(result, "provenance") <- "csv"
    result
  }, error = function(e) {
    message("Real funding matrix unavailable. Using fallback.")
    result <- mock_funding_matrix_fallback()
    attr(result, "provenance") <- "fallback"
    result
  })
}

#' Load CFP alignment data for a conservation measure
#' Falls back to mock_cfp_alignment_fallback() on error (synthetic fallback)
#' @param intervention Character name of the intervention
#' @return Data frame with alignment status and detail columns
#' @noRd
load_cfp_alignment <- function(intervention = "MPA Establishment") {
  tryCatch({
    all_cfp <- load_extdata("cfp_alignment.csv")
    result <- all_cfp[all_cfp$intervention == intervention, ]

    if (nrow(result) == 0) {
      stop("No CFP alignment data for intervention: ", intervention)
    }

    attr(result, "provenance") <- "csv"
    result
  }, error = function(e) {
    message("Real CFP alignment data unavailable (", conditionMessage(e),
            "). Using fallback.")
    result <- mock_cfp_alignment_fallback(intervention)
    attr(result, "provenance") <- "fallback"
    result
  })
}

#' Load pre-computed indicator time series from Eurostat/MSFD data
#' Falls back to mock_indicator_timeseries_fallback() on error (synthetic fallback)
#' @param region Character region name
#' @return Data frame with indicator values over time with confidence bands
#' @noRd
load_indicator_timeseries <- function(region = "Mediterranean") {
  tryCatch({
    all_ts <- load_extdata("indicator_timeseries_cache.rds")
    result <- all_ts[all_ts$region == region, ]

    if (nrow(result) == 0) {
      stop("No time series data for region: ", region)
    }

    # HELCOM indicators only for Baltic (consistent with fallback logic)
    helcom_only <- HELCOM_INDICATORS
    if (region != "Baltic") {
      result <- result[!result$indicator %in% helcom_only, ]
    }

    result <- ensure_columns(result,
      c("year", "indicator", "value", "lower", "upper", "region", "gbf_target"))
    attr(result, "provenance") <- "rds"
    result
  }, error = function(e) {
    message("Real indicator time series unavailable (", conditionMessage(e),
            "). Using fallback.")
    result <- mock_indicator_timeseries_fallback(region)
    attr(result, "provenance") <- "fallback"
    result
  })
}

#' Load narrative data (Durán et al. 2023)
#'
#' Returns a data.frame of 6 illustrative NFF narratives with
#' structured governance, policy, and marine example data.
#'
#' @return data.frame with columns: id, name, nff_position, weights,
#'   description, governance_model, key_policies, marine_examples,
#'   trade_offs, gbf_targets_prioritized, source
#' @noRd
load_narratives <- function() {
  tryCatch({
    narr <- load_extdata("narratives.json")
    attr(narr, "provenance") <- "json"
    narr
  }, error = function(e) {
    message("Narrative data unavailable (", conditionMessage(e),
            "). Using minimal fallback.")
    data.frame(
      id = c("arcology", "sharing", "optimizing", "commons", "stewardship", "dynamic"),
      name = c("Arcology", "Sharing through Sparing", "Optimizing Nature",
               "Innovative Commons", "Reciprocal Stewardship", "Dynamic Natures"),
      nff_position = c("Nature for Nature corner", "NfN-NfS edge",
                        "Nature for Society corner", "NfS-NaC edge",
                        "Nature as Culture corner", "NaC-NfN edge"),
      description = rep("Narrative data unavailable. See Duran et al. (2023).", 6),
      governance_model = rep("", 6),
      source = rep("Duran et al. (2023)", 6),
      weights = I(list(
        list(NfN = 100, NfS = 0,   NaC = 0),
        list(NfN = 50,  NfS = 50,  NaC = 0),
        list(NfN = 0,   NfS = 100, NaC = 0),
        list(NfN = 0,   NfS = 50,  NaC = 50),
        list(NfN = 0,   NfS = 0,   NaC = 100),
        list(NfN = 50,  NfS = 0,   NaC = 50)
      )),
      stringsAsFactors = FALSE
    )
  })
}
