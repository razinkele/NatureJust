#' Generate mock European NUTS2 regions with vulnerability scores (FALLBACK)
#' @return sf object with NUTS2-like polygons and mock indicators
#' @noRd
mock_nuts2_data_fallback <- function() {
  europe <- rnaturalearth::ne_countries(
    scale = 50,
    continent = "Europe",
    returnclass = "sf"
  )
  europe <- europe[!europe$sovereignt %in% c("Russia", "Iceland"), ]
  europe <- sf::st_transform(europe, 4326)

  set.seed(42)
  europe$vulnerability <- round(runif(nrow(europe), 0, 1), 2)
  europe$fisheries_dep <- round(runif(nrow(europe), 0, 1), 2)
  europe$population_pressure <- round(runif(nrow(europe), 0, 1), 2)
  europe$mpa_coverage <- round(runif(nrow(europe), 0.05, 0.45), 2)
  europe$poverty_rate <- round(runif(nrow(europe), 0.1, 0.6), 2)
  europe$income_disparity <- round(runif(nrow(europe), 0.2, 0.8), 2)
  europe$offshore_wind <- round(runif(nrow(europe), 0, 0.5), 2)
  europe$coastal_tourism <- round(runif(nrow(europe), 0.1, 0.9), 2)
  europe$shipping_intensity <- round(runif(nrow(europe), 0.05, 0.7), 2)
  europe$aquaculture <- round(runif(nrow(europe), 0, 0.4), 2)
  europe$bathing_quality <- round(runif(nrow(europe), 0.5, 1.0), 2)
  europe$blue_economy_jobs <- round(runif(nrow(europe), 0.05, 0.5), 2)

  # Schema alignment with real NUTS2 data (ensure NUTS_ID and NUTS_NAME exist)
  if (!"NUTS_ID" %in% names(europe)) {
    europe$NUTS_ID <- paste0(europe$iso_a2, "00")
  }
  if (!"NUTS_NAME" %in% names(europe)) {
    europe$NUTS_NAME <- europe$name
  }

  sea_basins <- c("Baltic", "North Sea", "Atlantic", "Mediterranean", "Black Sea")
  europe$sea_basin <- sample(sea_basins, nrow(europe), replace = TRUE)

  ecosystem_types <- c("Coastal", "Pelagic", "Deep-sea", "Estuarine", "Reef")
  europe$ecosystem_type <- sample(ecosystem_types, nrow(europe), replace = TRUE)

  europe
}

#' Generate synthetic MPA polygons (FALLBACK)
#' Synthetic fallback — real data in natura2000_marine.rds (from EEA)
#' Only executes when the RDS cache is missing.
#' @return sf object with random MPA rectangles across Europe
#' @noRd
mock_mpa_data_fallback <- function(n = 30) {
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

  sf::st_sf(
    name = paste("MPA", seq_len(n)),
    designation = sample(
      c("Natura 2000", "National MPA", "Proposed 30x30"),
      n, replace = TRUE
    ),
    geometry = sf::st_sfc(polys, crs = 4326)
  )
}

#' Generate mock scenario projection data (FALLBACK)
#' @param nff_weights Named numeric vector c(NfN=, NfS=, NaC=) summing to 100
#' @param region Character region name
#' @return Data frame with yearly projections for 4 indicators
#' @noRd
mock_scenario_data_fallback <- function(nff_weights = c(NfN = 34, NfS = 33, NaC = 33),
                                region = "Mediterranean") {
  set.seed(sum(nff_weights) + nchar(region))
  years <- 2025:2050

  # NFF weights influence trends
  nfn <- nff_weights["NfN"] / 100
  nfs <- nff_weights["NfS"] / 100
  nac <- nff_weights["NaC"] / 100

  indicators <- c(
    "Habitat Condition", "Ecosystem Services",
    "Livelihoods & Employment", "Equity Score",
    "Offshore Wind Capacity", "Bathing Water Quality",
    "Contaminant Status", "Eutrophication Status", "Underwater Noise"
  )
  means <- c(
    0.02 * nfn,
    0.015 * nfs,
    0.01 * nac - 0.005 * nfn,
    0.01 * (1 - max(abs(nfn - nfs), abs(nfs - nac), abs(nfn - nac))),
    0.012 * nfs + 0.005 * nac,
    0.008 * nfn + 0.005 * nfs,
    0.010 * nfn + 0.003 * nfs,
    0.008 * nfn + 0.005 * nfs,
    -0.003 * nfs + 0.005 * nfn
  )
  sds <- c(0.01, 0.01, 0.01, 0.008, 0.01, 0.01, 0.01, 0.01, 0.01)
  bases <- c(0.5, 0.5, 0.6, 0.5, 0.3, 0.7, 0.6, 0.55, 0.5)

  # HELCOM indicators only exist for Baltic region
  helcom_only <- c("Contaminant Status", "Eutrophication Status", "Underwater Noise")
  if (region != "Baltic") {
    keep <- !indicators %in% helcom_only
    indicators <- indicators[keep]
    means <- means[keep]
    sds <- sds[keep]
    bases <- bases[keep]
  }

  do.call(rbind, lapply(seq_along(indicators), function(i) {
    values <- bases[i] + cumsum(rnorm(length(years), mean = means[i], sd = sds[i]))
    values <- pmin(pmax(values, 0.05), 0.99)
    band_width <- sds[i] * sqrt(seq_along(years))
    data.frame(
      year = years,
      indicator = indicators[i],
      value = round(values, 3),
      lower = round(pmax(values - band_width, 0.01), 3),
      upper = round(pmin(values + band_width, 1.00), 3),
      region = region,
      stringsAsFactors = FALSE
    )
  }))
}

#' Generate mock justice scores for an intervention (FALLBACK)
#' @param intervention Character name of intervention
#' @return Data frame with 4 justice dimension scores
#' @noRd
mock_justice_scores_fallback <- function(intervention = "MPA Establishment") {
  set.seed(nchar(intervention))

  dimensions <- c("Distributional", "Procedural", "Recognitional", "Restorative")
  scores <- pmin(pmax(rnorm(4, mean = 0.6, sd = 0.25), 0), 1)

  data.frame(
    dimension = dimensions,
    score = round(scores, 2),
    status = dplyr::case_when(
      scores >= 0.7 ~ "green",
      scores >= 0.4 ~ "amber",
      TRUE ~ "red"
    ),
    description = c(
      "Fair distribution of costs and benefits across communities",
      "Meaningful participation of affected stakeholders in decision-making",
      "Recognition of diverse knowledge systems and cultural values",
      "Remediation of historical environmental injustices"
    )
  )
}

#' Generate mock Elliott's 10 Tenets scores for an intervention (FALLBACK)
#' @param intervention Character name of intervention
#' @return Data frame with 10 tenet scores
#' @noRd
mock_elliott_tenets_fallback <- function(intervention = "MPA Establishment") {
  set.seed(nchar(intervention) + 10)
  tenets <- c("Ecologically sustainable", "Technologically feasible",
              "Economically viable", "Socially desirable",
              "Ethically defensible", "Culturally inclusive",
              "Legally permissible", "Administratively achievable",
              "Effectively communicable", "Politically expedient")
  scores <- pmin(pmax(rnorm(10, mean = 0.6, sd = 0.2), 0), 1)
  data.frame(
    tenet = tenets,
    score = round(scores, 2),
    status = dplyr::case_when(
      scores >= 0.7 ~ "green",
      scores >= 0.4 ~ "amber",
      TRUE ~ "red"
    ),
    description = paste("Assessment of", tolower(tenets), "for", intervention)
  )
}

#' Mock list of interventions (FALLBACK)
#' @return Character vector of intervention names
#' @noRd
mock_interventions_fallback <- function() {
  c(
    "MPA Establishment",
    "Seagrass Restoration",
    "Offshore Wind Siting",
    "Fishing Quota Redistribution",
    "Coastal Wetland Conservation",
    "Posidonia Meadow Protection",
    "Marine Habitat Restoration",
    "Marine Litter Reduction",
    "Sustainable Aquaculture Zone",
    "Deep-Sea Mining Moratorium"
  )
}

#' Generate mock funding instrument matrix (FALLBACK)
#' @return Data frame of interventions vs EU funding eligibility
#' @noRd
mock_funding_matrix_fallback <- function() {
  interventions <- mock_interventions_fallback()
  funds <- c("EMFAF", "LIFE", "Cohesion Fund", "EAFRD", "Just Transition Fund")

  set.seed(99)
  mat <- matrix(
    sample(c("Eligible", "Partial", "Not eligible"), length(interventions) * length(funds), replace = TRUE,
           prob = c(0.4, 0.3, 0.3)),
    nrow = length(interventions)
  )

  df <- as.data.frame(mat)
  names(df) <- funds
  df$intervention <- interventions
  df[, c("intervention", funds)]
}

#' Generate mock CFP alignment data (FALLBACK)
#' @param intervention Character name of the intervention
#' @return Data frame with alignment status and detail columns
#' @noRd
mock_cfp_alignment_fallback <- function(intervention = "MPA Establishment") {
  set.seed(nchar(intervention))
  alignment <- sample(c("aligned", "partial", "conflict"), 1,
                      prob = c(0.4, 0.35, 0.25))
  data.frame(
    intervention = intervention,
    alignment = alignment,
    detail_1 = switch(alignment,
      aligned = "Compatible with existing Total Allowable Catch (TAC) regulations",
      partial = "May require adjustment to regional fishing effort limits",
      conflict = "Conflicts with allocated fishing quotas in target area"),
    detail_2 = switch(alignment,
      aligned = "Supports CFP discard ban implementation",
      partial = "Partial overlap with EMFAF fleet adaptation measures",
      conflict = "Requires Article 11 MSFD/CFP coordination procedure"),
    detail_3 = switch(alignment,
      aligned = "Aligns with Maximum Sustainable Yield (MSY) objectives",
      partial = "Needs coordination with Regional Advisory Councils",
      conflict = "May trigger compensation obligations under EMFAF Article 17"),
    stringsAsFactors = FALSE
  )
}

#' Generate mock indicator time series (FALLBACK)
#' @param region Character region name
#' @return Data frame with indicator values over time with confidence bands
#' @noRd
mock_indicator_timeseries_fallback <- function(region = "Mediterranean") {
  set.seed(nchar(region))
  years <- 2010:2025
  indicators <- c(
    "Marine Biodiversity Index",
    "Habitat Condition",
    "Ecosystem Services",
    "Community Wellbeing Index",
    "Governance Effectiveness",
    "Fish Stock Biomass",
    "Sustainable Fishing",
    "Offshore Wind Capacity",
    "Coastal Tourism Pressure",
    "Bathing Water Quality",
    "Contaminant Status",
    "Eutrophication Status",
    "Underwater Noise"
  )

  # GBF target values for each indicator
  gbf_targets <- c(
    "Marine Biodiversity Index" = 0.75,
    "Habitat Condition" = 0.80,
    "Ecosystem Services" = 0.70,
    "Community Wellbeing Index" = 0.65,
    "Governance Effectiveness" = 0.72,
    "Fish Stock Biomass" = 0.75,
    "Sustainable Fishing" = 0.70,
    "Offshore Wind Capacity" = 0.60,
    "Coastal Tourism Pressure" = 0.55,
    "Bathing Water Quality" = 0.85,
    "Contaminant Status" = 0.80,
    "Eutrophication Status" = 0.75,
    "Underwater Noise" = 0.70
  )

  # HELCOM-only indicators — only generate for Baltic, skip for other basins
  helcom_only <- c("Contaminant Status", "Eutrophication Status", "Underwater Noise")

  do.call(rbind, lapply(indicators, function(ind) {
    # Skip HELCOM-only indicators for non-Baltic basins
    if (ind %in% helcom_only && region != "Baltic") return(NULL)

    trend <- cumsum(rnorm(length(years), mean = 0.01, sd = 0.03))
    value <- 0.5 + trend
    gbf_val <- if (ind %in% names(gbf_targets)) gbf_targets[[ind]] else 0.70
    data.frame(
      year = years,
      indicator = ind,
      value = round(value, 3),
      lower = round(value - abs(rnorm(length(years), 0.05, 0.02)), 3),
      upper = round(value + abs(rnorm(length(years), 0.05, 0.02)), 3),
      region = region,
      gbf_target = gbf_val
    )
  }))
}
