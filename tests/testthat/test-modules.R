test_that("mod_home_ui returns taglist", {
  ui <- mod_home_ui("test")
  expect_true(inherits(ui, "shiny.tag.list") || inherits(ui, "shiny.tag"))
})

test_that("mod_spatial_ui returns taglist", {
  ui <- mod_spatial_ui("test")
  expect_true(inherits(ui, "shiny.tag.list") || inherits(ui, "shiny.tag"))
})

test_that("mod_scenarios_ui returns taglist", {
  ui <- mod_scenarios_ui("test")
  expect_true(inherits(ui, "shiny.tag.list") || inherits(ui, "shiny.tag"))
})

test_that("mod_justice_ui returns taglist", {
  ui <- mod_justice_ui("test")
  expect_true(inherits(ui, "shiny.tag.list") || inherits(ui, "shiny.tag"))
})

test_that("mod_governance_ui returns taglist", {
  ui <- mod_governance_ui("test")
  expect_true(inherits(ui, "shiny.tag.list") || inherits(ui, "shiny.tag"))
})

test_that("mod_dashboard_ui returns taglist", {
  ui <- mod_dashboard_ui("test")
  expect_true(inherits(ui, "shiny.tag.list") || inherits(ui, "shiny.tag"))
})

test_that("data loader functions work", {
  expect_s3_class(load_nuts2_data(), "sf")
  expect_s3_class(load_mpa_data(), "sf")
  expect_s3_class(load_scenario_data(), "data.frame")
  expect_s3_class(load_justice_scores(), "data.frame")
  expect_type(load_interventions(), "character")
  expect_s3_class(load_funding_matrix(), "data.frame")
  expect_s3_class(load_indicator_timeseries(), "data.frame")
  expect_s3_class(load_elliott_tenets(), "data.frame")
})

test_that("load_elliott_tenets returns 10 tenets with required columns", {
  df <- load_elliott_tenets("MPA Establishment")
  expect_equal(nrow(df), 10)
  expect_true(all(c("tenet", "score", "status", "description") %in% names(df)))
  expect_true(all(df$score >= 0 & df$score <= 1))
  expect_true(all(df$status %in% c("green", "amber", "red")))
})

test_that("load_cfp_alignment returns data with required columns", {
  df <- load_cfp_alignment("MPA Establishment")
  expect_s3_class(df, "data.frame")
  expect_true(all(c("intervention", "alignment", "detail_1", "detail_2", "detail_3") %in% names(df)))
  expect_true(df$alignment[1] %in% c("aligned", "partial", "conflict"))
})

test_that("load_cfp_alignment falls back gracefully for unknown intervention", {
  # Should fall back to mock_cfp_alignment_fallback for unknown interventions
  df <- load_cfp_alignment("Nonexistent Intervention")
  expect_s3_class(df, "data.frame")
  expect_true("alignment" %in% names(df))
})

test_that("load_nuts2_data has renamed population_pressure column", {
  nuts2 <- load_nuts2_data()
  expect_true("population_pressure" %in% names(nuts2))
  expect_false("conservation_pressure" %in% names(nuts2))
})

test_that("gbf_targets.csv indicator names match scenario_baselines.csv", {
  gbf <- tryCatch(
    load_extdata("gbf_targets.csv"),
    error = function(e) NULL
  )
  baselines <- tryCatch(
    load_extdata("scenario_baselines.csv"),
    error = function(e) NULL
  )

  if (!is.null(gbf) && !is.null(baselines)) {
    scenario_indicators <- unique(baselines$indicator)
    # All scenario indicators should have a matching GBF target
    matched <- scenario_indicators %in% gbf$indicator
    expect_true(all(matched),
      info = paste("Unmatched scenario indicators:",
                   paste(scenario_indicators[!matched], collapse = ", ")))
  }
})

test_that("scenario_baselines.csv has required columns", {
  baselines <- tryCatch(
    load_extdata("scenario_baselines.csv"),
    error = function(e) NULL
  )
  if (!is.null(baselines)) {
    expected_cols <- c("region", "indicator", "baseline_2025", "trend_rate", "variance", "source")
    expect_true(all(expected_cols %in% names(baselines)))
  }
})

test_that("cfp_alignment.csv has required columns", {
  cfp <- tryCatch(
    load_extdata("cfp_alignment.csv"),
    error = function(e) NULL
  )
  if (!is.null(cfp)) {
    expected_cols <- c("intervention", "alignment", "detail_1", "detail_2", "detail_3")
    expect_true(all(expected_cols %in% names(cfp)))
    expect_true(all(cfp$alignment %in% c("aligned", "partial", "conflict")))
  }
})

test_that("data loader functions set provenance attribute", {
  nuts2 <- load_nuts2_data()
  expect_true(!is.null(attr(nuts2, "provenance")))
  expect_true(attr(nuts2, "provenance") %in% c("rds", "fallback"))

  ts <- load_indicator_timeseries()
  expect_true(!is.null(attr(ts, "provenance")))
  expect_true(attr(ts, "provenance") %in% c("rds", "fallback"))

  scores <- load_justice_scores()
  expect_true(!is.null(attr(scores, "provenance")))
  expect_true(attr(scores, "provenance") %in% c("csv", "fallback"))
})

test_that("load_mpa_data returns sf with name and designation columns", {
  mpas <- load_mpa_data()
  expect_s3_class(mpas, "sf")
  expect_true("name" %in% names(mpas))
  expect_true("designation" %in% names(mpas))
  expect_true(!is.null(attr(mpas, "provenance")))
})

test_that("load_indicator_timeseries includes HELCOM indicators for Baltic", {
  ts <- load_indicator_timeseries("Baltic")
  indicators <- unique(ts$indicator)
  # HELCOM indicators should be present for Baltic (fallback or real)
  expect_true("Contaminant Status" %in% indicators ||
              attr(ts, "provenance") == "rds",
              info = "HELCOM indicators expected in Baltic (fallback or real)")
})

test_that("load_indicator_timeseries excludes HELCOM indicators for non-Baltic", {
  ts <- load_indicator_timeseries("Mediterranean")
  indicators <- unique(ts$indicator)
  # HELCOM-only indicators should NOT appear for non-Baltic
  expect_false("Contaminant Status" %in% indicators)
  expect_false("Eutrophication Status" %in% indicators)
  expect_false("Underwater Noise" %in% indicators)
})

test_that("gbf_targets.csv has 15 indicators including HELCOM", {
  gbf <- tryCatch(load_extdata("gbf_targets.csv"), error = function(e) NULL)
  if (!is.null(gbf)) {
    expect_equal(nrow(gbf), 15)
    expect_true("Contaminant Status" %in% gbf$indicator)
    expect_true("Eutrophication Status" %in% gbf$indicator)
    expect_true("Underwater Noise" %in% gbf$indicator)
  }
})

test_that("gfcm_stocks.csv loads with expected schema", {
  gfcm <- tryCatch(load_extdata("gfcm_stocks.csv"), error = function(e) NULL)
  if (!is.null(gfcm)) {
    expect_true(all(c("year", "basin", "indicator", "value", "source") %in% names(gfcm)))
    expect_true("Mediterranean" %in% gfcm$basin)
    expect_true("Black Sea" %in% gfcm$basin)
    expect_true("Fish Stock Biomass" %in% gfcm$indicator)
    expect_true("Sustainable Fishing" %in% gfcm$indicator)
  }
})

test_that("scenario_baselines.csv includes HELCOM indicators for Baltic", {
  baselines <- tryCatch(load_extdata("scenario_baselines.csv"), error = function(e) NULL)
  if (!is.null(baselines)) {
    baltic <- baselines[baselines$region == "Baltic", ]
    expect_true("Contaminant Status" %in% baltic$indicator)
    expect_true("Eutrophication Status" %in% baltic$indicator)
    expect_true("Underwater Noise" %in% baltic$indicator)
    # Non-Baltic should NOT have HELCOM indicators
    med <- baselines[baselines$region == "Mediterranean", ]
    expect_false("Contaminant Status" %in% med$indicator)
  }
})
