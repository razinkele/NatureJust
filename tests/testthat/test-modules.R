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

test_that("mock data functions work", {
  expect_s3_class(mock_nuts2_data(), "sf")
  expect_s3_class(mock_mpa_data(), "sf")
  expect_s3_class(mock_scenario_data(), "data.frame")
  expect_s3_class(mock_justice_scores(), "data.frame")
  expect_type(mock_interventions(), "character")
  expect_s3_class(mock_funding_matrix(), "data.frame")
  expect_s3_class(mock_indicator_timeseries(), "data.frame")
  expect_s3_class(mock_elliott_tenets(), "data.frame")
})

test_that("mock_elliott_tenets returns 10 tenets with required columns", {
  df <- mock_elliott_tenets("MPA Establishment")
  expect_equal(nrow(df), 10)
  expect_true(all(c("tenet", "score", "status", "description") %in% names(df)))
  expect_true(all(df$score >= 0 & df$score <= 1))
  expect_true(all(df$status %in% c("green", "amber", "red")))
})

test_that("mock_cfp_alignment returns data with required columns", {
  df <- mock_cfp_alignment("MPA Establishment")
  expect_s3_class(df, "data.frame")
  expect_true(all(c("intervention", "alignment", "detail_1", "detail_2", "detail_3") %in% names(df)))
  expect_true(df$alignment[1] %in% c("aligned", "partial", "conflict"))
})

test_that("mock_cfp_alignment falls back gracefully for unknown intervention", {
  # Should fall back to mock_cfp_alignment_fallback for unknown interventions
  df <- mock_cfp_alignment("Nonexistent Intervention")
  expect_s3_class(df, "data.frame")
  expect_true("alignment" %in% names(df))
})

test_that("mock_nuts2_data has renamed population_pressure column", {
  nuts2 <- mock_nuts2_data()
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
