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
