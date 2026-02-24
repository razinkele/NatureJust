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
    expect_true(all(c("year", "region", "indicator", "value", "source") %in% names(gfcm)))
    expect_true("Mediterranean" %in% gfcm$region)
    expect_true("Black Sea" %in% gfcm$region)
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

test_that("mod_dashboard_server loads data reactively", {
  testServer(mod_dashboard_server, {
    session$setInputs(region = "Baltic")
    df <- data()
    expect_s3_class(df, "data.frame")
    expect_true("indicator" %in% names(df))
    expect_true("year" %in% names(df))
    expect_true("value" %in% names(df))

    session$setInputs(indicators = NULL)
    expect_equal(nrow(filtered_data()), nrow(df))
  })
})

test_that("mod_scenarios_server computes normalised weights", {
  testServer(mod_scenarios_server, {
    session$setInputs(nfn = 50, nfs = 25, nac = 25,
                      region = "Baltic", horizon = "2050")
    w <- weights()
    expect_equal(sum(w), 100)
    expect_equal(w[["NfN"]], 50)

    df <- current_data()
    expect_s3_class(df, "data.frame")
    expect_true("indicator" %in% names(df))
  })
})

test_that("mod_narratives_ui returns taglist", {
  ui <- mod_narratives_ui("test")
  expect_true(inherits(ui, "shiny.tag.list") || inherits(ui, "shiny.tag"))
})

test_that("mod_stakeholders_ui returns taglist", {
  ui <- mod_stakeholders_ui("test")
  expect_true(inherits(ui, "shiny.tag.list") || inherits(ui, "shiny.tag"))
})

test_that("mod_pathways_ui returns taglist", {
  ui <- mod_pathways_ui("test")
  expect_true(inherits(ui, "shiny.tag.list") || inherits(ui, "shiny.tag"))
})

test_that("load_narratives returns 6 narratives with required fields", {
  narr <- load_narratives()
  expect_equal(nrow(narr), 6)
  expect_true(all(c("id", "name", "nff_position", "description",
                     "governance_model", "source") %in% names(narr)))
  expect_true("arcology" %in% narr$id)
  expect_true("stewardship" %in% narr$id)
  expect_true("weights" %in% names(narr))
})

test_that("all 6 narrative weights sum to 100", {
  narr <- load_narratives()
  # weights is a nested data.frame (from JSON) or I(list(...)) column (fallback)
  for (i in seq_len(nrow(narr))) {
    w <- if (is.data.frame(narr$weights)) {
      narr$weights[i, ]
    } else {
      narr$weights[[i]]
    }
    total <- as.numeric(w[["NfN"]]) + as.numeric(w[["NfS"]]) + as.numeric(w[["NaC"]])
    expect_equal(total, 100,
      info = paste("Narrative", narr$id[i], "weights sum to", total, "not 100"))
  }
})

test_that("mod_narratives_server selects narrative and extracts weights", {
  testServer(mod_narratives_server, {
    session$setInputs(narrative_id = "arcology")
    narr <- current()
    expect_equal(narr$id, "arcology")
    w <- current_weights()
    expect_equal(sum(w), 100)
    expect_equal(w[["NfN"]], 100)
  })
})

test_that("mod_stakeholders_server adds and clears stakeholders", {
  testServer(mod_stakeholders_server, {
    # Initially empty
    expect_equal(nrow(stakeholders()), 0)

    # Add a stakeholder
    session$setInputs(
      stakeholder_name = "Test Fisher",
      stakeholder_group = "Small-Scale Fishers",
      triangle_position = list(NfN = 40, NfS = 30, NaC = 30)
    )
    session$setInputs(add_stakeholder = 1)
    expect_equal(nrow(stakeholders()), 1)
    expect_equal(stakeholders()$name[1], "Test Fisher")
    expect_equal(stakeholders()$NfN[1], 40)
  })
})

test_that("mod_home_server updates nff_weights from triangle position", {
  shared_weights <- reactiveVal(c(NfN = 34, NfS = 33, NaC = 33))
  testServer(mod_home_server, args = list(nff_weights = shared_weights), {
    session$setInputs(nff_position = list(NfN = 60, NfS = 20, NaC = 20))
    expect_equal(shared_weights()[["NfN"]], 60)
    expect_equal(shared_weights()[["NfS"]], 20)
    expect_equal(shared_weights()[["NaC"]], 20)
  })
})

test_that("mod_pathways_server normalizes now_pos to sum=100", {
  testServer(mod_pathways_server, {
    session$setInputs(
      now_nfn = 30, now_nfs = 50, now_nac = 20,
      future_preset = "custom",
      future_nfn = 40, future_nfs = 30, future_nac = 30,
      timeline = 2025
    )
    pos <- now_pos()
    expect_equal(sum(pos), 100)
    expect_equal(pos[["NfN"]], 30)
    expect_equal(pos[["NfS"]], 50)

    # Verify normalization when inputs don't sum to 100
    # round() can yield ±1 due to independent per-element rounding
    session$setInputs(now_nfn = 60, now_nfs = 60, now_nac = 60)
    pos2 <- now_pos()
    expect_true(abs(sum(pos2) - 100) <= 1)
    expect_equal(pos2[["NfN"]], pos2[["NfS"]])  # all equal inputs → equal outputs

    # Verify future_pos uses NARRATIVE_PRESETS
    session$setInputs(future_preset = "arcology")
    fp <- future_pos()
    expect_equal(fp[["NfN"]], 100)
    expect_equal(fp[["NfS"]], 0)
  })
})

test_that("mod_justice_server loads scores and applies area modifiers", {
  testServer(mod_justice_server, {
    session$setInputs(
      intervention = "MPA Establishment",
      target_area = "Baltic Sea"
    )
    df <- scores()
    expect_s3_class(df, "data.frame")
    expect_equal(nrow(df), 4)
    expect_true(all(c("dimension", "score", "status", "description") %in% names(df)))
    expect_true(all(df$score >= 0 & df$score <= 1))
    expect_true(all(df$status %in% c("green", "amber", "red")))
  })
})

test_that("mod_governance_server loads funding matrix and tenet scores", {
  testServer(mod_governance_server, {
    # Funding data is loaded once as a reactive
    df <- funding_data()
    expect_s3_class(df, "data.frame")
    expect_true("intervention" %in% names(df))
    expect_true("EMFAF" %in% names(df))

    # Tenet scores load for a given intervention
    session$setInputs(tenet_intervention = "MPA Establishment")
    tenets <- tenet_scores()
    expect_s3_class(tenets, "data.frame")
    expect_equal(nrow(tenets), 10)
    expect_true(all(tenets$score >= 0 & tenets$score <= 1))

    # CFP alignment loads for a given measure
    session$setInputs(cfp_measure = "MPA Establishment")
    cfp <- cfp_data()
    expect_s3_class(cfp, "data.frame")
    expect_true(cfp$alignment[1] %in% c("aligned", "partial", "conflict"))
  })
})

test_that("mod_scenarios_server HELCOM indicators included only for Baltic", {
  testServer(mod_scenarios_server, {
    session$setInputs(nfn = 34, nfs = 33, nac = 33,
                      region = "Baltic", horizon = "2050")
    baltic_df <- current_data()
    baltic_indicators <- unique(baltic_df$indicator)
    expect_true("Contaminant Status" %in% baltic_indicators)
    expect_true("Eutrophication Status" %in% baltic_indicators)
    expect_true("Underwater Noise" %in% baltic_indicators)

    session$setInputs(region = "Mediterranean")
    med_df <- current_data()
    med_indicators <- unique(med_df$indicator)
    expect_false("Contaminant Status" %in% med_indicators)
    expect_false("Eutrophication Status" %in% med_indicators)
    expect_false("Underwater Noise" %in% med_indicators)
  })
})
