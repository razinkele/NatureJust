library(testthat)
library(shiny)

test_that("nff_weight_modifier returns correct modifiers", {
  # Test specific indicators with known formulas
  
  # Habitat Condition = 0.02 * nfn
  expect_equal(nff_weight_modifier("Habitat Condition", nfn=1, nfs=0, nac=0), 0.02)
  expect_equal(nff_weight_modifier("Habitat Condition", 0.5, 0, 0), 0.01)
  
  # Ecosystem Services = 0.015 * nfs
  expect_equal(nff_weight_modifier("Ecosystem Services", 0, 1, 0), 0.015)
  
  # Livelihoods = 0.01 * nac - 0.005 * nfn
  expect_equal(nff_weight_modifier("Livelihoods & Employment", 0.5, 0, 1.0), 0.01 * 1.0 - 0.005 * 0.5)
  
  # Equity Score = 0.01 * (1 - max_diff) where max_diff is max abs difference
  # Case: Balanced (33/33/33 approx) -> max_diff is near 0 -> score near 0.01
  expect_equal(nff_weight_modifier("Equity Score", 0.33, 0.33, 0.33), 0.01 * (1 - 0))
  
  # Case: Extreme (100/0/0) -> max_diff is 1 -> score 0
  expect_equal(nff_weight_modifier("Equity Score", 1, 0, 0), 0)
  
  # Default case
  expect_equal(nff_weight_modifier("Unknown Indicator", 0.3, 0.3, 0.4), 0)
})

test_that("mod_spatial_server computes composite NFF index correctly", {
  # We need to mock the data loading inside the module or use testServer with mocked data if possible.
  # Since load_nuts2_data uses caching and might be slow/complex to mock perfectly without mocking the function,
  # we will test the logic by setting inputs and checking side effects if possible, 
  # or rely on the fact that we can call internal reactives in testServer.
  
  shared_weights <- reactiveVal(c(NfN = 100, NfS = 0, NaC = 0))
  
  testServer(mod_spatial_server, args = list(nff_weights = shared_weights), {
    # Override the reactive data with a mock to ensure deterministic calculation
    # We can't easily override 'all_data' reactive inside the module without rewriting it to accept data.
    # However, we can check if 'composite_data' return value behaves as expected given the *real/fallback* data 
    # and our extreme weights.
    
    # Trigger calculation
    session$setInputs(show_nff_composite = TRUE)
    
    # Get the calculated data
    res <- composite_data()
    
    # aggregated checks
    expect_true("nff_composite" %in% names(res))
    expect_true(all(res$nff_composite >= 0 & res$nff_composite <= 1))
    
    # With NfN=100, NfS=0, NaC=0, the composite should be exactly the nfn_score
    # nfn_score = mean(vulnerability, mpa_coverage, bathing_quality)
    # We can check the first row manually if we knew the values, but we can check the range consistency.
    
    row1 <- res[1, ]
    nfn_score <- mean(c(row1$vulnerability, row1$mpa_coverage, row1$bathing_quality), na.rm = TRUE)
    # Module rounds
    expect_equal(row1$nff_composite, round(nfn_score, 3))
    
    # Change weights to NfS=100
    shared_weights(c(NfN = 0, NfS = 100, NaC = 0))
    session$flushReact() # Ensure reactives update
    
    res2 <- composite_data()
    row1_2 <- res2[1, ]
    nfs_score <- mean(c(row1_2$blue_economy_jobs, row1_2$offshore_wind, row1_2$fisheries_dep), na.rm = TRUE)
    # Module rounds to 3 decimal places
    expect_equal(row1_2$nff_composite, round(nfs_score, 3))
  })
})

test_that("mod_justice_server adjusts scores by area", {
  testServer(mod_justice_server, {
    # If using testServer with args not matching formals, it might complain if not handled.
    # mod_justice_server(id, intervention_choices = NULL)
    # testServer handles 'args' for module return value but the function itself is called by testServer.
    # We can pass args via 'args = list(...)'
    
    session$setInputs(intervention = "MPA Establishment")
    
    # 1. Check Baseline (or relative change)
    # Set to area with known negative modifiers
    session$setInputs(target_area = "Mediterranean")
    scores_med <- scores()
    
    # Set to area with known positive modifiers
    session$setInputs(target_area = "Baltic Sea")
    scores_baltic <- scores()
    
    # Expect Baltic scores to be higher than Med scores for same intervention
    # (Checking one dimension that varies, e.g. Distributional)
    
    # Check if we have data
    if(nrow(scores_med) > 0 && nrow(scores_baltic) > 0) {
      score_med_dist <- scores_med$score[scores_med$dimension == "Distributional"]
      score_baltic_dist <- scores_baltic$score[scores_baltic$dimension == "Distributional"]
      
      # We check mean if multiple rows, but here each dimension is unique per intervention usually
      expect_true(mean(score_baltic_dist, na.rm=TRUE) > mean(score_med_dist, na.rm=TRUE))
    }
  })
})

test_that("mod_dashboard_server sorts indicators by NFF weight", {
  # Mock weights favoring Nature for Society (NfS) -> Ecosystem Services should be high
  shared_weights <- reactiveVal(c(NfN = 0, NfS = 100, NaC = 0))
  
  testServer(mod_dashboard_server, args = list(nff_weights = shared_weights), {
    session$setInputs(region = "Baltic")
    
    # Trigger the filtered_data reactive
    # We need to make sure indicators input is NULL (all) or has multiple to see sorting
    session$setInputs(indicators = NULL)
    
    df <- filtered_data()
    # Check levels of the factor (which determine plot order)
    levels_sorted <- levels(df$indicator) # Wait, code says: df$indicator <- as.character(...) at the end.
    # Ah, the code sorts the dataframe rows but drops the factor levels locally 
    # OR it sets the factor levels then drops them?
    # Let's check the code in mod_dashboard.R again. 
    # It says: df$indicator <- factor(..., levels = sorted_inds); df <- df[order(...)]; df$indicator <- as.character(...)
    # So the *rows* should be sorted.
    
    # Get unique indicators in order of appearance
    unique_inds <- unique(df$indicator)
    
    # Calculate expected scores for a few key indicators with NfS=100
    # Ecosystem Services = 0.015 * 1 = 0.015
    # Habitat Condition = 0.02 * 0 = 0
    # Offshore Wind Capacity = 0.012 * 1 + 0 = 0.012
    
    # So Ecosystem Services should appear before Habitat Condition in the sorted list?
    # The sort is decreasing = TRUE.
    
    # Find positions
    pos_eco <- which(unique_inds == "Ecosystem Services")
    pos_hab <- which(unique_inds == "Habitat Condition")
    
    if(length(pos_eco) > 0 && length(pos_hab) > 0) {
        expect_lt(pos_eco, pos_hab) 
    }
  })
})

test_that("mod_scenarios_server handles extreme inputs without error", {
  testServer(mod_scenarios_server, {
    # Zero sum case (should be handled by normalization ref safe-guard)
    session$setInputs(nfn = 0, nfs = 0, nac = 0, region="Baltic")
    w <- weights()
    # Expect normalization to handle 0 sum (usually by treating as 1 or uniform, code says total=1 if 0)
    # If total=1, then 0/1 * 100 = 0. So returns 0,0,0.
    expect_equal(unname(w), c(0,0,0))
    
    # Verify projection plot doesn't crash
    output$projection_plot
    
    # Extreme values
    session$setInputs(nfn = 100, nfs = 100, nac = 100)
    w <- weights()
    # Should normalize to 33, 33, 33 (approx)
    expect_equal(unname(w), c(33, 33, 33))
  })
})
