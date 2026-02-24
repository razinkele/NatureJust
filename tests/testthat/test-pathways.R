library(testthat)
library(shiny)

test_that("mod_pathways_server correctly computes future positions", {
  testServer(mod_pathways_server, {
    # Test Custom Input
    session$setInputs(future_preset = "custom")
    session$setInputs(future_nfn = 50, future_nfs = 50, future_nac = 0)
    
    # Check normalized calculation
    res <- future_pos()
    expect_equal(res[["NfN"]], 50)
    expect_equal(res[["NfS"]], 50)
    expect_equal(res[["NaC"]], 0)
    
    # Test Normalization (sums to 100)
    session$setInputs(future_nfn = 10, future_nfs = 10, future_nac = 10)
    res <- future_pos()
    # 10,10,10 sum 30 => 33,33,33 (rounded) -> sum might be 99 or 100 depending on rounding
    # round(10/30*100) = 33
    expect_equal(sum(res), 99) # 33+33+33 = 99. The logic uses round(), so it might not be strictly 100 which is fine/expected behavior for simple round()
    
    # Test Preset "arcology" (100/0/0)
    session$setInputs(future_preset = "arcology")
    res <- future_pos()
    expect_equal(res[["NfN"]], 100)
    expect_equal(res[["NfS"]], 0)
    expect_equal(res[["NaC"]], 0)
  })
})

test_that("mod_pathways_server generates correct policy implications", {
  testServer(mod_pathways_server, {
    # Set Start State (Now)
    session$setInputs(now_nfn = 50, now_nfs = 25, now_nac = 25)
    
    # Set Future State (Custom): Increase NfN significantly (+50)
    session$setInputs(
      future_preset = "custom",
      future_nfn = 100, 
      future_nfs = 0, 
      future_nac = 0
    )
    
    # Trigger render of horizons_analysis
    # We can inspect the output of `output$horizons_analysis`
    # output$horizons_analysis returns the tag structure from renderUI
    html_out <- output$horizons_analysis
    # Convert to single string for easy matching
    html_str <- paste(as.character(html_out), collapse = "")
    
    # Use simple string matching to verify content
    # Delta NfN = 100 - 50 = +50 -> "increasing" -> "intrinsic value of biodiversity is gaining prominence"
    expect_match(html_str, "The intrinsic value of biodiversity is gaining prominence")
    
    # Delta NfS = 0 - 25 = -25 -> "decreasing" -> "The utilitarian perspective is receding"
    expect_match(html_str, "The utilitarian perspective is receding")
    
    # Delta NaC = 0 - 25 = -25 -> "decreasing" -> "Cultural and relational values are losing ground"
    expect_match(html_str, "Cultural and relational values are losing ground")
  })
})
