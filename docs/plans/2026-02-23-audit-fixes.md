# Audit Fixes Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Fix the 7 remaining issues from the deep codebase audit (1 critical, 2 medium, 4 low).

**Architecture:** Targeted fixes to existing files — no new modules or major refactors. Each fix is self-contained.

**Tech Stack:** R/Shiny, bslib, plotly, testthat

---

## Summary of Remaining Issues

| # | Severity | Issue | File(s) |
|---|----------|-------|---------|
| 1 | Critical | `here::here()` used but `here` not in DESCRIPTION | `data-raw/prepare_data.R`, `DESCRIPTION` |
| 2 | Medium | Bar plot compares scenarios at different horizons without warning | `R/mod_scenarios.R` |
| 3 | Medium | HELCOM layer IDs hardcoded without validation comment | `data-raw/prepare_data.R` |
| 4 | Low | Region selector naming inconsistent across modules | `R/mod_justice.R` |
| 5 | Low | JS `Shiny` object assumed available without guard | `inst/app/www/nff_triangle.js` |
| 6 | Low | `indicator_colors` hardcoded only in scenarios module | `R/mod_scenarios.R` (document only) |
| 7 | Low | No server-logic tests | `tests/testthat/test-modules.R` |

---

### Task 1: Fix `here` package dependency

The script `data-raw/prepare_data.R` uses `here::here()` on line 20, but `here` is
not declared in DESCRIPTION. Rather than adding a dependency just for one line in a
data-prep script, replace it with a robust alternative.

**Files:**
- Modify: `data-raw/prepare_data.R:19-23`

**Step 1: Replace `here::here()` with `rprojroot` fallback**

In `data-raw/prepare_data.R`, replace lines 19-23:

```r
# Resolve project root (works from RStudio or sourced from project dir)
project_root <- here::here()
if (!file.exists(file.path(project_root, "DESCRIPTION"))) {
  project_root <- getwd()
}
```

With:

```r
# Resolve project root — find directory containing DESCRIPTION
project_root <- tryCatch(
  here::here(),
  error = function(e) getwd()
)
if (!file.exists(file.path(project_root, "DESCRIPTION"))) {
  # Walk up from working directory to find project root
  candidate <- getwd()
  while (!file.exists(file.path(candidate, "DESCRIPTION")) &&
         candidate != dirname(candidate)) {
    candidate <- dirname(candidate)
  }
  project_root <- candidate
}
```

This removes the hard dependency on `here` while still supporting it when available.

**Step 2: Verify prepare_data.R still resolves project root**

Run (from project root):
```bash
Rscript --no-init-file -e "source('data-raw/prepare_data.R', echo = FALSE)"
```

Expected: Script runs without `here` package errors (may fail later if API unreachable — that's fine).

**Step 3: Commit**

```bash
git add data-raw/prepare_data.R
git commit -m "fix: remove hard dependency on here package in prepare_data.R"
```

---

### Task 2: Add horizon label to bar plot comparison

When users save scenarios with different time horizons (2030 vs 2050), the bar plot
compares them side by side without indicating the horizons differ. This is misleading.

**Files:**
- Modify: `R/mod_scenarios.R:275-303`

**Step 1: Update bar plot to include horizon in scenario label**

In `R/mod_scenarios.R`, replace lines 283-290:

```r
      df_list <- lapply(names(scenarios), function(nm) {
        sc <- scenarios[[nm]]
        d <- sc$data
        horizon_year <- sc$horizon
        d |>
          dplyr::filter(year == horizon_year) |>
          dplyr::mutate(scenario = nm)
      })
```

With:

```r
      df_list <- lapply(names(scenarios), function(nm) {
        sc <- scenarios[[nm]]
        d <- sc$data
        horizon_year <- sc$horizon
        d |>
          dplyr::filter(year == horizon_year) |>
          dplyr::mutate(scenario = paste0(nm, " @", horizon_year))
      })
```

This appends the horizon year to each scenario label (e.g., "S1: 34/33/33 @2050").

**Step 2: Verify the change compiles**

Source `R/mod_scenarios.R` in R and confirm no syntax errors.

**Step 3: Commit**

```bash
git add R/mod_scenarios.R
git commit -m "fix: show time horizon in scenario bar plot labels"
```

---

### Task 3: Document HELCOM layer IDs with validation comment

The HELCOM ArcGIS REST API layer IDs (0, 1, 2, 3, 4) are hardcoded in
`data-raw/prepare_data.R`. If HELCOM changes their MapServer structure, queries
will silently fail. Add a documentation comment with the verification URL.

**Files:**
- Modify: `data-raw/prepare_data.R` (the `helcom_layers` list definition)

**Step 1: Add documentation block above `helcom_layers`**

Find the `helcom_layers <- list(` definition and add above it:

```r
# HELCOM MADS MapServer layer IDs — verified against:
# https://maps.helcom.fi/arcgis/rest/services/MADS/Biodiversity/MapServer?f=json
# If layers change, query the URL above and update the id fields below.
# Last verified: 2026-02-23
```

**Step 2: Commit**

```bash
git add data-raw/prepare_data.R
git commit -m "docs: document HELCOM layer ID source and verification date"
```

---

### Task 4: Standardize Justice module region names

The Justice module uses different region names from other modules:
- Justice: "Baltic Sea", "Atlantic Coast", "Adriatic", "Aegean", "Celtic Sea"
- Dashboard/Scenarios: "Baltic", "Atlantic"

Since the Justice module's `target_area` serves a different purpose (finer-grained
area selection for interventions), the fix is to keep the richer choices but group
them under consistent parent names using optgroups.

**Files:**
- Modify: `R/mod_justice.R:15-19`

**Step 1: Replace flat region list with grouped choices**

In `R/mod_justice.R`, replace lines 15-19:

```r
      selectInput(
        ns("target_area"), "Target Area",
        choices = c("Baltic Sea", "North Sea", "Atlantic Coast",
                    "Mediterranean", "Black Sea", "Adriatic",
                    "Aegean", "Celtic Sea")
      ),
```

With:

```r
      selectInput(
        ns("target_area"), "Target Area",
        choices = list(
          "Baltic" = c("Baltic Sea"),
          "North Sea" = c("North Sea"),
          "Atlantic" = c("Atlantic Coast", "Celtic Sea"),
          "Mediterranean" = c("Mediterranean", "Adriatic", "Aegean"),
          "Black Sea" = c("Black Sea")
        )
      ),
```

This keeps the fine-grained choices while grouping under the 5 standard basin names.

**Step 2: Verify the module UI renders**

Source the module in R and call `mod_justice_ui("test")` — confirm it returns a valid tagList.

**Step 3: Commit**

```bash
git add R/mod_justice.R
git commit -m "fix: group Justice target areas under standard sea basin names"
```

---

### Task 5: Add Shiny existence guard in JavaScript

`nff_triangle.js` calls `Shiny.setInputValue()` on line 6 without checking
if `Shiny` is defined. This would throw a JS error if the script loads before
Shiny initializes (unlikely but possible).

**Files:**
- Modify: `inst/app/www/nff_triangle.js:5-6`

**Step 1: Add guard around Shiny.setInputValue**

In `inst/app/www/nff_triangle.js`, replace lines 5-6:

```javascript
NatureJust.navigateTo = function(target) {
  Shiny.setInputValue('main_nav', target, {priority: 'event'});
```

With:

```javascript
NatureJust.navigateTo = function(target) {
  if (typeof Shiny !== 'undefined') {
    Shiny.setInputValue('main_nav', target, {priority: 'event'});
  }
```

**Step 2: Commit**

```bash
git add inst/app/www/nff_triangle.js
git commit -m "fix: guard Shiny.setInputValue against undefined Shiny object"
```

---

### Task 6: Add server-logic tests with testServer()

Current tests cover UI rendering and data loaders but not reactive server logic.
Add `testServer()` tests for the two most complex modules: Dashboard and Scenarios.

**Files:**
- Modify: `tests/testthat/test-modules.R` (append new tests)

**Step 1: Add Dashboard server test**

Append to `tests/testthat/test-modules.R`:

```r
test_that("mod_dashboard_server loads data reactively", {
  testServer(mod_dashboard_server, {
    # Set region input
    session$setInputs(region = "Baltic")
    # data() reactive should return a data frame
    df <- data()
    expect_s3_class(df, "data.frame")
    expect_true("indicator" %in% names(df))
    expect_true("year" %in% names(df))
    expect_true("value" %in% names(df))

    # filtered_data() with no indicator selection returns full dataset
    session$setInputs(indicators = NULL)
    expect_equal(nrow(filtered_data()), nrow(df))
  })
})
```

**Step 2: Add Scenarios server test**

Append to `tests/testthat/test-modules.R`:

```r
test_that("mod_scenarios_server computes normalised weights", {
  testServer(mod_scenarios_server, {
    session$setInputs(nfn = 50, nfs = 25, nac = 25,
                      region = "Baltic", horizon = "2050")
    w <- weights()
    expect_equal(sum(w), 100)
    expect_equal(w[["NfN"]], 50)

    # current_data() should return a data frame
    df <- current_data()
    expect_s3_class(df, "data.frame")
    expect_true("indicator" %in% names(df))
  })
})
```

**Step 3: Run tests**

```bash
Rscript --no-init-file dev/run_tests.R
```

Expected: All tests pass (existing 21 + 2 new = 23).

**Step 4: Commit**

```bash
git add tests/testthat/test-modules.R
git commit -m "test: add testServer() tests for Dashboard and Scenarios modules"
```

---

### Task 7: Run full test suite and final verification

**Step 1: Run all tests**

```bash
Rscript --no-init-file dev/run_tests.R
```

Expected: 23+ tests pass, 0 failures.

**Step 2: Launch app and verify each module tab**

```bash
Rscript --no-init-file dev/launch.R
```

Check:
- Home tab loads with NFF triangle
- Spatial Equity map renders
- Scenarios projections plot renders
- Justice scorecard loads
- Governance tab loads
- Indicators dashboard loads with 13 indicators in picker

**Step 3: Final commit (if any fixes needed)**

```bash
git add -A
git commit -m "fix: address remaining audit findings"
```

---

## Key Files Changed

| File | Tasks |
|------|-------|
| `data-raw/prepare_data.R` | 1, 3 |
| `R/mod_scenarios.R` | 2 |
| `R/mod_justice.R` | 4 |
| `inst/app/www/nff_triangle.js` | 5 |
| `tests/testthat/test-modules.R` | 6 |

## What Was NOT Fixed (By Design)

- **Indicator colors centralization** (finding #17): Only one module uses custom colors.
  Extracting to a utils file is premature abstraction — revisit if a second module needs them.
- **ICES API caching** (finding #5): `prepare_data.R` runs offline once; runtime caching
  is unnecessary. The script already has fallback cascades.
- **HELCOM layer ID runtime validation** (finding #9): Would add network calls to an
  already complex pipeline. Documentation (Task 3) is sufficient.
