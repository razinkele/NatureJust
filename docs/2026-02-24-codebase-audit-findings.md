# NatureJust-EU — Codebase Audit Findings

**Date:** 2026-02-24  
**Scope:** Full R/Shiny codebase analysis — architecture, consistency, performance, and maintainability  
**Files reviewed:** All 13 R source files, JS, CSS, NAMESPACE, DESCRIPTION, tests, and `prepare_data.R`

---

## Table of Contents

1. [Architecture & Consistency Issues](#1-architecture--consistency-issues)
2. [Data Loading & Caching](#2-data-loading--caching)
3. [Duplicated Logic](#3-duplicated-logic)
4. [Reactivity & Performance](#4-reactivity--performance)
5. [Input Validation & Error Handling](#5-input-validation--error-handling)
6. [Namespace & Package Metadata](#6-namespace--package-metadata)
7. [Front-End (JS/CSS)](#7-front-end-jscss)
8. [Testing Gaps](#8-testing-gaps)
9. [Dead Code & Unused Artefacts](#9-dead-code--unused-artefacts)
10. [Recommendations Summary](#10-recommendations-summary)

---

## 1. Architecture & Consistency Issues

### 1.1 Hardcoded Cross-Module Reference

**File:** `app_server.R` (lines 27–30)  
**Issue:** The observer for `navigate_to_narrative` hardcodes the namespaced input ID `"narratives-narrative_id"`:

```r
updateSelectInput(session, "narratives-narrative_id",
                  selected = input$navigate_to_narrative)
```

This couples `app_server.R` to both the module ID `"narratives"` and the internal `selectInput` ID `"narrative_id"`. If either is renamed, the navigation breaks **silently** — no error at runtime, it simply does nothing.

**Fix:** Use a shared reactive or custom message handler to let the narratives module manage its own input state.

### 1.2 Inconsistent Data Subsetting Style

**Issue:** The codebase mixes base R subsetting and `dplyr` verbs without a clear pattern:
- `fct_real_data.R`: `baselines[baselines$region == region, ]` (base R)
- `mod_scenarios.R` bar plot: `dplyr::filter(year == horizon_year)` (dplyr)
- `mod_dashboard.R`: `df[df$year == max(df$year), ]` (base R)

This is not a bug, but inconsistency hinders readability and makes it harder for contributors to know which style to follow.

**Fix:** Adopt one convention. Since `dplyr` is already a hard dependency, standardising on `dplyr` verbs throughout would improve consistency.

### 1.3 `intervention_choices` Loaded Redundantly

**File:** `app_server.R` (line 20) + `mod_justice.R` (line 62) + `mod_governance.R` (line 103)

**Issue:** Intervention choices are loaded once in `app_server.R`:

```r
intervention_choices <- tryCatch(load_interventions(), error = function(e) "MPA Establishment")
```

Then passed to `mod_justice_server` and `mod_governance_server`. But both modules also have `observe({ ... }) |> bindEvent(TRUE, once = TRUE)` blocks that call `load_interventions()` again internally. The `intervention_choices` parameter is used as a fallback via `%||%`, but the observe still fires once, potentially loading the same data twice.

**Fix:** Remove the duplicate `observe(once = TRUE)` in both modules and rely solely on the passed-in parameter, or remove the parameter and let modules handle their own loading.

### 1.4 `mod_dashboard_server` Not NFF-Aware

**Issue:** Unlike `mod_spatial`, `mod_scenarios`, `mod_stakeholders`, and `mod_pathways`, the Dashboard module does not receive `nff_weights`. This means the indicator dashboard is completely disconnected from NFF governance priorities. Users adjusting weights in other tabs see no change in the dashboard.

**Fix:** Pass `nff_weights` to `mod_dashboard_server` and optionally weight/reorder indicators by NFF perspective relevance.

---

## 2. Data Loading & Caching

### 2.1 `load_nuts2_data()` — Heavy I/O in Reactive Context

**File:** `fct_real_data.R` (lines 5–155), called via `mod_spatial.R` (line 118)

**Issue:** `load_nuts2_data()` performs:
- 1× `readRDS` for geometries
- 1× `readRDS` for indicators cache  
- 1× `read.csv` for sea basin mapping
- 1× `read.csv` for ecosystem type mapping
- Multiple `dplyr::left_join` operations
- Multiple column-fill loops

This runs every time `all_data()` reactive invalidates. Although `reactive()` caches the result, the initial load is expensive and happens on the first visit to the Spatial Equity tab. There is no cross-session caching.

**Fix:** Use `cachem`/`memoryCache` or `bindCache()` to cache the joined sf object across reactive invalidations. Alternatively, pre-join all data in `prepare_data.R` so `load_nuts2_data()` becomes a single `readRDS()`.

### 2.2 `set.seed()` Inside Data Loaders

**Files:** `fct_real_data.R` (lines 51, 109, 119), `fct_fallback_data.R` (multiple locations)

**Issue:** `set.seed(42)` is called inside `load_nuts2_data()` to generate fallback column values. This mutates R's global random number state. In a Shiny server with concurrent sessions, this could cause subtle reproducibility issues, since `set.seed()` affects the entire R process.

Additionally, `load_scenario_data()` uses `set.seed(sum(nff_weights) + nchar(region))`, which means weights with the same sum produce identical trajectories (e.g., `NfN=50, NfS=30, NaC=20` and `NfN=20, NfS=50, NaC=30` both sum to 100).

**Fix:** Use `withr::with_seed()` to scope the RNG state locally. For `load_scenario_data()`, incorporate all three weight components individually in the seed calculation (e.g., `set.seed(nff_weights["NfN"] * 10000 + nff_weights["NfS"] * 100 + nff_weights["NaC"] + nchar(region))`).

### 2.3 Fallback Data Requires Internet

**File:** `fct_fallback_data.R` (line 5)

**Issue:** `mock_nuts2_data_fallback()` calls `rnaturalearth::ne_countries(scale = 50, ...)`. If the cached real data fails **and** the fallback also needs to download from Natural Earth servers, the app will fail entirely in air-gapped environments.

**Fix:** Bundle a minimal geometry fallback as an RDS in `inst/extdata/`, or make `rnaturalearth` a `Suggests` dependency and handle its absence gracefully.

### 2.4 Provenance Attribute Lost on Character Vectors

**File:** `fct_real_data.R` → `load_interventions()` (line 367)

**Issue:** `attr(result, "provenance") <- "csv"` is set on a character vector. R strips attributes from character vectors during many common operations (subsetting, `c()`, etc.). The provenance info is effectively unreliable for this function's return value.

**Fix:** Return a list with `$names` and `$provenance` components instead, or track provenance via a separate mechanism.

---

## 3. Duplicated Logic

### 3.1 NFF Weight Modifiers Duplicated

**Files:** `fct_real_data.R` (lines 231–244) and `fct_fallback_data.R` (lines 100–113)

**Issue:** The NFF weight modifier `switch()` block in `load_scenario_data()` is duplicated almost verbatim in `mock_scenario_data_fallback()`:

```r
# In fct_real_data.R:
weight_mod <- switch(row$indicator,
  "Habitat Condition" = 0.02 * nfn,
  ...
)
# In fct_fallback_data.R:
means <- c(0.02 * nfn, 0.015 * nfs, ...)
```

The coefficients are the same but expressed differently. If one changes, the other must be updated manually.

**Fix:** Extract the weight modifier logic into a shared helper function (e.g., `nff_weight_modifier(indicator, nfn, nfs, nac)`).

### 3.2 HELCOM Indicator Filtering Duplicated

**Files:** `fct_real_data.R` (line 413), `fct_fallback_data.R` (line 291), `mod_scenarios.R` (implicit via data)

**Issue:** The HELCOM-only indicator list `c("Contaminant Status", "Eutrophication Status", "Underwater Noise")` and the "only include for Baltic" logic is repeated in at least three locations.

**Fix:** Define `HELCOM_INDICATORS` as a package-level constant in `utils_helpers.R` and reference it everywhere.

### 3.3 Country-Basin Mapping Duplicated

**File:** `fct_real_data.R` (lines 86–101)

**Issue:** The `country_basins` named vector (mapping 2-letter country codes to sea basins) and `country_names` vector are hardcoded in `load_nuts2_data()`. If countries are added or mappings change, this must be updated alongside the CSV in `inst/extdata/`.

**Fix:** Move country-basin and country-name mappings to a CSV in `inst/extdata/` and load them with `load_extdata()`.

---

## 4. Reactivity & Performance

### 4.1 Leaflet Map Clears & Redraws All Layers on Any Change

**File:** `mod_spatial.R` (lines 217–300)

**Issue:** The `observe({})` block calls `leaflet::clearGroup(all_groups)` then re-adds every active layer whenever any checkbox or filter changes. This causes:
- Visible map flickering
- Unnecessary re-rendering of layers that haven't changed
- Performance degradation with many layers active

**Fix:** Track which layers changed and only update those. Use `leafletProxy` to add/remove specific groups rather than clearing all.

### 4.2 `ggplotly()` Overhead

**Files:** `mod_dashboard.R` (line 136), `mod_spatial.R` (line 324), `mod_scenarios.R` (line 387)

**Issue:** Three plots use `ggplot2::ggplot() |> plotly::ggplotly()` conversion. This pipeline is slower than native plotly because:
1. ggplot builds a full grob tree
2. plotly parses the ggplot object and converts it
3. The resulting plotly JSON is often larger than a natively-built plot

The scenarios projection plot already uses native `plotly::plot_ly()` correctly.

**Fix:** Convert the remaining three `ggplotly()` calls to native `plotly::plot_ly()` for faster rendering and smaller payloads.

### 4.3 Scenario Data Generated for Full 2025–2050 Range

**File:** `fct_real_data.R` (line 220)

**Issue:** `load_scenario_data()` always generates data for all years 2025–2050 (26 data points × number of indicators), even when the user selects a 2030 horizon. The trimming to the horizon year happens later in the rendering code (`data[data$year <= horizon_year, ]`).

**Impact:** Minor — only ~26 rows per indicator, but could be optimised if the function accepted a `horizon` parameter.

### 4.4 `renderUI` for Complex Static-ish Content

**Files:** `mod_pathways.R` (`horizons_analysis`), `mod_justice.R` (`scorecard`, `gap_analysis`), `mod_governance.R` (`tenet_cards`, `tenet_gaps`)

**Issue:** Several `renderUI` blocks generate large HTML trees that rebuild on every input change. For example, Three Horizons analysis rebuilds all three columns with long policy text whenever any NfN/NfS/NaC input changes by 1 unit.

**Fix:** Break large `renderUI` outputs into smaller reactive components, or use `req()` with debouncing to avoid rapid re-renders during slider dragging.

### 4.5 Funding Matrix Wrapped in Unnecessary `reactive()`

**File:** `mod_governance.R` (line 107)

```r
funding_data <- reactive({ load_funding_matrix() })
```

This reactive has no inputs — it evaluates once and caches. While not harmful, it adds reactive overhead. A plain variable assignment would be clearer:

```r
funding_data <- load_funding_matrix()
```

---

## 5. Input Validation & Error Handling

### 5.1 No Bounds Enforcement on Pathway NumericInputs

**File:** `mod_pathways.R` (lines 56–85)

**Issue:** The `numericInput()` widgets for Now and Future NfN/NfS/NaC have `min = 0, max = 100`, but Shiny's `numericInput` does not enforce these bounds — users can type values outside the range. The downstream normalization (`now_pos()`) handles this, but:
- No user feedback that their input was invalid
- Negative values are silently clamped to 0

**Fix:** Add `shiny::validate()` or `req()` checks with user-facing messages, or use `shinyWidgets::numericRangeInput` which enforces bounds.

### 5.2 `mock_cfp_alignment_fallback` Seed Collision

**File:** `fct_fallback_data.R` (line 237)

```r
set.seed(nchar(intervention))
```

**Issue:** Interventions with the same name length produce identical fallback alignment results. For example, "MPA Establishment" (17 chars) and any other 17-char intervention name get the same `set.seed(17)`.

**Fix:** Use `digest::digest(intervention)` or `sum(utf8ToInt(intervention))` for a more unique seed.

### 5.3 Silent Failure in `app_version` Detection

**File:** `app_server.R` (lines 36–46)

**Issue:** The About modal version detection tries `packageVersion()`, then reads DESCRIPTION from `app_sys("..")`. The relative path `..` from `system.file()` is unreliable in installed packages (it may point outside the package directory). This silently falls back to `"0.1.0"`.

**Fix:** Use `utils::packageDescription("NatureJust")$Version` which works reliably for installed packages.

---

## 6. Namespace & Package Metadata

### 6.1 Stale NAMESPACE File

**File:** `NAMESPACE`

**Issue:** The NAMESPACE contains only:

```
importFrom(shiny,NS,tagList)
export(run_app)
```

But multiple R files have `@import shiny` roxygen tags (`app_ui.R`, `app_server.R`, `mod_narratives.R`, `mod_stakeholders.R`, `mod_pathways.R`). If `devtools::document()` is run, it would generate `import(shiny)` which supersedes the `importFrom` — but currently the NAMESPACE appears to have been manually edited or not regenerated after adding roxygen tags.

**Fix:** Run `devtools::document()` to regenerate NAMESPACE from roxygen tags. Ensure all used external functions are either namespace-qualified (e.g., `dplyr::filter()`) or declared via `@importFrom`.

### 6.2 Missing `@importFrom` for Operator `%||%`

**Files:** `mod_justice.R` (line 62), `mod_governance.R` (line 103), `mod_pathways.R` (multiple)

**Issue:** The `%||%` operator is used but never explicitly imported. In R ≥ 4.1 it exists in base, but for older R versions this could fail. The DESCRIPTION doesn't specify an R version dependency.

**Fix:** Add `Depends: R (>= 4.1.0)` to DESCRIPTION or add `@importFrom rlang %||%` (rlang is a transitive dependency via dplyr).

### 6.3 `httr2` and `icesSAG` as `Suggests` but Used in `prepare_data.R`

**File:** `DESCRIPTION` (lines 41–42)

**Issue:** `httr2` and `icesSAG` are listed under `Suggests` but are loaded with `library()` in `prepare_data.R`. The `prepare_data.R` script will fail if these packages are not installed. Since this is a data preparation script (not called by the app at runtime), `Suggests` is semantically correct, but the script should check for their availability before calling `library()`.

**Fix:** Add `requireNamespace("icesSAG", quietly = TRUE)` checks at the top of `prepare_data.R` with informative error messages.

---

## 7. Front-End (JS/CSS)

### 7.1 External Font Dependency

**File:** `inst/app/www/custom.css` (line 8)

```css
@import url('https://fonts.googleapis.com/css2?family=Playfair+Display:...');
```

**Issue:** The CSS loads Google Fonts at runtime. In air-gapped, restricted-network, or GDPR-sensitive deployments, this:
- Fails silently (fonts fall back to system fonts)
- Sends user IP addresses to Google servers

**Fix:** Self-host the font files in `inst/app/www/fonts/` and use `@font-face` declarations. Or switch to system fonts only.

### 7.2 jQuery Dependency Assumed

**File:** `inst/app/www/nff_triangle.js`

**Issue:** The JS file uses `$()` (jQuery) extensively. While Shiny currently bundles jQuery, future versions may not. The code also uses `$(document).ready()` alongside `$(document).one('shiny:connected', ...)`, which is fine but worth noting as a jQuery dependency.

**Fix:** No immediate action required, but consider progressive migration to vanilla JS for future-proofing.

### 7.3 Narrative Data Duplicated in JS and R

**Files:** `nff_triangle.js` (lines 71–113) vs `utils_helpers.R` (`NARRATIVE_PRESETS`) vs `narratives.json`

**Issue:** Narrative names, descriptions, weights, and positions are defined in three places:
1. `NARRATIVE_PRESETS` in R (weights only)
2. `NARRATIVES` object in JS (weights, descriptions, governance models)
3. `narratives.json` (full data)

A comment in `utils_helpers.R` acknowledges this: *"If weights change, update the JS NARRATIVES object too."* This is a maintenance risk.

**Fix:** Generate the JS `NARRATIVES` object server-side from `narratives.json` and inject it as a `<script>` tag, or serve it as a JSON endpoint that the JS fetches.

---

## 8. Testing Gaps

### 8.1 No Integration/End-to-End Tests

**Issue:** The test suite (`test-modules.R`, 378 lines) covers:
- UI construction (returns taglist)
- Data loader outputs (correct classes and columns)
- `testServer()` for individual module reactives

Missing coverage:
- No tests for `app_server.R` cross-module wiring (e.g., navigating to narratives from triangle)
- No tests for NFF weight synchronization across modules
- No snapshot tests for rendered HTML/plots
- No `shinytest2` browser-based tests

**Fix:** Add `shinytest2` tests for at least the critical user flows (set NFF weights → verify scenario update → verify spatial map update).

### 8.2 No Negative/Edge-Case Tests

**Issue:** No tests for:
- Empty data (what happens when all filters exclude all regions?)
- Zero NFF weights (all three at 0)
- Very large stakeholder counts
- Concurrent session isolation

**Fix:** Add edge-case tests, especially for `nff_weights = c(0, 0, 0)` and empty filter results.

---

## 9. Dead Code & Unused Artefacts

### 9.1 `get_golem_config()` Never Called

**File:** `app_config.R` (lines 12–26)

**Issue:** The `get_golem_config()` function exists but is never called anywhere in the codebase. It's a golem template artefact.

**Fix:** Either use it (e.g., for configuring data paths) or remove it.

### 9.2 `ensure_columns()` Used Only Once

**File:** `utils_data_helpers.R` (lines 27–34)

**Issue:** The `ensure_columns()` helper is only called in `load_indicator_timeseries()`. Its generality suggests it was designed for broader use, but other data loaders handle missing columns with ad-hoc code (e.g., the fallback loops in `load_nuts2_data()`).

**Fix:** Apply `ensure_columns()` consistently in all data loaders, or inline it in the one place it's used.

### 9.3 `data/` Directory Exists but Is Empty

**Issue:** There is a `data/` directory in the project root, which in R packages is for lazy-loaded `.rda` files. The DESCRIPTION has `LazyData: true`, but there are no `.rda` files.

**Fix:** Remove `LazyData: true` from DESCRIPTION and delete the empty `data/` directory, or populate it if lazy-loaded data is planned.

---

## 10. Recommendations Summary

### High Priority (Bugs / Silent Failures)

| # | Issue | File(s) | Effort |
|---|-------|---------|--------|
| 1 | Hardcoded `"narratives-narrative_id"` cross-module reference | `app_server.R` | Low |
| 2 | `set.seed()` in global scope inside data loaders | `fct_real_data.R`, `fct_fallback_data.R` | Low |
| 3 | Stale NAMESPACE — run `devtools::document()` | `NAMESPACE` | Low |
| 4 | `app_version` detection uses unreliable `app_sys("..")` | `app_server.R` | Low |

### Medium Priority (Performance)

| # | Issue | File(s) | Effort |
|---|-------|---------|--------|
| 5 | Leaflet map clears/redraws all layers on any checkbox change | `mod_spatial.R` | Medium |
| 6 | `ggplotly()` conversion overhead (3 plots) | `mod_dashboard.R`, `mod_spatial.R`, `mod_scenarios.R` | Medium |
| 7 | `load_nuts2_data()` heavy I/O — add caching | `fct_real_data.R` | Medium |
| 8 | Pre-join spatial data in `prepare_data.R` | `data-raw/prepare_data.R` | Medium |

### Low Priority (Maintainability)

| # | Issue | File(s) | Effort |
|---|-------|---------|--------|
| 9 | Extract NFF weight modifiers into shared helper | `fct_real_data.R`, `fct_fallback_data.R` | Low |
| 10 | Define `HELCOM_INDICATORS` constant | `utils_helpers.R` | Low |
| 11 | Standardise on dplyr or base R subsetting | All R files | Medium |
| 12 | Self-host Google Fonts | `custom.css` | Low |
| 13 | Single-source narrative data (eliminate JS duplication) | `nff_triangle.js`, `utils_helpers.R` | Medium |
| 14 | Add R version dependency `R (>= 4.1.0)` for `%||%` | `DESCRIPTION` | Low |
| 15 | Remove `LazyData: true` and empty `data/` dir | `DESCRIPTION` | Low |
| 16 | Add `shinytest2` integration tests | `tests/` | High |

---

*Generated by codebase audit — NatureJust v0.1.0*
