# NatureJust-EU Shiny App Design

**Date:** 2026-02-21
**Status:** Approved
**Working title:** NatureJust-EU — A Decision-Support Tool for Equitable Biodiversity Governance

## Decisions

- **Phase:** Mock data prototype (simulated data, all modules as UI shells)
- **UI framework:** bslib + bsicons (Bootstrap 5)
- **App framework:** golem (R package structure)
- **Navigation:** bslib::page_navbar with tab panels per module
- **Architecture:** Approach 1 — modular golem with tab-based navigation

## Project Structure

```
NatureJust/
├── DESCRIPTION
├── NAMESPACE
├── R/
│   ├── app_config.R
│   ├── app_server.R
│   ├── app_ui.R
│   ├── run_app.R
│   ├── mod_home.R           # Module 0 — Home & NFF triangle
│   ├── mod_spatial.R        # Module 1 — Spatial Equity Diagnostic
│   ├── mod_scenarios.R      # Module 2 — NFF Scenario Explorer
│   ├── mod_justice.R        # Module 3 — Justice Impact Assessment
│   ├── mod_governance.R     # Module 4 — Governance & Funding
│   ├── mod_dashboard.R      # Module 5 — Indicator Dashboard
│   ├── fct_mock_data.R      # Mock data generators
│   └── utils_helpers.R      # Shared utility functions
├── inst/
│   ├── app/www/
│   │   ├── custom.css
│   │   └── nff_triangle.js
│   └── golem-config.yml
├── data-raw/
├── data/
├── tests/testthat/
├── Dockerfile
└── NatureJust.Rproj
```

## Main UI

```r
bslib::page_navbar(
  title = "NatureJust-EU",
  theme = bslib::bs_theme(version = 5, bootswatch = "flatly",
                           primary = "#2c7fb8", success = "#41ae76"),
  nav_panel("Home",           mod_home_ui("home")),
  nav_panel("Spatial Equity", mod_spatial_ui("spatial")),
  nav_panel("Scenarios",      mod_scenarios_ui("scenarios")),
  nav_panel("Justice",        mod_justice_ui("justice")),
  nav_panel("Governance",     mod_governance_ui("governance")),
  nav_panel("Indicators",     mod_dashboard_ui("dashboard"))
)
```

## Module Designs

### Module 0 — Home & Navigation
- Brief description of NatureJust-EU
- Interactive NFF triangle (SVG) with clickable vertices and edges
- Clicking a vertex switches to the most relevant tab via JS
- Tutorial/explanation of the NFF-GBF-Justice framework

### Module 1 — Spatial Equity Diagnostic
- Leaflet map centered on Europe with mock layers:
  - Natura 2000 / MPA polygons (simplified from rnaturalearth)
  - Socio-economic vulnerability choropleth (NUTS2, random 0-1)
  - Fisheries dependency layer (random values)
- Filter sidebar: country, sea basin, ecosystem type
- Overlap scatter plot (plotly): vulnerability vs. conservation pressure

### Module 2 — NFF Scenario Explorer
- Ternary input: 3 sliders constrained to sum to 100% (NfN, NfS, NaC)
- Scenario output cards: 4 mock trend lines per region/time horizon
- Comparison mode: up to 4 scenarios as radar chart + stacked bar
- GBF compliance traffic lights

### Module 3 — Justice Impact Assessment
- Intervention selector: dropdown of ~10 mock interventions
- Justice scorecard: 4-quadrant cards with traffic-light scores
- Gap analysis: text output with mock policy recommendations
- Download: HTML/PDF report via rmarkdown

### Module 4 — Governance & Funding Alignment
- DT::datatable of intervention types vs. EU funding instruments
- CFP alignment checker with conflict/alignment flags
- Stakeholder inclusion checklist

### Module 5 — Indicator Dashboard
- Region selector + indicator time series (plotly with confidence bands)
- GBF compliance traffic-light table
- Equity trend panel: paired ecological vs. equity line charts
- CSV data export

## Dependencies

### Core
shiny, bslib, bsicons, golem, leaflet, sf, plotly, ggplot2,
dplyr, tidyr, purrr, DT, shinyWidgets

### Mock data
rnaturalearth, rnaturalearthdata

### Reports
rmarkdown, knitr

### Optional (later phases)
ggtern, eurostat, duckdb, dbplyr, waiter

## Deployment
- Dockerfile for Docker deployment
- shinyapps.io via golem rsconnect integration
- renv lockfile for reproducibility

## Testing
- testthat skeleton with one smoke test per module
