# ICES + HELCOM + GFCM Data Integration Design

**Date:** 2026-02-23
**Status:** Draft — awaiting approval

## Summary

Integrate three authoritative marine data sources into NatureJust-EU's
indicator pipeline to replace Eurostat-proxy and modelled indicators with
assessment-grade data from the bodies that actually manage EU sea basins.

| Source | Coverage | Access Method | R Package |
|--------|----------|---------------|-----------|
| **ICES** (SAG + DATRAS) | NE Atlantic, Baltic, North Sea | REST API | `icesSAG`, `icesDatras` (CRAN) |
| **HELCOM** (HOLAS III) | Baltic Sea | ArcGIS REST API | None — use `httr2` + `jsonlite` |
| **GFCM** | Mediterranean, Black Sea | Manual CSV download | None — curated CSV in `inst/extdata/` |

---

## 1. Current State

The app currently has **10 time series indicators** in the Dashboard, sourced from:

| Indicator | Current Source | Quality |
|-----------|---------------|---------|
| Marine Biodiversity Index | Modelled (rnorm + regional baselines) | Synthetic |
| Habitat Condition | Modelled | Synthetic |
| Ecosystem Services | Modelled | Synthetic |
| Community Wellbeing Index | Modelled | Synthetic |
| Governance Effectiveness | Modelled | Synthetic |
| Fish Stock Biomass | Eurostat sdg_14_21 (where available) | Real but coarse (EU-wide) |
| Sustainable Fishing | Eurostat sdg_14_30 (overfished %) | Real but coarse |
| Offshore Wind Capacity | Eurostat nrg_inf_epcrw | Real |
| Coastal Tourism Pressure | Eurostat tour_occ_nin2c | Real |
| Bathing Water Quality | Eurostat sdg_14_40 | Real |

**Problem:** The first 5 indicators are entirely modelled from hardcoded baselines.
Fish Stock Biomass/Sustainable Fishing use EU-wide Eurostat aggregates with
regional noise added — not actual per-basin stock assessments.

---

## 2. Target State

### 2.1 Improved existing indicators

| Indicator | New Source | Method |
|-----------|-----------|--------|
| Fish Stock Biomass | **ICES SAG** (F/Fmsy, SSB per stock, per ecoregion) | Aggregate stocks by sea basin |
| Sustainable Fishing | **ICES SAG** (proportion of stocks fished at or below Fmsy) | Compute basin-level % |
| Habitat Condition (Baltic) | **HELCOM HOLAS III** integrated biodiversity status | Direct mapping |
| Marine Biodiversity Index (Baltic) | **HELCOM HOLAS III** biodiversity indicators | Direct mapping |
| Fish Stock Biomass (Med/Black Sea) | **GFCM** stock summaries | Manual CSV |
| Sustainable Fishing (Med/Black Sea) | **GFCM** overexploitation rates | Manual CSV |

### 2.2 New indicators (from HELCOM)

| New Indicator | Source | Theme |
|---------------|--------|-------|
| Contaminant Status | HELCOM HOLAS III hazardous substances assessment | Pressures |
| Eutrophication Status | HELCOM HOLAS III eutrophication assessment | Pressures |
| Underwater Noise | HELCOM pre-core noise indicators | Pressures |

These appear as new rows in the Dashboard time series picker, bringing the total
from 10 to **13 indicators**.

### 2.3 Updated scenario baselines

Replace hardcoded values in `scenario_baselines.csv` with ICES/HELCOM/GFCM
assessment values where available. The `source` column already exists — just
update the values and source citations.

---

## 3. Architecture

### 3.1 Data flow

```
prepare_data.R
  ├── Section 2: Eurostat NUTS2 indicators (unchanged)
  ├── Section 3: Time series
  │     ├── 3a: ICES fish stocks → basin-aggregated time series
  │     ├── 3b: HELCOM HOLAS III indicators → Baltic time series
  │     ├── 3c: GFCM manual CSV → Med/Black Sea fish time series
  │     ├── 3d: Eurostat TS (wind, tourism, bathing — unchanged)
  │     └── 3e: Modelled indicators (biodiversity, ecosystem services
  │             for non-Baltic basins — keep as modelled with updated
  │             baselines from OSPAR QSR / UNEP-MAP)
  └── Section 4: Natura 2000 MPA boundaries (unchanged)
```

### 3.2 Sea basin → source mapping

| Sea Basin | Fish Stocks | Environmental Indicators |
|-----------|-------------|--------------------------|
| Baltic | ICES SAG (ecoregion: Baltic Sea) | **HELCOM HOLAS III** (all themes) |
| North Sea | ICES SAG (ecoregion: Greater North Sea) | OSPAR QSR (modelled baselines) |
| Atlantic | ICES SAG (ecoregion: Celtic Seas, Bay of Biscay) | OSPAR QSR (modelled baselines) |
| Mediterranean | **GFCM CSV** | UNEP-MAP MED QSR (modelled baselines) |
| Black Sea | **GFCM CSV** | BSC State of Environment (modelled baselines) |

**Key design decision:** HELCOM indicators only apply to the Baltic basin.
For other basins, we keep the modelled approach but update baselines from
the relevant regional convention's latest assessment (OSPAR, UNEP-MAP, BSC).
The new HELCOM-only indicators (Contaminant Status, Eutrophication Status,
Underwater Noise) show `NA` for non-Baltic basins — the Dashboard already
handles missing indicators gracefully via the picker filter.

### 3.3 ICES stock → basin aggregation

ICES stock assessments are per-stock (e.g., `cod.27.24-32` = Eastern Baltic cod).
We need to:

1. Query all published stocks for the latest advice year via `icesSAG::getListStocks()`
2. Map each stock's `EcoRegion` field to our 5 sea basins
3. For each basin-year, compute:
   - **Fish Stock Biomass** = median(SSB / SSBmsy) across stocks
   - **Sustainable Fishing** = proportion of stocks where F <= Fmsy
4. Normalize both to 0–1 scale for consistency with other indicators

Mapping table:

| ICES EcoRegion | App Sea Basin |
|----------------|---------------|
| Baltic Sea | Baltic |
| Greater North Sea | North Sea |
| Celtic Seas | Atlantic |
| Bay of Biscay and the Iberian Coast | Atlantic |
| Oceanic Northeast Atlantic | Atlantic |
| Azores | Atlantic |

### 3.4 HELCOM ArcGIS REST integration

Query the MADS MapServer layers for HOLAS III results:

```
GET https://maps.helcom.fi/arcgis/rest/services/MADS/
    Indicators_and_assessments/MapServer/{LayerID}/query
    ?where=1=1&outFields=*&f=json
```

Key layers to query:

| Layer | Description | Maps to indicator |
|-------|-------------|-------------------|
| Integrated biodiversity status | Per-sub-basin assessment | Habitat Condition, Marine Biodiversity Index |
| Integrated eutrophication status | HEAT tool results | Eutrophication Status (NEW) |
| Integrated contamination status | Hazardous substances | Contaminant Status (NEW) |
| Continuous underwater noise | Sound levels | Underwater Noise (NEW) |

Each query returns JSON with assessment status per HELCOM sub-basin.
We aggregate to a single "Baltic" value (mean or median of sub-basin scores).

### 3.5 GFCM manual CSV format

Create `inst/extdata/gfcm_stocks.csv` with this schema:

```csv
year,basin,indicator,value,source
2015,Mediterranean,Fish Stock Biomass,0.42,GFCM SAF 2023
2016,Mediterranean,Fish Stock Biomass,0.44,GFCM SAF 2023
...
2015,Black Sea,Fish Stock Biomass,0.38,GFCM SAF 2023
...
2015,Mediterranean,Sustainable Fishing,0.35,GFCM SAF 2023
...
```

Document download instructions in `data-raw/README.md` pointing to:
- https://www.fao.org/gfcm/data/safs/en/ (Stock Assessment Forms)
- https://www.fao.org/gfcm/data/fleet/en/ (Fleet statistics)

---

## 4. New R Package Dependencies

| Package | Purpose | Source |
|---------|---------|--------|
| `icesSAG` | ICES stock assessment data | CRAN |
| `icesDatras` | ICES trawl survey data (optional, for future species indices) | CRAN |
| `httr2` | HTTP requests to HELCOM ArcGIS REST | CRAN |
| `jsonlite` | Parse JSON from HELCOM API | CRAN (likely already installed) |

**Note:** `httr2` is preferred over `httr` (newer, pipe-friendly, automatic
retry/rate-limiting).

---

## 5. Dashboard UI Changes

### 5.1 Indicator picker update

Add 3 new entries to `mod_dashboard_ui`:

```
"Contaminant Status (H)"       -- (H) = HELCOM, Baltic only
"Eutrophication Status (H)"    -- (H) = HELCOM, Baltic only
"Underwater Noise (H)"         -- (H) = HELCOM, Baltic only
```

The existing `(M)` suffix means "Modelled". Add a legend:
- `(M)` = Modelled from regional assessment baselines
- `(H)` = HELCOM HOLAS III assessment (Baltic only)
- Unmarked = Eurostat / ICES / GFCM time series

### 5.2 Source attribution in provenance footer

Update the provenance footer to show per-source breakdown:
- "Data sources: ICES SAG, HELCOM HOLAS III, Eurostat, GFCM"
- When viewing Baltic: emphasize HELCOM
- When viewing Med/Black Sea: note GFCM

### 5.3 GBF compliance table

Add GBF target rows in `gbf_targets.csv` for the 3 new indicators:

| Indicator | GBF Target | Source |
|-----------|------------|--------|
| Contaminant Status | 0.80 | MSFD D8 Good Environmental Status |
| Eutrophication Status | 0.75 | MSFD D5 Good Environmental Status |
| Underwater Noise | 0.70 | MSFD D11 threshold values |

---

## 6. Scenario Module Impact

### 6.1 Updated baselines

Replace values in `scenario_baselines.csv` with real assessment data where
ICES/HELCOM/GFCM provide them. Keep the stochastic projection engine
(`rnorm + cumsum`) — just anchor it to real baseline values.

### 6.2 New scenario indicators

Add the 3 new HELCOM indicators to scenario projections for Baltic only.
Other basins keep their current indicator set (no HELCOM data available).

This means `load_scenario_data()` returns **6 indicators for non-Baltic**
and **up to 9 for Baltic** (existing 6 + Contaminant, Eutrophication, Noise).

---

## 7. Error Handling & Fallbacks

Every new data source follows the established `load_*()` → `mock_*_fallback()`
pattern:

| Source | Failure Mode | Fallback |
|--------|-------------|----------|
| ICES SAG API | Network error, API change | Use cached `ices_stocks_cache.rds` from last successful run |
| HELCOM REST API | Network error, layer ID change | Use cached `helcom_holas3_cache.rds` |
| GFCM CSV | File missing | Use current Eurostat proxy (sdg_14_21/sdg_14_30) |

The `prepare_data.R` script caches all API results as RDS files.
The app only reads from the cache — never calls APIs at runtime.

---

## 8. Files Changed

| File | Change |
|------|--------|
| `data-raw/prepare_data.R` | Add sections 3a (ICES), 3b (HELCOM), 3c (GFCM) |
| `data-raw/README.md` | NEW: Document GFCM manual download steps |
| `inst/extdata/gfcm_stocks.csv` | NEW: Curated GFCM stock data |
| `inst/extdata/gbf_targets.csv` | Add 3 new indicator targets |
| `inst/extdata/scenario_baselines.csv` | Update baselines with ICES/HELCOM values |
| `inst/extdata/ices_stocks_cache.rds` | NEW: Cached ICES SAG aggregated data |
| `inst/extdata/helcom_holas3_cache.rds` | NEW: Cached HELCOM indicator data |
| `R/fct_real_data.R` | Update `load_indicator_timeseries()` to merge all sources |
| `R/mod_dashboard.R` | Add 3 indicators to picker, update provenance footer |
| `R/mod_scenarios.R` | Handle variable indicator count per basin |
| `DESCRIPTION` | Add icesSAG, httr2 to Imports |
| `tests/testthat/test-modules.R` | Add tests for new data sources |

---

## 9. What Stays Modelled (By Design)

Even with full integration, these remain modelled for non-Baltic basins:

- **Marine Biodiversity Index** (no pan-European assessment API)
- **Ecosystem Services** (IPBES provides narrative assessments, not time series)
- **Community Wellbeing Index** (no marine-specific time series exists)
- **Governance Effectiveness** (qualitative, not quantifiable from APIs)

For Baltic, HELCOM provides real data for the first two. The social/governance
indicators remain modelled everywhere — this is appropriate since they are
inherently normative and context-dependent.

---

## 10. Implementation Order

1. **ICES fish stocks** — highest value, mature R package, well-documented API
2. **GFCM CSV** — manual but straightforward, fills the Med/Black Sea gap
3. **HELCOM indicators** — most complex (REST API parsing), but transforms Baltic data quality
4. **Dashboard UI updates** — new indicators, updated provenance
5. **Scenario baseline updates** — ground projections in real assessment values
6. **Tests and verification**
