# Data Sources

## GFCM Stock Data (Manual Update)

The file `inst/extdata/gfcm_stocks.csv` contains curated fish stock assessment
data for the Mediterranean and Black Sea from GFCM.

### How to update

1. Visit https://www.fao.org/gfcm/data/safs/en/ (Stock Assessment Forms)
2. Download the latest stock summaries for Mediterranean and Black Sea
3. For each basin-year, compute:
   - **Fish Stock Biomass**: median(B/Bmsy) across assessed stocks, normalized 0-1
   - **Sustainable Fishing**: proportion of stocks where F <= Fmsy, normalized 0-1
4. Update `inst/extdata/gfcm_stocks.csv` with new rows
5. Re-run `data-raw/prepare_data.R` to rebuild the cache

### Additional GFCM data sources
- Fleet statistics: https://www.fao.org/gfcm/data/fleet/en/
- GFCM Data Collection Reference Framework (DCRF)
