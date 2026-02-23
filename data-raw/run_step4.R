library(sf)
library(dplyr)

project_root <- tryCatch(here::here(), error = function(e) getwd())
if (!file.exists(file.path(project_root, "DESCRIPTION"))) {
  candidate <- getwd()
  while (!file.exists(file.path(candidate, "DESCRIPTION")) &&
         candidate != dirname(candidate)) {
    candidate <- dirname(candidate)
  }
  project_root <- candidate
}
extdata_dir <- file.path(project_root, "inst", "extdata")

cat("=== Running Step 4: Natura 2000 MPA download ===\n")
cat("Output directory:", extdata_dir, "\n\n")

# Download Natura 2000 sites from EEA via the official GeoPackage
# Source: https://sdi.eea.europa.eu/datashare/s/mwzs9eNsJ9Sn4Q4 (end-2024 dataset)
n2k_url <- "https://sdi.eea.europa.eu/datashare/s/mwzs9eNsJ9Sn4Q4/download?path=/&files=Natura2000_end2024.gpkg"

# Use a fixed temp path so the file persists if R is interrupted
gpkg_path <- file.path(tempdir(), "Natura2000_end2024.gpkg")

if (file.exists(gpkg_path) && file.size(gpkg_path) > 1e9) {
  cat("Using previously downloaded file:", gpkg_path, "\n")
  cat("Size:", round(file.size(gpkg_path) / 1e6), "MB\n")
} else {
  cat("Downloading Natura2000_end2024.gpkg from EEA (~1.3 GB)...\n")
  old_timeout <- getOption("timeout")
  options(timeout = 1800)  # 30 minutes for large file
  download.file(n2k_url, gpkg_path, mode = "wb", quiet = FALSE)
  options(timeout = old_timeout)
  cat("Downloaded:", round(file.size(gpkg_path) / 1e6), "MB\n")
}

# --- Read & filter marine sites ---
# The GeoPackage has a relational structure:
#   NaturaSite_polygon  = geometries (SITECODE, SITENAME, MS, SITETYPE, geom)
#   NATURA2000SITES     = attributes (SITECODE, MARINE_AREA_PERCENTAGE, ...)
# Read tabular data first to identify marine sites before loading heavy geometries.

cat("\nReading NATURA2000SITES attribute table...\n")
n2k_attrs <- sf::st_read(gpkg_path, layer = "NATURA2000SITES", quiet = TRUE)
cat("Total sites:", nrow(n2k_attrs), "\n")

marine_codes <- n2k_attrs |>
  filter(!is.na(MARINE_AREA_PERCENTAGE), MARINE_AREA_PERCENTAGE > 0) |>
  pull(SITECODE)
cat("Marine/coastal sites:", length(marine_codes), "of", nrow(n2k_attrs), "total\n")

# Read only marine site polygons using SQL (much faster than all 27k)
cat("Reading marine site polygons via SQL...\n")
code_list <- paste0("'", marine_codes, "'", collapse = ", ")
sql <- paste0("SELECT * FROM NaturaSite_polygon WHERE SITECODE IN (", code_list, ")")
n2k_marine <- sf::st_read(gpkg_path, query = sql, quiet = TRUE)
cat("Polygons read:", nrow(n2k_marine), "\n")

# Join MARINE_AREA_PERCENTAGE from attributes
marine_attr_df <- n2k_attrs |>
  filter(SITECODE %in% marine_codes) |>
  select(SITECODE, MARINE_AREA_PERCENTAGE) |>
  as.data.frame()
if ("geometry" %in% names(marine_attr_df)) marine_attr_df$geometry <- NULL
n2k_marine <- merge(n2k_marine, marine_attr_df, by = "SITECODE", all.x = TRUE)

# Transform to WGS84
cat("Transforming to WGS84...\n")
n2k_marine <- sf::st_transform(n2k_marine, 4326)

# --- Aggressive GEOS simplification ---
# Disable s2 to use GEOS planar Douglas-Peucker (preserveTopology=FALSE).
# dTolerance = 0.01° (≈1.1 km) reduces ~180 MB → ~0.8 MB while keeping 93% of sites.
# The s2 spherical engine always preserves topology, preventing effective reduction.
cat("Simplifying with GEOS (dTol=0.01°, planar)...\n")
sf::sf_use_s2(FALSE)
n2k_marine <- sf::st_simplify(n2k_marine, preserveTopology = FALSE, dTolerance = 0.01)
n2k_marine <- n2k_marine[!sf::st_is_empty(n2k_marine), ]
n2k_marine <- sf::st_make_valid(n2k_marine)
sf::sf_use_s2(TRUE)  # restore default
cat("After simplification:", nrow(n2k_marine), "sites\n")

# Map SITETYPE codes to display names
if ("SITETYPE" %in% names(n2k_marine)) {
  n2k_marine$designation <- dplyr::case_when(
    n2k_marine$SITETYPE == "A" ~ "Natura 2000 - SPA",
    n2k_marine$SITETYPE == "B" ~ "Natura 2000 - SAC/SCI",
    n2k_marine$SITETYPE == "C" ~ "Natura 2000 - SPA + SAC/SCI",
    TRUE ~ paste0("Natura 2000 - ", n2k_marine$SITETYPE)
  )
} else {
  n2k_marine$designation <- "Natura 2000"
}

# Rename for app compatibility
if ("SITENAME" %in% names(n2k_marine)) n2k_marine$name <- n2k_marine$SITENAME
if ("MS" %in% names(n2k_marine)) n2k_marine$country <- n2k_marine$MS

# Select final columns
keep_cols <- intersect(
  c("SITECODE", "SITENAME", "SITETYPE", "MS", "MARINE_AREA_PERCENTAGE",
    "designation", "name", "country"),
  names(n2k_marine)
)
n2k_marine <- n2k_marine[, c(keep_cols, attr(n2k_marine, "sf_column"))]

saveRDS(n2k_marine, file.path(extdata_dir, "natura2000_marine.rds"))
cat("\nSaved natura2000_marine.rds:", nrow(n2k_marine), "marine sites\n")
cat("File size:", round(file.size(file.path(extdata_dir, "natura2000_marine.rds")) / 1e6, 1), "MB\n")

# Show distributions
cat("\nDesignation distribution:\n")
print(table(n2k_marine$designation))
cat("\nTop 10 countries by marine site count:\n")
print(head(sort(table(n2k_marine$country), decreasing = TRUE), 10))

# Cleanup
unlink(gpkg_path)
cat("\n=== Step 4 complete ===\n")
