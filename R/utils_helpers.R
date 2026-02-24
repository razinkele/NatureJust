#' Create a traffic light HTML span
#' @param status Character: "green", "amber", or "red"
#' @return shiny.tag
#' @noRd
traffic_light <- function(status) {
  htmltools::tags$span(class = paste("traffic-light", status))
}

#' NFF colour constants (single source of truth)
#' @noRd
NFF_COLORS <- list(
  NfN = "#0E7C7B",
  NfS = "#2A6F97",
  NaC = "#E07A5F"
)

#' HELCOM-only indicators — only valid for Baltic region
#' Single source of truth used by fct_real_data, fct_fallback_data, mod_scenarios
#' @noRd
HELCOM_INDICATORS <- c("Contaminant Status", "Eutrophication Status", "Underwater Noise")

#' NFF narrative preset weights (single source of truth)
#' Used by mod_pathways and mod_scenarios. At session start, app_server.R sends
#' these to the JS runtime via the 'set-narratives' custom message handler, so
#' nff_triangle.js is automatically kept in sync — no manual JS update needed.
#' @noRd
NARRATIVE_PRESETS <- list(
  arcology    = c(NfN = 100, NfS = 0,   NaC = 0),
  sharing     = c(NfN = 50,  NfS = 50,  NaC = 0),
  optimizing  = c(NfN = 0,   NfS = 100, NaC = 0),
  commons     = c(NfN = 0,   NfS = 50,  NaC = 50),
  stewardship = c(NfN = 0,   NfS = 0,   NaC = 100),
  dynamic     = c(NfN = 50,  NfS = 0,   NaC = 50),
  balanced    = c(NfN = 34,  NfS = 33,  NaC = 33)
)

#' Compute NFF weight modifier for an indicator
#'
#' Shared helper used by both load_scenario_data() and mock_scenario_data_fallback().
#' Encapsulates the NFF weight coefficients so they are defined in one place.
#'
#' @param indicator Character indicator name
#' @param nfn Numeric NfN weight as fraction (0–1)
#' @param nfs Numeric NfS weight as fraction (0–1)
#' @param nac Numeric NaC weight as fraction (0–1)
#' @return Numeric modifier
#' @noRd
nff_weight_modifier <- function(indicator, nfn, nfs, nac) {
  switch(indicator,
    "Habitat Condition"          = 0.02 * nfn,
    "Ecosystem Services"         = 0.015 * nfs,
    "Livelihoods & Employment"   = 0.01 * nac - 0.005 * nfn,
    "Equity Score"               = 0.01 * (1 - max(abs(nfn - nfs), abs(nfs - nac), abs(nfn - nac))),
    "Offshore Wind Capacity"     = 0.012 * nfs + 0.005 * nac,
    "Bathing Water Quality"      = 0.008 * nfn + 0.005 * nfs,
    "Contaminant Status"         = 0.010 * nfn + 0.003 * nfs,
    "Eutrophication Status"      = 0.008 * nfn + 0.005 * nfs,
    "Underwater Noise"           = -0.003 * nfs + 0.005 * nfn,
    0  # default: no NFF weight modification for unknown indicators
  )
}

#' Generate NFF weight badge set (colored pill spans)
#' @param w Named numeric vector with NfN, NfS, NaC keys (percentages)
#' @return shiny.tag (div with 3 badge spans)
#' @noRd
nff_badge_set <- function(w) {
  htmltools::div(
    class = "d-flex gap-2 flex-wrap",
    htmltools::tags$span(
      class = "badge rounded-pill",
      style = paste0("background-color: ", NFF_COLORS$NfN, "; font-size: 0.85rem;"),
      paste0("NfN ", w[["NfN"]], "%")
    ),
    htmltools::tags$span(
      class = "badge rounded-pill",
      style = paste0("background-color: ", NFF_COLORS$NfS, "; font-size: 0.85rem;"),
      paste0("NfS ", w[["NfS"]], "%")
    ),
    htmltools::tags$span(
      class = "badge rounded-pill",
      style = paste0("background-color: ", NFF_COLORS$NaC, "; font-size: 0.85rem;"),
      paste0("NaC ", w[["NaC"]], "%")
    )
  )
}

#' Convert traffic light status to human-readable label
#' @param status Character: "green", "amber", or "red"
#' @return Character label
#' @noRd
status_to_label <- function(status) {
  switch(status,
         green = "Adequate",
         amber = "Needs Attention",
         red = "Critical Gap",
         "Unknown")
}

#' Build a display name for a NUTS2 region
#' Falls back to sovereignt when NUTS_NAME is missing.
#' @param data sf or data.frame with optional NUTS_NAME and sovereignt columns
#' @return Character vector of display names
#' @noRd
region_display_name <- function(data) {
  if ("NUTS_NAME" %in% names(data)) {
    ifelse(is.na(data$NUTS_NAME), data$sovereignt, data$NUTS_NAME)
  } else {
    data$sovereignt
  }
}

#' Generate NFF triangle SVG markup
#'
#' Shared helper for the triangle widgets (Home, Stakeholders, Pathways, Narratives).
#' Each instance gets a unique suffix for SVG element IDs.
#'
#' @param suffix Character suffix for SVG IDs (e.g. "h", "s", "p", "n")
#' @param mode Character: "home" (narratives + clickable vertices),
#'   "stakeholder" (stakeholder dot group), "pathway" (pathway elements),
#'   "narrative" (read-only with narrative markers highlighted)
#' @return HTML string
#' @noRd
nff_triangle_svg <- function(suffix, mode = "home") {
  nfn <- NFF_COLORS$NfN
  nfs <- NFF_COLORS$NfS
  nac <- NFF_COLORS$NaC

  # Defs block (gradients, clip path, optional glow filter)
  defs <- paste0('
    <defs>
      <clipPath id="nff-tri-clip-', suffix, '">
        <polygon points="200,35 365,335 35,335"/>
      </clipPath>
      <radialGradient id="nff-grad-nfn-', suffix, '" cx="200" cy="35" r="200"
                      gradientUnits="userSpaceOnUse">
        <stop offset="0%"  stop-color="', nfn, '" stop-opacity="0.7"/>
        <stop offset="45%" stop-color="', nfn, '" stop-opacity="0.25"/>
        <stop offset="100%" stop-color="', nfn, '" stop-opacity="0"/>
      </radialGradient>
      <radialGradient id="nff-grad-nfs-', suffix, '" cx="365" cy="335" r="200"
                      gradientUnits="userSpaceOnUse">
        <stop offset="0%"  stop-color="', nfs, '" stop-opacity="0.7"/>
        <stop offset="45%" stop-color="', nfs, '" stop-opacity="0.25"/>
        <stop offset="100%" stop-color="', nfs, '" stop-opacity="0"/>
      </radialGradient>
      <radialGradient id="nff-grad-nac-', suffix, '" cx="35" cy="335" r="200"
                      gradientUnits="userSpaceOnUse">
        <stop offset="0%"  stop-color="', nac, '" stop-opacity="0.7"/>
        <stop offset="45%" stop-color="', nac, '" stop-opacity="0.25"/>
        <stop offset="100%" stop-color="', nac, '" stop-opacity="0"/>
      </radialGradient>',
      if (mode != "home") paste0('
      <filter id="nff-marker-glow-', suffix, '" x="-50%" y="-50%" width="200%" height="200%">
        <feGaussianBlur in="SourceGraphic" stdDeviation="3"/>
      </filter>') else "", '
    </defs>')

  # Gradient background
  gradient_bg <- paste0('
    <g clip-path="url(#nff-tri-clip-', suffix, ')" class="nff-gradient-bg">
      <polygon points="200,35 365,335 35,335" fill="#f0eeeb" opacity="0.5"/>
      <polygon points="200,35 365,335 35,335" fill="url(#nff-grad-nfn-', suffix, ')"/>
      <polygon points="200,35 365,335 35,335" fill="url(#nff-grad-nfs-', suffix, ')"/>
      <polygon points="200,35 365,335 35,335" fill="url(#nff-grad-nac-', suffix, ')"/>
    </g>
    <polygon class="nff-tri-outline" points="200,35 365,335 35,335" fill="none"/>')

  # Edge labels (Home only)
  edge_labels <- if (mode == "home") '
    <text class="edge-label" x="270" y="193"
          text-anchor="middle" transform="rotate(61,270,193)">
      Biodiversity &amp; Livelihoods</text>
    <text class="edge-label" x="130" y="193"
          text-anchor="middle" transform="rotate(-61,130,193)">
      Cultural &amp; Ecological</text>
    <text class="edge-label" x="200" y="320"
          text-anchor="middle">Social &amp; Cultural Values</text>' else ""

  # Narrative markers (Home and Narrative modes)
  narratives <- if (mode %in% c("home", "narrative")) '
    <polygon class="nff-narrative-marker" data-narrative="arcology"
             points="0,-7 7,0 0,7 -7,0" transform="translate(200,65)"/>
    <circle class="nff-narrative-hit" data-narrative="arcology"
            cx="200" cy="65" r="14" fill="transparent"
            tabindex="0" role="button" aria-label="Arcology narrative"/>
    <polygon class="nff-narrative-marker" data-narrative="optimizing"
             points="0,-7 7,0 0,7 -7,0" transform="translate(339,320)"/>
    <circle class="nff-narrative-hit" data-narrative="optimizing"
            cx="339" cy="320" r="14" fill="transparent"
            tabindex="0" role="button" aria-label="Optimizing Nature narrative"/>
    <polygon class="nff-narrative-marker" data-narrative="stewardship"
             points="0,-7 7,0 0,7 -7,0" transform="translate(61,320)"/>
    <circle class="nff-narrative-hit" data-narrative="stewardship"
            cx="61" cy="320" r="14" fill="transparent"
            tabindex="0" role="button" aria-label="Reciprocal Stewardship narrative"/>
    <polygon class="nff-narrative-marker" data-narrative="sharing"
             points="0,-7 7,0 0,7 -7,0" transform="translate(282.5,185)"/>
    <circle class="nff-narrative-hit" data-narrative="sharing"
            cx="282.5" cy="185" r="14" fill="transparent"
            tabindex="0" role="button" aria-label="Sharing through Sparing narrative"/>
    <polygon class="nff-narrative-marker" data-narrative="commons"
             points="0,-7 7,0 0,7 -7,0" transform="translate(200,335)"/>
    <circle class="nff-narrative-hit" data-narrative="commons"
            cx="200" cy="335" r="14" fill="transparent"
            tabindex="0" role="button" aria-label="Innovative Commons narrative"/>
    <polygon class="nff-narrative-marker" data-narrative="dynamic"
             points="0,-7 7,0 0,7 -7,0" transform="translate(117.5,185)"/>
    <circle class="nff-narrative-hit" data-narrative="dynamic"
            cx="117.5" cy="185" r="14" fill="transparent"
            tabindex="0" role="button" aria-label="Dynamic Natures narrative"/>' else ""

  # Vertex glow rings
  glow_rings <- '
    <circle class="vertex-glow" cx="200" cy="35" r="20"/>
    <circle class="vertex-glow" cx="365" cy="335" r="20" style="animation-delay:1.2s"/>
    <circle class="vertex-glow" cx="35"  cy="335" r="20" style="animation-delay:2.4s"/>'

  # Vertices (interactive for Home, static for Stakeholder/Pathway)
  vertices <- if (mode == "home") '
    <circle class="nff-vertex" data-target="Spatial Equity"
            cx="200" cy="35" r="11"
            tabindex="0" role="button"
            aria-label="Nature for Nature - navigate to Spatial Equity"/>
    <circle class="nff-vertex" data-target="Scenarios"
            cx="365" cy="335" r="11"
            tabindex="0" role="button"
            aria-label="Nature for Society - navigate to Scenarios"/>
    <circle class="nff-vertex" data-target="Justice"
            cx="35"  cy="335" r="11"
            tabindex="0" role="button"
            aria-label="Nature as Culture - navigate to Justice"/>' else '
    <circle class="nff-vertex-static" cx="200" cy="35" r="11" fill="#1B4965" opacity="0.7"/>
    <circle class="nff-vertex-static" cx="365" cy="335" r="11" fill="#1B4965" opacity="0.7"/>
    <circle class="nff-vertex-static" cx="35"  cy="335" r="11" fill="#1B4965" opacity="0.7"/>'

  # Vertex labels
  vlabels <- '
    <text class="vertex-label" x="200" y="16" text-anchor="middle">Nature for Nature</text>
    <text class="vertex-label" x="365" y="365" text-anchor="middle">Nature for Society</text>
    <text class="vertex-label" x="35" y="365" text-anchor="middle">Nature as Culture</text>'

  # Mode-specific group
  mode_group <- switch(mode,
    stakeholder = '
    <g class="stakeholder-dots"></g>',
    pathway = '
    <!-- Pathway elements will be inserted by JS -->',
    narrative = '',
    ""
  )

  paste0(
    '<svg class="nff-svg" viewBox="-50 0 500 400"',
    ' xmlns="http://www.w3.org/2000/svg" role="img"',
    ' aria-label="', switch(mode,
      home = "Interactive Nature Futures Framework triangle",
      stakeholder = "Stakeholder NFF positioning triangle",
      pathway = "Pathway NFF positioning triangle",
      narrative = "Narrative NFF positioning triangle"), '">',
    defs, gradient_bg, edge_labels, narratives,
    glow_rings, vertices, vlabels, mode_group,
    '</svg>'
  )
}
