# NFF Cross-Module Wiring & Research-Informed Enhancements

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Connect the NFF triangle to all modules via a shared reactive, add an NFF-weighted spatial composite, SSP presets, and narrative-to-scenario linking — transforming the triangle from decorative to functional.

**Architecture:** A single `nff_weights` reactive is created in `app_server.R` and passed to all 6 module servers. The Home triangle and Scenarios sliders bidirectionally sync via custom Shiny messages. The Spatial module gains an NFF-weighted composite choropleth. The Scenarios module gains SSP and narrative preset buttons.

**Tech Stack:** R/Shiny (golem), bslib, leaflet, plotly, jQuery, custom SVG/JS

---

## Phase 1: Cross-Module NFF Reactive

### Task 1: Create shared `nff_weights` reactive in `app_server.R`

**Files:**
- Modify: `R/app_server.R:6-12`

**Step 1: Add a shared reactive and pass it to all module servers**

Replace lines 6–12 of `app_server.R`:

```r
app_server <- function(input, output, session) {
  # ---- Shared NFF weights reactive ----
  # Writeable from Home triangle and Scenarios sliders
  nff_weights <- reactiveVal(c(NfN = 34, NfS = 33, NaC = 33))

  mod_home_server("home", nff_weights = nff_weights)
  mod_spatial_server("spatial", nff_weights = nff_weights)
  mod_scenarios_server("scenarios", nff_weights = nff_weights)
  mod_justice_server("justice")
  mod_governance_server("governance")
  mod_dashboard_server("dashboard")
```

Note: Justice and Governance don't need NFF weights yet (no composite scores). Dashboard could be wired later.

**Step 2: Verify no syntax errors**

Run: `Rscript -e "parse('R/app_server.R')"`
Expected: No errors (just parse, not execute — avoids loading dependencies)

**Step 3: Commit**

```bash
git add R/app_server.R
git commit -m "feat: create shared nff_weights reactiveVal in app_server"
```

---

### Task 2: Wire Home module to read/write shared `nff_weights`

**Files:**
- Modify: `R/mod_home.R:216-226` (server function)

**Step 1: Accept and use the shared reactive**

Replace the `mod_home_server` function:

```r
mod_home_server <- function(id, nff_weights = NULL) {
  moduleServer(id, function(input, output, session) {
    # Receive NFF position from JS triangle interaction
    observeEvent(input$nff_position, {
      pos <- input$nff_position
      if (!is.null(pos) && !is.null(nff_weights)) {
        nff_weights(c(
          NfN = as.integer(pos$NfN),
          NfS = as.integer(pos$NfS),
          NaC = as.integer(pos$NaC)
        ))
      }
    })

    # If external code changes nff_weights, update the JS triangle
    observe({
      if (is.null(nff_weights)) return()
      w <- nff_weights()
      session$sendCustomMessage("nff-update-position", list(
        NfN = w[["NfN"]], NfS = w[["NfS"]], NaC = w[["NaC"]]
      ))
    })
  })
}
```

Key points:
- `observeEvent(input$nff_position, ...)` writes to the shared reactive when the triangle is clicked.
- `observe({ ... session$sendCustomMessage(...) })` pushes external weight changes (e.g., from Scenarios sliders) back to the triangle JS.
- `nff_weights = NULL` default preserves backward compatibility if the module is used standalone.

**Step 2: Add the JS message handler in `nff_triangle.js`**

Add inside the `initTriangle()` function, after the narrative click handler block (end of file, before the closing `}`):

```javascript
  // ---- Receive weight updates from server (bidirectional sync) ----
  if (typeof Shiny !== 'undefined') {
    Shiny.addCustomMessageHandler('nff-update-position', function(msg) {
      // Avoid feedback loop: only update if significantly different
      var cur = baryCoords(
        parseFloat(posMarker.getAttribute('cx')),
        parseFloat(posMarker.getAttribute('cy'))
      );
      cur = clampBary(cur);
      var dNfN = Math.abs(cur.NfN - msg.NfN / 100);
      var dNfS = Math.abs(cur.NfS - msg.NfS / 100);
      var dNaC = Math.abs(cur.NaC - msg.NaC / 100);
      if (dNfN + dNfS + dNaC > 0.02) {
        updatePosition({
          NfN: msg.NfN / 100,
          NfS: msg.NfS / 100,
          NaC: msg.NaC / 100
        }, false);  // false = don't send back to Shiny (avoid loop)
      }
    });
  }
```

**Step 3: Verify parse**

Run: `Rscript -e "parse('R/mod_home.R')"`

**Step 4: Commit**

```bash
git add R/mod_home.R inst/app/www/nff_triangle.js
git commit -m "feat: wire Home triangle to shared nff_weights reactive"
```

---

### Task 3: Wire Scenarios module to read/write shared `nff_weights`

**Files:**
- Modify: `R/mod_scenarios.R:76-100`

**Step 1: Accept shared reactive + bidirectional sync with sliders**

Replace the server function signature and weights section:

```r
mod_scenarios_server <- function(id, nff_weights = NULL) {
  moduleServer(id, function(input, output, session) {

    # ---- Bidirectional NFF sync ----
    # Local flag to prevent feedback loops
    updating_from_external <- reactiveVal(FALSE)

    # When shared nff_weights changes externally, update sliders
    observe({
      if (is.null(nff_weights)) return()
      w <- nff_weights()
      # Only update if sliders are out of sync
      if (!isTRUE(all.equal(
        c(input$nfn, input$nfs, input$nac),
        c(w[["NfN"]], w[["NfS"]], w[["NaC"]])
      ))) {
        updating_from_external(TRUE)
        updateSliderInput(session, "nfn", value = w[["NfN"]])
        updateSliderInput(session, "nfs", value = w[["NfS"]])
        updateSliderInput(session, "nac", value = w[["NaC"]])
        # Reset flag after update cycle
        shiny::invalidateLater(200)
        updating_from_external(FALSE)
      }
    })

    # Normalised weights (from sliders)
    weights <- reactive({
      total <- input$nfn + input$nfs + input$nac
      if (total == 0) total <- 1
      c(NfN = round(input$nfn / total * 100),
        NfS = round(input$nfs / total * 100),
        NaC = round(input$nac / total * 100))
    })

    # When sliders change (user-driven), push to shared reactive
    observe({
      if (is.null(nff_weights)) return()
      if (isTRUE(updating_from_external())) return()
      w <- weights()
      nff_weights(w)
    })

    # ... rest of server unchanged from line 88 onward ...
```

The remaining server code (`output$weight_sum`, `current_data`, `saved`, plots, etc.) stays exactly as-is — it already uses `weights()` which continues to work.

**Step 2: Verify parse**

Run: `Rscript -e "parse('R/mod_scenarios.R')"`

**Step 3: Commit**

```bash
git add R/mod_scenarios.R
git commit -m "feat: wire Scenarios sliders to shared nff_weights reactive"
```

---

### Task 4: Wire Spatial module to receive `nff_weights`

**Files:**
- Modify: `R/mod_spatial.R:109` (server signature only — composite layer comes in Phase 2)

**Step 1: Accept the reactive parameter (no-op for now)**

Change line 109:

```r
mod_spatial_server <- function(id, nff_weights = NULL) {
```

The reactive is now available inside the module but not yet used. Phase 2 Task 6 adds the composite layer.

**Step 2: Commit**

```bash
git add R/mod_spatial.R
git commit -m "feat: accept nff_weights param in spatial module (prep for composite)"
```

---

### Task 5: Launch and verify bidirectional sync

**Verification procedure:**

1. Launch app: `Rscript dev/launch.R`
2. On Home page, click inside triangle near NfN vertex → weights should update readout to ~NfN 70%
3. Navigate to Scenarios tab → sliders should show matching values (~70/15/15)
4. Move the NfS slider to 60 → navigate back to Home → triangle position marker should have moved toward bottom-right
5. Console: no errors
6. No infinite update loops (weights stabilise immediately)

**Step 1: Launch and test**

Run: `Rscript dev/launch.R`
Open: http://127.0.0.1:7701

**Step 2: Commit verification note**

```bash
git commit --allow-empty -m "chore: verify bidirectional NFF sync (Home ↔ Scenarios)"
```

---

## Phase 2: NFF-Weighted Spatial Composite

### Task 6: Add NFF composite choropleth layer to the map

**Files:**
- Modify: `R/mod_spatial.R:31` (add checkbox to UI)
- Modify: `R/mod_spatial.R:109-210` (server: compute composite, add layer)

**Step 1: Add composite checkbox to UI sidebar**

After the `hr()` on line 31 and before the "Map Layers" `h6()` on line 32, insert:

```r
      h6("NFF Composite"),
      shinyWidgets::prettyCheckbox(
        ns("show_nff_composite"), "NFF-Weighted Equity Index",
        value = TRUE, status = "success"
      ),
      hr(),
```

**Step 2: Add composite calculation and layer to server**

Inside `mod_spatial_server`, after `all_data` and `mpas` reactives (around line 116), add:

```r
    # NFF-weighted composite equity index
    composite_data <- reactive({
      data <- regions()
      if (nrow(data) == 0 || is.null(nff_weights)) return(data)
      w <- nff_weights()
      nfn <- w[["NfN"]] / 100
      nfs <- w[["NfS"]] / 100
      nac <- w[["NaC"]] / 100

      # NfN indicators: ecological/conservation
      nfn_score <- rowMeans(data.frame(
        data$vulnerability,  # inverse GDP = ecological priority proxy
        data$mpa_coverage,
        data$bathing_quality
      ), na.rm = TRUE)

      # NfS indicators: economic/instrumental
      nfs_score <- rowMeans(data.frame(
        data$blue_economy_jobs,
        data$offshore_wind,
        data$fisheries_dep
      ), na.rm = TRUE)

      # NaC indicators: relational/cultural
      nac_score <- rowMeans(data.frame(
        data$coastal_tourism,
        data$aquaculture,
        1 - data$income_disparity  # equity = inverse disparity
      ), na.rm = TRUE)

      data$nff_composite <- nfn * nfn_score + nfs * nfs_score + nac * nac_score
      data$nff_composite <- round(data$nff_composite, 3)
      data
    })
```

Then in the `observe()` block that renders layers (around line 176), add composite layer rendering. Replace `data <- regions()` with `data <- composite_data()` on line 177, and add after the indicator layers loop (after the `for (ldef in layer_defs)` block ends):

```r
      # NFF composite layer
      if (isTRUE(input$show_nff_composite) && "nff_composite" %in% names(data)) {
        pal <- leaflet::colorNumeric(
          palette = c("#E07A5F", "#F2CC8F", "#0E7C7B"),
          domain = c(0, 1)
        )
        proxy <- proxy |>
          leaflet::addPolygons(
            data = data,
            fillColor = pal(data$nff_composite),
            fillOpacity = 0.7,
            weight = 1,
            color = "#333",
            label = ~paste0(
              ifelse(!is.null(data$NUTS_NAME) & !is.na(data$NUTS_NAME),
                     data$NUTS_NAME, sovereignt),
              " — NFF Equity: ", round(nff_composite, 2),
              " (NfN=", round(nff_weights()[["NfN"]]), "%",
              " NfS=", round(nff_weights()[["NfS"]]), "%",
              " NaC=", round(nff_weights()[["NaC"]]), "%)"),
            group = "NFF Composite"
          ) |>
          leaflet::addLegend(
            position = "bottomright",
            pal = pal, values = data$nff_composite,
            title = "NFF Equity Index",
            group = "NFF Composite"
          )
      }
```

Also update `all_groups` to include `"NFF Composite"`:

```r
    all_groups <- c("NFF Composite", vapply(layer_defs, `[[`, "", "group"), "MPAs")
```

**Step 3: Verify parse**

Run: `Rscript -e "parse('R/mod_spatial.R')"`

**Step 4: Commit**

```bash
git add R/mod_spatial.R
git commit -m "feat: add NFF-weighted composite equity layer to spatial map"
```

---

### Task 7: Verify spatial composite responds to triangle

**Verification:**

1. Launch app, navigate to Home, click near NfN vertex (high Nature for Nature)
2. Navigate to Spatial Equity → map should show composite coloured by ecological indicators
3. Navigate back to Home, click near NfS vertex (high Nature for Society)
4. Go to Spatial Equity → colours should shift (economic indicators now weighted higher)
5. The map tooltip should show the current NFF weights alongside the composite score

---

## Phase 3: SSP Presets (Alexander et al. 2023)

### Task 8: Add SSP preset buttons to Scenarios sidebar

**Files:**
- Modify: `R/mod_scenarios.R:11-19` (UI: add presets after weight summary)
- Modify: `R/mod_scenarios.R` (server: handle preset clicks)

**Step 1: Add preset UI**

In `mod_scenarios_ui`, after the `verbatimTextOutput(ns("weight_sum"))` on line 20, add:

```r
      hr(),
      h6("Scenario Presets"),
      p(class = "text-muted small",
        "Based on SSP\u2013NFF mapping (Alexander et al. 2023)"),
      div(
        class = "d-grid gap-1",
        actionButton(ns("preset_ssp1"), "SSP1 Sustainability",
                     class = "btn-outline-primary btn-sm"),
        actionButton(ns("preset_ssp2"), "SSP2 Middle of Road",
                     class = "btn-outline-secondary btn-sm"),
        actionButton(ns("preset_ssp3"), "SSP3 Regional Rivalry",
                     class = "btn-outline-secondary btn-sm"),
        actionButton(ns("preset_ssp4"), "SSP4 Inequality",
                     class = "btn-outline-secondary btn-sm"),
        actionButton(ns("preset_ssp5"), "SSP5 Fossil-Fueled",
                     class = "btn-outline-secondary btn-sm")
      ),
      hr(),
      h6("NFF Narratives"),
      p(class = "text-muted small",
        "Marine governance archetypes (Dur\u00e1n et al. 2023)"),
      div(
        class = "d-grid gap-1",
        actionButton(ns("preset_arcology"), "Arcology",
                     class = "btn-outline-primary btn-sm"),
        actionButton(ns("preset_sharing"), "Sharing through Sparing",
                     class = "btn-outline-primary btn-sm"),
        actionButton(ns("preset_optimizing"), "Optimizing Nature",
                     class = "btn-outline-primary btn-sm"),
        actionButton(ns("preset_commons"), "Innovative Commons",
                     class = "btn-outline-primary btn-sm"),
        actionButton(ns("preset_stewardship"), "Reciprocal Stewardship",
                     class = "btn-outline-primary btn-sm"),
        actionButton(ns("preset_dynamic"), "Dynamic Natures",
                     class = "btn-outline-primary btn-sm")
      ),
```

**Step 2: Add preset handlers in server**

Add after the `updating_from_external` block in the server:

```r
    # ---- SSP presets (Alexander et al. 2023) ----
    ssp_presets <- list(
      preset_ssp1 = c(NfN = 40, NfS = 30, NaC = 30),  # Sustainability
      preset_ssp2 = c(NfN = 34, NfS = 33, NaC = 33),  # Middle of Road
      preset_ssp3 = c(NfN = 15, NfS = 20, NaC = 65),  # Regional Rivalry
      preset_ssp4 = c(NfN = 10, NfS = 70, NaC = 20),  # Inequality
      preset_ssp5 = c(NfN = 5,  NfS = 85, NaC = 10)   # Fossil-Fueled
    )

    # ---- NFF narrative presets (Durán et al. 2023) ----
    narrative_presets <- list(
      preset_arcology    = c(NfN = 100, NfS = 0,  NaC = 0),
      preset_sharing     = c(NfN = 50,  NfS = 50, NaC = 0),
      preset_optimizing  = c(NfN = 0,   NfS = 100, NaC = 0),
      preset_commons     = c(NfN = 0,   NfS = 50, NaC = 50),
      preset_stewardship = c(NfN = 0,   NfS = 0,  NaC = 100),
      preset_dynamic     = c(NfN = 50,  NfS = 0,  NaC = 50)
    )

    all_presets <- c(ssp_presets, narrative_presets)

    lapply(names(all_presets), function(btn_id) {
      observeEvent(input[[btn_id]], {
        w <- all_presets[[btn_id]]
        if (!is.null(nff_weights)) {
          nff_weights(w)
        } else {
          updateSliderInput(session, "nfn", value = w[["NfN"]])
          updateSliderInput(session, "nfs", value = w[["NfS"]])
          updateSliderInput(session, "nac", value = w[["NaC"]])
        }
      })
    })
```

**Step 3: Verify parse**

Run: `Rscript -e "parse('R/mod_scenarios.R')"`

**Step 4: Commit**

```bash
git add R/mod_scenarios.R
git commit -m "feat: add SSP and NFF narrative presets to Scenarios sidebar"
```

---

### Task 9: Final verification — full integration test

**Verification checklist:**

| # | Test | Expected |
|---|------|----------|
| 1 | Launch app on port 7701 | No errors |
| 2 | Home: click inside triangle near NfN | Readout shows ~NfN 70% |
| 3 | Navigate to Scenarios | Sliders auto-update to ~70/15/15 |
| 4 | Scenarios: click "SSP5 Fossil-Fueled" | Sliders jump to 5/85/10 |
| 5 | Navigate to Home | Triangle position marker near NfS corner |
| 6 | Navigate to Spatial Equity | NFF Composite layer colours reflect NfS-heavy weighting |
| 7 | Scenarios: click "Arcology" preset | Sliders jump to 100/0/0 |
| 8 | Navigate to Spatial Equity | Map colours shift (ecological indicators now dominant) |
| 9 | Home: click narrative diamond "Innovative Commons" | Triangle snaps to NfS–NaC edge |
| 10 | Scenarios tab | Sliders match 0/50/50 |
| 11 | Dark mode toggle | All elements adapt |
| 12 | Console | Zero errors |

**Step 1: Run full verification**

Run: `Rscript dev/launch.R`

**Step 2: Commit**

```bash
git add -A
git commit -m "feat: complete NFF cross-module wiring with SSP presets and spatial composite"
```

---

## Phase 4 (Future): Research Frontier Enhancements

These phases are documented for future implementation. Each would be its own plan document.

### 4A: Pareto frontier visualisation (Haga et al. 2023)
- **What:** Run 100+ stochastic simulations per weight config, compute Pareto envelope
- **Where:** New function in `R/fct_real_data.R`, new plotly scatter in `R/mod_scenarios.R`
- **Blocker:** Compute cost — may need async/background processing via `promises`/`future`

### 4B: Participatory multi-stakeholder positioning (Rana et al. 2020; IPBES 2025)
- **What:** Multiple users submit triangle positions; aggregate as heatmap/point cloud
- **Where:** New module `R/mod_participatory.R`, persistent SQLite or JSON storage
- **Blocker:** Multi-session state management; potential WebSocket or shared-file approach

### 4C: Freshwater indicator expansion (Kramer et al. 2023)
- **What:** Add freshwater biodiversity, wetland extent, river connectivity, WFD compliance
- **Where:** New section in `data-raw/prepare_data.R`, additional columns in `nuts2_indicators_cache.rds`
- **Blocker:** Data sourcing from EEA WISE (Water Information System for Europe)

### 4D: Pathway visualisation on triangle (Kim et al. 2023)
- **What:** Animated dotted lines on SVG triangle showing governance trajectory over decades
- **Where:** `inst/app/www/nff_triangle.js` — new `drawPathway()` function, `R/mod_home.R` to push saved scenario waypoints
- **Blocker:** Requires temporal NFF weight sequences (currently only static configs per scenario)

### 4E: Three Horizons temporal overlay (IPBES 2025 Methodological Guidance)
- **What:** Visual overlay showing H1 (declining present), H2 (emerging innovation), H3 (desired future)
- **Where:** New tab in Scenarios or Home module
- **Blocker:** Requires mapping real indicator data back to NFF coordinates (inverse problem)

---

## Key Publications Referenced

| Author | Year | Relevance to this plan |
|--------|------|----------------------|
| Pereira et al. | 2020 | Original NFF triangle design (Phase 1 visual) |
| Durán et al. | 2023 | 6 illustrative narratives → preset buttons (Phase 3) |
| Alexander et al. | 2023 | SSP–NFF mapping → preset buttons (Phase 3) |
| Haga et al. | 2023 | Multi-objective optimisation → Pareto frontiers (Phase 4A) |
| Kim et al. | 2023 | Quantitative NFF operationalisation → pathway viz (Phase 4D) |
| Pereira et al. | 2023 | Marine NFF application → scenario indicators |
| Kramer et al. | 2023 | Freshwater NFF indicators → expansion (Phase 4C) |
| Rana et al. | 2020 | Participatory visioning → multi-stakeholder mode (Phase 4B) |
| IPBES | 2025 | NFF Methodological Guidance → Three Horizons (Phase 4E) |

---

## File Change Summary

| File | Phase | Change |
|------|-------|--------|
| `R/app_server.R` | 1 | Create `nff_weights` reactiveVal, pass to modules |
| `R/mod_home.R` | 1 | Accept reactive, write on triangle click, receive external updates |
| `inst/app/www/nff_triangle.js` | 1 | Add `Shiny.addCustomMessageHandler` for bidirectional sync |
| `R/mod_scenarios.R` | 1+3 | Accept reactive, sync sliders, add SSP + narrative presets |
| `R/mod_spatial.R` | 1+2 | Accept reactive, compute NFF composite, render choropleth |
