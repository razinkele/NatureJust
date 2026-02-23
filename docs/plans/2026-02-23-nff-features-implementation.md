# NFF Research-Informed Features — Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Implement the Tier 1 features from the NFF research roadmap — Narrative Deep-Dive Panel, Participatory Stakeholder Positioning, and Three Horizons Pathway Visualization — to transform NatureJust-EU from a demonstration tool into a research-grade participatory decision-support platform.

**Architecture:** Each feature is a new Shiny module (`mod_narratives.R`, `mod_stakeholders.R`, `mod_pathways.R`) wired into the existing `bslib::page_navbar` with access to the shared `nff_weights` reactiveVal. The narrative data is stored in `inst/extdata/narratives.json` and loaded by a helper function. JavaScript enhancements are added to the existing `nff_triangle.js`. All features follow the existing golem module pattern (UI + server functions, `NS()` namespacing, `moduleServer()`).

**Tech Stack:** R/Shiny (golem), bslib (Bootstrap 5), bsicons, plotly, jsonlite, existing nff_triangle.js

---

## Phase 1: Marine Narrative Deep-Dive Panel

> Based on Durán et al. (2023) illustrative narratives and Pereira et al. (2023) "The Living Infinite"

### Task 1: Create Narrative Data File

**Files:**
- Create: `inst/extdata/narratives.json`

**Step 1: Create the narrative data file**

Create `inst/extdata/narratives.json` with full structured content for each of the 6 narratives. Each narrative contains: id, name, nff_position, barycentric weights, description, governance_model, key_policies (array), marine_examples (array), trade_offs (array), gbf_targets_prioritized (array of target numbers), and source references.

```json
[
  {
    "id": "arcology",
    "name": "Arcology",
    "nff_position": "Nature for Nature corner",
    "weights": { "NfN": 100, "NfS": 0, "NaC": 0 },
    "description": "A future where biodiversity protection is the primary governance objective. High seas are designated as extensive marine protected areas with comprehensive no-take zones. Human extractive activities are minimized in favour of ecosystem recovery and intrinsic species rights. Conservation is valued as an end in itself, not merely for the services it provides.",
    "governance_model": "Strict preservation governance with strong enforcement mechanisms. International treaties prioritize biodiversity over economic exploitation. Scientific ecological assessments drive all management decisions.",
    "key_policies": [
      "Comprehensive no-take marine protected areas covering >50% of EEZ",
      "Complete ban on deep-sea mining and bottom trawling",
      "Rewilding programmes for degraded marine ecosystems",
      "Biodiversity courts for enforcement of ecological crimes"
    ],
    "marine_examples": [
      "Papahānaumokuākea Marine National Monument (USA) — 1.5M km² no-take",
      "Chagos Archipelago MPA (UK) — full no-take with scientific monitoring only",
      "Ross Sea MPA (Antarctica) — CCAMLR-managed, research-focused"
    ],
    "trade_offs": [
      "Significant reduction in commercial fisheries employment",
      "High enforcement costs for vast ocean areas",
      "Potential food security impacts in fish-dependent communities",
      "Tension with developing nations' rights to marine resources"
    ],
    "gbf_targets_prioritized": [3, 1, 2, 4],
    "source": "Durán et al. (2023); Pereira et al. (2020)"
  },
  {
    "id": "sharing",
    "name": "Sharing through Sparing",
    "nff_position": "NfN–NfS edge",
    "weights": { "NfN": 50, "NfS": 50, "NaC": 0 },
    "description": "A future balancing biodiversity conservation with sustainable resource use through spatial separation. Strictly protected 'spared' zones alternate with intensively managed 'shared' production zones. Fish stocks are rebuilt to maximum sustainable yield through science-based quotas, while biodiversity hotspots receive full protection.",
    "governance_model": "Evidence-based marine spatial planning with clear zoning. Scientific stock assessments determine harvest levels. Independent monitoring ensures compliance.",
    "key_policies": [
      "30x30 target implemented through science-based zoning",
      "MSY-based fisheries quotas with real-time monitoring",
      "Bycatch reduction technology mandates",
      "Ecosystem-based fisheries management (EBFM) frameworks"
    ],
    "marine_examples": [
      "Australian Great Barrier Reef Marine Park — multi-use zoning",
      "EU Common Fisheries Policy reformed quota system",
      "Iceland's ITQ fisheries management system"
    ],
    "trade_offs": [
      "Zoning can create spatial inequities (who gets protected vs. production zones)",
      "Requires robust enforcement infrastructure",
      "May undervalue cultural and relational connections to sea",
      "Risk of 'ocean grabbing' by industrial fleets in production zones"
    ],
    "gbf_targets_prioritized": [3, 10, 5, 14],
    "source": "Durán et al. (2023); Alexander et al. (2023)"
  },
  {
    "id": "optimizing",
    "name": "Optimizing Nature",
    "nff_position": "Nature for Society corner",
    "weights": { "NfN": 0, "NfS": 100, "NaC": 0 },
    "description": "A techno-optimist future where all marine systems are managed for maximum human benefit. Precision aquaculture, offshore energy, and biotechnology dominate. Nature is valued primarily for the ecosystem services it provides. Advanced monitoring and AI-driven management optimize resource extraction while maintaining minimal ecological thresholds.",
    "governance_model": "Market-driven governance with technology-enabled regulation. Cost-benefit analysis determines all management decisions. Public-private partnerships fund innovation.",
    "key_policies": [
      "Blue economy growth targets with GDP-linked marine indicators",
      "Offshore wind and wave energy industrial zones",
      "Precision aquaculture and mariculture expansion",
      "Ecosystem service payment schemes and blue carbon markets"
    ],
    "marine_examples": [
      "Norway's integrated ocean management plan — oil, fish, and wind",
      "Singapore's port-ecosystem optimization model",
      "China's mariculture mega-farms and offshore platforms"
    ],
    "trade_offs": [
      "Biodiversity reduced to utilitarian metrics",
      "Cultural connections to the sea commodified or lost",
      "Technology lock-in risks and energy-intensive management",
      "Equity concerns: benefits concentrate in technology-rich nations"
    ],
    "gbf_targets_prioritized": [9, 10, 11, 16],
    "source": "Durán et al. (2023)"
  },
  {
    "id": "commons",
    "name": "Innovative Commons",
    "nff_position": "NfS–NaC edge",
    "weights": { "NfN": 0, "NfS": 50, "NaC": 50 },
    "description": "A future where marine resources are governed as shared commons through participatory institutions. Communities combine traditional knowledge with modern ecological science for co-management. Fisheries cooperatives, community-based conservation, and social enterprises replace both state-controlled and market-driven models.",
    "governance_model": "Polycentric governance with nested community institutions (Ostrom-inspired). Decisions made through deliberative processes including all stakeholder groups. Blend of traditional and scientific knowledge systems.",
    "key_policies": [
      "Territorial use rights in fisheries (TURFs) for coastal communities",
      "Co-management agreements between government and fisher cooperatives",
      "Community-based marine conservation areas (CCAs)",
      "Social enterprise models for sustainable marine livelihoods"
    ],
    "marine_examples": [
      "Chile's TURF-reserve system — community fisheries + no-take areas",
      "Japan's cooperative fisheries management (gyogyō kyōdō kumiai)",
      "Fiji's locally managed marine areas (LMMAs)"
    ],
    "trade_offs": [
      "Scaling challenges: works best at local/regional level",
      "Requires strong social capital and trust",
      "May conflict with industrial-scale economic interests",
      "Coordination costs increase with number of stakeholders"
    ],
    "gbf_targets_prioritized": [22, 21, 5, 9],
    "source": "Durán et al. (2023); Ostrom (1990)"
  },
  {
    "id": "stewardship",
    "name": "Reciprocal Stewardship",
    "nff_position": "Nature as Culture corner",
    "weights": { "NfN": 0, "NfS": 0, "NaC": 100 },
    "description": "A future rooted in deep cultural and spiritual relationships with the sea. Indigenous and traditional ecological knowledge (TEK) guides marine governance. Small-scale artisanal fisheries, traditional aquaculture systems, and cultural seascapes are maintained through heritage governance. The sea is understood as a living entity with rights.",
    "governance_model": "Heritage-based governance grounded in indigenous and local knowledge. Customary marine tenure systems. Rights of nature frameworks. Intergenerational stewardship obligations.",
    "key_policies": [
      "Legal recognition of indigenous marine tenure and customary rights",
      "Rights of nature legislation for marine ecosystems",
      "Cultural seascape protection (UNESCO-style designations)",
      "Traditional ecological knowledge integration in all marine assessments"
    ],
    "marine_examples": [
      "New Zealand's Te Awa Tupua — river with legal personhood (model for marine)",
      "Torres Strait Islander sea country management (Australia)",
      "Sámi coastal fishing rights in Nordic countries"
    ],
    "trade_offs": [
      "Difficult to scale beyond culturally coherent communities",
      "Tension with modernization and economic development pressures",
      "Knowledge transmission gaps as elders pass",
      "Limited applicability in urbanized, multicultural coastal zones"
    ],
    "gbf_targets_prioritized": [21, 22, 3, 13],
    "source": "Durán et al. (2023); Pereira et al. (2023)"
  },
  {
    "id": "dynamic",
    "name": "Dynamic Natures",
    "nff_position": "NaC–NfN edge",
    "weights": { "NfN": 50, "NfS": 0, "NaC": 50 },
    "description": "A future integrating ecological conservation with biocultural heritage. Ecosystem-based management is informed by indigenous ecological knowledge. Traditional fishing rights are respected alongside scientific conservation goals. Nature and culture are understood as co-evolving — protecting one requires protecting the other.",
    "governance_model": "Biocultural conservation governance. Two-eyed seeing (Etuaptmumk) blending Western science and indigenous knowledge. Adaptive co-management with ecological and cultural indicators.",
    "key_policies": [
      "Biocultural heritage areas combining natural and cultural protection",
      "Ecosystem-based management informed by traditional ecological knowledge",
      "Adaptive co-management with both scientific and cultural monitoring",
      "Protected area governance by indigenous and local communities (ICCA)"
    ],
    "marine_examples": [
      "Great Bear Sea initiative (Canada) — First Nations and conservation partnership",
      "Hawaiian Humpback Whale National Marine Sanctuary — cultural integration",
      "Mediterranean small-scale fisheries with traditional practices"
    ],
    "trade_offs": [
      "Requires genuine power-sharing, not tokenistic consultation",
      "Potential conflicts between scientific and traditional assessments",
      "Resource-intensive governance (multiple knowledge systems to maintain)",
      "May be instrumentalized by conservation organizations"
    ],
    "gbf_targets_prioritized": [3, 21, 1, 22],
    "source": "Durán et al. (2023); Greenway (2022)"
  }
]
```

**Step 2: Verify the JSON is valid**

Run: `Rscript -e "jsonlite::fromJSON('inst/extdata/narratives.json') |> str()"`

Expected: Prints a data.frame of 6 observations with all expected columns.

**Step 3: Commit**

```bash
git add inst/extdata/narratives.json
git commit -m "feat: add structured narrative data (Durán et al. 2023)"
```

---

### Task 2: Create Narrative Data Loader

**Files:**
- Modify: `R/fct_real_data.R` — add `load_narratives()` function
- Modify: `tests/testthat/test-modules.R` — add narrative data tests

**Step 1: Write the test**

Add to `tests/testthat/test-modules.R`:

```r
test_that("load_narratives returns 6 narratives with required fields", {
  narr <- load_narratives()
  expect_equal(nrow(narr), 6)
  expect_true(all(c("id", "name", "nff_position", "description",
                     "governance_model", "key_policies", "marine_examples",
                     "trade_offs", "gbf_targets_prioritized", "source") %in% names(narr)))
  expect_true("arcology" %in% narr$id)
  expect_true("stewardship" %in% narr$id)
})

test_that("load_narratives includes NFF weight columns", {
  narr <- load_narratives()
  # Weights are nested in the JSON but should be available
  expect_true("weights" %in% names(narr) || all(c("NfN", "NfS", "NaC") %in% names(narr)))
})
```

**Step 2: Run tests to verify they fail**

Run: `Rscript -e "testthat::test_file('tests/testthat/test-modules.R')"`

Expected: FAIL — `load_narratives` not found.

**Step 3: Implement `load_narratives()`**

Add to end of `R/fct_real_data.R`:

```r
#' Load narrative data (Durán et al. 2023)
#'
#' Returns a data.frame of 6 illustrative NFF narratives with
#' structured governance, policy, and marine example data.
#'
#' @return data.frame with columns: id, name, nff_position, weights,
#'   description, governance_model, key_policies, marine_examples,
#'   trade_offs, gbf_targets_prioritized, source
#' @noRd
load_narratives <- function() {
  tryCatch({
    path <- load_extdata("narratives.json")
    narr <- jsonlite::fromJSON(path, simplifyDataFrame = TRUE)
    attr(narr, "provenance") <- "json"
    narr
  }, error = function(e) {
    # Minimal fallback with just the 6 IDs and names
    data.frame(
      id = c("arcology", "sharing", "optimizing", "commons", "stewardship", "dynamic"),
      name = c("Arcology", "Sharing through Sparing", "Optimizing Nature",
               "Innovative Commons", "Reciprocal Stewardship", "Dynamic Natures"),
      nff_position = c("Nature for Nature corner", "NfN-NfS edge",
                        "Nature for Society corner", "NfS-NaC edge",
                        "Nature as Culture corner", "NaC-NfN edge"),
      description = rep("Narrative data unavailable. See Durán et al. (2023).", 6),
      governance_model = rep("", 6),
      source = rep("Durán et al. (2023)", 6),
      stringsAsFactors = FALSE
    )
  })
}
```

**Step 4: Run tests to verify they pass**

Run: `Rscript -e "testthat::test_file('tests/testthat/test-modules.R')"`

Expected: All new tests PASS.

**Step 5: Commit**

```bash
git add R/fct_real_data.R tests/testthat/test-modules.R
git commit -m "feat: add load_narratives() data loader with fallback"
```

---

### Task 3: Create Narrative Deep-Dive Module UI

**Files:**
- Create: `R/mod_narratives.R`

**Step 1: Create the module file with UI function**

Create `R/mod_narratives.R`:

```r
#' Narratives module UI
#'
#' Marine Narrative Deep-Dive panel showing detailed governance
#' implications for each of the 6 illustrative NFF narratives
#' (Durán et al. 2023).
#'
#' @param id Module namespace id
#' @noRd
mod_narratives_ui <- function(id) {

  ns <- NS(id)

  tagList(
    div(
      class = "narratives-hero",
      h2("Marine Governance Narratives"),
      p(class = "text-muted",
        "Six illustrative narratives for nature futures in marine governance",
        tags$br(),
        tags$small("Based on Durán et al. (2023) and Pereira et al. (2023) 'The Living Infinite'"))
    ),

    bslib::layout_column_wrap(
      width = 1/2,

      # Left: Narrative selector + NFF mini-triangle
      bslib::card(
        bslib::card_header(
          class = "bg-primary text-white",
          "Select Narrative"
        ),
        bslib::card_body(
          selectInput(ns("narrative_id"), "Choose a narrative:",
                      choices = c(
                        "Arcology" = "arcology",
                        "Sharing through Sparing" = "sharing",
                        "Optimizing Nature" = "optimizing",
                        "Innovative Commons" = "commons",
                        "Reciprocal Stewardship" = "stewardship",
                        "Dynamic Natures" = "dynamic"
                      )),
          div(
            class = "text-center mt-3",
            uiOutput(ns("nff_position_badge"))
          ),
          hr(),
          actionButton(ns("run_as_scenario"),
                       "Run as Scenario",
                       class = "btn-primary w-100",
                       icon = icon("play")),
          p(class = "text-muted small mt-2",
            "Sets the NFF weights to this narrative's position",
            "and navigates to the Scenarios module.")
        )
      ),

      # Right: Narrative detail card
      bslib::card(
        bslib::card_header(
          class = "bg-primary text-white",
          uiOutput(ns("detail_header"))
        ),
        bslib::card_body(
          class = "narrative-detail-body",
          uiOutput(ns("narrative_detail"))
        )
      )
    )
  )
}
```

**Step 2: Add UI test**

Add to `tests/testthat/test-modules.R`:

```r
test_that("mod_narratives_ui returns taglist", {
  ui <- mod_narratives_ui("test")
  expect_true(inherits(ui, "shiny.tag.list") || inherits(ui, "shiny.tag"))
})
```

**Step 3: Run test**

Run: `Rscript -e "testthat::test_file('tests/testthat/test-modules.R')"`

Expected: PASS.

**Step 4: Commit**

```bash
git add R/mod_narratives.R tests/testthat/test-modules.R
git commit -m "feat: add mod_narratives_ui with narrative selector and detail card"
```

---

### Task 4: Create Narrative Deep-Dive Module Server

**Files:**
- Modify: `R/mod_narratives.R` — add server function

**Step 1: Add the server function**

Append to `R/mod_narratives.R`:

```r
#' Narratives module server
#'
#' @param id Module namespace id
#' @param nff_weights Shared NFF weights reactiveVal (optional)
#' @noRd
mod_narratives_server <- function(id, nff_weights = NULL) {
  moduleServer(id, function(input, output, session) {
    narratives <- load_narratives()

    # Current narrative data
    current <- reactive({
      req(input$narrative_id)
      idx <- which(narratives$id == input$narrative_id)
      if (length(idx) == 0) return(NULL)
      narratives[idx, ]
    })

    # NFF position badge (shows NfN/NfS/NaC weights)
    output$nff_position_badge <- renderUI({
      narr <- current()
      if (is.null(narr)) return(NULL)

      w <- narr$weights
      # Handle both nested list and flat column formats
      if (is.data.frame(w) || is.list(w)) {
        nfn <- w$NfN; nfs <- w$NfS; nac <- w$NaC
      } else {
        nfn <- 34; nfs <- 33; nac <- 33
      }

      div(
        class = "nff-position-badges",
        tags$span(class = "badge rounded-pill",
                  style = "background-color: #0E7C7B;",
                  paste0("NfN: ", nfn, "%")),
        tags$span(class = "badge rounded-pill",
                  style = "background-color: #2A6F97;",
                  paste0("NfS: ", nfs, "%")),
        tags$span(class = "badge rounded-pill",
                  style = "background-color: #E07A5F;",
                  paste0("NaC: ", nac, "%"))
      )
    })

    # Detail header
    output$detail_header <- renderUI({
      narr <- current()
      if (is.null(narr)) return("Narrative Detail")
      tagList(
        bsicons::bs_icon("book-half"), " ",
        narr$name
      )
    })

    # Main narrative detail panel
    output$narrative_detail <- renderUI({
      narr <- current()
      if (is.null(narr)) {
        return(p(class = "text-muted", "Select a narrative to view details."))
      }

      # Helper to render a list column as bullet points
      render_list <- function(x) {
        if (is.null(x) || length(x) == 0 || (length(x) == 1 && x == "")) {
          return(p(class = "text-muted small", "Data not available."))
        }
        # Handle list columns from JSON (may be a list of character vectors)
        items <- if (is.list(x)) unlist(x) else x
        tags$ul(class = "mb-3",
          lapply(items, function(item) tags$li(item))
        )
      }

      tagList(
        # Description
        div(class = "mb-3",
          tags$h6(bsicons::bs_icon("info-circle"), " Description"),
          p(narr$description)
        ),

        # Governance Model
        div(class = "mb-3",
          tags$h6(bsicons::bs_icon("bank"), " Governance Model"),
          p(narr$governance_model)
        ),

        # Key Policies
        div(class = "mb-3",
          tags$h6(bsicons::bs_icon("list-check"), " Key Policies"),
          render_list(narr$key_policies)
        ),

        # Marine Examples
        div(class = "mb-3",
          tags$h6(bsicons::bs_icon("globe-europe-africa"), " Real-World Marine Examples"),
          render_list(narr$marine_examples)
        ),

        # Trade-offs
        div(class = "mb-3",
          tags$h6(bsicons::bs_icon("arrow-left-right"), " Trade-offs"),
          render_list(narr$trade_offs)
        ),

        # GBF Targets
        div(class = "mb-3",
          tags$h6(bsicons::bs_icon("bullseye"), " GBF Targets Prioritized"),
          {
            tgts <- narr$gbf_targets_prioritized
            if (is.null(tgts) || length(unlist(tgts)) == 0) {
              p(class = "text-muted small", "Data not available.")
            } else {
              items <- unlist(tgts)
              div(class = "d-flex gap-2 flex-wrap",
                lapply(items, function(t) {
                  tags$span(class = "badge bg-secondary",
                            paste0("Target ", t))
                })
              )
            }
          }
        ),

        # Source reference
        div(class = "text-muted small mt-3 pt-2 border-top",
          bsicons::bs_icon("journal-text"), " ",
          narr$source
        )
      )
    })

    # "Run as Scenario" button → set NFF weights and navigate
    observeEvent(input$run_as_scenario, {
      narr <- current()
      if (is.null(narr)) return()

      w <- narr$weights
      if (is.data.frame(w) || is.list(w)) {
        new_weights <- c(NfN = as.integer(w$NfN),
                         NfS = as.integer(w$NfS),
                         NaC = as.integer(w$NaC))
      } else {
        new_weights <- c(NfN = 34, NfS = 33, NaC = 33)
      }

      if (!is.null(nff_weights)) {
        nff_weights(new_weights)
      }

      # Navigate to Scenarios tab
      NatureJust_navigate <- paste0(
        "document.querySelectorAll('.navbar-nav .nav-link').forEach(function(link) {",
        "  if (link.textContent.trim().indexOf('Scenarios') !== -1) link.click();",
        "});"
      )
      session$sendCustomMessage("eval-js", NatureJust_navigate)
    })
  })
}
```

Note: The `session$sendCustomMessage("eval-js", ...)` requires a small JS handler. An alternative is to use `shinyjs::runjs()` if shinyjs is available, or we can use `Shiny.setInputValue('main_nav', 'Scenarios', {priority: 'event'})` which the existing `app_server.R` already handles (the main_nav input is the `bslib::page_navbar` id). We'll use the Shiny input approach instead — replace the `session$sendCustomMessage` block with:

```r
      # Navigate to Scenarios tab via the shared navbar input
      session$sendCustomMessage("nff-navigate", "Scenarios")
```

And we'll add a handler in `nff_triangle.js` in a later task.

**Step 2: Run tests**

Run: `Rscript -e "testthat::test_file('tests/testthat/test-modules.R')"`

Expected: All PASS including the UI test from Task 3.

**Step 3: Commit**

```bash
git add R/mod_narratives.R
git commit -m "feat: add mod_narratives_server with detail rendering and scenario launch"
```

---

### Task 5: Wire Narratives Module into App

**Files:**
- Modify: `R/app_ui.R` — add Narratives nav_panel
- Modify: `R/app_server.R` — call mod_narratives_server
- Modify: `inst/app/www/nff_triangle.js` — add navigation handler

**Step 1: Add Narratives tab to UI**

In `R/app_ui.R`, add a new `bslib::nav_panel()` after the Home panel (line 33) and before the Spatial Equity panel:

```r
      bslib::nav_panel(
        title = "Narratives",
        icon = bsicons::bs_icon("book-half"),
        mod_narratives_ui("narratives")
      ),
```

**Step 2: Add server call**

In `R/app_server.R`, after line 11 (`mod_home_server("home", nff_weights = nff_weights)`), add:

```r
  mod_narratives_server("narratives", nff_weights = nff_weights)
```

**Step 3: Add navigation message handler to JS**

In `inst/app/www/nff_triangle.js`, inside the `if (typeof Shiny !== 'undefined')` block at the end of `initTriangle()` (around line 339), add:

```javascript
    Shiny.addCustomMessageHandler('nff-navigate', function(target) {
      NatureJust.navigateTo(target);
    });
```

**Step 4: Update `dev/launch.R` source order**

Ensure `R/mod_narratives.R` is sourced along with the other modules. Add it to the module sourcing section.

**Step 5: Launch app and verify**

Run: `Rscript dev/launch.R` on port 7701.

Verify:
1. "Narratives" tab appears in the navbar between Home and Spatial Equity
2. Selecting "Arcology" shows full detail panel with governance, policies, examples
3. NFF position badge shows correct weights (100/0/0 for Arcology)
4. "Run as Scenario" button changes NFF weights and navigates to Scenarios tab
5. All 6 narratives load with full content
6. No console errors

**Step 6: Commit**

```bash
git add R/app_ui.R R/app_server.R inst/app/www/nff_triangle.js dev/launch.R
git commit -m "feat: wire narratives module into app with navigation"
```

---

### Task 6: Add Narrative Panel CSS

**Files:**
- Modify: `inst/app/www/custom.css`

**Step 1: Add narrative-specific styles**

Append to `custom.css`:

```css
/* ── Narratives Module ── */

.narratives-hero {
  text-align: center;
  padding: 1.5rem 1rem 1rem;
}
.narratives-hero h2 {
  font-family: var(--font-display, "DM Serif Display", serif);
  color: var(--nj-navy, #1B4965);
  font-weight: 700;
}

.narrative-detail-body {
  max-height: 600px;
  overflow-y: auto;
}
.narrative-detail-body h6 {
  color: var(--nj-navy, #1B4965);
  font-weight: 600;
  margin-bottom: 0.5rem;
}
.narrative-detail-body ul {
  padding-left: 1.2rem;
}
.narrative-detail-body ul li {
  margin-bottom: 0.3rem;
  font-size: 0.88rem;
}
.narrative-detail-body .badge {
  font-size: 0.78rem;
}

.nff-position-badges {
  display: flex;
  justify-content: center;
  gap: 0.5rem;
}
.nff-position-badges .badge {
  font-size: 0.8rem;
  padding: 0.4em 0.7em;
}

/* Dark mode */
[data-bs-theme="dark"] .narratives-hero h2 {
  color: #e0ddd8;
}
[data-bs-theme="dark"] .narrative-detail-body h6 {
  color: #b8d4e3;
}
```

**Step 2: Verify styling in browser**

Launch app, navigate to Narratives tab. Confirm:
- Hero header is styled with serif font
- Detail panel scrolls if content exceeds 600px
- Badges show correct colors (teal, blue, coral)
- Dark mode adapts properly

**Step 3: Commit**

```bash
git add inst/app/www/custom.css
git commit -m "style: add narrative deep-dive panel CSS with dark mode"
```

---

### Task 7: Connect Triangle Narrative Clicks to Narratives Tab

**Files:**
- Modify: `inst/app/www/nff_triangle.js`

**Step 1: Enhance narrative marker click behavior**

Currently, clicking a narrative marker on the Home triangle snaps the position marker to that narrative's position. Enhance this so that a **double-click** on a narrative marker navigates to the Narratives tab with that narrative pre-selected.

In `nff_triangle.js`, after the existing narrative click handler (around line 330-335), add:

```javascript
  $(svg).on('dblclick', '.nff-narrative-hit', function(e) {
    e.stopPropagation();
    var narrativeId = $(this).data('narrative');
    if (typeof Shiny !== 'undefined' && typeof Shiny.setInputValue === 'function') {
      // Tell the narratives module which narrative to select
      Shiny.setInputValue('narratives-narrative_from_triangle', narrativeId, {priority: 'event'});
    }
    NatureJust.navigateTo('Narratives');
  });
```

**Step 2: Add observer in mod_narratives_server**

In `R/mod_narratives.R`, inside `mod_narratives_server()`, add an observer for the triangle-originated narrative selection. Note: this input comes from outside the module's namespace, so we need to use `session$input` from the parent. Instead, we can use a custom Shiny message approach.

Alternative cleaner approach: Add to the narrative marker tooltip a "View Details" link text that hints at double-click, and handle the input within the module's namespace:

In `nff_triangle.js`, change the narrative click handler to send the input using the home module's namespace-agnostic approach:

```javascript
  $(svg).on('dblclick', '.nff-narrative-hit', function(e) {
    e.stopPropagation();
    var narrativeId = $(this).data('narrative');
    if (typeof Shiny !== 'undefined') {
      Shiny.setInputValue('navigate_to_narrative', narrativeId, {priority: 'event'});
    }
  });
```

Then in `R/app_server.R`, add an observer that handles this cross-module navigation:

```r
  # Handle narrative navigation from triangle double-click
  observeEvent(input$navigate_to_narrative, {
    # Update the narratives module's dropdown
    updateSelectInput(session, "narratives-narrative_id",
                      selected = input$navigate_to_narrative)
    # Navigate to Narratives tab
    bslib::nav_select("main_nav", "Narratives", session = session)
  })
```

**Step 3: Update narrative tooltip to hint at double-click**

In the tooltip HTML (nff_triangle.js, around line 307-312), add a hint:

```javascript
    $narrativeTip.html(
      '<div class="nff-nt-name">' + n.name + '</div>' +
      '<div class="nff-nt-pos">' + n.pos + '</div>' +
      '<div class="nff-nt-desc">' + n.desc + '</div>' +
      '<div class="nff-nt-gov"><strong>Governance:</strong> ' + n.gov + '</div>' +
      '<div class="nff-nt-hint">Double-click for full details</div>'
    ).addClass('visible');
```

Add CSS for the hint:

```css
.nff-nt-hint {
  font-size: 0.72rem;
  color: #999;
  margin-top: 0.3rem;
  font-style: italic;
}
```

**Step 4: Launch and verify**

1. Home page: hover narrative marker → tooltip shows "Double-click for full details"
2. Double-click narrative marker → navigates to Narratives tab with that narrative selected
3. Single-click still snaps position as before

**Step 5: Commit**

```bash
git add inst/app/www/nff_triangle.js R/app_server.R inst/app/www/custom.css
git commit -m "feat: connect triangle narrative double-click to narratives tab"
```

---

## Phase 2: Participatory Stakeholder Positioning

> Based on Rana et al. (2020), NFF Methodological Guidance (2025), and "Catalyzing Change" (2025) literature review

### Task 8: Create Stakeholder Positioning Module UI

**Files:**
- Create: `R/mod_stakeholders.R`

**Step 1: Create the module UI**

Create `R/mod_stakeholders.R`:

```r
#' Stakeholder Positioning module UI
#'
#' Allows multiple stakeholders to place their positions on the NFF triangle,
#' then visualises the collective distribution with consensus zones,
#' centroids, and spread metrics.
#'
#' @param id Module namespace id
#' @noRd
mod_stakeholders_ui <- function(id) {
  ns <- NS(id)

  tagList(
    div(
      class = "stakeholders-hero",
      h2("Participatory Positioning"),
      p(class = "text-muted",
        "Multiple stakeholders place their NFF positions to visualise collective priorities",
        tags$br(),
        tags$small("Based on NFF Methodological Guidance (IPBES, 2025)"))
    ),

    bslib::layout_column_wrap(
      width = 1/3,
      heights_equal = "row",

      # Left: Add stakeholder form
      bslib::card(
        bslib::card_header(class = "bg-primary text-white", "Add Stakeholder"),
        bslib::card_body(
          textInput(ns("stakeholder_name"), "Name / Group",
                    placeholder = "e.g., Fisher Cooperative"),
          selectInput(ns("stakeholder_group"), "Stakeholder Group",
                      choices = c("Government", "Industry", "Civil Society",
                                  "Academia", "Indigenous/Local Community",
                                  "NGO", "Other")),
          p(class = "text-muted small",
            "Click the triangle to set this stakeholder's NFF position,",
            "then press 'Add'."),
          div(class = "d-flex gap-2 mb-2",
            tags$span(class = "badge rounded-pill",
                      style = "background:#0E7C7B",
                      textOutput(ns("preview_nfn"), inline = TRUE)),
            tags$span(class = "badge rounded-pill",
                      style = "background:#2A6F97",
                      textOutput(ns("preview_nfs"), inline = TRUE)),
            tags$span(class = "badge rounded-pill",
                      style = "background:#E07A5F",
                      textOutput(ns("preview_nac"), inline = TRUE))
          ),
          actionButton(ns("add_stakeholder"), "Add Stakeholder",
                       class = "btn-primary w-100", icon = icon("plus")),
          hr(),
          actionButton(ns("clear_all"), "Clear All",
                       class = "btn-outline-danger w-100 btn-sm",
                       icon = icon("trash")),
          downloadButton(ns("export_csv"), "Export CSV",
                         class = "btn-outline-secondary w-100 btn-sm mt-2")
        )
      ),

      # Centre: Interactive NFF triangle (same SVG as Home, but with multi-dot overlay)
      bslib::card(
        bslib::card_header(class = "bg-primary text-white",
          "Collective NFF Positioning"
        ),
        bslib::card_body(
          class = "nff-triangle-container",
          div(
            id = ns("stakeholder_triangle"),
            class = "nff-triangle-widget nff-stakeholder-mode",
            `data-input-id` = ns("triangle_position"),
            # The same SVG as mod_home.R but without narrative markers
            # (will be simplified — we reuse the triangle geometry)
            uiOutput(ns("triangle_svg"))
          ),
          div(class = "nff-weight-readout", id = ns("stakeholder_readout"))
        ),
        bslib::card_footer(
          class = "text-muted text-center small",
          "Click to position. Colored dots show all stakeholder positions."
        )
      ),

      # Right: Statistics
      bslib::card(
        bslib::card_header(class = "bg-primary text-white", "Analysis"),
        bslib::card_body(
          uiOutput(ns("stakeholder_stats")),
          hr(),
          h6("Stakeholder List"),
          DT::DTOutput(ns("stakeholder_table"), height = "300px")
        )
      )
    )
  )
}
```

**Step 2: Add UI test**

Add to `tests/testthat/test-modules.R`:

```r
test_that("mod_stakeholders_ui returns taglist", {
  ui <- mod_stakeholders_ui("test")
  expect_true(inherits(ui, "shiny.tag.list") || inherits(ui, "shiny.tag"))
})
```

**Step 3: Commit**

```bash
git add R/mod_stakeholders.R tests/testthat/test-modules.R
git commit -m "feat: add mod_stakeholders_ui with triangle, form, and analysis panels"
```

---

### Task 9: Create Stakeholder Positioning Module Server

**Files:**
- Modify: `R/mod_stakeholders.R` — add server function

**Step 1: Add the server function**

Append to `R/mod_stakeholders.R`:

```r
#' Stakeholder Positioning module server
#'
#' @param id Module namespace id
#' @param nff_weights Shared NFF weights reactiveVal (optional)
#' @noRd
mod_stakeholders_server <- function(id, nff_weights = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Stakeholder data store: list of rows
    stakeholders <- reactiveVal(data.frame(
      name = character(0),
      group = character(0),
      NfN = numeric(0),
      NfS = numeric(0),
      NaC = numeric(0),
      stringsAsFactors = FALSE
    ))

    # Current triangle click position (for preview)
    current_pos <- reactiveVal(c(NfN = 34, NfS = 33, NaC = 33))

    # Listen to triangle clicks
    observeEvent(input$triangle_position, {
      pos <- input$triangle_position
      if (!is.null(pos)) {
        current_pos(c(
          NfN = as.integer(pos$NfN),
          NfS = as.integer(pos$NfS),
          NaC = as.integer(pos$NaC)
        ))
      }
    })

    # Preview badges
    output$preview_nfn <- renderText({ paste0("NfN: ", current_pos()["NfN"], "%") })
    output$preview_nfs <- renderText({ paste0("NfS: ", current_pos()["NfS"], "%") })
    output$preview_nac <- renderText({ paste0("NaC: ", current_pos()["NaC"], "%") })

    # Render the triangle SVG (reuse the same SVG structure as mod_home)
    output$triangle_svg <- renderUI({
      HTML('
        <svg class="nff-svg" viewBox="-50 0 500 400"
             xmlns="http://www.w3.org/2000/svg" role="img"
             aria-label="Stakeholder positioning triangle">
          <defs>
            <clipPath id="nff-tri-clip-s">
              <polygon points="200,35 365,335 35,335"/>
            </clipPath>
            <radialGradient id="nff-grad-nfn-s" cx="200" cy="35" r="200"
                            gradientUnits="userSpaceOnUse">
              <stop offset="0%"  stop-color="#0E7C7B" stop-opacity="0.7"/>
              <stop offset="45%" stop-color="#0E7C7B" stop-opacity="0.25"/>
              <stop offset="100%" stop-color="#0E7C7B" stop-opacity="0"/>
            </radialGradient>
            <radialGradient id="nff-grad-nfs-s" cx="365" cy="335" r="200"
                            gradientUnits="userSpaceOnUse">
              <stop offset="0%"  stop-color="#2A6F97" stop-opacity="0.7"/>
              <stop offset="45%" stop-color="#2A6F97" stop-opacity="0.25"/>
              <stop offset="100%" stop-color="#2A6F97" stop-opacity="0"/>
            </radialGradient>
            <radialGradient id="nff-grad-nac-s" cx="35" cy="335" r="200"
                            gradientUnits="userSpaceOnUse">
              <stop offset="0%"  stop-color="#E07A5F" stop-opacity="0.7"/>
              <stop offset="45%" stop-color="#E07A5F" stop-opacity="0.25"/>
              <stop offset="100%" stop-color="#E07A5F" stop-opacity="0"/>
            </radialGradient>
          </defs>
          <g clip-path="url(#nff-tri-clip-s)" class="nff-gradient-bg">
            <polygon points="200,35 365,335 35,335" fill="#f0eeeb" opacity="0.5"/>
            <polygon points="200,35 365,335 35,335" fill="url(#nff-grad-nfn-s)"/>
            <polygon points="200,35 365,335 35,335" fill="url(#nff-grad-nfs-s)"/>
            <polygon points="200,35 365,335 35,335" fill="url(#nff-grad-nac-s)"/>
          </g>
          <polygon class="nff-tri-outline" points="200,35 365,335 35,335" fill="none"/>
          <text class="vertex-label" x="200" y="16" text-anchor="middle">Nature for Nature</text>
          <text class="vertex-label" x="365" y="365" text-anchor="middle">Nature for Society</text>
          <text class="vertex-label" x="35" y="365" text-anchor="middle">Nature as Culture</text>
          <circle class="nff-vertex" cx="200" cy="35" r="11"/>
          <circle class="nff-vertex" cx="365" cy="335" r="11"/>
          <circle class="nff-vertex" cx="35"  cy="335" r="11"/>
        </svg>
      ')
    })

    # Add stakeholder
    observeEvent(input$add_stakeholder, {
      name <- trimws(input$stakeholder_name)
      if (nchar(name) == 0) {
        showNotification("Please enter a stakeholder name.", type = "warning")
        return()
      }
      pos <- current_pos()
      new_row <- data.frame(
        name = name,
        group = input$stakeholder_group,
        NfN = pos["NfN"],
        NfS = pos["NfS"],
        NaC = pos["NaC"],
        stringsAsFactors = FALSE
      )
      stakeholders(rbind(stakeholders(), new_row))

      # Send dot to JS for rendering on the triangle
      session$sendCustomMessage("stakeholder-add-dot", list(
        NfN = pos["NfN"] / 100,
        NfS = pos["NfS"] / 100,
        NaC = pos["NaC"] / 100,
        name = name,
        group = input$stakeholder_group
      ))

      # Clear name input
      updateTextInput(session, "stakeholder_name", value = "")
      showNotification(paste("Added:", name), type = "message")
    })

    # Clear all
    observeEvent(input$clear_all, {
      stakeholders(data.frame(
        name = character(0), group = character(0),
        NfN = numeric(0), NfS = numeric(0), NaC = numeric(0),
        stringsAsFactors = FALSE
      ))
      session$sendCustomMessage("stakeholder-clear-dots", list())
    })

    # Statistics
    output$stakeholder_stats <- renderUI({
      df <- stakeholders()
      n <- nrow(df)
      if (n == 0) {
        return(p(class = "text-muted", "No stakeholders added yet.",
                 tags$br(), "Add at least 2 to see analysis."))
      }

      # Centroid
      centroid_nfn <- round(mean(df$NfN))
      centroid_nfs <- round(mean(df$NfS))
      centroid_nac <- round(mean(df$NaC))

      # Spread (standard deviation of barycentric coordinates)
      spread <- if (n >= 2) {
        round(sqrt(var(df$NfN/100) + var(df$NfS/100) + var(df$NaC/100)), 3)
      } else NA

      # Consensus zone (% of stakeholders within 10% of centroid)
      if (n >= 2) {
        dists <- sqrt((df$NfN - centroid_nfn)^2 +
                       (df$NfS - centroid_nfs)^2 +
                       (df$NaC - centroid_nac)^2) / 100
        consensus_pct <- round(sum(dists <= 0.15) / n * 100)
      } else {
        consensus_pct <- 100
      }

      tagList(
        h6("Summary"),
        div(class = "d-flex justify-content-between mb-1",
          tags$span("Stakeholders:"), tags$strong(n)
        ),
        div(class = "d-flex justify-content-between mb-1",
          tags$span("Centroid:"),
          tags$strong(paste0(centroid_nfn, "/", centroid_nfs, "/", centroid_nac))
        ),
        if (!is.na(spread)) {
          div(class = "d-flex justify-content-between mb-1",
            tags$span("Spread (SD):"),
            tags$strong(spread)
          )
        },
        div(class = "d-flex justify-content-between mb-1",
          tags$span("Consensus (\u226415%):"),
          tags$strong(paste0(consensus_pct, "%"))
        ),
        if (n >= 2 && !is.null(nff_weights)) {
          div(class = "mt-2",
            actionButton(ns("use_centroid"), "Use Centroid as Weights",
                         class = "btn-outline-primary btn-sm w-100")
          )
        }
      )
    })

    # Use centroid as NFF weights
    observeEvent(input$use_centroid, {
      df <- stakeholders()
      if (nrow(df) < 2 || is.null(nff_weights)) return()
      centroid <- c(
        NfN = as.integer(round(mean(df$NfN))),
        NfS = as.integer(round(mean(df$NfS))),
        NaC = as.integer(round(mean(df$NaC)))
      )
      # Normalize to sum to 100
      total <- sum(centroid)
      if (total != 100) {
        centroid["NfN"] <- centroid["NfN"] + (100L - total)
      }
      nff_weights(centroid)
      showNotification("NFF weights set to group centroid.", type = "message")
    })

    # Stakeholder table
    output$stakeholder_table <- DT::renderDT({
      df <- stakeholders()
      if (nrow(df) == 0) return(DT::datatable(data.frame(Message = "No data")))
      DT::datatable(df, rownames = FALSE, options = list(
        pageLength = 10, dom = "tp", scrollY = "250px"
      ))
    })

    # Export CSV
    output$export_csv <- downloadHandler(
      filename = function() {
        paste0("nff_stakeholder_positions_", Sys.Date(), ".csv")
      },
      content = function(file) {
        utils::write.csv(stakeholders(), file, row.names = FALSE)
      }
    )
  })
}
```

**Step 2: Add server test**

Add to `tests/testthat/test-modules.R`:

```r
test_that("mod_stakeholders_server manages stakeholder data", {
  testServer(mod_stakeholders_server, {
    # Initially empty
    expect_equal(nrow(stakeholders()), 0)
  })
})
```

**Step 3: Commit**

```bash
git add R/mod_stakeholders.R tests/testthat/test-modules.R
git commit -m "feat: add mod_stakeholders_server with positioning, stats, and export"
```

---

### Task 10: Add Stakeholder Triangle JS Support

**Files:**
- Modify: `inst/app/www/nff_triangle.js`

**Step 1: Add stakeholder dot rendering**

At the end of the `initTriangle()` function (before the closing `}`), add handlers for the stakeholder mode:

```javascript
  /* ────── Stakeholder mode: multi-dot overlay ────── */

  var GROUP_COLORS = {
    'Government':                '#1B4965',
    'Industry':                  '#5FA8D3',
    'Civil Society':             '#41ae76',
    'Academia':                  '#9467bd',
    'Indigenous/Local Community': '#E07A5F',
    'NGO':                       '#F2CC8F',
    'Other':                     '#9C9587'
  };

  if (typeof Shiny !== 'undefined') {
    Shiny.addCustomMessageHandler('stakeholder-add-dot', function(msg) {
      var xy = baryToXY(msg.NfN, msg.NfS, msg.NaC);
      var color = GROUP_COLORS[msg.group] || '#666';

      // Outer ring
      var ring = makeSVG('circle', {
        cx: xy.x, cy: xy.y, r: 9,
        fill: 'none', stroke: color, 'stroke-width': 2,
        opacity: 0.5, 'class': 'stakeholder-dot-ring'
      });
      // Inner dot
      var dot = makeSVG('circle', {
        cx: xy.x, cy: xy.y, r: 5,
        fill: color, opacity: 0.85, 'class': 'stakeholder-dot'
      });
      // Label (tiny, offset)
      var label = makeSVG('text', {
        x: xy.x + 12, y: xy.y + 4,
        'font-size': '9', fill: color, opacity: 0.7,
        'class': 'stakeholder-label'
      });
      label.textContent = msg.name.substring(0, 12);

      svg.appendChild(ring);
      svg.appendChild(dot);
      svg.appendChild(label);
    });

    Shiny.addCustomMessageHandler('stakeholder-clear-dots', function() {
      $(svg).find('.stakeholder-dot, .stakeholder-dot-ring, .stakeholder-label').remove();
    });
  }
```

**Step 2: Commit**

```bash
git add inst/app/www/nff_triangle.js
git commit -m "feat: add stakeholder dot rendering to NFF triangle JS"
```

---

### Task 11: Wire Stakeholder Module into App

**Files:**
- Modify: `R/app_ui.R` — add Stakeholders nav_panel
- Modify: `R/app_server.R` — call mod_stakeholders_server
- Modify: `dev/launch.R` — source new module

**Step 1: Add tab to UI**

In `R/app_ui.R`, add after the Narratives panel:

```r
      bslib::nav_panel(
        title = "Stakeholders",
        icon = bsicons::bs_icon("people"),
        mod_stakeholders_ui("stakeholders")
      ),
```

**Step 2: Add server call**

In `R/app_server.R`, add:

```r
  mod_stakeholders_server("stakeholders", nff_weights = nff_weights)
```

**Step 3: Update launch.R**

Add `source("R/mod_stakeholders.R")` to the module sourcing section.

**Step 4: Add stakeholder CSS**

Append to `inst/app/www/custom.css`:

```css
/* ── Stakeholders Module ── */

.stakeholders-hero {
  text-align: center;
  padding: 1.5rem 1rem 1rem;
}
.stakeholders-hero h2 {
  font-family: var(--font-display, "DM Serif Display", serif);
  color: var(--nj-navy, #1B4965);
  font-weight: 700;
}

.nff-stakeholder-mode .nff-position-marker {
  opacity: 0.4;
}
.nff-stakeholder-mode .nff-position-ring {
  opacity: 0.3;
}

[data-bs-theme="dark"] .stakeholders-hero h2 {
  color: #e0ddd8;
}
```

**Step 5: Launch and verify**

1. "Stakeholders" tab appears in navbar
2. Can type name, select group, click triangle to set position
3. "Add Stakeholder" creates a colored dot on the triangle
4. Statistics panel updates with centroid, spread, consensus %
5. Table shows all stakeholders
6. "Export CSV" downloads valid CSV
7. "Use Centroid as Weights" updates shared NFF weights
8. "Clear All" removes all dots and resets

**Step 6: Commit**

```bash
git add R/app_ui.R R/app_server.R dev/launch.R inst/app/www/custom.css R/mod_stakeholders.R
git commit -m "feat: wire stakeholder positioning module into app"
```

---

## Phase 3: Three Horizons Pathway Visualization

> Based on Pereira et al. (2020), NFF Methodological Guidance (2025), and the Three Horizons Framework

### Task 12: Create Pathways Module UI

**Files:**
- Create: `R/mod_pathways.R`

**Step 1: Create the module**

Create `R/mod_pathways.R`:

```r
#' Pathways module UI
#'
#' Three Horizons pathway visualization on the NFF triangle.
#' Users place "Now" and "Future" markers, and the module draws
#' animated pathways through the NFF state space.
#'
#' @param id Module namespace id
#' @noRd
mod_pathways_ui <- function(id) {
  ns <- NS(id)

  tagList(
    div(
      class = "pathways-hero",
      h2("Three Horizons Pathways"),
      p(class = "text-muted",
        "Visualise governance transitions from current state to desired futures",
        tags$br(),
        tags$small("Based on Pereira et al. (2020) and the Three Horizons Framework"))
    ),

    bslib::layout_column_wrap(
      width = 1/2,

      # Left: Pathway controls
      bslib::card(
        bslib::card_header(class = "bg-primary text-white", "Pathway Settings"),
        bslib::card_body(
          h6("Current State (Now)"),
          p(class = "text-muted small",
            "Where is marine governance today in your region?"),
          div(class = "d-flex gap-2 mb-2",
            numericInput(ns("now_nfn"), "NfN%", value = 20, min = 0, max = 100, width = "80px"),
            numericInput(ns("now_nfs"), "NfS%", value = 60, min = 0, max = 100, width = "80px"),
            numericInput(ns("now_nac"), "NaC%", value = 20, min = 0, max = 100, width = "80px")
          ),
          hr(),
          h6("Desired Future"),
          p(class = "text-muted small",
            "Where should governance move toward?"),
          selectInput(ns("future_preset"), "Select a narrative or custom:",
                      choices = c(
                        "Custom" = "custom",
                        "Arcology (100/0/0)" = "arcology",
                        "Sharing through Sparing (50/50/0)" = "sharing",
                        "Optimizing Nature (0/100/0)" = "optimizing",
                        "Innovative Commons (0/50/50)" = "commons",
                        "Reciprocal Stewardship (0/0/100)" = "stewardship",
                        "Dynamic Natures (50/0/50)" = "dynamic",
                        "Balanced (34/33/33)" = "balanced"
                      )),
          conditionalPanel(
            condition = sprintf("input['%s'] == 'custom'", ns("future_preset")),
            div(class = "d-flex gap-2 mb-2",
              numericInput(ns("future_nfn"), "NfN%", value = 40, min = 0, max = 100, width = "80px"),
              numericInput(ns("future_nfs"), "NfS%", value = 30, min = 0, max = 100, width = "80px"),
              numericInput(ns("future_nac"), "NaC%", value = 30, min = 0, max = 100, width = "80px")
            )
          ),
          hr(),
          sliderInput(ns("timeline"), "Timeline Position",
                      min = 2025, max = 2050, value = 2025,
                      step = 1, sep = "",
                      animate = animationOptions(interval = 200, loop = FALSE)),
          hr(),
          actionButton(ns("draw_pathway"), "Draw Pathway",
                       class = "btn-primary w-100", icon = icon("route")),
          actionButton(ns("clear_pathways"), "Clear All Pathways",
                       class = "btn-outline-secondary w-100 mt-2 btn-sm")
        )
      ),

      # Right: Triangle with pathway overlay + Three Horizons panel
      bslib::card(
        bslib::card_header(class = "bg-primary text-white",
          "Pathway Visualization"
        ),
        bslib::card_body(
          class = "nff-triangle-container",
          div(
            id = ns("pathway_triangle"),
            class = "nff-triangle-widget nff-pathway-mode",
            `data-input-id` = ns("pathway_position"),
            uiOutput(ns("pathway_triangle_svg"))
          ),
          div(class = "nff-weight-readout", id = ns("pathway_readout"))
        ),
        bslib::card_footer(
          class = "text-muted text-center small",
          "Dotted line shows governance transition pathway. Use the timeline slider to animate."
        )
      )
    ),

    # Three Horizons panel below
    bslib::card(
      class = "mt-3",
      bslib::card_header(class = "bg-primary text-white",
        "Three Horizons Analysis"
      ),
      bslib::card_body(
        uiOutput(ns("three_horizons"))
      )
    )
  )
}
```

**Step 2: Add UI test**

```r
test_that("mod_pathways_ui returns taglist", {
  ui <- mod_pathways_ui("test")
  expect_true(inherits(ui, "shiny.tag.list") || inherits(ui, "shiny.tag"))
})
```

**Step 3: Commit**

```bash
git add R/mod_pathways.R tests/testthat/test-modules.R
git commit -m "feat: add mod_pathways_ui with Three Horizons pathway controls"
```

---

### Task 13: Create Pathways Module Server

**Files:**
- Modify: `R/mod_pathways.R` — add server function

**Step 1: Add the server function**

Append to `R/mod_pathways.R`:

```r
#' Pathways module server
#'
#' @param id Module namespace id
#' @param nff_weights Shared NFF weights reactiveVal (optional)
#' @noRd
mod_pathways_server <- function(id, nff_weights = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Narrative presets for "Future" dropdown
    narrative_weights <- list(
      arcology    = c(NfN = 100, NfS = 0,   NaC = 0),
      sharing     = c(NfN = 50,  NfS = 50,  NaC = 0),
      optimizing  = c(NfN = 0,   NfS = 100, NaC = 0),
      commons     = c(NfN = 0,   NfS = 50,  NaC = 50),
      stewardship = c(NfN = 0,   NfS = 0,   NaC = 100),
      dynamic     = c(NfN = 50,  NfS = 0,   NaC = 50),
      balanced    = c(NfN = 34,  NfS = 33,  NaC = 33)
    )

    # Get future position based on selection
    future_pos <- reactive({
      preset <- input$future_preset
      if (preset == "custom") {
        c(NfN = input$future_nfn %||% 34,
          NfS = input$future_nfs %||% 33,
          NaC = input$future_nac %||% 33)
      } else {
        narrative_weights[[preset]] %||% c(NfN = 34, NfS = 33, NaC = 33)
      }
    })

    # Get "now" position
    now_pos <- reactive({
      c(NfN = input$now_nfn %||% 20,
        NfS = input$now_nfs %||% 60,
        NaC = input$now_nac %||% 20)
    })

    # Render the triangle SVG (same as stakeholder but with pathway elements)
    output$pathway_triangle_svg <- renderUI({
      HTML('
        <svg class="nff-svg" viewBox="-50 0 500 400"
             xmlns="http://www.w3.org/2000/svg" role="img"
             aria-label="NFF pathway visualization triangle">
          <defs>
            <clipPath id="nff-tri-clip-p">
              <polygon points="200,35 365,335 35,335"/>
            </clipPath>
            <radialGradient id="nff-grad-nfn-p" cx="200" cy="35" r="200"
                            gradientUnits="userSpaceOnUse">
              <stop offset="0%"  stop-color="#0E7C7B" stop-opacity="0.7"/>
              <stop offset="45%" stop-color="#0E7C7B" stop-opacity="0.25"/>
              <stop offset="100%" stop-color="#0E7C7B" stop-opacity="0"/>
            </radialGradient>
            <radialGradient id="nff-grad-nfs-p" cx="365" cy="335" r="200"
                            gradientUnits="userSpaceOnUse">
              <stop offset="0%"  stop-color="#2A6F97" stop-opacity="0.7"/>
              <stop offset="45%" stop-color="#2A6F97" stop-opacity="0.25"/>
              <stop offset="100%" stop-color="#2A6F97" stop-opacity="0"/>
            </radialGradient>
            <radialGradient id="nff-grad-nac-p" cx="35" cy="335" r="200"
                            gradientUnits="userSpaceOnUse">
              <stop offset="0%"  stop-color="#E07A5F" stop-opacity="0.7"/>
              <stop offset="45%" stop-color="#E07A5F" stop-opacity="0.25"/>
              <stop offset="100%" stop-color="#E07A5F" stop-opacity="0"/>
            </radialGradient>
          </defs>
          <g clip-path="url(#nff-tri-clip-p)" class="nff-gradient-bg">
            <polygon points="200,35 365,335 35,335" fill="#f0eeeb" opacity="0.5"/>
            <polygon points="200,35 365,335 35,335" fill="url(#nff-grad-nfn-p)"/>
            <polygon points="200,35 365,335 35,335" fill="url(#nff-grad-nfs-p)"/>
            <polygon points="200,35 365,335 35,335" fill="url(#nff-grad-nac-p)"/>
          </g>
          <polygon class="nff-tri-outline" points="200,35 365,335 35,335" fill="none"/>
          <text class="vertex-label" x="200" y="16" text-anchor="middle">Nature for Nature</text>
          <text class="vertex-label" x="365" y="365" text-anchor="middle">Nature for Society</text>
          <text class="vertex-label" x="35" y="365" text-anchor="middle">Nature as Culture</text>
          <circle class="nff-vertex" cx="200" cy="35" r="11"/>
          <circle class="nff-vertex" cx="365" cy="335" r="11"/>
          <circle class="nff-vertex" cx="35"  cy="335" r="11"/>
        </svg>
      ')
    })

    # Draw pathway on triangle
    observeEvent(input$draw_pathway, {
      now <- now_pos()
      future <- future_pos()

      session$sendCustomMessage("pathway-draw", list(
        now_NfN = now["NfN"] / 100,
        now_NfS = now["NfS"] / 100,
        now_NaC = now["NaC"] / 100,
        future_NfN = future["NfN"] / 100,
        future_NfS = future["NfS"] / 100,
        future_NaC = future["NaC"] / 100
      ))
    })

    # Animate along pathway based on timeline slider
    observe({
      req(input$timeline)
      progress <- (input$timeline - 2025) / (2050 - 2025)  # 0 to 1
      session$sendCustomMessage("pathway-animate", list(progress = progress))
    })

    # Clear all pathways
    observeEvent(input$clear_pathways, {
      session$sendCustomMessage("pathway-clear", list())
    })

    # Three Horizons Analysis — shows what declines, transitions, and emerges
    output$three_horizons <- renderUI({
      now <- now_pos()
      future <- future_pos()

      # Compute directional changes
      delta_nfn <- future["NfN"] - now["NfN"]
      delta_nfs <- future["NfS"] - now["NfS"]
      delta_nac <- future["NaC"] - now["NaC"]

      # Classify each perspective change
      classify <- function(delta, name) {
        if (delta > 10) return(list(horizon = "H3 (Emerging)", class = "text-success", icon = "arrow-up-circle"))
        if (delta < -10) return(list(horizon = "H1 (Declining)", class = "text-danger", icon = "arrow-down-circle"))
        return(list(horizon = "H2 (Transitioning)", class = "text-warning", icon = "arrow-left-right"))
      }

      h_nfn <- classify(delta_nfn, "Nature for Nature")
      h_nfs <- classify(delta_nfs, "Nature for Society")
      h_nac <- classify(delta_nac, "Nature as Culture")

      # Policy implications based on direction
      policy_text <- function(perspective, delta) {
        if (perspective == "NfN") {
          if (delta > 10) return("Expanding marine protected areas, strengthening conservation enforcement, increasing biodiversity monitoring.")
          if (delta < -10) return("Reducing strict protection zones, relaxing conservation requirements, prioritizing other values.")
          return("Maintaining current conservation levels with incremental adjustments.")
        }
        if (perspective == "NfS") {
          if (delta > 10) return("Scaling blue economy, expanding aquaculture, increasing offshore energy, optimizing resource extraction.")
          if (delta < -10) return("Reducing industrial extraction, transitioning away from intensive marine resource use.")
          return("Maintaining current economic use with efficiency improvements.")
        }
        # NaC
        if (delta > 10) return("Strengthening indigenous rights, expanding cultural heritage protections, integrating traditional knowledge.")
        if (delta < -10) return("Reducing cultural governance emphasis, modernizing traditional practices.")
        return("Maintaining cultural governance with gradual knowledge integration.")
      }

      tagList(
        bslib::layout_column_wrap(
          width = 1/3,
          bslib::card(
            class = "border-0",
            bslib::card_body(
              div(class = paste("text-center", h_nfn$class),
                bsicons::bs_icon(h_nfn$icon, size = "2rem"),
                h6("Nature for Nature"),
                tags$span(class = "badge", style = "background:#0E7C7B",
                          paste0(sprintf("%+d", delta_nfn), "%")),
                p(class = "small mt-2", h_nfn$horizon)
              ),
              p(class = "small text-muted", policy_text("NfN", delta_nfn))
            )
          ),
          bslib::card(
            class = "border-0",
            bslib::card_body(
              div(class = paste("text-center", h_nfs$class),
                bsicons::bs_icon(h_nfs$icon, size = "2rem"),
                h6("Nature for Society"),
                tags$span(class = "badge", style = "background:#2A6F97",
                          paste0(sprintf("%+d", delta_nfs), "%")),
                p(class = "small mt-2", h_nfs$horizon)
              ),
              p(class = "small text-muted", policy_text("NfS", delta_nfs))
            )
          ),
          bslib::card(
            class = "border-0",
            bslib::card_body(
              div(class = paste("text-center", h_nac$class),
                bsicons::bs_icon(h_nac$icon, size = "2rem"),
                h6("Nature as Culture"),
                tags$span(class = "badge", style = "background:#E07A5F",
                          paste0(sprintf("%+d", delta_nac), "%")),
                p(class = "small mt-2", h_nac$horizon)
              ),
              p(class = "small text-muted", policy_text("NaC", delta_nac))
            )
          )
        ),
        div(class = "text-muted small text-center mt-2 pt-2 border-top",
          bsicons::bs_icon("journal-text"), " ",
          "Three Horizons Framework: H1 = what needs to decline, ",
          "H2 = what is transitioning, H3 = what needs to emerge. ",
          "See Pereira et al. (2020) and NFF Methodological Guidance (2025)."
        )
      )
    })
  })
}
```

**Step 2: Commit**

```bash
git add R/mod_pathways.R
git commit -m "feat: add mod_pathways_server with Three Horizons analysis"
```

---

### Task 14: Add Pathway JS Rendering

**Files:**
- Modify: `inst/app/www/nff_triangle.js`

**Step 1: Add pathway rendering handlers**

Add at the end of `initTriangle()`:

```javascript
  /* ────── Pathway mode: animated dotted lines ────── */

  var pathwayElements = [];
  var currentPathway = null;

  if (typeof Shiny !== 'undefined') {
    Shiny.addCustomMessageHandler('pathway-draw', function(msg) {
      var startXY = baryToXY(msg.now_NfN, msg.now_NfS, msg.now_NaC);
      var endXY   = baryToXY(msg.future_NfN, msg.future_NfS, msg.future_NaC);

      // Dotted path line
      var path = makeSVG('line', {
        x1: startXY.x, y1: startXY.y,
        x2: endXY.x, y2: endXY.y,
        stroke: '#1B4965', 'stroke-width': 2.5,
        'stroke-dasharray': '6,4', opacity: 0.7,
        'class': 'pathway-line'
      });

      // "Now" marker (square)
      var nowMarker = makeSVG('rect', {
        x: startXY.x - 6, y: startXY.y - 6,
        width: 12, height: 12,
        fill: '#d9534f', stroke: '#fff', 'stroke-width': 1.5,
        rx: 2, 'class': 'pathway-now'
      });

      // "Future" marker (star-like)
      var futureMarker = makeSVG('polygon', {
        points: starPoints(endXY.x, endXY.y, 8, 4),
        fill: '#41ae76', stroke: '#fff', 'stroke-width': 1.5,
        'class': 'pathway-future'
      });

      // "Now" label
      var nowLabel = makeSVG('text', {
        x: startXY.x, y: startXY.y - 12,
        'text-anchor': 'middle', 'font-size': '10',
        fill: '#d9534f', 'class': 'pathway-label'
      });
      nowLabel.textContent = 'Now';

      // "Future" label
      var futureLabel = makeSVG('text', {
        x: endXY.x, y: endXY.y - 12,
        'text-anchor': 'middle', 'font-size': '10',
        fill: '#41ae76', 'class': 'pathway-label'
      });
      futureLabel.textContent = 'Future';

      // Animated position dot along the path
      var animDot = makeSVG('circle', {
        cx: startXY.x, cy: startXY.y, r: 6,
        fill: '#F2CC8F', stroke: '#1B4965', 'stroke-width': 1.5,
        'class': 'pathway-anim-dot'
      });

      var els = [path, nowMarker, futureMarker, nowLabel, futureLabel, animDot];
      els.forEach(function(el) { svg.appendChild(el); });
      pathwayElements = pathwayElements.concat(els);

      currentPathway = {
        start: startXY,
        end: endXY,
        animDot: animDot
      };
    });

    Shiny.addCustomMessageHandler('pathway-animate', function(msg) {
      if (!currentPathway) return;
      var t = Math.max(0, Math.min(1, msg.progress));
      var x = currentPathway.start.x + t * (currentPathway.end.x - currentPathway.start.x);
      var y = currentPathway.start.y + t * (currentPathway.end.y - currentPathway.start.y);
      currentPathway.animDot.setAttribute('cx', x);
      currentPathway.animDot.setAttribute('cy', y);
    });

    Shiny.addCustomMessageHandler('pathway-clear', function() {
      pathwayElements.forEach(function(el) { el.remove(); });
      pathwayElements = [];
      currentPathway = null;
    });
  }

  // Helper: generate star polygon points string
  function starPoints(cx, cy, outerR, innerR) {
    var pts = [];
    for (var i = 0; i < 5; i++) {
      var outerAngle = (i * 72 - 90) * Math.PI / 180;
      var innerAngle = ((i * 72) + 36 - 90) * Math.PI / 180;
      pts.push((cx + outerR * Math.cos(outerAngle)).toFixed(1) + ',' +
               (cy + outerR * Math.sin(outerAngle)).toFixed(1));
      pts.push((cx + innerR * Math.cos(innerAngle)).toFixed(1) + ',' +
               (cy + innerR * Math.sin(innerAngle)).toFixed(1));
    }
    return pts.join(' ');
  }
```

**Step 2: Add pathway CSS**

Append to `custom.css`:

```css
/* ── Pathways Module ── */

.pathways-hero {
  text-align: center;
  padding: 1.5rem 1rem 1rem;
}
.pathways-hero h2 {
  font-family: var(--font-display, "DM Serif Display", serif);
  color: var(--nj-navy, #1B4965);
  font-weight: 700;
}

.pathway-line {
  transition: opacity 0.3s;
}
.pathway-anim-dot {
  transition: cx 0.15s ease-out, cy 0.15s ease-out;
}

[data-bs-theme="dark"] .pathways-hero h2 {
  color: #e0ddd8;
}
[data-bs-theme="dark"] .pathway-line {
  stroke: #b8d4e3;
}
[data-bs-theme="dark"] .pathway-label {
  fill: #e0ddd8;
}
```

**Step 3: Commit**

```bash
git add inst/app/www/nff_triangle.js inst/app/www/custom.css
git commit -m "feat: add pathway rendering JS with animated dot and Three Horizons markers"
```

---

### Task 15: Wire Pathways Module into App

**Files:**
- Modify: `R/app_ui.R`
- Modify: `R/app_server.R`
- Modify: `dev/launch.R`

**Step 1: Add Pathways tab to UI**

In `R/app_ui.R`, add after the Stakeholders panel:

```r
      bslib::nav_panel(
        title = "Pathways",
        icon = bsicons::bs_icon("signpost-split"),
        mod_pathways_ui("pathways")
      ),
```

**Step 2: Add server call**

In `R/app_server.R`:

```r
  mod_pathways_server("pathways", nff_weights = nff_weights)
```

**Step 3: Update launch.R**

Add `source("R/mod_pathways.R")`.

**Step 4: Launch and verify**

1. "Pathways" tab appears in navbar
2. Set "Now" position (e.g., 20/60/20) and select "Arcology" as future
3. Click "Draw Pathway" → dotted line appears on triangle with Now/Future markers
4. Move timeline slider → animated dot moves along the path
5. Three Horizons panel shows correct H1/H2/H3 classification
6. "Clear All Pathways" removes all pathway elements
7. Dark mode works correctly

**Step 5: Run full test suite**

Run: `Rscript -e "testthat::test_dir('tests/testthat')"`

Expected: All tests PASS.

**Step 6: Commit**

```bash
git add R/app_ui.R R/app_server.R dev/launch.R
git commit -m "feat: wire Three Horizons pathways module into app"
```

---

### Task 16: Final Integration Test & Cleanup

**Files:**
- Modify: `tests/testthat/test-modules.R` — ensure all new modules have tests
- Modify: `DESCRIPTION` — add jsonlite dependency

**Step 1: Add jsonlite to DESCRIPTION Imports**

In `DESCRIPTION`, add `jsonlite` to the Imports field (needed by `load_narratives()`).

**Step 2: Run full test suite**

Run: `Rscript -e "testthat::test_dir('tests/testthat')"`

Expected: All tests PASS (including new tests for mod_narratives, mod_stakeholders, mod_pathways).

**Step 3: Launch app and do full smoke test**

Verify all 9 tabs work:
1. **Home** — NFF triangle with gradients, 6 narrative markers, click/drag positioning
2. **Narratives** — Select narrative → full detail panel, "Run as Scenario" works
3. **Stakeholders** — Add multiple stakeholders, see dots on triangle, statistics update
4. **Pathways** — Draw pathway, animate timeline, Three Horizons analysis
5. **Spatial Equity** — Map renders, NFF composite updates
6. **Scenarios** — Sliders bidirectional, presets, projections, GBF compliance
7. **Justice** — 4 dimensions, regional modifiers, gap analysis
8. **Governance** — All 4 sub-tabs
9. **Indicators** — Time series, GBF table, provenance

Verify cross-module NFF weight sync:
- Set weights on Home triangle → check Scenarios sliders update
- Set a narrative preset in Scenarios → check Home triangle moves
- Use stakeholder centroid → check Scenarios reflects it

**Step 4: Commit**

```bash
git add DESCRIPTION tests/testthat/test-modules.R
git commit -m "chore: add jsonlite dependency and final integration tests"
```

---

## Summary of All Files

| Task | Creates | Modifies |
|------|---------|----------|
| 1 | `inst/extdata/narratives.json` | — |
| 2 | — | `R/fct_real_data.R`, `tests/testthat/test-modules.R` |
| 3 | `R/mod_narratives.R` | `tests/testthat/test-modules.R` |
| 4 | — | `R/mod_narratives.R` |
| 5 | — | `R/app_ui.R`, `R/app_server.R`, `nff_triangle.js`, `dev/launch.R` |
| 6 | — | `custom.css` |
| 7 | — | `nff_triangle.js`, `R/app_server.R`, `custom.css` |
| 8 | `R/mod_stakeholders.R` | `tests/testthat/test-modules.R` |
| 9 | — | `R/mod_stakeholders.R`, `tests/testthat/test-modules.R` |
| 10 | — | `nff_triangle.js` |
| 11 | — | `R/app_ui.R`, `R/app_server.R`, `dev/launch.R`, `custom.css` |
| 12 | `R/mod_pathways.R` | `tests/testthat/test-modules.R` |
| 13 | — | `R/mod_pathways.R` |
| 14 | — | `nff_triangle.js`, `custom.css` |
| 15 | — | `R/app_ui.R`, `R/app_server.R`, `dev/launch.R` |
| 16 | — | `DESCRIPTION`, `tests/testthat/test-modules.R` |

## New Files Created

| File | Purpose |
|------|---------|
| `inst/extdata/narratives.json` | Structured narrative data for 6 NFF archetypes |
| `R/mod_narratives.R` | Narrative Deep-Dive module (UI + server) |
| `R/mod_stakeholders.R` | Participatory Stakeholder Positioning module (UI + server) |
| `R/mod_pathways.R` | Three Horizons Pathway Visualization module (UI + server) |

## Estimated Commit Count

16 granular commits across 3 phases.
