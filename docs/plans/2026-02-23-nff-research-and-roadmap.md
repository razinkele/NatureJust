# NatureJust-EU: Research-Informed Feature Roadmap

> Based on analysis of the [NFF Examples](https://www.naturefuturesframework.org/examples),
> [Methodological Guidance](https://www.naturefuturesframework.org/resources/methodological-guidance),
> 15+ peer-reviewed publications, and the 2025 literature review on NFF implementation.

**Date:** 2026-02-23
**Author:** Arturas Baziukas, Klaipeda University
**Context:** NatureJust-EU is a Shiny decision-support tool for equitable marine biodiversity governance, built on the NFF, GBF, and environmental justice principles.

---

## Current State (What We Have)

| Feature | Status |
|---------|--------|
| Interactive NFF triangle with 3-color radial gradient | Done |
| 6 illustrative narrative markers (Durán et al. 2023) | Done |
| Click/drag positioning with barycentric coordinates | Done |
| Bidirectional NFF weight sync (Home ↔ Scenarios ↔ Spatial) | Done |
| SSP–NFF preset mapping (Alexander et al. 2023) | Done |
| Scenario projections with confidence bands | Done |
| GBF compliance traffic lights | Done |
| Spatial Equity map with NFF-weighted composite index | Done |
| NUTS-2 real data (Eurostat, giscoR, ICES, HELCOM, GFCM) | Done |
| Dark mode support | Done |

---

## Tier 1: High Priority — Directly Implementable

### 1.1 Participatory Stakeholder Positioning

**Source:** Rana et al. (2020); NFF Methodological Guidance (2025);";";";";"; ";"; "Catalyzing Change" literature review (2025)

**What:** Allow multiple stakeholders to each place their position on the NFF triangle, then view the collective distribution — consensus zones, divergence areas, and group centroids.

**Why:** The 2025 literature review found that "participatory efforts have ranged from 8 to more than 100 individuals" and the framework is most powerful when used as a "boundary object to enable opening up of more plural perspectives." Currently NatureJust-EU only supports a single position. Multi-stakeholder positioning is the most-requested NFF feature in practice.

**Implementation idea:**
- "Add Stakeholder" button → name + triangle click → stored as colored dot
- Show all dots simultaneously on the triangle
- Compute and display: group centroid, convex hull of positions, standard deviation ellipse
- "Consensus zone" highlight where positions cluster
- Export stakeholder positions as CSV for workshop documentation

**Complexity:** Medium
**Files:** `R/mod_home.R`, `inst/app/www/nff_triangle.js`, new `R/mod_stakeholders.R`

---

### 1.2 Three Horizons Pathway Visualization

**Source:** Pereira et al. (2020); NFF Methodological Guidance; Three Horizons Framework

**What:** Show temporal pathways on the triangle as animated dotted lines from a "current state" position toward "desired future" positions. The Three Horizons Framework identifies what needs to *decline* (Horizon 1), what *transitions* dominate (Horizon 2), and what *emerges* (Horizon 3).

**Why:** The NFF is explicitly designed as a "target-seeking approach where practitioners plot temporal pathways from current degraded conditions toward desirable futures." This is the canonical way the triangle is used in practice, but no digital tool has implemented it interactively.

**Implementation idea:**
- User places two markers: "Now" (current governance state) and "Future" (desired state)
- Animated dotted path connects them through the triangle state space
- Timeline slider (2025 → 2030 → 2050) animates the position along the path
- Three Horizons panel alongside shows which governance features decline/emerge/transition
- Multiple pathways can be drawn for comparative analysis

**Complexity:** Medium-High
**Files:** `inst/app/www/nff_triangle.js`, `R/mod_home.R` or new `R/mod_pathways.R`

---

### 1.3 Marine Narrative Deep-Dive Panel

**Source:** Durán et al. (2023) illustrative narratives; Pereira et al. (2023) "The Living Infinite"

**What:** When a user clicks a narrative marker (diamond) or selects a narrative from a dropdown, show a rich detail panel with: governance model, policy instruments, marine spatial planning implications, real-world analogues, and indicator projections.

**Why:** The 6 narratives (Arcology, Sharing through Sparing, Optimizing Nature, Innovative Commons, Reciprocal Stewardship, Dynamic Natures) each have specific marine governance implications from Durán et al. Currently we show brief tooltips, but a full panel would ground the tool in published research and help policymakers understand the practical meaning of each position.

**Implementation idea:**
- Expandable card or modal triggered by narrative selection
- Content sections: Description, Governance Model, Key Policies, Marine Examples, Trade-offs
- Connect to scenario engine: "Run this narrative as a scenario" button
- Show which GBF targets each narrative prioritizes
- Include references to source publications

**Complexity:** Low-Medium
**Files:** `R/mod_home.R` or `R/mod_narratives.R`, `inst/extdata/narratives.csv`

---

### 1.4 Indicator Gap Analysis for Relational Values

**Source:** "Catalyzing Change" 2025 literature review; Kim et al. (2023)

**What:** Add explicit indicators for "Nature as Culture" (relational values), which the literature identifies as the weakest dimension in current NFF implementations.

**Why:** The 2025 review found "ongoing challenges in operationalising relational values." Kim et al. (2023) note "limited availability of indicators for certain nature value perspectives, particularly nature as culture." This is a known gap that NatureJust-EU could help address for marine contexts.

**Candidate relational indicators for EU marine regions:**
- Traditional fishing communities count (from national statistics)
- Cultural heritage marine sites (UNESCO, national registers)
- Indigenous/traditional ecological knowledge documentation index
- Coastal community wellbeing surveys
- Seascape character assessments
- Maritime cultural festival/event density

**Complexity:** Medium (data sourcing is the challenge)
**Files:** `inst/extdata/` new CSV files, `R/fct_real_data.R`, `R/mod_spatial.R`

---

## Tier 2: Medium Priority — Enhances Analytical Depth

### 2.1 Multi-Objective Optimization (Pareto Frontiers)

**Source:** Haga et al. (2023) — Sustainability Science

**What:** Replace or complement stochastic scenario projections with Pareto-optimal frontiers. Users could see trade-off curves between NFF perspectives rather than single trajectories.

**Why:** Haga et al. demonstrated that combining NFF with multi-objective optimization identifies "multiple, but few nature-positive and Pareto optimal strategies that satisfied NFF visions." This quantitative grounding is what policymakers need — not just "what could happen" but "what are the best achievable outcomes given trade-offs."

**Implementation idea:**
- For a given region and time horizon, compute multiple scenario runs across the NFF space
- Identify Pareto-optimal points (no scenario dominates another on all indicators)
- Visualize as a 3D Pareto surface projected onto the triangle
- Highlight "nature-positive" strategies vs. non-nature-positive ones
- Allow users to select a Pareto-optimal point and see its full indicator profile

**Complexity:** High
**Files:** New `R/fct_optimization.R`, `R/mod_scenarios.R`

---

### 2.2 Cross-Scale Integration

**Source:** "Catalyzing Change" 2025 review — identified as the #1 gap

**What:** Support multi-scale scenario analysis — e.g., how does a Baltic Sea regional scenario interact with national-level or EU-wide policy?

**Why:** The 2025 literature review found "no studies have yet integrated scenarios and models across multiple scales within a single project." This is the most significant gap in NFF research. NatureJust-EU's existing NUTS-2 data + sea basin structure is well-suited to demonstrate cross-scale integration.

**Implementation idea:**
- Nested analysis: EU → Sea Basin → NUTS-2 region
- Show how NFF weights at the regional level aggregate to basin-level outcomes
- Identify regions that are "outliers" relative to their basin's trajectory
- Compare national policy contexts across regions sharing a sea basin

**Complexity:** High
**Files:** New data aggregation functions, `R/mod_spatial.R`, `R/mod_scenarios.R`

---

### 2.3 Science Fiction Narratives / Creative Futures

**Source:** Pereira et al. (2023) "The Living Infinite" — Marine Policy

**What:** Integrate creative narrative elements — short speculative fiction vignettes for each NFF position that bring marine futures to life.

**Why:** Pereira et al. found that "engaging with the future through science fiction narratives allowed a more radical appreciation of what could be" and that "creative endeavours of co-production that promote and encourage imagination should be considered as important tools in the science-policy interface." This addresses the psychological barrier of ocean remoteness.

**Implementation idea:**
- For each of the 6 narratives, include a 200-word "Day in the Life 2050" vignette
- Optionally illustrated with AI-generated seascape imagery
- Accessible from the narrative detail panel (see 1.3)
- Could be collaboratively written with marine stakeholders

**Complexity:** Low (content creation, not code)
**Files:** `inst/extdata/narratives.csv` or `inst/app/www/narratives/`

---

### 2.4 Freshwater/Catchment Extension

**Source:** Kramer et al. (2023) — Sustainability Science

**What:** Expand indicator coverage to include freshwater ecosystem indicators for catchment-level planning.

**Why:** Kramer et al. explored NFF for "improved assessment and modeling of freshwater systems." Coastal regions in the EU are connected to river catchments — agricultural runoff, dam impacts, and water quality link marine and freshwater governance.

**Candidate indicators:**
- River water quality (EEA WISE database)
- Freshwater biodiversity index
- Catchment land use intensity
- Nutrient loading to coastal waters

**Complexity:** Medium
**Files:** `data-raw/prepare_data.R`, new data files, `R/fct_real_data.R`

---

## Tier 3: Visionary — Future Development

### 3.1 Urban Coastal NFF Adaptation

**Source:** Mansur et al. (2022) — Environmental Science & Policy

**What:** Adapt the NFF for urban coastal contexts where cities are simultaneously marine and urban governance actors (e.g., Barcelona, Copenhagen, Gdańsk).

---

### 3.2 Citizen Science Integration

**Source:** Diprose et al. (2022) — Citizen Science: Theory and Practice

**What:** Connect to citizen science platforms to use community-generated biodiversity observations as NFF indicators (e.g., iNaturalist data → "Nature for Nature" metric; cultural site visits → "Nature as Culture" metric).

---

### 3.3 Real-Time Policy Tracker

**What:** Monitor EU policy developments (Common Fisheries Policy, Marine Strategy Framework Directive, Habitats Directive) and automatically flag changes relevant to each NFF perspective.

---

### 3.4 AI-Assisted Scenario Narration

**What:** Use LLM-generated text to automatically produce scenario narratives based on the user's NFF position, selected region, and indicator projections. Each scenario could include policy recommendations tailored to the specific governance context.

---

## Key Publications Referenced

| # | Author(s) | Year | Title | Relevance |
|---|-----------|------|-------|-----------|
| 1 | Pereira et al. | 2020 | Developing multiscale nature-people scenarios | Original NFF triangle design |
| 2 | Rana et al. | 2020 | Youth visions for nature futures | Participatory positioning |
| 3 | Resende et al. | 2020 | Brazilian Cerrado water ecosystem services | NFF as exploration tool |
| 4 | Mansur et al. | 2022 | Urban nature management visions | Urban NFF adaptation |
| 5 | Diprose et al. | 2022 | NZ Garden Bird Survey | Citizen science + NFF |
| 6 | Quintero-Uribe et al. | 2022 | European landscape restoration | Scenario assessment |
| 7 | Greenway | 2022 | River-human relationships | Relational values theory |
| 8 | Alexander et al. | 2023 | SSP–NFF mapping | Climate-biodiversity bridge |
| 9 | Durán et al. | 2023 | Illustrative narratives | 6 narratives + marine policies |
| 10 | Dou et al. | 2023 | European land use scenarios 2050 | EU-scale NFF scenarios |
| 11 | Haga et al. | 2023 | Multi-objective optimization + NFF | Pareto-optimal futures |
| 12 | Kim et al. | 2023 | Modelling Nature Futures | Quantitative operationalization |
| 13 | Kramer et al. | 2023 | NFF for freshwater ecosystems | Expanded indicator set |
| 14 | Pereira et al. | 2023 | The Living Infinite (Marine Policy) | High-seas NFF application |
| 15 | IPBES | 2025 | NFF Methodological Guidance | Official operational framework |
| 16 | "Catalyzing Change" | 2025 | Literature review (31 studies) | Implementation gaps |

---

## Recommended Implementation Order

```
Phase 1 (Current sprint):  1.3 Narrative Deep-Dive Panel
                            ↓
Phase 2 (Next sprint):     1.1 Participatory Positioning
                           1.4 Relational Value Indicators
                            ↓
Phase 3 (Research sprint): 1.2 Three Horizons Pathways
                           2.3 Creative Futures Vignettes
                            ↓
Phase 4 (Advanced):        2.1 Pareto Optimization
                           2.2 Cross-Scale Integration
                           2.4 Freshwater Extension
```

The recommended order prioritizes features that:
1. Build on existing infrastructure (narrative data is already in the JS)
2. Address the most cited gaps in the 2025 literature review
3. Differentiate NatureJust-EU from generic scenario tools
4. Are feasible within the HORIZON EUROPE project timeline
