# Cartographic Editorial Visual Polish — Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Polish all 9 NatureJust-EU pages with a Cartographic Editorial aesthetic — Playfair Display + Figtree typography, asymmetric card borders, paper grain texture, minimal-rule tables, justice progress bars.

**Architecture:** CSS-first approach. Rewrite `inst/app/www/custom.css` to implement the new design system. Two R modules get minor HTML additions: `mod_justice.R` (progress bar divs) and `mod_governance.R` (DT styling adjustments). No layout or structural changes.

**Tech Stack:** CSS3 (custom properties, pseudo-elements, SVG data URIs), R/Shiny (bslib, DT), deployed to Shiny Server on laguna.ku.lt.

**Design doc:** `docs/plans/2026-02-24-cartographic-editorial-design.md`

---

### Task 1: Update CSS Design Tokens & Typography

**Files:**
- Modify: `inst/app/www/custom.css:12-44` (`:root` block)

**Step 1: Replace font variables**

Change the font-family CSS custom properties to use Google Fonts:
```css
--font-display: 'Playfair Display', Georgia, 'Times New Roman', serif;
--font-body: 'Figtree', system-ui, -apple-system, 'Segoe UI', sans-serif;
```

**Step 2: Add new design tokens**

Add to the `:root` block:
```css
--nj-amber-text: #C1800B;
--nj-green-text: #0E7C7B;
--nj-red-text: #C1666B;
--ease-editorial: cubic-bezier(0.25, 0.46, 0.45, 0.94);
```

**Step 3: Verify fonts load**

Open https://laguna.ku.lt/naturejust/ in browser, inspect computed font-family on `h1` and `body` text. Should show Playfair Display and Figtree respectively (already loaded in HTML head via Google Fonts).

**Step 4: Commit**
```bash
git add inst/app/www/custom.css
git commit -m "feat(css): update design tokens to Playfair Display + Figtree"
```

---

### Task 2: CSS Base, Navbar & Background Texture

**Files:**
- Modify: `inst/app/www/custom.css:57-126` (body, navbar sections)

**Step 1: Add paper grain texture to body**

After the `body { ... }` rule, add `body::after` pseudo-element with inline SVG noise pattern at opacity 0.025. The SVG should be a simple `<filter>` with `feTurbulence` for organic noise.

**Step 2: Slim down navbar**

- Reduce padding to `0.3rem 1.5rem`
- Tab text: `text-transform: uppercase; letter-spacing: 0.06em; font-size: 0.76rem; font-family: var(--font-body)`
- Active tab underline: 3px coral (up from 2px), width 24px
- Brand: `font-size: 1.15rem; font-weight: 600`

**Step 3: Verify visually** — navbar should be slimmer, tabs uppercase, paper grain barely visible on light background.

**Step 4: Commit**
```bash
git add inst/app/www/custom.css
git commit -m "feat(css): add paper grain texture, slim editorial navbar"
```

---

### Task 3: CSS Card Treatment — Asymmetric Editorial Borders

**Files:**
- Modify: `inst/app/www/custom.css:128-165` (card section)

**Step 1: Replace card styling**

Cards get asymmetric treatment:
```css
.card {
  background: #fff;
  border: none !important;
  border-left: 3px solid var(--nj-ocean) !important;
  border-top: 1px solid rgba(10,22,40,.06) !important;
  border-radius: 2px var(--radius-sm) var(--radius-sm) 2px !important;
  box-shadow: 0 1px 3px rgba(10,22,40,.04);
  transition: all .35s var(--ease-editorial);
}
.card:hover {
  border-left-color: var(--nj-blue) !important;
  transform: translateX(2px);
  box-shadow: 0 2px 8px rgba(10,22,40,.08);
}
```

**Step 2: Update card headers**

Remove gradient backgrounds, use clean typography:
```css
.card-header {
  background: transparent !important;
  border-bottom: 1px solid rgba(10,22,40,.06) !important;
  font-family: var(--font-display) !important;
  font-weight: 600;
  font-size: 0.95rem;
  color: var(--nj-navy) !important;
  padding: 0.8rem 1.15rem !important;
}
.card-header.bg-primary {
  background: transparent !important;
  color: var(--nj-navy) !important;
  border-bottom: 1px solid rgba(10,22,40,.06) !important;
}
```

**Step 3: Verify** — cards should show thick left ocean-blue border, thin top hairline, minimal shadow. Headers should be clean Playfair text without gradient backgrounds.

**Step 4: Commit**
```bash
git add inst/app/www/custom.css
git commit -m "feat(css): asymmetric editorial card borders"
```

---

### Task 4: CSS Hero Sections

**Files:**
- Modify: `inst/app/www/custom.css:244-311` (hero section)

**Step 1: Update all hero sections**

Home hero: larger type, remove animated background gradient, wider decorative rule.
```css
.home-hero {
  text-align: center;
  padding: 4rem 2rem 2.5rem;
  position: relative;
}
/* Remove the animated ::before gradient — editorial restraint */
.home-hero::before { display: none; }

.home-hero h1 {
  font-family: var(--font-display) !important;
  font-weight: 800 !important;
  font-size: 3.8rem !important;
  color: var(--nj-navy) !important;
  letter-spacing: -0.02em;
  line-height: 1.05;
  margin-bottom: 0.5rem;
}
.home-hero h1::after {
  width: 80px;
  height: 2px;
  background: linear-gradient(90deg, var(--nj-coral), var(--nj-teal));
}
.home-hero .subtitle {
  font-family: var(--font-display) !important;
  font-style: italic;
  font-size: 1.15rem !important;
  line-height: 1.7;
  max-width: 600px;
}
```

Module heroes: consistent styling at 2.2rem.
```css
.narratives-hero h2,
.stakeholders-hero h2,
.pathways-hero h2 {
  font-family: var(--font-display);
  font-size: 2.2rem;
  font-weight: 700;
  color: var(--nj-navy);
  letter-spacing: -0.01em;
}
```

**Step 2: Verify** — home hero should have oversized Playfair heading, clean whitespace (no animated blobs), wide gradient rule.

**Step 3: Commit**
```bash
git add inst/app/www/custom.css
git commit -m "feat(css): editorial hero sections with dramatic typography"
```

---

### Task 5: CSS Tables — Minimal-Rule Editorial

**Files:**
- Modify: `inst/app/www/custom.css:692-729` (DataTables section)

**Step 1: Replace table styling**

Minimal rules, no alternating rows, generous padding:
```css
table.dataTable { border-collapse: collapse; }
table.dataTable thead th {
  font-family: var(--font-body) !important;
  font-weight: 700 !important;
  font-size: 0.7rem;
  text-transform: uppercase;
  letter-spacing: 0.08em;
  color: var(--nj-navy);
  border-bottom: 2px solid var(--nj-ocean) !important;
  border-top: none !important;
  padding: 0.75rem 1rem !important;
}
table.dataTable tbody td {
  padding: 0.75rem 1rem !important;
  border-bottom: 1px solid rgba(10,22,40,.06) !important;
  border-top: none !important;
  font-size: 0.84rem;
  vertical-align: middle;
}
/* Remove alternating row backgrounds */
table.dataTable tbody tr { background: transparent !important; }
table.dataTable tbody tr:hover {
  background: rgba(42,111,151,.02) !important;
}
```

**Step 2: Commit**
```bash
git add inst/app/www/custom.css
git commit -m "feat(css): minimal-rule editorial tables"
```

---

### Task 6: CSS Sidebar Polish

**Files:**
- Modify: `inst/app/www/custom.css:167-202` (sidebar section)

**Step 1: Update sidebar headers to small-caps with hairline**

```css
.sidebar h6,
.sidebar .sidebar-title ~ h6 {
  font-family: var(--font-body);
  font-variant: small-caps;
  letter-spacing: 0.1em;
  font-size: 0.78rem;
  border-bottom: 1px solid var(--nj-mist);
  padding-bottom: 0.4rem;
  margin-bottom: 0.65rem;
  color: var(--nj-stone);
}
```

Better checkbox spacing:
```css
.sidebar .checkbox, .sidebar .radio {
  margin-bottom: 0.35rem;
}
.sidebar .pretty { margin-bottom: 0.3rem; }
```

**Step 2: Commit**
```bash
git add inst/app/www/custom.css
git commit -m "feat(css): editorial sidebar with small-caps headers"
```

---

### Task 7: CSS Section Headings & Value Boxes

**Files:**
- Modify: `inst/app/www/custom.css:803-818` (headings), `601-627` (value boxes)

**Step 1: Update heading decorations**

```css
h4, h5 {
  font-family: var(--font-display);
  font-weight: 700;
  color: var(--nj-navy);
  letter-spacing: -0.01em;
  font-size: 1.3rem;
}
h4.text-center::after {
  width: 48px;
  height: 2px;
  background: linear-gradient(90deg, var(--nj-coral), var(--nj-teal));
}
```

**Step 2: Update value boxes**

Value box values in Playfair Display:
```css
.bslib-value-box .value-box-value {
  font-family: var(--font-display) !important;
  font-weight: 700 !important;
  font-size: 1.6rem !important;
}
```

**Step 3: Commit**
```bash
git add inst/app/www/custom.css
git commit -m "feat(css): editorial headings with decorative rules"
```

---

### Task 8: CSS Animations — Staggered Entrance

**Files:**
- Modify: `inst/app/www/custom.css:855-869` (animations section)

**Step 1: Replace vertical fade-in with diagonal editorial slide**

```css
@keyframes fadeSlideIn {
  from { opacity: 0; transform: translateX(-8px) translateY(10px); }
  to   { opacity: 1; transform: translateX(0) translateY(0); }
}

.tab-pane > .bslib-gap-spacing > .card,
.tab-pane > .bslib-gap-spacing > .bslib-value-box,
.tab-pane > .bslib-gap-spacing > .bslib-layout-columns,
.tab-pane > .bslib-gap-spacing > h5 {
  animation: fadeSlideIn 0.5s var(--ease-editorial) both;
}

.tab-pane > .bslib-gap-spacing > *:nth-child(1) { animation-delay: 0s; }
.tab-pane > .bslib-gap-spacing > *:nth-child(2) { animation-delay: 0.06s; }
.tab-pane > .bslib-gap-spacing > *:nth-child(3) { animation-delay: 0.12s; }
.tab-pane > .bslib-gap-spacing > *:nth-child(4) { animation-delay: 0.18s; }
.tab-pane > .bslib-gap-spacing > *:nth-child(5) { animation-delay: 0.24s; }
.tab-pane > .bslib-gap-spacing > *:nth-child(6) { animation-delay: 0.30s; }
```

**Step 2: Commit**
```bash
git add inst/app/www/custom.css
git commit -m "feat(css): staggered diagonal entrance animations"
```

---

### Task 9: R — Justice Scorecard Progress Bars

**Files:**
- Modify: `R/mod_justice.R:111-135` (scorecard renderUI)

**Step 1: Add progress bar div to justice scorecard cards**

In `output$scorecard <- renderUI({...})`, modify the card body (line ~127-131) to include a progress bar div after the percentage:

```r
bslib::card_body(
  h3(
    HTML(paste0(score_pct, "<span class='pct-symbol'>%</span>")),
    class = "mb-1"
  ),
  p(class = "text-muted mb-1", status_to_label(status)),
  div(class = "justice-progress",
      style = paste0("--score:", score_pct, "%")),
  p(class = "small", df$description[i])
)
```

**Step 2: Add the same to tenet scorecard in `R/mod_governance.R:258-263`**

```r
bslib::card_body(
  class = "p-3",
  h3(
    HTML(paste0(score_pct, "<span class='pct-symbol'>%</span>")),
    class = "mb-1"
  ),
  p(class = "text-muted mb-1", status_to_label(status)),
  div(class = "justice-progress",
      style = paste0("--score:", score_pct, "%")),
  p(class = "small mb-0", df$description[i])
)
```

**Step 3: Add CSS for progress bars**

In `custom.css` after the justice-card rules:
```css
.justice-card h3 {
  font-family: var(--font-display) !important;
  font-weight: 700 !important;
  font-size: 2.8rem !important;
  color: var(--nj-navy);
  line-height: 1;
}
.pct-symbol {
  font-size: 1.4rem;
  font-weight: 600;
  opacity: 0.6;
}
.justice-progress {
  height: 4px;
  background: var(--nj-mist);
  border-radius: 2px;
  overflow: hidden;
  margin: 0.5rem 0;
}
.justice-progress::after {
  content: '';
  display: block;
  height: 100%;
  width: var(--score);
  border-radius: 2px;
  transition: width 0.8s var(--ease-expo);
}
.status-green .justice-progress::after {
  background: linear-gradient(90deg, var(--nj-teal), var(--nj-seafoam));
}
.status-amber .justice-progress::after {
  background: linear-gradient(90deg, var(--nj-sand), #E0B06B);
}
.status-red .justice-progress::after {
  background: linear-gradient(90deg, var(--nj-coral), var(--nj-rose));
}
```

**Step 4: Commit**
```bash
git add R/mod_justice.R R/mod_governance.R inst/app/www/custom.css
git commit -m "feat: add justice scorecard progress bars with animated fill"
```

---

### Task 10: R — Governance Table Editorial Styling

**Files:**
- Modify: `R/mod_governance.R:119-138` (funding_table DT render)

**Step 1: Update DT formatStyle to use text color instead of background fill**

Replace the existing `DT::formatStyle()` call:

```r
DT::datatable(
  df,
  options = list(
    pageLength = 10,
    dom = "ft",
    columnDefs = list(list(className = "dt-center", targets = 1:5))
  ),
  rownames = FALSE,
  class = "cell-border"
) |>
  DT::formatStyle(
    columns = c("EMFAF", "LIFE", "Cohesion Fund", "EAFRD",
                 "Just Transition Fund"),
    color = DT::styleEqual(
      c("Eligible", "Partial", "Not eligible"),
      c("#0E7C7B", "#C1800B", "#C1666B")
    ),
    fontWeight = DT::styleEqual(
      c("Eligible", "Partial", "Not eligible"),
      c("600", "600", "400")
    ),
    backgroundColor = DT::styleEqual(
      c("Eligible", "Partial", "Not eligible"),
      c("rgba(14,124,123,.06)", "rgba(242,204,143,.08)", "rgba(224,122,95,.04)")
    )
  )
```

**Step 2: Commit**
```bash
git add R/mod_governance.R
git commit -m "feat: editorial table styling with colored text instead of fills"
```

---

### Task 11: CSS Dark Mode Refinements

**Files:**
- Modify: `inst/app/www/custom.css` (dark mode section `[data-bs-theme="dark"]`)

**Step 1: Update dark mode variables and card styling**

```css
[data-bs-theme="dark"] .card {
  background: var(--nj-warm-white);
  border-color: rgba(255,255,255,.08) !important;
  border-left-color: var(--nj-sky) !important;
}
[data-bs-theme="dark"] .card-header {
  color: var(--nj-charcoal) !important;
  border-bottom-color: rgba(255,255,255,.06) !important;
}
[data-bs-theme="dark"] body::after {
  opacity: 0.015;
  filter: invert(1);
}
[data-bs-theme="dark"] .justice-card h3 { color: var(--nj-charcoal); }
[data-bs-theme="dark"] table.dataTable tbody td {
  border-bottom-color: rgba(255,255,255,.06) !important;
}
```

**Step 2: Commit**
```bash
git add inst/app/www/custom.css
git commit -m "feat(css): dark mode refinements for editorial theme"
```

---

### Task 12: Deploy & Visual Verification

**Step 1: Push all changes**
```bash
git push
```

**Step 2: Deploy to server**
```bash
bash deploy.sh
```

**Step 3: Visual verification**

Open https://laguna.ku.lt/naturejust/ and check all 9 tabs:
- [ ] Home: Playfair Display hero at 3.8rem, no animated blobs, clean whitespace
- [ ] Narratives: 2.2rem heading, asymmetric cards with left border
- [ ] Stakeholders: consistent card treatment, Figtree body text
- [ ] Pathways: sidebar with small-caps headers
- [ ] Spatial Equity: map card with left border, minimal-rule table if visible
- [ ] Scenarios: slim navbar, staggered card entrance
- [ ] Justice: progress bars on scorecard cards, 2.8rem Playfair percentages
- [ ] Governance: editorial table with colored text (not background fills)
- [ ] Indicators: consistent card and table styling
- [ ] Dark mode toggle: verify all pages in both light and dark

---

## Files Modified Summary

| File | Change Type | Description |
|------|------------|-------------|
| `inst/app/www/custom.css` | Major rewrite | Typography, cards, heroes, tables, sidebars, animations, dark mode |
| `R/mod_justice.R` | Minor | Add `<div class="justice-progress">` + `<span class="pct-symbol">` to scorecard |
| `R/mod_governance.R` | Minor | Update DT formatStyle to colored text + subtle backgrounds |
