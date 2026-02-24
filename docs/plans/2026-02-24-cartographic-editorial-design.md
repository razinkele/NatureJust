# NatureJust-EU — Cartographic Editorial Visual Polish

**Date:** 2026-02-24
**Direction:** Cartographic Editorial — magazine-like with dramatic typography, asymmetric layouts, paper textures, minimal-rule tables.
**Audience:** Dual-purpose — conference demos + EU reviewers AND daily marine planner working tool.
**Scope:** All 9 module pages. Primarily CSS changes (~90%), with minor R module HTML additions for Justice scorecard progress bars and Governance table styling.

---

## 1. Typography

- **Display font:** Playfair Display (already loaded via Google Fonts in `golem_add_external_resources`)
- **Body font:** Figtree (already loaded)
- CSS variable update:
  ```css
  --font-display: 'Playfair Display', Georgia, 'Times New Roman', serif;
  --font-body: 'Figtree', system-ui, -apple-system, 'Segoe UI', sans-serif;
  ```

### Scale
| Element | Current | New |
|---------|---------|-----|
| Hero h1 | 2.8rem Georgia | 3.8rem Playfair 800, -0.02em tracking |
| Module hero h2 | ~1.5rem Georgia | 2.2rem Playfair 700 |
| Section h4/h5 | ~1.25rem | 1.3rem Playfair 700, with bottom hairline ornament |
| Stat numbers (justice %) | ~1.5rem | 2.8rem Playfair 700, `%` at 1.4rem |
| Body text | 0.87rem system-ui | 0.87rem Figtree |
| Labels/uppercase | 0.75rem system-ui | 0.72rem Figtree, 0.08em tracking |
| Navbar tabs | 0.8rem | 0.76rem Figtree, uppercase, 0.06em tracking |

## 2. Page Background & Texture

Subtle SVG noise pattern overlaid on `body::after` at very low opacity (0.03). Creates tactile paper feel without loading external images.

```css
body::after {
  content: '';
  position: fixed;
  inset: 0;
  pointer-events: none;
  z-index: 9998;
  opacity: 0.03;
  background-image: url("data:image/svg+xml,..."); /* inline noise SVG */
}
```

Dark mode: same technique but inverted (light noise on dark background) at opacity 0.02.

## 3. Card Treatment — Asymmetric Editorial

Replace current uniform thin-border + shadow with:
- Left border: 3px solid `var(--nj-ocean)` (default), contextual colors for specific cards
- Top/right/bottom: 1px solid `rgba(10,22,40,.06)`
- Shadow: minimal `0 1px 3px rgba(10,22,40,.04)`
- Hover: border-left-color brightens, translateX(2px) shift
- Card headers: transparent background, no bottom border, just typography
- Remove card-header bg-primary gradient (replace with left-border color accent)

Specific card border colors:
- Nature/teal cards: `var(--nj-teal)`
- Society/coral cards: `var(--nj-coral)`
- Culture/sky cards: `var(--nj-sky)`
- Default: `var(--nj-ocean)`
- Justice cards: status-dependent (green/amber/red per existing pattern)

## 4. Hero Sections

All hero sections (`.home-hero`, `.narratives-hero`, `.stakeholders-hero`, `.pathways-hero`):
- Increase vertical padding: `4rem 2rem 2.5rem`
- Wider decorative gradient rule: 80px width, 2px height, gradient from coral→teal
- Remove animated radial gradient background from home-hero (clean whitespace)
- Subtitle: italic Playfair, line-height 1.7, max-width 600px
- Descriptive text: Figtree, 0.88rem, max-width 560px

## 5. Justice Scorecard (R + CSS)

### R change in `mod_justice.R`:
Add a `<div class="justice-progress">` after each score percentage, with a `style` attribute setting `--score: XX%` for the CSS-driven progress bar.

### CSS:
```css
.justice-card h3 {
  font-size: 2.8rem;
  font-family: var(--font-display);
  font-weight: 700;
}
.justice-card h3 .pct-symbol { font-size: 1.4rem; }
.justice-progress {
  height: 4px;
  background: var(--nj-mist);
  border-radius: 2px;
  overflow: hidden;
  margin-top: 0.5rem;
}
.justice-progress::after {
  content: '';
  display: block;
  height: 100%;
  width: var(--score);
  background: linear-gradient(90deg, var(--nj-teal), var(--nj-blue));
  border-radius: 2px;
  transition: width 0.6s var(--ease-expo);
}
```

Status label: uppercase Figtree badge, 0.68rem, muted background.

## 6. Governance Tables — Minimal-Rule Editorial

### DT formatStyle changes in `mod_governance.R`:
Replace cell background colors with text-color + border-left approach.

### CSS:
- Remove `.dataTable tbody tr:nth-child(odd)` striping
- Horizontal hairlines only (`border-bottom: 1px solid rgba(10,22,40,.06)`)
- Header row: 2px bottom border (keeping current)
- Status cells: colored text (green=#0E7C7B, amber=#C1800B, red=#C1666B) with a small `::before` circle pip
- More generous row padding: 0.75rem 1rem

## 7. Sidebar Polish

```css
.sidebar {
  background: var(--nj-warm-white);
  border-right: 1px solid var(--nj-mist);
}
.sidebar h6 {
  font-family: var(--font-body);
  font-variant: small-caps;
  letter-spacing: 0.1em;
  border-bottom: 1px solid var(--nj-mist);
  padding-bottom: 0.4rem;
  margin-bottom: 0.6rem;
}
```

Checkboxes: increase vertical gap (margin-bottom: 0.35rem).
Form controls: slightly warmer border color, refined focus ring.

## 8. Navigation Bar

- Slim: padding `0.35rem 1.5rem`
- Active tab: 3px bottom coral accent (up from 2px)
- Tab text: `text-transform: uppercase; letter-spacing: 0.06em; font-size: 0.76rem`
- Brand: Playfair Display 600, 1.15rem

## 9. Animations

- All cards: `animation: fadeSlideIn 0.5s var(--ease-expo) both` with staggered delays
- `@keyframes fadeSlideIn { from { opacity: 0; transform: translateX(-8px) translateY(10px); } }`
- Delay increments: 0.06s per child
- Justice progress bars: `transition: width 0.8s var(--ease-expo)` on load
- Respect `prefers-reduced-motion: reduce`

## 10. Dark Mode

- Paper texture → ink-wash (invert noise, opacity 0.02)
- Cards: `border-color: rgba(255,255,255,.08)`
- Coral accents brighten to `#F09070` for contrast
- Justice progress bars: brighter gradient (`--nj-sky` → `--nj-coral`)
- Table hairlines: `rgba(255,255,255,.08)`

---

## Implementation Order

1. **CSS Typography** — font variables, heading scale, body text
2. **CSS Background** — paper grain texture
3. **CSS Cards** — asymmetric border treatment
4. **CSS Heroes** — all 4 hero sections
5. **CSS Navbar** — slim, uppercase tabs
6. **CSS Sidebar** — small-caps headers, spacing
7. **CSS Tables** — minimal-rule editorial style
8. **CSS Animations** — staggered entrance
9. **R: mod_justice.R** — progress bar HTML elements
10. **R: mod_governance.R** — DT formatStyle adjustments
11. **CSS Dark Mode** — all dark mode refinements
12. **Test & Deploy** — verify all pages, push, deploy

## Files Modified

- `inst/app/www/custom.css` — primary change (rewrite/extend)
- `R/mod_justice.R` — add progress bar divs to scorecard output
- `R/mod_governance.R` — adjust DT formatStyle calls for editorial cells
