# ============================================================================
# irbs_setup.R
# IRBS bslib theme, font loading, global CSS, and footer helpers.
# Source this file first in app.R before any other IRBS component.
# ============================================================================

library(bslib)
library(htmltools)

# ── Bootstrap 5 theme ────────────────────────────────────────────────────────
# Maps IRBS tokens to Bootstrap semantic variables so standard bslib
# components (buttons, badges, value_box, etc.) automatically use IRBS colors.

irbs_theme <- bs_theme(
  version = 5,
  primary = "#FF5F05", # illini-orange  — buttons, links, active nav
  secondary = "#13294B", # illini-blue    — secondary actions, nav bg
  success = "#006230", # prairie        — positive states
  warning = "#FCB316", # harvest        — warnings
  danger = "#C84113", # altgeld        — errors
  info = "#009FD4", # arches         — info callouts
  "body-color" = "#13294B", # illini-blue    — default text
  "body-bg" = "#FFFFFF",
  base_font = font_google("Source Sans 3"),
  heading_font = font_google("Montserrat"),
  code_font = font_google("Source Code Pro")
)

# ── Atkinson Hyperlegible (data labels / accessible text) ────────────────────
# Not in bslib's three named font slots; loaded separately via Google Fonts.

atkinson_link <- tags$link(
  rel = "stylesheet",
  href = paste0(
    "https://fonts.googleapis.com/css2?family=Atkinson+Hyperlegible",
    ":ital,wght@0,400;0,700;1,400&display=swap"
  )
)

# ── Global CSS ───────────────────────────────────────────────────────────────
# CSS custom properties bridge IRBS tokens to component styles.
# Inject once via header = tagList(atkinson_link, irbs_css) in page_navbar().

# HTML() is required: without it htmltools escapes ">" (child selectors)
# to "&gt;", which silently breaks those CSS rules.
irbs_css <- tags$style(HTML(
  "

  /* ── IRBS design tokens ──────────────────────────────────────────────── */
  :root {
    --irbs-orange:      #FF5F05;
    --irbs-blue:        #13294B;
    --irbs-storm:       #707372;
    --irbs-storm-mid:   #9C9A9D;
    --irbs-storm-light: #C8C6C7;
    --irbs-prairie:     #006230;
    --irbs-harvest:     #FCB316;
    --irbs-altgeld:     #C84113;
    --irbs-industrial:  #1D58A7;
    --irbs-patina:      #007E8E;
    --irbs-arches:      #009FD4;
    --irbs-font-display:    'Montserrat', system-ui, sans-serif;
    --irbs-font-sans:       'Source Sans 3', system-ui, sans-serif;
    --irbs-font-accessible: 'Atkinson Hyperlegible', system-ui, sans-serif;
  }

  /* ── MetricCard ──────────────────────────────────────────────────────── */
  .irbs-metric-card {
    background: #fff;
    border: 1px solid var(--irbs-storm-light);
    border-radius: 12px;
    padding: 20px;
    box-shadow: 0 1px 3px rgba(19,41,75,.10);
    display: flex;
    flex-direction: column;
    gap: 6px;
    height: 100%;
  }
  .irbs-metric-label {
    font-family: var(--irbs-font-accessible);
    font-size: 11px;
    font-weight: 700;
    letter-spacing: .07em;
    text-transform: uppercase;
    color: var(--irbs-storm);
  }
  .irbs-metric-value {
    font-family: var(--irbs-font-display);
    font-size: 36px;
    font-weight: 700;
    line-height: 1.05;
    color: var(--irbs-blue);
  }
  .irbs-metric-unit {
    font-size: 15px;
    font-weight: 400;
    color: var(--irbs-storm);
    margin-left: 3px;
  }
  .irbs-metric-sub {
    font-family: var(--irbs-font-accessible);
    font-size: 14.4px;   /* 12px × 1.2 */
    color: var(--irbs-storm);
    margin-top: 4px;
  }
  .irbs-delta-up      { color: var(--irbs-prairie);  font-weight: 700; }
  .irbs-delta-down    { color: var(--irbs-altgeld);  font-weight: 700; }
  .irbs-delta-neutral { color: var(--irbs-storm);    font-weight: 700; }

  /* ── Data table ──────────────────────────────────────────────────────── */
  .irbs-table-wrap {
    border: 1px solid var(--irbs-storm-light);
    border-radius: 6px;
    overflow: hidden;
    box-shadow: 0 1px 3px rgba(19,41,75,.10);
  }
  .irbs-table-wrap table {
    width: 100%;
    border-collapse: collapse;
    font-family: var(--irbs-font-accessible);
    font-size: 13px;
  }
  .irbs-table-wrap thead tr { background: var(--irbs-blue); }
  .irbs-table-wrap thead th {
    font-size: 11px; font-weight: 700; letter-spacing: .06em;
    text-transform: uppercase; color: #fff;
    padding: 10px 14px; text-align: left; white-space: nowrap;
  }
  .irbs-table-wrap thead th.num { text-align: right; }
  .irbs-table-wrap tbody tr { border-bottom: 1px solid var(--irbs-storm-light); }
  .irbs-table-wrap tbody tr:last-child { border-bottom: none; }
  .irbs-table-wrap tbody tr:nth-child(even) { background: #F7F6F7; }
  .irbs-table-wrap tbody tr:hover { background: #EEF2F8; }
  .irbs-table-wrap tbody td {
    padding: 9px 14px; color: var(--irbs-blue); vertical-align: middle;
  }
  .irbs-table-wrap tbody td.num {
    text-align: right; font-variant-numeric: tabular-nums;
  }

  /* ── Badges (irbs_table badge_col) ───────────────────────────────────── */
  /* Shape and typography only. Background and text colour are set per value
     by irbs_table() from badge_colors / irbs_badge_palette.                 */
  .irbs-badge {
    display: inline-block;
    padding: 2px 9px;
    border-radius: 999px;
    font-size: 10px;
    font-weight: 700;
    letter-spacing: .05em;
    text-transform: uppercase;
    white-space: nowrap;
  }

  /* ── Footer ──────────────────────────────────────────────────────────── */
  .irbs-footer {
    border-top: 1px solid var(--irbs-storm-light);
    padding: 10px 24px;
    background: #FAFAFA;
  }
  .irbs-footer-inner {
    display: flex;
    align-items: center;
    gap: 10px;
  }
  .irbs-footer-text {
    font-family: var(--irbs-font-accessible);
    font-size: 12px;
    color: var(--irbs-storm);   /* 4.6:1 on #FAFAFA; storm-mid was 2.7:1 */
    line-height: 1.4;
  }
  .irbs-footer-text a {
    color: var(--irbs-storm);
    text-decoration: none;
  }
  .irbs-footer-text a:hover { color: var(--irbs-blue); }

  /* ── Taller navbar to give the IRBS logo room to breathe ───────────── */
  .navbar { min-height: 68px; }

  /* ── Brand (logo + title): vertically centred with the nav tabs ────── */
  /* bslib renders .navbar-brand as an inline element inside a block
     .navbar-header, which lets the line box grow and pins the logo to the
     top of a taller navbar. Flex both so the brand centres on the tabs.   */
  .navbar .navbar-header {
    display: flex;
    align-items: center;
  }
  .navbar .navbar-brand {
    display: flex;
    align-items: center;
    padding-top: 0;
    padding-bottom: 0;
    margin-right: 2.5rem;   /* gap before the nav_panel tabs (default 1rem) */
  }

  /* ── Non-fillable panels push the footer down ───────────────────────── */
  /* When some panels are fillable (page_navbar(fillable = 'Dashboard')),
     the page is a fixed-height fill container. A non-fillable panel taller
     than the window would otherwise overflow *under* the footer. Stop the
     tab content from shrinking whenever the active panel is non-fillable.
     Harmless when fillable = FALSE.                                        */
  .html-fill-container > .tab-content:has(> .tab-pane.active:not(.html-fill-container)) {
    flex-shrink: 0;
  }

  /* ── App title (navbar brand text beside the IRBS logo) ──────────────── */
  .irbs-app-title {
    font-family: var(--irbs-font-display);
    font-weight: 700;
    font-size: 21.6px;   /* 15px × 1.2 × 1.2 */
    color: #fff;
    white-space: nowrap;
  }

"
))

# ── Footer widget ─────────────────────────────────────────────────────────────
# Drop into page_navbar(footer = irbs_footer) for a consistent app footer.

irbs_footer <- div(
  class = "irbs-footer",
  div(
    class = "irbs-footer-inner",
    tags$a(
      href = "https://illinois.edu/",
      target = "_blank",
      rel = "noopener noreferrer",
      title = "University of Illinois",
      tags$img(
        src = "block_i_color.png",
        height = "20px",
        alt = "University of Illinois",
        style = "opacity:0.55; flex-shrink:0; display:block;"
      )
    ),
    div(
      class = "irbs-footer-text",
      tags$a(
        href = "https://illinois-river-bio-station.inhs.illinois.edu/",
        target = "_blank",
        rel = "noopener noreferrer",
        "Illinois River Biological Station"
      ),
      " · ",
      tags$a(
        href = "https://inhs.illinois.edu/",
        target = "_blank",
        rel = "noopener noreferrer",
        "Illinois Natural History Survey"
      ),
      " · ",
      tags$a(
        href = "https://illinois.edu/",
        target = "_blank",
        rel = "noopener noreferrer",
        "University of Illinois at Urbana-Champaign"
      )
    )
  )
)
