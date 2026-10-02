# ============================================================================
# app.R -- FishTracks Real Time
# ============================================================================
# Shiny app over the Fish_Tracks_Real_Time SQL Server database, presented
# with the IRBS design system (bslib theme, fonts, navbar, footer, metric
# cards, ggplot2 theme). Branding helpers are copied from the IRBS Shiny
# template:
#   code/irbs_setup.R     -- irbs_theme, atkinson_link, irbs_css, irbs_footer
#   code/ggplot2_theme.R  -- irbs_colors, theme_irbs_minimal(), irbs_fonts()
#   code/metric_card.R    -- metric_card()
#   www/irbs_inverse.png  -- IRBS logo (reversed) for the navbar
#   www/block_i_color.png -- Block I for the footer
# Dependencies beyond shiny/tidyverse/DBI/odbc: bslib, bsicons, htmltools,
# shinyWidgets, showtext, ragg (optional).
# ============================================================================

library(shiny)
library(bslib)
library(htmltools)
library(shinyWidgets)
library(tidyverse)
library(DBI)
library(odbc)
library(lubridate)
library(here)

source(here('code', 'helper_functions.R'))
source(here('code', 'irbs_setup.R'))
source(here('code', 'ggplot2_theme.R'))
source(here('code', 'metric_card.R'))

# Brand fonts for ggplot2 via showtext (Montserrat, Source Sans 3, Atkinson
# Hyperlegible). Falls back to system fonts if showtext or Google Fonts are
# unavailable, e.g. on a server without outbound internet.
tryCatch(irbs_fonts(), error = function(e) {
  message('IRBS plot fonts unavailable, using system fonts: ', e$message)
})

# renderPlot() draws with the ragg device when the package is installed, which
# is faster than the default Windows device and renders text more cleanly.
options(shiny.useragg = TRUE)
if (!requireNamespace('ragg', quietly = TRUE)) {
  message(
    "Package 'ragg' is not installed; run install.packages('ragg') ",
    "for faster plot rendering."
  )
}

# schema setting helper
dbo <- function(name) {
  Id(schema = "dbo", table = name)
}

con <- dbConnect(odbc::odbc(), "Fish_Tracks_Real_Time")

all_spp <- tbl(con, dbo('tag')) %>%
  select(common_name_e) %>%
  collect() %>%
  unique() %>%
  pull() %>%
  tolower() %>%
  c(., 'unknown')

# Species picker groups. A species is assigned to the first group whose
# pattern it matches; anything unmatched is native. Groups appear in this
# order, sorted alphabetically within each. Empty groups (e.g. black carp
# before it shows up in the tag table) are dropped from the picker.
species_groups <- list(
  'Invasive' = 'bighead carp|silver carp|grass carp|black carp',
  'Unknown' = '^unknown$',
  'Non-native' = '^common carp$',
  'Native' = '.'
)

species_choices <- local({
  remaining <- sort(all_spp)
  out <- list()
  for (grp in names(species_groups)) {
    hit <- remaining[str_detect(remaining, species_groups[[grp]])]
    if (length(hit)) {
      out[[grp]] <- hit
    }
    remaining <- setdiff(remaining, hit)
  }
  out
})

# Fixed colour per species so a species keeps its colour whatever else is
# selected. Invasive carp are assigned BY NAME so silver and bighead are always
# orange and blue (the two most-watched species must be distinguishable even
# when they are the only two present); unknown tags are neutral grey, the
# non-native watch species is earth, and natives draw from the cool
# supporting colours in alphabetical order.
species_palette <- local({
  assign_group <- function(spp, cols) {
    if (is.null(spp)) {
      return(character(0))
    }
    set_names(rep_len(cols, length(spp)), spp)
  }
  # unname(): irbs_pal() returns a named vector, and c('x' = named) would
  # produce the name 'x.illini-orange', which then fails to match a species.
  invasive_named <- c(
    'silver carp' = unname(irbs_pal('illini-orange')), # primary series
    'bighead carp' = unname(irbs_pal('illini-blue')),
    'silver carp/bighead carp' = unname(irbs_pal('altgeld')),
    'grass carp' = unname(irbs_pal('harvest')),
    'black carp' = unname(irbs_pal('berry'))
  )
  c(
    invasive_named[names(invasive_named) %in% species_choices$Invasive],
    # any invasive not named above falls back to the remaining palette
    assign_group(
      setdiff(species_choices$Invasive, names(invasive_named)),
      irbs_pal('industrial', 'patina')
    ),
    assign_group(species_choices$Unknown, irbs_pal('storm-mid')),
    assign_group(species_choices$`Non-native`, irbs_pal('earth')),
    assign_group(
      species_choices$Native,
      irbs_pal('prairie', 'industrial', 'patina', 'arches')
    )
  )
})

# Vertical offset between species series on the plot. Counts are small
# integers, so several species sit on exactly the same y most of the time;
# nudging each species by a fixed fraction of one fish keeps coincident
# lines and points distinguishable when the axis is small (where overlap
# matters most) and never lets a count be misread as its neighbour.
# Offsets are centred on the true count; tooltips and cards use n_fish.
dodge_step <- 0.15
dodge_for <- function(y_max) dodge_step

tbl_station <- tbl(con, dbo('station')) %>%
  collect() %>%
  mutate(plot_order = factor(plot_order, levels = sort(plot_order)))

# Receiver picker choices, grouped by river: a named list of named vectors
# (display station_label, return station_id). Rivers are ordered by the
# plot_order of their first station and receivers by plot_order within each
# river, so the picker reads in the same order as the facets. Selecting a
# whole river is done with the group checkbox on its heading.
receiver_choices <- tbl_station %>%
  distinct(river_name, station_id, station_label, plot_order) %>%
  arrange(plot_order) %>%
  mutate(river_name = fct_inorder(river_name)) %>%
  group_by(river_name) %>%
  summarise(
    choices = list(set_names(station_id, station_label)),
    .groups = 'drop'
  ) %>%
  with(set_names(choices, as.character(river_name)))

# Default selection: every receiver except those on the Sandusky River.
receiver_default <- tbl_station %>%
  filter(river_name != 'Sandusky River') %>%
  pull(station_id)

# Sidebar inputs are virtual-select widgets (shinyWidgets::virtualSelectInput).
# Multi-selects get a search box, a "Select all" checkbox covering the whole
# list, and, for grouped choices, a checkbox on each group heading that
# selects or clears that group. dropboxWrapper = 'body' lets the dropdown
# escape the narrow sidebar instead of being clipped by it.
multi_select <- function(inputId, label, choices, selected = NULL) {
  virtualSelectInput(
    inputId = inputId,
    label = label,
    choices = choices,
    selected = selected,
    multiple = TRUE,
    search = TRUE,
    selectAllOnlyVisible = TRUE, # "Select all" respects the search filter
    optionsCount = 12,
    # Closed control always reads "N selected" (or "All (N)") rather than
    # listing every selected name, which otherwise stretches the sidebar.
    alwaysShowSelectedOptionsCount = TRUE,
    optionSelectedText = 'selected',
    optionsSelectedText = 'selected',
    allOptionsSelectedText = 'All',
    dropboxWrapper = 'body',
    width = '100%'
  )
}

single_select <- function(inputId, label, choices, selected) {
  virtualSelectInput(
    inputId = inputId,
    label = label,
    choices = choices,
    selected = selected,
    multiple = FALSE,
    search = FALSE,
    hideClearButton = TRUE,
    dropboxWrapper = 'body',
    width = '100%'
  )
}


# App footer: the shared IRBS footer plus a data-source tagline on a second
# line, aligned with the link text. The Block I is 20px high; at its 243x351
# aspect ratio that is ~14px wide, plus the footer's 10px flex gap = 24px.
fishtracks_footer <- tagAppendChild(
  irbs_footer,
  div(
    class = 'irbs-footer-inner',
    style = 'margin-top:4px; padding-left:24px;',
    div(
      class = 'irbs-footer-text',
      'Detection data from ',
      tags$a(
        href = 'https://cm.water.usgs.gov/data/Fish_Tracks_Real_Time/',
        target = '_blank',
        rel = 'noopener noreferrer',
        'USGS Fish Tracks Real-Time receiver array'
      ),
      " · ",
      'Tag and receiver data from ',
      tags$a(
        href = 'https://umesc-gisdb03.er.usgs.gov/raft/',
        target = '_blank',
        rel = 'noopener noreferrer',
        'USGS Riverine Acoustic Fish Telemetry Network (RAFT)'
      )
    )
  )
)

# ── UI ───────────────────────────────────────────────────────────────────────

ui <- page_navbar(
  # IRBS logo + app name in the navbar brand area (styled in irbs_setup.R).
  title = tags$div(
    style = 'display:flex; align-items:center; gap:12px;',
    tags$img(
      src = 'irbs_inverse.png',
      height = '44px',
      alt = 'Illinois River Biological Station',
      style = 'flex-shrink:0;'
    ),
    tags$span(class = 'irbs-app-title', 'FishTracks Real Time')
  ),

  theme = irbs_theme,
  # bslib >= 0.9: navbar colour and light/dark text live in navbar_options()
  navbar_options = navbar_options(bg = '#13294B', theme = 'dark'), # illini-blue
  padding = '20px',
  gap = '16px',
  fillable = FALSE, # panels grow with content; plot card has its own height

  header = tagList(
    atkinson_link,
    irbs_css,
    # Receiver-activity tiles. Colour comes from a data attribute set per
    # tile (see heat_tile() in server), so the scale is defined once here.
    tags$style(HTML(
      '
      /* grid, not flex: every tile gets the same width, so a short last
         row is not stretched to fill the line */
      .heat-strip {
        display:grid; gap:10px;
        grid-template-columns: repeat(auto-fill, minmax(150px, 1fr));
      }
      .heat-tile {
        border-radius: 10px; padding: 10px 12px;
        border: 1px solid var(--irbs-storm-light);
        font-family: var(--irbs-font-accessible);
      }
      /* always reserve two lines for the name, so the numbers below sit at
         the same height on every tile whether the name wraps or not */
      .heat-tile-name {
        font-size: 12px; font-weight: 700; line-height: 1.2; min-height: 2.4em;
      }
      .heat-tile-value {
        font-family: var(--irbs-font-display); font-size: 28px;
        font-weight: 700; line-height: 1.1; margin-top: 2px;
      }
      .heat-tile-unit { font-size: 12px; font-weight: 400; margin-left: 3px; }
      .heat-tile-pct {
        font-family: var(--irbs-font-display); font-size: 20px;
        font-weight: 700; line-height: 1.1; margin-top: 4px;
      }
      .heat-tile-sub { font-size: 11.5px; line-height: 1.25; margin-top: 4px; }
      .heat-key { font-weight: 700; }
      .heat-tile[data-level="none"] { background:#fff; color:var(--irbs-storm); border-style:dashed; }
      .heat-tile[data-level="zero"] { background:#F2F1F2; color:var(--irbs-storm); }
      .heat-tile[data-level="low"]  { background:#EEF2F8; color:var(--irbs-blue); }
      .heat-tile[data-level="warm"] { background:var(--irbs-harvest); color:var(--irbs-blue); border-color:var(--irbs-harvest); }
      .heat-tile[data-level="hot"]  { background:var(--irbs-altgeld); color:#fff; border-color:var(--irbs-altgeld); }
      .heat-note { font-family: var(--irbs-font-accessible); font-size: 12.5px; color: var(--irbs-storm); }
      .heat-note.top { margin-bottom: 10px; }
      .heat-note.bottom { margin-top: 10px; }
      .heat-controls {
        margin-top: 14px; padding-top: 12px;
        border-top: 1px solid var(--irbs-storm-light);
      }
      .heat-controls-title {
        font-family: var(--irbs-font-accessible); font-size: 11px;
        font-weight: 700; letter-spacing: .07em; text-transform: uppercase;
        color: var(--irbs-storm); margin-bottom: 6px;
      }
      .heat-controls-row { display:flex; flex-wrap:wrap; gap:16px; }
      /* !important: the widget writes width:100% inline on both wrappers */
      .heat-controls-row > .form-group,
      .heat-controls-row .vscomp-ele { width: fit-content !important; min-width: 11em; }
      .heat-controls-row > .form-group { margin-bottom: 0; }
    '
    )),
    # Point the navbar-brand anchor at the IRBS website (bslib renders the
    # brand as an <a>, so a nested link would be invalid HTML).
    tags$script(HTML(
      '
      document.addEventListener("DOMContentLoaded", function () {
        var brand = document.querySelector(".navbar-brand");
        if (brand) {
          brand.href   = "https://illinois-river-bio-station.inhs.illinois.edu/";
          brand.target = "_blank";
          brand.rel    = "noopener noreferrer";
        }
      });
    '
    ))
  ),

  # Collapsible filter sidebar on the left (bslib). Fixed width keeps the
  # inputs from growing with their content; the toggle in the top corner
  # collapses it to give the plot the full width.
  sidebar = sidebar(
    title = 'Filters',
    width = 300,
    open = 'open',
    multi_select(
      inputId = 'species',
      label = 'Species',
      choices = species_choices,
      selected = c(species_choices$Invasive, species_choices$Unknown)
    ),
    multi_select(
      inputId = 'receiver_name',
      label = 'Receivers',
      choices = receiver_choices,
      selected = receiver_default
    ),
    single_select(
      inputId = 'lookback',
      label = 'Lookback period',
      choices = c('1 day' = 1, '1 week' = 7, '1 month' = 30, 'Max' = 'Max'),
      selected = '1'
    ),
    single_select(
      inputId = 'timeagg',
      label = 'Aggregate detections',
      choices = c('5 minutes', '1 hour', '1 day'),
      selected = '1 hour'
    ),
    # Overlay the all-species total; species series fade behind it.
    input_switch('show_total', 'Show all-species total', value = FALSE)
  ),

  nav_panel(
    'Detections',
    icon = bsicons::bs_icon('broadcast'),

    # Summary cards for the current selection and window. Three columns of
    # 3/12 each; the remaining quarter of the row is left empty on purpose
    # so the cards stay compact instead of stretching across the page.
    layout_columns(
      col_widths = c(3, 3, 3),
      fill = FALSE,
      gap = '16px',
      uiOutput('card_hottest'),
      uiOutput('card_receivers'),
      uiOutput('card_updated')
    ),

    # One tile per selected receiver, in river order, coloured by how the
    # current window compares with the receiver's own baseline. Collapsible,
    # so the plot can be brought up the page once the tiles have been read.
    accordion(
      id = 'heat_accordion',
      open = TRUE,
      accordion_panel(
        'Receiver activity',
        icon = bsicons::bs_icon('fire'),
        uiOutput('heat_tiles'),
        # The rating's two dials live with the card they tune, not in the
        # sidebar: "now" is the window counted, the baseline is what it is
        # ranked against. Both are hours; applied in R to a cached hourly
        # summary, so changing them never touches the database.
        div(
          class = 'heat-controls',
          div(class = 'heat-controls-title', 'Adjust the rating'),
          div(
            class = 'heat-controls-row',
            single_select(
              inputId = 'heat_recent',
              label = 'Count fish seen in the',
              choices = c(
                'Last hour' = 1,
                'Last 6 hours' = 6,
                'Last 24 hours' = 24
              ),
              selected = '6'
            ),
            single_select(
              inputId = 'heat_baseline',
              label = 'Compared with each receiver\u2019s',
              choices = c(
                'Last 24 hours' = 24,
                'Last 7 days' = 168,
                'Last 60 days' = 1440
              ),
              selected = '1440'
            )
          )
        )
      )
    ),

    card(
      full_screen = TRUE,
      height = '860px',
      card_header('Unique tagged fish detected'),
      card_body(
        style = 'overflow:hidden;',
        # Fixed y-axes make receivers comparable; free lets a quiet receiver
        # use its own scale when one busy site would otherwise flatten it.
        input_switch(
          'free_y',
          'Independent y-axis per receiver',
          value = FALSE
        ),
        # position:relative so the tooltip can be placed over the plot image
        div(
          style = 'position:relative;',
          plotOutput(
            'plot',
            height = 800,
            hover = hoverOpts(
              id = 'plot_hover',
              delay = 100,
              delayType = 'debounce',
              nullOutside = TRUE
            ),
            click = clickOpts(id = 'plot_click')
          ),
          uiOutput('plot_tooltip'), # transient, follows the cursor
          uiOutput('plot_pinned'), # pinned by click; has the copy button
          # Copy runs entirely in the browser inside the click gesture, which
          # the Clipboard API requires. The tag list travels in a data-
          # attribute so no server round trip sits between click and copy.
          tags$script(HTML(
            "
            $(document).on('click', '.tag-copy-btn', function() {
              var btn = this;
              navigator.clipboard.writeText(btn.dataset.tags).then(function() {
                var label = btn.textContent;
                btn.textContent = 'Copied';
                setTimeout(function() { btn.textContent = label; }, 1500);
              });
            });
            $(document).on('click', '.tag-pin-close', function() {
              Shiny.setInputValue('pin_close', Date.now());
            });
          "
          ))
        )
      )
    )
  ),

  footer = fishtracks_footer
)

# Timestamp of the last append job run that actually inserted rows. Defined
# at app level (outside server) with session = NULL so there is ONE poll per
# R process shared by every session: one small DBI query every 20 s no matter
# how many users, and all sessions see a data update in the same flush, so
# they compute the same cache keys and share the same cached results.
# Downstream reactives are only invalidated when the value changes.
last_data_update <- reactivePoll(
  intervalMillis = 20 * 1000,
  session = NULL,
  checkFunc = function() {
    dbGetQuery(
      con,
      "SELECT MAX(run_time) AS run_time FROM dbo.insert_log WHERE rows_inserted > 0"
    )$run_time
  },
  valueFunc = function() {
    dbGetQuery(
      con,
      "SELECT MAX(run_time) AS run_time FROM dbo.insert_log WHERE rows_inserted > 0"
    )$run_time
  }
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  # Receiver selection, debounced so a burst of clicks (e.g. toggling a whole
  # river via its group checkbox, then a couple of individual receivers) runs
  # one query rather than one per click.
  receivers_settled <- reactive(input$receiver_name) %>% debounce(750)

  time_threshold <- reactive({
    req(input$lookback)

    if (input$lookback == 'Max') {
      tbl(con, dbo('event')) %>%
        summarize(oldest_record = min(TimeStamp, na.rm = TRUE)) %>%
        pull(oldest_record)
    } else {
      as_datetime(now() - days(input$lookback))
    }
  })

  data_timefilter <- reactive({
    req(time_threshold())

    time_threshold_nonr <- time_threshold()

    tbl(con, dbo('event_animal')) %>%
      filter(TimeStamp >= time_threshold_nonr)
  })

  data_receiverfilter <- reactive({
    req(receivers_settled())
    selected_receivers_nonr <- receivers_settled()

    data_timefilter() %>%
      filter(
        station_id %in% selected_receivers_nonr
      )
  })

  # Species is NOT filtered here: the query aggregates every species so that
  # toggling species in the UI is a local filter on the collected summary
  # rows rather than a new round trip to SQL Server.
  data_sppjoin <- reactive({
    req(data_receiverfilter())

    data_receiverfilter() %>%
      left_join(tbl(con, dbo('tag')), by = c('TagID', 'animal_id')) %>%
      mutate(common_name_e = coalesce(common_name_e, 'unknown'))
  })

  # One row per station x species x bin, for all species. Cached on the four
  # inputs that shape the query, so any species combination is served from
  # the same collected result.
  data_timeagg <- reactive({
    req(data_sppjoin())
    req(input$timeagg)

    data_sppjoin() %>%
      aggregate_detections_lazy(unit = input$timeagg) %>%
      collect() %>%
      # odbc returns SQL Server counts as bit64::integer64. That class does
      # its own arithmetic and truncates doubles added to it (n + 0.15 == n),
      # which silently defeats the plot's species dodge; plain integers from
      # here on.
      mutate(across(c(n_fish, n_detections), as.integer))
  }) %>%
    bindCache(
      time_threshold(),
      receivers_settled(),
      input$timeagg,
      last_data_update()
    )

  # Bins in which each selected receiver logged at least one record, with or
  # without detections. event_animal only carries records that contain tags,
  # so this is what lets the plot tell "no fish" (record, zero tags) apart
  # from "no data" (logger or feed outage).
  data_records <- reactive({
    req(time_threshold(), receivers_settled(), input$timeagg)

    time_threshold_nonr <- time_threshold()
    selected_receivers_nonr <- receivers_settled()
    bin_expr <- bin_timestamp_sql(input$timeagg)

    tbl(con, dbo('event')) %>%
      filter(
        TimeStamp >= time_threshold_nonr,
        station_id %in% selected_receivers_nonr
      ) %>%
      mutate(TimeStamp_binned = sql(bin_expr)) %>%
      distinct(station_id, TimeStamp_binned) %>%
      collect()
  })

  # Intervals with no logger records, per selected receiver: a leading gap
  # (window start to first record), internal gaps (more than one bin between
  # consecutive records) and a trailing gap (last record to now, i.e. the
  # receiver is down right now). A receiver with no records at all in the
  # window gets one band across the whole panel. Drawn as pale bands under
  # the lines; derived from data_records(), so no extra query.
  data_gaps <- reactive({
    bin_s <- bin_width_seconds(input$timeagg)
    win_start <- time_threshold()
    # End the window at the newest record anywhere on the array, not now():
    # every station lags the feed by an hour or so, and only a receiver that
    # is behind the OTHERS should be flagged as down at the right edge.
    # No + bin_s: that gives one bin of tolerance, so a station is only
    # flagged at the right edge when it is two or more bins behind the
    # freshest station, not merely an hour behind in the normal USGS lag.
    win_end <- max(data_records()$TimeStamp_binned)

    station_labels_ordered <- tbl_station %>%
      filter(station_id %in% receivers_settled()) %>%
      distinct(station_label, plot_order) %>%
      arrange(plot_order) %>%
      pull(station_label)

    recorded <- data_records() %>%
      group_by(station_id) %>%
      summarise(bins = list(sort(TimeStamp_binned)), .groups = 'drop')

    tibble(station_id = receivers_settled()) %>%
      left_join(recorded, by = 'station_id') %>%
      mutate(
        bins = map(bins, ~ .x %||% as.POSIXct(character(0), tz = 'UTC')),
        # gap i runs from the end of recorded bin i-1 (or window start)
        # to the start of recorded bin i (or window end)
        xmin = map(bins, ~ c(win_start, .x + bin_s)),
        xmax = map(bins, ~ c(.x, win_end))
      ) %>%
      select(-bins) %>%
      unnest(c(xmin, xmax)) %>%
      filter(as.numeric(xmax) - as.numeric(xmin) > bin_s * 0.5) %>%
      left_join(tbl_station, by = 'station_id') %>%
      mutate(
        station_label = factor(station_label, levels = station_labels_ordered)
      )
  })

  data_timeseries_plot <- reactive({
    station_labels_ordered <- tbl_station %>%
      filter(station_id %in% receivers_settled()) %>%
      distinct(station_label, plot_order) %>%
      arrange(plot_order) %>%
      pull(station_label)

    req(input$species)

    fish <- data_timeagg() %>%
      filter(common_name_e %in% input$species)
    # palette order, so a species keeps its dodge slot whatever else is shown
    species_present <- intersect(names(species_palette), fish$common_name_e)
    # one offset per species, centred on zero: c(-0.15, 0, 0.15) for three
    dodge <- dodge_for(max(fish$n_fish, 1))
    species_offset <- set_names(
      dodge * (seq_along(species_present) - (length(species_present) + 1) / 2),
      species_present
    )
    bin_s <- bin_width_seconds(input$timeagg)
    gap_s <- bin_s * 1.5

    # Every recorded bin x every species seen in the window. Bins with a
    # record but no fish of that species become explicit zeros; bins with no
    # record are absent, and `segment` increments across them so geom_step
    # breaks instead of bridging the outage.
    rows <- data_records() %>%
      tidyr::crossing(common_name_e = species_present) %>%
      left_join(
        fish,
        by = c('station_id', 'common_name_e', 'TimeStamp_binned')
      ) %>%
      mutate(
        n_fish = coalesce(n_fish, 0L),
        n_detections = coalesce(n_detections, 0L)
      ) %>%
      arrange(station_id, common_name_e, TimeStamp_binned) %>%
      group_by(station_id, common_name_e) %>%
      mutate(
        segment = cumsum(
          c(TRUE, diff(as.numeric(TimeStamp_binned)) > gap_s)
        )
      ) %>%
      ungroup() %>%
      left_join(tbl_station, by = 'station_id') %>%
      mutate(
        station_label = factor(station_label, levels = station_labels_ordered),
        # drawn y: true count plus the species' dodge offset
        y_plot = n_fish + unname(species_offset[common_name_e]),
        dodge_step = dodge # carried along for the plot's y floor
      )

    # geom_step holds each value until the NEXT point, so the last bin of a
    # segment would otherwise end in a bare point with no run. Add a terminal
    # row one bin later carrying the same values: it completes the step and
    # is excluded from points and tooltips (is_end).
    ends <- rows %>%
      group_by(station_id, common_name_e, segment) %>%
      slice_max(TimeStamp_binned, n = 1, with_ties = FALSE) %>%
      ungroup() %>%
      mutate(TimeStamp_binned = TimeStamp_binned + bin_s, is_end = TRUE)

    bind_rows(mutate(rows, is_end = FALSE), ends)
  })

  # All-species total per station x bin. A fish has one species, so summing
  # the per-species unique counts gives the number of distinct fish; the tag
  # lists concatenate the same way. Drawn undodged, at the true count.
  data_total_plot <- reactive({
    data_timeseries_plot() %>%
      group_by(station_id, station_label, TimeStamp_binned, segment, is_end) %>%
      summarise(
        n_fish = sum(n_fish),
        n_detections = sum(n_detections),
        tag_ids = paste(na.omit(tag_ids), collapse = ', '),
        .groups = 'drop'
      ) %>%
      mutate(common_name_e = 'All species', y_plot = n_fish)
  })

  # Points the tooltip can land on: species points, plus the total's when
  # it is shown (listed first, so it wins a tie with a dodged species point).
  hover_points <- reactive({
    pts <- data_timeseries_plot() %>% filter(n_fish > 0, !is_end)
    if (isTRUE(input$show_total)) {
      pts <- bind_rows(data_total_plot() %>% filter(n_fish > 0, !is_end), pts)
    }
    pts
  })

  # ── Receiver activity ("heat") ─────────────────────────────────────────────
  # Hourly summaries for EVERY station and species over the longest baseline
  # (60 days) plus the longest window (24 h): one query, cached on the data
  # update time only, so it runs once per append and is shared by every
  # session. Everything the user can change is applied in R afterwards.
  # `coverage` is the set of hours with any logger record (tags or not); it
  # tells receiver_heat() which baseline windows are trustworthy.
  data_heat_hourly <- reactive({
    thr <- as_datetime(now() - days(62))
    bin_expr <- bin_timestamp_sql('1 hour')

    hourly <- tbl(con, dbo('event_animal')) %>%
      filter(TimeStamp >= thr) %>%
      left_join(tbl(con, dbo('tag')), by = c('TagID', 'animal_id')) %>%
      mutate(common_name_e = coalesce(common_name_e, 'unknown')) %>%
      aggregate_detections_lazy(unit = '1 hour') %>%
      select(-n_detections) %>%
      collect()

    coverage <- tbl(con, dbo('event')) %>%
      filter(TimeStamp >= thr) %>%
      mutate(TimeStamp_binned = sql(bin_expr)) %>%
      distinct(station_id, TimeStamp_binned) %>%
      collect()

    list(hourly = hourly, coverage = coverage)
  }) %>%
    bindCache(last_data_update())

  # Per selected receiver: fish in the current window, percentile against
  # the baseline, and how much of the current window the logger covered.
  data_heat <- reactive({
    req(
      input$species,
      receivers_settled(),
      input$heat_recent,
      input$heat_baseline
    )
    h <- data_heat_hourly()
    req(nrow(h$coverage) > 0)

    receiver_heat(
      hourly = h$hourly,
      coverage = h$coverage,
      stations = receivers_settled(),
      species = input$species,
      recent_h = as.numeric(input$heat_recent),
      baseline_h = as.numeric(input$heat_baseline)
    ) %>%
      left_join(tbl_station, by = 'station_id') %>%
      arrange(plot_order) %>%
      mutate(
        level = case_when(
          is.na(pctile) ~ 'none',
          n_now == 0 ~ 'zero',
          pctile < 0.5 ~ 'low',
          pctile < 0.9 ~ 'warm',
          TRUE ~ 'hot'
        )
      )
  })

  heat_labels <- reactive(list(
    recent = c('1' = 'last hour', '6' = 'last 6 h', '24' = 'last 24 h')[[
      input$heat_recent
    ]],
    baseline = c('24' = 'last 24 h', '168' = 'last 7 d', '1440' = 'last 60 d')[[
      input$heat_baseline
    ]]
  ))

  # Percentile as text ("92%"), or why there is none
  heat_pct <- function(r) {
    if (is.na(r$pctile)) NA_character_ else paste0(round(100 * r$pctile), '%')
  }

  heat_tile <- function(r, labels) {
    pct <- heat_pct(r)
    div(
      class = 'heat-tile',
      `data-level` = r$level,
      title = if (is.na(pct)) {
        'No baseline window with enough logger coverage'
      } else {
        sprintf(
          'More fish than %s of the %d %s windows in the %s (median %s fish)',
          pct,
          r$n_windows,
          labels$recent,
          labels$baseline,
          format(r$base_median)
        )
      },
      div(class = 'heat-tile-name', as.character(r$station_label)),
      div(
        class = 'heat-tile-value',
        format(r$n_now, big.mark = ','),
        span(class = 'heat-tile-unit', 'fish')
      ),
      if (is.na(pct)) {
        div(class = 'heat-tile-sub', 'no usable baseline')
      } else {
        div(class = 'heat-tile-pct', pct)
      },
      if (!is.na(pct) && r$coverage_now < 0.8) {
        div(class = 'heat-tile-sub', 'partial data in window')
      }
    )
  }

  # Legend entry in the colour of its tile (with a swatch, since gold text
  # alone is hard to read on white).
  heat_key <- function(level, colour, text) {
    tagList(
      span(
        class = 'heat-tile',
        `data-level` = level,
        style = paste(
          'display:inline-block; width:10px; height:10px; padding:0;',
          'border-radius:3px; margin-right:4px; vertical-align:-1px;'
        )
      ),
      span(class = 'heat-key', style = paste0('color:', colour, ';'), text)
    )
  }

  output$heat_tiles <- renderUI({
    h <- data_heat()
    labels <- heat_labels()
    tagList(
      # one string per sentence: htmltools puts whitespace between separate
      # children, which would float the punctuation ("6 h , and")
      div(
        class = 'heat-note top',
        paste0(
          'Unique tagged fish of the selected species in the ',
          labels$recent,
          ', and the share of windows of the same length in the ',
          labels$baseline,
          ' at that receiver with fewer fish.'
        )
      ),
      div(
        class = 'heat-strip',
        map(seq_len(nrow(h)), ~ heat_tile(h[.x, ], labels))
      ),
      div(
        class = 'heat-note bottom',
        heat_key('zero', 'var(--irbs-storm)', 'Grey:'),
        'none now \u00b7',
        heat_key('low', 'var(--irbs-blue)', 'Blue:'),
        'below the receiver\u2019s median \u00b7',
        heat_key('warm', '#C98A00', 'Gold:'),
        'above median \u00b7',
        heat_key('hot', 'var(--irbs-altgeld)', 'Red:'),
        '90th percentile or higher'
      )
    )
  })

  # ── Summary cards ──────────────────────────────────────────────────────────
  # All three read data the plot or tiles already need, so they add no queries.

  # Icon + text for a metric card label (metric_card() accepts tags in label).
  card_label <- function(icon, text) {
    tagList(
      tags$span(style = 'margin-right:6px;', bsicons::bs_icon(icon)),
      text
    )
  }

  output$card_hottest <- renderUI({
    h <- data_heat() %>% filter(!is.na(pctile))
    labels <- heat_labels()
    if (nrow(h) == 0) {
      return(metric_card(
        card_label('fire', 'Hottest receiver'),
        '\u2014',
        sub = 'no receiver has a usable baseline',
        accent = 'blue'
      ))
    }
    top <- h %>% arrange(desc(pctile), desc(n_now)) %>% slice(1)
    metric_card(
      card_label('fire', 'Hottest receiver'),
      # station labels are long for the 36px metric size
      tags$span(style = 'font-size:26px;', as.character(top$station_label)),
      sub = paste0(
        format(top$n_now, big.mark = ','),
        ' fish in the ',
        labels$recent,
        ' \u00b7 ',
        heat_pct(top),
        ' of the ',
        labels$baseline
      ),
      accent = switch(top$level, hot = 'orange', warm = 'harvest', 'blue')
    )
  })

  output$card_receivers <- renderUI({
    reporting <- unique(data_records()$station_id)
    selected <- receivers_settled()
    n_quiet <- length(setdiff(selected, reporting))
    metric_card(
      card_label('reception-4', 'Receivers reporting'),
      paste0(length(reporting), ' / ', length(selected)),
      sub = if (n_quiet == 0) {
        'all selected receivers logged records in this window'
      } else {
        paste0(n_quiet, ' with no records in this window')
      },
      accent = if (n_quiet == 0) 'prairie' else 'harvest'
    )
  })

  output$card_updated <- renderUI({
    updated <- last_data_update()
    metric_card(
      card_label('clock-history', 'Data updated'),
      if (is.null(updated) || is.na(updated)) {
        '\u2014'
      } else {
        format(with_tz(updated, 'America/Chicago'), '%H:%M')
      },
      unit = 'CT',
      sub = if (is.null(updated) || is.na(updated)) {
        'no completed append run found'
      } else {
        format(with_tz(updated, 'America/Chicago'), '%b %d, %Y')
      },
      accent = 'blue'
    )
  })

  # ── Tooltips: tag IDs behind a point ───────────────────────────────────────
  # Hover shows a transient box that follows the cursor (up to 8 tags).
  # Click pins a box in place with the full list and a copy-to-clipboard
  # button; click elsewhere on the plot, or its close control, to dismiss.
  # nearPoints() matches in pixel space within the facet panel under the
  # cursor. The plot stays a cached PNG; only these small UIs re-render.

  nearest_point <- function(coords) {
    nearPoints(
      hover_points(),
      coords,
      xvar = 'TimeStamp_binned',
      yvar = 'y_plot', # where the point is drawn, not the true count
      threshold = 12,
      maxpoints = 1
    )
  }

  # Build the box. `pinned` switches on pointer events, the full tag list,
  # and the copy / close controls.
  tag_box <- function(pt, left_px, top_px, pinned = FALSE) {
    plot_w <- session$clientData$output_plot_width
    box_w <- 260 + 12
    flip <- !is.null(plot_w) && (left_px + box_w > plot_w)
    style <- sprintf(
      paste(
        'position:absolute; z-index:%d; pointer-events:%s;',
        '%s:%dpx; top:%dpx;',
        'background:rgba(255,255,255,.97);',
        'border:1px solid var(--irbs-%s); border-radius:6px;',
        'box-shadow:0 1px 3px rgba(19,41,75,.15); padding:8px 10px;',
        'font-family:var(--irbs-font-accessible); font-size:12px;',
        'max-width:260px;'
      ),
      if (pinned) 101L else 100L,
      if (pinned) 'auto' else 'none',
      if (flip) 'right' else 'left',
      if (flip) as.integer(plot_w - left_px + 12) else as.integer(left_px + 12),
      as.integer(top_px + 12),
      if (pinned) 'blue' else 'storm-light'
    )

    tag_list <- strsplit(coalesce(pt$tag_ids, ''), ', ', fixed = TRUE)[[1]]
    shown <- if (pinned) tag_list else head(tag_list, 8)
    n_more <- length(tag_list) - length(shown)

    div(
      style = style,
      tags$div(
        style = 'display:flex; justify-content:space-between; gap:12px;',
        tags$span(
          style = 'font-weight:700; color:var(--irbs-blue);',
          as.character(pt$station_label)
        ),
        if (pinned) {
          tags$a(
            href = '#',
            class = 'tag-pin-close',
            style = 'color:var(--irbs-storm); text-decoration:none;',
            title = 'Close',
            onclick = 'return false;',
            HTML('&times;')
          )
        }
      ),
      tags$div(
        style = 'color:var(--irbs-storm);',
        format(pt$TimeStamp_binned, '%b %d %H:%M', tz = 'America/Chicago'),
        ' \u00b7 ',
        pt$common_name_e
      ),
      tags$div(sprintf(
        '%s fish, %s detections',
        format(pt$n_fish, big.mark = ','),
        format(pt$n_detections, big.mark = ',')
      )),
      tags$div(
        style = paste(
          'margin-top:4px; font-variant-numeric:tabular-nums;',
          if (pinned) 'max-height:160px; overflow-y:auto;'
        ),
        paste(shown, collapse = ', '),
        if (n_more > 0) {
          tags$span(
            style = 'color:var(--irbs-storm);',
            sprintf(' +%d more', n_more)
          )
        }
      ),
      if (pinned) {
        tags$button(
          type = 'button',
          class = 'btn btn-sm btn-outline-secondary tag-copy-btn',
          style = 'margin-top:8px; width:100%;',
          `data-tags` = paste(tag_list, collapse = ', '),
          sprintf('Copy %d tag IDs', length(tag_list))
        )
      }
    )
  }

  # Transient hover box (suppressed while a box is pinned).
  output$plot_tooltip <- renderUI({
    req(is.null(pinned()))
    hover <- input$plot_hover
    req(hover)
    pt <- nearest_point(hover)
    req(nrow(pt) == 1)
    tag_box(pt, hover$coords_css$x, hover$coords_css$y, pinned = FALSE)
  })

  # Pinned box: set by a click on a point, cleared by a click on empty plot,
  # the close control, or any change to the plotted data.
  pinned <- reactiveVal(NULL)

  observeEvent(input$plot_click, {
    click <- input$plot_click
    pt <- nearest_point(click)
    if (nrow(pt) == 1) {
      pinned(list(pt = pt, x = click$coords_css$x, y = click$coords_css$y))
    } else {
      pinned(NULL)
    }
  })
  observeEvent(input$pin_close, pinned(NULL))
  observeEvent(data_timeseries_plot(), pinned(NULL))

  output$plot_pinned <- renderUI({
    p <- pinned()
    req(p)
    tag_box(p$pt, p$x, p$y, pinned = TRUE)
  })

  # ── Plot ───────────────────────────────────────────────────────────────────

  output$plot <- renderPlot({
      req(data_timeseries_plot())

      d <- data_timeseries_plot()
      show_total <- isTRUE(input$show_total)
      # species series step back when the total is overlaid
      spp_alpha <- if (show_total) 0.3 else 0.8
      # room below zero for the lowest dodged series
      y_floor <- -max(d$dodge_step, 0) *
        (n_distinct(d$common_name_e) - 1) /
        2 -
        0.05
      # All-species total drawn on top, full opacity, in black (NULL adds
      # nothing to the plot when the switch is off)
      total_layers <- if (show_total) {
        list(
          geom_step(
            data = data_total_plot(),
            aes(group = segment, color = 'All species'),
            direction = 'hv',
            linewidth = 0.9
          ),
          geom_point(
            data = data_total_plot() %>% filter(n_fish > 0, !is_end),
            aes(color = 'All species'),
            pch = 16,
            size = 2.2
          )
        )
      }

      d %>%
        ggplot(aes(
          x = TimeStamp_binned,
          y = y_plot,
          color = common_name_e
        )) +
        # Pale bands where the logger recorded nothing (outages), drawn first
        # so the data sit on top.
        geom_rect(
          data = data_gaps(),
          aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
          inherit.aes = FALSE,
          fill = '#C8C6C7', # storm-light
          alpha = 0.35
        ) +
        # Steps: each bin's count holds until the next bin, which is what a
        # binned count means, and coincident series differ by where they
        # step. Breaks between segments (logger outages); zeros are drawn
        # as line only, so the baseline isn't a wall of points.
        geom_step(
          aes(group = interaction(common_name_e, segment)),
          direction = 'hv',
          alpha = spp_alpha
        ) +
        geom_point(
          data = ~ filter(.x, n_fish > 0, !is_end),
          pch = 16,
          alpha = spp_alpha
        ) +
        total_layers +
        facet_wrap(
          ~station_label,
          scales = if (isTRUE(input$free_y)) 'free_y' else 'fixed',
          drop = FALSE
        ) +
        labs(
          x = 'Time (Central)',
          y = paste0('Unique tagged fish (per ', input$timeagg, ')'),
          color = 'Species',
          caption = paste(
            'Grey bands: no logger records.',
            'Species series are offset vertically for legibility.'
          )
        ) +
        scale_color_manual(
          values = c(species_palette, 'All species' = '#000000')
        ) +
        theme_irbs_minimal(base_size = 15) +
        theme(
          legend.position = 'top',
          panel.spacing.x = unit(1.5, 'lines') # keep edge tick labels apart
        ) +
        scale_y_continuous(
          limits = c(y_floor, NA),
          # integer breaks from 0, ignoring the sub-zero dodge room
          breaks = function(lim) scales::breaks_pretty()(c(0, lim[2])),
          expand = expansion(mult = c(0, 0.15)),
          labels = scales::label_number(accuracy = 1)
        ) +
        # Break positions and labels adapt to the window: breaks_pretty() picks
        # ~8 evenly spaced ticks for any span, and label_date_short() prints
        # only the parts of the date that changed since the previous tick
        # (e.g. "Sep 22 / 23 / 24", or "Sep / Oct / 2026" across a year).
        scale_x_datetime(
          limits = c(time_threshold(), now()),
          # keep (and clip) points outside the limits rather than dropping
          # them: the first bin floors to before the window start and the
          # terminal step row sits one bin past the last record
          oob = scales::oob_keep,
          expand = c(0.02, 0.02),
          breaks = scales::breaks_pretty(n = 5), # ~6 h ticks at 1 day; panels are narrow
          labels = scales::label_date_short(),
          timezone = 'America/Chicago', # data are UTC; render in Central
          guide = guide_axis(check.overlap = TRUE)
        )
  }) %>%
    # Cache rendered plots (app-wide, so shared across sessions on Connect).
    # The key covers every input that shapes the plot, plus the time of the
    # last data update so the cache is invalidated exactly when new rows land.
    bindCache(
      time_threshold(),
      receivers_settled(),
      input$species,
      input$timeagg,
      input$lookback,
      input$show_total,
      input$free_y,
      last_data_update()
    )
}
# Run the application
shinyApp(ui = ui, server = server)
