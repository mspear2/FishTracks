library(shiny)
library(shinyWidgets)
library(tidyverse)
library(DBI)
library(odbc)
library(lubridate)
library(here)

source(here('code', 'helper_functions.R'))

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
  'Invasive'   = 'bighead carp|silver carp|grass carp|black carp',
  'Unknown'    = '^unknown$',
  'Non-native' = '^common carp$',
  'Native'     = '.'
)

species_choices <- local({
  remaining <- sort(all_spp)
  out <- list()
  for (grp in names(species_groups)) {
    hit <- remaining[str_detect(remaining, species_groups[[grp]])]
    if (length(hit)) out[[grp]] <- hit
    remaining <- setdiff(remaining, hit)
  }
  out
})

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


# Define UI for application that draws a histogram
ui <- fluidPage(
  # Application title
  titlePanel("FishTracks Real Time Prototype"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      width = 2,
      multi_select(
        inputId = 'species',
        label = 'Select spp.',
        choices = species_choices,
        selected = c(species_choices$Invasive, species_choices$Unknown)
      ),
      multi_select(
        inputId = 'receiver_name',
        label = 'Select receiver(s)',
        choices = receiver_choices,
        selected = receiver_default
      ),
      single_select(
        inputId = 'lookback',
        label = 'Select lookback period',
        choices = c('1 day' = 1, '1 week' = 7, '1 month' = 30, 'Max' = 'Max'),
        selected = '1'
      ),
      single_select(
        inputId = 'timeagg',
        label = 'Aggregate detections',
        choices = c('5 minutes', '1 hour', '1 day'),
        selected = '1 hour'
      )
    ),

    # Show a plot
    mainPanel(width = 10, plotOutput("plot", height = 800))
  )
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
      collect()
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
    species_present <- unique(fish$common_name_e)
    gap_s <- bin_width_seconds(input$timeagg) * 1.5

    # Every recorded bin x every species seen in the window. Bins with a
    # record but no fish of that species become explicit zeros; bins with no
    # record are absent, and `segment` increments across them so geom_line
    # breaks instead of bridging the outage.
    data_records() %>%
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
        station_label = factor(station_label, levels = station_labels_ordered)
      )
  })

  output$plot <- renderPlot({
    req(data_timeseries_plot())

    data_timeseries_plot() %>%
      ggplot(aes(
        x = TimeStamp_binned,
        y = n_fish,
        color = common_name_e,
        alpha = .7
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
      # Lines break between segments (logger outages); zeros are drawn as
      # line only, so the baseline isn't a wall of points.
      geom_line(aes(group = interaction(common_name_e, segment))) +
      geom_point(data = ~ filter(.x, n_fish > 0), pch = 16) +
      facet_wrap(~station_label, scales = 'fixed', drop = FALSE) +
      labs(
        x = 'Time (Central)',
        y = paste0('Unique tagged fish (per ', input$timeagg, ')'),
        color = 'Species',
        caption = 'Grey bands: no logger records'
      ) +
      theme_minimal(base_size = 15) +
      theme(panel.spacing.x = unit(1.5, 'lines')) + # keep edge tick labels apart
      scale_y_continuous(
        limits = c(0, NA),
        expand = expansion(mult = c(0, 0.15)),
        labels = scales::label_number(accuracy = 1)
      ) +
      # Break positions and labels adapt to the window: breaks_pretty() picks
      # ~8 evenly spaced ticks for any span, and label_date_short() prints
      # only the parts of the date that changed since the previous tick
      # (e.g. "Sep 22 / 23 / 24", or "Sep / Oct / 2026" across a year).
      scale_x_datetime(
        limits = c(time_threshold(), now()),
        expand = c(0.02, 0.02),
        breaks = scales::breaks_pretty(n = 5), # ~6 h ticks at 1 day; panels are narrow
        labels = scales::label_date_short(),
        timezone = 'America/Chicago', # data are UTC; render in Central
        guide = guide_axis(check.overlap = TRUE)
      ) +
      scale_alpha_identity()
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
      last_data_update()
    )
}
# Run the application
shinyApp(ui = ui, server = server)
