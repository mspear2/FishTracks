library(tidyverse)
library(DBI)
library(odbc)
library(lubridate)
library(here)

source(here('code', 'ggplot2_theme.R'))

con <- dbConnect(odbc::odbc(), "Fish_Tracks_Real_Time")

station <- '05538010'
since <- Sys.time() - days(21)

volts <- dbGetQuery(
  con,
  "SELECT TimeStamp, VRLineVolt, VRTagCount
     FROM dbo.event
    WHERE station_id = ? AND TimeStamp >= ?
    ORDER BY TimeStamp",
  params = list(station, since)
) %>%
  as_tibble() %>%
  mutate(TimeStamp = with_tz(force_tz(TimeStamp, 'UTC'), 'America/Chicago'))

# Logger outages: jumps between consecutive records longer than one poll
outages <- volts %>%
  mutate(
    prev = lag(TimeStamp),
    gap_min = as.numeric(TimeStamp - prev, units = 'mins')
  ) %>%
  filter(gap_min > 5) %>%
  transmute(gap_start = prev, gap_end = TimeStamp, gap_min)

# Receiver silent: logger records present but no voltage reported.
# Collapse consecutive NULL-voltage records into intervals.
silent <- volts %>%
  mutate(
    silent = is.na(VRLineVolt),
    run = cumsum(silent != lag(silent, default = FALSE))
  ) %>%
  filter(silent) %>%
  group_by(run) %>%
  summarise(
    start = min(TimeStamp),
    end = max(TimeStamp) + minutes(5),
    .groups = 'drop'
  )

# Segment id so geom_line breaks at every outage instead of bridging it
plot_df <- volts %>%
  filter(!is.na(VRLineVolt)) %>%
  mutate(
    gap_min = as.numeric(TimeStamp - lag(TimeStamp), units = 'mins'),
    segment = cumsum(coalesce(gap_min > 5, TRUE))
  )

ggplot(plot_df, aes(TimeStamp, VRLineVolt)) +
  geom_rect(
    data = outages,
    aes(xmin = gap_start, xmax = gap_end, ymin = -Inf, ymax = Inf),
    inherit.aes = FALSE,
    fill = '#C8C6C7',
    alpha = 0.35
  ) +
  geom_rect(
    data = silent,
    aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf),
    inherit.aes = FALSE,
    fill = unname(irbs_pal('harvest')),
    alpha = 0.25
  ) +
  geom_line(
    aes(group = segment),
    colour = unname(irbs_pal('illini-blue')),
    linewidth = 0.4
  ) +
  geom_point(colour = unname(irbs_pal('illini-blue')), size = 0.6) +
  scale_x_datetime(
    breaks = scales::breaks_pretty(n = 8),
    labels = scales::label_date_short(),
    timezone = 'America/Chicago'
  ) +
  labs(
    title = 'Above Brandon Rd. L&D (05538010): receiver line voltage',
    subtitle = sprintf(
      'Last 8 days · %d logger outages > 5 min (longest %d min) · %d intervals with logger up but receiver silent',
      nrow(outages),
      max(outages$gap_min),
      nrow(silent)
    ),
    x = 'Time (Central)',
    y = 'Receiver line voltage (V)',
    caption = paste(
      'Grey bands: no logger records. Amber bands: logger records with no receiver response.',
      'Data: USGS Fish Tracks Real-Time feed via IRBS database.'
    )
  ) +
  theme_irbs_minimal(base_size = 13)

ggsave(
  here('follow up items', 'brandon_rd_voltage_outages.png'),
  width = 11,
  height = 5,
  dpi = 150,
  bg = 'white'
)

dbDisconnect(con)
