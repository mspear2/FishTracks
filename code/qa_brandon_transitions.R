# QA: side-to-side transitions at Brandon Rd. L&D ##############################
#
# Pulls every detection at the two Brandon Road receivers, collapses each
# fish's history into runs on one side, and summarises the switches between
# sides. The goal is to pick thresholds for the passage-events feature
# (feature ideas/passage_events_plan.md):
#
#   visit gap  how long a silence at one station ends a visit
#   n, m       detections / minutes on the new side for a "confirmed" passage
#   t          a return to the old side within t hours means "not a passage"
#
# Read-only: queries the database, writes nothing to it. Outputs (CSVs, PNGs,
# a multi-page PDF of per-fish tracks) go to output/qa_brandon/.
#
# Timing uses the record-level TimeStamp (UTC), not TagIDTimeStamp_n, which
# still carries the time-zone offset bug. Resolution is therefore the logger's
# reporting interval (see record_summary below).

library(tidyverse)
library(DBI)
library(odbc)
library(here)

con <- dbConnect(odbc::odbc(), "Fish_Tracks_Real_Time")
dbo <- function(name) Id(schema = 'dbo', table = name)

# Parameters ####################################################################

below <- '412341088161001' # Below Brandon Rd. L&D (downstream side)
above <- '05541498' # Above Brandon Rd. L&D (upstream side)
visit_gap_h <- 2 # starting guess; section 2 checks it
tz_local <- 'America/Chicago'
n_track_pages <- 20 # fish with the most transitions, one PDF page each

out_dir <- here('output', 'qa_brandon')
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

side_of <- function(station_id) if_else(station_id == above, 'above', 'below')

# 1. Data #######################################################################

# Detections: one row per fish per logger record. A fish can occupy several
# tag slots in one record, hence distinct().
det_raw <- tbl(con, dbo('event_animal')) %>%
  filter(station_id %in% c(below, above)) %>%
  left_join(
    tbl(con, dbo('tag')) %>% select(TagID, animal_id, common_name_e),
    by = c('TagID', 'animal_id')
  ) %>%
  mutate(
    fish_id = coalesce(as.character(animal_id), TagID),
    common_name_e = coalesce(common_name_e, 'unknown')
  ) %>%
  distinct(fish_id, common_name_e, station_id, TimeStamp) %>%
  collect() %>%
  mutate(side = side_of(station_id)) # TimeStamp is UTC (odbc default)

# One species per fish (most frequent label), so a fish whose tag sometimes
# lacks an animal_id isn't counted twice
fish_species <- det_raw %>%
  count(fish_id, common_name_e) %>%
  slice_max(n, by = fish_id, with_ties = FALSE) %>%
  select(fish_id, common_name_e)

det <- det_raw %>%
  distinct(fish_id, station_id, side, TimeStamp) %>%
  arrange(fish_id, TimeStamp)

# Logger records (with or without tags): reporting interval and outages, so a
# silent side can be told apart from an offline side
records <- tbl(con, dbo('event')) %>%
  filter(station_id %in% c(below, above)) %>%
  distinct(station_id, TimeStamp) %>%
  collect() %>%
  mutate(side = side_of(station_id)) %>%
  arrange(station_id, TimeStamp) %>%
  group_by(station_id) %>%
  mutate(
    dt_min = as.numeric(difftime(TimeStamp, lag(TimeStamp), units = 'mins'))
  ) %>%
  ungroup()

record_summary <- records %>%
  group_by(station_id, side) %>%
  summarise(
    first_record = min(TimeStamp),
    last_record = max(TimeStamp),
    n_records = n(),
    median_interval_min = median(dt_min, na.rm = TRUE),
    n_outages_gt_1h = sum(dt_min > 60, na.rm = TRUE),
    hours_offline = sum(dt_min[dt_min > 60], na.rm = TRUE) / 60,
    .groups = 'drop'
  )
print(record_summary)

record_interval_min <- max(record_summary$median_interval_min)

# Outage spans (gap > 3 x the usual interval), for shading the track plots
outages <- records %>%
  filter(dt_min > 3 * record_interval_min) %>%
  transmute(side, xmin = TimeStamp - dt_min * 60, xmax = TimeStamp)

cat(
  '\nFish detected:',
  n_distinct(det$fish_id),
  '| both sides:',
  det %>%
    distinct(fish_id, side) %>%
    count(fish_id) %>%
    filter(n == 2) %>%
    nrow(),
  '\n'
)

# 2. Visit gap: silences between consecutive detections of the same fish ######
# A trough between "still here" gaps (minutes) and "left and came back" gaps
# (days) is where the visit gap belongs. Switches are shown for contrast.

gaps <- det %>%
  group_by(fish_id) %>%
  mutate(
    kind = if_else(side == lag(side), 'same side', 'switched side'),
    gap_min = as.numeric(difftime(TimeStamp, lag(TimeStamp), units = 'mins'))
  ) %>%
  ungroup() %>%
  filter(!is.na(gap_min))

gaps %>%
  group_by(kind) %>%
  summarise(
    n = n(),
    q = list(quantile(gap_min, c(.5, .75, .9, .95, .99))),
    .groups = 'drop'
  ) %>%
  unnest_wider(q) %>%
  print()

p_gaps <- ggplot(gaps, aes(gap_min, fill = kind)) +
  geom_histogram(bins = 80, alpha = 0.7, position = 'identity') +
  geom_vline(xintercept = visit_gap_h * 60, linetype = 2) +
  scale_x_log10(
    breaks = c(1, 10, 60, 360, 1440, 10080, 43200),
    labels = c('1 min', '10 min', '1 h', '6 h', '1 d', '1 wk', '30 d')
  ) +
  scale_y_log10() +
  labs(
    x = 'Gap since previous detection of the same fish (log scale)',
    y = 'Count',
    fill = NULL,
    title = 'Inter-detection gaps at Brandon Rd. receivers',
    subtitle = sprintf('Dashed line: current visit gap (%s h)', visit_gap_h)
  ) +
  theme_bw()
ggsave(file.path(out_dir, '1_gaps.png'), p_gaps, width = 9, height = 5)

# Visits: stays split by the visit gap (saved for reference; transitions
# below use runs, which ignore gaps)
visits <- det %>%
  group_by(fish_id) %>%
  mutate(
    new_visit = is.na(lag(side)) |
      side != lag(side) |
      difftime(TimeStamp, lag(TimeStamp), units = 'hours') > visit_gap_h,
    visit_id = cumsum(new_visit)
  ) %>%
  group_by(fish_id, visit_id, station_id, side) %>%
  summarise(
    start = min(TimeStamp),
    end = max(TimeStamp),
    n_det = n(),
    .groups = 'drop'
  )

# 3. Runs and transitions ######################################################
# A run is an unbroken stretch of detections on one side, whatever the gaps.
# With two receivers every run change is a candidate crossing.

runs <- det %>%
  group_by(fish_id) %>%
  mutate(run_id = cumsum(is.na(lag(side)) | side != lag(side))) %>%
  group_by(fish_id, run_id, side) %>%
  summarise(
    start = min(TimeStamp),
    end = max(TimeStamp),
    n_det = n(),
    .groups = 'drop'
  ) %>%
  mutate(duration_min = as.numeric(difftime(end, start, units = 'mins')))

transitions <- runs %>%
  arrange(fish_id, run_id) %>%
  group_by(fish_id) %>%
  mutate(
    from_n_det = n_det,
    arrival_start = lead(start),
    arrival_end = lead(end),
    arrival_n_det = lead(n_det),
    arrival_duration_min = lead(duration_min),
    back_start = lead(start, 2), # next run is necessarily the old side
    n_transitions_fish = n() - 1
  ) %>%
  ungroup() %>%
  filter(!is.na(arrival_start)) %>%
  transmute(
    fish_id,
    direction = if_else(side == 'below', 'upstream', 'downstream'),
    left_at = end,
    arrived_at = arrival_start,
    transit_min = as.numeric(difftime(arrival_start, end, units = 'mins')),
    from_n_det,
    arrival_n_det,
    arrival_duration_min,
    # hours from the last detection on the new side to the first back on the
    # old side; NA = never went back
    return_h = as.numeric(difftime(back_start, arrival_end, units = 'hours')),
    n_transitions_fish
  ) %>%
  left_join(fish_species, by = 'fish_id') %>%
  # was either logger offline during the transit? (a silent side is not proof
  # the fish wasn't there)
  rowwise() %>%
  mutate(
    outage_in_transit = any(outages$xmin < arrived_at & outages$xmax > left_at)
  ) %>%
  ungroup()

cat('\nTransitions:', nrow(transitions), '\n')
transitions %>% count(direction, common_name_e) %>% print(n = Inf)

# 4. What the switches look like ###############################################

# Near-simultaneous switches (within ~2 records): likely range overlap rather
# than movement through the lock or over the dam
transitions %>%
  mutate(
    transit = cut(
      transit_min,
      c(-Inf, min(2 * record_interval_min, 59), 60, 360, 1440, Inf),
      labels = c('<= 2 records', '<= 1 h', '1-6 h', '6-24 h', '> 24 h')
    )
  ) %>%
  count(direction, transit) %>%
  pivot_wider(names_from = direction, values_from = n, values_fill = 0) %>%
  print()

# Ping-pong: fish-days with many switches
pingpong <- transitions %>%
  mutate(day = as_date(with_tz(arrived_at, tz_local))) %>%
  count(fish_id, common_name_e, day, name = 'switches') %>%
  arrange(desc(switches))
cat('\nFish-days with >= 4 switches:', sum(pingpong$switches >= 4), '\n')
print(head(pingpong, 15))

p_transit <- ggplot(
  transitions,
  aes(pmax(transit_min, 0.5), fill = direction)
) +
  geom_histogram(bins = 60, alpha = 0.7, position = 'identity') +
  scale_x_log10(
    breaks = c(1, 10, 60, 360, 1440, 10080),
    labels = c('1 min', '10 min', '1 h', '6 h', '1 d', '1 wk')
  ) +
  labs(
    x = 'Last detection on old side to first on new side (log scale)',
    y = 'Transitions',
    fill = NULL,
    title = 'Transit times across Brandon Rd. L&D'
  ) +
  theme_bw()
ggsave(file.path(out_dir, '2_transit.png'), p_transit, width = 9, height = 5)

# Arrival strength vs whether the fish went back: confirmed passages should
# separate from overlap noise (few detections, quick return)
p_arrival <- transitions %>%
  mutate(
    went_back = case_when(
      is.na(return_h) ~ 'never went back',
      return_h <= 1 ~ 'back within 1 h',
      return_h <= 24 ~ 'back within 24 h',
      TRUE ~ 'back after > 24 h'
    )
  ) %>%
  ggplot(aes(
    arrival_n_det,
    pmax(arrival_duration_min, 0.5),
    color = went_back
  )) +
  geom_jitter(width = 0.05, height = 0.05, alpha = 0.6) +
  scale_x_log10() +
  scale_y_log10(
    breaks = c(1, 10, 60, 360, 1440, 10080),
    labels = c('1 min', '10 min', '1 h', '6 h', '1 d', '1 wk')
  ) +
  facet_wrap(~direction) +
  labs(
    x = 'Detections on the new side (log)',
    y = 'Time on the new side (log)',
    color = NULL,
    title = 'Arrival strength vs return to the old side'
  ) +
  theme_bw()
ggsave(file.path(out_dir, '3_arrival.png'), p_arrival, width = 10, height = 5)

# 5. Threshold sensitivity ######################################################
# Transitions that would count as "confirmed" under each combination of
# n (min detections on the new side), m (min minutes there) and t (no return
# within t hours).

sensitivity <- expand_grid(
  n_min = c(1, 2, 3, 5, 10),
  m_min = c(0, 30, 120),
  t_h = c(1, 6, 24, 72)
) %>%
  mutate(
    confirmed = pmap(list(n_min, m_min, t_h), \(n, m, t) {
      transitions %>%
        filter(
          arrival_n_det >= n,
          arrival_duration_min >= m,
          is.na(return_h) | return_h > t
        ) %>%
        count(direction)
    })
  ) %>%
  unnest(confirmed) %>%
  pivot_wider(names_from = direction, values_from = n, values_fill = 0)
print(sensitivity, n = Inf)

# 6. Per-fish tracks ############################################################
# One page per fish: which side it was heard on over time. Grey bands are
# logger outages on that side. Read these alongside the tables above.

track_fish <- transitions %>%
  distinct(fish_id, n_transitions_fish) %>%
  slice_max(n_transitions_fish, n = n_track_pages, with_ties = FALSE) %>%
  pull(fish_id)

y_of <- c(below = 1, above = 2)

pdf(file.path(out_dir, '4_tracks.pdf'), width = 11, height = 4)
for (f in track_fish) {
  d <- det %>% filter(fish_id == f) %>% mutate(y = y_of[side])
  rng <- range(d$TimeStamp)
  o <- outages %>%
    filter(xmax > rng[1], xmin < rng[2]) %>%
    mutate(y = y_of[side])
  spp <- fish_species$common_name_e[fish_species$fish_id == f]
  n_tr <- sum(transitions$fish_id == f)

  p <- ggplot(d, aes(TimeStamp, y)) +
    geom_rect(
      data = o,
      aes(xmin = xmin, xmax = xmax, ymin = y - 0.3, ymax = y + 0.3),
      inherit.aes = FALSE,
      fill = 'grey80'
    ) +
    geom_step(color = 'grey60', direction = 'hv') +
    geom_point(aes(color = side), size = 1.2) +
    scale_y_continuous(
      breaks = 1:2,
      labels = c('Below Brandon', 'Above Brandon'),
      limits = c(0.6, 2.4)
    ) +
    scale_x_datetime(timezone = tz_local) +
    scale_color_manual(
      values = c(below = '#13294B', above = '#FF5F05'),
      guide = 'none'
    ) +
    labs(
      x = 'Time (Central)',
      y = NULL,
      title = sprintf('%s (%s): %d transitions', f, spp, n_tr)
    ) +
    theme_bw()
  print(p)
}
dev.off()

# 7. Save tables ################################################################

write_csv(record_summary, file.path(out_dir, 'record_summary.csv'))
write_csv(transitions, file.path(out_dir, 'transitions.csv'))
write_csv(runs, file.path(out_dir, 'runs.csv'))
write_csv(visits, file.path(out_dir, 'visits.csv'))
write_csv(pingpong, file.path(out_dir, 'pingpong_fish_days.csv'))
write_csv(sensitivity, file.path(out_dir, 'threshold_sensitivity.csv'))

dbDisconnect(con)
cat('\nOutputs in', out_dir, '\n')
