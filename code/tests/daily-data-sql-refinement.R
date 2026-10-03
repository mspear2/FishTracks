library(tidyverse)
library(DBI)
library(odbc)
library(here)
source(here('code', 'helper_functions.R'))
con <- dbConnect(odbc::odbc(), 'Fish_Tracks_Real_Time')
dbo <- function(name) Id(schema = 'dbo', table = name)

# the pre-change version: floor every detection row to a Central day
aggregate_old <- function(tbl_lazy) {
  tbl_lazy %>%
    mutate(
      TimeStamp_binned = sql(bin_timestamp_sql('1 day')),
      fish_key = coalesce(as.character(animal_id), TagID)
    ) %>%
    group_by(station_id, common_name_e, TimeStamp_binned, fish_key) %>%
    summarise(
      TagID = min(TagID, na.rm = TRUE),
      n_det = n(),
      .groups = 'drop'
    ) %>%
    group_by(station_id, common_name_e, TimeStamp_binned) %>%
    summarise(
      n_fish = n(),
      n_detections = sum(n_det, na.rm = TRUE),
      tag_ids = sql("STRING_AGG(CAST(TagID AS nvarchar(max)), ', ')"),
      .groups = 'drop'
    )
}

src <- tbl(con, dbo('event_animal')) %>%
  left_join(tbl(con, dbo('tag')), by = c('TagID', 'animal_id')) %>%
  mutate(common_name_e = coalesce(common_name_e, 'unknown'))

# sort the tag lists so STRING_AGG order can't cause false differences
tidy <- function(d) {
  d %>%
    mutate(
      across(c(n_fish, n_detections), as.integer),
      tag_ids = map_chr(
        strsplit(tag_ids, ', ', fixed = TRUE),
        ~ paste(sort(.x), collapse = ', ')
      )
    ) %>%
    arrange(station_id, common_name_e, TimeStamp_binned)
}

old <- system.time(old_d <- tidy(collect(aggregate_old(src))))
new <- system.time(
  new_d <- tidy(collect(aggregate_detections_lazy(src, '1 day')))
)
cat('old:', old[['elapsed']], 's  new:', new[['elapsed']], 's\n')
cat('rows:', nrow(old_d), nrow(new_d), '\n')
cat(
  'identical:',
  isTRUE(all.equal(old_d, new_d, check.attributes = FALSE)),
  '\n'
)
cat('total fish-days old/new:', sum(old_d$n_fish), sum(new_d$n_fish), '\n')
# anything that differs, if anything does
bind_rows(
  old = anti_join(old_d, new_d),
  new = anti_join(new_d, old_d),
  .id = 'only_in'
)
dbDisconnect(con)
