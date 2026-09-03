library(tidyverse)
library(DBI)
library(odbc)
library(here)

source(here('code', 'helper_functions.R'))

con <- dbConnect(odbc::odbc(), "Fish_Tracks_Real_Time")
tbl_station <- tbl(con, Id(schema = 'dbo', table = "station"))
tbl_event <- tbl(con, Id(schema = 'dbo', table = "event"))
tbl_tag <- tbl(con, Id(schema = 'dbo', table = "tag"))
tbl_nat <- tbl(con, Id(schema = 'dbo', table = "non_animal_tag"))
v_deployments <- tbl(con, "deployment_intervals")
v_event_animal <- tbl(con, "event_animal")


tbl_eve

tbl_event


tbl_event %>% glimpse


aggregate_detections(tbl_event)

tbl_event %>%
event_pivot_longer() %>%
  left_join(
    tbl_tag %>%
      assemble_TagID()
  )%>%
  filter(TagID %in% nat) %>%
  collect() %>%
  View




tags_intervals <- tbl_tag %>%
  mutate(
    TagID = paste(tag_code_space, tag_id_code, sep = "-")
  ) %>%
  arrange(TagID, utc_release_date_time) %>%
  group_by(TagID) %>%
  mutate(
    next_release = lead(utc_release_date_time)
  ) %>%
  ungroup()



joined <- tbl_event %>%
  collect() %>%
  left_join(
    v_deployments,
    join_by(
      TagID,
      TimeStamp >= deployment_start,
      TimeStamp < deployment_end
    )
  )




most_detected <- v_event_animal %>%
  group_by(TagID, animal_id) %>%
  summarise(n_detections = n()) %>%
  ungroup() %>%
  arrange(desc(n_detections)) %>%
  head(n = 10) %>%
  collect()

most_detected_events <- v_event_animal %>%
  filter(TagID %in% most_detected$TagID) %>%
  left_join(tbl_tag, by = c('TagID', 'animal_id')) %>%
  left_join(tbl_event, by = 'DetectionID') %>%
  left_join(tbl_station) %>%
  collect()



v_event_animal %>%
  left_join(
    tbl_event %>% select(DetectionID, station_id),
    by = "DetectionID"
  ) %>%
  left_join(tbl_station) %>%
  collect() %>%
  ggplot(aes(x = TagIDTimeStamp, y = as.factor(station_id), color = TagID)) +
  geom_point() +
  geom_line() +
  facet_wrap(~river_name)


mult_det_tags <- v_event_animal %>%
  left_join(
    tbl_event %>% select(DetectionID, station_id),
    by = "DetectionID"
  ) %>%
  collect() %>%
  group_by(TagID, animal_id) %>%
  summarise(
    n_stations = n_distinct(station_id),
    stations = paste(sort(unique(station_id)), collapse = ", "),
    .groups = "drop"
  ) %>%
  filter(n_stations > 1) %>%
  pull(TagID) 


v_event_animal %>%
  filter(TagID %in% mult_det_tags) %>%
  left_join(tbl_event) %>%
  left_join(tbl_station)  %>%
  collect() %>%
  ggplot(aes(x = TimeStamp, y = as.factor(station_name), color = TagID, group = TagID)) +
  geom_point() +
  geom_line() +
  facet_wrap(~river_name, scales = 'free_y') +
  scale_y_discrete(
    labels = scales::label_wrap(20) 
  )



unk <- v_event_animal %>% 
  left_join(tbl_event %>% select(DetectionID, station_id)) %>%
  left_join(tbl_station) %>%
  left_join(tbl_tag %>% select(animal_id, common_name_e)) %>%
  collect() %>%
  filter(is.na(animal_id)) %>%
  pull(TagID) %>%
  unique()



v_event_animal %>%
  collect() %>%
  group_by(TagID) %>%
  count() %>%
  ungroup() %>%
  filter(n <= 1)

dbDisconnect(con)

tags %>%
  filter(tag_code_space == 'A69-9001', tag_id_code == '55906')


tags %>%
  group_by(common_name_e) %>%
  summarise(
    n = n(),
    .groups = 'drop'
  ) %>%
  mutate(
    pct = round(n / sum(n) * 100, 2)
  ) %>%
  arrange(-pct) %>%
  clipr::write_clip()



v_event_animal %>%
  left_join(tbl_event %>% select(DetectionID, station_id)) %>%
  left_join(tbl_station) %>%
  filter(station_id == '411955088280601') %>%
  filter(TagID == 'A69-1604-29108') %>%
  collect()


new_wp_tags <- 
c(
'A69-1601-29672',
'A69-1601-62032',
'A69-1601-62867',
'A69-1601-62876',
'A69-1602-25029',
'A69-1602-5623',
'A69-1604-14669',
'A69-1604-29037',
'A69-1604-29044',
'A69-1604-29045',
'A69-1604-29048',
'A69-1604-29063',
'A69-1604-29084',
'A69-1604-29097',
'A69-1604-29104',
'A69-1604-29108',
'A69-1605-16996'
)




old_wp_tags <- v_event_animal %>%
  left_join(tbl_event %>% select(DetectionID, station_id)) %>%
  left_join(tbl_station) %>%
  filter(station_id == '411955088280601') %>%
  filter(TagID %in% new_wp_tags) %>%
  select(TagID) %>%
  distinct() %>%
  collect() %>%
  pull()


excess_tags <- setdiff(new_wp_tags, old_wp_tags) 
overlap_tags <- intersect(new_wp_tags, old_wp_tags) 

tbl_tag %>%
  filter(TagID %in% excess_tags) %>%
  collect() %>%
  View


tbl_tag %>%
  filter(TagID %in% overlap_tags) %>%
  collect() %>%
  nrow()

