library(tidyverse)

con <- dbConnect(odbc::odbc(), "Fish_Tracks_Real_Time")
tbl_station <- tbl(con, Id(schema = 'dbo', table = "station")) %>% collect()
dbDisconnect(con)

df <- map_df(tbl_station$station_id, get_fishtracks)

df %>%
  group_by(station_id) %>%
  arrange(desc(TimeStamp)) %>%
  slice(1) %>%
  ungroup() %>%
  select(station_id, TimeStamp) %>%
  mutate(
    time_since_timestamp = as.numeric((now() - TimeStamp)) * 60
  )
