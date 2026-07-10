library(tidyverse)
library(DBI)
library(odbc)
library(here)

source(here('code', 'helper_functions.R'))

con <- dbConnect(odbc::odbc(), "Fish_Tracks_Real_Time")
tbl_station <- tbl(con, Id(schema = 'dbo', table = "station")) %>% collect()
tbl_event <- tbl(con, Id(schema = 'dbo', table = "event")) %>% collect()
tbl_tag <- tbl(con, Id(schema = 'dbo', table = "tag")) %>% collect()

dbDisconnect(con)


tbl_event


tbl_event %>% glimpse



aggregate_detections(tbl_event)

tbl_event %>%
event_pivot_longer() %>%
  aggregate_detections()
