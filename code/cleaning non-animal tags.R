library(here)
library(tidyverse)

int <- read.csv(here('data', 'Non-animal tags from Appel', 'intermediate.csv'))

int$TagID

int <- int %>%
  mutate(
    tag_code_space = str_extract(TagID, '(^...-....)-.*$', 1),
    tag_id_code = str_extract(TagID, '^...-....-(.*$)', 1)
  ) %>%
  distinct() %>%
  select(-TagID)


write.csv(
  int,
  here(
    'data',
    'Non-animal tags from Appel',
    'Tx, Nextrak, and unknown matches_clean.csv'
  )
)


int


# more tags sent on 8/27/26
library(DBI)
library(odbc)

con <- dbConnect(odbc::odbc(), "Fish_Tracks_Real_Time")
tbl_nat <- tbl(con, Id(schema = 'dbo', table = "non_animal_tag"))

existing_nat <- tbl_nat %>%
  pull(TagID)

more_tx <- read.csv(here(
  'data',
  'Non-animal tags from Appel',
  'raw',
  'receiver transmitters in RAFT 20260827.csv'
))


more_tx <-
  more_tx %>%
  rename(TagID = V1) %>%
  select(-V2) %>%
  mutate(
    tag_code_space = str_extract(TagID, '(^...-....)-.*$', 1),
    tag_id_code = str_extract(TagID, '^...-....-(.*$)', 1)
  ) %>%
  distinct() %>%
  filter(!TagID %in% existing_nat) %>%
  select(-TagID)


more_tx %>%
  write.csv(
    here(
      'data',
      'Non-animal tags from Appel',
      'Treceiver transmitters in RAFT 20260827_clean.csv'
    ),
    row.names = FALSE
  )
