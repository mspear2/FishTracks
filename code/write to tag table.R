library(tidyverse)
library(here)

raft_files_path <- here('data', 'Transmitter data from RAFT')
raft_files <- list.files(
  raft_files_path,
  pattern = '\\.csv$',
  full.names = TRUE
)
raft_files <- raft_files[-8]
# read and combine
tags <- map_dfr(
  raft_files,
  ~ read_csv(.x, col_types = cols(.default = col_character()))
)

tags

# correct animal_id values that mistakenly have two dates. they also mistakenly have a _ isntead of a -
tags <- tags %>%
  mutate(
    animal_id = str_replace(
      animal_id,
      "_(\\d{4}-\\d{2}-\\d{2})-\\1$",
      "-\\1"
    )
  )


new_tags <- read_csv(
  here(
    'data',
    'Transmitter data from RAFT',
    'USACECDILR RAFT Exported Transmitter Data 20260917.csv'
  ),
  col_types = cols(.default = col_character())
) %>%
  filter(!animal_id %in% tags$animal_id)


library(DBI)
library(odbc)
con <- dbConnect(odbc::odbc(), "Fish_Tracks_Real_Time")

tbl_tag <- tbl(con, Id(schema = 'dbo', table = "tag"))

existing_tags <- tbl_tag %>% pull(animal_id)


new_tags %>%
  filter(!animal_id %in% existing_tags)

schema <- dbGetQuery(
  con,
  "
SELECT
    COLUMN_NAME,
    DATA_TYPE
FROM INFORMATION_SCHEMA.COLUMNS
WHERE TABLE_NAME = 'tag'
ORDER BY ORDINAL_POSITION
"
)
type_map <- c(
  int = "integer",
  bigint = "integer",
  smallint = "integer",
  float = "numeric",
  real = "numeric",
  numeric = "numeric",
  decimal = "numeric",
  varchar = "character",
  nvarchar = "character",
  char = "character"
)

for (i in seq_len(nrow(schema))) {
  col <- schema$COLUMN_NAME[i]
  typ <- schema$DATA_TYPE[i]

  if (!col %in% names(tags)) {
    next
  }

  if (typ %in% c("datetime", "datetime2")) {
    tags[[col]] <- as.POSIXct(tags[[col]], tz = "UTC")
  } else if (typ == "date") {
    tags[[col]] <- as.Date(tags[[col]])
  } else if (!is.na(type_map[typ])) {
    tags[[col]] <- do.call(
      paste0("as.", type_map[typ]),
      list(tags[[col]])
    )
  }
}

for (i in seq_len(nrow(schema))) {
  col <- schema$COLUMN_NAME[i]
  typ <- schema$DATA_TYPE[i]

  if (!col %in% names(new_tags)) {
    next
  }

  if (typ %in% c("datetime", "datetime2")) {
    new_tags[[col]] <- as.POSIXct(new_tags[[col]], tz = "UTC")
  } else if (typ == "date") {
    new_tags[[col]] <- as.Date(new_tags[[col]])
  } else if (!is.na(type_map[typ])) {
    new_tags[[col]] <- do.call(
      paste0("as.", type_map[typ]),
      list(new_tags[[col]])
    )
  }
}


dbWriteTable(
  con,
  "tag",
  new_tags,
  append = TRUE,
  row.names = FALSE
)

dbDisconnect(con)
