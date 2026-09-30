library(tidyverse)
library(httr)
library(rvest)
library(stringr)

# check if URL exists ####
url_exists <- function(u) {
  tryCatch(
    {
      res <- HEAD(u)
      status_code(res) == 200
    },
    error = function(e) {
      FALSE
    }
  )
}

# read USGS FishTracks .csv URLs ####
read_csv_usgs <- function(url) {
  res <- GET(
    url,
    user_agent("Mozilla/5.0"),
    timeout(30)
  )

  stop_for_status(res)

  txt <- content(res, as = "text", encoding = "UTF-8")

  read_csv(
    I(txt),
    col_names = FALSE,
    show_col_types = FALSE,
    col_types = cols(.default = col_character()),
    na = c("", "NaN", "NA", "N/A", "NULL", "--", " ", NaN)
  )
}

# get FishTracks from web ####
get_fishtracks <- function(station_id, tz_lookup) {
  url_prefix <- "https://cm.water.usgs.gov/data/Fish_Tracks_Real_Time/"
  html_url <- paste0(url_prefix, station_id, ".html")
  csv_url <- paste0(url_prefix, station_id, ".csv")

  csv_url_exists <- url_exists(csv_url)

  if (!csv_url_exists) {
    stop(
      sprintf(
        "Failed to access CSV URL:\n %s\nThe resource does not exist or is unreachable.",
        csv_url
      ),
      call. = FALSE
    )
  }

  html_url_exists <- url_exists(html_url)

  if (!html_url_exists) {
    message(
      sprintf(
        "Failed to access HTML URL:\n %s\nThe resource does not exist or is unreachable. Time Zone will default to CST, Station Name with default to NULL",
        html_url
      ),
      call. = FALSE
    )
  }

  df <- tryCatch(
    {
      read_csv_usgs(csv_url)
    },
    error = function(e) {
      stop(
        sprintf(
          "URL is reachable but failed to read as CSV:\n  %s\nError: %s",
          csv_url,
          e$message
        ),
        call. = FALSE
      )
    }
  )

  if (ncol(df) != 69) {
    warning(paste0(
      "Downloaded data table for Station ID ",
      station_id,
      " is not expected shape (69 columns). Station may be skipped."
    ))
  } else {
    colnames(df) <- c(
      'TimeStamp',
      'Record',
      'VRDetectCount',
      'VRLineVolt',
      'VRBatVolt',
      'VRTemp',
      'VRDetectMem',
      'VRTagCount',
      'VRUniqTagCount',
      paste0("TagID_", 1:30),
      paste0("TagIDTimeStamp_", 1:30)
    )

    df <- df %>%
      mutate(across(everything(), ~ na_if(.x, ""))) %>%
      mutate(
        across(contains('TimeStamp'), as.POSIXct),
        across(
          all_of(c('Record', 'VRDetectCount', 'VRTagCount', 'VRUniqTagCount')),
          as.integer
        ),
        across(
          all_of(c('VRLineVolt', 'VRBatVolt', 'VRTemp', 'VRDetectMem')),
          as.numeric
        )
      ) %>%
      mutate(
        across(where(is.numeric), ~ replace(., is.nan(.), NA))
      )
  }

  df <- df %>%
    mutate(station_id = station_id) %>%
    relocate(station_id) %>%
    left_join(tz_lookup, by = 'station_id') %>%
    rowwise() %>%
    mutate(
      across(where(is.POSIXct), ~ force_tz(.x, tzone = tz))
    ) %>%
    ungroup() %>%
    select(-tz)

  message(paste0(
    'Station ID ',
    station_id,
    ': ',
    prettyNum(nrow(df), big.mark = ','),
    ' rows successfully retrieved.'
  ))

  return(df)
}

# Pivot event table longer ####
event_pivot_longer <- function(event) {
  event %>%
    pivot_longer(
      cols = matches("^TagID(_\\d+)?$|^TagIDTimeStamp_\\d+$"),
      names_to = c(".value", "idx"),
      names_pattern = "(TagID|TagIDTimeStamp)_?(\\d+)"
    ) %>%
    filter(!is.na(TagID) & TagID != "")
}

# Aggregate detections ####
aggregate_detections <- function(df, unit = "30 minutes") {
  df %>%
    mutate(
      TimeStamp_binned = floor_date(TimeStamp, unit = unit)
    ) %>%
    group_by(animal_id, TagID, station_id, common_name_e, TimeStamp_binned) %>%
    summarise(
      detections = n(),
      presence = 1L,
      .groups = "drop"
    )
}

# SQL Server expression that floors [TimeStamp] to a bin ####
# Shared by aggregate_detections_lazy() and the logger-record query in app.R
# so both sides of the join bin identically.
# TimeStamp is stored in UTC. 5-minute and hourly bins are floored in UTC,
# which coincides with Central boundaries (whole-hour offset). Daily bins
# are floored to Central CALENDAR days: convert to Central, take the date,
# convert that midnight back to UTC. AT TIME ZONE handles DST, so the bin
# for a 23- or 25-hour day is still that local day. The result stays UTC.
# ('Central Standard Time' is SQL Server's zone name; it covers CDT too.)
bin_timestamp_sql <- function(unit = c("5 minutes", "1 hour", "1 day")) {
  unit <- match.arg(unit)

  switch(
    unit,
    "5 minutes" = "DATEADD(minute, (DATEDIFF(minute, 0, [TimeStamp]) / 5) * 5, 0)",
    "1 hour" = "DATEADD(hour, DATEDIFF(hour, 0, [TimeStamp]), 0)",
    "1 day" = paste0(
      "CAST((CAST(CAST(([TimeStamp] AT TIME ZONE 'UTC' ",
      "AT TIME ZONE 'Central Standard Time') AS date) AS datetime2) ",
      "AT TIME ZONE 'Central Standard Time' AT TIME ZONE 'UTC') AS datetime2)"
    )
  )
}

# Width of a bin in seconds, for detecting gaps between consecutive bins ####
bin_width_seconds <- function(unit = c("5 minutes", "1 hour", "1 day")) {
  unit <- match.arg(unit)
  c("5 minutes" = 300, "1 hour" = 3600, "1 day" = 86400)[[unit]]
}

# Aggregate detections ####
aggregate_detections_lazy <- function(
  tbl_lazy,
  unit = c("5 minutes", "1 hour", "1 day")
) {
  unit <- match.arg(unit)
  bin_expr <- bin_timestamp_sql(unit)

  tbl_lazy %>%
    mutate(
      TimeStamp_binned = sql(bin_expr),
      # One key per fish: the animal when known, otherwise the tag itself.
      fish_key = coalesce(as.character(animal_id), TagID)
    ) %>%
    # Level 1: one row per fish per bin (STRING_AGG has no DISTINCT, so the
    # de-duplication has to happen before the tag list is built).
    group_by(station_id, common_name_e, TimeStamp_binned, fish_key) %>%
    summarise(
      TagID = min(TagID, na.rm = TRUE),
      n_det = n(),
      .groups = "drop"
    ) %>%
    # Level 2: one row per station x species x bin, with the tag list.
    group_by(station_id, common_name_e, TimeStamp_binned) %>%
    summarise(
      n_fish = n(),
      n_detections = sum(n_det, na.rm = TRUE),
      # CAST to nvarchar(max): STRING_AGG errors past 8,000 bytes otherwise.
      tag_ids = sql("STRING_AGG(CAST(TagID AS nvarchar(max)), ', ')"),
      .groups = "drop"
    )
}

# Assemble TagID in tag tabl ematching format of TagID in event table ####
assemble_TagID <- function(tag) {
  tag %>%
    mutate(TagID = paste(trimws(tag_code_space), tag_id_code, sep = '-'))
}
