library(tidyverse)
library(httr)
library(rvest)
library(stringr)

# helper functions

# check if URL exists
url_exists <- function(u) {
  tryCatch({
    res <- HEAD(u)
    status_code(res) == 200
  }, error = function(e) {
    FALSE
  })
}

# main function
get_fishtracks <- function(station_id){

  # get station table for timezone
  fishtracks_con <- dbConnect(odbc::odbc(), "Fish_Tracks_Real_Time")
  
  tbl_station <- tbl(fishtracks_con, "station") %>% collect()
  
  dbDisconnect(fishtracks_con)
  
  
  url_prefix <- "https://cm.water.usgs.gov/data/Fish_Tracks_Real_Time/"
  html_url <- paste0(url_prefix, station_id, ".html")
  csv_url <- paste0(url_prefix, station_id, ".csv")
  
  csv_url_exists <- url_exists(csv_url)
  
  if(!csv_url_exists){
    stop(
      sprintf(
        "Failed to access CSV URL:\n %s\nThe resource does not exist or is unreachable.",
        csv_url
      ),
      call. = FALSE
    )
  }
  
  html_url_exists <- url_exists(html_url)
  
  if(!html_url_exists){
    message(
      sprintf(
        "Failed to access HTML URL:\n %s\nThe resource does not exist or is unreachable. Time Zone will default to CST, Station Name with default to NULL",
        html_url
      ),
      call. = FALSE
    )
  }
  
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
      col_names = FALSE, show_col_types = FALSE, 
      col_types = cols(.default = col_character()), 
      na = c("", "NA", "N/A", "NULL", "--", " ")
    )
  }
  
  
  

  df <- tryCatch({
    read_csv_usgs(csv_url)
  }, error = function(e) {
    stop(
      sprintf(
        "URL is reachable but failed to read as CSV:\n  %s\nError: %s",
        url, e$message
      ),
      call. = FALSE
    )
  })
  
  
  if(ncol(df) != 69){
    warning(paste0("Downloaded data table for Station ID ", station_id, " is not expected shape (69 columns). Station may be skipped."))
  }else{
  
  names(df) <- c(
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
      across(all_of(c('Record', 'VRDetectCount', 'VRTagCount', 'VRUniqTagCount')), as.integer),
      across(all_of(c('VRLineVolt', 'VRBatVolt', 'VRTemp', 'VRDetectMem')), as.numeric)
    )
  
  }
  
  df <- df %>%
    mutate(station_id = station_id) %>%
    relocate(station_id) %>%
    left_join(tbl_station %>% select(station_id, tz), by = 'station_id') %>%
    rowwise() %>%
    mutate(
      across(where(is.POSIXct), ~ force_tz(.x, tzone = tz))
    ) %>%
    ungroup() %>%
    select(-tz)
  
  message(paste0('Station ID ', station_id, ': ', prettyNum(nrow(df), big.mark = ','), ' rows successfully retrieved.'))
  
  return(df)
  }


