library(tidyverse)
library(DBI)
library(odbc)
library(here)

source(here('code', 'fn_get_fishtracks.R'))



df <- get_fishtracks(station_id = '412341088161001')
