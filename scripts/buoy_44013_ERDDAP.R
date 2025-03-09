#'--------------------------------------
#' Station 44013
#' Boston Approach Lighted Buoy BF NOAA 44013
#' NDBC ID: 44013
#' Lat: 42.346 Lon: -70.651
#' https://www.ndbc.noaa.gov/station_realtime.php?station=44013
#' https://mariners.neracoos.org/platform/44013
#' Boston Harbor
#' Owned and maintained by NDBC
#'--------------------------------------

library(readr) # for reading in files
library(lubridate) # for date time formats
library(dplyr) # for data manipulation and transformation
library(tidyr) # for tidying and reshaping data
library(rerddap) # for accessing ERDDAP servers
library(ggplot2) # for visualization

# ---- NDBC_44013 NERACOOS ERDDAP Data ----
#' Data available via data.NERACOOS ERDDAP base URL:
#' https://data.neracoos.org/erddap/index.html
#' https://data.neracoos.org/erddap/info/NDBC_44013/index.html
#' -------------------------------------
#' # NOTE - Data available for station 44013 via ERDDAP appears to only be available
# starting Jan 23, 2023


# Get information about the NDBC_44013 data set
info_NDBC_44013 <- rerddap::info("NDBC_44013",
                                 url = "data.neracoos.org/erddap")

# Get all data for NDBC_44013 data set
NDBC_44013 <- 
  tabledap(info_NDBC_44013)


# ---- NDBC_44013 IOOS ERDDAP Data ----
#' Data available via IOOS base URL:
#' https://erddap.sensors.ioos.us/erddap/index.html
#' https://erddap.sensors.ioos.us/erddap/info/gov-ndbc-44013/index.html
#' -------------------------------------
#' # NOTE - Data available for station 44013 via ERDDAP appears to only be available
# starting Jan 01, 2015

info_NDBC_44013 <- rerddap::info("gov-ndbc-44013",
                                 url = "erddap.sensors.ioos.us/erddap")

# Get all data for NDBC_44013 data set
NDBC_44013 <- 
  tabledap(info_NDBC_44013, fields = "time")












