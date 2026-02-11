#'--------------------------
#' Load SST from NOAA CoastWatch ERDDAP
#' Sea Surface Temperature
#' Multi-scale Ultra-high Resolution (MUR) SST Analysis fv04.1
#' Global, 0.01°
#' 2002-present, Daily
#' Data access: https://coastwatch.pfeg.noaa.gov/erddap/info/jplMURSST41/index.html
#' Metadata: https://podaac.jpl.nasa.gov/dataset/MUR-JPL-L4-GLOB-v4.1
#' Acknowledgement: These data were provided by JPL under support by NASA MEaSUREs program.
#'--------------------------

# ---- Load Libraries ----
library(dplyr)
library(lubridate)
library(purrr) # for map
library(readr)
library(rerddap) # for accessing ERDDAP servers

# ---- Define Functions ----

# Attempting to pull too much data at once from the NOAA CoastWatch ERDDAP server can cause failure
# so it is helpful to request subsets of the data. The following function is designed to work
# with the rerddap::griddap() function to break up requests into smaller time chunks, and then 
# bind then back together.

# Define a function to download data in chunks (helpful to prevent crashing/errors)
pull_sst_chunks <- function(data_set_info, 
                            fields, 
                            latitude, 
                            longitude, 
                            start_date, 
                            end_date,
                            chunk_months = 6) {
  
  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)
  
  # Create sequence of chunk start dates
  chunk_starts <- seq.Date(from = start_date, to = end_date, by = paste0(chunk_months, " months"))
  
  # Make chunk end dates (6 months later or up to end_date)
  # Note - %m+% lets you add a month to a date without exceeding the last day of the new month
  # https://search.r-project.org/CRAN/refmans/lubridate/html/mplus.html
  # Note - this line effectively takes the starting date, adds 6 months, and then for each section
  # (pmin for each combo instead of just 1 min) takes that 6 month later value, or the set end_date
  # if end_date is earlier that the calculated end date
  chunk_ends <- pmin(chunk_starts %m+% months(chunk_months) - days(1), end_date)
  
  # Pull data for each chunk
  sst_list <- map2(chunk_starts, chunk_ends, function(chunk_start, chunk_end) {
    message(paste("Pulling chunk:", chunk_start, "to", chunk_end))
    
    # Much of the structure below using tryCatch is inspired by:
    # https://stackoverflow.com/questions/12193779/how-to-use-the-trycatch-function
    tryCatch({
      griddap(data_set_info,
              fields = fields,
              latitude = latitude,
              longitude = longitude,
              time = c(as.character(chunk_start), as.character(chunk_end))
      )$data |> 
        
        # Above returns some columns as arrays - need to flatten
        mutate(across(where(is.array), as.vector))
    }, error = function(e) {
      message(paste("Failed chunk:", chunk_start, "-", chunk_end))
      return(NULL)
    })
  })
  
  sst_combined <- bind_rows(sst_list)
  
  return(sst_combined)
}


# ---- Load Data ----
# NOTE - The earliest available day is 2002-06-02

# Get information about the data set
data_set_info <- rerddap::info("jplMURSST41",
                               url = "https://coastwatch.pfeg.noaa.gov/erddap")


# Duck Harbor, Cqpe Cod, MA, 2003-01-01 through 2023-12-31
sst_data_dh <- pull_sst_chunks(
  data_set_info = data_set_info,
  fields = c("analysed_sst"),
  latitude = c(41.94, 41.94),
  longitude = c(-70.09, -70.09),
  start_date = "2003-01-01",
  end_date = "2023-12-31",
  chunk_months = 6
)

# Examine data
str(sst_data_dh)
summary(sst_data_dh)
visdat::vis_dat(sst_data_dh)
View(sst_data_dh)

# Export data to locally saved csv (to avoid having to do pull from ERDDAP each time)
# write_csv(sst_data_dh, "data/noaa_coastwatch_sst/sst_data_dh.csv")
