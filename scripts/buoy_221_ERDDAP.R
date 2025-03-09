#'--------------------------------------
#' CDIP221 Buoy https://www.ndbc.noaa.gov/station_history.php?station=44090
#' Cape Cod Bay, MA
#' Owned and maintained by Woods Hole Group/NERACOOS
#' Data provided by Scripps Institution of Oceanography
#' National Data Buoy Center Station 44090
#' Waverider Buoy
#' 41.840 N 70.329 W (41°50'24" N 70°19'43" W)
#' Site elevation: sea level
#' Sea temp depth: 0.46 m below water line
#'--------------------------------------

# TODO: Calculate daily mean and high values to reduce size of data sets.
#       Only include days with complete data

# TODO: Initial visualization - variable change over time
#'--------------------------------------

library(readr) # for reading in files
library(lubridate) # for date time formats
library(dplyr) # for data manipulation and transformation
library(tidyr) # for tidying and reshaping data
library(rerddap) # for accessing ERDDAP servers
library(ggplot2) # for visualization

# ---- Importing files via ERDDAP ----
#' -------------------------------------
#' Importing files via ERDDAP
#' CDIP ERDDAP base URL:
#' https://erddap.cdip.ucsd.edu/erddap/index.html
#' -------------------------------------

# ---- Air temperature measurements ----

# Get information about the cat4 (air measurements) data set
data_info_cat4 <- rerddap::info("cat4_agg",
                           url = "erddap.cdip.ucsd.edu/erddap")

# TODO: Import all data for selected variables, including qcs, and convert non-"good" 
# quality data to NAs

# Get time and air temp data for station 221 from the cat4_agg data set
# and QC flags for each variable other than time
buoy_221_cat4 <- 
  tabledap(data_info_cat4,
           'station_id="221"', # query limited to Cape Cod station 221
           fields = c("time",
                      "cat4AirTemperature",
                      "cat4FlagPrimary")
           )

# # Examining the data
# str(buoy_221_cat4)
# summary(buoy_221_cat4)
# visdat::vis_dat(buoy_221_cat4)
# skimr::skim(buoy_221_cat4)

# Clean imported data
buoy_221_cat4 <- buoy_221_cat4 |> 
  mutate(
    time = ymd_hms(time, tz = "UTC"), # convert time column to date time
    date = as.Date(time), # create date column based on time column
    cat4AirTemperature = as.numeric(cat4AirTemperature), # convert air temp to numeric
    # Convert non-"good" quality data to NAs. qc flags of 1 are considered "good"
    cat4AirTemperature = ifelse(cat4FlagPrimary != 1, NA, cat4AirTemperature)
  ) |> 
  # Remove QC flag columns
  select(-cat4FlagPrimary)


# Calculate daily mean and high values to reduce size of data sets
buoy_221_cat4_daily <- 
  buoy_221_cat4 |> 
  group_by(date) |> 
  summarize(
    mean_air_temp = mean(cat4AirTemperature),
    max_air_temp = max(cat4AirTemperature)
  ) |> 
  ungroup()

# ---- Sea surface temperature (SST) measurements ----

# Get data set information
data_info_sst <- rerddap::info("sst_agg",
                               url = "erddap.cdip.ucsd.edu/erddap")

# Get time and SST data for station 221 from the sst_agg data set
# and QC flags for each variable other than time
buoy_221_sst <- 
  tabledap(data_info_sst,
           'station_id="221"', # query limited to Cape Cod station 221
           fields = c("time",
                      "sstSeaSurfaceTemperature",
                      "sstFlagPrimary")
           )

# Clean imported data
buoy_221_sst <- buoy_221_sst |> 
  mutate(
    time = ymd_hms(time, tz = "UTC"), # convert time column to date time
    date = as.Date(time), # create date column based on time column
    sstSeaSurfaceTemperature = as.numeric(sstSeaSurfaceTemperature), # sst to numeric
    # Convert non-"good" quality data to NAs. qc flags of 1 are considered "good"
    sstSeaSurfaceTemperature = ifelse(sstFlagPrimary != 1, NA, sstSeaSurfaceTemperature)
  ) |> 
  # Remove QC flag columns
  select(-sstFlagPrimary)


# Calculate daily mean and high values to reduce size of data sets
buoy_221_sst_daily <- 
  buoy_221_sst |> 
  group_by(date) |> 
  summarize(
    mean_sst = mean(sstSeaSurfaceTemperature),
    max_sst = max(sstSeaSurfaceTemperature)
  ) |> 
  ungroup()

# ---- Surface Current (acm) measurements ----
# (acm = acoustic current measurements ?)

# TODO: Do we need other data in this data set?

# Get data set information
data_info_acm <- rerddap::info("acm_agg",
                               url = "erddap.cdip.ucsd.edu/erddap")

# Get time, speed, direction, and vertical speed for station 221
# from the acm_agg data set
# and QC flags for each variable other than time
buoy_221_acm <- 
  tabledap(data_info_acm,
           'station_id="221"', # query limited to Cape Cod station 221
           fields = c("time",
                      "acmSpeed",
                      "acmDirection",
                      "acmVerticalSpeed",
                      "acmFlagPrimary")
           )

# Clean imported data
buoy_221_acm <- buoy_221_acm |> 
  mutate(
    time = ymd_hms(time, tz = "UTC"), # convert time column to date time
    date = as.Date(time), # create date column based on time column
    across( # convert all other variables to numeric
      c(acmSpeed, acmDirection, acmVerticalSpeed), as.numeric),
    # Convert non-"good" quality data to NAs. qc flags of 1 are considered "good"
    acmSpeed = ifelse(acmFlagPrimary != 1, NA, acmSpeed),
    acmDirection = ifelse(acmFlagPrimary != 1, NA, acmDirection),
    acmFlagPrimary = ifelse(acmFlagPrimary != 1, NA, acmFlagPrimary)
  ) |> 
  # Remove QC flag columns
  select(-acmFlagPrimary)
    

# Calculate daily mean values to reduce size of data sets
buoy_221_acm_daily <- 
  buoy_221_acm |> 
  group_by(date) |> 
  summarize(
    mean_acm_speed = mean(acmSpeed),
    max_acm_speed = max(acmSpeed),
# TODO: Review this calculation and confirm if correct
    mean_acm_direction = {
  
      # Convert wind direction to radians
      rad = (acmDirection * pi / 180)
      
      # Calculate the x and y components of the wind vector
      x = mean(cos(rad))
      y = mean(sin(rad))
      
      # Convert the x and y components back to an angle (in degrees)
      mean_direction = atan2(y, x) * 180 / pi
      
      # Normalize the result to a range of 0 to 360 degrees
      ifelse(mean_direction < 0, mean_direction + 360, mean_direction)
    },
  mean_acm_vertical_speed = mean(acmVerticalSpeed)
  ) |> 
  ungroup()

# ---- Wave measurements ----

# TODO: Do we need other data in this data set?

# Get data set information
data_info_wave <- rerddap::info("wave_agg",
                               url = "erddap.cdip.ucsd.edu/erddap")

# Get time, significant wave height, peak wave period, and average wave period data
# for station 221 from the wave_agg data set
# and QC flags for each variable other than time
buoy_221_wave <- 
  tabledap(data_info_wave,
           'station_id="221"', # query limited to Cape Cod station 221
           fields = c("time", "waveHs", "	waveTp","waveTa", "waveFlagPrimary")
           )

# Clean data
buoy_221_wave <- buoy_221_wave |> 
  mutate(
    time = ymd_hms(time, tz = "UTC"), # convert time column to date time
    date = as.Date(time), # create date column based on time column
    across( # convert all other variables to numeric
      c(waveHs, waveTp, waveTa), as.numeric),
    # Convert non-"good" quality data to NAs. qc flags of 1 are considered "good"
    waveHs = ifelse(waveFlagPrimary != 1, NA, waveHs),
    waveTp = ifelse(waveFlagPrimary != 1, NA, waveTp),
    waveTa = ifelse(waveFlagPrimary != 1, NA, waveTa)
  ) |> 
  # Remove QC flag columns
  select(-waveFlagPrimary)
  

# Calculate daily mean and high values to reduce size of data sets
buoy_221_wave_daily <- 
  buoy_221_wave |> 
  group_by(date) |> 
  summarize(
    mean_wave_hs = mean(waveHs),
    mean_wave_tp = mean(waveTp),
    mean_wave_Ta = mean(waveTa)
  ) |> 
  ungroup()

# ---- Create data set for analysis ----

# Join all data frames by date
buoy_221_daily <- 
  purrr::reduce(
    list(buoy_221_cat4_daily,
         buoy_221_sst_daily,
         buoy_221_wave_daily,
         buoy_221_wave_daily),
    full_join, by = "date") |> 
  arrange(date)

# ---- TODO ----


# Review structure of files. Identify all NA, Null, or otherwise invalid cells

# Standardize all invalid cells to NA

# Remove all unwanted variables

# Remove all rows with NAs? More likely keep them, but be sure 



# Goal: Group all years into one data frame and display
# summary statistics and exploratory graphs
