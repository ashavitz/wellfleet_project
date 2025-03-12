# ---- Header ----
#'--------------------------------------
#' NERACOOS Buoy A01 
#' Massachusetts Bay
#' NDBC ID: 44029
#' Lat: 42.53 Lon: -70.56
#' University of Maine
#' https://mariners.neracoos.org/platform/A01
#' https://www.ndbc.noaa.gov/station_page.php?station=44029
#' Massachusetts Bay
#' Owned and maintained by NERACOOS
#' NERACOOS ERDDAP base URL:
#' https://www.neracoos.org/erddap/index.html
#'--------------------------------------

# ---- TODO ----
# TODO: Calculate daily mean and high values to reduce size of data sets.
#       Only include days with complete data

# TODO: Initial visualization - variable change over time
#'--------------------------------------

# ---- Load Libraries ----
library(readr) # for reading in files
library(lubridate) # for date time formats
library(dplyr) # for data manipulation and transformation
library(tidyr) # for tidying and reshaping data
library(rerddap) # for accessing ERDDAP servers
library(ggplot2) # for visualization


# ---- 	A01 Aanderaa - Historic Surface Currents (and 2m depth water temperature) ----

# Get information about the A01_aanderaa_hist data set
info_A01_aanderaa_hist <- rerddap::info("A01_aanderaa_hist",
                                        url = "neracoos.org/erddap")

# Get time, current speed, current direction, and water temp data,
# and QC flags for each variable other than time
A01_aanderaa_hist <- 
  tabledap(info_A01_aanderaa_hist,
           fields = c("time",
                      "current_speed", "current_speed_qc",
                      "current_direction", "current_direction_qc",
                      "temperature", "temperature_qc")
  )

# Examining the data
# str(A01_aanderaa_hist)
# summary(A01_aanderaa_hist)
# visdat::vis_dat(A01_aanderaa_hist)
# skimr::skim(A01_aanderaa_hist)

# Clean imported data
A01_aanderaa_hist <- A01_aanderaa_hist |> 
  mutate(
    # convert time column to date time, and create date, year, month, and day columns
    time = ymd_hms(time, tz = "UTC"), 
    date = as.Date(time),
    year = year(time),
    month = month(time),
    day = day(time),
    
    # convert to numeric
    across(c(current_speed, current_direction, temperature), as.numeric),
    
    # Convert non-"good" quality data to NAs. qc flags of 0 are considered "good"
    current_speed = ifelse(current_speed_qc != 0, NA, current_speed),
    current_direction = ifelse(current_direction_qc != 0, NA, current_direction),
    temperature = ifelse(temperature_qc != 0, NA, temperature)
  ) |> 
  
  # Remove QC flag columns and move ymd columns to start
  select(-current_speed_qc, -current_direction_qc, -temperature_qc) |> 
  relocate(date, year, month, day, .before = time)

  
# Check for duplicated rows of data
duplicates_logical <- 
  duplicated(A01_aanderaa_hist) | duplicated(A01_aanderaa_hist, fromLast = TRUE)

# Subset the original data to keep all duplicate rows
if(any(duplicates_logical)){
  A01_duplicates <- A01_aanderaa_hist[duplicates_logical, ]
  print("Duplicates found")
} else {
  print("Duplicates NOT found")
}
# NOTE - No Duplicates found

# Subset data to keep only days with full set of hourly measurements
A01_aanderaa_hist_full_days <- A01_aanderaa_hist |>
  # Count how many measurements per unique day
  group_by(date) |>
  summarize(n_measurements = n(), .groups = "drop") |>
  # Keep only days with exactly 24 measurements per day
  filter(n_measurements == 24) |>
  select(-n_measurements) |>
  # Join back to original data
  inner_join(A01_aanderaa_hist, by = "date")

# Calculate daily mean for all variables
# TODO - Need to use proper mean calculation for direction variable
A01_aanderaa_hist_daily <- A01_aanderaa_hist_full_days |> 
  group_by(date) |> 
  summarize(
    across(c(current_speed, temperature),
           list(mean = function(x) if (any(is.na(x))) NA_real_ else mean(x))),
    .groups = "drop"
  )

# Plot daily mean values for each variable

variables <- c("current_speed_mean", "temperature_mean")

for (var in variables) {
  p <- ggplot(A01_aanderaa_hist_daily,
              aes(x = date,
                  y = .data[[var]])) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    
    labs(x = "Date",
         y = var,
         title = paste(var, "Time Series - A01 Buoy")) +
    scale_color_brewer(palette = "Set2")
  
  print(p)
}


# For each year with a full set of hourly measurements, calculate annual mean


# ---- A01 Directional Waves ----

# Get information about the A01_waves_mstrain_all data set
info_A01_wave_dir <- rerddap::info("A01_waves_mstrain_all",
                                        url = "neracoos.org/erddap")

# Get time, significant wave height, dominant wave period,
# mean wave direction, and swell wave height data
# and QC flags for each variable other than time
A01_wave_dir <- 
  tabledap(info_A01_wave_dir,
           fields = c("time",
                      "significant_wave_height_3", "significant_wave_height_3_qc",
                      "dominant_wave_period_3", "dominant_wave_period_3_qc",
                      "mean_wave_direction_3", "mean_wave_direction_3_qc",
                      "swell_wave_height_3", "swell_wave_height_3_qc")
  )

# Clean imported data
A01_wave_dir <- A01_wave_dir |> 
  mutate(
    time = ymd_hms(time, tz = "UTC"), # convert time column to date time
    date = as.Date(time), # create date column based on time column
    across(c(significant_wave_height_3,
             dominant_wave_period_3,
             mean_wave_direction_3,
             swell_wave_height_3),
           as.numeric), # convert to numeric
    # Convert non-"good" quality data to NAs. qc flags of 0 are considered "good"
    significant_wave_height_3 = ifelse(
      significant_wave_height_3_qc != 0, NA, significant_wave_height_3),
    dominant_wave_period_3 = ifelse(
      dominant_wave_period_3_qc != 0, NA, dominant_wave_period_3),
    mean_wave_direction_3 = ifelse(
      mean_wave_direction_3_qc != 0, NA, mean_wave_direction_3),
    swell_wave_height_3 = ifelse(
      swell_wave_height_3_qc != 0, NA, swell_wave_height_3),
  ) |> 
  # Remove QC flag columns
  select(-significant_wave_height_3_qc,
         -dominant_wave_period_3_qc,
         -mean_wave_direction_3_qc,
         -swell_wave_height_3_qc)


# ---- A01 Accelerometer - Waves ----

# Get information about the A01_accelerometer_all data set
info_A01_wave_acc <- rerddap::info("A01_accelerometer_all",
                                        url = "neracoos.org/erddap")

# Get time, significant wave height, and dominant wave period data
# and QC flags for each variable other than time
A01_wave_acc <- 
  tabledap(info_A01_wave_acc,
           fields = c("time",
                      "significant_wave_height", "significant_wave_height_qc",
                      "dominant_wave_period", "dominant_wave_period_qc")
  )

# Clean imported data
A01_wave_acc <- A01_wave_acc |> 
  mutate(
    time = ymd_hms(time, tz = "UTC"), # convert time column to date time
    date = as.Date(time), # create date column based on time column
    across(c(significant_wave_height, dominant_wave_period),
           as.numeric), # convert to numeric
    # Convert non-"good" quality data to NAs. qc flags of 0 are considered "good"
    significant_wave_height = ifelse(
      significant_wave_height_qc != 0, NA, significant_wave_height),
    dominant_wave_period = ifelse(
      dominant_wave_period_qc != 0, NA, dominant_wave_period),
  ) |> 
  # Remove QC flag columns
  select(-significant_wave_height_qc,
         -dominant_wave_period_qc)

# ---- 	----

# ----  ----

