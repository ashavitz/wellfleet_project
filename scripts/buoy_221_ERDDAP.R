# ---- Header ----
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

# ---- Importing files via ERDDAP ----
#' -------------------------------------
#' Importing files via ERDDAP
#' CDIP ERDDAP base URL:
#' https://erddap.cdip.ucsd.edu/erddap/index.html
#' -------------------------------------

# ---- Load Libraries ----
library(readr) # for reading in files
library(lubridate) # for date time formats
library(dplyr) # for data manipulation and transformation
library(tidyr) # for tidying and reshaping data
library(rerddap) # for accessing ERDDAP servers
library(ggplot2) # for visualization

# ---- Set Global ggplot Themes ----

# Set ggplot theme to minimal, rotate x axis labels, and center plot titles
theme_set(
  theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5)
    )
)

# ---- Air temperature measurements ----

# Get information about the cat4 (air measurements) data set
data_info_cat4 <- rerddap::info("cat4_agg",
                           url = "erddap.cdip.ucsd.edu/erddap")


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
# (acm = acoustic current measurements)

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
    mean_acm_direction = as.numeric(circular::mean.circular(
      circular::circular(acmDirection, units = "degrees", modulo = "2pi"))),
  mean_acm_vertical_speed = mean(acmVerticalSpeed)
  ) |> 
  ungroup()

# ---- Wave measurements ----

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
         buoy_221_acm_daily,
         buoy_221_wave_daily),
    full_join, by = "date") |> 
  arrange(date)


# ---- Plot all Variables ----

# Daily plots

# Create a list of variable names, excluding the 'date' column
scalar_variable_means <- colnames(buoy_221_daily)[!colnames(buoy_221_daily) 
                                             %in% c("date", "mean_acm_direction")]
angular_variable_means <- c("mean_acm_direction")
variable_means <- c(scalar_variable_means, angular_variable_means)
variable_means_meta <- list(
  mean_air_temp = "Mean Air Temperature (degC)",
  max_air_temp = "Max Air Temperature (degC)",
  mean_sst = "Mean SST (degC)",
  max_sst = "Max SST (deg C)",
  mean_acm_speed = "Mean Current Speed 0.75m depth (m/s)",
  max_acm_speed = "Max Current Speed 0.75m depth (m/s)",
  mean_acm_vertical_speed = "Mean Vertical Current Speed 0.75m depth (m/s)",
  mean_wave_hs = "Mean Wave Significant Height (m)",
  mean_wave_tp = "Mean Wave Period (s)
            (variance spectral density max)",
  mean_wave_Ta = "Mean Wave Period (s)
    (variance spectral density first frequency moment)",
  mean_acm_direction = "Current Direction 0.75m depth (degT)"
)


for (var in variable_means) {
  p <- ggplot(buoy_221_daily,
              aes(x = date,
                  y = .data[[var]])) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    labs(x = "Date",
         y = variable_means_meta[[var]],
         title = paste("Daily Time Series - 221 Buoy (Cape Cod Bay)",
                       variable_means_meta[[var]], sep = "\n")) +
    scale_color_brewer(palette = "Set2")
  
  print(p)
}


# Annual plots

# For each year with at least 80% complete data (80% of days have data) calculate annual mean
# Calculate annual mean for all variables
buoy_221_annual <- buoy_221_daily |> 
  mutate(year = year(date)) |> 
  group_by(year) |> 
  summarize(
    across(all_of(scalar_variable_means),
           
           # Calculate mean if there are non-NA variable observations for at least 80% of a 365 day year
           list(mean = function(x) if (sum(!is.na(x)) >= 0.8 * 365) {mean(x, na.rm = TRUE)}
                else {NA_real_}),
           .names = "{.col}"
    ),
    across(all_of(angular_variable_means),
           list(mean = function (x) if (sum(!is.na(x)) >= 0.8 * 365) {
             
             # Use circular package to calculate circular mean
             as.numeric(circular::mean.circular(
               circular::circular(x, units = "degrees", modulo = "2pi"), na.rm = TRUE))} 
             else {NA_real_}),
           .names = "{.col}"
    ),
    .groups = "drop"
  )


# Plot annual means
for (var in variable_means) {
  p <- ggplot(buoy_221_annual,
              aes(x = year,
                  y = .data[[var]])) +
    geom_point(size = 3) +
    geom_smooth(method = "lm", se = FALSE) +
    labs(x = "Date",
         y = variable_means_meta[[var]],
         title = paste("Annual Time Series - 221 Buoy (Cape Cod Bay)",
                       variable_means_meta[[var]], sep = "\n"),
         caption = "(only years with at least 80% complete data included)") +
    scale_color_brewer(palette = "Set2")
  
  print(p)
}
