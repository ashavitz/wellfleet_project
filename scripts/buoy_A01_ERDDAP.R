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

# TODO: Plot annual averages (not missing averages)

# TODO: Initial visualization - variable change over time
#'--------------------------------------

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

# ---- Custom Functions ----

make_compass_plot <- function(df, direction_var, speed_var, year_var) {
  
  max_speed <- max(df[[deparse(substitute(speed_var))]], na.rm = TRUE)
  # Plot annual current direction and speed on a compass plot
  p <- ggplot(df) +
    geom_segment(
      aes(
        x = {{direction_var}},
        xend = {{direction_var}},
        y = 0,
        yend = {{speed_var}},
        color = {{year_var}}
      ),
      arrow = arrow(length = unit(0.2, "cm")),
      alpha = 0.7
    ) +
    scale_x_continuous(
      breaks = seq(0, 360, by = 45),
      limits = c(0, 360)
    ) +
    coord_polar(start = 0, direction = 1) +
    theme_minimal() +
    labs(
      title = "Direction Over Time Compass Plot",
      x = "Direction (degrees)",
      y = "Speed"
    ) +
    # Add a north arrow
    annotate(
      "segment",
      x = 0,
      xend = 0,
      y = 0,
      yend = max_speed,
      arrow = arrow(length = grid::unit(0.2, "cm")),
      color = "black"
    ) +
    annotate("text", x = 0, y = max_speed + 0.5, label = "N") +
    # Add year labels
    geom_text(
      aes(
        x = {{direction_var}},
        y = {{speed_var}},
        label = as.character({{year_var}})
      ),
      color = "black",
      size = 2,
      position = position_jitter(0.6),
      angle = 270,
      hjust = -1,
      vjust = 0
    )
  
  return(p)
}

# ---- A01 Aanderaa - Realtime Surface Currents and O2 ----

# Get information about the A01_aanderaa_o2_all data set
info_A01_aanderaa_o2_all <- rerddap::info("A01_aanderaa_o2_all",
                                        url = "neracoos.org/erddap")

# Get all data
A01_aanderaa_o2_all <- 
  tabledap(info_A01_aanderaa_o2_all,
           fields = c("time",
                      "current_speed", "current_speed_qc",
                      "current_direction", "current_direction_qc",
                      "temperature", "temperature_qc")
  )

# Clean imported data
A01_aanderaa_o2_all <- A01_aanderaa_o2_all |> 
  
  # Remove any duplicated rows of data
  distinct() |> 
  
  # convert time column to date time, and create date, year, month, and day columns
  mutate(
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


# Subset data to keep only days with full set of hourly measurements
A01_aanderaa_o2_all_full_days <- A01_aanderaa_o2_all |>
  # Count how many measurements per unique day
  group_by(date) |>
  summarize(n_measurements = n(), .groups = "drop") |>
  # Keep only days with exactly 24 measurements per day
  filter(n_measurements == 24) |>
  select(-n_measurements) |>
  # Join back to original data
  inner_join(A01_aanderaa_o2_all, by = "date")

# Calculate daily mean for all variables
A01_aanderaa_o2_all_daily_summary <- A01_aanderaa_o2_all_full_days |> 
  group_by(date, year, month, day) |> 
  summarize(
    across(c(current_speed, temperature),
           list(mean = function(x) if (any(is.na(x))) NA_real_ else mean(x))),
    current_direction_mean = if (any(is.na(current_direction))) {
      NA_real_
    } else {
      # Use circular package to calculate circular mean
      as.numeric(circular::mean.circular(
        circular::circular(current_direction, units = "degrees", modulo = "2pi")))
    },
    .groups = "drop"
  )

# Plot daily mean values for each variable
variables <- c("current_speed_mean", "current_direction_mean", "temperature_mean")

for (var in variables) {
  p <- ggplot(A01_aanderaa_o2_all_daily_summary,
              aes(x = date,
                  y = .data[[var]])) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    labs(x = "Date",
         y = var,
         title = paste(var, "Daily Time Series - A01 Buoy")) +
    scale_color_brewer(palette = "Set2")
  
  print(p)
}


# For each year with at least 80% complete data (80% of days have full data) calculate annual mean
# No years have complete data

# Subset data to keep only years with 80% complete data
A01_aanderaa_o2_all_annual_80p <- A01_aanderaa_o2_all_daily_summary |> 
  # Exclude rows with any NAs
  filter(complete.cases(A01_aanderaa_o2_all_daily_summary)) |> 
  # Count how many complete days per year
  group_by(year) |> 
  summarize(n_measurements = n(), .groups = "drop") |> 
  # Keep only years with at least 80% complete data
  filter(n_measurements >= (0.8 * 365)) |> 
  select(-n_measurements) |> 
  # Join back to original data
  inner_join(A01_aanderaa_o2_all_full_days, by = "year")

# Calculate annual mean for all variables
A01_aanderaa_o2_all_annual_summary <- A01_aanderaa_o2_all_annual_80p |> 
  group_by(year) |> 
  summarize(
    current_speed_mean = mean(current_speed, na.rm = TRUE),
    temperature_mean = mean(temperature, na.rm = TRUE),
    current_direction_mean =
      # Use circular package to calculate circular mean
      as.numeric(circular::mean.circular(
        circular::circular(current_direction, units = "degrees", modulo = "2pi"),  na.rm = TRUE)),
    .groups = "drop"
  )

for (var in variables) {
  p <- ggplot(A01_aanderaa_o2_all_annual_summary,
              aes(x = year,
                  y = .data[[var]])) +
    geom_point(size = 3) +
    geom_smooth(method = "lm", se = FALSE) +
    labs(x = "Date",
         y = var,
         title = paste(var, "Annual Time Series - A01 Buoy"),
         subtitle = "(only years with at least 80% complete data included)") +
    scale_color_brewer(palette = "Set2")
  
  print(p)
}

# Make compass vector plot using custom function
make_compass_plot(A01_aanderaa_o2_all_annual_summary,
                  current_direction_mean,
                  current_speed_mean,
                  year)

# ---- A01_met_all ----

# Get information about the A01_met_all data set
info_A01_met_all <- rerddap::info("A01_met_all",
                                  url = "neracoos.org/erddap")

scalar_variables <- c("air_temperature",
                      "barometric_pressure",
                      "wind_gust",
                      "wind_speed",
                      "wind_2_gust", 
                      "wind_2_speed",
                      "visibility")

angular_variables <- c("wind_direction",
                        "wind_2_direction")

variables <- c(scalar_variables, angular_variables)

qc_variables <- paste0(variables, "_qc")

# Get all data
A01_met_all <- tabledap(info_A01_met_all,
                        fields = c("time", variables, qc_variables))

# Clean imported data
A01_met_all <- A01_met_all |> 
  
  # Remove any duplicated rows of data
  distinct() |> 
  
  # convert time column to date time, and create date, year, month, and day columns
  mutate(
    time = ymd_hms(time, tz = "UTC"), 
    date = as.Date(time),
    year = year(time),
    month = month(time),
    day = day(time),
    
    # convert to numeric
    across(all_of(variables), as.numeric),
    
    # Loop through variables and apply QC filtering. Only keep QC = 0 ("good" quality)
    across(variables,
           ~ ifelse(A01_met_all[[paste0(cur_column(), "_qc")]] != 0, NA, .),
           .names = "{.col}")
  ) |> 
  
  # Remove QC flag columns and move ymd columns to start
  select(-all_of(qc_variables)) |> 
  relocate(date, year, month, day, .before = time) |> 
  
  # Columns labeled "_2" pick up at the chronological end of their corresponding original variable. 
  # Thus, merge corresponding columns into single columns
  mutate(
    wind_speed = ifelse(is.na(wind_speed), wind_2_speed, wind_speed),
    wind_gust = ifelse(is.na(wind_gust), wind_2_gust, wind_gust),
    wind_direction = ifelse(is.na(wind_direction), wind_2_direction, wind_direction)
  ) |> 
  select(-c(wind_2_speed, wind_2_gust, wind_2_direction)) 

# Updat list of variables
scalar_variables <- c("air_temperature",
                      "barometric_pressure",
                      "wind_gust",
                      "wind_speed",
                      "visibility")

angular_variables <- c("wind_direction")

variables <- c(scalar_variables, angular_variables)


# NOTE - Did NOT check that data was full/complete for each day. Need to determine the best
# approach for this, because days have different intervals of collection

# Calculate daily mean for all variables
A01_met_all_daily_summary <- A01_met_all |> 
  group_by(date, year, month, day) |> 
  summarize(
    across(all_of(scalar_variables),
           list(mean = function(x) if (any(is.na(x))) NA_real_ else mean(x))),
    across(all_of(angular_variables),
           list(mean = function (x) if (any(is.na(x))) {NA_real_} 
                else {
                  # Use circular package to calculate circular mean
                  as.numeric(circular::mean.circular(
                    circular::circular(x, units = "degrees", modulo = "2pi")))
                  }
                )
           ),
    .groups = "drop"
    )


# Plot daily mean values for each variable
scalar_variable_means <- paste0(scalar_variables, "_mean")
angular_variable_means <- paste0(angular_variables, "_mean")
variable_means <- paste0(variables, "_mean")

for (var in variable_means) {
  p <- ggplot(A01_met_all_daily_summary,
              aes(x = date,
                  y = .data[[var]])) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    labs(x = "Date",
         y = var,
         title = paste(var, "Daily Time Series - A01 Buoy")) +
    scale_color_brewer(palette = "Set2")
  
  print(p)
}


# For each year with at least 80% complete data (80% of days have data) calculate annual mean
# Calculate annual mean for all variables
A01_met_all_annual_summary <- A01_met_all_daily_summary |> 
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
  p <- ggplot(A01_met_all_annual_summary,
              aes(x = year,
                  y = .data[[var]])) +
    geom_point(size = 3) +
    geom_smooth(method = "lm", se = FALSE) +
    labs(x = "Date",
         y = var,
         title = paste(var, "Annual Time Series - A01 Buoy")) +
    scale_color_brewer(palette = "Set2")
  
  print(p)
}

# Make compass vector plot using custom function
make_compass_plot(A01_met_all_annual_summary,
                  wind_direction_mean,
                  wind_speed_mean,
                  year)

# ---- A01 Aanderaa - Historic Surface Currents (and 2m depth water temperature) ----

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
  
  # Remove any duplicated rows of data
  distinct() |> 
  
  # convert time column to date time, and create date, year, month, and day columns
  mutate(
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
A01_aanderaa_hist_daily <- A01_aanderaa_hist_full_days |> 
  group_by(date, year, month, day) |> 
  summarize(
    across(c(current_speed, temperature),
           list(mean = function(x) if (any(is.na(x))) NA_real_ else mean(x))),
    current_direction_mean = if (any(is.na(current_direction))) {
      NA_real_
    } else {
# Use circular package to calculate circular mean
      as.numeric(circular::mean.circular(
        circular::circular(current_direction, units = "degrees", modulo = "2pi")))
    },
    .groups = "drop"
  )

# Plot daily mean values for each variable
variables <- c("current_speed_mean", "current_direction_mean", "temperature_mean")

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

# Check which years have a full set of full days (none do)
A01_aanderaa_hist_annual <- A01_aanderaa_hist_daily |> 
  group_by(year) |> 
  summarize(n_measurements = n(), .groups = "drop")




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






