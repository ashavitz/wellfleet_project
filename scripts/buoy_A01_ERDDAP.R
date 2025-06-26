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

#'--------------------------------------

# ---- Load Libraries ----
library(readr) # for reading in files
library(lubridate) # for date time formats
library(dplyr) # for data manipulation and transformation
library(tidyr) # for tidying and reshaping data
library(rerddap) # for accessing ERDDAP servers
library(ggplot2) # for visualization
library(ggpmisc) # for annotating plots with p & R2 of fitted polynomial via stat_poly_eq()


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

make_compass_plot <- function(df, direction_var, speed_var, year_var,
                              units_dir, units_speed, medium_type) {
  
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
      # arrow = arrow(length = unit(0.2, "cm")),
      alpha = 0.7
    ) +
    scale_x_continuous(
      breaks = seq(0, 360, by = 45),
      limits = c(0, 360)
    ) +
    coord_polar(start = 0, direction = 1) +
    theme_minimal() +
    labs(
      title = paste(medium_type, "Direction & Speed Over Time Compass Plot"),
      x = units_dir,
      y = units_speed
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

# ---- A01 CTD 2001 - 2025 ----

# Get information about the A01_sbe37_all data set
info_A01_CTD <- rerddap::info("A01_sbe37_all",
                                   url = "neracoos.org/erddap")

# Get time, temperature, salinity, and depth data
# and QC flags for each variable other than time and depth
A01_CTD <- 
  tabledap(info_A01_CTD,
           fields = c("time",
                      "temperature", "temperature_qc",
                      "salinity", "salinity_qc",
                      "depth")
           )


# Clean imported data
A01_CTD <- A01_CTD |> 
  
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
    across(c(temperature, salinity, depth), as.numeric),
    
    # Convert non-"good" quality data to NAs. qc flags of 0 are considered "good"
    temperature = ifelse(temperature_qc != 0, NA, temperature),
    salinity = ifelse(salinity_qc != 0, NA, salinity)
    ) |> 
  
  # Remove QC flag columns and move ymd columns to start
  select(-temperature_qc, -salinity_qc) |> 
  relocate(date, year, month, day, .before = time)


# Only 1m and 20m depths have substantial quantities of data. Remove other depths for analysis.
A01_CTD <- A01_CTD |> 
  filter(depth %in% c(1, 20))


# Calculate daily mean for all variables at each depth
A01_CTD_daily <- A01_CTD |> 
  group_by(date, year, month, day, depth) |> 
  summarize(
    temperature = mean(temperature, na.rm = TRUE),
    salinity = mean(salinity, na.rm = TRUE),
    .groups = "drop"
    )

# Plot daily mean values for each variable
variables <- c("temperature", "salinity")
variables_meta <- list(
  temperature = "Mean Temperature (degC)",
  salinity = "Mean Salinity (psu)"
  )

for (var in variables) {
  
  # Determine y axis range and create vertical padded y_max so annotations will be visible
  y_max <- max(A01_CTD_daily[[var]], na.rm = TRUE)
  y_min <- min(A01_CTD_daily[[var]], na.rm = TRUE)
  range_size = y_max - y_min
  y_max_buffered <- y_max + (range_size * 0.2)
  
  # Plot
  p <- ggplot(A01_CTD_daily,
              aes(x = date,
                  y = .data[[var]])) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    labs(x = "Date",
         y = variables_meta[[var]],
         title = paste("Daily Time Series - A01 Buoy", variables_meta[[var]], sep = "\n")) +
    scale_color_brewer(palette = "Set2") +
    
    # Facet by depth
    facet_wrap(~depth) +
    
    # Add vertical padding
    scale_y_continuous(limits = c(NA, y_max_buffered)) +
    
    # Annotate plot with simple linear model p-values and R2 values
    stat_poly_eq(
      aes(
        label = after_stat(
          paste0(..p.value.label.., "~~~", ..rr.label..)
        )
      ),
      parse = TRUE,
      color = "blue")
  
  print(p)
}


# Annual Means

# Determine valid years (where each month has >= 80% complete data) for each variable and depth
# For each variable and depth, calculate the annual mean only for years valid for that variable

A01_CTD_props <- A01_CTD_daily |> 
  # Group by year, month, and depth
  group_by(year, month, depth) |> 
  # For each variable, determine how many real daily measurements are recorded (not NA or NaN)
  summarize(
    across(
      .cols = all_of(variables),
      .fns = ~sum(!is.na(.)),
      .names = "{.col}"
    ),
    .groups = "drop"
  ) |> 
  # Create a column for days in the month determined with days_in_month()
  mutate(
    days_in_month = days_in_month(ymd(paste(year, month, "01", sep = "-")))) |> 
  # For each variable, calculate proportion of real daily measurements for each month 
  mutate(
    across(
      .cols = all_of(variables),
      .fns = ~. / days_in_month,
      .names = "{.col}_prop"
    )
  )

# Create a df showing which years are valid,
# based on each month having at least 80% complete daily data AND there being 12 total months
validity_by_year <- A01_CTD_props |> 
  select(year, month, depth, ends_with("_prop")) |> 
  pivot_longer(
    cols = ends_with("_prop"),
    names_to = "variable",
    values_to = "prop"
  ) |> 
  group_by(year, depth, variable) |> 
  summarize(
    n_months = n(),
    all_months_above_80 = all(prop >= 0.8),
    .groups = "drop"
  ) |> 
  mutate(
    status = ifelse(n_months == 12 & all_months_above_80, "valid", "not valid")
  ) |> 
  mutate(variable = sub("_prop$", "", variable))

# Calculate annual means
A01_CTD_annual <- A01_CTD_daily |> 
  group_by(year, depth) |> 
  summarize(
    temperature = mean(temperature, na.rm = TRUE),
    salinity = mean(salinity, na.rm = TRUE),
    .groups = "drop"
  ) |> 
  pivot_longer(
    cols = c(temperature, salinity),
    names_to = "variable",
    values_to = "annual_mean"
  ) |> 
  left_join(validity_by_year, by = c("year", "depth", "variable")) |> 
  # For invalid year-depth-variables, change the calculated mean to NA
  mutate(
    annual_mean = ifelse(status == "valid", annual_mean, NA_real_)
  ) |> 
  select(-c(n_months, all_months_above_80, status)) |> 
  # Pivot back wider for plotting
  pivot_wider(names_from = variable, values_from = annual_mean)


# Plot annual mean data
for (var in variables) {
  
  # Determine y axis range and create vertical padded y_max so annotations will be visible
  y_max <- max(A01_CTD_annual[[var]], na.rm = TRUE)
  y_min <- min(A01_CTD_annual[[var]], na.rm = TRUE)
  range_size = y_max - y_min
  y_max_buffered <- y_max + (range_size * 0.2)
  
  # Plot
  p <- ggplot(A01_CTD_annual,
              aes(x = year,
                  y = .data[[var]])) +
    geom_point(size = 3) +
    geom_smooth(method = "lm", se = FALSE) +
    labs(x = "Date",
         y = variables_meta[[var]],
         title = paste("Annual Time Series - A01 Buoy", variables_meta[[var]], sep = "\n"),
         caption = "(only years in which each month contains at least 80% complete daily data)") +
    scale_color_brewer(palette = "Set2") +
    
    # Add vertical padding
    scale_y_continuous(limits = c(NA, y_max_buffered)) +
    
    # Facet by depth
    facet_wrap(~depth) +
    
    # Annotate plot with simple linear model p-values and R2 values
    stat_poly_eq(
      aes(
        label = after_stat(
          paste0(..p.value.label.., "~~~", ..rr.label..)
        )
      ),
      parse = TRUE,
      color = "blue")
  
  print(p)
}

# Separate temperature into different variables by depth
A01_CTD_annual_wide <- A01_CTD_annual |>
  pivot_wider(
    id_cols = year,
    names_from = depth,
    values_from = c(temperature, salinity)
  ) |> 
  # Rename to add "m" at the end of variable names to indicate units
  rename_with(~ paste0(., "m"), -year) 

# Reestablish variables and variable meta data
variables <- c("temperature_1m", "temperature_20m", "salinity_1m", "salinity_20m")
variables_meta <- list(
  temperature_1m = "Mean Temperature @1m depth (degC)",
  temperature_20m = "Mean Temperature @20m depth (degC)",
  salinity_1m = "Mean Salinity @1m depth (psu)",
  salinity_20m = "Mean Salinity @20m depth (psu)"
)

# Plot annual mean data (again)
for (var in variables) {
  
  # Determine y axis range and create vertical padded y_max so annotations will be visible
  y_max <- max(A01_CTD_annual_wide[[var]], na.rm = TRUE)
  y_min <- min(A01_CTD_annual_wide[[var]], na.rm = TRUE)
  range_size = y_max - y_min
  y_max_buffered <- y_max + (range_size * 0.2)
  
  # Plot
  p <- ggplot(A01_CTD_annual_wide,
              aes(x = year,
                  y = .data[[var]])) +
    geom_point(size = 3) +
    geom_smooth(method = "lm", se = FALSE) +
    labs(x = "Date",
         y = variables_meta[[var]],
         title = paste("Annual Time Series - A01 Buoy", variables_meta[[var]], sep = "\n"),
         caption = "(only years in which each month contains at least 80% complete daily data)") +
    scale_color_brewer(palette = "Set2") +
    
    # Add vertical padding
    scale_y_continuous(limits = c(NA, y_max_buffered)) +
    
    # Annotate plot with simple linear model p-values and R2 values
    stat_poly_eq(
      aes(
        label = after_stat(
          paste0(..p.value.label.., "~~~", ..rr.label..)
        )
      ),
      parse = TRUE,
      color = "blue")
  
  print(p)
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
variables_meta <- list(
  current_speed_mean = "Mean Current Speed (cm/s)",
  current_direction_mean = "Circular Mean Current Direction (degT)",
  temperature_mean = "Mean Temperature @ 2m depth (degC)"
)

for (var in variables) {
  
  # Determine y axis range and create vertical padded y_max so annotations will be visible
  y_max <- max(A01_aanderaa_o2_all_daily_summary[[var]], na.rm = TRUE)
  y_min <- min(A01_aanderaa_o2_all_daily_summary[[var]], na.rm = TRUE)
  range_size = y_max - y_min
  y_max_buffered <- y_max + (range_size * 0.2)
  
  # Plot
  p <- ggplot(A01_aanderaa_o2_all_daily_summary,
              aes(x = date,
                  y = .data[[var]])) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    labs(x = "Date",
         y = variables_meta[[var]],
         title = paste("Daily Time Series - A01 Buoy", variables_meta[[var]], sep = "\n")) +
    scale_color_brewer(palette = "Set2") +
    
    # Add vertical padding
    scale_y_continuous(limits = c(NA, y_max_buffered)) +
    
    # Annotate plot with simple linear model p-values and R2 values
    stat_poly_eq(
      aes(
        label = after_stat(
          paste0(..p.value.label.., "~~~", ..rr.label..)
        )
      ),
      parse = TRUE,
      color = "blue")
  
  print(p)
}


# Annual Means

# Determine valid years (where each month has >= 80% complete data) for each variable
# For each variable, calculate the annual mean only for years valid for that variable

A01_aanderaa_o2_props <- A01_aanderaa_o2_all_daily_summary |> 
  # Group by year and month  
  group_by(year, month) |> 
  # For each variable, determine how many real daily measurements are recorded (not NA or NaN)
  summarize(
    across(
      .cols = all_of(variables),
      .fns = ~sum(!is.na(.)),
      .names = "{.col}"
    ),
    .groups = "drop"
  ) |> 
  # Create a column for days in the month determined with days_in_month()
  mutate(
    days_in_month = days_in_month(ymd(paste(year, month, "01", sep = "-")))) |> 
  # For each variable, calculate proportion of real daily measurements for each month 
  mutate(
    across(
      .cols = all_of(variables),
      .fns = ~. / days_in_month,
      .names = "{.col}_prop"
    )
  )

# Create a df showing which years are valid,
# based on each month having at least 80% complete daily data AND there being 12 total months
validity_by_year <- A01_aanderaa_o2_props |> 
  select(year, month, ends_with("_prop")) |> 
  pivot_longer(
    cols = ends_with("_prop"),
    names_to = "variable",
    values_to = "prop"
  ) |> 
  group_by(year, variable) |> 
  summarize(
    n_months = n(),
    all_months_above_80 = all(prop >= 0.8),
    .groups = "drop"
  ) |> 
  mutate(
    status = ifelse(n_months == 12 & all_months_above_80, "valid", "not valid")
  ) |> 
  mutate(variable = sub("_prop$", "", variable))

# Calculate annual means
A01_aanderaa_o2_all_annual_summary <- A01_aanderaa_o2_all_daily_summary |> 
  group_by(year) |> 
  summarize(
    current_speed_mean = mean(current_speed_mean, na.rm = TRUE),
    temperature_mean = mean(temperature_mean, na.rm = TRUE),
    current_direction_mean =
      # Use circular package to calculate circular mean
      as.numeric(circular::mean.circular(
        circular::circular(current_direction_mean, units = "degrees", modulo = "2pi"),  na.rm = TRUE)),
    .groups = "drop"
  ) |> 
  pivot_longer(
    cols = -year,
    names_to = "variable",
    values_to = "annual_mean"
  ) |> 
  left_join(validity_by_year, by = c("year", "variable")) |> 
  # For invalid year-variables, change the calculated mean to NA
  mutate(
    annual_mean = ifelse(status == "valid", annual_mean, NA_real_)
  ) |> 
  select(-c(n_months, all_months_above_80, status)) |> 
  # Pivot back wider for plotting
  pivot_wider(names_from = variable, values_from = annual_mean)


# Plot annual mean data
for (var in variables) {
  
  # Determine y axis range and create vertical padded y_max so annotations will be visible
  y_max <- max(A01_aanderaa_o2_all_annual_summary[[var]], na.rm = TRUE)
  y_min <- min(A01_aanderaa_o2_all_annual_summary[[var]], na.rm = TRUE)
  range_size = y_max - y_min
  y_max_buffered <- y_max + (range_size * 0.2)
  
  # Plot
  p <- ggplot(A01_aanderaa_o2_all_annual_summary,
              aes(x = year,
                  y = .data[[var]])) +
    geom_point(size = 3) +
    geom_smooth(method = "lm", se = FALSE) +
    labs(x = "Date",
         y = variables_meta[[var]],
         title = paste("Annual Time Series - A01 Buoy", variables_meta[[var]], sep = "\n"),
         caption = "(only years in which each month contains at least 80% complete daily data)") +
    scale_color_brewer(palette = "Set2") +
    
    # Add vertical padding
    scale_y_continuous(limits = c(NA, y_max_buffered)) +
    
    # Annotate plot with simple linear model p-values and R2 values
    stat_poly_eq(
      aes(
        label = after_stat(
          paste0(..p.value.label.., "~~~", ..rr.label..)
        )
      ),
      parse = TRUE,
      color = "blue")
  
  print(p)
}

# Make compass vector plot using custom function
make_compass_plot(A01_aanderaa_o2_all_annual_summary,
                  current_direction_mean,
                  current_speed_mean,
                  year,
                  units_dir = "Direction (degT)",
                  units_speed = "Current Speed (cm/s)",
                  medium_type = "Current")

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

# # Examine data recording time intervals
# intervals <- A01_met_all |> 
#   select(time) |> 
#   mutate(
#     time = ymd_hms(time),
#     time_lagged = lag(time),
#     time_difference_minutes = as.numeric(difftime(time, time_lagged, units = "mins"))
#     ) |> 
#   group_by(time_difference_minutes) |> 
#   summarize(count = n())

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

# Update list of variables
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
    # also calculate a simple, non-circular mean for wind direction, for comparison to DKP analysis
    wind_direction_mean_simple = mean(wind_direction),
    .groups = "drop"
    )


# Plot daily mean values for each variable
scalar_variable_means <- c(paste0(scalar_variables, "_mean"), "wind_direction_mean_simple")
angular_variable_means <- paste0(angular_variables, "_mean")
variable_means <- c(paste0(variables, "_mean"), "wind_direction_mean_simple")
variable_means_meta <- list(
  air_temperature_mean = "Mean Air Temperature (degC)",
  barometric_pressure_mean = "Mean Barometric Pressure (millibars)",
  wind_gust_mean = "Mean Wind Gust Speed (m/s)",
  wind_speed_mean = "Mean Wind Speed (m/s)",
  visibility_mean = "Mean Visibility (meters)",
  wind_direction_mean = "Circular Mean Wind Direction (degT)",
  wind_direction_mean_simple = "Simple Mean Wind Direction (angular degrees)"
)


for (var in variable_means) {
  
  # Determine y axis range and create vertical padded y_max so annotations will be visible
  y_max <- max(A01_met_all_daily_summary[[var]], na.rm = TRUE)
  y_min <- min(A01_met_all_daily_summary[[var]], na.rm = TRUE)
  range_size = y_max - y_min
  y_max_buffered <- y_max + (range_size * 0.2)
  
  # Plot
  p <- ggplot(A01_met_all_daily_summary,
              aes(x = date,
                  y = .data[[var]])) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    labs(x = "Date",
         y = variable_means_meta[[var]],
         title = paste("Daily Time Series - A01 Buoy", variable_means_meta[[var]], sep = "\n"),
         caption = "NOTE - Did NOT check that data was full/complete for each day.
         Need to determine the best approach for this, because days have different intervals of collection") +
    scale_color_brewer(palette = "Set2") + 
    
    # Add vertical padding
    scale_y_continuous(limits = c(NA, y_max_buffered)) +
    
    # Annotate plot with simple linear model p-values and R2 values
    stat_poly_eq(
      aes(
        label = after_stat(
          paste0(..p.value.label.., "~~~", ..rr.label..)
        )
      ),
      parse = TRUE,
      color = "blue")
  
  print(p)
}

# Annual Means

# Determine valid years (where each month has >= 80% complete data) for each variable
# For each variable, calculate the annual mean only for years valid for that variable

A01_met_props <- A01_met_all_daily_summary |> 
  # Group by year and month  
  group_by(year, month) |> 
  # For each variable, determine how many real daily measurements are recorded (not NA or NaN)
  summarize(
    across(
      .cols = all_of(variable_means),
      .fns = ~sum(!is.na(.)),
      .names = "{.col}"
    ),
    .groups = "drop"
  ) |> 
  # Create a column for days in the month determined with days_in_month()
  mutate(
    days_in_month = days_in_month(ymd(paste(year, month, "01", sep = "-")))) |> 
  # For each variable, calculate proportion of real daily measurements for each month 
  mutate(
    across(
      .cols = all_of(variable_means),
      .fns = ~. / days_in_month,
      .names = "{.col}_prop"
    )
  )

# Create a df showing which years are valid,
# based on each month having at least 80% complete daily data AND there being 12 total months
validity_by_year <- A01_met_props |> 
  select(year, month, ends_with("_prop")) |> 
  pivot_longer(
    cols = ends_with("_prop"),
    names_to = "variable",
    values_to = "prop"
  ) |> 
  group_by(year, variable) |> 
  summarize(
    n_months = n(),
    all_months_above_80 = all(prop >= 0.8),
    .groups = "drop"
  ) |> 
  mutate(
    status = ifelse(n_months == 12 & all_months_above_80, "valid", "not valid")
  ) |> 
  mutate(variable = sub("_prop$", "", variable))

# Calculate annual means
A01_met_all_annual_summary <- A01_met_all_daily_summary |> 
  group_by(year) |> 
  summarize(
    across(
      all_of(scalar_variable_means),
      ~ mean(.x, na.rm = TRUE)
    ),
    across(
      all_of(angular_variable_means),
      ~ as.numeric(
        circular::mean.circular(
          circular::circular(.x, units = "degrees", modulo = "2pi"),
          na.rm = TRUE
        )
      )
    ),
    .groups = "drop"
  ) |> 
  pivot_longer(
    cols = -year,
    names_to = "variable",
    values_to = "annual_mean"
  ) |> 
  left_join(validity_by_year, by = c("year", "variable")) |> 
  # For invalid year-variables, change the calculated mean to NA
  mutate(
    annual_mean = ifelse(status == "valid", annual_mean, NA_real_)
  ) |> 
  select(-c(n_months, all_months_above_80, status)) |> 
  # Pivot back wider for plotting
  pivot_wider(names_from = variable, values_from = annual_mean)


# Plot annual means
for (var in variable_means) {
  
  # Determine y axis range and create vertical padded y_max so annotations will be visible
  y_max <- max(A01_met_all_annual_summary[[var]], na.rm = TRUE)
  y_min <- min(A01_met_all_annual_summary[[var]], na.rm = TRUE)
  range_size = y_max - y_min
  y_max_buffered <- y_max + (range_size * 0.2)
  
  # Plot
  p <- ggplot(A01_met_all_annual_summary,
              aes(x = year,
                  y = .data[[var]])) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    labs(x = "Date",
         y = variable_means_meta[[var]],
         title = paste("Annual Time Series - A01 Buoy", variable_means_meta[[var]], sep = "\n"),
         caption = "(only years with at least 80% complete data included)") +
    scale_color_brewer(palette = "Set2") + 
    
    # Add vertical padding
    scale_y_continuous(limits = c(NA, y_max_buffered)) +
    
    # Annotate plot with simple linear model p-values and R2 values
    stat_poly_eq(
      aes(
        label = after_stat(
          paste0(..p.value.label.., "~~~", ..rr.label..)
        )
      ),
      parse = TRUE,
      color = "blue")
  
  print(p)
}


# Plot simple mean wind direction for 2003 - 2014 for comparison with DKP
for (var in list("wind_direction_mean_simple")) {
  
  # Determine y axis range and create vertical padded y_max so annotations will be visible
  y_max <- max(A01_met_all_annual_summary[[var]], na.rm = TRUE)
  y_min <- min(A01_met_all_annual_summary[[var]], na.rm = TRUE)
  range_size = y_max - y_min
  y_max_buffered <- y_max + (range_size * 0.2)
  
  p <- ggplot(A01_met_all_annual_summary,
              aes(x = year,
                  y = .data[[var]])) +
    geom_point(color = "orange") +
    geom_line(data = filter(A01_met_all_annual_summary, !is.na(wind_direction_mean_simple)),
              color = "orange") +
    geom_smooth(method = "lm", se = FALSE) +
    labs(x = "Date",
         y = variable_means_meta[[var]],
         title = paste("Annual Time Series - A01 Buoy", variable_means_meta[[var]], sep = "\n"),
         caption = "(only years with at least 80% complete data included)") +
    scale_color_brewer(palette = "Set2") +
    
    # Add vertical padding
    scale_y_continuous(limits = c(NA, y_max_buffered)) +
    
    # Annotate plot with simple linear model p-values and R2 values
    stat_poly_eq(
      aes(
        label = after_stat(
          paste0(..p.value.label.., "~~~", ..rr.label..)
        )
      ),
      parse = TRUE,
      color = "blue")
  
  print(p)
}

# Make compass vector plot using custom function
make_compass_plot(A01_met_all_annual_summary,
                  wind_direction_mean,
                  wind_speed_mean,
                  year,
                  units_dir = "Direction (degT)",
                  units_speed = "Wind Speed (m/s)",
                  medium_type = "Wind")

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
variables_meta <- list(
  current_speed_mean = "Mean Current Speed (cm/s)",
  current_direction_mean = "Circular Mean Current Direction (degT)",
  temperature_mean = "Mean Temperature @ 2m depth (degC)"
)

for (var in variables) {
  
  # Determine y axis range and create vertical padded y_max so annotations will be visible
  y_max <- max(A01_aanderaa_hist_daily[[var]], na.rm = TRUE)
  y_min <- min(A01_aanderaa_hist_daily[[var]], na.rm = TRUE)
  range_size = y_max - y_min
  y_max_buffered <- y_max + (range_size * 0.2)
  
  # Plot
  p <- ggplot(A01_aanderaa_hist_daily,
              aes(x = date,
                  y = .data[[var]])) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    
    labs(x = "Date",
         y = variables_meta[[var]],
         title = paste("Daily Time Series - A01 Buoy", variables_meta[[var]], sep = "\n")) +
    scale_color_brewer(palette = "Set2") + 
    
    # Add vertical padding
    scale_y_continuous(limits = c(NA, y_max_buffered)) +
    
    # Annotate plot with simple linear model p-values and R2 values
    stat_poly_eq(
      aes(
        label = after_stat(
          paste0(..p.value.label.., "~~~", ..rr.label..)
        )
      ),
      parse = TRUE,
      color = "blue")
  
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
    across(c(significant_wave_height, dominant_wave_period), as.numeric),
    
    # Convert non-"good" quality data to NAs. qc flags of 0 are considered "good"
    significant_wave_height = ifelse(significant_wave_height_qc != 0, NA, significant_wave_height),
    dominant_wave_period = ifelse(dominant_wave_period_qc != 0, NA, dominant_wave_period)
  ) |> 
  
  # Remove QC flag columns and move ymd columns to start
  select(-significant_wave_height_qc, -dominant_wave_period_qc) |> 
  relocate(date, year, month, day, .before = time)


# Calculate daily mean for all variables at each depth
A01_wave_acc_daily <- A01_wave_acc |> 
  group_by(date, year, month, day) |> 
  summarize(
    significant_wave_height = mean(significant_wave_height, na.rm = TRUE),
    dominant_wave_period = mean(dominant_wave_period, na.rm = TRUE),
    .groups = "drop"
  )

# Plot daily mean values for each variable
variables <- c("significant_wave_height", "dominant_wave_period")
variables_meta <- list(
  significant_wave_height = "Mean Significant Wave Height (m)",
  dominant_wave_period = "Mean Dominant Waver Period (s)"
)

for (var in variables) {
  
  # Determine y axis range and create vertical padded y_max so annotations will be visible
  y_max <- max(A01_wave_acc_daily[[var]], na.rm = TRUE)
  y_min <- min(A01_wave_acc_daily[[var]], na.rm = TRUE)
  range_size = y_max - y_min
  y_max_buffered <- y_max + (range_size * 0.2)
  
  # Plot
  p <- ggplot(A01_wave_acc_daily,
              aes(x = date,
                  y = .data[[var]])) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    labs(x = "Date",
         y = variables_meta[[var]],
         title = paste("Daily Time Series - A01 Buoy", variables_meta[[var]], sep = "\n")) +
    scale_color_brewer(palette = "Set2") +
    
    # Add vertical padding
    scale_y_continuous(limits = c(NA, y_max_buffered)) +
    
    # Annotate plot with simple linear model p-values and R2 values
    stat_poly_eq(
      aes(
        label = after_stat(
          paste0(..p.value.label.., "~~~", ..rr.label..)
        )
      ),
      parse = TRUE,
      color = "blue")
  
  print(p)
}


# Annual Means

# Determine valid years (where each month has >= 80% complete data) for each variable
# For each variable, calculate the annual mean only for years valid for that variable

A01_wave_acc_props <- A01_wave_acc_daily |> 
  # Group by year and month
  group_by(year, month) |> 
  # For each variable, determine how many real daily measurements are recorded (not NA or NaN)
  summarize(
    across(
      .cols = all_of(variables),
      .fns = ~sum(!is.na(.)),
      .names = "{.col}"
    ),
    .groups = "drop"
  ) |> 
  # Create a column for days in the month determined with days_in_month()
  mutate(
    days_in_month = days_in_month(ymd(paste(year, month, "01", sep = "-")))) |> 
  # For each variable, calculate proportion of real daily measurements for each month 
  mutate(
    across(
      .cols = all_of(variables),
      .fns = ~. / days_in_month,
      .names = "{.col}_prop"
    )
  )

# Create a df showing which years are valid,
# based on each month having at least 80% complete daily data AND there being 12 total months
validity_by_year <- A01_wave_acc_props |> 
  select(year, month, ends_with("_prop")) |> 
  pivot_longer(
    cols = ends_with("_prop"),
    names_to = "variable",
    values_to = "prop"
  ) |> 
  group_by(year, variable) |> 
  summarize(
    n_months = n(),
    all_months_above_80 = all(prop >= 0.8),
    .groups = "drop"
  ) |> 
  mutate(
    status = ifelse(n_months == 12 & all_months_above_80, "valid", "not valid")
  ) |> 
  mutate(variable = sub("_prop$", "", variable))

# Calculate annual means
A01_wave_acc_annual <- A01_wave_acc_daily |> 
  group_by(year) |> 
  summarize(
    significant_wave_height = mean(significant_wave_height, na.rm = TRUE),
    dominant_wave_period = mean(dominant_wave_period, na.rm = TRUE),
    .groups = "drop"
  ) |> 
  pivot_longer(
    cols = c(significant_wave_height, dominant_wave_period),
    names_to = "variable",
    values_to = "annual_mean"
  ) |> 
  left_join(validity_by_year, by = c("year", "variable")) |> 
  # For invalid year-variables, change the calculated mean to NA
  mutate(
    annual_mean = ifelse(status == "valid", annual_mean, NA_real_)
  ) |> 
  select(-c(n_months, all_months_above_80, status)) |> 
  # Pivot back wider for plotting
  pivot_wider(names_from = variable, values_from = annual_mean)


# Plot annual mean data
for (var in variables) {
  
  # Determine y axis range and create vertical padded y_max so annotations will be visible
  y_max <- max(A01_wave_acc_annual[[var]], na.rm = TRUE)
  y_min <- min(A01_wave_acc_annual[[var]], na.rm = TRUE)
  range_size = y_max - y_min
  y_max_buffered <- y_max + (range_size * 0.2)
  
  # Plot
  p <- ggplot(A01_wave_acc_annual,
              aes(x = year,
                  y = .data[[var]])) +
    geom_point(size = 3) +
    geom_smooth(method = "lm", se = FALSE) +
    labs(x = "Date",
         y = variables_meta[[var]],
         title = paste("Annual Time Series - A01 Buoy", variables_meta[[var]], sep = "\n"),
         caption = "(only years in which each month contains at least 80% complete daily data)") +
    scale_color_brewer(palette = "Set2") +
    
    # Add vertical padding
    scale_y_continuous(limits = c(NA, y_max_buffered)) +
    
    # Annotate plot with simple linear model p-values and R2 values
    stat_poly_eq(
      aes(
        label = after_stat(
          paste0(..p.value.label.., "~~~", ..rr.label..)
        )
      ),
      parse = TRUE,
      color = "blue")
  
  print(p)
}


# ---- Export Annual Data ----
# Join all annual summary data frames by year
A01_buoy_annual <- 
  purrr::reduce(
    list(A01_CTD_annual_wide,
         A01_met_all_annual_summary,
         A01_aanderaa_o2_all_annual_summary,
         A01_wave_acc_annual),
    full_join, by = "year") |> 
  arrange(year)
  

# Write annual summary data to csv
# write_csv(A01_buoy_annual, here::here("data", "summary_data", "buoy_a01_annual.csv"))
