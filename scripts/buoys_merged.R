# ---- Header ----
#'--------------------------------------
#' Data from Massachusetts Bay, Boston Harbor, Cape Cod Bay
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


# ---- Load and Visualize Buoy Data ----

# Load data for each buoy
buoy_221_annual <- read_csv("data/summary_data/buoy_221_annual.csv")
buoy_a01_annual <- read_csv("data/summary_data/buoy_a01_annual.csv")
buoy_44013_annual <- read_csv("data/summary_data/buoy_44013_annual.csv")
  
  
# Standardize headers and units to prepare to bind rows
buoy_221_annual <- buoy_221_annual |> 
  select(year, mean_air_temp, mean_sst) |> 
  rename(air_temp_mean = mean_air_temp,
         sst_mean = mean_sst) |> 
  mutate(buoy = "221")

buoy_a01_annual <- buoy_a01_annual |> 
  select(year, air_temperature_mean, temperature_mean,
         wind_direction_mean_simple, wind_direction_mean, wind_speed_mean) |> 
  rename(air_temp_mean = air_temperature_mean,
         sst_mean = temperature_mean,
         wind_direction_simple_mean = wind_direction_mean_simple) |> 
  mutate(buoy = "A01")

buoy_44013_annual <- buoy_44013_annual |> 
  select(YYYY, ATMP, WTMP, WDIR_simple, WDIR, WSPD) |> 
  rename(year = YYYY, 
         air_temp_mean = ATMP, 
         sst_mean = WTMP,
         wind_direction_simple_mean = WDIR_simple,
         wind_direction_mean = WDIR, 
         wind_speed_mean = WSPD) |> 
  mutate(buoy = "NDBC 44013")


# Details on Variables:
# air_temp_mean = Air Temperature (degC)
# sst_mean = Sea surface Temperature (degC)
# wind_direction_simple_mean = Wind Direction (degT) calculated as simple mean
# wind_direction_mean = Wind Direction (degT) calculated as circular mean
# wind_speed_mean = Wind Speed(m/s)
  
  
# Bind rows to merge data frames into one
buoy_data_annual <- bind_rows(buoy_221_annual, buoy_44013_annual, buoy_a01_annual)

# Store variables of and variable metadata
variables <- c("air_temp_mean",
               "sst_mean",
               "wind_direction_simple_mean", 
               "wind_direction_mean",
               "wind_speed_mean")

variables_meta <- list(
  air_temp_mean = "Air temperature (degC)", 
  sst_mean = "Sea surface temperature (degC)",
  wind_direction_simple_mean = "Simple Mean Wind Direction (degT)",
  wind_direction_mean = "Circular Mean Wind Direction (degT)", 
  wind_speed_mean = "Wind speed (m/s)")

  
# Plot all variables over time, color by buoy
for (var in variables) {
  p <- ggplot(buoy_data_annual,
              aes(x = year,
                  y = .data[[var]],
                  color = buoy)) +
    geom_point() +
    geom_smooth(method = "lm", se = TRUE, alpha = 0.1) +
    labs(x = "Date",
         y = variables_meta[[var]],
         title = paste("Annual Time Series",
                       variables_meta[[var]], sep = "\n")) +
    scale_color_brewer(palette = "Set2") +
    scale_x_continuous(
      breaks = seq(min(buoy_data_annual$year), max(buoy_data_annual$year), by = 2))
  
  print(p)
}


# Plot line graphs
for (var in variables) {
  p <- ggplot(buoy_data_annual,
              aes(x = year,
                  y = .data[[var]],
                  color = buoy)) +
    geom_point() +
    geom_line() +
    geom_smooth(method = "lm", se = TRUE, alpha = 0.1, linetype = "dotted") +
    labs(x = "Date",
         y = variables_meta[[var]],
         title = paste("Annual Time Series",
                       variables_meta[[var]], sep = "\n")) +
    scale_color_manual(
      values = c("A01" = "orange", "NDBC 44013" = "blue", "221" = "green")
    ) +
    scale_x_continuous(
      breaks = seq(min(buoy_data_annual$year), max(buoy_data_annual$year), by = 2))
  
  print(p)
}


# Plot line graphs for wind direction
for (var in c("wind_direction_mean", "wind_direction_simple_mean")) {
  p <- ggplot(buoy_data_annual |> filter(year >= 2003, year <= 2014),
              aes(x = year,
                  y = .data[[var]],
                  color = buoy)) +
    geom_point() +
    geom_line() +
    geom_smooth(method = "lm", se = TRUE, alpha = 0.1, linetype = "dotted") +
    labs(x = "Date",
         y = variables_meta[[var]],
         title = paste("Annual Time Series",
                       variables_meta[[var]], sep = "\n")) +
    scale_color_manual(
      values = c("A01" = "orange", "NDBC 44013" = "blue", "221" = "green")
    ) +
    scale_x_continuous(
      breaks = seq(min(buoy_data_annual$year), max(buoy_data_annual$year), by = 2))
  
  print(p)
}

