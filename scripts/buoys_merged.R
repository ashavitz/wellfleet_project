# ---- Header ----
#'--------------------------------------
#' Data from Massachusetts Bay, Boston Harbor, Cape Cod Bay
#'--------------------------------------

# ---- TODO ----

#'--------------------------------------

# ---- Load Libraries ----
library(dplyr) # for data manipulation and transformation
library(ggplot2) # for visualization
library(ggpmisc) # for annotating plots with p & R2 of fitted polynomial via stat_poly_eq()
library(lubridate) # for date time formats
library(readr) # for reading in files
library(rerddap) # for accessing ERDDAP servers
library(tidyr) # for tidying and reshaping data

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
  select(year, air_temperature_mean, temperature_1m, temperature_20m,
         wind_direction_mean_simple, wind_direction_mean, wind_speed_mean) |> 
  rename(air_temp_mean = air_temperature_mean,
         sst_mean = temperature_1m,
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
               "temperature_20m",
               "wind_direction_simple_mean", 
               "wind_direction_mean",
               "wind_speed_mean")

variables_meta <- list(
  air_temp_mean = "Air temperature (degC)", 
  sst_mean = "Sea surface temperature (degC)",
  temperature_20m = "Water Temperature @20m (degC)",
  wind_direction_simple_mean = "Simple Mean Wind Direction (degT)",
  wind_direction_mean = "Mean Wind Direction, circular (degT)", 
  wind_speed_mean = "Wind speed (m/s)")


# Plot all variables over time, color by buoy
for (var in variables) {
  
  # For each variable, only include a buoy in the plot (and thus legend) 
  # if it has at least 2 real (non-NA) measurements (2, so a line can be drawn)
  plot_data <- buoy_data_annual |>
    group_by(buoy) |>
    filter(sum(!is.na(.data[[var]])) >= 2) |>
    ungroup()
  
  # Determine y axis range and create vertical padded y_max so annotations will be visible
  y_max <- max(plot_data[[var]], na.rm = TRUE)
  y_min <- min(plot_data[[var]], na.rm = TRUE)
  range_size = y_max - y_min
  y_max_buffered <- y_max + (range_size * 0.2)
  
  # Plot
  p <- ggplot(plot_data,
              aes(x = year,
                  y = .data[[var]],
                  color = buoy)) +
    geom_point() +
    geom_smooth(method = "lm", se = TRUE, alpha = 0.1) +
    labs(x = "Date",
         y = variables_meta[[var]],
         color = "Buoy",
         title = paste("Annual Time Series",
                       variables_meta[[var]], sep = "\n")) +
    scale_color_manual(
      values = c("A01" = "orange", "NDBC 44013" = "blue", "221" = "green3")
    ) +
    scale_x_continuous(
      breaks = seq(min(plot_data$year), max(plot_data$year), by = 2)) +
    
    # Add vertical padding
    scale_y_continuous(limits = c(NA, y_max_buffered)) +
    
    # Annotate plot with simple linear model p-values and R2 values
    stat_poly_eq(
      aes(group = buoy, 
          color = buoy,
          label = after_stat(
            paste0(
                   ..p.value.label.., "~~~",
                   ..rr.label..
            ))),
      # formula = y ~ x,
      parse = TRUE,
      size = 3, 
      label.x = "left",
      label.y = "top",
      vstep = 0.025
    )
  
  print(p)
}


# Plot line graphs
for (var in variables) {
  
  # For each variable, only include a buoy in the plot (and thus legend) 
  # if it has at least 2 real (non-NA) measurements (2, so a line can be drawn)
  plot_data <- buoy_data_annual |>
    group_by(buoy) |>
    filter(sum(!is.na(.data[[var]])) >= 2) |>
    ungroup()
  
  # Determine y axis range and create vertical padded y_max so annotations will be visible
  y_max <- max(plot_data[[var]], na.rm = TRUE)
  y_min <- min(plot_data[[var]], na.rm = TRUE)
  range_size = y_max - y_min
  y_max_buffered <- y_max + (range_size * 0.2)
  
  # Plot
  p <- ggplot(plot_data,
              aes(x = year,
                  y = .data[[var]],
                  color = buoy)) +
    geom_point() +
    geom_line(data = filter(plot_data, !is.na(.data[[var]]))) +
    geom_smooth(method = "lm", se = TRUE, alpha = 0.1, linetype = "dotted") +
    labs(x = "Date",
         y = variables_meta[[var]],
         title = paste("Annual Time Series",
                       variables_meta[[var]], sep = "\n")) +
    scale_color_manual(
      values = c("A01" = "orange", "NDBC 44013" = "blue", "221" = "green3")
    ) +
    scale_x_continuous(
      breaks = seq(min(plot_data$year), max(plot_data$year), by = 2)) +
    
    # Add vertical padding
    scale_y_continuous(limits = c(NA, y_max_buffered)) +
    
    # Annotate plot with simple linear model p-values and R2 values
    stat_poly_eq(
      aes(group = buoy, 
          color = buoy,
          label = after_stat(
            paste0(
              ..p.value.label.., "~~~",
              ..rr.label..
            ))),
      # formula = y ~ x,
      parse = TRUE,
      size = 3, 
      label.x = "left",
      label.y = "top",
      vstep = 0.025
    )
  
  print(p)
}


# Plot line graphs for wind direction
for (var in c("wind_direction_mean", "wind_direction_simple_mean")) {
  
  # Determine y axis range and create vertical padded y_max so annotations will be visible
  y_max <- max(buoy_data_annual[[var]], na.rm = TRUE)
  y_min <- min(buoy_data_annual[[var]], na.rm = TRUE)
  range_size = y_max - y_min
  y_max_buffered <- y_max + (range_size * 0.2)
  
  # Plot
  p <- ggplot(buoy_data_annual |> filter(year >= 2003, year <= 2014),
              aes(x = year,
                  y = .data[[var]],
                  color = buoy)) +
    geom_point() +
    geom_line(data = filter(buoy_data_annual, !is.na(.data[[var]]) & year %in% c(2003:2014))) +
    geom_smooth(method = "lm", se = TRUE, alpha = 0.1, linetype = "dotted") +
    labs(x = "Date",
         y = variables_meta[[var]],
         title = paste("Annual Time Series",
                       variables_meta[[var]], sep = "\n")) +
    scale_color_manual(
      values = c("A01" = "orange", "NDBC 44013" = "blue", "221" = "green3")
    ) +
    scale_x_continuous(
      breaks = seq(min(buoy_data_annual$year), max(buoy_data_annual$year), by = 2)) +
    
    # Add vertical padding
    scale_y_continuous(limits = c(NA, y_max_buffered)) +
    
    # Annotate plot with simple linear model p-values and R2 values
    stat_poly_eq(
      aes(group = buoy, 
          color = buoy,
          label = after_stat(
            paste0(
              ..p.value.label.., "~~~",
              ..rr.label..
            ))),
      # formula = y ~ x,
      parse = TRUE,
      size = 3, 
      label.x = "left",
      label.y = "top",
      vstep = 0.025
    )
  
  print(p)
}

