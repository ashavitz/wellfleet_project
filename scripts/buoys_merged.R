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
library(paletteer) # for color palettes
library(patchwork) # for making joint plots
library(readr) # for reading in files
library(rerddap) # for accessing ERDDAP servers
library(tidyr) # for tidying and reshaping data


# ---- Load Buoy Data ----

# Load data for each buoy
buoy_221_annual <- read_csv("data/summary_data/buoy_221_annual.csv")
buoy_a01_annual <- read_csv("data/summary_data/buoy_a01_annual.csv")
buoy_44013_annual <- read_csv("data/summary_data/buoy_44013_annual.csv")
  
  
# Standardize headers and units to prepare to bind rows
buoy_221_annual <- buoy_221_annual |> 
  select(year, mean_air_temp, mean_sst, mean_wave_hs, mean_wave_tp, mean_wave_ta) |> 
  rename(air_temp_mean = mean_air_temp,
         sst_mean = mean_sst,
         sig_wave_height_mean = mean_wave_hs, 
         dominant_wave_period_mean = mean_wave_tp,
         avg_wave_period_mean = mean_wave_ta) |> 
  mutate(buoy = "CDIP 221",
         location = "Cape Cod Bay")

buoy_a01_annual <- buoy_a01_annual |> 
  select(year, air_temperature_mean, temperature_1m, temperature_20m,
         wind_direction_mean_simple, wind_direction_mean, wind_speed_mean, significant_wave_height, dominant_wave_period) |> 
  rename(air_temp_mean = air_temperature_mean,
         sst_mean = temperature_1m,
         wind_direction_simple_mean = wind_direction_mean_simple,
         sig_wave_height_mean = significant_wave_height,
         dominant_wave_period_mean = dominant_wave_period) |> 
  mutate(buoy = "NERACOOS A01",
         location = "Massachusetts Bay")

buoy_44013_annual <- buoy_44013_annual |> 
  select(YYYY, ATMP, WTMP, WDIR_simple, WDIR, WSPD, WVHT, DPD, APD) |> 
  rename(year = YYYY, 
         air_temp_mean = ATMP, 
         sst_mean = WTMP,
         wind_direction_simple_mean = WDIR_simple,
         wind_direction_mean = WDIR, 
         wind_speed_mean = WSPD,
         sig_wave_height_mean = WVHT,
         dominant_wave_period_mean = DPD,
         avg_wave_period_mean = APD) |> 
  mutate(buoy = "NOAA 44013",
         location = "Outer Boston Harbor")


# Details on Variables:
# air_temp_mean = Air Temperature (degC)
# sst_mean = Sea surface Temperature (degC)
# wind_direction_simple_mean = Wind Direction (degT) calculated as simple mean
# wind_direction_mean = Wind Direction (degT) calculated as circular mean
# wind_speed_mean = Wind Speed(m/s)
  
  
# Bind rows to merge data frames into one
buoy_data_annual <- bind_rows(buoy_221_annual, buoy_44013_annual, buoy_a01_annual) |> 
  relocate(c(buoy, location), .after = year)

# Store variables of and variable metadata
variables <- c("air_temp_mean",
               "sst_mean",
               "temperature_20m",
               "wind_direction_simple_mean", 
               "wind_direction_mean",
               "wind_speed_mean",
               "sig_wave_height_mean",
               "dominant_wave_period_mean",
               "avg_wave_period_mean")

variables_meta <- list(
  air_temp_mean = "Air temperature (°C)", 
  sst_mean = "Sea surface temperature (°C)",
  temperature_20m = "Water Temperature @20m (°C)",
  wind_direction_simple_mean = "Simple Mean Wind Direction (°T)",
  wind_direction_mean = "Mean Wind Direction, circular (°T)", 
  wind_speed_mean = "Wind speed (m/s)",
  sig_wave_height_mean = "Mean Significant Wave Height (m)",
  dominant_wave_period_mean = "Mean Dominant Wave Period (s)",
  avg_wave_period_mean = "Mean Average Wave Period (s)")


# ---- Set Global ggplot Themes and Plotting Objects ----

# Set ggplot theme to minimal, rotate x axis labels, and center plot titles
theme_set(
  theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5)
    )
)

# Establish colors to use for plotting
# NOte: manually applying color palette from yarrr::google
buoy_colors <- c(
  "Massachusetts Bay" = "#F9B90AFF",
  "Outer Boston Harbor" = "#3D79F3FF",
  "Cape Cod Bay" = "#E6352FFF"
)

# Wrapped display labels for the legend (keeps color mapping intact)
buoy_labels <- c(
  "Cape Cod Bay"        = "Cape Cod Bay",
  "Massachusetts Bay"   = "Massachusetts\nBay",
  "Outer Boston Harbor" = "Outer Boston\nHarbor"
)

scale_color_buoy <- function(...) {
  scale_color_manual(values = buoy_colors, labels = buoy_labels, ...)
}

# Annotate plot with simple linear model p-values and R2 values
# Since parse = TRUE, this will read the annotation as plotmath format
# So to include site labels, shQuote(levels(factor(...))) ensures the
# correct station ID is passed to the annotation as a quote, which won't be parsed
# `data` is captured in the function environment so that factor levels are derived
# from the actual (possibly filtered) plot data, keeping labels aligned with colors.
make_stat_annotation <- function(data, size = 3, vstep = 0.04) {
  # Pre-compute factor levels from the actual plot data and inline them into the
  # expression via bquote() + !! so they are resolved at function call time,
  # avoiding shadowing of 'data' by ggplot2's internal evaluation context.
  loc_levels <- levels(factor(data$location))
  label_expr <- bquote(paste0(
    shQuote(.(loc_levels)[as.integer(grp.label)]),
    ":", "~~~",
    ..p.value.label.., "~~~",
    ..rr.label..
  ))
  stat_poly_eq(
    aes(label = after_stat(!!label_expr)),
    parse = TRUE,
    size = size,
    label.x = "left",
    label.y = "top",
    vstep = vstep
  )
}

# ---- Visualize Buoy Data ----

# Plot all variables over time, color by buoy
# Scatterplots with linear lines of best fit:
for (var in variables) {
  
  # For each variable, only include a buoy in the plot (and thus legend) 
  # if it has at least 2 real (non-NA) measurements (2, so a line can be drawn)
  plot_data <- buoy_data_annual |>
    group_by(location) |>
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
                  color = location)) +
    geom_point() +
    geom_smooth(method = "lm", se = TRUE, alpha = 0.1) +
    labs(
      x = "",
      y = variables_meta[[var]],
      color = "Buoy",
      # title = paste("Annual Time Series",
      #               variables_meta[[var]], sep = "\n"),
      # caption = "(only years in which each month contains at least 80% complete daily data)"
      ) +
    scale_color_buoy() +
    scale_x_continuous(
      breaks = seq(min(plot_data$year), max(plot_data$year), by = 2)) +
    
    # Add vertical padding
    scale_y_continuous(limits = c(NA, y_max_buffered)) +
    
    # Annotate plot with simple linear model p-values and R2 values
    make_stat_annotation(plot_data)
  
  print(p)
}


# Plot all variables over time, color by buoy
# Line graphs overlaid by linear lines of best fit:
for (var in variables) {
  
  # For each variable, only include a buoy in the plot (and thus legend) 
  # if it has at least 2 real (non-NA) measurements (2, so a line can be drawn)
  plot_data <- buoy_data_annual |>
    group_by(location) |>
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
                  color = location)) +
    geom_point() +
    geom_line(data = filter(plot_data, !is.na(.data[[var]]))) +
    geom_smooth(method = "lm", se = TRUE, alpha = 0.1, linetype = "dotted") +
    labs(
      x = "",
      y = variables_meta[[var]],
      color = "Buoy",
      # title = paste("Annual Time Series",
      #               variables_meta[[var]], sep = "\n"),
      # caption = "(only years in which each month contains at least 80% complete daily data)"
      ) +
    scale_color_buoy() +
    scale_x_continuous(
      breaks = seq(min(plot_data$year), max(plot_data$year), by = 2)) +
    
    # Add vertical padding
    scale_y_continuous(limits = c(NA, y_max_buffered)) +
    
    # Annotate plot with simple linear model p-values and R2 values
    make_stat_annotation(plot_data)
  
  print(p)
}


# Plot line graphs for wind direction up to 2014 (for comparison with previous reports)
for (var in c("wind_direction_mean", "wind_direction_simple_mean")) {
  
  plot_data <- buoy_data_annual |>
    filter(year >= 2003, year <= 2014) |>
    group_by(location) |>
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
                  color = location)) +
    geom_point() +
    geom_line(data = filter(plot_data, !is.na(.data[[var]]))) +
    geom_smooth(method = "lm", se = TRUE, alpha = 0.1, linetype = "dotted") +
    labs(x = "Date",
         y = variables_meta[[var]],
         title = paste("Annual Time Series",
                       variables_meta[[var]], sep = "\n"),
         caption = "(only years in which each month contains at least 80% complete daily data)"
    ) +
    scale_color_buoy() +
    scale_x_continuous(
      breaks = seq(min(plot_data$year), max(plot_data$year), by = 2)) +
    
    # Add vertical padding
    scale_y_continuous(limits = c(NA, y_max_buffered)) +
    
    # Annotate plot with simple linear model p-values and R2 values
    make_stat_annotation(plot_data)
  
  print(p)
}


### ---- Plots for Final Report ----

# Establish themes and layers for plotting
theme_set(
  theme_minimal() +
    theme(
      axis.text.x  = element_text(size = 12, angle = 45, hjust = 1),
      axis.text.y  = element_text(size = 12),
      legend.title = element_text(size = 16),
      legend.text  = element_text(size = 12)
    )
)

# make_stat_annotation() is defined above; use size = 4 and vstep = 0.05 for final report plots


# Plot time series of mean annual air temperature

# Prepare data for plotting
# Only include a buoy in the plot (and thus legend) if it has at least 2 real 
# (non-NA) measurements (2, so a line can be drawn)
plot_data <- buoy_data_annual |>
  filter(!is.na(air_temp_mean)) |> 
  group_by(location) |>
  filter(n() >= 2) |>
  ungroup()
  
# Determine y axis range and create vertical padded y_max so annotations will be visible
y_max <- max(plot_data$air_temp_mean, na.rm = TRUE)
y_min <- min(plot_data$air_temp_mean, na.rm = TRUE)
range_size = y_max - y_min
y_max_buffered <- y_max + (range_size * 0.2)
  
# Plot
p1 <- ggplot(plot_data, aes(x = year, y = air_temp_mean, color = location)) +
  geom_point() +
  geom_line() +
  geom_smooth(method = "lm", se = TRUE, alpha = 0.1, linetype = "dotted") +
  labs(
    x = "",
    y = "Air Temperature (°C)",
    color = "Buoy",
    tag = "a)"
    ) +
  scale_color_buoy() +
  scale_x_continuous(
    breaks = seq(min(plot_data$year), max(plot_data$year), by = 2)) +

  # Add vertical padding
  scale_y_continuous(limits = c(NA, y_max_buffered)) +

  # Annotate plot with simple linear model p-values and R2 values
  make_stat_annotation(plot_data, size = 4, vstep = 0.05) +
  theme(legend.position = "none")


# Plot time series of mean annual surface water temperature

# Prepare data for plotting
# Only include a buoy in the plot (and thus legend) if it has at least 2 real 
# (non-NA) measurements (2, so a line can be drawn)
plot_data <- buoy_data_annual |>
  filter(!is.na(sst_mean)) |> 
  group_by(location) |>
  filter(n() >= 2) |>
  ungroup()

# Determine y axis range and create vertical padded y_max so annotations will be visible
y_max <- max(plot_data$sst_mean, na.rm = TRUE)
y_min <- min(plot_data$sst_mean, na.rm = TRUE)
range_size = y_max - y_min
y_max_buffered <- y_max + (range_size * 0.2)

# Plot
p2 <- ggplot(plot_data, aes(x = year, y = sst_mean, color = location)) +
  geom_point() +
  geom_line() +
  geom_smooth(method = "lm", se = TRUE, alpha = 0.1, linetype = "dotted") +
  labs(
    x = "",
    y = "Water Temperature (°C)",
    color = "Buoy",
    tag = "b)"
  ) +
  scale_color_buoy() +
  scale_x_continuous(
    breaks = seq(min(plot_data$year), max(plot_data$year), by = 2)) +
  
  # Add vertical padding
  scale_y_continuous(limits = c(NA, y_max_buffered)) +
  
  # Annotate plot with simple linear model p-values and R2 values
  make_stat_annotation(plot_data, size = 4, vstep = 0.05)

p1 + p2



# Plot time series of mean annual wind speed and direction

# Prepare data for plotting
# Only include a buoy in the plot (and thus legend) if it has at least 2 real 
# (non-NA) measurements (2, so a line can be drawn)
plot_data <- buoy_data_annual |>
  filter(!is.na(wind_speed_mean)) |> 
  group_by(location) |>
  filter(n() >= 2) |>
  ungroup()

# Determine y axis range and create vertical padded y_max so annotations will be visible
y_max <- max(plot_data$wind_speed_mean, na.rm = TRUE)
y_min <- min(plot_data$wind_speed_mean, na.rm = TRUE)
range_size = y_max - y_min
y_max_buffered <- y_max + (range_size * 0.2)

# Plot
p3 <- ggplot(plot_data, aes(x = year, y = wind_speed_mean, color = location)) +
  geom_point() +
  geom_line() +
  geom_smooth(method = "lm", se = TRUE, alpha = 0.1, linetype = "dotted") +
  labs(
    x = "",
    y = "Wind Speed (m/s)",
    color = "Buoy",
    tag = "a)"
  ) +
  scale_color_buoy() +
  scale_x_continuous(
    breaks = seq(min(plot_data$year), max(plot_data$year), by = 2)) +
  
  # Add vertical padding
  scale_y_continuous(limits = c(NA, y_max_buffered)) +
  
  # Annotate plot with simple linear model p-values and R2 values
  make_stat_annotation(plot_data, size = 4, vstep = 0.05) +
  theme(legend.position = "none")


# Plot time series of circular mean annual wind direction 

# Prepare data for plotting
# Only include a buoy in the plot (and thus legend) if it has at least 2 real 
# (non-NA) measurements (2, so a line can be drawn)
plot_data <- buoy_data_annual |>
  filter(!is.na(wind_direction_mean)) |> 
  group_by(location) |>
  filter(n() >= 2) |>
  ungroup()

# Determine y axis range and create vertical padded y_max so annotations will be visible
y_max <- max(plot_data$wind_direction_mean, na.rm = TRUE)
y_min <- min(plot_data$wind_direction_mean, na.rm = TRUE)
range_size = y_max - y_min
y_max_buffered <- y_max + (range_size * 0.2)

# Plot
p4 <- ggplot(plot_data, aes(x = year, y = wind_direction_mean, color = location)) +
  geom_point() +
  geom_line() +
  geom_smooth(method = "lm", se = TRUE, alpha = 0.1, linetype = "dotted") +
  labs(
    x = "",
    y = "Wind Direction, circular (°T)",
    color = "Buoy",
    tag = "b)"
  ) +
  scale_color_buoy() +
  scale_x_continuous(
    breaks = seq(min(plot_data$year), max(plot_data$year), by = 2)) +
  
  # Add vertical padding
  scale_y_continuous(limits = c(NA, y_max_buffered)) +
  
  # Annotate plot with simple linear model p-values and R2 values
  make_stat_annotation(plot_data, size = 4, vstep = 0.05)

p3 + p4




# Plot time series of mean average wave period 

# Prepare data for plotting
# Only include a buoy in the plot (and thus legend) if it has at least 2 real 
# (non-NA) measurements (2, so a line can be drawn)
plot_data <- buoy_data_annual |>
  filter(!is.na(avg_wave_period_mean)) |> 
  group_by(location) |>
  filter(n() >= 2) |>
  ungroup()

# Determine y axis range and create vertical padded y_max so annotations will be visible
y_max <- max(plot_data$avg_wave_period_mean, na.rm = TRUE)
y_min <- min(plot_data$avg_wave_period_mean, na.rm = TRUE)
range_size = y_max - y_min
y_max_buffered <- y_max + (range_size * 0.2)

# Plot
p5 <- ggplot(plot_data, aes(x = year, y = avg_wave_period_mean, color = location)) +
  geom_point() +
  geom_line() +
  geom_smooth(method = "lm", se = TRUE, alpha = 0.1, linetype = "dotted") +
  labs(
    x = "",
    y = "Average Wave Period (s)",
    color = "Buoy",
    tag = "a)"
  ) +
  scale_color_buoy() +
  scale_x_continuous(
    breaks = seq(min(plot_data$year), max(plot_data$year), by = 2)) +
  
  # Add vertical padding
  scale_y_continuous(limits = c(NA, y_max_buffered)) +
  
  # Annotate plot with simple linear model p-values and R2 values
  make_stat_annotation(plot_data, size = 4, vstep = 0.05) +
  theme(legend.position = "none")


# Plot time series of mean significant wave height

# Prepare data for plotting
# Only include a buoy in the plot (and thus legend) if it has at least 2 real 
# (non-NA) measurements (2, so a line can be drawn)
plot_data <- buoy_data_annual |>
  filter(!is.na(sig_wave_height_mean)) |> 
  group_by(location) |>
  filter(n() >= 2) |>
  ungroup()

# Determine y axis range and create vertical padded y_max so annotations will be visible
y_max <- max(plot_data$sig_wave_height_mean, na.rm = TRUE)
y_min <- min(plot_data$sig_wave_height_mean, na.rm = TRUE)
range_size = y_max - y_min
y_max_buffered <- y_max + (range_size * 0.25)

# Plot
p6 <- ggplot(plot_data, aes(x = year, y = sig_wave_height_mean, color = location)) +
  geom_point() +
  geom_line() +
  geom_smooth(method = "lm", se = TRUE, alpha = 0.1, linetype = "dotted") +
  labs(
    x = "",
    y = "Significant Wave Height (m)",
    color = "Buoy", 
    tag = "b)"
  ) +
  scale_color_buoy() +
  scale_x_continuous(
    breaks = seq(min(plot_data$year), max(plot_data$year), by = 2)) +
  
  # Add vertical padding
  scale_y_continuous(limits = c(NA, y_max_buffered)) +
  
  # Annotate plot with simple linear model p-values and R2 values
  make_stat_annotation(plot_data, size = 4, vstep = 0.05)

p5 + p6

