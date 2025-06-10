# ---- Header ----
#'--------------------------------------
#' Boston Logan and Hyannis Barnstable Airport Wind Data
#' Accessed via NOAA Find a Station Data Tool
#' https://www.ncdc.noaa.gov/cdo-web/datatools/findstation
#' Boston Logan: https://www.ncdc.noaa.gov/cdo-web/datasets/GHCND/stations/GHCND:USW00014739/detail
#' Hyannis Barnstable: https://www.ncdc.noaa.gov/cdo-web/datasets/GHCND/stations/GHCND:USW00094720/detail
#' metadata: https://www.ncei.noaa.gov/pub/data/cdo/documentation/GHCND_documentation.pdf
#'--------------------------------------

# ---- TODO ---- 

#'--------------------------------------

# ---- Load Libraries ----
library(readr) # for reading in files
library(lubridate) # for date time formats
library(dplyr) # for data manipulation and transformation
library(tidyr) # for tidying and reshaping data
library(ggplot2) # for visualization
library(ggpmisc) # for annotating plots with p & R2 of fitted polynomial via stat_poly_eq()
library(circular) # for calculating circular means


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

# ---- Read in local files ----

boston_wind <- read_csv("data/land_wind_data/boston_wind_data_1984_2024.csv") |> 
  mutate(location = "Boston")
hyannis_wind <- read_csv("data/land_wind_data/hyannis_wind_data_1999_2024.csv") |> 
  mutate(location = "Hyannis")
wind_data <- bind_rows(boston_wind, hyannis_wind) |> 
  mutate(year = year(DATE))

# Metadata
# AWND = Average daily wind speed (m/s)
# WDF2 = Direction of fastest 2-minute wind (degrees) - Assumed to be from True North
# WSF2 = Fastest 2-minute wind speed (m/s)

# ---- Plot All Data (Daily) ----

# Plot average daily wind speed
ggplot(wind_data,
       aes(x = DATE, y = AWND)) +
  geom_point() +
  facet_wrap(~location)

# Plot Fastest 2-minute wind speed (m/s)
ggplot(wind_data,
       aes(x = DATE, y = WSF2)) +
  geom_point() +
  facet_wrap(~location)

# Plot direction of fastest 2-minute wind
ggplot(wind_data,
       aes(x = DATE, y = WDF2)) +
  geom_point() +
  facet_wrap(~location)

# ---- Calculate Annual Average Values ----

# # Check number of non-missing values per location per year
# wind_data |> 
#   group_by(location, year) |> 
#   summarise(
#     AWND_non_na = sum(!is.na(AWND)),
#     WDF2_non_na = sum(!is.na(WDF2)),
#     WSF2_non_na = sum(!is.na(WSF2))
#   ) |> 
#   View()
# # All years have mostly complete data, except Boston 1996 2-minute wind speed data 
# # (first year collected)

# Calculate annual average values
wind_data_annual <- wind_data |>  
  group_by(year, location) |> 
  summarise(
    # Only include location years with at least 80% of a full year's worth of measurements (365 days)
    mean_AWND = ifelse(sum(is.na(AWND)) > 365 * 0.2, NA_real_, mean(AWND, na.rm = TRUE)),
    mean_WSF2 = ifelse(sum(is.na(WSF2)) > 365 * 0.2, NA_real_, mean(WSF2, na.rm = TRUE)),
    mean_WDF2 = if (sum(is.na(WDF2)) > 365 * 0.2) NA_real_ else {
      as.numeric(mean.circular(
        circular(WDF2, units = "degrees", modulo = "2pi"),
        na.rm = TRUE))
    },
    .groups = "drop"
  )


# ---- Plot Average Annual Data ----

# Create list of variables
variables <- c("mean_AWND", "mean_WSF2", "mean_WDF2")
variables_meta <- list(mean_AWND = "Average Annual Wind Speed (m/s)",
                       mean_WSF2 = "Average Annual Fastest 2-minute Wind Speed (m/s)",
                       mean_WDF2 = "Average Annual Fastest 2-minute Wind Direction (°T)")

# Plot each variable
for (var in variables) {
  
  # Determine y axis range and create vertical padded y_max so annotations will be visible
  y_max <- max(wind_data_annual[[var]], na.rm = TRUE)
  y_min <- min(wind_data_annual[[var]], na.rm = TRUE)
  range_size = y_max - y_min
  y_max_buffered <- y_max + (range_size * 0.2)
  
  # Determine first year with data
  start_year = min(
    filter(wind_data_annual, !is.na(wind_data_annual[[var]]))$year
    )
  
  # Plot
  p <- ggplot(wind_data_annual,
         aes(x = year, y = .data[[var]], color = location)) +
    geom_point() +
    geom_smooth(method = "lm") +
    labs(
      x = "Date",
      y = variables_meta[[var]]
    ) +
    facet_wrap(~location) +
    # Add vertical padding
    scale_x_continuous(limits = c(start_year, NA)) +
    
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

