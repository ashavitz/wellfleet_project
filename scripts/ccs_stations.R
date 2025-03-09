#'--------------------------------------
#' Center for Coastal Studies - Cape Cod Water Quality Data
#' Data download source: https://www.capecodbay-monitor.org/download
#' Attribution and Citing: https://www.capecodbay-monitor.org/download
#' Map of stations: https://www.capecodbay-monitor.org/
#' Depth: surface
#'--------------------------------------

# TODO: Import and clean data
# TODO: Calculate daily mean and high values to reduce size of data sets.
#       Only include days with complete data

# TODO: Initial visualization - variable change over time
# TODO: Add threshold reference lines
# --------------------------------------

# ---- Load Libraries ----

library(readr) # for reading in files
library(lubridate) # for date time formats
library(dplyr) # for data manipulation and transformation
library(tidyr) # for tidying and reshaping data
library(ggplot2) # for data visualization
library(RColorBrewer) # for data viz color palettes

# ---- csv import from CCS ----

# Read in csv file from local directory
file_path_ccs_data <- "data/ccs_data_all/station_data.csv"
ccs_data_all <- read_csv(file_path_ccs_data, col_names = TRUE)

# Examining the data
# str(ccs_data_all)
# summary(ccs_data_all)
# visdat::vis_dat(ccs_data_all)
# skimr::skim(ccs_data_all)

#' Relevant station IDs and Names
  #' station id # : station name
    #' 1 : 5N
    #' 2 : 5S
    #' 3 : 5SX

#' Other neaerby station IDs and Names
    #' 4 : 6M
    #' 5 : 6S
    #' 11 : Blackfish Creek
    #' 22 : Great Island Channel
    #' 25 : Inner Pamet
    #' 29 : Inner Wellfleet Harbor
    #' 38 : Pamet
    #' 44 : Sunken Meadow
    #' 45 : Wellfleet Harbor
    #' 67 : North Sunken Meadow
    #' 75 : Pamet River
    #' 94 : WH-5

# Filter data frame to keep only data from geographically relevant stations
station_ids_wellfleet <- c(1,2,3)
ccs_data_wellfleet <- filter(ccs_data_all,
                             internal_station_id %in% station_ids_wellfleet) |> 
  # create year column
  mutate(year_collected = year(collected_at)) |> 
  relocate(year_collected, .after = collected_at)

# Create list of all column names for each water quality variable
wq_variables <- names(ccs_data_all)[-c(1:3)]


# ---- Plot All Data Over Time ----

# For each variable, mean is plotted against year.
# Colored by station ID.
for (var in wq_variables) {
  p <- ggplot(ccs_data_wellfleet, aes(x = collected_at,
                                            y = .data[[var]],
                                            color = as.factor(internal_station_id))) +
    geom_point() +
    labs(x = "Date", y = var, title = paste("Time Series of", var)) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  print(p)
}

# ---- Plot All Data Over Time - ~Monthly (all collection dates) ----

start_year <- min(ccs_data_wellfleet$collected_at, na.rm = TRUE)
end_year   <- max(ccs_data_wellfleet$collected_at, na.rm = TRUE)

# Monthly temperature
ggplot(data = ccs_data_wellfleet,
       mapping = aes(x = collected_at, 
                     y = temperature_C,
       color = as.factor(internal_station_id))) +
  geom_point() +
  
  # add 25 Celcius threshold line
  geom_hline(yintercept = 25, linetype = 'dotted', color = 'red', size = 2) +
  labs(x = "Date",
       y = "Temperature (°C)",
       color = "CCS Station ID",
       title = paste("Temp (C) Time Series - CCS Wellfleet")) +
  scale_x_datetime(
    limits = c(start_year, end_year),
    date_breaks = "2 year", 
    date_labels = "%Y",
    labels = function(x) format(x, "%Y")) + # extract just the year for the labels
  scale_color_brewer(palette = "Set2") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1,),
        plot.title = element_text(hjust = 0.5))


# ---- Plot Annual Medians ----

# Create df of annual means by station
ccs_data_wellfleet_medians <- ccs_data_wellfleet |> 
  # TODO: When grouping by year, make sure to only include years with all monthly measurements
  # Summarize 
  group_by(internal_station_id, year_collected) |> 
  summarize(
    across(all_of(wq_variables), 
           ~median(.x, na.rm = TRUE))) |> 
  ungroup()

# For each variable, mean is plotted against year.
# Colored by station ID.
for (var in wq_variables) {
  p <- ggplot(ccs_data_wellfleet_medians, aes(x = year_collected,
                                            y = .data[[var]],
                                            color = as.factor(internal_station_id))) +
    geom_point() +
    labs(x = "Year", y = var, title = paste("Annual Median Time Series of ", var)) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  print(p)
}


# ---- Plot Annual Means ----

# Create df of annual means by station
ccs_data_wellfleet_means <- ccs_data_wellfleet |> 
  # TODO: When grouping by year, make sure to only include years with all monthly measurements
  # Summarize 
  group_by(internal_station_id, year_collected) |> 
  summarize(
    across(all_of(wq_variables), 
           ~mean(.x, na.rm = TRUE))) |> 
  ungroup()

# For each variable, mean is plotted against year.
# Colored by station ID.
for (var in wq_variables) {
  p <- ggplot(ccs_data_wellfleet_means, aes(x = year_collected,
                                            y = .data[[var]],
                                            color = as.factor(internal_station_id))) +
    geom_point() +
    labs(x = "Year", y = var, title = paste("Annual Mean Time Series of ", var)) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  print(p)
}

# 
# # create a list of reference values 
# reference_values <- list(
#   temperature_C = 25,
#   dissolved_oxygen_mg_L = 6,
#   chlorophyll_ug_L = 5.1,
#   turbidty_NTU = 5,
# )



