# ---- Header ----
#'--------------------------------------
#' Center for Coastal Studies - Cape Cod Water Quality Data
#' Data download source: https://www.capecodbay-monitor.org/download
#' Attribution and Citing: https://www.capecodbay-monitor.org/download
#' Map of stations: https://www.capecodbay-monitor.org/
#' Depth: surface
#'--------------------------------------

# ---- TODO ----



# ---- Load Libraries ----

library(readr) # for reading in files
library(lubridate) # for date time formats
library(dplyr) # for data manipulation and transformation
library(tidyr) # for tidying and reshaping data
library(ggplot2) # for data visualization
library(RColorBrewer) # for data viz color palettes
library(patchwork) # for displaying graphs together

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
  # create year and month columns
  mutate(year_collected = year(collected_at),
         month = month(collected_at)) |>
  relocate(year_collected, month, .after = collected_at)

# Create list of all water quality variables of interest
wq_variables <- c(
  "temperature_C",                   
  "salinity",                       
  "dissolved_oxygen_mg/L",           
  "chlorophyll_ug/L",               
  "pheophytin_ug/L",                 
  "turbidty_NTU",                   
  "nitrate_nitrite_uM",              
  "ammonium_uM",                    
  "ortho_phosphate_uM",              
  "total_nitrogen_uM",               
  "total_phosphorus_uM"           
)

# Create a named list of threshold values associated with wq variables
thresholds <- c(
  temperature_C = 25,
  `dissolved_oxygen_mg/L` = 6,
  `chlorophyll_ug/L` = 5.1,
  turbidty_NTU = 5,
  total_nitrogen_uM = 21.42,
  total_phosphorus_uM = 2.29
)

# Set start year and end year for plotting
start_year <- min(ccs_data_wellfleet$collected_at, na.rm = TRUE)
end_year   <- max(ccs_data_wellfleet$collected_at, na.rm = TRUE)

# ---- Filter Only "Full" Years With Exactly One Monthly Measurement ----

# NOTE - For each station, the following block only includes years with exactly one
# measurement per month (no more no less). A few years have months with more than one
# measurement, which could be included if only one of those measurements is selected.

ccs_wellfleet_full_years <- ccs_data_wellfleet |>
  # Count how many measurements per station, year, and month
  group_by(internal_station_id, year_collected, month) |>
  summarize(n_measurements = n(), .groups = "drop") |>
  # For each station and year, check how many months had exactly one measurement
  group_by(internal_station_id, year_collected) |>
  summarize(months_with_one_measurement = sum(n_measurements == 1), .groups = "drop") |>
  # Keep only station-years with exactly one measurement per month (12 months, no extras)
  filter(months_with_one_measurement == 12) |>
  select(internal_station_id, year_collected) |>
  # Join back to original data to filter only good station-years, removing id column
  inner_join(ccs_data_wellfleet, by = c("internal_station_id", "year_collected"))


# Calculate mean & median for all water quality variables
ccs_wellfleet_annual <- ccs_wellfleet_full_years |> 
  group_by(internal_station_id, year_collected) |> 
  summarize(
    across(
      all_of(wq_variables),
      list(
        mean = function(x) if (any(is.na(x))) NA_real_ else mean(x),
        median = function(x) if (any(is.na(x))) NA_real_ else median(x)
      )
    ),
    .groups = "drop"
  )


# ---- Filter Only "Full" Summers With Exactly One Monthly Measurement June - September ----

# Filter out non-summer months
ccs_wellfleet_summers <- filter(ccs_data_wellfleet, month %in% 6:9)

ccs_wellfleet_full_summers <- ccs_wellfleet_summers |> 
  # Count how many measurements per station, year, and month
  group_by(internal_station_id, year_collected, month) |>
  summarize(n_measurements = n(), .groups = "drop") |>
  # For each station and year, check how many months had exactly one measurement
  group_by(internal_station_id, year_collected) |>
  summarize(months_with_one_measurement = sum(n_measurements == 1), .groups = "drop") |>
  # Keep only station-years with exactly one measurement per month (4 months, no extras)
  filter(months_with_one_measurement == 4) |>
  select(internal_station_id, year_collected) |>
  # Join back to original data to filter only good station-years, removing id column
  inner_join(ccs_wellfleet_summers, by = c("internal_station_id", "year_collected")) 
  # Filter once more 


# Calculate (summer) mean & median for all water quality variables
ccs_wellfleet_summers <- ccs_wellfleet_full_summers |> 
  group_by(internal_station_id, year_collected) |> 
  summarize(
    across(
      all_of(wq_variables),
      list(
        mean = function(x) if (any(is.na(x))) NA_real_ else mean(x),
        median = function(x) if (any(is.na(x))) NA_real_ else median(x)
      )
    ),
    .groups = "drop"
  )


# ---- Set Global ggplot Themes ----

# Set ggplot theme to minimal, rotate x axis labels, and center plot titles
theme_set(
  theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.title = element_text(hjust = 0.5)
    )
)

# ---- Plot All Data Over Time ----

# For each relevant variable, plot all ~monthly data over time
for (var in wq_variables) {
  p <- ggplot(ccs_data_wellfleet, aes(x = collected_at,
                                      y = .data[[var]],
                                      color = as.factor(internal_station_id))) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +

    # add threshold line, if available
    geom_hline(yintercept = thresholds[var], linetype = 'dotted', color = 'red', linewidth = 2) +
    
    labs(x = "Date",
         y = var,
         color = "CCS Station ID",
         title = paste(var, " Time Series - CCS Wellfleet")) +
    scale_x_datetime(
      limits = c(start_year, end_year),
      date_breaks = "2 year", 
      date_labels = "%Y",
      labels = date_format("%Y")) + # extract just the year for the labels
    scale_color_brewer(palette = "Set2")
  
  print(p)
}
  

# For each relevant variable, plot Annual mean and mean values
for (var in wq_variables) {
  
  # Create annual mean plot
  var_name <- paste0(var, "_mean")
  p_1 <- ggplot(ccs_wellfleet_annual, aes(x = year_collected,
                                        y = .data[[var_name]],
                                        color = as.factor(internal_station_id))) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    labs(x = "Year",
         y = var,
         color = "CCS Station ID",
         title = paste("Annual", var_name, "Time Series - CCS Wellfleet")) +
    scale_color_brewer(palette = "Set2")
  
  # Create annual median plot
  var_name <- paste0(var, "_median")
  p_2 <- ggplot(ccs_wellfleet_annual, aes(x = year_collected,
                                        y = .data[[var_name]],
                                        color = as.factor(internal_station_id))) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    labs(x = "Year",
         y = var,
         color = "CCS Station ID",
         title = paste("Annual", var_name, "Time Series - CCS Wellfleet")) +
    scale_color_brewer(palette = "Set2")
    
  # Combine the plots using patchwork with adjusted spacing
  combined_plot <- p_1 + p_2 + 
    # Stack the plots vertically
    plot_layout(ncol = 1, heights = c(1, 1)) +
    plot_annotation(title = paste("Annual -", var))
  
  print(combined_plot)
}


# For each relevant variable, plot Summer mean and mean values
for (var in wq_variables) {
  
  # Create summer mean plot
  var_name <- paste0(var, "_mean")
  p_1 <- ggplot(ccs_wellfleet_summers, aes(x = year_collected,
                                          y = .data[[var_name]],
                                          color = as.factor(internal_station_id))) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    
    # add threshold line, if available
    geom_hline(yintercept = thresholds[var], linetype = 'dotted', color = 'red', linewidth = 2) +
    
    labs(x = "Year",
         y = var,
         color = "CCS Station ID",
         title = paste("Summer", var_name, "Time Series - CCS Wellfleet")) +
    scale_color_brewer(palette = "Set2")
  
  # Create summer median plot
  var_name <- paste0(var, "_median")
  p_2 <- ggplot(ccs_wellfleet_summers, aes(x = year_collected,
                                          y = .data[[var_name]],
                                          color = as.factor(internal_station_id))) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    
    # add threshold line, if available
    geom_hline(yintercept = thresholds[var], linetype = 'dotted', color = 'red', linewidth = 2) +
    
    labs(x = "Year",
         y = var,
         color = "CCS Station ID",
         title = paste("Summer", var_name, "Time Series - CCS Wellfleet")) +
    scale_color_brewer(palette = "Set2")
  
  combined_plot <- p_1 + p_2 + 
    plot_layout(ncol = 1, heights = c(1, 1)) +
    plot_annotation(title = paste("Summer -", var))
  
  print(combined_plot)
}


# ---- Plots by Variable ----

# _____________________________ Temperature _____________________________

# All Data (~Monthly)
ggplot(data = ccs_data_wellfleet,
       mapping = aes(x = collected_at, 
                     y = temperature_C,
       color = as.factor(internal_station_id))) +
  geom_point() +
  
  # add 25 Celsius threshold line
  geom_hline(yintercept = 25, linetype = 'dotted', color = 'red', linewidth = 2) +
  labs(x = "Date",
       y = "Temperature (°C)",
       color = "CCS Station ID",
       title = paste("Temp (C) Time Series - CCS Wellfleet")) +
  scale_x_datetime(
    limits = c(start_year, end_year),
    date_breaks = "2 year", 
    date_labels = "%Y",
    labels = date_format("%Y")) + # extract just the year for the labels
  scale_color_brewer(palette = "Set2")


# Annual Mean
ggplot(data = ccs_wellfleet_annual,
       mapping = aes(x = year_collected, 
                     y = temperature_C_mean,
                     color = as.factor(internal_station_id))) +
  geom_point(size = 3.5) +
  labs(x = "Year",
       y = "Annual Mean Temperature (°C)",
       color = "CCS Station ID",
       title = paste("Annual Mean Temp (C) Time Series - CCS Wellfleet")) +
  scale_color_brewer(palette = "Set2")


# Annual Median
ggplot(data = ccs_wellfleet_annual,
       mapping = aes(x = year_collected, 
                     y = temperature_C_median,
                     color = as.factor(internal_station_id))) +
  geom_point(size = 3.5) +
  labs(x = "Date",
       y = "Annual Median Temperature (°C)",
       color = "CCS Station ID",
       title = paste("Annual Median Temp (C) Time Series - CCS Wellfleet")) +
  scale_color_brewer(palette = "Set2")

# _____________________________ Dissolved Oxygen _____________________________

# All Data (~Monthly)
ggplot(data = ccs_data_wellfleet,
       mapping = aes(x = collected_at, 
                     y = `dissolved_oxygen_mg/L`,
                     color = as.factor(internal_station_id))) +
  geom_point() +
  
  # add 6 mg/L threshold line
  geom_hline(yintercept = 6, linetype = 'dotted', color = 'red', linewidth = 2) +
  labs(x = "Date",
       y = "DO (mg/L)",
       color = "CCS Station ID",
       title = paste("DO Time Series - CCS Wellfleet")) +
  scale_x_datetime(
    limits = c(start_year, end_year),
    date_breaks = "2 year", 
    date_labels = "%Y",
    labels = date_format("%Y")) +
  scale_color_brewer(palette = "Set2")


# Annual Mean
ggplot(data = ccs_wellfleet_annual,
       mapping = aes(x = year_collected, 
                     y = `dissolved_oxygen_mg/L_mean`,
                     color = as.factor(internal_station_id))) +
  geom_point(size = 3.5) +
  labs(x = "Year",
       y = "Annual Mean DO (mg/L)",
       color = "CCS Station ID",
       title = paste("Annual Mean DO Time Series - CCS Wellfleet")) +
  scale_color_brewer(palette = "Set2")


# Annual Median
ggplot(data = ccs_wellfleet_annual,
       mapping = aes(x = year_collected, 
                     y = `dissolved_oxygen_mg/L_median`,
                     color = as.factor(internal_station_id))) +
  geom_point(size = 3.5) +
  labs(x = "Date",
       y = "Annual Median DO (mg/L)",
       color = "CCS Station ID",
       title = paste("Annual Median DO Time Series - CCS Wellfleet")) +
  scale_color_brewer(palette = "Set2")


# _____________________________ Chlorophyll a _____________________________

# All Data (~Monthly)
ggplot(data = ccs_data_wellfleet,
       mapping = aes(x = collected_at, 
                     y = `chlorophyll_ug/L`,
                     color = as.factor(internal_station_id))) +
  geom_point() +
  
  # add 5.1 u/L threshold line
  geom_hline(yintercept = 5.1, linetype = 'dotted', color = 'red', linewidth = 2) +
  labs(x = "Date",
       y = "Chlorophyll a (ug/L)",
       color = "CCS Station ID",
       title = paste("Chlorophyll Time Series - CCS Wellfleet")) +
  scale_x_datetime(
    limits = c(start_year, end_year),
    date_breaks = "2 year", 
    date_labels = "%Y",
    labels = date_format("%Y")) +
  scale_color_brewer(palette = "Set2")


# Annual Mean
ggplot(data = ccs_wellfleet_annual,
       mapping = aes(x = year_collected, 
                     y = `chlorophyll_ug/L_mean`,
                     color = as.factor(internal_station_id))) +
  geom_point(size = 3.5) +
  labs(x = "Year",
       y = "Annual Mean Chlorophyll a (ug/L)",
       color = "CCS Station ID",
       title = paste("Annual Mean Chlorophyll a Time Series - CCS Wellfleet")) +
  scale_color_brewer(palette = "Set2")


# Annual Median
ggplot(data = ccs_wellfleet_annual,
       mapping = aes(x = year_collected, 
                     y = `chlorophyll_ug/L_median`,
                     color = as.factor(internal_station_id))) +
  geom_point(size = 3.5) +
  labs(x = "Date",
       y = "Annual Median Chlorophyll a (ug/L)",
       color = "CCS Station ID",
       title = paste("Annual Median Chlorophyll a Time Series - CCS Wellfleet")) +
  scale_color_brewer(palette = "Set2")


# _____________________________ Total Nitrogen _____________________________
# Data set units in uM (micromoles / L)
# Threshold value of 0.071 mg/L converted to 21.42 uM
  # MW N = 14.006720 µg/L N (https://www.ices.dk/data/tools/Pages/Unit-conversions.aspx)
  # 1 mg N/L = 71.394 µM/L

# All Data (~Monthly)
ggplot(data = ccs_data_wellfleet,
       mapping = aes(x = collected_at, 
                     y = total_nitrogen_uM,
                     color = as.factor(internal_station_id))) +
  geom_point() +
  
  # add 21.42 uM threshold line
  geom_hline(yintercept = 21.42, linetype = 'dotted', color = 'red', linewidth = 2) +
  labs(x = "Date",
       y = "Total N (uM)",
       color = "CCS Station ID",
       title = paste("Total Nitrogen Time Series - CCS Wellfleet")) +
  scale_x_datetime(
    limits = c(start_year, end_year),
    date_breaks = "2 year", 
    date_labels = "%Y",
    labels = date_format("%Y")) +
  scale_color_brewer(palette = "Set2")


# Annual Mean
ggplot(data = ccs_wellfleet_annual,
       mapping = aes(x = year_collected, 
                     y = total_nitrogen_uM_mean,
                     color = as.factor(internal_station_id))) +
  geom_point(size = 3.5) +
  labs(x = "Year",
       y = "Annual Mean Total N (uM)",
       color = "CCS Station ID",
       title = paste("Annual Mean Total N Time Series - CCS Wellfleet")) +
  scale_color_brewer(palette = "Set2")


# Annual Median
ggplot(data = ccs_wellfleet_annual,
       mapping = aes(x = year_collected, 
                     y = total_nitrogen_uM_median,
                     color = as.factor(internal_station_id))) +
  geom_point(size = 3.5) +
  labs(x = "Date",
       y = "Annual Median Total N (uM)",
       color = "CCS Station ID",
       title = paste("Annual Median Total N Time Series - CCS Wellfleet")) +
  scale_color_brewer(palette = "Set2")


# _____________________________ Total Phosphorus _____________________________
# Data set units in uM (micromoles / L)
# Threshold value of 0.071 mg/L converted to 2.29 uM
  # MW P = 30.973762 µg/L P (https://www.ices.dk/data/tools/Pages/Unit-conversions.aspx)
  # 1 mg P/L = 32.285 µM/L

# All Data (~Monthly)
ggplot(data = ccs_data_wellfleet,
       mapping = aes(x = collected_at, 
                     y = total_phosphorus_uM,
                     color = as.factor(internal_station_id))) +
  geom_point() +
  
  # add 2.29 uM threshold line
  geom_hline(yintercept = 2.29, linetype = 'dotted', color = 'red', linewidth = 2) +
  labs(x = "Date",
       y = "Total P (uM)",
       color = "CCS Station ID",
       title = paste("Total Phorphorus Time Series - CCS Wellfleet")) +
  scale_x_datetime(
    limits = c(start_year, end_year),
    date_breaks = "2 year", 
    date_labels = "%Y",
    labels = date_format("%Y")) +
  scale_color_brewer(palette = "Set2")


# Annual Mean
ggplot(data = ccs_wellfleet_annual,
       mapping = aes(x = year_collected, 
                     y = total_phosphorus_uM_mean,
                     color = as.factor(internal_station_id))) +
  geom_point(size = 3.5) +
  labs(x = "Year",
       y = "Annual Mean Total P (uM)",
       color = "CCS Station ID",
       title = paste("Annual Mean Total P Time Series - CCS Wellfleet")) +
  scale_color_brewer(palette = "Set2")


# Annual Median
ggplot(data = ccs_wellfleet_annual,
       mapping = aes(x = year_collected, 
                     y = total_phosphorus_uM_median,
                     color = as.factor(internal_station_id))) +
  geom_point(size = 3.5) +
  labs(x = "Date",
       y = "Annual Median Total P (uM)",
       color = "CCS Station ID",
       title = paste("Annual Median Total P Time Series - CCS Wellfleet")) +
  scale_color_brewer(palette = "Set2")


# _____________________________ Turbidity _____________________________

# All Data (~Monthly)
ggplot(data = ccs_data_wellfleet,
       mapping = aes(x = collected_at, 
                     y = turbidty_NTU,
                     color = as.factor(internal_station_id))) +
  geom_point() +
  
  # add 5 NTU threshold line
  geom_hline(yintercept = 5, linetype = 'dotted', color = 'red', linewidth = 2) +
  labs(x = "Date",
       y = "Turbidity (NTU)",
       color = "CCS Station ID",
       title = paste("Turbidity Time Series - CCS Wellfleet")) +
  scale_x_datetime(
    limits = c(start_year, end_year),
    date_breaks = "2 year", 
    date_labels = "%Y",
    labels = date_format("%Y")) +
  scale_color_brewer(palette = "Set2")


# _____________________________ Salinity _____________________________

# All Data (~Monthly)
ggplot(data = ccs_data_wellfleet,
       mapping = aes(x = collected_at, 
                     y = salinity,
                     color = as.factor(internal_station_id))) +
  geom_point() +
  
  labs(x = "Date",
       y = "Salinity",
       color = "CCS Station ID",
       title = paste("Salinity Time Series - CCS Wellfleet")) +
  scale_x_datetime(
    limits = c(start_year, end_year),
    date_breaks = "2 year", 
    date_labels = "%Y",
    labels = date_format("%Y")) +
  scale_color_brewer(palette = "Set2")


# Annual Mean
ggplot(data = ccs_wellfleet_annual,
       mapping = aes(x = year_collected, 
                     y = salinity_mean,
                     color = as.factor(internal_station_id))) +
  geom_point(size = 3.5) +
  labs(x = "Year",
       y = "Annual Mean Salinity",
       color = "CCS Station ID",
       title = paste("Annual Mean Salinity Time Series - CCS Wellfleet")) +
  scale_color_brewer(palette = "Set2")


