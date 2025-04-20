# ---- Header ----
#'--------------------------------------
#' Station 44013
#' Boston Approach Lighted Buoy BF NOAA 44013
#' NDBC ID: 44013
#' Lat: 42.346 Lon: -70.651
#' https://www.ndbc.noaa.gov/station_realtime.php?station=44013
#' https://mariners.neracoos.org/platform/44013
#' Boston Harbor
#' Owned and maintained by NDBC
#'--------------------------------------

# ---- TODO ----
# TODO - Currently excluding "999.0" when reading in data is likely removing valid values from the
# PRES and BAR columns.

# TODO - Determine when and how recording cadences change to be able to properly calculate averages.


#'--------------------------------------

# ---- Load Libraries ----
library(readr) # for reading in files
library(lubridate) # for date time formats
library(dplyr) # for data manipulation and transformation
library(tidyr) # for tidying and reshaping data
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


# ---- Load and Clean NDBC Data ----
# NOTE - Some NDBC txt data files have an extra space at the end of some rows,
# causing issues reading data. To solve this, any spaces at the end of each line
# are removed (uncomment below if you need to run this):

# years <- 1995:2024
# for (year in years) {
#   file_contents <- readLines(paste0("data/buoy_44013_data/44013h", year, ".txt"))
#   file_contents <- trimws(file_contents, which = "right")
#   writeLines(file_contents, paste0("data/buoy_44013_data/44013h", year, "_cleaned.txt"))
# }


# Read in all data files
# Files through 2006 have one header line. Post-2006 have two header lines.

# Read in 1995 - 2006 data
years <- 1995:2006
buoy_data_list <- list()

# Loop through years and read files into the list
for (year in years) {
  file_path <- paste0("data/buoy_44013_data/44013h", year, "_cleaned.txt")
  buoy_data_list[[as.character(year)]] <- 
    
    # using read.table here to include fill for blank values. read_table is used elsewhere.
    read.table(file_path,
               header = TRUE,
               fill = TRUE,           # Fill missing columns with NA
               sep = "",              
               na.strings = c("", "99.0", "99.00", "999", "999.0", "9999.0"),
               colClasses = "character")
}


# Read in 2007 - 2024 data, taking 1st row as header and skipping 2nd row metadata label
years <- 2007:2024
for (year in years) {
  file_path <- paste0("data/buoy_44013_data/44013h", year, "_cleaned.txt")
  header_info <- read_table(file_path,
                            n_max = 1,
                            col_names = FALSE)
  buoy_data_list[[as.character(year)]] <-
    read_table(file_path,
               as.character(header_info[1, ]),
               skip = 2, 
               col_types = cols(.default = col_character()), # read all as chr to standardize
               na = c("", "99.0", "99.00", "999", "999.0", "9999.0")) 
}
rm(header_info)

# # Access data for a specific year (e.g., 2005)
# buoy_data_list[["2005"]]
# str(buoy_data_list[["1995"]])
# View(buoy_data_list[["1995"]])


# merge all data
buoy_data <- bind_rows(buoy_data_list, .id = "year") |> 
  
  # merge duplicate columns to make single column (year, wind direction, barometric pressure) 
  mutate(YYYY = coalesce(YY, YYYY, `#YY`),
         YYYY = if_else(nchar(YYYY) == 2, paste0("19", YYYY), YYYY),
         WDIR = if_else(is.na(WDIR), WD, WDIR),
         PRES = if_else(is.na(PRES), BAR, PRES)) |> 
  select(-c(year, YY, `#YY`, WD, BAR)) |> 
  
  # organize date info to initial columns
  relocate(YYYY, MM, DD, hh, mm, WDIR, .before = WSPD) |>  
  
  # Remove any duplicated rows of data
  distinct()


# Convert each column to the appropriate data type
buoy_data <- buoy_data |> 
  mutate(
    across(-c(YYYY, MM, DD, hh, mm), as.numeric),
    YYYY = as.integer(YYYY),
    MM = as.integer(MM),
    DD = as.integer(DD),
    hh = as.integer(hh),
    mm = ifelse(is.na(as.integer(mm)), 0, as.integer(mm)), # Replace NA with 0
    
  # create datetime column
    datetime = make_datetime(YYYY, MM, DD, hh, mm) 
  )|> 
  relocate(datetime, .before = YYYY)


# Details on Variables:
  # WDIR - Wind Direction (degT)
  # WSPD - Wind speed (m/s)
  # GST	- Peak 5 or 8 second gust speed (m/s)
  # WVHT - Significant wave height (meters)
  # DPD	- Dominant wave period (seconds)
  # APD	- Average wave period (seconds)
  # MWD	- The direction from which the waves at the dominant period (DPD) are coming (degT)
  # PRES - Sea level pressure (hPa)
  # ATMP - Air temperature (degC)
  # WTMP - Sea surface temperature (degC)
  # DEWP -Dewpoint temperature (not stated - assuming degC)
  # VIS	- Station visibility (nautical miles)
  # TIDE - The water level in feet above or below MLLW (ft)


# ---- Calculate & Plot Daily and Annual Means ----

# NOTE - Between October 28th & 29th, 2020, measurement cadence switches to every 10 minutes
# for WDIR, WSPD, GST, ATMP, WTMP, DEWP, and PRES


angular_variables <- c("WDIR", "MWD")
scalar_variables <- c("WSPD","GST", "WVHT", "DPD", "APD", "PRES",
                      "ATMP","WTMP", "DEWP", "VIS","TIDE")
variables <- c(angular_variables, scalar_variables)
variables_meta <- list(
  WDIR = "Circular Mean Wind Direction (degT)",
  WSPD = "Wind speed (m/s)",
  GST	= "Peak 5 or 8 second gust speed (m/s)",
  WVHT = "Significant wave height (meters)",
  DPD	= "Dominant wave period (seconds)",
  APD	= "Average wave period (seconds)",
  MWD	= "Wave direction at dominant period (degT)",
  PRES = "Sea level pressure (hPa)",
  ATMP = "Air temperature (degC)",
  WTMP = "Sea surface temperature (degC)",
  DEWP = "Dewpoint temperature (degC)",
  VIS	= "Station visibility (nautical miles)",
  TIDE = "Water level above or below MLLW (ft)"
)


# Calculate daily mean for all variables
buoy_data_daily <- buoy_data |> 
  group_by(YYYY, MM, DD) |> 
  summarize(
    across(all_of(scalar_variables), ~ mean(.x, na.rm = TRUE)),
    across(all_of(angular_variables), ~
             as.numeric(circular::mean.circular(
               circular::circular(.x, units = "degrees", modulo = "2pi"),
               na.rm = TRUE))
    ),
    # also calculate a simple, non-circular mean for wind direction, for comparison to DKP analysis
    WDIR_simple = mean(WDIR),
    .groups = "drop"
  ) |> 
  mutate(date = make_date(YYYY, MM, DD)) |> 
  relocate(WDIR, .before = WSPD) |> 
  relocate(WDIR_simple, .after = WDIR) |> 
  relocate(MWD, .before = ATMP) |> 
  relocate(date, .before = YYYY)
  
# Update variables
  scalar_variables <- c("WDIR_simple", scalar_variables)
  variables <- c("WDIR_simple", variables)
  variables_meta <- c(variables_meta, WDIR_simple = "Simple Mean Wind Direction (degT)")
  

# Plot daily mean values for each variable
for (var in variables) {
  p <- ggplot(buoy_data_daily,
              aes(x = date,
                  y = .data[[var]])) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    labs(x = "Date",
         y = variables_meta[[var]],
         title = paste("Daily Time Series - Boston Harbor Buoy",
                       variables_meta[[var]], sep = "\n")) +
    scale_color_brewer(palette = "Set2") +
    scale_x_date(
      breaks = seq(min(buoy_data_daily$date), max(buoy_data_daily$date), by = "2 years"),
      date_labels = "%Y")
  
  print(p)
}


# Calculate annual mean for all variables

# # Checking for years significantly lacking data
# counts <- buoy_data |>
#   group_by(YYYY) |>
#   summarize(count_obs = n())
# 1997 and 2012 are missing large time periods of data

buoy_data_annual <- buoy_data_daily |> 
  group_by(YYYY) |> 
  # 1997 and 2012 are missing large time periods of data. Excluding from annual summaries  
  filter(!(YYYY %in% c(1997, 2012))) |> 
  summarize(
    # scalar variables: only mean if ≥80% non‑NA, else NA
    across(all_of(scalar_variables),
           ~ ifelse(
             sum(!is.na(.x)) >= 0.8 * n(),
             mean(.x, na.rm = TRUE),
             NA_real_
           )
    ),
    
    # angular variables: only circular‐mean if ≥80% non‑NA, else NA
    across(all_of(angular_variables),
           ~ ifelse(
             sum(!is.na(.x)) >= 0.8 * n(),
             as.numeric(
               circular::mean.circular(
                 circular::circular(.x, units = "degrees", modulo = "2pi"),
                 na.rm = TRUE
                 )
               ),
             NA_real_
           )
    ),
    .groups = "drop"
  ) |> 
  relocate(WDIR, .before = WSPD) |> 
  relocate(MWD, .before = ATMP)


# Plot annual mean values for each variable
for (var in variables) {
  p <- ggplot(buoy_data_annual,
              aes(x = YYYY,
                  y = .data[[var]])) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    labs(x = "Date",
         y = variables_meta[[var]],
         title = paste("Annual Time Series - Boston Harbor Buoy",
                       variables_meta[[var]], sep = "\n")) +
    scale_color_brewer(palette = "Set2") +
    scale_x_continuous(
      breaks = seq(min(buoy_data_annual$YYYY), max(buoy_data_annual$YYYY), by = 2))
  
  print(p)
}

# Plot Simple Mean Wind Direction 2003 - 2014 for comparison with DKP
for (var in list("WDIR_simple")) {
  p <- ggplot(filter(buoy_data_annual, YYYY %in% c(2003:2014)),
              aes(x = YYYY,
                  y = .data[[var]])) +
    geom_point(color = "blue") +
    geom_line(color = "blue") +
    geom_smooth(method = "lm", se = FALSE) +
    labs(x = "Date",
         y = variables_meta[[var]],
         title = paste("Annual Time Series - Boston Harbor Buoy",
                       variables_meta[[var]], sep = "\n")) +
    scale_color_brewer(palette = "Set2") +
    scale_x_continuous(
      breaks = seq(min(buoy_data_annual$YYYY), max(buoy_data_annual$YYYY), by = 2))
  
  print(p)
}





# ---- Export Annual Data ----
# write_csv(buoy_data_annual, here::here("data", "summary_data", "buoy_44013_annual.csv"))


