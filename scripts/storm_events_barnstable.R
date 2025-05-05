# ---- Header ----
#'--------------------------------------
#' NOAA Storm Events Database: https://www.ncdc.noaa.gov/stormevents/
#' Barnstable County 1999-01-01 through 2024-12-31
#' All Events
#' (Data is available as early as 1950. 1999 was selected because data is less organized and harder
#' to analyze prior to 1999 for Barnstable County)
#' Metadata: 
#' https://www.ncei.noaa.gov/pub/data/swdi/stormevents/csvfiles/Storm-Data-Bulk-csv-Format.pdf
#'--------------------------------------

# ---- TODO ----


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


# ---- Load and Visualize NOAA Storm Events Data ----

barnstable_events_1999_2024 <- bind_rows(
  read_csv("data/noaa_storm_events/storm_data_barnstable_1999_2010.csv"),
  read_csv("data/noaa_storm_events/storm_data_barnstable_2011_2024.csv")
) |> 
  select(-ABSOLUTE_ROWNUMBER)

# Clean data
storm_events <- barnstable_events_1999_2024 |> 
  select(c(BEGIN_DATE, EVENT_TYPE, MAGNITUDE)) |> 
  mutate(date = mdy(BEGIN_DATE),
         year = year(date),
         month = month(date), 
         day = day(date),
         event_type = EVENT_TYPE, 
         magnitude = MAGNITUDE
         ) |> 
  select(-c(BEGIN_DATE, EVENT_TYPE, MAGNITUDE)) |> 
  # remove irrelevant events types
  filter(event_type %in% c("Coastal Flood", "Flash Flood", "Flood", "High Surf", "High Wind",
                           "Storm Surge/Tide", "Strong Wind", "Thunderstorm Wind", "Tornado",
                           "Tropical Storm", "Winter Storm")) |>   
  # remove duplicates
  distinct() |> 
  # Some events appear to be recorded many times in a single day. 
  # Condense into single day, keeping maximum magnitude if applicable
  group_by(date, year, month, day, event_type) |> 
  summarize(magnitude_max = max(magnitude), .groups = "drop")

# Plot event frequency over time
ggplot(storm_events, 
       aes(x = as.character(year))) + 
  geom_bar() +
  facet_wrap(~event_type) +
  labs(title = "Barnstable County - NOAA Storm Event Frequeny",
       subtitle = "1999 - 2024", 
       x = "Year", 
       y = "Frequency of Recorded Events")

### Plot max magnitude (knots) of all wind events

# Filter only wind events
wind_events <- storm_events |> 
  filter(event_type %in% c("High Wind", "Strong Wind", "Thunderstorm Wind",
                           "Tornado", "Tropical Storm", "Winter Storm"))

# Determine max
max_mag <- max(wind_events$magnitude_max, na.rm = TRUE)

for (yr in unique(wind_events$year)) {
  data_year <- filter(wind_events, year == yr)
  
  # Skip if all magnitudes are NA
  if (all(is.na(data_year$magnitude_max))) next
  
  start_date <- as.Date(paste0(yr, "-01-01"))
  end_date <- as.Date(paste0(yr, "-12-31"))
  
  p <- ggplot(data_year,
              aes(x = date, y = magnitude_max, fill = event_type)) +
    geom_col(width = 0.8) +  # Set consistent bar thickness
    scale_x_date(
      limits = c(start_date, end_date),
      breaks = seq(start_date, end_date, by = "1 month"),
      date_labels = "%b"
    ) +
    scale_y_continuous(limits = c(0, max_mag)) +
    labs(
      title = paste("Wind Events in", yr),
      x = "Date",
      y = "Max Magnitude (knots)",
      fill = "Event Type"
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  print(p)
}
