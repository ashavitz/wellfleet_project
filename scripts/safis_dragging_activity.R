# ---- Header ----
#'--------------------------------------
#' SAFIS Dragging Data from DMF
#' Shellfish growing areas CCB 6,8,9,17,20
#' 2010-2022
#' SPECIES INCLUDES
# CLAM, ARK, BLOOD
# CLAM, QUAHOG, NORTHERN
# CLAM, SURF, ATLANTIC
# MUSSEL, BLUE
# OYSTER, EASTERN
# SCALLOP, BAY
# SCALLOP, SEA
# WHELK, CHANNELED
# WHELK, KNOBBED
# WHELK, LIGHTNING
#'--------------------------------------

# ---- Map of Growing Areas ----

# See Mass.gov shellfish classification areas: 
# https://www.mass.gov/info-details/shellfish-classification-areas

library(png)
img <- readPNG("data/safis/cc_shellfish_growing_areas_map.png")
plot(as.raster(img))


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


# ---- Load and Visualize SAFIS Data ----

# Load data
# Note: Live pound data for 2012 could not be provided as it wouold violate confidentiality
dredge_data <- read_csv("data/safis/DMF_CC_Shellfish_Dredging_data.csv")

# Time series bar plots of live pounds and trip count data on dual axis plot
ggplot(dredge_data,
       aes(x = as.character(YEAR))) +
  geom_bar(aes(y = `LIVE POUNDS`, fill = "Live Pounds"),
           stat = "identity",, width = 0.4, position = position_nudge(x = -0.2)) +
  
  # Plot trip count scaled by 1000 to align with Live Pounds axis
  geom_bar(aes(y = `TRIP COUNT` * 1000, fill = "Trip Count"),
           stat = "identity", width = 0.4, position = position_nudge(x = 0.2)) +
  
  # Add a transparent bar or rectangle for 2012
  geom_bar(data = subset(dredge_data, YEAR == 2012),
           aes(y = 200000, fill = "Data Unavailable"),
           stat = "identity", alpha = 0.4,
           width = 0.4, position = position_nudge(x = -0.2)) +
  
  # Add text label over the bar
  geom_text(data = subset(dredge_data, YEAR == 2012),
            aes(y = 160000, label = "Unavailable:\nConfidential"),
            position = position_nudge(x = -0.2),
            size = 3, color = "red") +
  
  # Create second axis scaled by 1/1000 to align with Trip Count
  scale_y_continuous(
    sec.axis = sec_axis(~ . / 1000, name = "Trip Count"),
    expand = c(0,0)) + 
  labs(title = "Dragging Activity for Shellfish Growing Areas CCB 6,8,9,17,20",
       x = "Year",
       y = "Live Pounds",
       caption = "2012 Trip Count Data Unavailable due to Confidentiality") + 
  
  # Set fill color manually and define legend
  scale_fill_manual(
    name = "Metric",
    values = c("Live Pounds" = "skyblue", 
               "Trip Count" = "orange",
               "Data Unavailable" = "grey"))
