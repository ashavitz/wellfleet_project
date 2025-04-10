# ---- Header ----
#'--------------------------------------
#' Duck Harbor, Wellfleet MA
#' NPS Eelgrass Monitoring Data 2003-2024
#' Complete data set provided by Holly Plaisted (NPS)
#' Corresponding data at seagrassnet.org
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


# ---- Load NPS Data ----

# Load monitoring data (% cover, wasting disease, seed production) . Isolate relevant data for current analysis
dh_data_all <- read_csv("data/nps_duck_harbor/duckharbor_zm_data_03_24.csv")
dh_data <- select(dh_data_all, c(Location, Date, Transect, Percent_Cover, Density_Source, Fruits, Wasting)) |> 
  mutate(Date = mdy(Date),
         Reproductive_Shoots = Fruits) |> 
  select(-Fruits)

# visdat::vis_dat(dh_data)

# Clean data

# Remove Percent_Cover NA rows
dh_data <- filter(dh_data, is.na(Percent_Cover) == FALSE)

# For most instances when Percent_Cover == 0, Density_Source and Wasting are NA, but there are
# a few inconsistencies. Standardize by making all NA where Percent_Cover == 0
dh_data <- dh_data |> 
  mutate(Density_Source = ifelse(Percent_Cover == 0, NA, Density_Source),
         Wasting = ifelse(Percent_Cover ==0, NA, Wasting))

# Load PAR data
par_data <- read_csv("data/nps_duck_harbor/duck_harbor_PAR.csv") |> 
  mutate(date_time = mdy_hm(`Date/Time (UTC-4:00)`)) |> relocate(date_time, .before = PAR1) |> select(-`Date/Time (UTC-4:00)`)
  

# Load water temperature data
water_temp_data_a <- read_csv("data/nps_duck_harbor/Water_Temp.A@CACO_Seagrass_MA20_1.EntireRecord_20250321.csv", skip = 14)
water_temp_data_b <- read_csv("data/nps_duck_harbor/Water_Temp.B@CACO_Seagrass_MA20_1.EntireRecord_20250321.csv", skip = 14)
water_temp_data_c <- read_csv("data/nps_duck_harbor/Water_Temp.C@CACO_Seagrass_MA20_1.EntireRecord_20250321.csv", skip = 14)



# ---- Visualize Monitoring Data ----

# Density_Source entries indicate which 1/16th m quadrant of the 1/4 m quadrat the density was 
# measured (BL = bottom left; TL = top left; BR = bottom right; TR = top right; Entire = Entire).
# When cover is < 20%, all shoots in the 1/4 m are counted, but when above 20, only the shoots 
# in 1/16th m are counted

# Scale reproductive shoot counts according to Density_Source
dh_data <- dh_data |> 
  mutate(Reproductive_Shoots = ifelse(Density_Source %in% c("BL", "BR", "TL", "TR"),
                                      Reproductive_Shoots * 4,
                                      Reproductive_Shoots))

# Calculate mean % cover & mean count of reproductive shoots per transect per year
dh_data_summary <- dh_data |> 
  group_by(Transect, Date) |> 
  summarize(Percent_Cover = mean(Percent_Cover),
            Reproductive_Shoots = mean(Reproductive_Shoots)) |> 
  ungroup()

# Plot mean % cover over time
ggplot(dh_data_summary,
       aes(x = Date, y = Percent_Cover, color = Transect)) +
  geom_line() +
  labs(title = "Mean Percent Cover Over Time",
       y = "Mean Percent Cover")

ggplot(dh_data_summary,
       aes(x = Date, y = Percent_Cover, color = Transect)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  labs(title = "Mean Percent Cover Over Time (with smoothing)",
       y = "Mean Percent Cover")

# Plot mean count of reproductive over time
ggplot(dh_data_summary,
       aes(x = Date, y = Reproductive_Shoots, color = Transect)) +
  geom_line() +
  labs(title = "Mean Count Reproductive Shoots Over Time",
       y = "Mean Count Reproductive Shoots")

ggplot(dh_data_summary,
       aes(x = Date, y = Reproductive_Shoots, color = Transect)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  labs(title = "Mean Count Reproductive Shoots Over Time (with smoothing)",
       y = "Mean Count Reproductive Shoots")


# Calculate counts for each categorization of wasting disease by transect by year
# Plot (bar graph) over time









