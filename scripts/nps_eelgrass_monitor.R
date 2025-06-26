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
library(dplyr) # for data manipulation and transformation
library(ggplot2) # for visualization
library(lubridate) # for date time formats
library(paletteer) # for color palettes
library(readr) # for reading in files
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


# ---- Load NPS Monitoring Data ----

# Load monitoring data (% cover, wasting disease, seed production) . Isolate relevant data for current analysis
dh_data_all <- read_csv("data/nps_duck_harbor/duckharbor_zm_data_03_24.csv")
dh_data <- select(dh_data_all,
                  c(Location, Date, Transect,
                    Percent_Cover, Density_Source,
                    `Shoot Density_0.25m2`, Fruits, Wasting)) |> 
  mutate(Date = mdy(Date),
         Reproductive_Shoots = Fruits) |> 
  select(-Fruits)

# visdat::vis_dat(dh_data)

# Clean data

# NOTE - Density_Source entries indicate which 1/16th m quadrant of the 1/4 m quadrat the density was 
# measured (BL = bottom left; TL = top left; BR = bottom right; TR = top right; Entire = Entire).
# When cover is < 20%, all shoots in the 1/4 m are counted, but when above 20, only the shoots 
# in 1/16th m are counted

# Remove row where Percent_Cover or shoot density has NA values
dh_data <- dh_data |> 
  filter(!is.na(Percent_Cover) & !is.na(`Shoot Density_0.25m2`)) |> 
  
# For most instances when Percent_Cover == 0, Density_Source and Wasting are NA, but there are
# a few inconsistencies. Standardize by making all NA where Percent_Cover == 0
  mutate(Density_Source = ifelse(Percent_Cover == 0, NA, Density_Source),
         Wasting = ifelse(Percent_Cover == 0, NA, Wasting),
# Create separate columns for Wasting == c(High, Low, Tr, NA) in order to summarize n()
         wasting_high = ifelse(Wasting == "High", 1, 0),
         wasting_low = ifelse(Wasting == "Low", 1, 0),
         wasting_trace = ifelse(Wasting == "Tr", 1, 0),
         wasting_none = ifelse(Wasting == "0", 1, 0)
)


# ---- Visualize Monitoring Data ----

# Calculate mean % cover & mean count of reproductive shoots per transect per year
dh_data_summary <- dh_data |> 
  group_by(Transect, Date) |> 
  summarize(across(c(Percent_Cover, Reproductive_Shoots, `Shoot Density_0.25m2`),
                   \(x) mean(x, na.rm = TRUE)),
            wasting_high = sum(wasting_high, na.rm = TRUE),
            wasting_low = sum(wasting_low, na.rm = TRUE),
            wasting_trace = sum(wasting_trace, na.rm = TRUE),
            wasting_none = sum(wasting_none, na.rm = TRUE),
            .groups = "drop") |> 
  # calculate proportion of total shoot density
  mutate(repro_shoot_prop = Reproductive_Shoots / `Shoot Density_0.25m2`) |> 
  relocate(repro_shoot_prop, .before = wasting_high)

# Plot mean % cover over time
ggplot(dh_data_summary,
       aes(x = Date, y = Percent_Cover, color = Transect)) +
  geom_line() +
  labs(title = "Mean Percent Cover Over Time",
       y = "Mean Percent Cover") +
  scale_color_paletteer_d("yarrr::google")

ggplot(dh_data_summary,
       aes(x = Date, y = Percent_Cover, color = Transect)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  labs(title = "Mean Percent Cover Over Time (with smoothing)",
       y = "Mean Percent Cover") +
  scale_color_paletteer_d("yarrr::google")


# Plot reproductive shoots and proportion of reproductive shoots

# For examining reproductive shoots, only include May through September (fruiting months)
dh_data_summary_fruiting <- dh_data_summary |> 
  filter(month(Date) %in% c(5,6,7,8,9)) |> 
# Average by year to reduce to single data point per year
# NOTE - It is likely more appropriate to do this step earlier. May need to revise
  group_by(Transect, Year = year(Date)) |> 
  summarize(repro_shoot_prop = mean(repro_shoot_prop),
            Reproductive_Shoots = mean(Reproductive_Shoots), 
            Percent_Cover = mean(Percent_Cover)/100,
            .groups = "drop") |> 
  mutate(non_repro_shoot_prop = 1 - repro_shoot_prop)


# Plot mean count of reproductive over time
ggplot(dh_data_summary_fruiting,
       aes(x = Year, y = Reproductive_Shoots, color = Transect)) +
  geom_line() +
  labs(title = "Mean Count Reproductive Shoots Over Time",
       y = "Mean Count Reproductive Shoots") +
  scale_color_paletteer_d("yarrr::google")

ggplot(dh_data_summary_fruiting,
       aes(x = Year, y = Reproductive_Shoots, color = Transect)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  labs(title = "Mean Count Reproductive Shoots Over Time (with smoothing)",
       y = "Mean Count Reproductive Shoots") +
  scale_color_paletteer_d("yarrr::google")


# Plot proportion of reproductive shoots out of shoot density (total counted shoots)
# ggplot(dh_data_summary_fruiting,
#        aes(x = as.character(Year), y = repro_shoot_prop)) +
#   geom_bar(stat = "identity") +
#   scale_y_continuous(
#     labels = scales::percent_format(),
#     limits = c(0, 1)
#     ) +
#   facet_wrap(~Transect) + 
#   labs(title = "Proportion of Reproductive Shoots by Year",
#        caption = "Only May through September Included")


# Create facet labels for faceted plots
transect_labels <- c(
  "A" = "Transect A (Shallow)",
  "B" = "Transect B (Mid Depth)",
  "C" = "Transect C (Deep)"
)

# Bar plot showing percent cover by year, colored by proportion of shoots which are reproductive
ggplot(dh_data_summary_fruiting,
       aes(x = as.character(Year), y = Percent_Cover, fill = repro_shoot_prop)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(
    labels = scales::percent_format(),
    limits = c(0, 1)
  ) +
  facet_wrap(~Transect, labeller = labeller(Transect = transect_labels)) + 
  labs(
    title = "% Cover & Proportion Reproductive Shoots by Year",
    caption = "Only May through September Included",
    fill = "Reproductive
    Proportion",
    x = "Year",
    y = "Percent Cover"
  ) +
  theme(
    strip.text = element_text(size = 8),
    plot.title = element_text(hjust = 0.5, face = "bold")
  )


# Pivot longer for alternative bar plots
dh_fruiting_long <- dh_data_summary_fruiting |> 
  pivot_longer(cols = c(repro_shoot_prop, non_repro_shoot_prop, Percent_Cover),
               names_to = "Metric",
               values_to = "Value")


# ggplot(filter(dh_fruiting_long, Metric != "non_repro_shoot_prop"),
#        aes(x = as.character(Year), y = Value, fill = Metric)) +
#   geom_bar(stat = "identity", position = "dodge") +
#   scale_y_continuous(
#     labels = scales::percent_format(),
#     limits = c(0, 1)
#   ) +
#   facet_wrap(~Transect) + 
#   labs(title = "% Cover & Proportion of Reproductive Shoots by Year",
#        caption = "Only May through September Included")


# Stacked bar plot showing % Cover & Proportion of Reproductive Shoots by Year
ggplot(filter(dh_fruiting_long, Metric != "Percent_Cover"),
       aes(x = as.character(Year), y = Value, fill = Metric)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(
    labels = scales::percent_format(),
    limits = c(0, 1)
  ) +
  facet_wrap(~Transect, labeller = labeller(Transect = transect_labels)) +
  labs(title = "Proportion of Reproductive Shoots by Year",
       caption = "Only May through September Included",
       x = "Year",
       y = "Proportion of Total Shoots",
       fill = "Cover Type"
       ) +
  scale_fill_manual(
    values = c("non_repro_shoot_prop" = "#A9B7A9", "repro_shoot_prop" = "darkgreen"),
    labels = c("non_repro_shoot_prop" = "Non-Reproductive Shoots", 
               "repro_shoot_prop" = "Reproductive Shoots")
  ) +
  theme(
    strip.text = element_text(size = 8),
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

# ggplot(dh_data_summary_fruiting,
#        aes(x = as.character(Year), y = repro_shoot_prop, fill = Transect)) +
#   geom_bar(stat = "identity", position = position_dodge()) +
#   scale_y_continuous(
#     labels = scales::percent_format(),
#     limits = c(0, 1)
#   ) +
#   labs(title = "Proportion of Reproductive Shoots by Year",
#        caption = "Only May through September Included")


# Wasting disease

# Summarize proportions of each classification of wasting level by year
dh_data_summary_wasting <- dh_data_summary |> 
  group_by(Transect, Year = year(Date)) |> 
  summarize(Percent_Cover = mean(Percent_Cover)/100,
            across(starts_with("wasting_"), ~sum(.x, na.rm = TRUE))) |> 
  mutate(total = rowSums(across(starts_with("wasting_"))),
         across(starts_with("wasting_"), ~ .x / total, .names = "prop_{.col}"))
  
# pivot wasting proportions long for plotting
dh_wasting_long <- dh_data_summary_wasting |> 
  pivot_longer(cols = c(prop_wasting_high, prop_wasting_low, prop_wasting_trace, prop_wasting_none),
               names_to = "wasting_status",
               values_to = "proportion")

# Plot proportions of wasting disease impact level (bar graph) over time
ggplot(dh_wasting_long,
       aes(x = as.character(Year), y = proportion, fill = wasting_status)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(
    labels = scales::percent_format(),
    limits = c(0, 1)) + 
  scale_fill_manual(
    values = c(
      "prop_wasting_high" = "red",
      "prop_wasting_low" = "skyblue",
      "prop_wasting_trace" = "lightgreen",
      "prop_wasting_none" = "gray"),
    labels = c(
      "prop_wasting_high" = "High",
      "prop_wasting_low" = "Low",
      "prop_wasting_trace" = "Trace",
      "prop_wasting_none" = "None")) +
  facet_wrap(~Transect, labeller = labeller(Transect = transect_labels)) +
  labs(title = "Wasting Status Proportion by Year",
       x = "Year",
       y = "Proportion") +
  theme(
    strip.text = element_text(size = 8),
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

ggplot(dh_data_summary_wasting,
       aes(x = as.character(Year), y = Percent_Cover, fill = prop_wasting_high)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(
    labels = scales::percent_format(),
    limits = c(0, 1)) +
  scale_fill_gradient(low = "gray", high = "red") +
  facet_wrap(~Transect, labeller = labeller(Transect = transect_labels)) +
  labs(title = "% Cover & Proportion High Wasting Disease by Year",
       x = "Year",
       y = "Percent Cover",
       fill = "Proportion with High
    Levels of Wasting") +
  theme(
    strip.text = element_text(size = 8),
    plot.title = element_text(hjust = 0.5, face = "bold")
  )


# ---- Load and Visualize PAR Data ----
# Load PAR data
par_data_raw <- read_csv("data/nps_duck_harbor/duck_harbor_PAR.csv")

par_data <- par_data_raw |>
  mutate(date_time = mdy_hm(`Date/Time (UTC-4:00)`)) |>
  select(c(date_time, Kd)) |> 
  filter(!is.na(Kd))

# Establish threshold Kd values based on NPS report (Good, Fair, Poor)
# Threshold values as defined by Kopp and Neckles (2009), and utilized in NPS (draft) report
# on Condition and Trends of Estuarine Water Quality and Seagrass in Cape Cod National Seashore
# Northeast Coastal and Barrier Network, 2003 – 2022
# Units: m-1
threshold_poor_fair <- 1.61
threshold_fair_good <- 0.92

# Plot all data scatter plot, x axis date time, y axis Kd
ggplot(par_data, aes(x = date_time, y = Kd)) +
  geom_point() + 
  
  # Add threshold lines
  geom_hline(yintercept = threshold_poor_fair,
             linetype = 'dashed', color = 'red', linewidth = 1, alpha = 0.5) +
  geom_hline(yintercept = threshold_fair_good,
             linetype = 'dashed', color = 'orange', linewidth = 1, alpha = 0.5) +
  # geom_smooth(method = lm) + 
  scale_x_datetime(
    limits = as.POSIXct(c("2018-01-01", "2025-01-01")),
    date_breaks = "1 year",
    date_labels = "%Y") +
  # Add text labels to denote threshold line meanings
  annotate("text", x = as.POSIXct("2020-01-01", tz = "UTC"), y = threshold_fair_good - 0.05,
    label = "Good", color = "darkgreen", size = 3) +
  annotate("text", x = as.POSIXct("2020-01-01", tz = "UTC"), y = threshold_fair_good + 0.05,
           label = "Fair", color = "orange", size = 3) + 
  annotate("text", x = as.POSIXct("2020-01-01", tz = "UTC"), y = threshold_poor_fair - 0.05,
           label = "Fair", color = "orange", size = 3) + 
  annotate("text", x = as.POSIXct("2020-01-01", tz = "UTC"), y = threshold_poor_fair + 0.05,
           label = "Poor", color = "red", size = 3) +
  labs(title = "PAR Attenuation (Kd) at Duck Harbor",
       x = "Datetime (Annual Intervals Displayed)",
       y = expression("Kd (m"^{-1}*")"))


# Create data frame with just Kd values, day of year, year,
# and Kd classification (Good, Fair, Poor)
par_data_quality <- par_data |> 
  mutate(year = year(date_time),
         day_of_year = yday(date_time),
         quality = case_when(
           Kd < 0.92 ~ "Good",
           Kd < 1.61 ~ "Fair",
           Kd >= 1.61 ~ "Poor")) |> 
  group_by(year, day_of_year, quality) |> 
  summarize(count = n(), .groups = "drop") |> 
  mutate(hours_measured_per_day = count / 4) 

# Reorder quality factor to control the stacking order
par_data_quality$quality <- factor(par_data_quality$quality, levels = c("Poor", "Fair", "Good"))

# Plot stacked bar chart, showing hours of Good, Fair, Poor per day of year, faceted by year
ggplot(par_data_quality,
       aes(x = factor(day_of_year), y = hours_measured_per_day, fill = quality)) +
  geom_bar(stat = "identity", position = "stack", color = "white", linewidth = 0.2) +
  scale_fill_manual(values = c("Poor" = "yellow2", "Fair" = "purple4", "Good" = "aquamarine4")) +
  scale_x_discrete(limits = as.character(140:230), breaks = seq(140, 230, by = 10)) + 
  labs(
    title = "PAR Attenuation Kd Quality Classification by Day of Year",
    x = "Day of Year",
    y = "Total Hours per Day",
    fill = "Quality"
  ) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        
        # Modify facet formatting
        strip.text = element_text(size = 10),
        strip.background = element_rect(fill = "grey", color = "black", size = 0.5),
        panel.spacing = unit(1, "lines"),
        axis.ticks.x = element_line()) +
  facet_wrap(~ year, scales = "free_x")

# ---- Load and Visualize Water Temp Data ----

# Load water temperature data and add transect identifier variable
water_temp_data_a <- 
  read_csv("data/nps_duck_harbor/Water_Temp.A@CACO_Seagrass_MA20_1.EntireRecord_20250321.csv", 
           skip = 14) |>
  mutate(Transect = "A")

water_temp_data_b <- 
  read_csv("data/nps_duck_harbor/Water_Temp.B@CACO_Seagrass_MA20_1.EntireRecord_20250321.csv",
           skip = 14) |> 
  mutate(Transect = "B")

water_temp_data_c <- 
  read_csv("data/nps_duck_harbor/Water_Temp.C@CACO_Seagrass_MA20_1.EntireRecord_20250321.csv",
           skip = 14) |> 
  mutate(Transect = "C")

# Bind temperature data into long data frame
water_temp_data <- 
  bind_rows(water_temp_data_a, water_temp_data_b, water_temp_data_c) |> 
  rename(Temp_C = Value, ) |> 
  rename(Date_Time = `Timestamp (UTC-04:00)`) |> 
  select(Date_Time, Temp_C, Transect) |> 
  mutate(Day = day(Date_Time), Month = month(Date_Time), Year = year(Date_Time))

# Remove raw stored raw data
remove(list = c("water_temp_data_a", "water_temp_data_b", "water_temp_data_c"))

# DAILY

# Create new data frame with daily temperature means
# Assuming the daily temperature fluctuations are minimal, and thus not checking whether
# individual days have a "complete" set of data
water_temp_daily <- water_temp_data |> 
  group_by(Year, Month, Day, Transect) |> 
  summarize(Temp_C = mean(Temp_C), .groups = "drop") |> 
  mutate(Date = make_date(Year, Month, Day))

# Plot all daily temperature over time
ggplot(water_temp_daily, 
       aes(x = Date, y = Temp_C, color = Transect)) +
  geom_point() +
  scale_color_paletteer_d("yarrr::google")


# ANNUAL 

# Determine valid years (where each month has >= 80% complete data) for each variable
# Calculate the annual mean only for valid years

water_temp_props <- water_temp_daily |> 
  # Group by year and month  
  group_by(Year, Month, Transect) |> 
  # Determine how many real daily measurements are recorded (not NA or NaN)
  summarize(Temp_C_count = sum(!is.na(Temp_C)), .groups = "drop") |> 
  # Create a column for days in the month determined with days_in_month()
  mutate(
    days_in_month = days_in_month(ymd(paste(Year, Month, "01", sep = "-")))) |> 
  # Calculate proportion of real daily measurements for each month 
  mutate(Temp_C_prop = Temp_C_count / days_in_month)

# Create a df showing which years are valid,
# based on each month having at least 80% complete daily data AND there being 12 total months
validity_by_year <- water_temp_props |> 
  select(Year, Month, Transect, ends_with("_prop")) |> 
  group_by(Year, Transect) |> 
  summarize(
    n_months = n(),
    all_months_above_80 = all(Temp_C_prop >= 0.8),
    .groups = "drop"
  ) |> 
  mutate(
    status = ifelse(n_months == 12 & all_months_above_80, "valid", "not valid")
  )

# Calculate annual mean temperature for valid years
water_temp_annual <- water_temp_daily |>
  group_by(Year, Transect) |> 
  summarize(Temp_C = mean(Temp_C), .groups = "drop") |> 
  left_join(validity_by_year, by = c("Year", "Transect")) |> 
  # For invalid years, change the calculated mean to NA
  mutate(
    Temp_C = ifelse(status == "valid", Temp_C, NA_real_)
  ) |> 
  select(-c(n_months, all_months_above_80, status))


# Plot annual mean temperatures
ggplot(water_temp_annual,
       aes(x = Year, y = Temp_C, color = Transect)) +
  geom_point() +
  geom_line() +
  labs(title = "Annual Mean Temperatures by Transect") +
  scale_color_paletteer_d("yarrr::google")


# SUMMER (June - September)

# Determine valid summers (where each month has >= 80% complete data) for each variable
# Calculate the summer mean only for valid summers

water_temp_props <- water_temp_daily |> 
  # Group by year and month  
  filter(Month %in% c(6,7,8,9)) |> 
  group_by(Year, Month, Transect) |> 
  # Determine how many real daily measurements are recorded (not NA or NaN)
  summarize(Temp_C_count = sum(!is.na(Temp_C)), .groups = "drop") |> 
  # Create a column for days in the month determined with days_in_month()
  mutate(
    days_in_month = days_in_month(ymd(paste(Year, Month, "01", sep = "-")))) |> 
  # Calculate proportion of real daily measurements for each month 
  mutate(Temp_C_prop = Temp_C_count / days_in_month)

# Create a df showing which summers are valid,
# based on each month having at least 80% complete daily data AND there being 4 total months
validity_by_year <- water_temp_props |> 
  select(Year, Month, Transect, ends_with("_prop")) |> 
  group_by(Year, Transect) |> 
  summarize(
    n_months = n(),
    all_months_above_80 = all(Temp_C_prop >= 0.8),
    .groups = "drop"
  ) |> 
  mutate(
    status = ifelse(n_months == 4 & all_months_above_80, "valid", "not valid")
  )

# Calculate summer mean temperature for valid summers
water_temp_summer <- water_temp_daily |>
  filter(Month %in% c(6,7,8,9)) |> 
  group_by(Year, Transect) |> 
  summarize(Temp_C = mean(Temp_C), .groups = "drop") |> 
  left_join(validity_by_year, by = c("Year", "Transect")) |> 
  # For invalid summers, change the calculated mean to NA
  mutate(
    Temp_C = ifelse(status == "valid", Temp_C, NA_real_)
  ) |> 
  select(-c(n_months, all_months_above_80, status))


# Plot summer mean temperatures
ggplot(water_temp_summer,
       aes(x = Year, y = Temp_C, color = Transect)) +
  geom_point() +
  geom_line() +
  labs(title = "Summer (June - Sep) Mean Temperatures by Transect") +
  scale_color_paletteer_d("yarrr::google")



# Compare transect annual mean temps to remote sensing SST

# Load NOAA CoastWatch SST data for gps point near Duck Harbor (41.94, -70.09)
# Originally accessed via NOAA CoastWatch ERDDAP
# https://coastwatch.noaa.gov/erddap/info/noaacwecnMURdaily/index.html
sst_data_dh <- read_csv("data/noaa_coastwatch_sst/sst_data_dh.csv") |> mutate(site = "duck_harbor") |> 
  mutate(Year = year(time))

# Calculate annual means and restructure data to match transect data frame
sst_dh_annual <-sst_data_dh |> 
  group_by(Year) |> 
  summarize(Temp_C = mean(sst), .groups = "drop") |> 
  mutate(Transect = "CoastWatch SST") |> 
  relocate(Transect, .after = Year)

# Bind temperature data frames
water_temp_annual <- water_temp_annual |> 
  bind_rows(sst_dh_annual)

# Plot annual mean temperatures
ggplot(water_temp_annual,
       aes(x = Year, y = Temp_C, color = Transect)) +
  geom_point() +
  geom_line() +
  labs(title = "Annual Mean Temperatures by Transect") +
  scale_color_paletteer_d("yarrr::google")


### Transect Temperature & Wasting Disease

# Join summer temperature data to wasting data set
dh_data_summary_wasting <- dh_data_summary_wasting |> 
  left_join(water_temp_summer, by = join_by(Transect, Year)) |> 
  rename(summer_mean_temp = Temp_C)

# Plot proportion high wasting, colored by summer temperature
ggplot(dh_data_summary_wasting,
       aes(x = as.character(Year), y = prop_wasting_high, fill = summer_mean_temp)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(
    labels = scales::percent_format(),
    limits = c(0, 1)) +
  scale_fill_gradient(low = "blue", high = "darkorange") +
  facet_wrap(~Transect, labeller = labeller(Transect = transect_labels)) +
  labs(title = "Proportion High Wasting Disease & Summer Mean Temp by Year",
       x = "Year",
       y = "Proportion with High Wasting Disease",
       fill = "Summer Mean Temp") +
  theme(
    strip.text = element_text(size = 8),
    plot.title = element_text(hjust = 0.5, face = "bold")
  )


### Create column for previous year mean summer temperature

# Separate wasting data into separate transect data frames
dh_data_summary_wasting <- dh_data_summary_wasting |>
  arrange(Transect, Year) |> 
  group_by(Transect) |> 
  mutate(
    summer_mean_temp_prev = lag(summer_mean_temp)
  ) |> 
  ungroup()

# Plot proportion high wasting, colored by previous summer temperature 
ggplot(dh_data_summary_wasting,
       aes(x = as.character(Year), y = prop_wasting_high, fill = summer_mean_temp_prev)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(
    labels = scales::percent_format(),
    limits = c(0, 1)) +
  scale_fill_gradient(low = "blue", high = "darkorange") +
  facet_wrap(~Transect, labeller = labeller(Transect = transect_labels)) +
  labs(title = "Proportion High Wasting Disease & Previous Summer Mean Temp by Year",
       x = "Year",
       y = "Proportion with High Wasting Disease",
       fill = "Previous Summer Mean Temp") +
  theme(
    strip.text = element_text(size = 8),
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

### Percent cover and wasting disease

# Plot percent cover, colored by summer temperature 
ggplot(dh_data_summary_wasting,
       aes(x = as.character(Year), y = Percent_Cover, fill = summer_mean_temp)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(
    labels = scales::percent_format(),
    limits = c(0, 1)) +
  scale_fill_gradient(low = "blue", high = "darkorange") +
  facet_wrap(~Transect, labeller = labeller(Transect = transect_labels)) +
  labs(title = "Percent Cover & Summer Mean Temp by Year",
       x = "Year",
       y = "Percent Cover",
       fill = "Summer Mean Temp") +
  theme(
    strip.text = element_text(size = 8),
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

# Plot percent cover, colored by previous summer temperature 
ggplot(dh_data_summary_wasting,
       aes(x = as.character(Year), y = Percent_Cover, fill = summer_mean_temp_prev)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(
    labels = scales::percent_format(),
    limits = c(0, 1)) +
  scale_fill_gradient(low = "blue", high = "darkorange") +
  facet_wrap(~Transect, labeller = labeller(Transect = transect_labels)) +
  labs(title = "Percent Cover & Previous Summer Mean Temp by Year",
       x = "Year",
       y = "Percent Cover",
       fill = "Previous Summer Mean Temp") +
  theme(
    strip.text = element_text(size = 8),
    plot.title = element_text(hjust = 0.5, face = "bold")
  )


### Plot same plots as above but with just transect A temperatures for longer time series

# Add column for transect A summe mean temperatures
dh_data_summary_wasting <- dh_data_summary_wasting |> 
  left_join(
    select(
      filter(water_temp_summer, Transect == "A"),
      c(Year, Temp_C)
    ), 
    by = c("Year")
  )|>
  rename(summer_mean_temp_A = Temp_C) |> 
  # create column for previous summer mean transect A temp
  arrange(Transect, Year) |> 
  group_by(Transect) |> 
  mutate(
    summer_mean_temp_A_prev = lag(summer_mean_temp_A)
  ) |> 
  ungroup()


# Plot proportion high wasting, colored by transect A summer temperature
ggplot(dh_data_summary_wasting,
       aes(x = as.character(Year), y = prop_wasting_high, fill = summer_mean_temp_A)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(
    labels = scales::percent_format(),
    limits = c(0, 1)) +
  scale_fill_gradient(low = "blue", high = "darkorange") +
  facet_wrap(~Transect, labeller = labeller(Transect = transect_labels)) +
  labs(title = "Proportion High Wasting Disease 
            & Transect A Summer Mean Temp by Year",
       x = "Year",
       y = "Proportion with High Wasting Disease",
       fill = "Transect A
Summer Mean Temp") +
  theme(
    strip.text = element_text(size = 8),
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

# Plot proportion high wasting, colored by transect A previous summer temperature 
ggplot(dh_data_summary_wasting,
       aes(x = as.character(Year), y = prop_wasting_high, fill = summer_mean_temp_A_prev)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(
    labels = scales::percent_format(),
    limits = c(0, 1)) +
  scale_fill_gradient(low = "blue", high = "darkorange") +
  facet_wrap(~Transect, labeller = labeller(Transect = transect_labels)) +
  labs(title = "Proportion High Wasting Disease &
          Transect A Previous Summer Mean Temp by Year",
       x = "Year",
       y = "Proportion with High Wasting Disease",
       fill = "Tansect A Previous
Summer Mean Temp") +
  theme(
    strip.text = element_text(size = 8),
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

### Percent cover and wasting disease

# Plot percent cover, colored by transect A summer temperature 
ggplot(dh_data_summary_wasting,
       aes(x = as.character(Year), y = Percent_Cover, fill = summer_mean_temp_A)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(
    labels = scales::percent_format(),
    limits = c(0, 1)) +
  scale_fill_gradient(low = "blue", high = "darkorange") +
  facet_wrap(~Transect, labeller = labeller(Transect = transect_labels)) +
  labs(title = "Percent Cover & Transect A 
       Summer Mean Temp by Year",
       x = "Year",
       y = "Percent Cover",
       fill = "Transect A 
Summer Mean Temp") +
  theme(
    strip.text = element_text(size = 8),
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

# Plot percent cover, colored by transect A previous summer temperature 
ggplot(dh_data_summary_wasting,
       aes(x = as.character(Year), y = Percent_Cover, fill = summer_mean_temp_A_prev)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(
    labels = scales::percent_format(),
    limits = c(0, 1)) +
  scale_fill_gradient(low = "blue", high = "darkorange") +
  facet_wrap(~Transect, labeller = labeller(Transect = transect_labels)) +
  labs(title = "Percent Cover & Transect A
       Previous Summer Mean Temp by Year",
       x = "Year",
       y = "Percent Cover",
       fill = "Transect A Previous
Summer Mean Temp") +
  theme(
    strip.text = element_text(size = 8),
    plot.title = element_text(hjust = 0.5, face = "bold")
  )


### Aggregate across transects (annual mean % cover across all transects)
dh_wasting_aggregated <- dh_data_summary_wasting |> 
  group_by(Year) |> 
  summarize(Percent_Cover = mean(Percent_Cover, na.rm = TRUE),
            prop_wasting_high = mean(prop_wasting_high, na.rm = TRUE), 
            summer_mean_temp_A = mean(summer_mean_temp_A, na.rm = TRUE),
            summer_mean_temp_A_prev = mean(summer_mean_temp_A_prev, na.rm = TRUE),
            .groups = "drop"
  )


# Same plots as above, but for aggregated data

# Plot proportion high wasting, colored by transect A summer temperature
ggplot(dh_wasting_aggregated,
       aes(x = as.character(Year), y = prop_wasting_high, fill = summer_mean_temp_A)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(
    labels = scales::percent_format(),
    limits = c(0, 1)) +
  scale_fill_gradient(low = "blue", high = "darkorange") +
  labs(title = "Aggregated Proportion High Wasting Disease 
            & Transect A Summer Mean Temp by Year",
       x = "Year",
       y = "Proportion with High Wasting Disease",
       fill = "Transect A
Summer Mean Temp") +
  theme(
    strip.text = element_text(size = 8),
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

# Plot proportion high wasting, colored by transect A previous summer temperature 
ggplot(dh_wasting_aggregated,
       aes(x = as.character(Year), y = prop_wasting_high, fill = summer_mean_temp_A_prev)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(
    labels = scales::percent_format(),
    limits = c(0, 1)) +
  scale_fill_gradient(low = "blue", high = "darkorange") +
  labs(title = "Aggregated Proportion High Wasting Disease &
          Transect A Previous Summer Mean Temp by Year",
       x = "Year",
       y = "Proportion with High Wasting Disease",
       fill = "Tansect A Previous
Summer Mean Temp") +
  theme(
    strip.text = element_text(size = 8),
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

### Percent cover and wasting disease

# Plot percent cover, colored by transect A summer temperature 
ggplot(dh_wasting_aggregated,
       aes(x = as.character(Year), y = Percent_Cover, fill = summer_mean_temp_A)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(
    labels = scales::percent_format(),
    limits = c(0, 1)) +
  scale_fill_gradient(low = "blue", high = "darkorange") +
  labs(title = "Aggregated Percent Cover & Transect A 
       Summer Mean Temp by Year",
       x = "Year",
       y = "Percent Cover",
       fill = "Transect A 
Summer Mean Temp") +
  theme(
    strip.text = element_text(size = 8),
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

# Plot percent cover, colored by transect A previous summer temperature 
ggplot(dh_wasting_aggregated,
       aes(x = as.character(Year), y = Percent_Cover, fill = summer_mean_temp_A_prev)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(
    labels = scales::percent_format(),
    limits = c(0, 1)) +
  scale_fill_gradient(low = "blue", high = "darkorange") +
  labs(title = "Aggregated Percent Cover & Transect A
       Previous Summer Mean Temp by Year",
       x = "Year",
       y = "Percent Cover",
       fill = "Transect A Previous
Summer Mean Temp") +
  theme(
    strip.text = element_text(size = 8),
    plot.title = element_text(hjust = 0.5, face = "bold")
  )
       
