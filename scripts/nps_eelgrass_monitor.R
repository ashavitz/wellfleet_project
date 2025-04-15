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
# Scale reproductive shoot counts according to Density_Source         
         Reproductive_Shoots = ifelse(Density_Source %in% c("BL", "BR", "TL", "TR"),
                                      Reproductive_Shoots * 4,
                                      Reproductive_Shoots),
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
       y = "Mean Percent Cover")

ggplot(dh_data_summary,
       aes(x = Date, y = Percent_Cover, color = Transect)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  labs(title = "Mean Percent Cover Over Time (with smoothing)",
       y = "Mean Percent Cover")


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
       y = "Mean Count Reproductive Shoots")

ggplot(dh_data_summary_fruiting,
       aes(x = Year, y = Reproductive_Shoots, color = Transect)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  labs(title = "Mean Count Reproductive Shoots Over Time (with smoothing)",
       y = "Mean Count Reproductive Shoots")


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

ggplot(dh_data_summary_fruiting,
       aes(x = as.character(Year), y = Percent_Cover, fill = repro_shoot_prop)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(
    labels = scales::percent_format(),
    limits = c(0, 1)
  ) +
  facet_wrap(~Transect) + 
  labs(title = "% Cover & Proportion Reproductive Shoots by Year",
       caption = "Only May through September Included")



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

ggplot(filter(dh_fruiting_long, Metric != "Percent_Cover"),
       aes(x = as.character(Year), y = Value, fill = Metric)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(
    labels = scales::percent_format(),
    limits = c(0, 1)
  ) +
  facet_wrap(~Transect) + 
  labs(title = "% Cover & Proportion of Reproductive Shoots by Year",
       caption = "Only May through September Included")

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
      "prop_wasting_none" = "gray")) +
  facet_wrap(~Transect) +
  labs(title = "Wasting Status Proportion by Year")

ggplot(dh_data_summary_wasting,
       aes(x = as.character(Year), y = Percent_Cover, fill = prop_wasting_high)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(
    labels = scales::percent_format(),
    limits = c(0, 1)) +
  scale_fill_gradient(low = "gray", high = "red") +
  facet_wrap(~Transect) + 
  labs(title = "% Cover & Proportion High Wasting Disease by Year")


# ---- Load PAR and Water Temp Data ----
# Load PAR data
par_data <- read_csv("data/nps_duck_harbor/duck_harbor_PAR.csv") |> 
  mutate(date_time = mdy_hm(`Date/Time (UTC-4:00)`)) |> relocate(date_time, .before = PAR1) |> select(-`Date/Time (UTC-4:00)`)


# Load water temperature data
water_temp_data_a <- read_csv("data/nps_duck_harbor/Water_Temp.A@CACO_Seagrass_MA20_1.EntireRecord_20250321.csv", skip = 14)
water_temp_data_b <- read_csv("data/nps_duck_harbor/Water_Temp.B@CACO_Seagrass_MA20_1.EntireRecord_20250321.csv", skip = 14)
water_temp_data_c <- read_csv("data/nps_duck_harbor/Water_Temp.C@CACO_Seagrass_MA20_1.EntireRecord_20250321.csv", skip = 14)






