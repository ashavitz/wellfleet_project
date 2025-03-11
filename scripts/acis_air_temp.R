# ---- All ----

#'--------------------------------------
#' SC ACIS Climate Data
#' https://scacis.rcc-acis.org/
#' Single-Station Products: Monthly Summarized Data
#' Variables: Avg temp, Min temp, Max temp, Precipitation
#' Year range: 1893 - 2025
#' Station: Hyannis
#'--------------------------------------


library(readr) # for reading in files
library(lubridate) # for date time formats
library(dplyr) # for data manipulation and transformation
library(ggplot2) # for data visualization

# Import csv file from local folder
hyannis_climate_data <- read_csv("data/acis_data/climate_date_hyannis_1893_2025.csv")

# Fix data types
hyannis_climate_data <- hyannis_climate_data |> 
  mutate(
    # convert Date to date format, yyyy-mm-01
    Date = ymd(paste0(Date, "-01")),
    
    # add year and month columns
    Year = year(Date),
    Month = month(Date),
    MonthName = factor(month.name[Month], levels = month.name),
    
    # convert missing values "M" and trace values "T" to NA
    across(c(MeanAvgTemperature, LowestMinTemperature,
             HighestMaxTemperature, TotalPrecipitation),
           ~ na_if(., "M") |> na_if("T")),
    
    # convert variables to numeric
    across(c(MeanAvgTemperature, LowestMinTemperature,
             HighestMaxTemperature, TotalPrecipitation),
            as.numeric)
  )

# Extract only data since 1995
hyannis_data_1995_2025 <- filter(hyannis_climate_data, Year >= 1995)


# graph all 4 variables over time
ggplot(data = hyannis_data_1995_2025,
                mapping = aes(x = Date)) +
  geom_point(mapping = aes(y = MeanAvgTemperature)) +
  geom_smooth(mapping = aes(y = MeanAvgTemperature,
                           color = "MeanAvgTemperature"),
              method = "lm",
              se = FALSE) +
  geom_point(mapping = aes(y = LowestMinTemperature)) +
  geom_smooth(mapping = aes(y = LowestMinTemperature,
                            color = "LowestMinTemperature"),
              method = "lm",
              se = FALSE) +
  geom_point(mapping = aes(y = HighestMaxTemperature)) +
  geom_smooth(mapping = aes(y = HighestMaxTemperature,
                            color = "HighestMaxTemperature"),
              method = "lm",
              se = FALSE) +
  scale_color_manual(values = c("MeanAvgTemperature" = "grey", 
                                "LowestMinTemperature" = "blue", 
                                "HighestMaxTemperature" = "red")) +
  facet_wrap(~MonthName) +
  labs(title = "Air Temperature Change 1995 - 2025", 
       y = "Temperature (°F)", color = "Temperature Variables")


# Same 4 variables over time
ggplot(data = hyannis_data_1995_2025,
       mapping = aes(x = Date)) +
  geom_line(mapping = aes(y = MeanAvgTemperature,
                            color = "MeanAvgTemperature")) +
  geom_line(mapping = aes(y = LowestMinTemperature,
                            color = "LowestMinTemperature")) +
  geom_line(mapping = aes(y = HighestMaxTemperature,
                            color = "HighestMaxTemperature")) +
  scale_color_manual(values = c("MeanAvgTemperature" = "grey", 
                                "LowestMinTemperature" = "blue", 
                                "HighestMaxTemperature" = "red")) +
  facet_wrap(~MonthName) +
  labs(title = "Air Temperature Change 1995 - 2025", 
       y = "Temperature (°F)", color = "Temperature Variables")
