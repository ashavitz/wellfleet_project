#'--------------------------------------
#' SC ACIS Climate Data
#' https://scacis.rcc-acis.org/
#' Single-Station Products: Monthly Summarized Data
#' Variables: Avg temp, Min temp, Max temp, Precipitation
#' Year range: 1893 - 2025
#' Station: Hyannis
#'--------------------------------------

# TODO: Calculate daily mean and high values to reduce size of data sets.
#       Only include days with complete data

# TODO: Initial visualization - variable change over time

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


# graph all 4 variables over time
ggplot(data = hyannis_climate_data,
                mapping = aes(x = Date)) +
  geom_smooth(mapping = aes(y = MeanAvgTemperature,
                            color = "MeanAvgTemperature")) +
  geom_smooth(mapping = aes(y = LowestMinTemperature,
                            color = "LowestMinTemperature")) +
  geom_smooth(mapping = aes(y = HighestMaxTemperature,
                            color = "HighestMaxTemperature")) +
  scale_color_manual(values = c("MeanAvgTemperature" = "grey", 
                                "LowestMinTemperature" = "blue", 
                                "HighestMaxTemperature" = "red")) +
  facet_wrap(~MonthName) +
  labs(title = "Air Temperature Change 1893 - 2025", 
       y = "Temperature (°F)", color = "Temperature Variables")
