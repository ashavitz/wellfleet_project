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
# TODO: Import data

# TODO: Calculate daily mean and high values to reduce size of data sets.
#       Only include days with complete data

# TODO: Initial visualization - variable change over time
#'--------------------------------------

library(readr) # for reading in files
library(lubridate) # for date time formats
library(dplyr) # for data manipulation and transformation
library(tidyr) # for tidying and reshaping data
library(ggplot2) # for visualization


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

# Read in 1995 - 2016 data
years <- 1995:2016
buoy_data_list <- list()

# Loop through years and read files into the list
for (year in years) {
  file_path <- paste0("data/buoy_44013_data/44013h", year, "_cleaned.txt")
  buoy_data_list[[as.character(year)]] <- 
    read_table(file_path,
               col_names = TRUE,
               col_types = cols(.default = col_character())) # read all as chr to standardize
}

# Read in 2017 - 2024 data, taking 1st row as header and skipping 2nd row metadata label
years <- 2017:2024
for (year in years) {
  file_path <- paste0("data/buoy_44013_data/44013h", year, "_cleaned.txt")
  header_info <- read_table(file_path,
                            n_max = 1,
                            col_names = FALSE)
  buoy_data_list[[as.character(year)]] <-
    read_table(file_path,
               as.character(header_info[1, ]),
               skip = 2, 
               col_types = cols(.default = col_character())) # read all as chr to standardize)
}
rm(header_info)

# # Access data for a specific year (e.g., 2005)
# buoy_data_list[["2005"]]
# str(buoy_data_list[["1995"]])
# View(buoy_data_list[["1995"]])


buoy_data <- bind_rows(buoy_data_list, .id = "year") |> 
  mutate(YYYY = coalesce(YY, YYYY, `#YY`),
         YYYY = if_else(nchar(YYYY) == 2, paste0("19", YYYY), YYYY)) |> 
  select(-c(YY, `#YY`)) |> 
  relocate(YYYY, MM, DD, hh, mm, .after = year)

buoy_data_cleaned <- buoy_data |> 
  filter(year != YYYY)


# standardize column headers and variable class

# merge all data sets



