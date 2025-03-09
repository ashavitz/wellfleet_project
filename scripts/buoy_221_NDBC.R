#'--------------------------------------
#' CDIP221 Buoy
#' Cape Cod Bay, MA
#' Owned and maintained by Woods Hole Group/NERACOOS
#' Data provided by Scripps Institution of Oceanography
#' National Data Buoy Center Station 44090
#' Waverider Buoy
#' 41.840 N 70.329 W (41°50'24" N 70°19'43" W)
#' Site elevation: sea level
#' Sea temp depth: 0.46 m below water line
#'--------------------------------------

library(readr) # for reading in files
library(lubridate) # for date time formats
library(dplyr) # for data manipulation and transformation

# ---- csv import from NDBC ----
#' -------------------------------------
#' csv import from NDBC
#' .txt files downloaded from: 
#' https://www.ndbc.noaa.gov/station_history.php?station=44090
#' -------------------------------------

# Read in all data files
# 2016
file_path_buoy_221 <- "data/buoy_221_meteorological/44090h2016.txt"
header_info_2016 <- read_table(file_path_buoy_221,
                               n_max = 2,
                               col_names = FALSE)

buoy_data_2016 <- read_table(file_path_buoy_221,
                             col_names = as.character(header_info_2016[1, ]),
                             skip = 2)

# Store units separately
buoy_units_2016 <- as.character(header_info_2016[2, ])



# ...other years...
# 2023
file_path_buoy_221 <- file_path_buoy_221 <- "data/buoy_221_meteorological/44090h2023.txt"
header_info_2023 <- read_table(file_path_buoy_221,
                               n_max = 2,
                               col_names = FALSE)

buoy_data_2023 <- read_table(file_path_buoy_221,
                             col_names = as.character(header_info_2023[1, ]),
                             skip = 2)