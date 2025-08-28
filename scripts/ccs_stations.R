# ---- Header ----
#'--------------------------------------
#' Center for Coastal Studies - Cape Cod Water Quality Data
#' Data download source: https://www.capecodbay-monitor.org/download
#' Attribution and Citing: https://www.capecodbay-monitor.org/download
#' Map of stations: https://www.capecodbay-monitor.org/
#' Depth: surface
#'--------------------------------------

# ---- TODO ----



# ---- Load Libraries ----
library(broom) # for tidy model outputs
library(dplyr) # for data manipulation and transformation
library(ggplot2) # for data visualization
library(ggpmisc) # for annotating plots with p & R2 of fitted polynomial via stat_poly_eq()
library(lubridate) # for date time formats
library(paletteer) # for color palettes
library(patchwork) # for displaying graphs together
library(purrr) # for map
library(RColorBrewer) # for data viz color palettes
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

# ---- csv import from CCS ----

# Read in csv file from local directory
file_path_ccs_data <- "data/ccs_data_all/station_data.csv"
ccs_data_all <- read_csv(file_path_ccs_data, col_names = TRUE)

# Examining the data
# str(ccs_data_all)
# summary(ccs_data_all)
# visdat::vis_dat(ccs_data_all)
# skimr::skim(ccs_data_all)

#' Relevant station IDs and Names
  #' station id # : station name
    #' 1 : 5N
    #' 2 : 5S
    #' 3 : 5SX

#' Other neaerby station IDs and Names
    #' 4 : 6M
    #' 5 : 6S
    #' 11 : Blackfish Creek
    #' 22 : Great Island Channel
    #' 25 : Inner Pamet
    #' 29 : Inner Wellfleet Harbor
    #' 38 : Pamet
    #' 44 : Sunken Meadow
    #' 45 : Wellfleet Harbor
    #' 67 : North Sunken Meadow
    #' 75 : Pamet River
    #' 94 : WH-5

# Filter data frame to keep only data from geographically relevant stations
station_ids_wellfleet <- c(1,2,3)
ccs_data_wellfleet <- filter(ccs_data_all,
                             internal_station_id %in% station_ids_wellfleet) |> 
  # create year and month columns
  mutate(year_collected = year(collected_at),
         month = month(collected_at),
         internal_station_id = case_when(
           internal_station_id == 1 ~ "1 (5N)",
           internal_station_id == 2 ~ "2 (5S)",
           internal_station_id == 3 ~ "3 (5SX)"
           )
         ) |>
  relocate(year_collected, month, .after = collected_at)

# Create list of all water quality variables of interest
wq_variables <- c(
  "temperature_C",                   
  "salinity",                       
  "dissolved_oxygen_mg/L",           
  "chlorophyll_ug/L",               
  "pheophytin_ug/L",                 
  "turbidty_NTU",                   
  "nitrate_nitrite_uM",              
  "ammonium_uM",                    
  "ortho_phosphate_uM",              
  "total_nitrogen_uM",               
  "total_phosphorus_uM"           
)

# Create a named list of threshold values associated with wq variables
thresholds <- c(
  temperature_C = 25,
  `dissolved_oxygen_mg/L` = 6,
  `chlorophyll_ug/L` = 5.1,
  `log10_chlorophyll_ug/L` = log10(5.1),
  turbidty_NTU = 5,
  total_nitrogen_uM = 21.42,
  log10_total_nitrogen_uM = log10(21.42),
  total_phosphorus_uM = 2.29,
  log10_total_phosphorus_uM = log10(2.29)
)

# Set start year and end year for plotting
start_year <- min(ccs_data_wellfleet$collected_at, na.rm = TRUE)
end_year   <- max(ccs_data_wellfleet$collected_at, na.rm = TRUE)


# ---- Check for duplicate data rows in raw CCS data ----

# # Identify which rows are duplicates in wellfleet data (ignore id and internal_station_id)
# duplicates_logical <- duplicated(ccs_data_wellfleet[, -c(1, 2)]) | duplicated(ccs_data_wellfleet[, -c(1, 2)], fromLast = TRUE)
# 
# # Subset the original data to keep all duplicate rows
# if(any(duplicates_logical)){
#   ccs_duplicates <- ccs_data_all[duplicates_logical, ]
#   print("Duplicates found")
# } else {
#   print("Duplicates NOT found")
# }
# 
# # Note - No duplicate data rows identified for the 3 wellfleet sites examined

# ---- Filter Only "Full" Years With at least One Monthly Measurement ----

# NOTE - For each station, the following block only includes years with a full 12 months of data
#      - Many years have 11 months of data (and some 10 or fewer), which could be included

ccs_wellfleet_full_years <- ccs_data_wellfleet |>
  # Count how many measurements per station, year, and month
  group_by(internal_station_id, year_collected, month) |>
  summarize(n_measurements = n(), 
            # average across each month to reduce months with multiple measurements to one average
            across(where(is.numeric), ~ mean(.x, na.rm = TRUE)),
            .groups = "drop"
            ) |>
  # For each station and year, check how many months had data
  group_by(internal_station_id, year_collected) |>
  summarize(n_months = n(), .groups = "drop") |>
  # Keep only station-years with exactly 12 months of measurements
  filter(n_months == 12) |>
  select(internal_station_id, year_collected) |>
  # Join back to original data to filter only good station-years, removing id column
  inner_join(ccs_data_wellfleet, by = c("internal_station_id", "year_collected"))


# Calculate annual mean & median for all water quality variables
ccs_wellfleet_annual <- ccs_wellfleet_full_years |> 
  group_by(internal_station_id, year_collected) |> 
  summarize(
    across(
      all_of(wq_variables),
      list(
        mean = function(x) if (any(is.na(x))) NA_real_ else mean(x),
        median = function(x) if (any(is.na(x))) NA_real_ else median(x)
      )
    ),
    .groups = "drop"
  )

# ---- Filter Only "Full" Summers With Exactly One Monthly Measurement June - September ----

# Filter out non-summer months
ccs_wellfleet_summers <- filter(ccs_data_wellfleet, month %in% 6:9)

ccs_wellfleet_full_summers <- ccs_wellfleet_summers |> 
  # Count how many measurements per station, year, and month
  group_by(internal_station_id, year_collected, month) |>
  summarize(n_measurements = n(), 
            # average across each month to reduce months with multiple measurements to one average
            across(where(is.numeric), ~ mean(.x, na.rm = TRUE)),
            .groups = "drop"
            ) |>
  # For each station and year, check how many months had exactly one measurement
  group_by(internal_station_id, year_collected) |>
  summarize(months_with_one_measurement = sum(n_measurements == 1), .groups = "drop") |>
  # Keep only station-years with exactly one measurement per month (4 months, no extras)
  filter(months_with_one_measurement == 4) |>
  select(internal_station_id, year_collected) |>
  # Join back to original data to filter only good station-years, removing id column
  inner_join(ccs_wellfleet_summers, by = c("internal_station_id", "year_collected")) 


# Calculate (summer) mean & median for all water quality variables
ccs_wellfleet_summers <- ccs_wellfleet_full_summers |> 
  group_by(internal_station_id, year_collected) |> 
  summarize(
    across(
      all_of(wq_variables),
      list(
        mean = function(x) if (any(is.na(x))) NA_real_ else mean(x),
        median = function(x) if (any(is.na(x))) NA_real_ else median(x)
      )
    ),
    .groups = "drop"
  )

# ---- Log10 Transform Variables ----

# Add log transformed chlorophyll, pheophytin, and nutrient values
ccs_data_wellfleet <- ccs_data_wellfleet |>
  mutate(`log10_chlorophyll_ug/L` = log10(`chlorophyll_ug/L`),
         `log10_pheophytin_ug/L` = log10(`pheophytin_ug/L`), 
         log10_nitrate_nitrite_uM = log10(nitrate_nitrite_uM), 
         log10_ammonium_uM = log10(ammonium_uM), 
         log10_ortho_phosphate_uM = log10(ortho_phosphate_uM), 
         log10_total_nitrogen_uM = log10(total_nitrogen_uM),
         log10_total_phosphorus_uM = log10(total_phosphorus_uM))

ccs_wellfleet_annual <- ccs_wellfleet_annual |>
  mutate(`log10_chlorophyll_ug/L_mean` = log10(`chlorophyll_ug/L_mean`),
         `log10_chlorophyll_ug/L_median` = log10(`chlorophyll_ug/L_median`),
         `log10_pheophytin_ug/L_mean` = log10(`pheophytin_ug/L_mean`),
         `log10_pheophytin_ug/L_median` = log10(`pheophytin_ug/L_median`),
         log10_nitrate_nitrite_uM_mean = log10(nitrate_nitrite_uM_mean), 
         log10_nitrate_nitrite_uM_median = log10(nitrate_nitrite_uM_median),
         log10_ammonium_uM_mean = log10(ammonium_uM_mean), 
         log10_ammonium_uM_median = log10(ammonium_uM_median),
         log10_ortho_phosphate_uM_mean = log10(ortho_phosphate_uM_mean), 
         log10_ortho_phosphate_uM_median = log10(ortho_phosphate_uM_median), 
         log10_total_nitrogen_uM_mean = log10(total_nitrogen_uM_mean),
         log10_total_nitrogen_uM_median = log10(total_nitrogen_uM_median),
         log10_total_phosphorus_uM_mean = log10(total_phosphorus_uM_mean),
         log10_total_phosphorus_uM_median = log10(total_phosphorus_uM_median))

ccs_wellfleet_full_summers <- ccs_wellfleet_full_summers |> 
  mutate(`log10_chlorophyll_ug/L` = log10(`chlorophyll_ug/L`),
         `log10_pheophytin_ug/L` = log10(`pheophytin_ug/L`), 
         log10_nitrate_nitrite_uM = log10(nitrate_nitrite_uM), 
         log10_ammonium_uM = log10(ammonium_uM), 
         log10_ortho_phosphate_uM = log10(ortho_phosphate_uM), 
         log10_total_nitrogen_uM = log10(total_nitrogen_uM),
         log10_total_phosphorus_uM = log10(total_phosphorus_uM))

ccs_wellfleet_summers <- ccs_wellfleet_summers |>
  mutate(`log10_chlorophyll_ug/L_mean` = log10(`chlorophyll_ug/L_mean`),
         `log10_chlorophyll_ug/L_median` = log10(`chlorophyll_ug/L_median`),
         `log10_pheophytin_ug/L_mean` = log10(`pheophytin_ug/L_mean`),
         `log10_pheophytin_ug/L_median` = log10(`pheophytin_ug/L_median`),
         log10_nitrate_nitrite_uM_mean = log10(nitrate_nitrite_uM_mean), 
         log10_nitrate_nitrite_uM_median = log10(nitrate_nitrite_uM_median),
         log10_ammonium_uM_mean = log10(ammonium_uM_mean), 
         log10_ammonium_uM_median = log10(ammonium_uM_median),
         log10_ortho_phosphate_uM_mean = log10(ortho_phosphate_uM_mean), 
         log10_ortho_phosphate_uM_median = log10(ortho_phosphate_uM_median), 
         log10_total_nitrogen_uM_mean = log10(total_nitrogen_uM_mean),
         log10_total_nitrogen_uM_median = log10(total_nitrogen_uM_median),
         log10_total_phosphorus_uM_mean = log10(total_phosphorus_uM_mean),
         log10_total_phosphorus_uM_median = log10(total_phosphorus_uM_median))

# Update wq_variables to add log transformed chlorophyll
wq_variables <- c(wq_variables,
                  "log10_chlorophyll_ug/L",
                  "log10_pheophytin_ug/L",
                  "log10_nitrate_nitrite_uM",
                  "log10_ammonium_uM",
                  "log10_ortho_phosphate_uM",
                  "log10_total_nitrogen_uM",
                  "log10_total_phosphorus_uM")




# ---- Create metadata variables for labeling ----
wq_variables_meta <- list(
  temperature_C = "Temperature (°C)",
  salinity = "Salinity (PSU)",
  `dissolved_oxygen_mg/L` = "Dissolved Oxygen (mg/L)",
  `chlorophyll_ug/L` = "Chlorophyll (µg/L)",
  `pheophytin_ug/L` = "Pheophytin (µg/L)",
  turbidty_NTU = "Turbidity (NTU)",
  nitrate_nitrite_uM = "Nitrate + Nitrite (µM)",
  ammonium_uM = "Ammonium (µM)",
  ortho_phosphate_uM = "Orthophosphate (µM)",
  total_nitrogen_uM = "Total Nitrogen (µM)",
  total_phosphorus_uM = "Total Phosphorus (µM)",
  `log10_chlorophyll_ug/L` = "log10 Chlorophyll [log10(µg/L)]",
  `log10_pheophytin_ug/L` = "log10 Pheophytin [log10(µg/L)]",
  log10_nitrate_nitrite_uM = "log10 Nitrate + Nitrite [log10(µM)]",
  log10_ammonium_uM = "log10 Ammonium [log10(µM)]",
  log10_ortho_phosphate_uM = "log10 Orthophosphate [log10(µM)]",
  log10_total_nitrogen_uM = "log10 Total Nitrogen [log10(µM)]",
  log10_total_phosphorus_uM = "log10 Total Phosphorus [log10(µM)]"
)

# Function for customizing this labeling for plotting:
adjust_label <- function(label, extra_text) {
  if (startsWith(label, "log10 ")) {
    # Remove "log10 ", add custom text, then keep the rest
    rest <- sub("^log10 ", "", label)
    paste("log10", extra_text, rest)
  } else {
    paste(extra_text, label)
  }
}


# # ---- Simple Linear Models - Annual ----
# 
# ### Build simple linear models for annual means and medians
# 
# # Adjust year_collected to years following [initial year]. 
# # This will not change the slope, but will change the intercept to be more interpretable
# ccs_wellfleet_annual <- ccs_wellfleet_annual |>
#   mutate(years_post_start = year_collected - min(year_collected))
# 
# # Create a tibble with columns: buoy, variable, list column of lm(variable ~ year_collected)
# buoy <- unique(ccs_wellfleet_annual$internal_station_id)
# variable <- names(select(ccs_wellfleet_annual,
#                          -c("internal_station_id", "year_collected", "years_post_start")))
# annual_lms <- crossing(buoy, variable)
# annual_lms <- annual_lms |> 
#   rowwise() |> 
#   mutate(model = list(
#     lm(
#       formula = as.formula(paste0("`", variable, "` ~ years_post_start")),
#       data = filter(ccs_wellfleet_annual, internal_station_id == buoy)
#     )
#   )) |> 
#   ungroup()
# 
# # For each row (model) extract relevant summary statistics
# annual_lms <- annual_lms |>
#   mutate(
#     tidy_summary = map(model, tidy), # for slope, slope std.error, and slope p.value
#     glance_summary = map(model, glance), # for R2
#     intercept = map_dbl(tidy_summary, ~ .x |> filter(term == "(Intercept)") |> pull(estimate)),
#     slope = map_dbl(tidy_summary, ~ .x |> filter(term == "years_post_start") |> pull(estimate)),
#     std_error = map_dbl(tidy_summary, ~ .x |> filter(term == "years_post_start") |> pull(std.error)),
#     p_value = map_dbl(tidy_summary, ~ .x |> filter(term == "years_post_start") |> pull(p.value)),
#     r_squared = map_dbl(glance_summary, ~ .x$r.squared), 
#     label = paste0(buoy, ":  y = ",
#                    round(intercept, 2), " + ",
#                    round(slope, 3), "x,  ",
#                    "R² = ", round(r_squared, 2), ", ",
#                    "p = ", signif(p_value, 3))
#   )
# 
# # annual_lms$tidy_summary[[1]]
# # annual_lms$glance_summary[[1]]



# ---- Simple Linear Models - Monthly ----



# ---- Plot All Data ----
# For each relevant variable, plot all ~monthly data over time
for (var in wq_variables) {
  
  # Determine y axis range and create vertical padded y_max so annotations will be visible
  y_max <- max(ccs_data_wellfleet[[var]], na.rm = TRUE)
  y_min <- min(ccs_data_wellfleet[[var]], na.rm = TRUE)
  range_size = y_max - y_min
  y_max_buffered <- y_max + (range_size * 0.2)
  
  # Plot
  p <- ggplot(ccs_data_wellfleet, aes(x = collected_at,
                                      y = .data[[var]],
                                      color = as.factor(internal_station_id))) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +

    # add threshold line, if available
    geom_hline(yintercept = thresholds[var], linetype = 'dotted', color = 'red', linewidth = 2) +
    
    labs(x = "Date",
         y = wq_variables_meta[[var]],
         color = "CCS Station ID",
         title = wq_variables_meta[[var]],
         subtitle = "All Data Time Series - CCS Wellfleet") +
    scale_x_datetime(
      limits = c(start_year, end_year),
      date_breaks = "2 year", 
      date_labels = "%Y",
      labels = date_format("%Y")) + # extract just the year for the labels
    scale_color_paletteer_d("yarrr::google") +
    
    # Add vertical padding
    scale_y_continuous(limits = c(NA, y_max_buffered)) +
    
    # Annotate plot with simple linear model p-values and R2 values
    stat_poly_eq(
      aes(group = internal_station_id, 
          color = internal_station_id,
          label = after_stat(
            paste0("Station: ", grp.label, "~~~",
                   ..p.value.label.., "~~~",
                   ..rr.label..
            ))),
      # formula = y ~ x,
      parse = TRUE,
      size = 3, 
      label.x = "left",
      label.y = "top",
      vstep = 0.025
    )
  
  print(p)
}


# ---- Plot Annual Mean and Median ----

# For each relevant variable, plot Annual mean and median values
for (var in wq_variables) {
  
  # Set variable name to _mean
  var_name <- paste0(var, "_mean")
  
  # Determine y axis range and create vertical padded y_max so annotations will be visible
  y_max <- max(ccs_wellfleet_annual[[var_name]], na.rm = TRUE)
  y_min <- min(ccs_wellfleet_annual[[var_name]], na.rm = TRUE)
  range_size = y_max - y_min
  y_max_buffered <- y_max + (range_size * 0.2)
  # # Update range size with new buffered y max
  # range_size = y_max_buffered - y_min
    
  # Create annual mean plot
  p_1 <- ggplot(ccs_wellfleet_annual, aes(x = year_collected,
                                        y = .data[[var_name]],
                                        color = as.factor(internal_station_id))) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    
    labs(
      x = "Year",
      y = wq_variables_meta[[var]],
      color = "CCS Station ID",
      title = adjust_label(wq_variables_meta[[var]], "Mean"),
      ) +
    scale_color_paletteer_d("yarrr::google") +
    theme(
      plot.title = element_text(size = 12),
      axis.title.y = element_text(size = 8)
      ) +
    
    # Add vertical padding
    scale_y_continuous(limits = c(NA, y_max_buffered)) +
  
    # Annotate plot with simple linear model p-values and R2 values
    stat_poly_eq(
      aes(group = internal_station_id, 
          color = internal_station_id,
          label = after_stat(
            paste0("Station: ", grp.label, "~~~",
                   ..p.value.label.., "~~~",
                   ..rr.label..
            ))),
      # formula = y ~ x,
      parse = TRUE,
      label.x = "left",
      label.y = "top",
      vstep = 0.06,
      size = 2.5
    ) 
  
  
  ## Method for adding regression statistic labels based on previously build models
  # # Add labels
  # labels_to_add <- annual_lms |>
  #   filter(variable == var_name) |>
  #   mutate(x = min(ccs_wellfleet_annual$year_collected, na.rm = TRUE),
  #          y = seq(from = y_max_buffered, length.out = n(), by = (range_size * -0.08)))
  
  # p_1 <- p_1 + 
  #   geom_text(data = labels_to_add,
  #             aes(x = x, y = y, label = label, color = as.factor(buoy)),
  #             inherit.aes = FALSE,
  #             hjust = 0, 
  #             vjust = 1,
  #             size = 2.5,
  #             show.legend = FALSE)


  
  # Set variable name to _median
  var_name <- paste0(var, "_median")
  
  # Determine y axis range and create vertical padded y_max so annotations will be visible
  y_max <- max(ccs_wellfleet_annual[[var_name]], na.rm = TRUE)
  y_min <- min(ccs_wellfleet_annual[[var_name]], na.rm = TRUE)
  range_size = y_max - y_min
  y_max_buffered <- y_max + (range_size * 0.2)
  
  # Create annual median plot
  p_2 <- ggplot(ccs_wellfleet_annual, aes(x = year_collected,
                                        y = .data[[var_name]],
                                        color = as.factor(internal_station_id))) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    labs(
      x = "Year",
     y = wq_variables_meta[[var]],
     color = "CCS Station ID",
     title = adjust_label(wq_variables_meta[[var]], "Median")
    ) +
    scale_color_paletteer_d("yarrr::google") +
    theme(
      plot.title = element_text(size = 12),
      axis.title.y = element_text(size = 8)
    ) +
    
    # Add vertical padding
    scale_y_continuous(limits = c(NA, y_max_buffered)) +
    
    # Annotate plot with simple linear model p-values and R2 values
    stat_poly_eq(
      aes(group = internal_station_id, 
          color = internal_station_id,
          label = after_stat(
            paste0("Station: ", grp.label, "~~~",
                   ..p.value.label.., "~~~",
                   ..rr.label..
            ))),
      # formula = y ~ x,
      parse = TRUE,
      label.x = "left",
      label.y = "top",
      vstep = 0.06,
      size = 2.5
    ) 
    
  # Combine the plots using patchwork with adjusted spacing
  combined_plot <- p_1 + p_2 + 
    # Stack the plots vertically
    plot_layout(ncol = 1, heights = c(1, 1)) +
    plot_annotation(
      title = "Annual Time Series - CCS Wellfleet",
      theme = theme(plot.title = element_text(face = "bold"))
    )
  
  print(combined_plot)
}


# ---- Plot All Summer Data ----

# For each relevant variable, plot all Summer ~monthly data over time
for (var in wq_variables) {
  
  # Determine y axis range and create vertical padded y_max so annotations will be visible
  y_max <- max(ccs_wellfleet_full_summers[[var]], na.rm = TRUE)
  y_min <- min(ccs_wellfleet_full_summers[[var]], na.rm = TRUE)
  range_size = y_max - y_min
  y_max_buffered <- y_max + (range_size * 0.2)
  
  # Plot
  p <- ggplot(ccs_wellfleet_full_summers, aes(x = collected_at,
                                      y = .data[[var]],
                                      color = as.factor(internal_station_id))) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +

    # add threshold line, if available
    geom_hline(yintercept = thresholds[var], linetype = 'dotted', color = 'red', linewidth = 2) +

    labs(
      x = "Date",
      y = wq_variables_meta[[var]],
      color = "CCS Station ID",
      title = paste(wq_variables_meta[[var]]),
      subtitle = "Summer Data Time Series - CCS Wellfleet"
      ) +
    scale_x_datetime(
      limits = c(start_year, end_year),
      date_breaks = "2 year",
      date_labels = "%Y",
      labels = date_format("%Y")) +
    scale_color_paletteer_d("yarrr::google") + 
    
    # Add vertical padding
    scale_y_continuous(limits = c(NA, y_max_buffered)) +
    
    # Annotate plot with simple linear model p-values and R2 values
    stat_poly_eq(
      aes(group = internal_station_id, 
          color = internal_station_id,
          label = after_stat(
            paste0("Station: ", grp.label, "~~~",
                   ..p.value.label.., "~~~",
                   ..rr.label..
            ))),
      # formula = y ~ x,
      parse = TRUE,
      size = 3, 
      label.x = "left",
      label.y = "top",
      vstep = 0.025
    )

  print(p)
}


# ---- Plot Summer Mean and Median ----

# For each relevant variable, plot Summer mean and median values
for (var in wq_variables) {
  
  var_name <- paste0(var, "_mean")
  
  # Determine y axis range and create vertical padded y_max so annotations will be visible
  y_max <- max(ccs_wellfleet_summers[[var_name]], na.rm = TRUE)
  y_min <- min(ccs_wellfleet_summers[[var_name]], na.rm = TRUE)
  range_size = y_max - y_min
  y_max_buffered <- y_max + (range_size * 0.2)
  
  # Create summer mean plot
  p_1 <- ggplot(ccs_wellfleet_summers, aes(x = year_collected,
                                          y = .data[[var_name]],
                                          color = as.factor(internal_station_id))) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    
    # add threshold line, if available
    geom_hline(yintercept = thresholds[var], linetype = 'dotted', color = 'red', linewidth = 2) +
    
    labs(x = "Year",
         y = wq_variables_meta[[var]],
         color = "CCS Station ID",
         title = adjust_label(wq_variables_meta[[var]], "Summer Mean")
         ) +
    scale_color_paletteer_d("yarrr::google") +
    theme(
      plot.title = element_text(size = 12),
      axis.title.y = element_text(size = 8)
    ) +
    
    # Add vertical padding
    scale_y_continuous(limits = c(NA, y_max_buffered)) +
    
    # Annotate plot with simple linear model p-values and R2 values
    stat_poly_eq(
      aes(group = internal_station_id, 
          color = internal_station_id,
          label = after_stat(
            paste0("Station: ", grp.label, "~~~",
                   ..p.value.label.., "~~~",
                   ..rr.label..
            ))),
      # formula = y ~ x,
      parse = TRUE,
      label.x = "left",
      label.y = "top",
      vstep = 0.06,
      size = 2.5
    ) 
  
  
  
  # Create summer median plot
  var_name <- paste0(var, "_median")
  
  # Determine y axis range and create vertical padded y_max so annotations will be visible
  y_max <- max(ccs_wellfleet_summers[[var_name]], na.rm = TRUE)
  y_min <- min(ccs_wellfleet_summers[[var_name]], na.rm = TRUE)
  range_size = y_max - y_min
  y_max_buffered <- y_max + (range_size * 0.2)
  
  # Plot
  p_2 <- ggplot(ccs_wellfleet_summers, aes(x = year_collected,
                                          y = .data[[var_name]],
                                          color = as.factor(internal_station_id))) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    
    # add threshold line, if available
    geom_hline(yintercept = thresholds[var], linetype = 'dotted', color = 'red', linewidth = 2) +
    
    labs(x = "Year",
         y = wq_variables_meta[[var]],
         color = "CCS Station ID",
         title = adjust_label(wq_variables_meta[[var]], "Summer Median")
         ) +
    scale_color_paletteer_d("yarrr::google") +
    theme(
      plot.title = element_text(size = 12),
      axis.title.y = element_text(size = 8)
    ) +
    
    # Add vertical padding
    scale_y_continuous(limits = c(NA, y_max_buffered)) +
    
    # Annotate plot with simple linear model p-values and R2 values
    stat_poly_eq(
      aes(group = internal_station_id, 
          color = internal_station_id,
          label = after_stat(
            paste0("Station: ", grp.label, "~~~",
                   ..p.value.label.., "~~~",
                   ..rr.label..
            ))),
      # formula = y ~ x,
      parse = TRUE,
      label.x = "left",
      label.y = "top",
      vstep = 0.06,
      size = 2.5
    ) 
  
  
  combined_plot <- p_1 + p_2 + 
    plot_layout(ncol = 1, heights = c(1, 1)) +
    plot_annotation(
      title = "Summer Time Series - CCS Wellfleet",
      theme = theme(plot.title = element_text(face = "bold"))
    )
  
  print(combined_plot)
}


# ---- Plot Summer Mean ----

# For each relevant variable, plot Summer mean and mean values
for (var in wq_variables) {
  
  var_name <- paste0(var, "_mean")
  
  # Determine y axis range and create vertical padded y_max so annotations will be visible
  y_max <- max(ccs_wellfleet_summers[[var_name]], na.rm = TRUE)
  y_min <- min(ccs_wellfleet_summers[[var_name]], na.rm = TRUE)
  range_size = y_max - y_min
  y_max_buffered <- y_max + (range_size * 0.2)
  
  # Create summer mean plot
  p_1 <- ggplot(ccs_wellfleet_summers, aes(x = year_collected,
                                           y = .data[[var_name]],
                                           color = as.factor(internal_station_id))) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    
    # add threshold line, if available
    geom_hline(yintercept = thresholds[var], linetype = 'dotted', color = 'red', linewidth = 2) +
    
    labs(
      x = "Date",
      y = wq_variables_meta[[var]],
      color = "CCS Station ID",
      title = paste(wq_variables_meta[[var]]),
      subtitle = "Annual Summer Mean Time Series - CCS Wellfleet"
    ) +
    scale_x_continuous(
      breaks = seq(
        min(ccs_wellfleet_summers$year_collected), 
        max(ccs_wellfleet_summers$year_collected), 
        by = 2
      )
    ) +
    scale_color_paletteer_d("yarrr::google") +
    
    # Add vertical padding
    scale_y_continuous(limits = c(NA, y_max_buffered)) +
    
    # Annotate plot with simple linear model p-values and R2 values
    stat_poly_eq(
      aes(group = internal_station_id, 
          color = internal_station_id,
          label = after_stat(
            paste0("Station: ", grp.label, "~~~",
                   ..p.value.label.., "~~~",
                   ..rr.label..
            ))),
      # formula = y ~ x,
      parse = TRUE,
      size = 3, 
      label.x = "left",
      label.y = "top",
      vstep = 0.025
    ) 
  
  print(p_1)
}



# # ---- Plots by Variable (Incomplete) ----
# 
# # _____________________________ Temperature _____________________________
# 
# # All Data (~Monthly)
# ggplot(data = ccs_data_wellfleet,
#        mapping = aes(x = collected_at, 
#                      y = temperature_C,
#        color = as.factor(internal_station_id))) +
#   geom_point() +
#   
#   # add 25 Celsius threshold line
#   geom_hline(yintercept = 25, linetype = 'dotted', color = 'red', linewidth = 2) +
#   labs(x = "Date",
#        y = "Temperature (°C)",
#        color = "CCS Station ID",
#        title = paste("Temp (C) Time Series - CCS Wellfleet")) +
#   scale_x_datetime(
#     limits = c(start_year, end_year),
#     date_breaks = "2 year", 
#     date_labels = "%Y",
#     labels = date_format("%Y")) + # extract just the year for the labels
#   scale_color_brewer(palette = "Set2")
# 
# 
# # Annual Mean
# ggplot(data = ccs_wellfleet_annual,
#        mapping = aes(x = year_collected, 
#                      y = temperature_C_mean,
#                      color = as.factor(internal_station_id))) +
#   geom_point(size = 3.5) +
#   labs(x = "Year",
#        y = "Annual Mean Temperature (°C)",
#        color = "CCS Station ID",
#        title = paste("Annual Mean Temp (C) Time Series - CCS Wellfleet")) +
#   scale_color_brewer(palette = "Set2")
# 
# 
# # Annual Median
# ggplot(data = ccs_wellfleet_annual,
#        mapping = aes(x = year_collected, 
#                      y = temperature_C_median,
#                      color = as.factor(internal_station_id))) +
#   geom_point(size = 3.5) +
#   labs(x = "Date",
#        y = "Annual Median Temperature (°C)",
#        color = "CCS Station ID",
#        title = paste("Annual Median Temp (C) Time Series - CCS Wellfleet")) +
#   scale_color_brewer(palette = "Set2")
# 
# # _____________________________ Dissolved Oxygen _____________________________
# 
# # All Data (~Monthly)
# ggplot(data = ccs_data_wellfleet,
#        mapping = aes(x = collected_at, 
#                      y = `dissolved_oxygen_mg/L`,
#                      color = as.factor(internal_station_id))) +
#   geom_point() +
#   
#   # add 6 mg/L threshold line
#   geom_hline(yintercept = 6, linetype = 'dotted', color = 'red', linewidth = 2) +
#   labs(x = "Date",
#        y = "DO (mg/L)",
#        color = "CCS Station ID",
#        title = paste("DO Time Series - CCS Wellfleet")) +
#   scale_x_datetime(
#     limits = c(start_year, end_year),
#     date_breaks = "2 year", 
#     date_labels = "%Y",
#     labels = date_format("%Y")) +
#   scale_color_brewer(palette = "Set2")
# 
# 
# # Annual Mean
# ggplot(data = ccs_wellfleet_annual,
#        mapping = aes(x = year_collected, 
#                      y = `dissolved_oxygen_mg/L_mean`,
#                      color = as.factor(internal_station_id))) +
#   geom_point(size = 3.5) +
#   labs(x = "Year",
#        y = "Annual Mean DO (mg/L)",
#        color = "CCS Station ID",
#        title = paste("Annual Mean DO Time Series - CCS Wellfleet")) +
#   scale_color_brewer(palette = "Set2")
# 
# 
# # Annual Median
# ggplot(data = ccs_wellfleet_annual,
#        mapping = aes(x = year_collected, 
#                      y = `dissolved_oxygen_mg/L_median`,
#                      color = as.factor(internal_station_id))) +
#   geom_point(size = 3.5) +
#   labs(x = "Date",
#        y = "Annual Median DO (mg/L)",
#        color = "CCS Station ID",
#        title = paste("Annual Median DO Time Series - CCS Wellfleet")) +
#   scale_color_brewer(palette = "Set2")
# 
# 
# # _____________________________ Chlorophyll a _____________________________
# 
# # All Data (~Monthly)
# ggplot(data = ccs_data_wellfleet,
#        mapping = aes(x = collected_at, 
#                      y = `chlorophyll_ug/L`,
#                      color = as.factor(internal_station_id))) +
#   geom_point() +
#   
#   # add 5.1 u/L threshold line
#   geom_hline(yintercept = 5.1, linetype = 'dotted', color = 'red', linewidth = 2) +
#   labs(x = "Date",
#        y = "Chlorophyll a (ug/L)",
#        color = "CCS Station ID",
#        title = paste("Chlorophyll Time Series - CCS Wellfleet")) +
#   scale_x_datetime(
#     limits = c(start_year, end_year),
#     date_breaks = "2 year", 
#     date_labels = "%Y",
#     labels = date_format("%Y")) +
#   scale_color_brewer(palette = "Set2")
# 
# 
# # Annual Mean
# ggplot(data = ccs_wellfleet_annual,
#        mapping = aes(x = year_collected, 
#                      y = `chlorophyll_ug/L_mean`,
#                      color = as.factor(internal_station_id))) +
#   geom_point(size = 3.5) +
#   labs(x = "Year",
#        y = "Annual Mean Chlorophyll a (ug/L)",
#        color = "CCS Station ID",
#        title = paste("Annual Mean Chlorophyll a Time Series - CCS Wellfleet")) +
#   scale_color_brewer(palette = "Set2")
# 
# 
# # Annual Median
# ggplot(data = ccs_wellfleet_annual,
#        mapping = aes(x = year_collected, 
#                      y = `chlorophyll_ug/L_median`,
#                      color = as.factor(internal_station_id))) +
#   geom_point(size = 3.5) +
#   labs(x = "Date",
#        y = "Annual Median Chlorophyll a (ug/L)",
#        color = "CCS Station ID",
#        title = paste("Annual Median Chlorophyll a Time Series - CCS Wellfleet")) +
#   scale_color_brewer(palette = "Set2")
# 
# 
# # _____________________________ Total Nitrogen _____________________________
# # Data set units in uM (micromoles / L)
# # Threshold value of 0.071 mg/L converted to 21.42 uM
#   # MW N = 14.006720 µg/L N (https://www.ices.dk/data/tools/Pages/Unit-conversions.aspx)
#   # 1 mg N/L = 71.394 µM/L
# 
# # All Data (~Monthly)
# ggplot(data = ccs_data_wellfleet,
#        mapping = aes(x = collected_at, 
#                      y = total_nitrogen_uM,
#                      color = as.factor(internal_station_id))) +
#   geom_point() +
#   
#   # add 21.42 uM threshold line
#   geom_hline(yintercept = 21.42, linetype = 'dotted', color = 'red', linewidth = 2) +
#   labs(x = "Date",
#        y = "Total N (uM)",
#        color = "CCS Station ID",
#        title = paste("Total Nitrogen Time Series - CCS Wellfleet")) +
#   scale_x_datetime(
#     limits = c(start_year, end_year),
#     date_breaks = "2 year", 
#     date_labels = "%Y",
#     labels = date_format("%Y")) +
#   scale_color_brewer(palette = "Set2")
# 
# 
# # Annual Mean
# ggplot(data = ccs_wellfleet_annual,
#        mapping = aes(x = year_collected, 
#                      y = total_nitrogen_uM_mean,
#                      color = as.factor(internal_station_id))) +
#   geom_point(size = 3.5) +
#   labs(x = "Year",
#        y = "Annual Mean Total N (uM)",
#        color = "CCS Station ID",
#        title = paste("Annual Mean Total N Time Series - CCS Wellfleet")) +
#   scale_color_brewer(palette = "Set2")
# 
# 
# # Annual Median
# ggplot(data = ccs_wellfleet_annual,
#        mapping = aes(x = year_collected, 
#                      y = total_nitrogen_uM_median,
#                      color = as.factor(internal_station_id))) +
#   geom_point(size = 3.5) +
#   labs(x = "Date",
#        y = "Annual Median Total N (uM)",
#        color = "CCS Station ID",
#        title = paste("Annual Median Total N Time Series - CCS Wellfleet")) +
#   scale_color_brewer(palette = "Set2")
# 
# 
# # _____________________________ Total Phosphorus _____________________________
# # Data set units in uM (micromoles / L)
# # Threshold value of 0.071 mg/L converted to 2.29 uM
#   # MW P = 30.973762 µg/L P (https://www.ices.dk/data/tools/Pages/Unit-conversions.aspx)
#   # 1 mg P/L = 32.285 µM/L
# 
# # All Data (~Monthly)
# ggplot(data = ccs_data_wellfleet,
#        mapping = aes(x = collected_at, 
#                      y = total_phosphorus_uM,
#                      color = as.factor(internal_station_id))) +
#   geom_point() +
#   
#   # add 2.29 uM threshold line
#   geom_hline(yintercept = 2.29, linetype = 'dotted', color = 'red', linewidth = 2) +
#   labs(x = "Date",
#        y = "Total P (uM)",
#        color = "CCS Station ID",
#        title = paste("Total Phorphorus Time Series - CCS Wellfleet")) +
#   scale_x_datetime(
#     limits = c(start_year, end_year),
#     date_breaks = "2 year", 
#     date_labels = "%Y",
#     labels = date_format("%Y")) +
#   scale_color_brewer(palette = "Set2")
# 
# 
# # Annual Mean
# ggplot(data = ccs_wellfleet_annual,
#        mapping = aes(x = year_collected, 
#                      y = total_phosphorus_uM_mean,
#                      color = as.factor(internal_station_id))) +
#   geom_point(size = 3.5) +
#   labs(x = "Year",
#        y = "Annual Mean Total P (uM)",
#        color = "CCS Station ID",
#        title = paste("Annual Mean Total P Time Series - CCS Wellfleet")) +
#   scale_color_brewer(palette = "Set2")
# 
# 
# # Annual Median
# ggplot(data = ccs_wellfleet_annual,
#        mapping = aes(x = year_collected, 
#                      y = total_phosphorus_uM_median,
#                      color = as.factor(internal_station_id))) +
#   geom_point(size = 3.5) +
#   labs(x = "Date",
#        y = "Annual Median Total P (uM)",
#        color = "CCS Station ID",
#        title = paste("Annual Median Total P Time Series - CCS Wellfleet")) +
#   scale_color_brewer(palette = "Set2")
# 
# 
# # _____________________________ Turbidity _____________________________
# 
# # All Data (~Monthly)
# ggplot(data = ccs_data_wellfleet,
#        mapping = aes(x = collected_at, 
#                      y = turbidty_NTU,
#                      color = as.factor(internal_station_id))) +
#   geom_point() +
#   
#   # add 5 NTU threshold line
#   geom_hline(yintercept = 5, linetype = 'dotted', color = 'red', linewidth = 2) +
#   labs(x = "Date",
#        y = "Turbidity (NTU)",
#        color = "CCS Station ID",
#        title = paste("Turbidity Time Series - CCS Wellfleet")) +
#   scale_x_datetime(
#     limits = c(start_year, end_year),
#     date_breaks = "2 year", 
#     date_labels = "%Y",
#     labels = date_format("%Y")) +
#   scale_color_brewer(palette = "Set2")
# 
# 
# # _____________________________ Salinity _____________________________
# 
# # All Data (~Monthly)
# ggplot(data = ccs_data_wellfleet,
#        mapping = aes(x = collected_at, 
#                      y = salinity,
#                      color = as.factor(internal_station_id))) +
#   geom_point() +
#   
#   labs(x = "Date",
#        y = "Salinity",
#        color = "CCS Station ID",
#        title = paste("Salinity Time Series - CCS Wellfleet")) +
#   scale_x_datetime(
#     limits = c(start_year, end_year),
#     date_breaks = "2 year", 
#     date_labels = "%Y",
#     labels = date_format("%Y")) +
#   scale_color_brewer(palette = "Set2")
# 
# 
# # Annual Mean
# ggplot(data = ccs_wellfleet_annual,
#        mapping = aes(x = year_collected, 
#                      y = salinity_mean,
#                      color = as.factor(internal_station_id))) +
#   geom_point(size = 3.5) +
#   labs(x = "Year",
#        y = "Annual Mean Salinity",
#        color = "CCS Station ID",
#        title = paste("Annual Mean Salinity Time Series - CCS Wellfleet")) +
#   scale_color_brewer(palette = "Set2")


